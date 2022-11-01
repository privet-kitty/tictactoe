(defpackage :tictactoe
  (:use :cl)
  (:import-from :sb-c #:mask-signed-field)
  (:export #:board #:bref #:solve-top-left #:simulate #:analyze-game-tree #:valid-board-p
           #:learn-double))

(in-package :tictactoe)

(defconstant +player1+ 1)
(defconstant +player2+ -1)
(defconstant +draw+ 0)
(defconstant +unknown+ -128)

(deftype board () '(unsigned-byte 18))
(deftype int8 () '(signed-byte 8))

(cp/integer-pack:define-integer-pack move (i 2) (j 2))

(declaim (inline get-index))
(defun get-index (i j)
  (* 2 (+ (* i 3) j)))

(declaim (inline bref))
(defun bref (board i j)
  (declare (board board)
           ((unsigned-byte 2) i j))
  (mask-signed-field 2 (ldb (byte 2 (get-index i j)) board)))

(declaim (inline bset))
(defun bset (board i j new-value)
  (dpb new-value (byte 2 (get-index i j)) board))

(define-setf-expander bref (board i j &environment env)
  (multiple-value-bind (tmps vals stores store-form access-form)
      (get-setf-expansion board env)
    (when (cdr stores)
      (error "SETF BREF too hairy."))
    (let ((i-tmp (gensym "I"))
          (j-tmp (gensym "J"))
          (store (gensym "NEW")))
      (values `(,i-tmp ,j-tmp ,@tmps)
              `(,i ,j ,@vals)
              (list store)
              `(let ((,(car stores) (bset ,access-form ,i-tmp ,j-tmp  ,store)))
                 ,store-form
                 ,store)
              `(bref ,access-form ,i ,j)))))

(declaim (ftype (function * (values (integer -1 1) &optional)) detect-winner))
(defun detect-winner (board)
  "Returns a winner if it exists, otherwise returns 0. Note that this
function may return any value if BOARD is not valid."
  (declare (optimize (speed 3))
           (board board))
  (loop for p from -1 to 1 by 2
        when (or (= p (bref board 0 0) (bref board 0 1) (bref board 0 2))
                 (= p (bref board 1 0) (bref board 1 1) (bref board 1 2))
                 (= p (bref board 2 0) (bref board 2 1) (bref board 2 2))
                 (= p (bref board 0 0) (bref board 1 0) (bref board 2 0))
                 (= p (bref board 0 1) (bref board 1 1) (bref board 2 1))
                 (= p (bref board 0 2) (bref board 1 2) (bref board 2 2))
                 (= p (bref board 0 0) (bref board 1 1) (bref board 2 2))
                 (= p (bref board 0 2) (bref board 1 1) (bref board 2 0)))
        do (return p)
        finally (return +draw+)))

(defun consist-of-legal-mark-p (board)
  (declare (optimize (speed 3))
           (board board))
  (dotimes (i 3)
    (dotimes (j 3)
      (unless (<= -1 (bref board i j) 1)
        (return-from consist-of-legal-mark-p))))
  t)

(declaim (ftype (function * (values (integer 0 9) (integer 0 9) &optional))
                count-each-mark))
(defun count-each-mark (board)
  (declare (optimize (speed 3))
           (board board))
  (let ((p1 0)
        (p2 0))
    (declare ((integer 0 9) p1 p2))
    (dotimes (i 3)
      (dotimes (j 3)
        (ecase (bref board i j)
          (0)
          (1 (incf p1))
          (-1 (incf p2)))))
    (values p1 p2)))

(defun valid-board-p (board)
  (declare (optimize (speed 3))
           (board board))
  (and (consist-of-legal-mark-p board)
       (or (zerop board)
           (multiple-value-bind (p1 p2) (count-each-mark board)
             (and (<= 0 (- p1 p2) 1)
                  (let ((last-player (if (= p1 p2) +player2+ +player1+)))
                    (dotimes (i 3)
                      (dotimes (j 3)
                        (when (= last-player (bref board i j))
                          (setf (bref board i j) 0)
                          (when (zerop (detect-winner board))
                            (return-from valid-board-p t))
                          (setf (bref board i j) last-player))))))))))

(defun analyze-game-tree ()
  (declare (optimize (speed 3)))
  (let ((res (make-array (ash 1 18) :element-type 'int8 :initial-element +unknown+)))
    (labels ((recur (depth board)
               (declare (board board)
                        (fixnum depth))
               (assert (<= depth 9))
               (setf (aref res board)
                     (cond ((not (= +unknown+ (aref res board)))
                            (aref res board))
                           ((or (= depth 9) (not (zerop (detect-winner board))))
                            (detect-winner board))
                           (t
                            (let ((player (if (evenp depth) +player1+ +player2+))
                                  win-p
                                  draw-p)
                              (dotimes (i 3)
                                (dotimes (j 3)
                                  (when (zerop (bref board i j))
                                    (setf (bref board i j) player)
                                    (let ((next-state (recur (+ 1 depth) board)))
                                      (cond ((= next-state player)
                                             (setq win-p t))
                                            ((= next-state +draw+)
                                             (setq draw-p t))))
                                    (setf (bref board i j) 0))))
                              (cond (win-p player)
                                    (draw-p +draw+)
                                    (t (flip-player player)))))))))
      (recur 0 0))
    res))

(defun println-all-positions (&optional (stream *standard-output*))
  (let ((evaluation (analyze-game-tree))
        (count 0))
    (dotimes (board (ash 1 18))
      (unless (= +unknown+ (aref evaluation board))
        (uiop:println (aref evaluation board) stream)
        (println-board board)
        (incf count)))
    count))

(defparameter *state* (sb-ext:seed-random-state 0))

(defun solve-with-ev (ev explore-rate board player)
  (declare (optimize (speed 3))
           ((simple-array double-float (*)) ev)
           (double-float explore-rate)
           ((integer -1 1) player))
  (assert (zerop (detect-winner board)))
  (if (< (random 1d0 *state*) explore-rate)
      (let ((len 0)
            (move-cands 0))
        (declare ((unsigned-byte 36) move-cands)
                 ((integer 0 9) len))
        (dotimes (i 3)
          (dotimes (j 3)
            (when (zerop (bref board i j))
              (setf (ldb (byte 4 (* 4 len)) move-cands) (pack-move i j))
              (incf len))))
        (with-unpacking-move (i j)
            (ldb (byte 4 (* 4 (random len *state*))) move-cands)
          (values i j)))
      (let ((opt-i nil)
            (opt-j nil)
            (opt-value (if (= player +player1+) -1d0 1d0)))
        (dotimes (i 3)
          (dotimes (j 3)
            (when (zerop (bref board i j))
              (setf (bref board i j) player)
              (when (if (= player +player1+)
                        (<= opt-value (aref ev board))
                        (<= (aref ev board) opt-value))
                (setq opt-value (aref ev board)
                      opt-i i
                      opt-j j))
              (setf (bref board i j) 0))))
        (assert opt-i)
        (values opt-i opt-j))))

(defun simulate-self-play (ev1 ev2 explore-rate1 explore-rate2 &key log-p)
  (declare (optimize (speed 3)))
  (let ((board 0)
        moves)
    (dotimes (turn 9)
      (unless (zerop (detect-winner board))
        (return))
      (let* ((player (if (evenp turn) +player1+ +player2+))
             (ev (if (= player +player1+) ev1 ev2))
             (explore-rate (if (= player +player1+) explore-rate1 explore-rate2)))
        (multiple-value-bind (i j) (solve-with-ev ev explore-rate board player)
          (assert (zerop (bref board i j)))
          (push (pack-move i j) moves)
          (setf (bref board i j) player))
        (when log-p
          (println-board board *standard-output*)
          (terpri *standard-output*))))
    (values board moves)))

(defun simulate-against-solver (ev explore-rate solver ev-player &key log-p)
  (declare (optimize (speed 3))
           (function solver)
           ((integer -1 1) ev-player))
  (let ((board 0)
        moves)
    (dotimes (turn 9)
      (unless (zerop (detect-winner board))
        (return))
      (let* ((player (if (evenp turn) +player1+ +player2+)))
        (multiple-value-bind (i j)
            (if (= player ev-player)
                (solve-with-ev ev explore-rate board player)
                (funcall solver board player))
          (assert (zerop (bref board i j)))
          (push (pack-move i j) moves)
          (setf (bref board i j) player))
        (when log-p
          (println-board board *standard-output*)
          (terpri *standard-output*))))
    (values board moves)))

(defun board-to-line (board)
  (let ((res (make-string 9 :element-type 'base-char :initial-element #\Nul)))
    (dotimes (i 3)
      (dotimes (j 3)
        (setf (aref res (+ (* i 3) j))
              (ecase (bref board i j)
                (0 #\.)
                (1 #\o)
                (-1 #\x)))))
    res))

(defun parse-board (string)
  (assert (= 9 (length string)))
  (let ((board 0))
    (declare (board board))
    (loop for c across string
          for i from 0
          do (setf (ldb (byte 2 (* 2 i)) board)
                   (ecase c
                     (#\o +player1+)
                     (#\x +player2+)
                     (#\. 0))))
    board))

(defun println-board (board &optional (stream *standard-output*))
  (dotimes (i 3)
    (dotimes (j 3 (write-char #\Newline stream))
      (ecase (bref board i j)
        (0 (write-char #\. stream))
        (1 (write-char #\o stream))
        (-1 (write-char #\x stream))))))

(declaim (inline flip-player))
(defun flip-player (player) (- player))

(defun make-init-evaluation ()
  (let ((res (make-array (ash 1 18) :element-type 'double-float :initial-element 0d0)))
    (dotimes (board (length res))
      (setf (aref res board)
            (float (detect-winner board) 1d0)))
    res))

(define-modify-macro maxf (value) max)
(define-modify-macro minf (value) min)

(defmacro dbg (&rest forms)
  (declare (ignorable forms))
  #+swank (if (= (length forms) 1)
              `(format *error-output* "~A => ~A~%" ',(car forms) ,(car forms))
              `(format *error-output* "~A => ~A~%" ',forms `(,,@forms))))

(declaim (inline calc-new-value))
(defun calc-new-value (parent-value child-value step)
  (+ parent-value (* step (- child-value parent-value))))

(declaim ((simple-array double-float (*)) evaluation-vector))
(defun update (evaluation moves board player step)
  (declare (optimize (speed 3))
           (board board)
           ((integer -1 1) player)
           (double-float step)
           ((simple-array double-float (*)) evaluation))
  (let* ((value2 (float (detect-winner board) 1d0))
         (board2
           (let ((board board))
             (loop
               (with-unpacking-move (i j) (car moves)
                 (when (= (bref board i j) player)
                   (return board))
                 (setf (bref board i j) 0)
                 (pop moves)))))
         (board1 (with-unpacking-move (i j) (pop moves)
                   (bset board2 i j 0)))
         (board0 (with-unpacking-move (i j) (pop moves)
                   (bset board1 i j 0))))
    (setf (aref evaluation board2)
          (calc-new-value (aref evaluation board2) value2 step)
          value2 (aref evaluation board2))
    (loop
      (let ((optimal-value2 (if (= player +player1+) -1d0 1d0)))
        (declare (double-float optimal-value2))
        (dotimes (i 3)
          (dotimes (j 3)
            (when (zerop (bref board1 i j))
              (let ((board (bset board1 i j player)))
                (if (= player +player1+)
                    (maxf optimal-value2 (aref evaluation board))
                    (minf optimal-value2 (aref evaluation board)))))))
        (when (= optimal-value2 value2)
          (let* ((old (aref evaluation board0))
                 (new (calc-new-value old value2 step)))
            (setf (aref evaluation board0) new))))
      (unless (cdr moves)
        (return))
      (setq board2 board0
            value2 (aref evaluation board2)
            board1 (with-unpacking-move (i j) (pop moves)
                     (bset board2 i j 0))
            board0 (with-unpacking-move (i j) (pop moves)
                     (bset board1 i j 0))))))

(defun test ()
  (let ((ev (make-init-evaluation))
        (board #b000000001111010101)
        (moves (list (pack-move 0 2) (pack-move 1 1) (pack-move 0 1) (pack-move 1 0) (pack-move 0 0)))
        (player -1))
    (update ev moves board player 0.4d0))
  ;; (let ((ev (make-init-evaluation))
  ;;       (board #b000001111111000101)
  ;;       (moves (list (pack-move 1 2) (pack-move 2 0) (pack-move 1 1) (pack-move 0 1) (pack-move 1 0) (pack-move 0 0)))
  ;;       (player -1))
  ;;   (update ev moves board player 0.4d0))
  )

(defun solve-random (board player)
  (declare (ignore player))
  (let ((len 0)
        (move-cands 0))
    (declare ((unsigned-byte 36) move-cands)
             ((integer 0 9) len))
    (dotimes (i 3)
      (dotimes (j 3)
        (when (zerop (bref board i j))
          (setf (ldb (byte 4 (* 4 len)) move-cands) (pack-move i j))
          (incf len))))
    (assert (not (zerop len)))
    (with-unpacking-move (i j)
        (ldb (byte 4 (* 4 (random len *state*))) move-cands)
      (values i j))))

(defun solve-top-left (board player)
  (declare (ignore player))
  (dotimes (i 3)
    (dotimes (j 3)
      (when (zerop (bref board i j))
        (return-from solve-top-left (values i j)))))
  (assert "Huh?"))

(defun learn-single (init-step explore-rate solver ev-player n)
  (let ((ev (make-init-evaluation)))
    (dotimes (trial n)
      (multiple-value-bind (board moves)
          (simulate-against-solver ev explore-rate solver ev-player)
        (let ((step (/ (* (float init-step 1d0) (- n trial)) n)))
          (update ev moves board ev-player step))))
    ev))

(defun calc-win-rate (ev solver ev-player n-game)
  (let ((win1 0)
        (draw 0)
        (win2 0))
    (dotimes (i n-game)
      (let* ((board (simulate-against-solver ev 0d0 solver ev-player)))
        (ecase (detect-winner board)
          (-1 (incf win2))
          (0 (incf draw))
          (1 (incf win1)))))
    (values (float (/ win1 n-game))
            (float (/ draw n-game))
            (float (/ win2 n-game)))))

(defun learn-double (init-step explore-rate n)
  (let ((ev1 (make-init-evaluation))
        (ev2 (make-init-evaluation)))
    (dotimes (trial n)
      (multiple-value-bind (board moves)
          (simulate-self-play ev1 ev2 explore-rate explore-rate)
        (let ((step (/ (* (float init-step 1d0) (- n trial)) n)))
          (update ev1 moves board +player1+ step)
          (update ev2 moves board +player2+ step))))
    (let ((res (make-array (ash 1 18) :element-type 'double-float :initial-element 0d0)))
      (dotimes (i (length res))
        (setf (aref res i) (if (zerop (aref ev1 i))
                               (aref ev2 i)
                               (aref ev1 i))))
      res)))

(defun calc-rmse-ev (ev)
  (let ((truth (tictactoe:analyze-game-tree))
        (dif 0d0))
	(dotimes (i (length truth))
	  (when (<= -1 (aref truth i) 1)
        (incf dif (expt (- (aref truth i) (aref ev i)) 2))))
	(sqrt dif)))

(defun experiment-self-play ()
  (loop for i from 0 to 100 by 5
        for erate = (/ i 100d0)
        for ev = (learn-double 1.0d0 erate 10000000)
        for rmse = (calc-rmse-ev ev)
        do (format t "~A ~A~%" erate rmse)))

(defun experiment-against-random (ev-player n-game)
  (loop for i from 0 to 100 by 5
        for erate = (/ i 100d0)
        for ev = (learn-single 1.0d0 erate #'solve-random ev-player n-game)
        do (multiple-value-bind (w d l) (calc-win-rate ev #'solve-random ev-player 100000)
             (format t "~2$ ~3$ ~3$ ~3$~%" erate w d l))))
