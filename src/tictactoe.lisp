(defpackage :tictactoe
  (:use :cl)
  (:export #:board #:bref #:solve-top-left #:simulate #:analyze-game-tree #:valid-board-p))

(in-package :tictactoe)

(defconstant +player1+ 1)
(defconstant +player2+ 2)
(defconstant +draw+ 3)

(deftype board () '(unsigned-byte 18))

(declaim (inline get-index))
(defun get-index (i j)
  (* 2 (+ (* i 3) j)))

(declaim (inline bref))
(defun bref (board i j)
  (declare (board board)
           ((unsigned-byte 2) i j))
  (ldb (byte 2 (get-index i j)) board))

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
              `(let ((,(car stores) (dpb ,store (byte 2 (get-index ,i-tmp ,j-tmp)) ,access-form)))
                 ,store-form
                 ,store)
              `(bref ,access-form ,i ,j)))))

(declaim (ftype (function * (values (unsigned-byte 2) &optional)) detect-winner))
(defun detect-winner (board)
  "Returns a winner or indicates a draw if the game is over, otherwise returns
0. Note that this function may return any value if BOARD is not valid."
  (declare (optimize (speed 3))
           (board board))
  (loop for p from 1 to 2
        when (or (= p (bref board 0 0) (bref board 0 1) (bref board 0 2))
                 (= p (bref board 1 0) (bref board 1 1) (bref board 1 2))
                 (= p (bref board 2 0) (bref board 2 1) (bref board 2 2))
                 (= p (bref board 0 0) (bref board 1 0) (bref board 2 0))
                 (= p (bref board 0 1) (bref board 1 1) (bref board 2 1))
                 (= p (bref board 0 2) (bref board 1 2) (bref board 2 2))
                 (= p (bref board 0 0) (bref board 1 1) (bref board 2 2))
                 (= p (bref board 0 2) (bref board 1 1) (bref board 2 0)))
        do (return-from detect-winner p))
  (dotimes (i 3)
    (dotimes (j 3)
      (when (zerop (bref board i j))
        (return-from detect-winner 0))))
  +draw+)

(defun consist-of-legal-mark-p (board)
  (declare (optimize (speed 3))
           (board board))
  (dotimes (i 3)
    (dotimes (j 3)
      (when (= 3 (bref board i j))
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
          (2 (incf p2)))))
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
  (let ((res (make-array (ash 1 18) :element-type '(unsigned-byte 2) :initial-element 0)))
    (labels ((recur (depth board)
               (declare (board board)
                        (fixnum depth))
               (assert (<= depth 9))
               (cond ((not (zerop (aref res board))) board)
                     ((not (zerop (detect-winner board)))
                      (setf (aref res board) (detect-winner board)))
                     (t (let ((player (if (evenp depth) 1 2))
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
                          (setf (aref res board)
                                (cond (win-p player)
                                      (draw-p +draw+)
                                      (t (flip-player player)))))))))
      (recur 0 0))
    res))

(defun println-all-positions (&optional (stream *standard-output*))
  (let ((evaluation (analyze-game-tree))
        (count 0))
    (dotimes (board (ash 1 18))
      (unless (zerop (aref evaluation board))
        (uiop:println (aref evaluation board) stream)
        (println-board board)
        (incf count)))
    count))

(defun simulate (solver1 solver2)
  (let ((board 0))
    (loop (multiple-value-bind (i j) (funcall solver1 board 1)
            (assert (zerop (bref board i j)))
            (setf (bref board i j) 1))
          (when (= 1 (aref *winner-vector* board))
            (return (values 1 board)))
          (when (= 3 (aref *winner-vector* board))
            (return (values 3 board)))
          (assert (= 0 (aref *winner-vector* board)))
          (multiple-value-bind (i j) (funcall solver2 board 2)
            (assert (zerop (bref board i j)))
            (setf (bref board i j) 2))
          (when (= 2 (aref *winner-vector* board))
            (return (values 2 board)))
          (assert (= 0 (aref *winner-vector* board))))))

(defun println-board (board &optional (stream *standard-output*))
  (dotimes (i 3)
    (dotimes (j 3 (write-char #\Newline stream))
      (ecase (bref board i j)
        (0 (write-char #\. stream))
        (1 (write-char #\o stream))
        (2 (write-char #\x stream)))))
  (write-char #\Newline stream))

(declaim (inline flip-player))
(defun flip-player (player) (logxor #b11 player))

(defun solve-top-left (board player)
  (declare (ignore player))
  (dotimes (i 3)
    (dotimes (j 3)
      (when (zerop (bref board i j))
        (return-from solve-top-left (values i j)))))
  (assert "Huh?"))

