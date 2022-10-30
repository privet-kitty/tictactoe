(uiop:define-package tictactoe/test
  (:use :cl :tictactoe :fiveam)
  (:export #:main-suite)
  (:import-from :tictactoe #:get-index #:+unknown+))

(in-package :tictactoe/test)

;; NOTE: To run this test file, execute `(asdf:test-system :tictactoe)' in your Lisp.

(def-suite main-suite)
(in-suite main-suite)

(test get-index
  (is (= (get-index 0 1) 2)))

(test bref
  (let ((vec (vector #b1011)))
    (is (= (bref (aref vec 0) 0 0) -1))
    (setf (bref (aref vec 0) 1 0) #b10)
    (is (= (aref vec 0) #b10001011))
    (setf (logbitp 1 (bref (aref vec 0) 1 1)) t)
    (is (= (aref vec 0) #b1010001011))
    (setf (bref (aref vec 0) 0 1) 0)
    (is (= (aref vec 0) #b1010000011))
    (is (= 0 (bref (aref vec 0) 0 2)))
    (is (= -2 (bref (aref vec 0) 1 0)))
    (setf (bref (aref vec 0) 0 2) -1)
    (is (= (aref vec 0) #b1010110011))
    (setf (bref (aref vec 0) 0 2) -2)
    (is (= (aref vec 0) #b1010100011))))

(test validity
  (let ((res (analyze-game-tree))
        (*test-dribble* nil))
    (loop for board below (ash 1 18)
          when (valid-board-p board)
          do (is (<= -1 (aref res board) 1))
          else
          do (is (= +unknown+ (aref res board))))
    ;; number of all possible positions in tic-tac-toe
    (is (= 5478 (loop for i below (length res)
                      count (/= +unknown+ (aref res i)))))))
