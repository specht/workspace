(defun bubble-sort (arr)
  (let ((n (length arr)))
    (loop for i from 0 below (- n 1)
          do (loop for j from 0 below (- n i 1)
                   do (when (> (aref arr j) (aref arr (1+ j)))
                        (rotatef (aref arr j) (aref arr (1+ j)))))))
  arr)

(defun main ()
  (let ((arr (make-array 10 :element-type 'integer))
        (random-state (make-random-state t))) ; Initialize random state with a seed
    (loop for i from 0 below 10
          do (setf (aref arr i) (random 100 random-state))) ; Use the random state
    (format t "Original array: ~a~%" arr)
    (format t "Sorted array: ~a~%" (bubble-sort arr))))

(main)