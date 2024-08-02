(defun bubble-sort (sequence &optional (compare #'<))
  "sort a sequence (array or list) in place with an optional comparison
function (cl:< is the default)"
  (loop with sorted = nil until sorted do
    (setf sorted t)
    ;; assume the list is sorted before checking it
    (loop for a below (1- (length sequence)) do
      ;; loop through the sequence
      (unless (funcall compare
                       ;; use 'compare' function to check seq[a] vs. seq[a+1]
                       (elt sequence a)
                       (elt sequence (1+ a)))
        ;; if 'compare' function is unfulfilled, rotate [a] and [a+1]
        (rotatef (elt sequence a)
                 (elt sequence (1+ a)))
        ;; and of course this means the list is not yet sorted
        (setf sorted nil)))))

(let ((list
        ;; create a local scope in which 'list' is bound to the following
        ;; definition
        (mapcar #'random
                ;; map the function 'random' to our ten-element list
                ;; this creates random values between 0 and 100
                (make-list 10 :initial-element 100))))
  (format t "Original list: ~a~%" list)
  (bubble-sort list)
  ;; list is now sorted
  (format t "Sorted list: ~a~%" list))
