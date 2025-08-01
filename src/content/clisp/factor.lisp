(defun prime-factors (number &optional (return-values '()))
  ;; function declaration has an optional return-values parameter for
  ;; tail-recursive use
  (when (> number 0)
    ;; no prime factors for negative numbers
    (loop with max-divisor = (isqrt number)
          ;; loop through possible divisors
          ;; stop at divisor = square root of number
          for divisor = 2
            then (if (evenp divisor)
                     ;; if divisor is even, increment
                     ;; if not, add two
                     (1+ divisor) (+ divisor 2))
          do (cond ((> divisor max-divisor)
                    ;; have we checked all possible divisors but none fit?
                    ;; number is prime => return with any return-values
                    (return (cons (list number 1) return-values)))
                   ((zerop (rem number divisor))
                    ;; is number evenly divisible by divisor?
                    ;; we have found a factor of the number.
                    ;; recursively call prime-factors
                    (return
                      (prime-factors
                       ;; use number/divisor as new number
                       (truncate number divisor)
                       ;; add factor to return-values list (or increment
                       ;; counter if more sensible)
                       (if (eq divisor (caar return-values))
                           (cons 
                            (list (caar return-values) (1+ (cadar return-values)))
                            (cdr return-values))
                           (cons (list divisor 1) return-values)))))))))

(format T "Please enter the number to factorise: ~%")
(format T "Prime factors: ~d~%" (prime-factors (read)))