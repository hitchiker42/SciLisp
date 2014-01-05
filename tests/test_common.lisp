(defmacro assert-eq (x y) `(assert (eq ,x ,y)))
(defmacro assert-equal (x y) `(assert (equal ,x ,y)))
(defmacro assert-!eq (x y) `(assert (not (eq ,x ,y))))
(defmacro assert-!equal (x y) `(assert (not (equal ,x ,y))))
(defmacro assert-fp-equal (x y)
  `(assert (> Meps (abs-diff ,x ,y))))
(defmacro assert-error (expr)
  `(assert (errorp ,expr)))
#|the fact this function works is kinda awesome because it's so much
 |in the spirit of lisp, it uses a recursive higher order function that
 |is locally bound and uses a variable from it's enclosing environment
 |also I shadow a variable and it works out fine|#
(defun is-sorted (ls f)
  (flet ((acc (l ls)
           (if (nilp ls)
               #t
               (if (f l (car ls))
                   (acc (car ls) (cdr ls))
                   #f))))
    (acc (car ls) (cdr ls))))
(defun twos-compliment (x) (++ (lognot x)))
