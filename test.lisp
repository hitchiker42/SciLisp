(def ls '(1 23 34 56 78))
(def ls2 (iota 10))
(def ls3 (iota -1 1 0.1))
(def arr [1 2 3 4 5 7 8 9 0])
(def arr2 [1.0 4.0 8.9 3 0.9])
(defun sum (x)(reduce x +))
(define prod (lambda (x) (reduce x +)))
(sum ls);issue here
(print ls)
(sum ls2);and here 
(prod ls3);and here
(car ls)
(cdr ls2)
(cdddr ls3)
(caddr 1s)
(print (cons ls ls2))
(def ls4 (cons 4 ls3))
(def ls5 (array->list arr))
(+ 1 2)
(+ 1.9 2)
(- 2 3 )
(- 3.4 88)
(cons 3 3)
(cdr (cons 83 9))
(log 388.8)
(cos pi)
(sin pi)
(tan pi)
(exp e)
(expt 4 4)
(expt e Meps)
(abs -21.9)
(abs 23)
(abs 3.9)
(abs -339)
(mod 9 4)
(mod 994.9 2)
(mod 39.88 8.8)
(ash 344 4)
(ash 344 -4)
(lrand)
(drand)
(drand 1000)
(length ls)
(length arr)
(consp arr)
(cons? ls4)
(cons? '(3 3 4 5 5))
(nilp (car arr))
(nilp ls3)
(setq i 10)
(while (> i 0)
  (setq i (- i 1)))
(if (numberp 33)
    (progn (print "pass")
           (if (numberp ls)
               (print "fail")
               (print "pass")))
    (print "fail"))
;issue here
(if (ash 99 -4)
    (prog1 
        99
      (+ 3 9))
    (prog1
        88
      (+ 2 8)))
