;-*- mode: SciLisp; -*-
#|*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************|#
#|SciLisp regression tests|#
#|definations|#
(defun fact-acc (acc n) (if (<= n 1) acc (fact-acc (* n acc) (-- n))))
(defun sum (x)(reduce x +))
(defun is-even (n) (if (= 0 n) #t (is-odd (-- n))))
(defun is-odd (n) (if (= 0 n) #f (is-even (-- n))))
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
(define prod (lambda (x) (reduce x *)))
(def ls '(1 23 34 56 78))
(def ls2 (iota 10))
(def ls3 (iota -1 1 0.1))
(def arr [1 2 3 4 5 7 8 9 0])
(def arr2 [1.0 4.0 8.9 3 0.9])
(setq arr3 [0 99 0.9 9e88 3.9 0x88])
(def arr4 [0 9 3 4 2 5 7 4 9 2 1 10 22 5])
(def arr-iota (iota 0 10 1 1))
(let ((x 5))
  (defun add5 (y) (+ x y)))
#|tests of functions|#
(assert-eq (sum ls) 192)
(assert-equal '(1 23 34 56 78) ls)
(print ls)
(sum ls2)
(prod ls3)
(car ls)
(cdr ls2)
(cdddr ls3)
(caddr ls)
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
(add (bigfloat "1e77") (bigint "1003847") 83 11.0 00.1 99)
(sub (bigfloat "1e999") (bigint "10294857") 884 11.0 110.9 93.9)
(lrand)
(drand)
(drand 1000)
(aref arr-iota 4)
(length ls)
(length arr)
(consp arr)
(add5 6)
(nilp (car arr))
(nilp ls3)
(ccall "gsl_sf_fact" "libgsl" :real64 '(:uint32) '(4))
(ccall "gsl_sf_poch" "libgsl" :real64 '(:real64 :real64) '(4. 5.))
(fact-acc 1 4)
(fact-acc 1.0 15.0)
(if (eq 0.9 (aref arr3 2))
    #t
    (raise-error "aref failure"))
(let ((error-message '(raise-error "expected error")))
  (if (not (errorp error-message))
      error-message
      nil))
;(cons? ls4)
;(cons? '(3 3 4 5 5))

;(setq copyright-literal ?©)
(setq utf8-copyright ?\u00a9)
(print ?\?)
(print ?\x3f)
#|(print copyright-literal)|#
(print utf8-copyright)
(pwd)
(system "ls")
(system "touch temp.temp")
(def file (fopen "temp.temp" "w"))
(fputs "Hello, World!
" file)
(fclose file)
(system "cat temp.temp")
(system "rm temp.temp")
(array-qsort arr4 <)
(print arr4)
(array-qsort arr4 > #t)
(print arr4)
#|tests of special forms|#
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
(def i 15)
(print i)
(do (i 0 (++ i) (<= i 10)) (print i))
(print i)
(dolist (i '(1 2 3 4 5 6 7 8 9 10)) (print i))
(let ((x 5)) (print x))
(let ((x '(1 3 5))) (print x))
(let ((x 5)
      (y 6))
  (progn
    (print x)
    (print y)))
(and (print "should print")
     (print "should print") #f
     (print "shouldn't print"))
(or (progn (print "should print") #f)
    (print "should print")
    (print "shouldn't print"))
(is-even 17)
(is-odd 17)
(is-even 8)
(is-odd 8)
(qsort '(1 4 2 9 3 8 4 0 274 49 1 929 99 2 37 4 82 3 8 39 2 39 9) >)
(qsort '(1 4 2 9 3 8 4 0 274 49 1 929 99 2 37 4 82 3 8 39 2 39 9) <=)
(def re-test1 (re-compile "\(real\([0-9]*\)\)\|\(int\([0-9]*\)\)"))
(def re-test2 (re-compile "\(const\)?[[:space:]]*char\(\*\)?"))
(def match1 (re-match re-test1 "real99"))
(print match1)
(if match1
    (print (re-subexpr match1 1))
    nil)
(def match2 (re-match re-test1 "int88"))
(if match2 (print (re-subexpr match2 4))nil)
(defmacro make-addr (name num)
  `(defun ,name (x)(+ ,num x)))
(defmacro make-addr2 (name num1 num2)
  `(defun ,name (x)(add ,num1 ,num2 x)))
(def thirty-three 33)
(make-addr add33 thirty-three)
(assert-eq (add33 22) 55)
(make-addr add2 2)
(assert-eq (add2 2) 4)
(make-addr add44 44)
(assert-eq (add44 94) 138)
(make-addr2 add22-and-44 22 44)
(assert-eq 132 (add22-and-44 66))
(defmacro ++! (x) `(setq ,x (++ ,x)))
(defmacro --! (x) `(setq ,x (-- ,x)))
(def x 0)
(++ x)
(assert-eq x 0)
(++! x)
(assert-eq x 1)
(-- x)
(assert-eq x 1)
(--! x)
(assert-eq x 0)
(let ((ls (list-mergesort (rand-list 20) >)))
  (assert (is-sorted ls >)))
(let ((arr (rand-array 100))
      (ls (rand-list 100)))
  (progn
    (assert-equal arr (list->array (array->list arr)))
    (assert-equal ls (array->list (list->array ls)))
    (assert-equal (list-qsort ls >) (list-mergesort ls >))))
(defmacro when (cond-test &rest args) `(if ,cond-test (progn ,@args) nil))
(when (> 2 1)
  (print "testing when")
  (print "still testing")
  (print "done testing"))
;; Local Variables:
;; mode: SciLisp
;; End:
