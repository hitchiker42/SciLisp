(load "test_common.lisp")
(setq ls '(1 3 5 7 9 11 13));sanity test
(setq ls-2 (list ?a ?b "c" "d" ls '(1 . 2)));test list evaluates its args
(setq ls-3 (iota 10))
(setq ls-4 (iota -10 10 0.1))
(setq ls-5 (rand-list 10))
(setq ls-6 (rand-list 10 :real64))
(setq ls-7 (iota -10 10 1))
(let ((temp (rand-list 10)))
  (assert-equal (qsort rand-list >) (merge-sort rand-list >)))
(setq pair '(1 . 2))
(assert-equal pair (cons 1 2))
(assert-equal 0 (apply '+ ls-7))
(print (cons ls ls-2))
(setq ls-copy ls)
(assert-eq ls ls-copy)
(assert-equal ls ls-copy)
(seq ls-copy (copy-cons ls))
(assert (not (eq ls ls-copy)))
(assert-equal ls ls-copy)
(assert (is-sorted (qsort ls-5)))
