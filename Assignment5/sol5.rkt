#lang racket
(provide (all-defined-out)) ;; exports the defined variables in this file.

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body)
(struct glet (var e body) #:transparent) ;; a global binding that overrides any local binding (let var = e in body)
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0
(struct num-array  (size) #:transparent)  ;; a number array  (initialized to zeroes), e.g., (num-array-var 10)
                                                     ;; e.g. (num-array 4)

(struct num-array-at   (e1 e2) #:transparent) ;; e1 evaluates to num-array and e2 evaluates to int (index of the value to access) index starts from 0
                                              ;; (num-array-at (num-array 4) 3)
                                              ;; (num-array-at (num-array 4) 4) ;  this should give a nice error messaeg (like "array access out of bound")
                                              ;; (num-array-at (num-array 4) -1) ;  this should give a nice error messaeg (like "array access out of bound")

(struct num-array-set  (e1 e2 e3) #:transparent) ;; e1 evaluates to num-array-var, e2 evaluates to int (index of the value to access), and e3 evaluates to a number 
                                              ;; (num-array-set (num-array 4) 0 42)
                                              ;; (num-array-set (num-array 4) 5 42) ; this should give a nice error messaeg (like "array access out of bound")
                                              ;; (num-array-set (num-array 4) -1 42) ; this should give a nice error messaeg (like "array access out of bound")

(define (num-array-object? v) ;; hackish implementation of testing num-array object. We assume that if a value is mpair, it is a num-array object.
  (mpair? v))

(define (array-length array)
  (if (eq? (mcdr array) null)
      1
      (+ 1 (array-length (mcdr array)))))
(define (make-array-object length)  
    (if (= length 0)
        null
        (mcons (int 0) (make-array-object (- length 1)))))
(define (set-array-val array index val)
  (if (= index 0)
      (set-mcar! array val)
      (set-array-val (mcdr array) (- index 1) val)))
(define (array-at array index)
  (if (= index 0)
      (mcar array)
      (array-at (mcdr array) (- index 1))))

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

;; CHANGE (put your solutions here)
(define (racketlist->mupllist rlist)
  (if (null? rlist)
      (aunit)
      (apair (car rlist) (racketlist->mupllist (cdr rlist)))))

(define (mupllist->racketlist mlist)
  (if (aunit? mlist)
      null
      (cons (apair-e1 mlist) (mupllist->racketlist (apair-e2 mlist)))))

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))
           
;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.

(define (concat-list a b)
  (if (null? a)
      b
      (cons (car a) (concat-list (cdr a) b))))

(define (eval-under-env-and-genv e env genv)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env-and-genv (add-e1 e) env genv)]
               [v2 (eval-under-env-and-genv (add-e2 e) env genv)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; CHANGE add more cases here
        [(int? e) e]
        [(fun? e) (closure env e)]
        [(ifgreater? e)
         (let ([v1 (eval-under-env-and-genv (ifgreater-e1 e) env genv)]
               [v2 (eval-under-env-and-genv (ifgreater-e2 e) env genv)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env-and-genv (ifgreater-e3 e) env genv)
                   (eval-under-env-and-genv (ifgreater-e4 e) env genv))
               (error "MUPL ifgreater applied to non-number")))]
        [(closure? e) e]
        [(mlet? e)
         (letrec ([v (eval-under-env-and-genv (mlet-e e) env genv)]
                  [newenv (cons (cons (mlet-var e) v) env)])
           (eval-under-env-and-genv (mlet-body e) newenv genv))]
        [(glet? e)
         (letrec ([v (eval-under-env-and-genv (glet-e e) env genv)]
                  [newenv (cons (cons (glet-var e) v) env)]
                  [newgenv (cons (cons (glet-var e) v) genv)])
           (eval-under-env-and-genv (glet-body e) newenv newgenv))]
        [(call? e)
         (let ([v1 (eval-under-env-and-genv (call-funexp e) env genv)]
               [v2 (eval-under-env-and-genv (call-actual e) env genv)])
           (if (closure? v1)
               (letrec ([clfun (closure-fun v1)]
                        [clenv (concat-list genv (closure-env v1))]
                        [funarg (cons (fun-formal clfun) v2)]
                        [funmap (cons (fun-nameopt clfun) v1)]
                        [funbody (fun-body clfun)])
                 (if (eq? (fun-nameopt clfun) #f)
                     (eval-under-env-and-genv funbody (cons funarg clenv) genv)
                     (eval-under-env-and-genv funbody (cons funmap (cons funarg clenv)) genv)))

               (error "MUPL cannot call non-closure function")))]
        [(apair? e)
         (apair (eval-under-env-and-genv (apair-e1 e) env genv) (eval-under-env-and-genv (apair-e2 e) env genv))]
        [(fst? e)
         (let ([v (eval-under-env-and-genv (fst-e e) env genv)])
           (if (apair? v)
               (apair-e1 v)
               (error "MUPL fst cannot evaluate non-apair value")))]
        [(snd? e)
         (let ([v (eval-under-env-and-genv (snd-e e) env genv)])
           (if (apair? v)
               (apair-e2 v)
               (error "MUPL snd cannot evaluate non-apair value")))]
        [(isaunit? e)
         (let ([v (eval-under-env-and-genv (isaunit-e e) env genv)])
           (if (aunit? v)
               (int 1)
               (int 0)))]
        [(aunit? e) e]
        [(num-array? e)
         (if (exact-nonnegative-integer? (num-array-size e)) (make-array-object (num-array-size e)) (error "Wrong Size"))]
        [(num-array-at? e)
         (let ([array (eval-under-env-and-genv (num-array-at-e1 e) env genv)]
               [idx (num-array-at-e2 e)])
           (cond [(not (num-array-object? array)) (error "MUPL array search cannot be applied non-array value")]
                 [(< idx 0) (error "Array index out of bound error")]
                 [(null? array) (error "Array index out of bound error")]
                 [(>= idx (array-length array)) (error "Array index out of bound error")]
                 [#t (array-at array)]))]
         [(num-array-set? e)
         (let ([array (eval-under-env-and-genv (num-array-set-e1 e) env genv)]
               [idx (num-array-set-e2 e)]
               [val (eval-under-env-and-genv (num-array-set-e3 e) env genv)])
          (cond [(not (num-array-object? array)) (error "MUPL array mutation cannot be applied non-array value")]
                [(not (int? val)) (error "MUPL array cannot include non-int value")]
                [(< idx 0) (error "Array index out of bound error")]
                [(null? array) (error "Array index out of bound error")]
                [(>= idx (array-length array)) (error "Array index out of bound error")]
                [#t (let ([a (set-array-val array idx val)])
                    val)]))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; CHANGE add more cases here
        [(int? e) e]
        [(fun? e) (closure env e)]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-number")))]
        [(closure? e) e]
        [(mlet? e)
         (letrec ([v (eval-under-env (mlet-e e) env)]
                  [newenv (cons (cons (mlet-var e) v) env)])
           (eval-under-env (mlet-body e) newenv))]
        [(glet? e)
         (letrec ([v (eval-under-env (glet-e e) env)]
                  [newenv (cons (cons (glet-var e) v) env)]
                  [genv (cons (cons (glet-var e) v) null)])
           (eval-under-env-and-genv (glet-body e) newenv genv))]
        [(call? e)
         (let ([v1 (eval-under-env (call-funexp e) env)]
               [v2 (eval-under-env (call-actual e) env)])
           (if (closure? v1)
               (letrec ([clfun (closure-fun v1)]
                        [clenv (closure-env v1)]
                        [funarg (cons (fun-formal clfun) v2)]
                        [funmap (cons (fun-nameopt clfun) v1)]
                        [funbody (fun-body clfun)])
                 (if (eq? (fun-nameopt clfun) #f)
                     (eval-under-env funbody (cons funarg clenv))
                     (eval-under-env funbody (cons funmap (cons funarg clenv)))))

               (error "MUPL cannot call non-closure function")))]
        [(apair? e)
         (apair (eval-under-env (apair-e1 e) env) (eval-under-env (apair-e2 e) env))]
        [(fst? e)
         (let ([v (eval-under-env (fst-e e) env)])
           (if (apair? v)
               (apair-e1 v)
               (error "MUPL fst cannot evaluate non-apair value")))]
        [(snd? e)
         (let ([v (eval-under-env (snd-e e) env)])
           (if (apair? v)
               (apair-e2 v)
               (error "MUPL snd cannot evaluate non-apair value")))]
        [(isaunit? e)
         (let ([v (eval-under-env (isaunit-e e) env)])
           (if (aunit? v)
               (int 1)
               (int 0)))]
        [(aunit? e) e]
        [(num-array? e)
         (if (exact-nonnegative-integer? (num-array-size e)) (make-array-object (num-array-size e)) (error "Wrong Size"))]
        [(num-array-at? e)
         (let ([array (eval-under-env (num-array-at-e1 e) env)]
               [idx (num-array-at-e2 e)])
           (cond [(not (num-array-object? array)) (error "MUPL array search cannot be applied non-array value")]
                 [(< idx 0) (error "Array index out of bound error")]
                 [(null? array) (error "Array index out of bound error")]
                 [(>= idx (array-length array)) (error "Array index out of bound error")]
                 [#t (array-at array idx)]))]
        [(num-array-set? e)
         (let ([array (eval-under-env (num-array-set-e1 e) env)]
               [idx (num-array-set-e2 e)]
               [val (eval-under-env (num-array-set-e3 e) env)])
          (cond [(not (num-array-object? array)) (error "MUPL array mutation cannot be applied non-array value")]
                [(not (int? val)) (error "MUPL array cannot include non-int value")]
                [(< idx 0) (error "Array index out of bound error")]
                [(null? array) (error "Array index out of bound error")]
                [(>= idx (array-length array)) (error "Array index out of bound error")]
                [#t (let ([a (set-array-val array idx val)])
                    val)]))]        
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3) (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (let ([nowmap (car lstlst)])
        (mlet (car nowmap) (cdr nowmap) (mlet* (cdr lstlst) e2)))))

(define (ifeq e1 e2 e3 e4)
  (mlet "_x" e1
        (mlet "_y" e2
              (ifgreater (var "_x") (var "_y") e4
                         (ifgreater (var "_y") (var "_x") e4 e3)))))

;; Problem 4

(define mupl-map
  (fun #f "func"
       (fun "muplmap" "mupllist"
            (ifaunit (var "mupllist")
                     (aunit)
                     (apair (call (var "func") (fst (var "mupllist"))) (call (var "muplmap") (snd (var "mupllist"))))))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "I"
             (call (var "map") (fun #f "n" (add (var "n") (var "I")))))))


(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make grading
;; more difficult, so copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
