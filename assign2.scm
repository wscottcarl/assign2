; go through and redo any while loops as recursion

(define (author)
   (println "AUTHOR: William Scott Carl wscarl@crimson.ua.edu")
   )

(define (exprTest # $expr target)
   (define result (catch (eval $expr #)))
   (if (error? result)
      (println $expr " is EXCEPTION: "(result'value)
         " (it should be " target ")")
      (println $expr " is " result
         " (it should be " target ")")
      )
   )
   
(define (iterator # $x items $)
   (define lamList (eval (lambda ($x) $) #))
   (map lamList items)
   )

(define (peval f @)
   ; if 'MISSING is not in @ 
   ; then return lambda function
   ; else use lambda to collect another list of variables
   ; then replace each 'MISSING with corresponding var from new list
   ; and generate a lambda
   ; and finally evaluate the generated lambda function
   (define p @)
   (lambda (@)
      (define lst (cons nil nil))
   ; old version
;      (while (and (valid? p) (valid? @))
;         (if (eq? (car p) 'MISSING)
;            ; add car of @ to lst
;            (define lst (append lst (cons (car @) nil)))
;            ; else add car of p to lst
;            (define lst (append lst (cons (car p) nil)))
;            )
;         (define p (cdr p))
;         (define @ (cdr @))
;         )
   ; new version
      (define (constLst p @ ls)
;         (print lst)
;         (pause)
         (if (eq? (car p) 'MISSING)
            ; add car of @ to lst
            (define ls (append ls (cons (car @) nil)))
            ; else add car of p to lst
            (define ls (append ls (cons (car p) nil)))
            )
         (if (and (valid? (cdr p)) (valid? (cdr @))) 
            (constLst (cdr p) (cdr @))
            lst
            )
         )
   ; old version
;      (while (valid? p)
;         (define lst (append lst (cons (car p) nil)))
;         (define p (cdr p))
;         )
   ; new version
      (define (finishLst p ls)
         (define ls (append ls (cons (car p) nil)))
         (if (valid? (cdr p)) 
            (finishList (cdr p)))
            lst
         )
;      (print p)
;      (print @)
;      (print lst)
;      (pause)
      (define lst (constLst p @ lst))
;      (print lst)
;      (print p)
;      (print @)
      (define lst (finishLst p lst))
      (define lst (cdr lst))
      (apply f lst)
      )
   )



(define (no-locals code)
   (define useCode (cddr code))
   (define defs (cons nil nil))
   (define lamb (cons nil nil))
   (define final (cons "(" nil))
   (define final (append final (cons (car code) nil)))
   (define final (append final (cons (cadr code) nil)))

   ; given an executable line, find what variables have local
   ; defines and return a list of variable names
   (define (getParams exec defs)
      
      )
   ; given an executable line, find what variables have local 
   ; defines and return a list of variable definitions
   (define (getParamDefs exec)
      
      )
   ; at this point the function name and params have been stored
   ; (define (nsq a) 

   ; create a definition list
   (while (eq? (car (car useCode)) 'define)
      (define defs (append defs (cons (car useCode) nil)))
      (define useCode (cdr useCode))
      )

   ; create an execution list
   (while (valid? (car useCode))
      (define execs (append execs (cons (car useCode) nil)))
      (define useCode (cdr useCode))
      )

   ; restructure each command as a lambda
   ; ((lambda (params) (exec)) defParams)
   (while (valid? execs)
      (define lam (cons "((lambda " nil))
      (define lam (append (lam (cons (getParams (car execs)) nil))))
      (define lam (append (lam (cons (car execs) nil))))
      (define lam (append (lam (cons ") " nil))))
      (define lam (append (lam (getParamDefs (car execs)) nil)))
      (define execs (cdr execs))
      )
   
   ; move all lambdas into final code string
   (while (valid? lamb)
      (define final (append final (cons (car lamb) nil)))
      (define lamb (cdr lamb))
      )
   )

;(define (run1)
;   (exprTest ())
;   )

(define (run2)
    (define (f x y) (+ x y))
    (define x 1)
    (define y 2)
    (define . 'MISSING)
    (exprTest ((peval f 1 2)) 3)
    (exprTest ((peval f . 2) 1) 3)
    (exprTest ((peval f . .) 1 2) 3)
    )

;(define (run3)
;   
;   )

(define (run4)
   
   )


;(run1)
(run2)
;(run4)
