; volkan ozturk
; 2019400033
; compiling: yes
; complete: yes
#lang racket

(provide (all-defined-out))

; 10 points
(define := (lambda (var value) (list var value)))

; 10 points
(define -- (lambda args
  (list 'let (foldr cons '() args))
  )
)

; 10 points
(define @ (lambda (bindings expr) (append bindings expr)
(if (and (list? expr) (equal? (length expr) 1)) (append bindings expr)
(append bindings (list expr))

)
))

; 20 points
(define split_at_delim1 (lambda (delim args)
(cond ((equal? '() args) '(()) )
      ((equal? delim (car args)) (cons '() (split_at_delim1 delim (cdr args))))
      (else (let ([v (split_at_delim1 delim (cdr args))]) (cons (cons (car args) (car v)) (cdr v)) ) )
)                      
                          
))
(define erase_parantheses (lambda (param)
(if (and (list? (car param)) (equal? (cdr param) '()) ) (erase_parantheses (car param))
param
)
))

(define split_at_delim (lambda (delim args)
(remove* '(()) (split_at_delim1 delim args))
))
; 30 points
(define parse_expr (lambda (expr)
(let ([vexpr (erase_parantheses expr)])                   
(cond ((not (equal? (car (split_at_delim '+ vexpr)) vexpr)) (cons '+ (map parse_expr (split_at_delim '+ vexpr))))
      ((not (equal? (car (split_at_delim '* vexpr)) vexpr)) (cons '* (map parse_expr (split_at_delim '* vexpr))))
      ((not (equal? (car (split_at_delim '@ vexpr)) vexpr))   (apply @ (map parse_expr (split_at_delim '@ vexpr))))
      ((not (equal? (car (split_at_delim '-- vexpr)) vexpr))  (apply -- (map parse_expr (split_at_delim '-- vexpr))))
      ((not (equal? (car (split_at_delim ':= vexpr)) vexpr)) (eval (cons ':= (map parse_expr (split_at_delim ':= vexpr)))))
      ((number? (car vexpr)) (car vexpr))
      ((equal? 'quote (car vexpr)) vexpr)
      (else (car vexpr))
)
)
))
; 20 points
(define eval_expr (lambda (expr)
(eval (parse_expr expr))
))







