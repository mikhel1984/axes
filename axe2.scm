;;; S.Mikhel, 2016 - camis.dat@gmail.com
;;; Проверено в Guile

;;;=============== Описание ===================

;;; Данная программа предназначена для вычисления элементов
;;; матрицы преобразования от предмета к изображению (ABCD).
;;; Матрица формируется на основе списка элементов, задаваемых 
;;; пользователем. Данные могут быть представлены как в 
;;; численном виде, так и символьно.

;;;=========  Математические операции =========

;;;    a + b
(define (f+ a b)
   (cond 
      ((and (number? a) (number? b)) (+ a b))      
      ((and (number? a) (= a 0)) b)
      ((and (number? b) (= b 0)) a)
      (else (list '+ a b)))) 
;;;   a* b	       
(define (f* a b)
   (cond 
      ((and (number? a) (number? b)) (* a b))      	
      ((and (number? a) (= a 1)) b)
      ((and (number? a) (= a 0)) 0)
      ((and (number? b) (= b 1)) a)
      ((and (number? b) (= b 0)) 0)      
      (else (list '* a b))))
;;;   a - b	       
(define (f- a b)
   (cond 
      ((and (number? a) (number? b)) (- a b))      
      ((and (number? b) (= b 0)) a)
      ((mat-eqv? a b) 0)
      (else (list '- a b))))
;;;   a / b	       
(define (f/ a b)
   (cond 
      ((eqv? b 0) 'ZERO_DIVIDE)
      ((and (number? a) (number? b)) (/ a b))      
      ((and (number? a) (= a 0)) 0)
      ((and (number? b) (= b 1)) a)
      ((mat-eqv? a b) 1)
      (else (list '/ a b))))
      
;;;======= Сравнение математических выражений =========
;;; Выражения записываются с двумя аргументами, например
;;; a + b + c -> (+ a (+ b c))
;;;-----------------------------------------------------

;;;  does mathematical expressions equal?
(define (mat-eqv? a b)
   (if (list? a)
      (if (list? b)
          (expr-eqv? a b)
	  #f)
      (eqv? a b))) 
;;;   expr1 == expr2	       
(define (expr-eqv? e1 e2)
   (if (eqv? (car e1) (car e2))     
      (if (direct-eqv? (cdr e1) (cdr e2))
         #t
	 (and (member (car e1) '(+ *))
	          (reverse-eqv? (cdr e1) (cdr e2))))
      #f))
;;;   (a b) == (a b)       
(define (direct-eqv? a b)
   (and (mat-eqv? (car a) (car b))
            (mat-eqv? (cadr a) (cadr b))))
;;;   (a b) == (b a)     
(define (reverse-eqv? a b)
   (and (mat-eqv? (car a) (cadr b))
            (mat-eqv? (car b) (cadr a))))

;;;==================== Матрицы =====================
;;; Матрицы 2x2, записываются по строкам:
;;; (a b c d) -> (a b
;;;               c d)
;;;--------------------------------------------------

;;;   (a b c d) -> a
(define-macro (x00 m)
   `(car ,m))
;;;   (a b c d) -> b    
(define-macro (x01 m)
   `(cadr ,m))
;;;   (a b c d) -> c    
(define-macro (x10 m)
   `(caddr ,m))
;;;   (a b c d) -> d    
(define-macro (x11 m)
   `(cadddr ,m))
;;;   (a b c d) -> (a b)  
(define (row0 m)
   (list (x00 m) (x01 m)))
;;;   (a b c d) -> (c d)
(define (row1 m)  
   (cddr m))
;;;   (a b c d) -> (a c)    
(define (col0 m)
   (list (x00 m) (x10 m)))
;;;   (a b c d) -> (b d)    
(define (col1 m)
   (list (x01 m) (x11 m)))
;;;   (a1 a2) * (b1 b2)    
(define (vec-prod v1 v2)
   (apply f+ (map f* v1 v2)))
;;;   (a1 b1 c1 d1) * (a2 b2 c2 d2)       
(define (mat-prod m1 m2)   
   (list 
      (vec-prod (row0 m1) (col0 m2))
      (vec-prod (row0 m1) (col1 m2))
      (vec-prod (row1 m1) (col0 m2))
      (vec-prod (row1 m1) (col1 m2))))
;;;   product matrices in list      
(define (mat-list-prod lst)   
   (let loop ((l lst) (res '(1 0 0 1)))
      (if (null? l)
         res
	 (loop (cdr l) (mat-prod res (car l))))))
	 
(define (all-number? lst)
   (let loop ((L lst) (res #t))
      (if (or (null? L) (not res))
           res
	   (loop (cdr L) (number? (car L))))))
	 
(define (mat-num-prod lst)
   (let loop ((L lst) (res '(1 0 0 1)) (acc '()))
      (cond 
         ((null? L) (reverse (cons res acc)))
	 ((all-number? (car L)) 
	    (loop (cdr L) (mat-prod res (car L)) acc))
	 (else 
	    (loop (cdr L) '(1 0 0 1) (cons (car L) (cons res acc)))))))
      
;;;========== Инфиксная форма выражения =============

;;;   (* a (+ b c)) -> a * (b + c)
(define (print-expr e)
   (print-one e #f))
;;;   print expression
(define (print-one expr par?)
   (if par? (display "("))
   (if (list? expr)
      (let ((op (car expr)) (e1 (cadr expr)) (e2 (caddr expr))) 
         (if (list? e1)
            (print-inside e1 op)            
            (if (not (and (eqv? op '-) (eqv? e1 0))) ; 0 - a -> -a
               (print-number e1)))	    
	 (display op)  
	 (if (list? e2)
	    (print-inside e2 op)
	    (print-number e2)))
      (print-number expr))        ; if expr - not list
   (if par? (display ")")))
;;;  add parenthesis if need   
(define (print-inside expr op)
   (case op
      ((+) (print-one expr #f))
      ((-) (print-one expr (member (car expr) '(+ -))))
      ((*) (print-one expr (member (car expr) '(+ -))))
      ((/) (print-one expr #t))))
      
(define (print-number n)
   (if (number? n)
      (begin 
         (if (negative? n) (display "("))
	 (display n)
	 (if (negative? n) (display ")")))
      (display n)))

;;;=============== Список элементов ==================
;;; Участки схемы представляются в виде списков:
;;; (T len [n]) - оптический путь длиной len (с показателем преломления n)
;;; (R rad) - радиус кривизны поверхности rad
;;; (P f) - тонкая линза с фокусным расстоянием f
;;;---------------------------------------------------

;;; convert list of elements to list of matrix    
(define (to-matrix-list grp)       
   (let loop ((prev '()) (curr grp) (res '()))
      (if (null? curr)
          res
	  (let ((head (car curr)) (tail (cdr curr)))
	     (loop head tail
	        (cons
		   (get-m prev head (if (null? tail) '() (car tail)))
		   res))))))
;;; convert element to matrix	
(define (get-m p c n)
   (case (car c)
      ((T) (list 1 (f/ (cadr c) (get-n c)) 0 1)) 
      ((P) (list 1 0 (f- 0 (f/ 1 (cadr c))) 1))
      ((R) (list 1 0 (f- 0 (f/ (f- (get-n n) (get-n p)) (cadr c))) 1))
      ((Q) (list 1 0 (f/ (f* 2 (get-n p)) (cadr c)) 1))
      ((M) (cdr c))
      (else '(1 0 0 1))))
;;;   get value of refractiv index	
(define (get-n lst)
   (cond 
      ((null? lst) 1)
      ((eqv? (car lst) 'T)          
         (if (null? (cddr lst))
	     1
	     (caddr lst)))
      (else 1)))
      
;;;=============== Диалог ===============

;;;   dialog, expect (T ...) or (R ...) or (P ...), () to stop   
(define (get-scheme)
   (display "Enter elements in form (type val [val2]), to stop print ()")
   (newline)
   (get-next '()))    
;;;   get element   
(define (get-next group)
   (display ": ")
   (let ((s (read)))
      (cond
         ((not (list? s)) (wrong-read "Must be a list" group))
	 ((null? s) (reverse group))
	 ((string? (car s)) (get-next (get-from-file (car s))))
	 ((not (member (car s) '(P T R Q M))) (wrong-read "Must begin with P, T, Q, R or M" group))	  
         ((and (eqv? (car s) 'M) (not (= (length s) 5))) (wrong-read "Matrix must contain 4 elements" group))	 
	 (else (get-next (cons s group))))))
;;;   error input  
(define (wrong-read msg group)
   (display msg)
   (newline)
   (get-next group))
;;; read elements from file   
(define (get-from-file f)
   (call-with-input-file f get-from-port))
;;; working with file   
(define (get-from-port p)
   (let loop ((grp '()))
      (let ((s (read p)))
         (cond 
	    ((eof-object? s) (display "Done") (newline) grp)    ; how to print file name?
	    ((not (list? s)) ((display s) (display " not a list") (newline))) ;  break all
	    ((null? s) (display "Done") (newline) grp)
	    ((string? (car s)) (loop (append (get-from-file (car s)) grp)))
            (else (loop (cons s grp)))))))
;;; print result	    
(define (ABCD-print m)   
   (for-each 
      (lambda (x y) (display x) (print-expr y) (newline))
      '("A = " "B = " "C = " "D = ") m))
	
;;;============= Программа ==============

(define ABCD-list (to-matrix-list (get-scheme)))
(define ABCD-temp (mat-num-prod ABCD-list))
(define ABCD (mat-list-prod ABCD-temp))
(ABCD-print ABCD)



