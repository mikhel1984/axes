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
      ((equal? a b) 0)
      (else (list '- a b))))
;;;   a / b	       
(define (f/ a b)
   (cond 
      ((eqv? b 0) 'ZERO_DIVIDE)
      ((and (number? a) (number? b)) (/ a b))      
      ((and (number? a) (= a 0)) 0)
      ((and (number? b) (= b 1)) a)      
      ((equal? a b) 1)
      (else (list '/ a b))))
      
;;;==================== Матрицы =====================
;;; Матрицы 2x2, записываются по строкам:
;;; (a b c d) -> (a b
;;;               c d)
;;;--------------------------------------------------

;;;   (a b c d) -> a
(define-macro (x00 m) `(car ,m))
;;;   (a b c d) -> b    
(define-macro (x01 m) `(cadr ,m))
;;;   (a b c d) -> c    
(define-macro (x10 m) `(caddr ,m))
;;;   (a b c d) -> d    
(define-macro (x11 m) `(cadddr ,m))
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
;;;   product of matrices in list      
(define (mat-list-prod lst)   
   (let loop ((l lst) (res '(1 0 0 1)))
      (if (null? l)
         res
	 (loop (cdr l) (mat-prod res (car l))))))
;;;   check if all elements are numbers (for nonempy list)	 
(define (all-number? lst)
   (let loop ((L lst) (res #t))
      (if (or (null? L) (not res))
           res
	   (loop (cdr L) (number? (car L))))))
;;;   reduced list where number matrixes were multiplied	 
(define (mat-num-prod lst)
   (let loop ((L lst) (res '(1 0 0 1)) (acc '()))
      (cond 
         ((null? L) (reverse (cons res acc)))
	 ((all-number? (car L)) 
	    (loop (cdr L) (mat-prod res (car L)) acc))
	 (else 
	    (loop (cdr L) '(1 0 0 1) (cons (car L) (cons res acc)))))))
      
;;;========== Инфиксная форма выражения =============

;;; convert number or symbol to string   
(define (num->str n)
   (if (number? n)
      (if (negative? n)
          (string-append "(" (number->string n) ")")
	  (number->string n))
      (symbol->string n)))
;;;  add parenthesis if need
(define (str-inside expr op)
   (case op
      ((+) (get-str expr #f))
      ((- *) (get-str expr (memq (car expr) '(+ -))))
      ((/) (get-str expr #t))))
;;;  (+ a b) -> "a + b"    
(define (get-str expr par?)
   (if (list? expr)
      (let ((op (car expr)) (e1 (cadr expr)) (e2 (caddr expr)))
         (string-append 
            (if par? "(" "")
            (if (list? e1) (str-inside e1 op)
                 (if (and (eq? op '-) (eqv? e1 0)) "" (num->str e1)))
            (num->str op)
            (if (list? e2) (str-inside e2 op) (num->str e2))
            (if par? ")" "")))
      (string-append
         (if par? "(" "")
         (num->str expr)
         (if par? ")" ""))))
;;;   (* a (+ b c)) ->" a * (b + c)"
(define (expr->str e)
   (get-str e #f))

;;;=============== Список элементов ==================
;;; Участки схемы представляются в виде списков:
;;; (T len [n]) - оптический путь длиной len (с показателем преломления n)
;;; (R rad) - радиус кривизны поверхности rad
;;; (P f) - тонкая линза с фокусным расстоянием f
;;; (Q rad) - отражающая поверхность с радиусов кривизны rad
;;; (M m00 m01 m10 m11) - произвольная матрица 2x2
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
;;;   get value of refractive index	
(define (get-n lst)
   (cond 
      ((null? lst) 1)
      ((eq? (car lst) 'T)          
         (if (null? (cddr lst))
	     1
	     (caddr lst)))
      (else 1)))
      
;;;=============== Диалог ===============

;;;   dialog, expect (T ...) or (R ...) or (P ...) or (Q ...) or (M ...), () to stop   
(define (get-scheme)
   (display "Перечислите элементы в виде (тип значение [знач2]), для окончания введите ()\n")
   (get-next '()))    
;;;   get element   
(define (get-next group)
   (display ": ")
   (let ((s (read)))
      (cond
         ((not (list? s)) (wrong-read "Ожидается список" group))
	 ((null? s) (reverse group))
	 ((string? (car s)) (get-next (get-from-file (car s))))
	 ((not (memq (car s) '(P T R Q M))) (wrong-read "Допустимые типы: P, T, Q, R и M" group))	  
         ((and (eq? (car s) 'M) (not (= (length s) 5))) (wrong-read "Матрица должна содержать 4 элемента" group))	 
	 (else (get-next (cons s group))))))
;;;   error input  
(define (wrong-read msg group)
   (display msg)
   (newline)
   (get-next group))
;;;   read elements from file   
(define (get-from-file f)
   (let ((res (call-with-input-file f get-from-port)))
      (display f) (display " - загружен\n")
      res))
;;;   working with file   
(define (get-from-port p)
   (let loop ((grp '()))
      (let ((s (read p)))
         (cond 
	    ((eof-object? s) grp)    ; how to print file name?
	    ((not (list? s)) ((display s) (display " не список\n"))) ;  break all
	    ((null? s) grp)
	    ((string? (car s)) (loop (append (get-from-file (car s)) grp)))
            (else (loop (cons s grp)))))))
;;; print result to display	    
(define (ABCD-print m)   
   (for-each 
      (lambda (x y) (display x) (display (expr->str y)) (newline))
      '("A = " "B = " "C = " "D = ") m))
;;;   save to file with given name   
(define (file-save mat p name)
   (display (dot-correct name) p)
   (display " : matrix(\n" p)
   (display (string-append "[" (expr->str (x00 mat)) ", " (expr->str (x01 mat)) "],\n") p)
   (display (string-append "[" (expr->str (x10 mat)) ", " (expr->str (x11 mat)) "]\n") p)
   (display ");" p))
;;;   "abc.txt" -> "abc" 
(define (dot-correct name)
   (let ((s (memq #\. (reverse (string->list name))))) ; "abc" -> (b c a)
      (if s (list->string (reverse (cdr s)))
             name)))  
;;;   initial function for Axes      
(define (main-dialog)
   (let* ((ABCD-list (to-matrix-list (get-scheme)))
	     (ABCD (mat-list-prod (mat-num-prod ABCD-list))))
      (ABCD-print ABCD)
      (continue-dialog ABCD)))
;;;   additional options      
(define (continue-dialog mat)
   (display "Команды: new - новый расчёт, (to имя) - сохранение в файл, q(uit) - выход\n")
   (display "? ")
   (let ((s (read)))
      (cond
         ((eqv? s 'new) (main-dialog))         
	 ((or (null? s) (eqv? s 'q) (eqv? s 'quit)) (display "Пока!\n"))
	 ((and (list? s) (eq? (car s) 'to) (string? (cadr s)))
	    (let ((fname (cadr s)))
	       (call-with-output-file fname
	          (lambda (i) 
		     (file-save mat i fname)))
		(display "Сохранено\n")
		(continue-dialog mat)))
	 (else (continue-dialog mat)))))   
	
;;;============= Запуск ==============

(main-dialog)



