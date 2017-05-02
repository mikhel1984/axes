;;; S.Mikhel, 2016 - camis.dat@gmail.com
;;; Tested in Guile

;;;=============== Description ===================

;;; This program can be used for calculation the matrix of 
;;; transformation from object to image (ABCD).
;;; Matrix is based on list of elements, which are declared
;;; by user. Data can be represented both in numerical and 
;;; symbolical form.

(load "Axes-math.scm")

;;;==================== Matrix =====================
;;; Matrix 2x2 is represented by rows
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
      
;;;   Matrix to list multiplication
(define (mat-list-prod lst)   
   (let loop ((l lst) (res '(1 0 0 1)))
      (if (null? l)
         res
	 (loop (cdr l) (mat-prod res (car l))))))
	 
;;;   Check if all elements are numbers
(define (all-number? lst)
   (let loop ((L lst) (res #t))
      (if (or (null? L) (not res))
           res
	   (loop (cdr L) (number? (car L))))))
	   
;;;   Multiplication only for numerical matrices
(define (mat-num-prod lst)
   (let loop ((L lst) (res '(1 0 0 1)) (acc '()))
      (cond 
         ((null? L) (reverse (cons res acc)))
	 ((all-number? (car L)) 
	    (loop (cdr L) (mat-prod res (car L)) acc))
	 (else 
	    (loop (cdr L) '(1 0 0 1) (cons (car L) (cons res acc)))))))
      
;;;========== Infix form =============

;;;  Convert number to string
(define (num->str n)
   (if (number? n)
      (if (negative? n)
          (string-append "(" (number->string n) ")")
	  (number->string n))
      (symbol->string n)))
      
;;;  Additional brackets
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

;;;=============== List of elements ==================
;;; Parts of optical path should be represented as lists:
;;; (T len [n]) - optical path with length len (and refractive index n)
;;; (R rad) - radius of surface curvature rad
;;; (P f) - thin lens with focus f
;;; (Q rad) - reflecting surface with radius rad
;;; (M m00 m01 m10 m11) - arbitrary matrix 2x2
;;;---------------------------------------------------

;;;  Transform list of elements into matrix
(define (to-matrix-list grp)      
   (let loop ((prev '()) (curr grp) (res '()))
      (if (null? curr)
          res
	  (let ((head (car curr)) (tail (cdr curr)))
	     (loop head tail
	        (cons
		   (get-m prev head (if (null? tail) '() (car tail)))
		   res))))))
		   
;;;  Create matrix for element
(define (get-m p c n)
   (case (car c)
      ((T) (list 1 (f/ (cadr c) (get-n c)) 0 1)) 
      ((P) (list 1 0 (f- 0 (f/ 1 (cadr c))) 1))
      ((R) (list 1 0 (f- 0 (f/ (f- (get-n n) (get-n p)) (cadr c))) 1))
      ((Q) (list 1 0 (f/ (f* 2 (get-n p)) (cadr c)) 1))
      ((M) (cdr c))
      (else '(1 0 0 1))))
      
;;;  Get refractive index
(define (get-n lst)
   (cond 
      ((null? lst) 1)
      ((eq? (car lst) 'T)          
         (if (null? (cddr lst))
	     1
	     (caddr lst)))
      (else 1)))
      
;;;=============== Dialog ===============

;;;   Dialog function
(define (get-scheme)
   (display "Enumerate elements in form (type value [value2]), print () to finish\n")
   (get-next '()))    
   
;;;   Read element
(define (get-next group)
   (display ": ")
   (let ((s (read)))
      (cond
         ((not (list? s)) (wrong-read "List is expected" group))
	 ((null? s) (reverse group))
	 ((string? (car s)) (get-next (append (get-from-file (car s)) group)))
	 ((not (memq (car s) '(P T R Q M))) (wrong-read "Possible types: P, T, Q, R and M" group))	  
         ((and (eq? (car s) 'M) (not (= (length s) 5))) (wrong-read "Matrix must contain 4 elements" group))	 
	 (else (get-next (cons s group))))))
	 
;;;   Error processing
(define (wrong-read msg group)
   (display msg)
   (newline)
   (get-next group))
   
;;;   Get element from file
(define (get-from-file f)
   (let ((res (call-with-input-file f get-from-port)))
      (display f) (display " - loaded\n")
      res))
      
;;;   Working with files
(define (get-from-port p)
   (let loop ((grp '()))
      (let ((s (read p)))
         (cond 
	    ((eof-object? s) grp)
	    ((not (list? s)) ((display s) (display " not a list\n"))) ;  break all
	    ((null? s) grp)
	    ((string? (car s)) (loop (append (get-from-file (car s)) grp)))
            (else (loop (cons s grp)))))))
	    
;;;  Print the result
(define (ABCD-print m)   
   (for-each 
      (lambda (x y) (display x) (display (expr->str y)) (newline))
      '("A = " "B = " "C = " "D = ") m))
      
;;;   Save as file (for Maxima) 
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
	     
;;;   Run Axes program      
(define (main-dialog)
   (let* ((ABCD-list (to-matrix-list (get-scheme)))
	     (ABCD-init (mat-list-prod (mat-num-prod ABCD-list)))
             (ABCD (map simplify ABCD-init)))
      (ABCD-print ABCD)
      (continue-dialog ABCD)))
      
;;;  Additional opportunities
(define (continue-dialog mat)
   (display "Commands: new - new calculation, (to 'name') - save as file, q(uit) - exit,\n")
   (display "((variable1 value1) (variable2 value2) ...) - recalculation\n")
   (display "? ")
   (let ((s (read)))
      (cond
         ;; create new schema
         ((eqv? s 'new) (main-dialog))    
         ;; exit
	 ((or (null? s) (eqv? s 'q) (eqv? s 'quit)) (display "Bye!\n"))
	 ;; save to file
	 ((and (list? s) (eq? (car s) 'to) (string? (cadr s)))
	    (let ((fname (cadr s)))
	       (call-with-output-file fname
	          (lambda (i) 
		     (file-save mat i fname)))
		(display "Saved\n")
		(continue-dialog mat)))
	 ;; calculate for new data
	 ((and (list? s) (list? (car s)))
	    (let ((ABCD-tmp (map 
	                (lambda (i)  (simplify (eval-for i s))) 
		        mat)))
	       (ABCD-print ABCD-tmp)
	       (continue-dialog mat)))
         ;; continue dialog
	 (else (continue-dialog mat)))))   
	
;;;============= Let's go! ==============

(main-dialog)



