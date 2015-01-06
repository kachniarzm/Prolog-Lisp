;rozbija elementy listy na listy (powstaje lista list)
(defun split_list(q) ;(a b c) -> ((a) (b) (c))
	(if (not (null q))
		(cons (list (car q)) (split_list (rest q)))
	)
)

;joinuje listy po odpowiadajacych elementach
(defun merge_lists(q1 q2) ;(a b c) + (d e f) -> ((a d)(b e)(c f))
	;zapewniamy ze oba parametry sa listami
	(if (not (listp q1))
		(setf q1 (list q1))
	)
	(if (not (listp q2))
		(setf q2 (list q2))
	)
	
	;zapewniamy ze pierwsze elementy obu parametrow sa listami
	(if (not (listp (first q1)))
		(setf (first q1) (list (first q1)))
	)
	(if (not (listp (first q2)))
		(setf (first q2) (list (first q2)))
	)
	
	;laczymy rekurencyjnie pierwsze elementy (listy) z q1 i q2
	(if (not (and (null (first q1)) (null (first q2))))
		(cons (append (first q1) (first q2)) (merge_lists (rest q1) (rest q2)))
	)
)

; transpozycja macierzy (nie dziala na wektorach)
(defun transpose(matrix)
	(setf result nil)
	(dolist (row matrix result)
		(setq result (merge_lists result (copy-list row)))
	)
)

;zastosuj operator "op" dla "el" oraz kazdego elementu listy (wyglada troche rozbudowanie bo dalem pare ficzerow dla wyrazen symbolicznych np a/a = 1)
(defun op_el_list(op el q) ;(* 10 '(3 5 -9)) -> (30 50 -90)
	(if (not (null q))
		(if (and (numberp (first q)) (numberp el))
			(append (list (apply op (list (first q) el))) (op_el_list op el (rest q)))
			(cond	
				((or (and (eq (first q) 0) (or (eq op '*) (eq op '/)))
						(and (eq  el 0) (eq op '*)))
					(append (list 0) (op_el_list op el (rest q)))	
				)
				((and (eq op '/) (eq (first q) el))
					(append (list 1) (op_el_list op el (rest q)))	
				)
				(T (append (list (list (first q) op el)) (op_el_list op el (rest q))))
			)			
		)
	)
)

;zastosuj operator "op" dla odpowiadajacyhc elementow wejsciowych list // dziala tez na symbolach
(defun op_lists(op q1 q2) ;(a b c) + (d e f) -> (a+d b+e c+f)
	(if (not (and (null q1) (null q2)))
		(if (and (numberp (first q1)) (numberp (first q2)))
			(cons (apply op (list (first q1) (first q2))) (op_lists op (rest q1) (rest q2)))
			(cons (list (first q1) op (first q2)) (op_lists op (rest q1) (rest q2)))
		)
	)
)

;zamienia wiersze w macierzy
(defun swap_rows(g i j)
	(setf tmp (nth i g))
	(setf (nth i g) (nth j g))
	(setf (nth j g) tmp)
	(block nil (return g))
)

;zastosuj operator "op" dla odpowiadajacych elementow macierzy (dzialania na elementach!)
(defun op_matrix(op x1 x2)
	(mapcar (lambda(r1 r2) (op_lists op r1 r2)) x1 x2)
)

(defun add(x1 x2)
	;(op_lists '+ (first x1) (first x2))
	(mapcar (lambda(r1 r2) (op_lists '+ r1 r2)) x1 x2)
)
(defun minus(x1 x2)
	;(op_lists '+ (first x1) (first x2))
	(mapcar (lambda(r1 r2) (op_lists '- r1 r2)) x1 x2)
)

;"nth" w wersji macierzowej
(defun nthm(m row col)
	(nth col (nth row m))
)

;eliminacja gaussa (b moze byc macierza)
(defun gauss(a b)
	(setq n (length a))
	(setq g (merge_lists a b))
	;(printm g)
	
	(dotimes (i n g);i = nr elementu glownego
		;(print "NEW ITERATION")
		(setq k (nthm g i i)) ;element glowny
		(setf (nth i g) (op_el_list '/ k (nth i g))) ;normalizujemy wiersz ktory zawiera element glowny
		;(print "ENTERING LOOP")
		; redukujemy pozostale wiersze
		(dotimes (other_row_num n)
			(if (not (eq i other_row_num))
				(progn
					(setq k (nthm g other_row_num i))
					;(printm g)
					(setf (nth other_row_num g) (op_lists '- (nth other_row_num g) (op_el_list '* k (nth i g))))
				)
			)
		)
	)
	
	(dotimes (i n g)
		(setf (nth i g) (nthcdr n (nth i g)))
	)
)

;generuje wersor z jedynka na "n-i" miejscu
(defun eye_vector(n i)
	(if (> n 0)
		(if (eq n i)
			(cons 1 (eye_vector (- n 1) i))
			(cons 0 (eye_vector (- n 1) i))
		)
	)
)

;generuje macierz jednostkowa o rozmiarze "n"
(defun eye(n)
	(setq result ())
	(dotimes (i n result)
		(setq result (cons (eye_vector n (+ i 1)) result))
		;(print (eye_vector n (- n i)))
	)
)

;odwraca macierz
(defun invert(a)
	(setq n (length a))
	(setq b (eye n))
	(gauss a b)
)

;drukuje macierz
(defun printm(matrix)
	;(print 'matrix)
	(dolist (row matrix)
		(print row)
	)
	(format t "~%") ;newline
)



;sumowanie elementów listy
(defun sum_list(list1)    
    (if (null list1)
        0
        (+ 
            (first list1) 
            (sum_list (rest list1))
        )   
    )   
)
;mnożenie skalarne
(defun scalar-multiply(l1 l2)
	(sum_list (op_lists '*  l1 l2))
)
;funkcja mnozenia dwóch macierzy
(defun matrix-mul(m1 m2)
	(setq m3 (transpose m2))
	(matrix-multiply m1 m3)
)

(defun matrix-multiply(m1 m2)	   
	   (if (null m1)
		 nil
		(cons (rowfun (first m1) m2)(matrix-multiply (rest m1) m2))
		)		
)

(defmacro macro_matrix-multiply(m1 m2)	   
	   `(if (null ,m1)
		 nil
		(cons (rowfun (first ,m1) ,m2)(macro_matrix-multiply (rest ,m1) ,m2))
		)		
)


; mnozenie wiersza przez macierz
(defun rowfun(m1 m3) ; row * matrix
	(if (null m3)
	   nil
	   (cons (scalar-multiply m1 (first m3)) (rowfun m1 (rest m3)))
	)
)

(defmacro macro_rowfun(m1 m3) ; row * matrix
	`(if (not (null ,m3))
		(cons (scalar-multiply ,m1 (first ,m3)) (macro_rowfun ,m1 (rest ,m3)))
	)
)

;funkcja usuwająca z listy li n-ty element
(defun RemoveNthElem(li n)
	(if (null li) 
		NIL
		(if (zerop n) 
				(rest li) 		
				(cons(first li) (RemoveNthElem (rest li) (- n 1)))		
		)
	)

)

(defmacro macro_RemoveNthElem(li n)
	`(if (null ,li) 
		NIL
		(if (zerop ,n) 
				(rest ,li) 		
				(cons(first ,li) (macro_RemoveNthElem (rest ,li) (- ,n 1)))		
		)
	)
)


;funkcja wyliczająca sume dla wyznacznika
(defun countpart(i j M)

	(if (< j 0)
		0
		(+ (*( * (expt -1 (+ i j)) (nth j(first M)) ) (det(getijM i j M))) (countpart i (- j 1) M))
	)
)

; funkcja licząca wyznacznik macierzy M
(defun det(M)
	(if(equal (length M) 2)
	 (-(* (first (first M)) (first (reverse(first(reverse M))))) (* (first(reverse (first M))) (first(first(reverse M)))))
	 (countpart '0 (- (length M) 1) M)
	)
)

; funkcja zwracająca macierz bez itego wiersza i j kolumny
(defun getijM(i j M)
	(setf M (RemoveNthElem M i))
	;(dolist (a M M) (setf M (RemoveNthElem a j)))
	(mapcar (lambda(M j) (RemoveNthElem M j)) M (CreateList j '0 (length M)))
)


; funkcja tworząca list wielkości size z elementami o wartościach i
(defun CreateList(i iter size)
	(if (< iter size)
		(cons i (CreateList i (+ iter 1) size))
		nil
	)
)


(defmacro macro_CreateList(i iter size)
	`(if (< ,iter ,size)
		(cons ,i (macro_CreateList ,i (+ ,iter 1) ,size))
		nil
	)
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; przyklady dzialania i testy ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq q '(4 5 6 7))
(setq m1 '((5 6 7)(8 9 10)(11 12 13)))
(setq m4 '((5 4 3)(2 1 0)(-1 -2 -3)))
(setq m2 '((a b c)(d e f)(g h i)))
(setq m3 '((aa bb)(cc 56)))
(setq m5 '((ee ff)(gg 44)))
(setq m6 '((zz)))
(setq m7 '((xx)))
(setq m8 '((a b) (c d)))

(setq a1 '((7 4)(3 2)))
(setq b1 '((1 2)(3 1)))
(setq a2 '((2 1 -1)(-3 -1 2)(-2 1 2)))
(setq b2 '(8 -11 -3))
(setq a3 '((1 3 4)(0 3 0)(2 -2 -3 )))
(setq a4 '((4 2 -5 8)(1 1 -2 0)(4 0 0 0)(3 -1 -2 4)))
(setq a5 '((1 -1 2)(3 0 -4)(2 3 5)))
(print "rowfun")
(print(rowfun (first a1) b1))
(print (macroexpand-1 '(macro_rowfun (first a1) b1)))
(print(macro_rowfun (first a1) b1))
(print "transpose")
(print(transpose a1))
(print "Matrix-mul")
(print(matrix-mul a1 b1))
(print(matrix-mul a3 a5))
(print "macro_matrix_mul")
(print(macroexpand-1 '(macro_matrix-multiply a1 (transpose b1))))
(print(macro_matrix-multiply a1 (transpose b1)))
(print "det")
(print(det b1))
(print(det a3))
(print(det a4)) 
(print(det a5))
(print "getijm")
(print(getijM 0 0 m1 ))
(print "removenthelem")
(print(RemoveNthElem '(2 3 4 2 1) 2))
(print "macro_removenthelem")
(print(macroexpand-1 '(macro_RemoveNthElem '(2 3 4 2 1) 2)))
(print(macro_RemoveNthElem '(2 3 4 2 1) 2))
(print "createlist")
(print(CreateList '3 '0 '10))
(print(macroexpand-1 '(macro_CreateList '3 '0 '10)))
(print(macro_CreateList '3 '0 '10))
;(mapcar (lambda(row) (split_list row))a1)
;(setq r1 (nth 0 m2))
;(setq r2 (nth 1 m2))
;(setq r3 (nth 2 m2))

;(setq ga1 (gauss a1 b1))
(print "#################################################################")
;(printm ga1)
;(printm (invert a1))
;(printm (invert a2))
;(printm (invert m8))
;(print "#################################################################")
;(setq ga2 (gauss a2 b2))
(print "#################################################################")
;(printm ga2)
;(print "#################################################################")
;(printm (add m1 m4))
;(printm (add m3 m5)) 
;(printm (minus m3 m5)) 
;(printm (op_matrix '* m1 m2)) 
;(print "#################################################################")
;(print (op_el_list '/ 'a '(a b c)))
;(print (op_el_list '+ '10 '(2 3 4)))
;(print (op_el_list '+ 'a '(2 3 4)))
;(print (op_el_list '- '10 '(x y z)))
;(print (eye_vector 5 4))
;(printm (eye 4))
