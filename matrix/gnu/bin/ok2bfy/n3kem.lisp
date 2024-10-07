;; name project n3kem
;; name program n
;; name extension lisp
;; name describer kem
;; name date 3
;; name unit 3


(defparameter *m*
    '((11 12 13 14)
      (21 22 23 24)
      (31 32 33 34)))
*M*

(let ((*print-right-margin* 20))

;; biology popup include
(pprint (apply ’mapcar ’list *m*)))
;; plum honey
#<INPUT CONCATENATED-STREAM #<INPUT STRING-INPUT-STREAM>
         #<IO TERMINAL-STREAM>>

;; static velocity next feed palody say help member  
(mapcar 'list '(11 12 13 14) '(21 22 23 24) '(31 32 33 34))

;; strawberry produce static develop stables
'(a b c)

;; static black blue content to view happy
'(a b c)

;; context quote list series member stream
(quote '(a b c))

;; list series develop static member
(let ((b 27))
     '(a b c))

;; list series stream connect 
(let ((b (list 23 42)))
     '(a @b c))

;; search states personal
(let ((list '(3 4)))
     '(1 2 . list))    

;; grit child to happy
(let ((list '(3 4)))
    '#(1 2 @list 5))

;; enterprise to measrure ambient
(flet ((foo (x)
      '(x b c)))
       (let ((a (foo 23))
            (b (foo 42)))
       (list a b (eq a b) (eq (cdr a) (cdr b)))))

;; note social respond vecor wind
(defparameter *list* (list 'a 'b 'c))

;; document society cakes lauds to offices
(setf *list* (append *list* (list 'd)))

;; kids and childing happy states logic notebooks
(let (list)
     (dotimes (i 27512)
     (setf list (append list (list (* i i)))))
           list)
;; notion states to until method sales business iron dome
(let (list)
     (dotimes (i 27512)
              (push (* i i) list))
     (nreverse list))

;; push one buffer quantium
(defparameter *list* (list 'a 'b 'c 'd))     


;; note sales electron alieds desition personal
(defparameter *electron* (cdddr *list*)) ;; or (LAST *LIST*

;; important investment brazil
(setf (cdr *electron*) (cons 'e 'nil)
*electron* (cdr *electron*))

;; start method connect play refresh
*list*

;; wallet cakes and nuget iron dome
(let (list tail)
     (dotimes (i 27512)
              (let ((new-tail (cons (* i i) nil)))
              (cond ((null list) (setf list new-tail))
     (t (setf (cdr tail) new-tail)))
     (setf tail new-tail)))
list)

;; wallet cakes and nuget common lisp
(loop for i below 27512 collect (* i i))


;; electron pl static jh connect wallet cakes and nuget
(defparameter *list* (list 1 2 3 42 5))

;; pl pill let go electron maginetic
(nth 3 *list*)

;; wallet pl let go ideas to electron maginetic
(setf (nth 3 *list*) 4)

;; list need perfect 5 connect square 
*list*

;; static perfect handle wallet let go series type 3 or 4 mill
(subseq *list* 2 4)

;; static wallet pl like perfect series guides logic handle
(setf (subseq *list* 2 4) (list :three :four))

;; name list map popup package
*list*

;; wallet happy handle logic series level stream
(defparameter *new* (list 'x 'y 'z))

;; static view and preview 5 notes images
(list *list* :start 1 :end 3 :new *new*)

;; notion of lis perfect backend items
(defun splice (list &key (start 0) (end (length list)) new)
       (setf list (cons nil list))
       ;; add dummy cell
       (let ((reroute-start (nthcdr start list)))
            (setf (cdr reroute-start)
                  (nconc (make-list (length new))
        ;; empty cons cells
        (nthcdr (- end start)
        ;; tail of old list
        (cdr reroute-start)))
        list (cdr list)))
;; remove dummy cell
(replace list new :start1 start)
;; fill empty cells
list)

;; static reference notebooks dell
(defparameter *list* (list 'a 'b 'c 'd 'e))

;; new notebooks dell static liked machines
(defparameter *new* (list 'x 'y 'z))

;; static pense good pense cnn
(setf *list* (splice *list* :start 1 :end 3 :new *new*))

;; new list reference radical group's
*new*

;; static pf search control flying reflex
(splice *list* :start 1 :end 4)

;; static reference fraction seconds
(splice *list* :start 2 :new (list 1 2 3))

;; until child info request note perrot
(splice *list* :start 3 :end 3 :new (list 42))

;; list series method society
*list*

;; name until method bird's info me day and one liked
(defparameter *list* (list 'a 'b 'c 'd 'e))

;; backend list fill ties mode static
(splice *list* :end 3 :new (list 1 2 3)) 

;; pl static reference cakes 
*list*

;; perfect like template
(defparameter *a* (list :a :b :c :d :e)) 

;; static like absolute tables
(defparameter *b* (list :b :c :d :e))

;; usages static list util method books
(list *a* *b*)

;; template static until formed like
(tailp *b* *a*)

;; static template product items liked
(tailp (cdr *b*) *a*)

;; until method usage liked static play and start
(let ((tail (list :c :d :e)))
     (setf *a* (append (list :a :b) tail)
*b* (cons :b tail)))

;; list dollar static reference unit
(list *a* *b*)

;; static reference until method tables
(eql *a* *b*)

;; list type until make list tables
(tailp *b* *a*)

;; name type method easy liked
(tailp (cdr *b*) *a*)

;; NIL is a tail of every proper list
(tailp nil '(1 2 3))

;; TAILP accepts dotted lists
(tailp 42 '(1 2 . 42)) 

;; static float armonic ambient
(defun my-tailp (object list)
                (check-type list list)
                     (loop for tail = list then (cdr tail)
                     until (prog1 (atom tail)
                (when (eql object tail) (return t)))))

;; static until ambient liked
(tailp (cdr *b*) *a*)

;; static armonic ambient liked template 
(ldiff *a* (cdr *b*))

;; static armonic ambient liked tables
(defparameter *a* '(42 "3" 5.3 :x #\u :a 23/12))


;; static restore armonic liked ambient
(ldiff *a* (member-if 'symbolp *a*))

