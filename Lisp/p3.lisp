;;; Filename: p3.lisp
;;; Assignment: LISP Insertion Sort Function 
;;; Build Instructions:
;;;      Execute in CLI with the command `clisp p3.lisp' to sort with a generic list
;;; Directions:
;;;      Run the file using the instructions given above, or
;;;      use `clisp p3.lisp <args>' where <args> is a list of numbers
;;;      to sort any arbitrary list.
;;;      EXAMPLE: `clisp p3.lisp 1 4 2 -2 4`


;;; Begin insertion sort function `lins`
(defun lins (inputArr)
  ;; outer loop iterates from second element to last element in the array
  (loop for i from 1 to (1- (length inputArr))
	;; we assign `element` with the element at the ith index in the array
	do (let ((element (elt inputArr i))
		 ;; we then define j as i - 1
		 (j (- i 1)))
	     ;; the inner loop then loops while j is non-negative and
	     ;; while the element at the jth position is larger than the ith element
	     (loop while (and (>= j 0) (> (elt inputArr j) element))
		   ;; if the while condition is true, we should move the jth element
		   do (setf (elt inputArr (1+ j)) (elt inputArr j))
		   ;; then decrement j
		   (decf j))
	     ;; once the while conditions are no longer true, we have found
	     ;; where to insert the ith element. We then increment j (since we
	     ;; previously decremented it) and insert the ith element in its position
	     (setf (elt inputArr (1+ j)) element))))


;;; Begin Main
;; convert args into an input array
(setq input (mapcar #'parse-integer *args*))

;; check if args were present 
(if (null input)
  ;; if there are no elements passed as args, create an arbitrary array of numbers  
  (let ((arr '(3 341 4 22 9 872 2 1 651 3211 48 216 035 456 12 123 12 1 1 1 1)))
    (lins arr)  ; sort the array
    (princ arr) ; print the array
    )
  ;; otherwise, group the following calls for the passed array of values
  (progn
    (lins input)    ; sort the passed array of values
    (princ input))) ; print the passed array of values


