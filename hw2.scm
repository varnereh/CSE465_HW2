#lang scheme
; ---------------------------------------------
; DO NOT REMOVE OR CHANGE ANYTHING UNTIL LINE 26
; ---------------------------------------------

; zipcodes.scm contains all the US zipcodes.
; This file must be in the same folder as hw2.scm file,
; and you should not modify it. Your code
; should work for other instances of this file.
(require "zipcodes.scm")

; Helper function
(define (mydisplay value)
	(display value)
	(newline)
)

; Helper function
(define (line func)
        (display "--------- ")
        (display func)
        (display " ------------")
        (newline)
)

; ================ Solve the following functions ===================










; Return a list with only the negatives items
(define (negatives lst)
	(if (null? lst)
            '() ; empty list if null
            (if (> 0 (car lst)) ; else check if negative
                (cons (car lst) (negatives (cdr lst)))  ; construct new list with the negative val if true
                (negatives (cdr lst)) ; else move on
                )
            )
)

(line "negatives")
(mydisplay (negatives '()))  ; -> ()
(mydisplay (negatives '(-1)))  ; -> (-1)
(mydisplay (negatives '(-1 1 2 3 4 -4 5)))  ; -> (-1 -4)
(mydisplay (negatives '(1 1 2 3 4 4 5)))  ; -> ()
(line "negatives")




; ---------------------------------------------

; Returns true if the two lists have identical structure
; in terms of how many elements and nested lists they have in the same order
; ChatGPT assisted with order of checking
(define (struct lst1 lst2)
	(cond ; condition for better formatting, since there are so many checks
          ((and (null? lst1) (null? lst2)) #t) ; if both of them are null
          ((or (null? lst1) (null? lst2)) #f) ; if one is null and the other is not, false
          ((and (list? (car lst1)) (list? (car lst2)))   ; check if both first elements are lists
              (and (struct (car lst1) (car lst2))(struct (cdr lst1) (cdr lst2))))  ; recursively check rest
          ((and (not (list? (car lst1))) (not (pair? (car lst2)))) ; if both first elements aren't lists
           (struct (cdr lst1) (cdr lst2)))
          (else #f)) ; anything else means they are different
)

(line "struct")
(mydisplay (struct '(a b c (c a b)) '(1 2 3 (a b c))))  ; -> #t
(mydisplay (struct '(a b c d (c a b)) '(1 2 3 (a b c))))  ; -> #f
(mydisplay (struct '(a b c (c a b)) '(1 2 3 (a b c) 0)))  ; -> #f
(line "struct")




; ---------------------------------------------

; Helper methods for min and max (just a min function and a max function)
; Finds minimum value
(define (minlst lst)
  (if (= (length lst) 1)
      (car lst)
      (min (car lst) (minlst (cdr lst)))
      )
)

; Finds maximum value
(define (maxlst lst)
  (if (= (length lst) 1)
      (car lst)
      (max (car lst) (maxlst (cdr lst)))
      )
)


; Returns a list of two numeric values. The first is the smallest
; in the list and the second is the largest in the list. 
; lst -- contains numeric values, and length is >= 1.
(define (minAndMax lst)
  (list (minlst lst) (maxlst lst)) ; use helper methods to just return the two
)

(line "minAndMax")
(mydisplay (minAndMax '(1 2 -3 4 2)))  ; -> (-3 4)
(mydisplay (minAndMax '(1)))  ; -> (1 1)
(line "minAndMax")





; ---------------------------------------------

; Returns a list identical to the first list, while having all elements
; that are inside nested lists taken out. So we want to flatten all elements and have
; them all in a single list. For example '(a (a a) a))) should become (a a a a)
(define (flatten lst)
	(if (null? lst)
            '() ; return empty lst if empty
            (if (list? (car lst)) ; else check if element is a list
                (append (flatten (car lst)) (flatten (cdr lst))) ; flatten first element and then do the rest
                (cons (car lst) (flatten (cdr lst))))) ; if first element is not list, construct list and then recursively do rest
)

(line "flatten")
(mydisplay (flatten '(a b c)))  ; -> (a b c)
(mydisplay (flatten '(a (a a) a)))  ; -> (a a a a)
(mydisplay (flatten '((a b) (c (d) e) f)))  ; -> (a b c d e f)
(line "flatten")




; ---------------------------------------------

; Helper method for cross product. Pairs element with another
(define (crossproduct_help lst1element lst2element)
  (if (null? lst2element)
      '() ; return empty list if null
      (cons (list lst1element (car lst2element)) ; else construct list with the elements
      (crossproduct_help lst1element (cdr lst2element))))
)


; The paramters are two lists. The result should contain the cross product
; between the two lists: 
; The inputs '(1 2) and '(a b c) should return a single list:
; ((1 a) (1 b) (1 c) (2 a) (2 b) (2 c))
; lst1 & lst2 -- two flat lists.
(define (crossproduct lst1 lst2)
	(if (null? lst1) ; check if the first list is empty
            '() ; return empty list if so
            (append (crossproduct_help (car lst1) lst2) ; use a helper method to recursively pair the first num of lst1 with lst2
                    (crossproduct (cdr lst1) lst2)))  ; recursively do the rest of the pairs
)

(line "crossproduct")
(mydisplay (crossproduct '(1 2) '(a b c)))
(line "crossproduct")




; ---------------------------------------------

; Returns the first latitude and longitude of a particular zip code.
; if there are multiple latitude and longitude pairs for the same zip code,
; the function should only return the first pair. e.g. (53.3628 -167.5107)
; zipcode -- 5 digit integer
; zips -- the zipcode DB- You MUST pass the 'zipcodes' function
; from the 'zipcodes.scm' file for this. You can just call 'zipcodes' directly
; as shown in the sample example
(define (getLatLon zipcode zips)
	(if (null? zips)
            '() ; return empty list if null
            (let ((element (car zips)))  ; get the first element in the list (ChatGPT taught me of (let))
            (if (= (car element) zipcode) ; if the zip code element in the list matches input zip
                (list (list-ref element 4) (list-ref element 5)) ; return the fourth and fifth element of the list
                (getLatLon zipcode(cdr zips))))) ; check the rest of the list
)

(line "getLatLon")
(mydisplay (getLatLon 99612 zipcodes))
(line "getLatLon")




; ---------------------------------------------

; Helper methods for getCommonPlaces

; Method to get all relevant places
(define (getPlaces state zips)
  (if (null? zips) ; check if null
      '()  ; if so, return empty list
      (let ((entry (car zips))) ; else, get variable of the first zip line
      (if (equal? state (list-ref entry 2))  ; if state matches the argument
            (cons (list-ref entry 1) (getPlaces state (cdr zips)))  ; if so, add the name of the place, and get the rest of the places
            (getPlaces state (cdr zips)))))  ; skip it if it doesn't match, and check rest
)

; Method to determine which places are common
(define (commonPlaces places1 places2)
  (if (null? places1) ; check if null
      '()  ; if so, return empty list
      (if (member (car places1) places2) ; if the place is in both lists (ChatGPT introduced me to (member))
          (cons (car places1) (commonPlaces (cdr places1) places2))  ; if so, add it to result and find the rest of the places
          (commonPlaces (cdr places1) places2))) ; else, skip and check rest
)       

; Returns a list of all the place names common to two states.
; state1 -- the first state to look for
; state2 -- the second state to look for
; zips -- the zipcode DB
(define (getCommonPlaces state1 state2 zips)
	(let ((places1 (getPlaces state1 zips))   ; make variable for state1 places
        (places2 (getPlaces state2 zips)))  ; make variable for state2 places
    (commonPlaces places1 places2))        ; use the variables to determine the common places
)

(line "getCommonPlaces")
(mydisplay (getCommonPlaces "OH" "MI" zipcodes))
(line "getCommonPlaces")




; ---------------------------------------------

; Returns the number of zipcode entries for a particular state.
; state -- state
; zips -- zipcode DB
(define (zipCount state zips)
	(if (null? zips) ; check if null
            0 ; return zero as base case
            (let ((element (car zips))) ; else grab an element from zips
            (if (equal? state (list-ref element 2)) ; if the state matches the line
               (+ 1 (zipCount state (cdr zips))) ; if it matches, add one to count and check the rest
               (zipCount state (cdr zips))))) ; if not, don't add to count, but check rest of the list                    
)

(line "zipCount")
(mydisplay (zipCount "OH" zipcodes))
(line "zipCount")




; ---------------------------------------------

; Some sample predicates
(define (POS? x) (> x 0))
(define (NEG? x) (< x 0))
(define (LARGE? x) (>= (abs x) 10))
(define (SMALL? x) (not (LARGE? x)))

; Helper method to check if all the filters apply
(define (filtersApply element filters)
  (if (null? filters) ; check if null
      #t  ; if there are no filters, then it is true
      (if ((car filters) element)  ; else apply the first filter
          (filtersApply element (cdr filters))  ; check the rest of the filters
          #f)))  ; if any filter doesn't work, return false


; Returns a list of items that satisfy a set of predicates.
; For example (filterList '(1 2 3 4 100) '(EVEN?)) should return the even numbers (2 4 100)
; (filterList '(1 2 3 4 100) '(EVEN? SMALL?)) should return (2 4)
; lst -- flat list of items
; filters -- list of predicates to apply to the individual elements

(define (filterList lst filters)
    (if (null? lst) ; check if null
       '()  ; if lst is null (base case) return empty list
       (let ((element (car lst))) ; make an element a variable
       (if (filtersApply element filters)  ; check if the element works with the filters
           (cons element (filterList (cdr lst) filters))  ; if it does, add it to result, and check the rest
           (filterList (cdr lst) filters))))  ; if it doesn't, don't add it, and check the rest
)

(line "filterList")
(mydisplay (filterList '(1 2 3 11 22 33 -1 -2 -3 -11 -22 -33) (list POS?)))
(mydisplay (filterList '(1 2 3 11 22 33 -1 -2 -3 -11 -22 -33) (list POS? even?)))
(mydisplay (filterList '(1 2 3 11 22 33 -1 -2 -3 -11 -22 -33) (list POS? even? LARGE?)))
(line "filterList")













; ---------------------------------------------

; #### Only for Graduate Students ####
; Returns a list of all the place names common to a set of states.
; states -- is list of state names
; zips -- the zipcode DB
(define (getCommonPlaces2 states zips)
	'("Oxford" "Franklin")
)

(line "getCommonPlaces2")
(mydisplay (getCommonPlaces2 '("OH" "MI" "PA") zipcodes))
(line "getCommonPlaces2")

; ---------------------------------------------

; #### Only for Graduate Students ####
; Returns the distance between two zip codes in "meters".
; Use lat/lon. Do some research to compute this.
; You can find some info here: https://www.movable-type.co.uk/scripts/latlong.html
; zip1 & zip2 -- the two zip codes in question.
; zips -- zipcode DB
(define (getDistanceBetweenZipCodes zip1 zip2 zips)
	0
)

(line "getDistanceBetweenZipCodes")
(mydisplay (getDistanceBetweenZipCodes 45056 48122 zipcodes))
(line "getDistanceBetweenZipCodes")
; ---------------------------------------------



