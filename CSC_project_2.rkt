#lang racket
(require csc151)
(define data-raw
  (read-csv-file "Data-Table 1.csv"))

; Step one: Clean dataset by filtering out columns that are useful

;;; Procedure:
;;;   select-cols
;;; Parameters:
;;;   row, a list
;;;   cols, a list
;;; Purpose:
;;;   select the columns in a row of a table specified by the indexes provided
;;; Produces:
;;;;  result, a list 
;;; Preconditions:
;;;   cols is a list of integers.
;;; Postconditions:
;;;   * if cols is null, it should return null.
;;;   * otherwise, it will make a new table based on row, which is a list of characteristics of one university,
;;;    and cols, a list of number, which is corrsponding to desired columns.

(define select-cols
  (lambda (row cols)
    (let kernel ([row row]
                 [col cols])
      (if (null? col)
          null
          (cons (list-ref row (car col))
                (kernel row (cdr col)))))))

#|
The indexes in cols-needed represent following columns
0) ;"Name"            
1) ;"Applicants total"
2) ;"Admissions total"              
6) ;"SAT Critical Reading 25th percentile score"
7) ;"SAT Critical Reading 75th percentile score"
8) ;"SAT Math 25th percentile score"
9) ;"SAT Math 75th percentile score"             
10) ;"SAT Writing 25th percentile score"
11) ;"SAT Writing 75th percentile score"              
12) ;"ACT Composite 25th percentile score"
13) ;"ACT Composite 75th percentile score"              
20) ;"Estimated freshman undergraduate enrollment, total"
32) ;"Tuition and fees, 2013-14"
33) ;"Total price for in-state students living on campus 2013-14"
34) ;"Total price for out-of-state students living on campus 2013-14"
35) ;"State"
|#

(define cols-needed
  (list 0 1 2 6 7 8 9 10 11 12 13 20 32 33 34 35))

#|
Reference for columns in data-selected
0) ;"Name"
              
1) ;"Applicants total"
2) ;"Admissions total"
              
3) ;"SAT Critical Reading 25th percentile score"
4) ;"SAT Critical Reading 75th percentile score"
5) ;"SAT Math 25th percentile score"
6) ;"SAT Math 75th percentile score"
              
7) ;"SAT Writing 25th percentile score"
8) ;"SAT Writing 75th percentile score"
              
9) ;"ACT Composite 25th percentile score"
10) ;"ACT Composite 75th percentile score"
              
11) ;"Estimated freshman undergraduate enrollment, total"

12) ;"Tuition and fees, 2013-14"
13) ;"Total price for in-state students living on campus 2013-14"
14) ;"Total price for out-of-state students living on campus 2013-14"

15) ;"State"
|#

(define data-selected  
  (drop (map (section select-cols <> cols-needed) data-raw) 1))


; Step two: Process data

; 2.1 Size
;;; Procedure:
;;;   gen-size
;;; Parameters:
;;;   row, a list
;;; Purpose:
;;;   to determine the size of univeristies in the table.
;;;   append a new column to the original table with the sizes of the school 
;;; Produces:
;;;;  result, a list 
;;; Preconditions:
;;;   length of row should be larger than 12.
;;; Postconditions:
;;;   * if (list-ref row 11) is not a number, it should add "n/a" to the last part of the list.
;;;   * if (list-ref row 11) is less than 500, it should add "small" to the last part of the list.
;;;   * if (list-ref row 11) is bigger than 500 but less than 1000, it should add "medium" to the last part of the list.
;;;   * if (list-ref row 11) is bigger or equal to 1000, it should add "large" to the last part of the list.

(define gen-size
  (lambda (row)
    (let ([size (list-ref row 11)])
      (cond
        [(not (number? size))
         (append row (list "n/a"))]
        [(< size 500)
         (append row (list "small"))]
        [(< size 1000)
         (append row (list "medium"))]
        [else
         (append row (list "large"))]))))

(define data-size
  (map gen-size data-selected))

; 2.2 SAT
;;; Procedure:
;;;   gen-sat
;;; Parameters:
;;;   row, a list
;;; Purpose:
;;;   to calculate the 75th percetile SAT score of univeristies in the table.
;;; Produces:
;;;   result, a list 
;;; Preconditions:
;;;   length of row should be larger than 7.
;;; Postconditions:
;;;   * if (list-ref row 4) is not a number, it should add "n/a" to the last part of the list.
;;;   * otherwise, it should add up reading and math score and append the result to the list.

(define gen-sat
  (lambda (row)
    (let ([reading (list-ref row 4)]
          [math (list-ref row 6)])
      (cond
        [(not (number? reading))
         (append row (list "n/a"))]
        [else
         (append row (list (+ reading math)))]))))

(define data-sat
  (map gen-sat data-size))

; 2.3 Acceptance rate
;;; Procedure:
;;;   gen-accpt
;;; Parameters:
;;;   row, a list
;;; Purpose:
;;;   to calculate the acceptance rate of univeristies in the table.
;;; Produces:
;;;;  result, a list 
;;; Preconditions:
;;;   length of row should be larger than 3.
;;; Postconditions:
;;;   * if (list-ref row 1) is not a number or zero, it should add "n/a" to the last part of the list.
;;;   * if (list-ref row 2) is zero, it should add "n/a" to the last part of the list.
;;;   * otherwise, it should divide admissions by applicants, and add the result to the last part of the list.

(define gen-accpt
 (lambda (row)
   (let ([applicants (list-ref row 1)]
         [admission (list-ref row 2)])
     (cond
       [(not (number? applicants))
        (append row (list "n/a"))]
       [(zero? applicants)
        (append row (list "n/a"))]
       [(zero? admission)
        (append row (list "n/a"))]
       [else
        (append row (list (exact->inexact (/ admission applicants))))]))))

(define data-accpt
  (map gen-accpt data-sat))

; 2.4 Total cost
;;; Procedure:
;;;   gen-cost
;;; Parameters:
;;;   row, a list
;;; Purpose:
;;;   to calculate the total cost of univeristies in the table.
;;; Produces:
;;;;  result, a list 
;;; Preconditions:
;;;   length of row should be larger than 15.
;;; Postconditions:
;;;   * if (list-ref row 12) is not a number, it should add "n/a" to the last part of the list.
;;;   * if (list-ref row 2) is zero, it should add "n/a" to the last part of the list.
;;;   * otherwise, it should divide admissions by applicants, and add the result to the last part of the list.

(define gen-cost
  (lambda (row)
    (let ([living (list-ref row 14)])
      (cond
        [(not (number? living))
         (append row (list "n/a"))]
        [else
         (append row (list living))]))))

(define data-cost
  (map gen-cost data-accpt))

; 2.5 Finalized table
(define cols-final
  '(0 16 17 10 18 19 15))

#|
Reference for final table 
0 ;Name
1 ;Size
2 ;SAT Reading + Math ranking for 75th percentile
3 ;ACT score for 75th percentile 
4 ;Acceptance rate
5 ;Total cost
6 ;State
|#

(define data-final
  (map (section select-cols <> cols-final) data-cost))


; Step three: Filtering according to requirements

; Procedure to sort according to acceptance rate
;;; Procedure:
;;;   sort-accpt
;;; Parameters:
;;;   table, a list of lists
;;; Purpose:
;;;   sort table according to the acceptance rate.
;;; Produces:
;;;   result, a list of lists
;;; Preconditions:
;;;   * each lists in table contains ("name of the college" "size of the college" "75th percentile SAT score" "75th percentile ACT score" "acceptance rate" "total cost" "name of the state")
;;;   * "name of the college", "size of the college", and "name of the State" are string(s).
;;;   * "75th percentile SAT score", "75th percentile ACT score", "acceptance rate", and "total cost" are numbers. 
;;; Postconditions:
;;;   * previous list has the same or higher acceptance rate and a list after has the same or lower acceptance rate.
;;;   * result is sorted from the highest acceptance rate to the lowest acceptance rate.

(define sort-accpt
  (lambda (table)
    (sort (filter
           (lambda (x)
             (number? (list-ref x 4))) table)
          (lambda (v1 v2)
            (> (list-ref v1 4) (list-ref v2 4))))))

; 3.1 Filter by size
;;; Procedure:
;;;   fil-size
;;; Parameters:
;;;   size, a scheme value
;;;   table, a list of lists
;;; Purpose:
;;;   take lists of colleges based on desired size and sort table according to the acceptance rate.
;;; Produces:
;;;   result, a list of lists or null.
;;; Preconditions:
;;;   * size is the size of the college and it would be either "small", "medium", or "large".
;;;   * size = #f if user has no preference. 
;;;   * each lists in table contains ("name of the college" "size of the college" "75th percentile SAT score" "75th percentile ACT score" "acceptance rate" "total cost" "name of the state")
;;;   * "name of the college", "size of the college", and "name of the state" are string(s).
;;;   * "75th percentile SAT score", "75th percentile ACT score", "acceptance rate", and "total cost" are numbers.
;;; Postconditions:
;;;   * filter out the desired size of the college (small, medium, or large) and then sort them from the highest acceptance rate to the lowest acceptance rate.
;;;   * the second element in every lists in result is the given size.
;;;   * previous list has the same or higher acceptance rate and a list after has the same or lower acceptance rate.
;;;   * if size = #f, result produces table.
;;;   * if given some scheme value besides "small", "medium", or "large", result = null.

(define fil-size
  (lambda (size table)
    (if size
        (sort-accpt (filter
                     (lambda (x)
                       (equal? size (cadr x))) table))
        table)))

; 3.2 Filter by SAT
;;; Procedure:
;;;   fil-sat
;;; Parameters:
;;;   sat, a real number or #f
;;;   table, a list of lists
;;; Purpose:
;;;   take lists of colleges based on given SAT score and sort table according to the acceptance rate.
;;; Produces:
;;;   result, a list of lists or null.
;;; Preconditions:
;;;   * sat is an integer and a given SAT score provided by the user.
;;;   * sat = #f if user did not take the SAT.
;;;   * each lists in table contains ("name of the college" "size of the college" "75th percentile SAT score" "75th percentile ACT score" "acceptance rate" "total cost" "name of the state")
;;;   * "name of the college", "size of the college", and "name of the state" are string(s).
;;;   * "75th percentile SAT score", "75th percentile ACT score", "acceptance rate", and "total cost" are numbers.
;;; Postconditions:
;;;   * filter out colleges that have lower SAT score than sat and then sort them from the highest acceptance rate to the lowest acceptance rate.
;;;   * the third element in every lists in result is less than sat. 
;;;   * if sat is too low, result = null.
;;;   * if sat is not an integer, result = null. 
;;;   * if sat = #f, result produces table.
;;;   * previous list has the same or higher acceptance rate and a list after has the same or lower acceptance rate.

(define fil-sat
  (lambda (sat table)
    (if sat
        (sort-accpt (filter
                     (lambda (x)
                       (< (caddr x) sat))
                     (filter
                      (lambda (x)
                        (number? (caddr x)))
                      table)))
        table)))

; 3.3 Filter by ACT
;;; Procedure:
;;;   fil-act
;;; Parameters:
;;;   act, a real number or #f
;;;   table, a list of lists
;;; Purpose:
;;;   take lists of colleges based on given ACT score and sort table according to the acceptance rate.
;;; Produces:
;;;   result, a list of lists or null.
;;; Preconditions:
;;;   * act is an integer and a given ACT score provided by the user.
;;;   * act = #f if user did not take the ACT.
;;;   * each lists in table contains ("name of the college" "size of the college" "75th percentile SAT score" "75th percentile ACT score" "acceptance rate" "total cost" "name of the state")
;;;   * "name of the college", "size of the college", and "name of the state" are string(s).
;;;   * "75th percentile SAT score", "75th percentile ACT score", "acceptance rate", and "total cost" are numbers.
;;; Postconditions:
;;;   * filter out colleges that have lower ACT score than act and then sort them from the highest acceptance rate to the lowest acceptance rate.
;;;   * the fourth element in every lists in result is less than act. 
;;;   * if act is too low, result = null.
;;;   * if act is not an integer, result = null.
;;;   * if act = #f, result produces table.
;;;   * previous list has the same or higher acceptance rate and a list after has the same or lower acceptance rate.

(define fil-act
  (lambda (act table)
    (if act
        (sort-accpt (filter
                     (lambda (x)
                       (< (cadddr x) act))
                     (filter
                      (lambda (x)
                        (number? (cadddr x)))
                      table)))
        table)))

; 3.4 Filter by cost
;;; Procedure:
;;;   fil-cost
;;; Parameters:
;;;   cost, a real number or #f
;;;   table, a list of lists
;;; Purpose:
;;;   take lists of colleges based on given cost the user can afford and sort table according to the acceptance rate.
;;; Produces:
;;;   result, a list of lists or null.
;;; Preconditions:
;;;   * cost is an integer and a given cost provided by the user.
;;;   * cost = #f if user did not provide the cost he/she can afford.
;;;   * each lists in table contains ("name of the college" "size of the college" "75th percentile SAT score" "75th percentile ACT score" "acceptance rate" "total cost" "name of the state")
;;;   * "name of the college", "size of the college", and "name of the state" are string(s).
;;;   * "75th percentile SAT score", "75th percentile ACT score", "acceptance rate", and "total cost" are numbers.
;;; Postconditions:
;;;   * filter out colleges that cost less than cost and then sort them from the highest acceptance rate to the lowest acceptance rate.
;;;   * the sixth element in every lists in result is less than cost.
;;;   * if cost is too low, result = null.
;;;   * if cost is not an integer, result = null.
;;;   * if cost = #f, result produces table.
;;;   * previous list has the same or higher acceptance rate and a list after has the same or lower acceptance rate.

(define fil-cost
  (lambda (cost table)
    (if cost
        (sort-accpt (filter
                     (lambda (x)
                       (< (list-ref x 5) cost))
                     (filter
                      (lambda (x)
                        (number? (list-ref x 5)))
                      table)))
    table)))

; 3.5 Filter by location
;;; Procedure:
;;;   fil-loc
;;; Parameters:
;;;   loc, a scheme value
;;;   table, a list of lists
;;; Purpose:
;;;   take lists of colleges based on desired location and sort table according to the acceptance rate.
;;; Produces:
;;;   result, a list of lists or null. 
;;; Preconditions:
;;;   * loc is the location of the college and it would be the name of the state.
;;;   * loc = #f if user has no preference.
;;;   * each lists in table contains ("name of the college" "size of the college" "75th percentile SAT score" "75th percentile ACT score" "acceptance rate" "total cost" "name of the state")
;;;   * "name of the college", "size of the college", and "name of the state" are string(s).
;;;   * "75th percentile SAT score", "75th percentile ACT score", "acceptance rate", and "total cost" are numbers.
;;; Postconditions:
;;;   * filter out the desired location of the college and then sort them from the highest acceptance rate to the lowest acceptance rate.
;;;   * the seventh element in every lists in result is the given location.
;;;   * previous list has the same or higher acceptance rate and a list after has the same or lower acceptance rate.
;;;   * if loc = #f, result produces table.
;;;   * if given some scheme value besides the name of the state, result = null.

(define fil-loc
  (lambda (loc table)
    (if loc
        (sort-accpt (filter
                     (lambda (x)
                       (equal? (list-ref x 6) loc))
                     table))
        table)))


; Step four: Putting everything together

;;; Procedure:
;;;   search-college
;;; Parameters:
;;;   size, a scheme value
;;;   sat, a real number or #f
;;;   act, a real number of #f
;;;   cost, a real number or #f
;;;   loc, a scheme value
;;; Purpose:
;;;   provide recommended lists of various colleges based on size, sat, act, cost, and loc. 
;;; Produces:
;;;   result, a list of lists or null.
;;; Preconditions:
;;;   * size is either "small", "medium", or "large" and indicates the size of the college.
;;;   * sat is an integer and indicates user's SAT score.
;;;   * act is an integer and indicates user's ACT score.
;;;   * cost is an integer and indicates user's cost he/she can afford.
;;;   * loc is a name of the state where the user wants to attend. 
;;;   * #f in any parameter(s) indicate(s) that the user did not have any preference to look up colleges.
;;; Postconditions:
;;;   * if result has more than ten lists, take only the first ten.
;;;   * if all the parameters = #f, take the first ten lists from data-final.
;;;   * (length result) = 10
;;;   * result is sorted from the highest acceptance rate to the lowest acceptance rate after the procedure filters size, sat, act, cost, and/or loc depending on the given information. 
;;;   * if given parameter(s) is/are below the lowest SAT score, lowest ACT score, and/or lowest costs provided in the entire list, result = null.
;;;   * if size does not equal to "small", "medium", or "large", and/or loc is not a name of a state, result = null.

;;; comment: this algorithm gives the user the freedom to ignore certain criteria by specifing the requirement with #f
;;; for instance, if I would like to find a school with small size and in New York, I would simpy command
;;; (search-college small #f #f #f "New York")

(define search-college
  (lambda (size sat act cost loc)
    (let ([table
           (fil-loc loc
             (fil-cost cost
             (fil-act act
             (fil-sat sat
             (fil-size size data-final)))))])
      (if (< (length table) 11)
          table
          (take table 10)))))