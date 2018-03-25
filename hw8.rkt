#lang racket

(provide 
 lorint time-calls
 total-order?
 sorted? insert merge
 isort msort
 count-compares)

; Please do not change lines above this one.

;************************************************************
; CS 201a HW #8  DUE Sunday December 10th at 11:59 pm, 
; via the submit system on the Zoo.  This assignment is worth 90 points.
;************************************************************
; Name: Olivia Roth
; Email address: olivia.roth@yale.edu
;************************************************************

; Computer science topics: running times of programs, insertion sort,
; merge sort.

; You may write auxiliary procedure(s) in addition to
; the one(s) specified in the problem.  Please include
; a comment for each one explaining its input and results.

;************************************************************

; Timing procedures.
; The Racket procedure (current-inexact-milliseconds)
; returns a real (in the sense of Racket) number representing
; the current time in milliseconds since midnight UTC, January 1, 1970.

;************************************************************
; ** problem 1 ** (10 points)

; Write two procedures

; (lorint count bound)
; (time-calls reps proc args)

; (lorint count bound) takes a nonnegative
; integer count and a positive integer bound
; and returns a list of count randomly chosen integers 
; in the range from 0 through bound - 1.

; (time-calls reps proc args) takes 
; a nonnegative integer, reps,
; a procedure, proc,
; a list of arguments for the procedure, args,
; and returns the amount of time in SECONDS elapsed
; in calling proc on args a number of times equal to reps.

; Recall that we can apply a proc to args with (apply proc args).
; Note that a millisecond is 1/1000 of a second.

; Examples of lorint
;> (lorint 10 100)
;'(49 14 28 15 12 80 33 69 18 57)
;> (lorint 10 3)
;'(0 0 2 1 0 0 1 2 0 1)

; The following examples of time-calls were run on my workstation and
; show that calling the built-in plus procedure 10,000 times on
; the arguments 13 and 14 took somewhat more than 0.001 seconds,
; while doing the same thing 100,000 times took somewhat more
; than 0.01 seconds, and a million times took somewhat more than 0.1
; seconds.  The first two runs show random variation in the measured times.

; When the number of repetitions is multiplied by 10, the time is
; also (approximately) multiplied by 10.

;> (time-calls 10000 + (list 13 14))
;0.00168701171875
;> (time-calls 10000 + (list 13 14))
;0.00122412109375
;> (time-calls 100000 + (list 13 14))
;0.012380859375
;> (time-calls 1000000 + (list 13 14))
;0.12706494140625

; The following examples show timings (on my workstation)
; for creating lists of 100,000 or 200,000 or 300,000
; random numbers in the range 0 to 9 inclusive.
; About a third of a second suffices in the last case.

;> (time-calls 1 lorint (list 100000 10))
;0.074503173828125
;> (time-calls 1 lorint (list 200000 10))
;0.19560009765625
;> (time-calls 1 lorint (list 300000 10))
;0.33381982421875
;******************************(******************************

(define (lorint count bound)
  (cond ((= count 0) '())
        (else (cons (random bound) (lorint (- count 1) bound)))))
         
(define (time-calls reps proc args)
  (let ([x (current-inexact-milliseconds)])
    (aux-time reps proc args)
    (/ (- (current-inexact-milliseconds) x) 1000)))

;this method 'apply's the proc to the args n number of times
;if n>0, proc is applied and the auxilarry method subtracts 1
;from n and calls itself again until n=0
(define (aux-time n proc args)
  (cond ((> n 0) (apply proc args) (aux-time (- n 1) proc args))))
;************************************************************
; ** problem 2 ** (15 points)
; For this problem, use your procedure time-calls
; to time the built-in Racket procedures:

; length, take, drop

; and report the following measurements, and answer the following questions.
; Comment out your responses with semicolons.

; For length, report measurements of 100 repetitions of calling length
; on a list of length k * 100,000 for k = 1,2,3,4.

; For take and drop, report measurements of 100 repetitions of calling take (or drop)
; on a list of length k * 100,000 for k = 1,2,3,4, with the number
; of elements to take (or drop) being half the length of the list.

; You may want to do several measurements because of random variation.

; For each procedure, is O(1) or O(n) a better description of the running
; time as a function of the length n of the list argument?  Compare the
; times taken by the three procedures on comparable inputs -- which is 
; fastest? slowest?  Can you explain *why* on the basis of how lists and
; their operations are implemented?  (Complex statistical analysis is not
; necessary.)
;************************************************************

;Each trial is being run 100 times, so each reult has been divided by 100:

;LENGTH
;> (time-calls 100 length (list (lorint 100000 10))) / 100 = 
;0.0003150048828125
;> (time-calls 100 length (list (lorint 200000 10))) / 100 = 
;0.00071239501953125
;> (time-calls 100 length (list (lorint 300000 10))) / 100 = 
;0.0009680322265625
;> (time-calls 100 length (list (lorint 400000 10))) / 100 = 
;0.0013064111328125

;TAKE
;> (time-calls 100 take (list (lorint 100000 10) 50000)) / 100 = 
;0.0038894091796875
;> (time-calls 100 take (list (lorint 200000 10) 100000)) / 100 = 
;0.0134447265625
;> (time-calls 100 take (list (lorint 300000 10) 150000)) / 100 = 
;0.021286647921875
;> (time-calls 100 take (list (lorint 400000 10) 200000)) / 100 = 
;0.02601688720703125

;DROP
;> (time-calls 100 drop (list (lorint 100000 10) 50000)) / 100 = 
;0.0001704443359375
;> (time-calls 100 drop (list (lorint 200000 10) 100000)) / 100 = 
;0.00039106689453125
;> (time-calls 100 drop (list (lorint 300000 10) 150000)) / 100 = 
;0.00059684814453125
;> (time-calls 100 drop (list (lorint 400000 10) 200000)) / 100 = 
;0.000832158203125

;The best decriptors:
;Length is O(n). Take is O(1). Drop is O(1).

;Looking at comparable inputs, take is the slowest and drop is the fastest.

;For length, you must run through the entire list to see how many elements there are, so it is O(n).
;For Take, you must run though a certain number of elements to produce your final result. The number
;of elements doesn't depend on the length of the list, but rather the second argument given. Therefore,
;the complexity is O(1).
;For Drop, you must run through a certain elements to get to the final part of the list you want to keep.
;The number of elements doesn't depend on the length of the list, but rather the second argument given. Therefore,
;the complexity is O(1).
;For take and drop, their efficiency is not relative to the length of the list, but rather varies with the second argument,
;which states how many elements you must pass through.
;Take is the slowest, because it must create a new list of all the elements taken off of the original list.
;Drop is the fastest, because it only goes through a certain number of the elements,
;and prints off the rest of the list. In most cases, you don't need to go through every element.


;************************************************************
; We represent a total ordering on a set X of values via a predicate
; (compare? x y), that returns #t or #f.  The results must
; satisfy the following properties for all values x, y, z from the set X:
; (1) if (equal? x y) => #t then (compare? x y) => #t,
;* (1) (if (equal? x y) 
          ; (compare? x y) 
           ;#t)
; (2) if (and (compare? x y) (compare? y x)) => #t, then (equal? x y) => #t,
;* (2) (if (and (compare? x y) (compare? y x)) 
          ; (equal? x y) 
          ; #t)
; (3) if (and (compare? x y)(compare? y z)) => #t, then (compare? x z) => #t,
;* (3) (if (and (compare? x y)(compare? y z)) 
          ; (compare? x z) 
          ; #t)
; (4) (or (compare? x y) (compare? y x)) => #t.
;* (4) (or (compare? x y) (compare? y x))

; If the set X is finite, then we can write a procedure to test
; whether all these properties hold of a proposed total ordering compare? 
; on the set X.  This is what the next problem asks you to do.
; Note that you do NOT need to complete this problem before doing
; the subsequent ones.

;************************************************************
; ** problem 3 ** (10 points)
; Write one procedure

; (total-order? compare? domain)

; that takes a predicate (compare? x y) and a list of values domain
; such that whenever x and y are values from domain, (compare? x y)
; returns either #t or #f.
; The procedure returns #t if compare? is a total order on domain
; (that is, satisfies the four properties above for all x, y, z from domain),
; and #f otherwise.

; Hint: it might be helpful to write a procedure to check these conditions
; one pair x, y at a time.

; QUESTION: What is the running time of your procedure in terms of n,
; the number of elements in the domain.  Assume compare? takes time O(1).
; Give your answer in terms of O, Theta, or Omega, as appropriate and
; explain why it is correct.  Comment out your answer with semicolons.

; Examples
;> (total-order? <= '(1 3 5 4))
;#t
;> (total-order? < '(1 3 5 4))
;#f
;> (total-order? >= '(3 2 4 5 1))
;#t
;> (total-order? string<=? (list "hi" "hey" "hello"))
;#t
;> (total-order? equal? (list "hi" "hey" "hello"))
;#f
;************************************************************

(define (total-order? compare? domain)
  (let* ([double (calls-make-lst-2 '() domain '())]
        [triple (triple-lst domain double '())])
    (and (run-through-doub compare? double) (run-through-trip compare? triple))))

;given a list of all the possible doubles that can be made from the domain,
;if the first, second, and fourth conditions of total order are satisified,
;it calls itself again with the cdr of lst. If any of these conditions
;returns false, the procedure halts and returns false.
;if lst is empty (so, it has run through all of the doubles), it returns true.
(define (run-through-doub compare? lst)
  (cond ((empty? lst) #t)
        ((and (proc1 compare? (car lst)) (proc2 compare? (car lst)) (proc4 compare? (car lst))) (run-through-doub compare? (cdr lst)))
        (else #f)))
;given a list of all the possible triples that can be made from the domain,
;if the first, second, and fourth conditions of total order are satisified,
;it calls itself again with the cdr of lst. If any of these conditions
;returns false, the procedure halts and returns false.
;if lst is empty (so, it has run through all of the triples), it returns true.
(define (run-through-trip compare? lst)
  (cond ((empty? lst) #t)
        ((proc3 compare? (car lst)) (run-through-trip compare? (cdr lst)))
        (else #f)))

;creates a list of all the doubles that can be created from the domain.
;the method takes 3 lists.
;lstb contains all the elements before the item we're looking at
;lsta contains the current item we're looking at (car lsta) in addition to every elememt after it
;final contains all the appends of calls to make-lst-2
;how lsta and lstb work:
;      lstb       lsta
;1.    '()      '(1 2 3)
;2.   '(1)       '(2 3)
;3.  '(1 2)       '(3)
(define (calls-make-lst-2 lstb lsta final);(calls-make-lst-2 '() '(1 2 3 4) '())
  (cond ((empty? lsta) final)
        (else (calls-make-lst-2 (append lstb (list (car lsta))) (cdr lsta) (append final (make-lst-2 (append lstb lsta) (car lsta) '()))))))

;cons it with every element in the domain
;takes in a list of all the elements in the domain (before).
;the procedure cons it to the car of before and adds it to the list final
;returns final once the method has cycled through all values
(define (make-lst-2 before it final)
  (cond ((empty? before) (reverse final))
        (else (make-lst-2 (cdr before) it (cons (list it (car before)) final)))))

;calls add-on with each item in the domain and the list of doubles
;takes the domain, the list of double already assembled, and an empty final list that will eventually be returned
(define (triple-lst lst double final);(triple '(1 2 3 4) (calls-make-lst-2 '() '(1 2 3 4) '()) '())
  (cond ((empty? lst) final)
        (else (triple-lst (cdr lst) double (append final (add-on double (car lst) '()))))))

;appends item onto each element in the list of doubles, to return a list of all the possible triples
(define (add-on lst item final);(add-on '((1 1) (1 2) (1 3) (1 4) (2 1) (2 2) (2 3) (2 4) (3 1) (3 2) (3 3) (3 4) (4 1) (4 2) (4 3) (4 4)) 1 '())
  (cond ((empty? lst) final)
        (else (add-on (cdr lst) item (append final (cons (reverse (cons item (reverse (car lst)))) '()))))))

;tests whether the given double statisfies the first condition of total-order
(define (proc1 compare? lst)
  (if (equal? (first lst) (last lst)) 
      (compare? (first lst) (last lst)) 
      #t))

;tests whether the given double statisfies the second condition of total-order
(define (proc2 compare? lst)
  (if (and (compare? (first lst) (last lst)) (compare? (last lst) (first lst))) 
      (equal? (first lst) (last lst)) 
      #t))

;tests whether the given triple statisfies the third condition of total-order
(define (proc3 compare? lst)
  (if (and (compare? (car lst) (car (cdr lst)))(compare? (car (cdr lst)) (last lst)))
      (compare? (car lst) (last lst))
      #t))

;tests whether the given double statisfies the fourth condition of total-order
(define (proc4 compare? lst)
  (or (compare? (car lst) (last lst)) (compare? (last lst) (car lst))))
;************************************************************

; Now we turn to sorting a list of elements with respect to a given
; comparison operator.  You don't need to have done problem 3 to
; do the following problems.

;************************************************************
; ** problem 4 ** (15 points)
; Write three procedures

; (sorted? compare? lst)
; (insert compare? item lst)
; (merge compare? lst1 lst2)

; For each of these procedures, you may assume that
; compare? is a total order on the elements of lst,
; item and the elements of lst, and the elements of lst1 and lst2,
; respectively.

; (sorted? compare? lst)
; takes a list of items and returns #t or #f
; depending on whether the items of lst are
; sorted with respect to the comparison predicate
; compare?
; In other words, the result should be #f if and only if
; there are two consecutive elements of lst for
; which compare? returns #f.

; (insert compare? item lst)
; inserts an item into a list lst of items
; which is sorted with respect to the compare?
; predicate, so that the resulting list of
; items is also sorted with respect to the
; compare? predicate.

; (merge compare? lst1 lst2)
; takes two lists of elements lst1 and lst2, each of which is sorted with
; respect to the compare? predicate, and produces as its result a list
; of all the items in lst1 and lst2 (preserving duplicates) that is
; sorted with respect to compare?

; Examples
;> (sorted? <= '(1 4 5 8 10))
;#t
;> (sorted? >= '(10 9 4 7 6))
;#f
;> (insert <= 3 '(1 2 4 5))
;'(1 2 3 4 5)
;> (insert string>=? "hello" (list "the" "best" "arrangment"))
;'("the" "hello" "best" "arrangment")
;> (merge >= '(10 7 4 2 1) '(22 9 5))
;'(22 10 9 7 5 4 2 1)
;> (merge string<=? (list "a" "novel" "thought") (list "more" "predictive"))
;'("a" "more" "novel" "predictive" "thought")
;************************************************************

(define (sorted? compare? lst)
  (cond ((<= (length lst) 1) #t)
        ((compare? (first lst) (second lst)) (sorted? compare? (cdr lst)))
        (else #f)))

(define (insert compare? item lst)
  (insert-help compare? item lst '()))

;does the same thing as insert, but takes an additional argument (final)
;final contains all the elements that come before the item is able to be inserted
(define (insert-help compare? item lst final)
  (cond ((empty? lst) (append final (list item)))
        ((compare? item (car lst)) (append final (list item) lst))
        (else (insert-help compare? item (cdr lst) (append final (list (car lst)))))))
  
(define (merge compare? lst1 lst2)
  (if (>= (length lst1) (length lst2))
      (merge-help compare? lst1 lst2 '())
      (merge-help compare? lst2 lst1 '())))

;does the same thing as merge, but takes an additional argument (final) and makes the longer list lst1 (so there is less merging)
;final contains all the elements from lst1 that come before the item is able to be inserted
(define (merge-help compare? lst1 lst2 final)
  (cond ((empty? lst2) (append final lst1))
        ((empty? lst1) (append final lst2))
        ((compare? (car lst2) (car lst1)) (merge-help compare? lst1 (cdr lst2) (append final (list (car lst2)))))
        (else (merge-help compare? (cdr lst1) lst2 (append final (list (car lst1)))))))
;************************************************************
; ** problem 5 ** (10 points)
; Write two procedures

; (isort compare? lst)
; (msort compare? lst)

; Each takes a total order comparison predicate compare? and a list
; lst of items, and returns a list of all the elements in lst (duplicates
; preserved) arranged so that they are sorted with respect to compare?

; (isort compare? lst) should use (insert compare? item lst) and
; should implement insertion sort.

; (msort compare? lst) should use (merge lst1 lst2) and should
; implement merge sort.

; Examples
;> (isort string<=? (list "predictive" "novel" "a" "more" "thought"))
;'("a" "more" "novel" "predictive" "thought")
;> (msort string>=? (list "predictive" "novel" "a" "more" "thought"))
;'("thought" "predictive" "novel" "more" "a")
;************************************************************

(define (isort compare? lst)
  (cond ((empty? lst) '())
        (else (insert compare? (car lst) (isort compare? (cdr lst))))))

(define (msort compare? lst)
  (let ([len (length lst)])
        (msort-help compare? lst len)))

;this does exactly what msort is supposed to do, but I wanted to pass in the variable,
;len (which gives the length of the list), so I made an new method that takes an additional variable.
(define (msort-help compare? lst len)
;  (display lst)
;  (display "\n")
  (cond
    [(<= len 0) '()]
    [(<= len 2) (merge compare? (list (car lst)) (cdr lst))]
    ;((<= (length lst) 2) (merge compare? (list (car lst)) (cdr lst)))
    [else (merge compare?
                 (msort-help compare? (take lst (quotient len 2)) (quotient len 2))
                 (msort-help compare? (drop lst (quotient len 2)) (- len (quotient len 2))))]))

;************************************************************
; ** problem 6 ** (20 points)
; By using sufficiently long lists of integers,
; and possibly repeating and averaging measurements, 
; give empirical evidence for the claims that:
; (1) your implementation of insertion sort (isort, above) has best
; case time Theta(n) and worst case time of Theta(n^2).
; (2) your implementation of merge sort (msort, above) has best case and
; worst case times of Theta(n log n).

; Please identify inputs that give best and worst cases for your
; implementations of isort and msort.

; QUESTION: Empirically, what do the average running times of isort and
; msort seem to be?  Be sure that you use sufficiently long lists of
; randomly chosen integers in a range larger than the length of the list,
; so that there are unlikely to be many duplicate values.

; QUESTION: Roughly what is the longest list of random integers that your isort
; procedure can sort in 10 seconds?  Same question for your msort procedure?

; Because of memory caching and other effects, the timing behaviors will not
; necessarily uniform over the whole range of feasible input lengths.
;************************************************************
;Each trial is being run 5 times, so the result has been divided by 5.

;ISORT - SORTED - This is roughly inscreasing by theta(n).
;There is a linear relationship (theata(n)) between each test.
;For example, from a list of 50,000 to a list of 100,000, the time taken to
;construct the list doubles (just like the number of elements in the list).
;This makes sense because if all the numbers are already in order,
;isort just goes through the entire sorted list once.
;> (time-calls 5 isort (list <= (build-list 500 values))) / 5 =
;0.00020073242187499998
;> (time-calls 5 isort (list <= (build-list 1000 values))) / 5 =
;0.000200537109375
;> (time-calls 5 isort (list <= (build-list 10000 values))) / 5 =
;0.00441123046875
;> (time-calls 5 isort (list <= (build-list 50000 values))) / 5 =
;0.040507080078125
;> (time-calls 5 isort (list <= (build-list 100000 values))) / 5 =
;0.095258056640625

;ISORT - UNSORTED (I know it's unsorted because build-list is reversed)
;I did not go futher than 5000 calls, because it took too much time.
;However, we can see that the time it takes to run each procesdure
;increases very quickly when compared to the sorted insertion sort,
;The efficiency of this worst case isort is theta(n^2); in the tests
;we can see that as n doubles, the amount of time it takes to run
;the experiment about quadruples, indicating theta(n^2) efficiency.
;This makes sense because isort cycles through every element in the unsorted
;list. Then, for each of these elements, isort must then go through the
;list AGAIN to determine where to place the element, which means it is O(n^2)
;efficient.
;> (time-calls 5 isort (list <= (reverse (build-list 500 values)))) / 5 =
;0.921743798828125
;(time-calls 5 isort (list <= (reverse (build-list 1000 values)))) / 5 =
;6.1848108886718745
;> (time-calls 5 isort (list <= (reverse (build-list 5000 values)))) / 5 =
;407.1110034179688

;MSORT - SORTED
;> (time-calls 5 msort (list <= (build-list 500 values))) / 5 =
;0.00441689453125
;> (time-calls 5 msort (list <= (build-list 1000 values))) / 5 =
;0.012840771484375
;> (time-calls 5 msort (list <= (build-list 5000 values))) / 5 =
;0.229209521484375
;> (time-calls 5 msort (list <= (build-list 10000 values))) / 5 =
;0.817080908203125
;> (time-calls 5 msort (list <= (build-list 50000 values))) / 5 =
;14.47252593626293

;MSORT - UNSORTED (I know it's unsorted because build-list is reversed)
;> (time-calls 5 msort (list <= (reverse (build-list 500 values)))) / 5 =
;0.0036095214843750003
;> (time-calls 5 msort (list <= (reverse (build-list 1000 values)))) / 5 =
;0.009431396484375
;> (time-calls 5 msort (list <= (reverse (build-list 5000 values)))) / 5 =
;0.22850683593749999
;> (time-calls 5 msort (list <= (reverse (build-list 10000 values)))) / 5 =
;0.903148583984375
;> (time-calls 5 msort (list <= (reverse (build-list 50000 values)))) / 5 =
;16.6950341796875

;Both msort trials (sorted and unsorted) produce fairly similar numbers for each call to time-calls.
;The numbers they produce are higher than the sorted isort (theta(n)), and
;the numbers are not increasing linearly. Also, the results are much
;lower than the completely unsorted isort (theta(n^2)), so the efficiency
;must be better than that. Logicaly, this leaves us with theta(n log(n))
;(the only efficiency that falls between n and n^2). This makes
;sense, because while the amount of time each call takes grows as the list
;of elements does, their relationship is greater than linear and much, much less than
;n^2. It makes sense that msort is nlogn, because nlogn implies that the data is repeatedly
;being cut in half again, and agian, and again, and each half is being individually
;processed. This is exactly what is happening in msort.
;the first half and the second half of each list are used to call the procedure again
;until there are 2 or fewer elements in the list.

;ISORT - Best case: every item is already sorted, and nothing needs to be moved.
;        Worst case: the list is in the reverse of sorted (ie. if compare is <=,
;                    a worst case scenario list would be '(9 8 7 5).
;MSORT - The best case and worst case are the same. No matter what, merge sorts
;                    two lists, one element at a time, so the efficiency is the same.


;ISORT - AVERAGE:
;> (time-calls 5 isort (list <= (lorint 50 10000000))) / 5 =
;0.00020068359375000002
;> (time-calls 5 isort (list <= (lorint 500 10000000))) / 5 =
;0.24134355468750002
;> (time-calls 5 isort (list <= (lorint 1000 10000000))) / 5 =
;1.3215357421875
;> (time-calls 5 isort (list <= (lorint 1500 10000000))) / 5 =
;4.184970129248545
;> (time-calls 5 isort (list <= (lorint 2000 10000000))) / 5 =
;8.95845336914063

;MSORT - AVERAGE:
;> (time-calls 5 msort (list <= (lorint 50 10000000))) / 5 =
;0.000143798828125
;> (time-calls 5 msort (list <= (lorint 500 10000000))) / 5 =
;0.0110296875
;> (time-calls 5 msort (list <= (lorint 1000 10000000))) / 5 =
;0.033489453125
;> (time-calls 5 msort (list <= (lorint 1500 10000000))) / 5 =
;0.071986376953125
;(time-calls 5 msort (list <= (lorint 2000 10000000))) / 5 =
;0.125940087890625

;The average run time of isort is O(n^2). This amount of time
;    for each call is increasing incredibly quickly. You can see as
;    the number of elements in the list double from 1000 to 2000,
;    the time about quadruples. It makes sense the it is O(n^2) for the
;    average case, because isort will still pass through the entire list
;    and most likely need to move each element along a portion of the list
;    to put it in the correct place, which is O(n^2).
;The average run time of msort is O(n log(n)). As mentioned above,
;    The numbers msort produces are higher than the sorted isort (O(n)), and
;    the numbers are not increasing linearly. Also, the results are much
;    lower than the completely unsorted isort (O(n^2)), so the efficiency
;    must be better than that. This leaves us with O(n log(n)). This makes
;    sense, because while the amount of time each call takes grows as the list
;    of elements does, their relationship is greater than linear and much, much less than
;    n^2.It makes sense that msort is nlogn, because nlogn implies that the data is repeatedly
;    being cut in half again, and agian, and again, and each half is being individually
;    processed. This is exactly what is happening in msort.
;    the first half and the second half of each list are used to call the procedure again
;    until there are 2 or fewer elements in the list.


;Roughly the longest list of random integers that your isort/msort
;procedure can sort in 10 seconds (10 tests were run for each call
;to make sure they consistently returned around 10 seconds):
;isort: (time-calls 1 isort (list <= (lorint 2000 10))) - Returned: 10.194572021484374
;msort: (time-calls 1 msort (list <= (lorint 20000 10))) - Returned: 10.0754775390625

;************************************************************
; ** problem 7 ** (10 points)
; Write one procedure

; (count-compares sort compare? lst)

; that returns the number of calls to the compare? procedure
; when we apply the procedure sort to the arguments compare? and lst.
; Think of sort as a procedure like msort or isort, taking a comparison
; predicate and a list as its arguments, though sort could
; be some other sorting procedure devised for testing purposes.

; The trick here is to take the compare? procedure and "wrap" it
; in another procedure that will count the number of times it
; is called.  Then call sort with the "wrapped" compare? and lst
; as inputs.  Finally, return the final count from the "wrapped"
; compare? as the value from count-compares.

; Please read about the mutator set! to help you keep the count.

; Examples (yours may randomly vary.)
;> (count-compares msort <= (lorint 10 100))
;23
;> (count-compares msort <= (lorint 10 100))
;22
;> (count-compares isort <= (lorint 10 100))
;34
;************************************************************
(define count 0)

(define (count-compares sort compare? lst)
  (set! count 0)
  (apply sort (list (lambda (x y) (set! count (+ count 1)) (compare? x y)) lst)) count)

;************************************************************
; ** problem 8 ** (0 points)
; This is where the test code normally appears.
; For this assignment, write your own tests.  

(define *testing-flag* #t)
(define error display)  ;; turn off error messages

(define (test name got expected)
  (cond (*testing-flag*
	 (let* ((expected (if (procedure? expected)
			      (and (expected got) 'OK-TEST)
			      expected))
		(prefix (if (equal? got expected)
			    '***OK***
			    '---X---)))
	   (list 'testing name prefix 'got: got 'expected: expected)))))


;********* end of hw8, end of hws! **************************


