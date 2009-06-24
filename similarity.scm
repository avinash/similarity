;;; Similarity system
;;;
;;; Avinash Meetoo
;;; avinash@knowledge7.com
;;; Knowledge Seven Ltd
;;; 4 March 2008

#lang scheme

(require scheme/list)

;;; simple save and load functions
(define (save filename anything)
  (with-output-to-file filename (lambda () (write anything)) 'replace))

(define (load filename)
  (with-input-from-file filename (lambda () (read))))

;;; calculates the average rating given by each person
(define (people-average-rating people)
  (define (person-average-rating person)
    (list (car person) (/ (foldl + 0 (cdr person)) (length (cdr person)))))
  (map person-average-rating people))

;;; finds the most generous person (i.e. the one with the highest average rating)
(define (person-most-generous people)
  (define (more-generous p1 p2)
    (cond ((> (car (cdr p1)) (car (cdr p2))) p1)
          (else p2)))
  (cond ((null? people) '())
        (else (let* ((averages (people-average-rating people)))
                (foldl more-generous (car averages) (cdr averages))))))

;;; extracts the ratings given to one of the 10 songs by all people
(define (song-ratings people song)
  (define (song-rating p)
    (list-ref p song))
  (map song-rating people))

;;; returns the average rating obtained by one of the 10 songs
(define (song-average-rating people song)
  (/ (foldl + 0 (song-ratings people song)) (length people)))

;;; returns the average ratings obtained by all the 10 songs
(define (songs-average-rating people)
  (define (song-average-rating-helper song)
    (song-average-rating people song))
  (map song-average-rating-helper '(1 2 3 4 5 6 7 8 9 10)))

;;; returns the distance between two different persons
(define (euclidean-distance p1 p2)
  (define (square n)
    (expt n 2))
  (define (sum-of-squares s1 s2 acc)
    (cond ((null? s1) acc)
          (else (sum-of-squares (cdr s1) (cdr s2) (+ acc (square (- (car s1) (car s2))))))))
  (sqrt (sum-of-squares (cdr p1) (cdr p2) 0)))

;;; returns a list containing the euclidean distances between a specific person and all others
(define (similarity-scores name people)
  (let* ((specific-person (assoc name people))
         (people-filtered (filter (lambda (p) (not (equal? name (car p)))) people)))
    (map (lambda (p) (list (car p) (euclidean-distance p specific-person))) people-filtered)))

;;; finds the closest person (i.e. with the lowest similarity score) to a specific person
(define (closest-person name people)
  (define (closer p1 p2)
    (cond ((< (car (cdr p1)) (car (cdr p2))) p1)
          (else p2)))
  (cond ((null? people) '())
        (else (let* ((similarity (similarity-scores name people)))
                (foldl closer (car similarity) (cdr similarity))))))

;;; utility functions

;;; one-of-the-names
(define (one-of-the-names ratings list-of-ids)
  (car (list-ref ratings 
                 (modulo (foldl + 0 list-of-ids) (length ratings)))))

;;; generates a list of people with random ratings for 10 items
(define (generate-people)
  (let* ((acm-turing-winners '(adleman allen bachman backus blum brooks cerf clarke cocke codd cook corbato
                                       dahl dijkstra emerson engelbart feigenbaum floyd gray hamming hartmanis hoare
                                       hopcroft iverson kahan kahn karp kay knuth lampson mccarthy milner minsky naur 
                                       newell nygaard perlis pnueli rabin reddy ritchie rivest scott shamir sifakis 
                                       simon stearns sutherland tarjan thompson wilkes wilkinson wirth yao)))
    (define (random-value dummy)
      (round (* 10 (random))))
    (define (make-person name)
      (cons name (map random-value (vector->list (make-vector 10 0)))))
    (map make-person acm-turing-winners)))

;;; load "people" from disk to have something to play with
(define people (load "people.txt"))

(print (closest-person 'yao people))
