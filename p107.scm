;; 2010-08-27
;; 
;; Yuri Arapov <yuridichesky@gmail.com>
;; 
;; Project Euler
;; 
;; http://projecteuler.net/index.php?section=problems&id=107
;;
;; Problem 107
;; 21 October 2005
;;
;; The following undirected network consists of seven vertices and twelve edges
;; with a total weight of 243.
;;
;; The same network can be represented by the matrix below.
;;      A   B   C   D   E   F   G
;;      A   -   16  12  21  -   -   -
;;      B   16  -   -   17  20  -   -
;;      C   12  -   -   28  -   31  -
;;      D   21  17  28  -   18  19  23
;;      E   -   20  -   18  -   -   11
;;      F   -   -   31  19  -   -   27
;;      G   -   -   -   23  11  27  -
;;
;; However, it is possible to optimise the network by removing some edges and
;; still ensure that all points on the network remain connected. The network
;; which achieves the maximum saving is shown below. It has a weight of 93,
;; representing a saving of 243 − 93 = 150 from the original network.
;;
;; Using network.txt (right click and 'Save Link/Target As...'), a 6K text file
;; containing a network with forty vertices, and given in matrix form, find the
;; maximum saving which can be achieved by removing redundant edges whilst
;; ensuring that the network remains connected.
;;
;; Answer: 259679
;;
;; NOTE: It's implementation of Kruskal's minimum spanning tree 
;;       algorithm, see http://en.wikipedia.org/wiki/Kruskal%27s_algorithm


(load "read-file.scm")


(define (make-matrix s)        (list->vector (map list->vector s)))
(define (matrix-size n)        (vector-length n))
(define (matrix-ref n row col) (vector-ref (vector-ref n row) col))


;; Merge sorted lists.
;; The < operator must be defined for lists elements.
;;
(define (merge-sorted s1 s2)
  (let loop ((s1 s1)
             (s2 s2)
             (res '()))
    (cond
      ((and (null? s1) (null? s2))  (reverse res))
      ((null? s1)                   (loop s1 (cdr s2) (cons (car s2) res)))
      ((null? s2)                   (loop (cdr s1) s2 (cons (car s1) res)))
      (else
        (if (< (car s1) (car s2))
          (loop (cdr s1) s2 (cons (car s1) res))
          (loop s1 (cdr s2) (cons (car s2) res)))))))


;;
;; NOTE: tree is a vector of sorted vertices.
;;

;; Make a tree containing single vertex.
;;
(define (make-tree vertex)
  (make-vector 1 vertex))



;; Test if given tree contains given vertex.  Binary search.
;;
(define (tree-contains? tree vertex)
  (let loop ((l 0)
             (r (1- (vector-length tree))))
    (if (< r l)
      #f
      (let* ((c (quotient (+ l r) 2))
             (v (vector-ref tree c)))
        (if (= vertex v)
          #t
          (if (< vertex v)
            (loop l (if (= r c) (1- c) c))
            (loop (if (= l c) (1+ c) c) r)))))))


;; Merge two trees into single one.
;;
(define (tree-merge t1 t2)
  (list->vector (merge-sorted (vector->list t1) (vector->list t2))))


;; Test if trees are the same.
;;
(define (tree=? t1 t2)
  (= (vector-ref t1 0) (vector-ref t2 0)))



;; Edge is a 3-element list: distance, vertex from, vertex to.
;;
(define (make-edge dist from to)    (list dist from to))
(define (edge-dist e)               (car e))
(define (edge-from e)               (cadr e))
(define (edge-to   e)               (caddr e))



;; Turn distance matrix into list of edges.
;;
(define (distance-matrix->edges dm)
  (let ((size (matrix-size dm)))
    (let loop ((row 0)
               (col 1)
               (res '()))
      (cond
        ((= row size)   (reverse res))
        ((= col size)   (loop (+ row 1) (+ row 2) res))
        (else
          (let ((dist (matrix-ref dm row col)))
            (if (not dist)
              (loop row (1+ col) res)
              (loop row (1+ col) (cons (make-edge dist row col) res)))))))))



;; Search forest for a tree containing given vertex.
;;
(define (find-tree forest vertex)
  (find (lambda (tree) (tree-contains? tree vertex)) forest))



;; Merge trees t1 and t2 in given forest: replace t1 and t2 with
;; result of their merge.
;;
(define (forest-merge forest t1 t2)
  (cons
    (tree-merge t1 t2)
    (filter (lambda (tree) (and (not (tree=? tree t1))
                                (not (tree=? tree t2))))
            forest)))


;; Kruskal's minimum spanning tree algorithm.
;;
(define (min-spanning-tree distance-matrix)
  (let ((edges  ;; initial edges sorted by distance in ascending order
          (sort
            (distance-matrix->edges distance-matrix)
            (lambda (i j) (< (edge-dist i) (edge-dist j)))))

        (forest ;; initial forest: one vertex per tree
          (map make-tree (iota (matrix-size distance-matrix)))))

    (let loop ((forest forest)
               (edges  edges)
               (mst    '()))
      (if (null? edges)
        mst
        (let* ((e  (car edges))
               (v1 (edge-from e))
               (v2 (edge-to e))
               (t1 (find-tree forest v1))
               (t2 (find-tree forest v2)))
          (if (tree=? t1 t2)
            (loop  ;; edge e belongs to single tree: ignored
              forest 
              (cdr edges) 
              mst)
            (loop  ;; edge e connects trees t1 and t2
              (forest-merge forest t1 t2) 
              (cdr edges)
              (cons e mst))))))))


;; Solve problem 107 on given distance matrix.
;; Example: (p107-int (test-network))
;;
(define (p107-int distance-matrix)

  (define (edges->dist edges)
    (fold (lambda (e res) (+ res (edge-dist e))) 0 edges))

  (let ((edges (distance-matrix->edges distance-matrix))
        (mst   (min-spanning-tree distance-matrix)))
    (- (edges->dist edges)
       (edges->dist mst))))



;; Solve problem 107.
;;
(define (p107) (p107-int (read-network "network.txt")))



;; Read network (distance matrix) from file.
;;
(define (read-network file)
  (make-matrix
    (read-file-with
      file
      (lambda (line)
        (map 
          (lambda (x)
            (if (string=? x "-")
              #f
              (string->number x)))
          (string-split line #\,))))))



(define (test-network)
  (make-matrix
    '((#f  16  12  21  #f  #f  #f)
      (16  #f  #f  17  20  #f  #f)
      (12  #f  #f  28  #f  31  #f)
      (21  17  28  #f  18  19  23)
      (#f  20  #f  18  #f  #f  11)
      (#f  #f  31  19  #f  #f  27)
      (#f  #f  #f  23  11  27  #f))))



;; end of file
;; vim: ts=4 sw=4
