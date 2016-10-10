#lang r6rs
(import (rnrs base)
        (rnrs io simple))


;self-balancing splay trees in functional style
;splay trees: http://www.cs.cmu.edu/~sleator/papers/self-adjusting.pdf

;tree construction
(define null-tree '())
(define null-tree? null?)
(define (tree value left right)
  (list value left right))
(define value car)
(define left cadr)
(define right caddr)
(define (leaf? node)
  (and (null-tree? (left node)) (null-tree? (right node))))

;splay operations
(define (zig grandparent parent node)
  (tree (value node)
        (left node)
        (tree (value parent)
              (right node)
              (right parent))))

(define (zigzig grandparent parent node)
  (tree (value node)
        (left node)
        (tree (value parent)
              (right node)
              (tree (value grandparent)
                    (right parent)
                    (right grandparent)))))

(define (zigzag grandparent parent node)
  (tree (value node)
        (tree (value parent)
              (left parent)
              (left node))
        (tree (value grandparent)
              (right node)
              (right grandparent))))

;symmetric cases
(define (zag grandparent parent node)
  (tree (value node)
        (tree (value parent)
              (left parent)
              (left node))
        (right node)))

(define (zagzag grandparent parent node)
  (tree (value node)
        (tree (value parent)
              (tree (value grandparent)
                    (left grandparent)
                    (left parent))
              (left node))
        (right node)))

(define (zagzig grandparent parent node)
  (tree (value node)
        (tree (value grandparent)
              (left grandparent)
              (left node))
        (tree (value parent)
              (right node)
              (right parent))))

;splay auxiliary functions
(define (identity grandparent parent node)
  node)
(define (false . ignore) #f)
(define (return arg . ignore) arg)

;splay table
;         \  grandparent
; parent   \ root (1)  left (3)    right (5)
; root  (0)  identity  impossible  impossible 
; left  (1)  zig       zigzig      zagzig
; right (2)  zag       zigzag      zagzag

(define splay-table  
  (vector    ; parent + grandparent
   false     ; 0 + 0 -> impossible
   identity  ; 1 + 0
   zig       ; 1 + 1
   zag       ; 1 + 2 (0 + 3 -> impossible)
   zigzig    ; 3 + 1
   zigzag    ; 3 + 2 (0 + 5 -> impossible)
   zagzig    ; 5 + 1
   zagzag))  ; 5 + 2

;directions
(define at-root  0)
(define go-left  1)
(define go-right 2)

(define (splay-ref grandparent parent)
  ;implements 2D array
  (vector-ref splay-table (+ 1 (* 2 grandparent) parent)))

;splay
;main operation
(define (splay root key <<? ==? succeed fail)
  (let splay   ;-)
    ((grandparent root)
     (parent root)
     (node root)
     (grandparent-edge at-root)
     (parent-edge at-root)
     (grandparent-continue return)
     (parent-continue return))
    (let* ((splay-operation (splay-ref grandparent-edge parent-edge))
           ;continuation wraps the splay operation
           (node-continue (lambda (new-tree)
                            ;debug
                            ;(display splay-function)
                            ;(display new-tree)
                            ;(newline)
                            (grandparent-continue (splay-operation grandparent parent new-tree)))))
      (cond
        ((null-tree? node)
         ;capture failure (see split and insert)
         (let ((result (fail)))
           (if result (node-continue result) result)))
        ((==? key node)
         ;call continuation chain of splay functions
         ;to move the element to root
         (node-continue (succeed node)))
        ((<<? key node)
         ;recursive call left, pass new continuation
         (splay parent node (left node) parent-edge go-left parent-continue node-continue))
        (else
         ;recursive call right, pass new continuation
         (splay parent node (right node) parent-edge go-right parent-continue node-continue))))))

;join
;assume tree-a << tree-b
(define (join tree-a tree-b <<? ==?)
  ;splay at greatest element in tree-a
  (let ((result (splay tree-a
                       '()
                       ;<<? always returns false
                       false
                       ;identify rightmost node
                       (lambda (key node) (null-tree? (right node)))
                       ;append tree-b as right child
                       (lambda (node) (tree (value node) (left node) tree-b))
                       false)))
    (if result result tree-b)))

;split(t, i)
;return list of two trees
(define (split tree-a key <<? ==?)
  (let ((result (splay tree-a
                       key
                       <<?
                       (lambda (key node)
                         (or (==? key node)
                             (leaf? node)))
                       return
                       false)))
    (if result
        (let ((value (value result))
              (left (left result))
              (right (right result)))
          (if (<<? key result)
              (list left
                    (tree value null-tree right))
              (list (tree value left null-tree)
                    right)))
        (list null-tree null-tree))))

;locate
;return tree with the key at the root or false
(define (locate tree key <<? ==?)
  (splay tree key <<? ==? return false))

;delete
;return tree without key element
(define (delete tree key <<? ==?)
  (let ((result (locate tree key <<? ==?)))
    (if result (join (left result) (right result) <<? ==?) tree)))

;insert
;return tree with updated or inserted key at the root
(define (insert node key <<? ==?)
  (splay node
         key
         <<?
         ==?
         (lambda (node) (tree key (left node) (right node)))
         (lambda () (tree key null-tree null-tree))))

;;test
;(define ==? (lambda (key node) (= key (value node))))
;(define <<? (lambda (key node) (< key (value node))))
;
;(define i (tree 2 null-tree null-tree))
;(define j null-tree)
;
;(join j i <<? ==?)
;;(2 () ())
;(join i j <<? ==?)
;;(2 () ())
;(split i 2 <<? ==?)
;;((2 () ()) ())
;(insert j 3 <<? ==?)
;;(1 () ())
;(delete i 4 <<? ==?)
;;()
;(delete j 5 <<? ==?)
;;()
;(locate i 6 <<? ==?)
;;#f
;
;(define k (tree 5 (tree 1 null-tree (tree 2 null-tree (tree 3 null-tree (tree 4 null-tree null-tree)))) (tree 6 null-tree (tree 7 null-tree (tree 8 null-tree (tree 9 null-tree (tree 10 null-tree null-tree)))))))