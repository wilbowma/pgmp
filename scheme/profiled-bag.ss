(eval-when (compile) (compile-profile #f))
(module (bst-create-empty bst-traverse-inorder bst-add bst-member?)
  (define first car)
(define rest cdr)
;; Code below copied from http://www.ics.uci.edu/~kay/courses/141/bst.txt

;;; Binary Search Tree (BST)
;
; This code implements a simple BST of integers.
;
; Our binary tree will be represented as follows:
; either an empty list or a list with three elements:  
; the value, the left subtree, and the right subtree.
; So, the tree
;                      5
;                    /   \
;                  4       6
; would be (5 (4 () ()) (6 () ())).
;
; Note that only the six routines below (two constructors,
; one predicate, and three field-selectors) depend on the
; tree being implemented as described above.  All the rest
; of the routines are written in terms of these six, so if
; we chose to change the representation, we could do it by
; changing only these six routines.
;
; Remember as you read that these routines are written in 
; the functional style---they RETURN something (in many
; cases a binary search tree) rather than changing any 
; existing values in place.

(define bst-create-empty   ; constructor---create an empty BST
  (lambda ()
    '()))

(define bst-create         ; constructor
  (lambda (value left-subtree right-subtree)
    (list value left-subtree right-subtree)))

(define bst-isempty? (lambda (BST) (null? BST)))

; Field selector routines.  These make the rest of the code
; a lot clearer since we can say bst-left-subtree instead of
; (first (rest BST)) )
(define bst-value (lambda (BST) (first BST)))
(define bst-left-subtree (lambda (BST) (first (rest BST))))
(define bst-right-subtree (lambda (BST) (first (rest (rest BST)))))

(define bst-add    ; return tree with value added
  (lambda (BST value)
    (cond
      ((bst-isempty? BST)           ; if empty, create a new node
       (bst-create value (bst-create-empty) (bst-create-empty))) 
      ((< value (bst-value BST))    ; add node to left subtree 
       (bst-create (bst-value BST)  ; (functionally, by building new tree)
                   (bst-add (bst-left-subtree BST) value)
                   (bst-right-subtree BST)))
      ((> value (bst-value BST))    ; add node to right subtree
       (bst-create (bst-value BST)
                   (bst-left-subtree BST)
                   (bst-add (bst-right-subtree BST) value)))
      (else BST))))                 ; it's already there; don't do anything

(define bst-delete   ; return tree with value deleted
  (lambda (BST value)
    (cond
      ((bst-isempty? BST) (bst-create-empty))
      ((= value (bst-value BST)) (bst-delete-root BST))
      ((< value (bst-value BST))
       (bst-create (bst-value BST)
                   (bst-delete (bst-left-subtree BST) value)
                   (bst-right-subtree BST)))
      (else ; (> value (bst-value BST)) -- commented out for efficiency
        (bst-create (bst-value BST)
                    (bst-left-subtree BST)
                    (bst-delete (bst-right-subtree BST) value))))))

(define bst-delete-root   ; return tree with root deleted
  (lambda (BST)
    (cond
      ; If the root has no children, result is empty tree
      ((bst-isleaf? BST) (bst-create-empty))
      ; If the root has one child (right or left), 
      ; result is that child (and descendants)
      ((bst-isempty? (bst-left-subtree BST)) (bst-right-subtree BST))
      ((bst-isempty? (bst-right-subtree BST)) (bst-left-subtree BST))
      ; If the root has two children, 
      ; replace value with leftmost child of right subtree
      (else (let* ((replacement-value
                     (bst-value (bst-leftmost-child (bst-right-subtree BST))))
                   (new-right-subtree
                     (bst-delete (bst-right-subtree BST) replacement-value)))
              (bst-create replacement-value 
                          (bst-left-subtree BST) 
                          new-right-subtree))))))

(define bst-isleaf?
  (lambda (BST)
    (and (bst-isempty? (bst-left-subtree BST))
         (bst-isempty? (bst-right-subtree BST)))))

(define bst-leftmost-child
  (lambda (BST)
    (cond
      ((bst-isempty? BST) (bst-create-empty))
      ((bst-isempty? (bst-left-subtree BST)) BST)
      (else (bst-leftmost-child (bst-left-subtree BST))))))

(define bst-member?   ; find a value in the tree
  (lambda (BST value)
    (cond
      ((bst-isempty? BST) #f)
      ((= value (bst-value BST)) #t)
      ((< value (bst-value BST)) (bst-member? (bst-left-subtree BST) value))
      (else (bst-member? (bst-right-subtree BST) value)))))

(define bst-traverse-inorder   ; return an inorder list of values
  (lambda (BST)                ; We return a standard list so we can cross 
    (cond                      ; the abstraction barrier safely.  The calling
      ((bst-isempty? BST) '()) ; program can then process the list as it likes.
      (else (append
              (bst-traverse-inorder (bst-left-subtree BST))
              (list (bst-value BST)) ; each argument to append must be a list
              (bst-traverse-inorder (bst-right-subtree BST)))))))

(define bst-size  ; number of items in the tree
  (lambda (BST)
    (cond
      ((bst-isempty? BST) 0)
      (else (+ 1
               (bst-size (bst-left-subtree BST))
               (bst-size (bst-right-subtree BST)))))))

(define bst-height
  (lambda (BST)
    (cond
      ((bst-isempty? BST) -1) ; by definition empty tree's height is -1
      (else (+ 1 (max (bst-height (bst-left-subtree BST))
                      (bst-height (bst-right-subtree BST))))))))

(define bst-balanced?         ; check for perfect balance
  (lambda (BST)
    (cond
      ((bst-isempty? BST) #t)
      (else (and (= (bst-height (bst-left-subtree BST)) 
                           (bst-balanced? (bst-left-subtree BST))
                 (bst-balanced? (bst-right-subtree BST))))))))
  
(define bst-shortest-path-to-leaf
  (lambda (BST)
    (cond
      ((bst-isempty? BST) -1)
      (else (+ 1 (min (bst-shortest-path-to-leaf (bst-left-subtree BST))
                      (bst-shortest-path-to-leaf (bst-right-subtree BST))))))))
  
(define bst-relatively-balanced?
  (lambda (BST)
    (>= 1 (abs (- (bst-height BST) (bst-shortest-path-to-leaf BST))))))
  
; Notes
;
; The names of all the binary-search-tree functions above 
; start with "bst-".  This is just a matter of convention;
; it's not required.  In fact, it's possible to build
; object-oriented code in Scheme, where the operations 
; would be associated with each object so we wouldn't need
; either the "bst-" prefix or the BST parameter.
)
(eval-when (compile) (compile-profile #t))

(meta define (choose ht i1* i2*)
  ;; Pick a BST when we search more often than map; otherwise pick a list.
  (if (>= (hashtable-ref ht 'bag-member? 0)
        (hashtable-ref ht 'bag-map 0))
      i1*
      i2*))

(define-syntax profile-implement 
  (let ([ht (make-eq-hashtable)])
    (lambda (x)
      (syntax-case x (define)
        [(_ (define (name* args* ...) i1* i2*) ...)
         (andmap identifier? #'(name* ... args* ... ...))
         (with-syntax ()
           (for-each 
             (lambda (name i1 i2)
               (hashtable-set! ht (syntax->datum name)
                 (fx+ (profile-query-count-syntax i1) (profile-query-count-syntax i2))))
             #'(name* ...) #'(i1* ...) #'(i2* ...)) 
           (with-syntax ([(body* ...) (choose ht #'(i1* ...) #'(i2* ...))])
             #`(begin (define (name* args* ...) body*) ...)))]))))

(profile-implement 
  (define (bag)
    (bst-create-empty)
    (list))
  (define (bag-add x b) 
    (bst-add b x)
    (cons x b))
  (define (bag-map f b)
    (fold-left bst-add (bst-create-empty) (map f (bst-traverse-inorder b)))
    (map f b))
  (define (bag-member? x b)
    (bst-member? x b)
    (and (member x b) #t)))
