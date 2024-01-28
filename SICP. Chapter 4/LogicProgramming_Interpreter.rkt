#lang sicp

; This code implements a query interpreter. It will be a collection of procedures.

; Firstable, there will be included a collection of procedures from previous chapters that are necessary  

; From 3.3.3. Representing tables. 

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define get '())

(define put '())

; From 3.5. Streams

(define (stream-car stream) 
  (car stream))

(define (stream-cdr stream) 
  (force (cdr stream)))

(define (display-line x)
  (newline)
  (display x))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin 
        (proc (stream-car s))
        (stream-for-each proc 
                         (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc 
                    (map stream-cdr
                         argstreams))))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (stream-append s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (stream-append (stream-cdr s1) s2))))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))


; From 4.1. The Metacircular Evaluator.

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (prompt-for-input string)
  (newline) (newline)
  (display string) (newline))

; From Section 4.4.4 Implementing the Query System

(define input-prompt  ";;; Query input:")
(define output-prompt ";;; Query results:")

; The Driver Loop

(define (query-driver-loop)
  (prompt-for-input input-prompt)
  (let ((q (query-syntax-process (read))))
    (cond ((assertion-to-be-added? q)
           (add-rule-or-assertion! (add-assertion-body q))
           (newline)
           (display "Assertion added to data base.")
           (query-driver-loop))
          (else
           (newline)
           (display output-prompt)
           (display-stream
            (stream-map
             (lambda (frame)
               (instantiate q
                 frame
                 (lambda (v f)
                   (contract-question-mark v))))
             (qeval q (singleton-stream '()))))
           (query-driver-loop)))))

; Instantiation

(define (instantiate exp frame unbound-var-handler)
  (define (copy exp)
    (cond ((var? exp)
           (let ((binding (binding-in-frame exp frame)))
             (if binding
                 (copy (binding-value binding))
                 (unbound-var-handler exp frame))))
          ((pair? exp)
           (cons (copy (car exp)) (copy (cdr exp))))
          (else exp)))
  (copy exp))

; The Evaluator

(define (qeval query frame-stream) ; Inputs: a query and a stream of frames.
  (let ((qproc (get (type query) 'qeval)))
    (if qproc
        (qproc (contents query) frame-stream) ; Any query that is not identified as a special form is assumed to be a simple query.
        (simple-query query frame-stream)))) ; Output: a stream of extended frames.

; Simple queries

(define (simple-query query-pattern frame-stream)
  (stream-flatmap
   (lambda (frame)
     (stream-append-delayed
      (find-assertions query-pattern frame)
      (delay (apply-rules query-pattern frame))))
   frame-stream))

; Compound queries

(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (conjoin (rest-conjuncts conjuncts)
               (qeval 
                (first-conjunct conjuncts)
                frame-stream))))

(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave-delayed
       (qeval (first-disjunct disjuncts) frame-stream)
       (delay (disjoin (rest-disjuncts disjuncts)
                       frame-stream)))))

; Filters

(define (negate operands frame-stream)
  (stream-flatmap
   (lambda (frame)
     (if (stream-null? (qeval (negated-query operands)
                              (singleton-stream frame)))
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))

(define (lisp-value call frame-stream)
  (stream-flatmap
   (lambda (frame)
     (if (execute
          (instantiate
              call
            frame
            (lambda (v f)
              (error "Unknown pat var: LISP-VALUE" v))))
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))

(define (execute exp)
  (apply (eval (predicate exp) logic-programming-data-base) ; Here it goes the name of the data-base we are interested in working with
         (args exp)))

(define (always-true ignore frame-stream) frame-stream)

; Finding assertions by pattern matching

(define (find-assertions pattern frame)
  (stream-flatmap (lambda (datum) 
                    (check-an-assertion datum pattern frame))
                  (fetch-assertions pattern frame)))

(define (check-an-assertion assertion query-pat query-frame)
  (let ((match-result
         (pattern-match query-pat assertion query-frame)))
    (if (eq? match-result 'failed)
        the-empty-stream
        (singleton-stream match-result))))

(define (pattern-match pat dat frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? pat dat) frame)
        ((var? pat) (extend-if-consistent pat dat frame))
        ((and (pair? pat) (pair? dat))
         (pattern-match (cdr pat) 
                        (cdr dat)
                        (pattern-match (car pat)
                                       (car dat)
                                       frame)))
        (else 'failed)))

(define (extend-if-consistent var dat frame)
  (let ((binding (binding-in-frame var frame)))
    (if binding
        (pattern-match (binding-value binding) dat frame)
        (extend var dat frame))))

; Rules and Unification

(define (apply-rules pattern frame)
  (stream-flatmap (lambda (rule)
                    (apply-a-rule rule pattern frame))
                  (fetch-rules pattern frame)))

(define (apply-a-rule rule query-pattern query-frame)
  (let ((clean-rule (rename-variables-in rule)))
    (let ((unify-result
           (unify-match query-pattern
                        (conclusion clean-rule)
                        query-frame)))
      (if (eq? unify-result 'failed)
          the-empty-stream
          (qeval (rule-body clean-rule) (singleton-stream unify-result))))))

(define (rename-variables-in rule)
  (let ((rule-application-id (new-rule-application-id)))
    (define (tree-walk exp)
      (cond ((var? exp)
             (make-new-variable exp rule-application-id))
            ((pair? exp)
             (cons (tree-walk (car exp))
                   (tree-walk (cdr exp))))
            (else exp)))
    (tree-walk rule)))

(define (unify-match p1 p2 frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? p1 p2) frame)
        ((var? p1) (extend-if-possible p1 p2 frame))
        ((var? p2) (extend-if-possible p2 p1 frame))
        ((and (pair? p1) (pair? p2))
         (unify-match (cdr p1) 
                      (cdr p2)
                      (unify-match (car p1)
                                   (car p2)
                                   frame)))
        (else 'failed)))

(define (extend-if-possible var val frame)
  (let ((binding (binding-in-frame var frame)))
    (cond (binding
           (unify-match
            (binding-value binding) val frame))
          ((var? val)       
           (let ((binding (binding-in-frame val frame)))
             (if binding
                 (unify-match
                  var (binding-value binding) frame)
                 (extend var val frame))))
          ((depends-on? val var frame) 
           'failed)
          (else (extend var val frame)))))

(define (depends-on? exp var frame)
  (define (tree-walk e)
    (cond ((var? e)
           (if (equal? var e)
               true
               (let ((b (binding-in-frame e frame)))
                 (if b
                     (tree-walk (binding-value b))
                     false))))
          ((pair? e)
           (or (tree-walk (car e))
               (tree-walk (cdr e))))
          (else false)))
  (tree-walk exp))

; Maintaining the Data Base

(define THE-ASSERTIONS the-empty-stream)

(define (fetch-assertions pattern frame)
  (if (use-index? pattern)
      (get-indexed-assertions pattern)
      (get-all-assertions)))

(define (get-all-assertions) THE-ASSERTIONS)

(define (get-indexed-assertions pattern)
  (get-stream (index-key-of pattern)
              'assertion-stream))

(define (get-stream key1 key2)
  (let ((s (get key1 key2)))
    (if s s the-empty-stream)))

(define THE-RULES the-empty-stream)

(define (fetch-rules pattern frame)
  (if (use-index? pattern)
      (get-indexed-rules pattern)
      (get-all-rules)))

(define (get-all-rules) THE-RULES)

(define (get-indexed-rules pattern)
  (stream-append
   (get-stream (index-key-of pattern) 'rule-stream)
   (get-stream '? 'rule-stream)))

(define (add-rule-or-assertion! assertion)
  (if (rule? assertion)
      (add-rule! assertion)
      (add-assertion! assertion)))

(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (let ((old-assertions THE-ASSERTIONS))
    (set! THE-ASSERTIONS
          (cons-stream assertion old-assertions))
    'ok))

(define (add-rule! rule)
  (store-rule-in-index rule)
  (let ((old-rules THE-RULES))
    (set! THE-RULES (cons-stream rule old-rules))
    'ok))

(define (store-assertion-in-index assertion)
  (if (indexable? assertion)
      (let ((key (index-key-of assertion)))
        (let ((current-assertion-stream
               (get-stream 
                key 'assertion-stream)))
          (put key
               'assertion-stream
               (cons-stream 
                assertion
                current-assertion-stream))))))

(define (store-rule-in-index rule)
  (let ((pattern (conclusion rule)))
    (if (indexable? pattern)
        (let ((key (index-key-of pattern)))
          (let ((current-rule-stream
                 (get-stream 
                  key 'rule-stream)))
            (put key
                 'rule-stream
                 (cons-stream 
                  rule
                  current-rule-stream)))))))

(define (indexable? pat)
  (or (constant-symbol? (car pat))
      (var? (car pat))))

(define (index-key-of pat)
  (let ((key (car pat)))
    (if (var? key) '? key)))

(define (use-index? pat)
  (constant-symbol? (car pat)))

; Stream Operations

(define (stream-append-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream
       (stream-car s1)
       (stream-append-delayed (stream-cdr s1) delayed-s2))))

(define (interleave-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream
       (stream-car s1)
       (interleave-delayed (force delayed-s2)
                           (delay (stream-cdr s1))))))

(define (stream-flatmap proc s)
  (flatten-stream (stream-map proc s)))

(define (flatten-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave-delayed
       (stream-car stream)
       (delay (flatten-stream (stream-cdr stream))))))

(define (singleton-stream x)
  (cons-stream x the-empty-stream))

; Query syntax procedures

(define (type exp)
  (if (pair? exp)
      (car exp)
      (error "Unknown expression TYPE" exp)))

(define (contents exp)
  (if (pair? exp)
      (cdr exp)
      (error "Unknown expression CONTENTS" exp)))

(define (assertion-to-be-added? exp)
  (eq? (type exp) 'assert!))

(define (add-assertion-body exp)
  (car (contents exp)))

(define (empty-conjunction? exps) (null? exps))
(define (first-conjunct exps) (car exps))
(define (rest-conjuncts exps) (cdr exps))

(define (empty-disjunction? exps) (null? exps))
(define (first-disjunct exps) (car exps))
(define (rest-disjuncts exps) (cdr exps))

(define (negated-query exps) (car exps))

(define (predicate exps) (car exps))
(define (args exps) (cdr exps))

(define (rule? statement)
  (tagged-list? statement 'rule))

(define (conclusion rule) (cadr rule))

(define (rule-body rule)
  (if (null? (cddr rule))
      '(always-true)
      (caddr rule)))

(define (query-syntax-process exp)
  (map-over-symbols expand-question-mark exp))

(define (map-over-symbols proc exp)
  (cond ((pair? exp)
         (cons (map-over-symbols proc (car exp))
               (map-over-symbols proc (cdr exp))))
        ((symbol? exp) (proc exp))
        (else exp)))

(define (expand-question-mark symbol)
  (let ((chars (symbol->string symbol)))
    (if (string=? (substring chars 0 1) "?")
        (list '?
              (string->symbol
               (substring chars 1 (string-length chars))))
        symbol)))

(define (var? exp)
  (tagged-list? exp '?))

(define (constant-symbol? exp) (symbol? exp))

(define rule-counter 0)

(define (new-rule-application-id)
  (set! rule-counter (+ 1 rule-counter))
  rule-counter)

(define (make-new-variable var rule-application-id)
  (cons '? (cons rule-application-id (cdr var))))

(define (contract-question-mark variable)
  (string->symbol
   (string-append "?"
                  (if (number? (cadr variable))
                      (string-append (symbol->string (caddr variable))
                                     "-"
                                     (number->string (cadr variable)))
                      (symbol->string (cadr variable))))))

; Frames and Bindings

(define (make-binding variable value)
  (cons variable value))

(define (binding-variable binding)
  (car binding))

(define (binding-value binding)
  (cdr binding))

(define (binding-in-frame variable frame)
  (assoc variable frame))

(define (extend variable value frame)
  (cons (make-binding variable value) frame))

; Final procedure to initialize the data-base with all the assertions and rules 

(define (init-data-base rules-and-assertions) ; This will be called using the name of our data-base (defined below)
  
  (define (deal-out r-and-a rules assertions) ; This procedure has three arguments. It will firstly be used only with the first one,
    ; as all the rules and assertions will be included there.
    
    (cond ((null? r-and-a) ; If the data-base has no rules or assertions...
           (set! THE-ASSERTIONS assertions)
           (set! THE-RULES rules)
           'done)
          
          (else ; This goes one by one
           (let ((s (query-syntax-process (car r-and-a))))
             ; query-sintax-process transform pattern variables in the expression to the internal format
             ; For example, a pattern such as (job ?x ?y) is actually represented internally by the system as (job (? x) (? y))
             
             (cond ((rule? s)
                    ; Once is done it, it breaks the process in two types, rules and assertions
                    (store-rule-in-index s)
                    ; It stores the rule 
                    (deal-out (cdr r-and-a) ; It continues with the other part
                              (cons s rules) ; It updates the rules
                              assertions)) ; It doesn't update assertions
                   
                   (else ; If it is not a rule, it is an assertion
                    (store-assertion-in-index s)
                    ; It stores the assertion
                    (deal-out (cdr r-and-a) ; It continues with the other part
                              rules ; It doesn't update rules
                              (cons s assertions)))))))) ; It updates assertions

  ; Here are added the different options for the qeval procedure
  ; qeval procedure identifies special forms by a data-directed dispatch using get and put
  ; any query that is not identified as a special form is assumed to be a simple query and it is processed by simple-query
  (let ((operation-table (make-table)))
    (set! get (operation-table 'lookup-proc))
    (set! put (operation-table 'insert-proc!)))
  (put 'and 'qeval conjoin)
  (put 'or 'qeval disjoin)
  (put 'not 'qeval negate)
  (put 'lisp-value 'qeval lisp-value)
  (put 'always-true 'qeval always-true)
  ; Final call to finish the initialization
  (deal-out rules-and-assertions '() '()))

; Buulding the data-base

(define logic-programming-data-base
  
  '(

    ; General data from the workers of a company, including address, position, salary and supervirsor
    
    (address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))
    (job (Bitdiddle Ben) (computer wizard))
    (salary (Bitdiddle Ben) 60000)
    (supervisor (Bitdiddle Ben) (Warbucks Oliver))

    (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78))
    (job (Hacker Alyssa P) (computer programmer))
    (salary (Hacker Alyssa P) 40000)
    (supervisor (Hacker Alyssa P) (Bitdiddle Ben))

    (address (Fect Cy D) (Cambridge (Ames Street) 3))
    (job (Fect Cy D) (computer programmer))
    (salary (Fect Cy D) 35000)
    (supervisor (Fect Cy D) (Bitdiddle Ben))

    (address (Tweakit Lem E) (Boston (Bay State Road) 22))
    (job (Tweakit Lem E) (computer technician))
    (salary (Tweakit Lem E) 25000)
    (supervisor (Tweakit Lem E) (Bitdiddle Ben))

    (address (Reasoner Louis) (Slumerville (Pine Tree Road) 80))
    (job (Reasoner Louis) (computer programmer trainee))
    (salary (Reasoner Louis) 30000)
    (supervisor (Reasoner Louis) (Hacker Alyssa P)) 

    (address (Warbucks Oliver) (Swellesley (Top Heap Road)))
    (job (Warbucks Oliver) (administration big wheel))
    (salary (Warbucks Oliver) 150000)

    (address (Scrooge Eben) (Weston (Shady Lane) 10))
    (job (Scrooge Eben) (accounting chief accountant))
    (salary (Scrooge Eben) 75000)
    (supervisor (Scrooge Eben) (Warbucks Oliver))

    (address (Cratchet Robert) (Allston (N Harvard Street) 16))
    (job (Cratchet Robert) (accounting scrivener))
    (salary (Cratchet Robert) 18000)
    (supervisor (Cratchet Robert) (Scrooge Eben))

    (address (Aull DeWitt) (Slumerville (Onion Square) 5))
    (job (Aull DeWitt) (administration secretary))
    (salary (Aull DeWitt) 25000)
    (supervisor (Aull DeWitt) (Warbucks Oliver))

    ; The data base also contains assertions about which kind of jobs can be done by people holding other kinds of jobs

    (can-do-job (computer wizard) (computer programmer))
    (can-do-job (computer wizard) (computer technician))
    (can-do-job (computer programmer) (computer programmer trainee))
    (can-do-job (administration secretary) (administration big wheel))

    ; Rules, i.e., abstract queries

    (rule (lives-near ?person-1 ?person-2) ; two people live near if they live in the same town
          (and (address ?person-1 (?town . ?rest-1))
               (address ?person-2 (?town . ?rest-2))
               (not (same ?person-1 ?person-2))))

    (rule (same ?x ?x)) ; the same relation

    (rule (wheel ?person) ; a person is a 'wheel' in an organization if he supervises someone who is in turn a supervisor
          (and (supervisor ?middle-manager ?person)
               (supervisor ?x ?middle-manager)))
   
    ; Recursion in a rule
    ; A staff person is outranked by a boss in the organization
    (rule (outranked-by ?staff-person ?boss)
          (or (supervisor ?staff-person ?boss) ;  if the boss is the person's supervisor 
              (and (supervisor ?staff-person ?middle-manager) ; or if the person's supervisor is outranked by the boss
                   (outranked-by ?middle-manager ?boss))))

    ; Rule from Exercise 4.57
    ; A rule that says if one person inside the organization can do other person's job
    (rule (person-replacement ?person1 ?person2)
          (and (not (same ?person1 ?person2)) ; person1 and person2 are not the same person
               (job ?person1 ?job1) ; To generate job1
               (job ?person2 ?job2) ; To generate job2
               (or (same ?job1 ?job2) ; Either person 1 does the same job as person 2
                   (can-do-job ?job1 ?job2)))) ; or someone who does person 1's job can also do person 2's job

    ; Rule from Exercise 4.58
    ; A person is a 'big-shot' in a division if the person works in the division but does not have a supervisor who works in the division
    (rule (big-shot ?person ?division)
          (and (job ?person (?division . ?position))
               (or (not (supervisor ?person ?boss))
                   (not (and (supervisor ?person ?boss)
                             (job ?boss (?division . ?position2)))))))

    ; Exercise 4.59. Meetings
    (meeting accounting (Monday 9am))
    (meeting administration (Monday 10am))
    (meeting computer (Wednesday 3pm))
    (meeting administration (Friday 1pm))
    (meeting whole-company (Wednesday 4pm))
    ; A rule that says a person's meetings
    (rule (meeting-time ?person ?day-and-time)
          (or (meeting whole-company ?day-and-time)
              (and (job ?person (?division . ?rest))
                   (meeting ?division ?day-and-time))))

    ; Append rule as a logic program
    ; There are two rules that can characterize append
    ; 1. For any list y, the empty list and y append to form 1
    (rule (append-to-form () ?y ?y)) ; No body, so it holds for any value of ?y
    ; 2. For any u,v,y and z, (cons u v) and y append to form (cons u z) if v and y append to form z
    (rule (append-to-form (?u . ?v) ?y (?u . ?z))
          (append-to-form ?v ?y ?z))

    ; Exercise 4.61. Rule next-to
    ; 1. For any list with at least two elements, being them x and y, x is next to y
    (rule (?x next-to ?y in (?x ?y . ?u)))
    ; 2. For a list consisting in the element v and some other elements (dot notation) . z, x is next to y if it is in the second part
    (rule (?x next-to ?y in (?v .?z))
          (?x next-to ?y in ?z))

    ; Exercise 4.62. Rule last-pair
    ; 1. If a list has only one element, the last pair is that element
    (rule (last-pair (?x) (?x)))
    ; 2. For any list, (?x) is the last pair if it is the last element. 
    (rule (last-pair (?u . ?v) (?x))
          (last-pair ?v (?x)))
    ; The use of the parenthesis here is related to how Scheme treats car and cdr.
    ; This rule is based on the Exercise 2.17, and as it was discussed there, there is a difference between returning the last value and the last pair
    ; After checking with the following queries
    ; (last-pair (3) ?x) should return (last-pair (3) (3))
    ; (last-pair (1 2 3) ?x) should return (last-pair (1 2 3) (3))
    ; (last-pair (2 ?x) (3)) should return (last-pair (2 3) (3))
    ; This was decided to be the solution

    ; Exercise 4.63. Genesis 4 data-base
    (son Adam Cain)
    (son Cain Enoch)
    (son Enoch Irad)
    (son Irad Mehujael)
    (son Mehujael Methushael)
    (son Methushael Lamech)
    (wife Lamech Ada)
    (son Ada Jabal)
    (son Ada Jubal)

    ; Rules
    ; "If S is the son of F, and F is the son of G, then S is the grandson of G"
    ; "If W is the wife of M, and S is the son of W, then S is the son of M"
    (rule (father ?S ?F)
          (or (son ?F ?S)
              (and (wife ?F ?W)
                   (son ?W ?S))))

    (rule (grandson ?G ?S)
          (and (father ?S ?F)
               (father ?F ?G)))

    ; New examples
    ; Example 1
    (man Socrates) ; Assertion
    (rule (mortal ?person) ; Rule, for all objects, if it is a man, it is mortal
          (man ?person))
    ; Let's see how (mortal Socrates) is derived after the assertiond and rule from above
    ; Let's assume (not (mortal Socrates))
    ; From the rule, as being a man implies being mortal, and using the equivalence A implies B is equivalent to not A or B, we have the three following statements
    ; (not (mortal Socrates))
    ; (man Socrates) (it is an Assertion)
    ; (or (not (man ?person)) (mortal ?person)), where ?person can be substituted by Socrates, then
    ;
    ; (man Socrates)
    ; (not (mortal Socrates))   
    ; (or (not (man Socrates)) (mortal Socrates))
    ;
    ; Looking the two last clauses, as we are approching the problem from (not (mortal Socrates)), the consequence is that (not (man Socrates)), which is wrong, since
    ; (man Socrates) is an assertion of the database.

    ; Example 2
    (rule (lastname ?person ?name)
          (lastname (father ?person ?father) ?name))
    
    ; Example 3
    ;   (ingredient tallow soap)
    ;   (ingredient water potash)
    ;   (ingredient ashes potash)
    ;   
    ;   (rule (supply ?potash)
    ;         (and (ingredient water ?potash)
    ;              (ingredient ashes ?potash)))
    ;   
    ;   (rule (production ?soap)
    ;         (and (ingredient tallow ?soap)
    ;              (supply potash)))
    ; 1. Facts
    (ingredient tallow)
    (ingredient water)
    (ingredient ashes)
    ; 2. Rules
    (rule (supply ?potash)
          (and (ingredient water)
               (ingredient ashes)))
   
    (rule (production ?soap)
          (and (ingredient tallow)
               (supply potash)))

    ; Example 4
    (transport bus home)
    ;(transport taxi home)
    ;(transport walk home)
    ;(transport bike home)
    (rule (arrive ?home)
          (or (transport bus ?home)
              (transport taxi ?home)
              (transport bike ?home)
              (transport walk ?home)))
   
    ))

; (init-data-base logic-programming-data-base)
; (query-driver-loop)