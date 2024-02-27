#lang sicp

; 4.4 Logic Programming. Initial exercises
;
; Simple queries
;
; Exercise 4.55
;
; a)

(supervisor ?x (Bitididdle Ben))

; b)

(job ?name (accounting . ?position))

; c)

(address ?name (Slumerville . ?place))

; Compound queries
;
; Exercise 4.56
;
; a)

(and (supervisor ?x (Bitdiddle Ben))
     (address ?x ?y))

; b)

(and (salary (Bitdiddle Ben) ?number) 
     (salary ?person ?amount) 
     (lisp-value < ?amount ?number))

(and (salary (Bitdiddle Ben) ?number) 
     (salary ?person ?amount) 
     (lisp-value < ?amount ?number)) 


; c)

(and (supervisor ?x ?y)
     (not (job ?y (computer . ?type)))
     (job ?y ?position))


; Rules
;
; Exercise 4.57

(rule (person-replacement ?person1 ?person2)
      (and (not (same ?person1 ?person2)) ; person1 and person2 are not the same person
           (job ?person1 ?job1) ; To generate job1
           (job ?person2 ?job2) ; To generate job2
           (or (same ?job1 ?job2) ; Either person 1 does the same job as person 2
               (can-do-job ?job1 ?job2)))) ; or someone who does person 1's job can also do person 2's job

; a)

(person-replacement ?person1 (Fect Cy D))

; b)

(and (person-replacement ?person1 ?person2) (and (salary ?person1 ?salary1) (salary ?person2 ?salary2) (lisp-value < ?salary1 ?salary2)))

; Exercise 4.58

(rule (big-shot ?person ?division)
      (and (job ?person (?division . ?position))
           (or (not (supervisor ?person ?boss))
               (not (and (supervisor ?person ?boss)
                         (job ?boss (?division . ?position2)))))))

; Exercise 4.59
;
; a)

(meeting ?group (Friday ?time))

; b)

(rule (meeting-time ?person ?day-and-time)
      (or (meeting whole-company ?day-and-time)
          (and (job ?person (?division . ?rest))
               (meeting ?division ?day-and-time))))

; c)

(meeting-time (Hacker Alyssa P) (Wednesday ?time))

; Exercise 4.60
;
; Because of the definition of the rule lives-near, both answers are different and both fulfill the conditions asked
; It could be added a rule for ordering the names of the people in a way that there is only one possibility for each answer

; Logic as programs
;
; Exercise 4.61
;
; (?x next-to ?y in (1 (2 3) 4))

((2 3) next-to 4 in (1 (2 3) 4))
(1 next-to (2 3) in (1 (2 3) 4))

; (?x next-to 1 in (2 1 3 1))

(2 next-to 1 in (2 1 3 1))
(3 next-to 1 in (2 1 3 1))

; Exercise 4.62

; (last-pair (3) ?x) should return (last-pair (3) (3))
; (last-pair (1 2 3) ?x) should return (last-pair (1 2 3) (3))
; (last-pair (2 ?x) (3)) should return (last-pair (2 3) (3))
; (last-pair ?x (3)) could return any list having as last element 3. Infinite solutions.

(rule (last-pair (?x) (?x)))

(rule (last-pair (?u . ?v) (?x))
      (last-pair ?v (?x)))
      

; Exercise 4.63

; son assertions follow (son ?father ?son)
; wife assertions follow (wife ?husband ?wife)

(rule (father ?sonn ?fatherr)
      (or (son ?fatherr ?sonn)
          (and (wife ?fatherr ?mom)
               (son ?mom ?sonn))))

(rule (grandson ?grandf ?grands)
      (and (father ?grands ?fatherr)
           (father ?fatherr ?grandf)))

; Exercise 4.64

(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          (and (outranked-by ?middle-manager ?boss)
               (supervisor ?staff-person ?middle-manager))))

(outranked-by (Bitdiddle Ben) ?who)

; or queries are unified in parallel
; query (outranked-by ?middle-manager ?who) -> Infinite loop, as ?middle-manager is unbound

