#lang racket

; Generic operations

; Adding new types: create all the constructors, selectors and update the procedure with a new block with predicate for this new type.
; Adding new operations: create the new operation, very straightforward.

; Data-directed style

; Adding new types: as in Page 182 with the rectangular package, implement constructor, selector and operations and use the get procedure to update the table.
; Adding new operations: update every package and create new entries in the table.

; Message-passing style

; Adding new types: creation of one constructor (as the previous exercise).
; Adding new operations: editing each constructor.

; Most appropiate for a system in which nee types must often be added: Message-passing or data-directed
; Most appropiate for a system in which nee types must often be added: Generic operations