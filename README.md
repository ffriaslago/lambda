README.md

* Repository on structured programming. It contains a set of exercises done through the work of a bachelor thesis to obtain a Mathematics degree in Universidad de Cantabria.

* The difference between the extension .rkt and .bak files is that the former ones contain the proper exercise while the latter contain a recovery version of it, usually not a full one, so there is no point of paying attention to them, as they normally contain an incomplete or more simple version of the same exercises. It can be noticed too that, although it was tried to do most of them with the DrRacket editor, some of the exercises are in a PDF format for a better visualisation.

* Interpreter: it is composed by the two main files of the thesis.
  * The one named ExercisesLogicProgramming.rkt contains a set of problems of the section 4.4 of \textit{SICP}. It serves as a preparation for composing a generous database to test the interpreter and to learn how to implement the Horn Clauses in Scheme.
  * The one named LogicProgramming$\_$Interpreter.rkt contains the main goal of the thesis, it can be separated into three parts. The first one is all the definitions of the procedures necessary for the interpreter. The second one is the proper interpreter and the third one is a big database containing all the 
    facts and rules derived from the exercises and the examples showed in the introduction.
* Bratko. Chapter 1: All the exercises from the first chapter of Bratko, separated by sections. The format is .txt, although they were made using the platform https://swish.swi-prolog.org/ that creates files with extension .pl and can be used to run the code given.
* Bratko. Chapter 2: Same as previous folder, but for the second chapter of the book.
* SICP. Chapter 1: It consists of all the exercises from the first chapter of SICP. As it is an introductory one thought to learn the basis of Lisp, it was done completely.
* SICP. Chapter 2: It does not include all the exercises from chapter 2 of SICP, albeit the majority of them. The ones from the last section of the chapter were discarded because it needed implementations explained in chapter 3 and it seemed ineffective to solve exercises that could not be run.
* SICP. Chapter 3: roughly 80 % of the exercises were made. Section 3.4 was skipped, as concurrency was not a relevant topic for the interpreter. Finally, the last two subsections of Streams were omitted too.
* SICP. Chapter 4: It encloses most of the exercises from Section 4.1, where the book shows how to build the evaluator that the user had been using until there. It serves as a good exercise of decomposition and abstraction.
