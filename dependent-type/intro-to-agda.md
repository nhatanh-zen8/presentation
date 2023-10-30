% Introduction to dependent type with Agda
% Octorber 27, 2023

# Dependent typing
- 4 relations between term (value) and type:
  - Value depends on value: function
  - Value depends on type: default value/method in interface
  - Type depends on type: generics
  - *Type depends on value*: dependent type

# What is dependent typing good for
- Curry-Howard correspondence/isomorphism: type system equals logic system, program equals proof, value of a type equal witness of a theorem's validity
  of a theorem
- 2 sides of the spectrum:
  - Highly expressive encoding static guarantees about computer programs: Idris
  - Proof assistance: Coq, Lean
  - Agda: something in the middle, lean more toward proof assistance
- Dependently typed languages can make use of tactics/heuristics to *search* for proofs/programs
- LLMs as proof searching tactics has been a very active area of research in recent years
=> Huge potential

# What is Agda?
- Dependently typed
- Purely functional
  - Applicative programming model based on a lambda calculus
  - Referential transparency

# Type-driven development
- Dependent type systems are highly expressive 
- Can model pretty much arbitrary domains and their constraints
- Programmers can focus on modelling the domain with types, and compilers can help guiding/refining from types to correct
  implementations
