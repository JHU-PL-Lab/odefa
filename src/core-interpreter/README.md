# Todo
1. Add interpreter support for extended grammar
  * Input
  * Formulae/predicates
  * Iota dictionary - maps program points (identifiers to values (start with ints))
  * The iota dictionary will be blank at first. First implement formula tracking
2. Extend interpreter to keep track of formulae
3. Make bare-bones, really simple examples
4. Implement support for simplest rules and work up to the more subtle, complex rules.

wrote substitution methods - one for vars and another for values
wrote check_formula. have not thoroughly tested any methods above
took a first stab at the input case in lookup. right now always returns true. 
