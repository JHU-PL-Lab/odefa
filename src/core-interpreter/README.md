# Todo
1. Add interpreter support for extended grammar
  * Input
  * Formulae/predicates
  * Iota dictionary - maps program points (identifiers to values (start with ints))
  * The iota dictionary will be blank at first. First implement formula tracking
2. Extend interpreter to keep track of formulae
3. Make bare-bones, really simple examples
4. Implement support for simplest rules and work up to the more subtle, complex rules.

currently trying to figure out how to implement the iota dictionary passing and Formulae
not sure but I think only context stack is passed and the lookup stack is kept
through the program stack.
currently reading through lookup match case Start_clause and encountered lookup_ctx.

not sure what call by need means - but its always been false so far.
