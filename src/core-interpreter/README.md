### TODO list
1. Implement branching decision at call sites.
 - branching_call_sites_simple.code is the test. Currently commented out.
 - need to make method that find the point of non-determinism. When you start
 inside a conditional and realize there are multiple valid choices, it needs to fire.
 - Go through all call sites first and then increment successor function

2. Other examples.
  - Start in the middle of recursion function - see input has to be non zero
  - `x = input; y = x + x `. Need to remember the current binding of x.

3. New version of the test generation implementation.
  - More algebraic - construct formula and use sat solver at the end

4. SAT solver.
  - To utilize for the new version of the implementation.

5. Complete CFG.
  - Use ddpa to generate the CFG, then transform that so it can be used as an input for implementation.
  - Need encoding for input, not sure if anything is needed.

6. Tests for benchmarks.

#### On return
 - tackle #1
 - try making CFG and finding point of multiple call sites
 - think can count number of valid predecessor nodes 
