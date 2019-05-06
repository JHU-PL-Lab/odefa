#!/usr/bin/python
# usage: python test.py "x+y>5,x>1,y>1"

from z3 import *
import sys

formula = sys.argv[1]

print("formula")
print(formula)

# make list for formula variables to keep track of duplicates and make solver variables
# right now, there's a restriction on vars that are allowed to be booleans - starts with bool
vars = []
varCandidates = formula.split()
for word in varCandidates:
    # print(word)
    var = (word.split("AAA"))[0]
    # print(var)
    if (word[0].isalpha()) and (word not in vars):
        vars.append(word)
        if (word[0:4] == "bool"):
            print("we got bool")
            exec(word + "= Bool('" + word + "')")
        elif (word in ["True","False","Not","And","Or"]):
            continue
        else:
            exec(word + "= Real('" + word + "')")
print("vars")
print(vars)

print("\n-------------------------------------------------- Results --------------------------------------------------")
exec("solve(" + formula + ")")
