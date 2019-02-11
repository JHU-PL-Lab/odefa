#!/usr/bin/python

# Right now the model will make extra variables because
# True and False are parsed by for loop

# usage: python test.py "x+y>5,x>1,y>1"

from z3 import *
import sys

# x = Bool('x')
# y
# solve(x == True,y == Not(x),)
# solve(Or(x <= 0, 0 < x))


formula = sys.argv[1]

print("formula")
print(formula)

# make list for formula variables to keep track of duplicates and make solver variables
# right now, there's a restriction on vars that are allowed to be booleans
vars = []
for c in formula:
    if (c.isalpha()) and (c not in vars):
        vars.append(c)
        if (c in ('a','b','c','d','e')):
            exec(c + "= Bool('" + c +  "')")
        else:
            exec(c + "= Real('" + c + "')")

print("vars")
print(vars)

# make solver and solve
# # s = Solver()
print("\nResults:")
# try:
exec("solve(" + formula + ")")
# except Z3Exception:
    # print("t    ")

# exec("s.add(" + formula + ")")

# print out output
# print(s.check())
# print(s.model())
