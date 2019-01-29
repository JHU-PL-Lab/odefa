#!/usr/bin/python

# Right now the model will make extra variables because
# True and False are parsed by for loop

# usage: python test.py "x+y>5,x>1,y>1"

from z3 import *
import sys

formula = sys.argv[1]

print("formula")
print(formula)

# make list for formula variables to keep track of duplicates and make solver variables
vars = []
for c in formula:
    if (c.isalpha()) and (c not in vars):
        vars.append(c)
        exec(c + "= Real(c)")

print("vars")
print(vars)

# make solver and solve
s = Solver()
exec("s.add(" + formula + ")")
print("type")

# print out output
print(s.check())
print(s.model())
