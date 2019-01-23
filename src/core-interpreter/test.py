#!/usr/bin/python

def convert( formula, dictionary ):
    output = Int(0)
    for c in formula:
        if c.isalpha():
            output = output + dictionary[c]
        else:
            output = output + c
    return output

from z3 import *
import sys

formula = sys.argv[1]
print("formula")
print(formula)

# make list for formula variables and maps them to solver variables
counter = 0
vars = {}
for c in formula:
    if (c.isalpha()) and (c not in vars):
        vars[c] = Real(c)
print("vars")
print(vars)

# make solver and solve
s = Solver()
s.add(vars['x'] + vars['y'] > 5, vars['x'] > 1, vars['y'] > 1)
print("type")
print(type(vars['x'] > 1))

# print("convert")
# print(type(convert(formula, vars)))
# print(convert(formula, vars))

# print out output
print(s.check())
print(s.model())
