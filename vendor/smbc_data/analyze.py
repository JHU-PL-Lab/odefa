#!/usr/bin/env python3

import re, sys

if len(sys.argv) == 1:
    results = 'results.txt'
else:
    results = sys.argv[1]

data = open(results, 'r').read().split("=======================")[1:]
print("parsed `%s` (%d entries)" % (results, len(data)))

def res_of_solver(solver, chunk):
    if solver == 'smbc':
        if 'UNSAT' in chunk: return 'unsat'
        if 'SAT' in chunk: return 'sat'
    elif solver == 'lazy smallcheck':
        if 'Counter example found' in chunk: return 'sat'
    elif solver == 'hbmc':
        if 'Counterexample!' in chunk: return 'sat'
        if 'Contradiction!' in chunk: return 'unsat'
    elif solver == 'cvc4':
        if 'unsat' in chunk: return 'unsat'
        if 'sat' in chunk: return 'sat'
    elif 'inox' in solver:
        if '=> UNSAT' in chunk: return 'unsat'
        if '=> SAT' in chunk: return 'sat'
    return 'unknown'

categories = {} # by category of problem, then file, then solver
global_res = {} # by category of problem, then solver

for by_problem in data:
    file_ = list(re.finditer(r'run on (.*)', by_problem))[0].group(1)
    cat = file_.split('/')[0]

    cat_results = categories.setdefault(cat, dict()) # results for this category
    file_results = {} # results for this file
    cat_results[file_] = file_results

    print("file %s (category `%s`)" % (file_, cat))

    # find each solver
    for by_solver in by_problem.split("-----------------------")[1:]:
        solver = list(re.finditer(r'- (.*)', by_solver))[0].group(1)

        res = res_of_solver(solver, by_solver)
        time_ = re.search('\n([0-9.:]+)\n', by_solver)
        if time_:
            time_ = time_.group(1)
        else:
            time_ = "<unknown>"

        print("  solver %s: %s in %s" % (solver,res,time_))

        file_results[solver] = res

        cat_to_s = global_res.setdefault(cat, dict())
        s_to_res = cat_to_s.setdefault(solver, dict())
        n = s_to_res.setdefault(res, 0)
        s_to_res[res] = n+1

print()
for (cat,by_s) in global_res.items():
    print("category %s (%d problem(s)):" % (cat, len(categories[cat])))
    for (solver, by_res) in by_s.items():
        l = sorted(by_res.items())
        print("  %-20s %s" % (solver+':', " ".join("%8s:%3d" % tup for tup in l)))
