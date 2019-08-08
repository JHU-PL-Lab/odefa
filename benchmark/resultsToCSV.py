#!/usr/bin/env python3

from resultsLib import *

import argparse
import csv
import os
import re

RESULT_TIME_REGEXES = [
        # P4F
        re.compile("Analysis run for: ([0-9]+) milliseconds"),
        # Odefa and Boomerang original
        re.compile("Analysis took ([0-9]+) ms"),
        # Boomerang SPDS
        re.compile("Total analysis time: ([0-9]+) ms"),
    ]

def parse_args():
    parser = argparse.ArgumentParser(description="Extract information from benchmark output into CSVs.")
    parser.add_argument("--dir", metavar="DIR", type=str, default="results",
                        help="The directory containing the benchmark output.")
    return parser.parse_args()

def read_files(results_directory):
    """
    Reads all files in the results/ directory, taking them to be a set of
    results from the benchmarking script.  The returned value is a dictionary
    mapping result names to results.  A result name is a frozen set of key-value
    pairs which describe the test case (e.g. monovariant DDPA on test case tak).
    A result is a dictionary containing facts about the test (e.g. a "time"
    field describing execution time in milliseconds).
    """
    data = {}
    # For each result file...
    for result_filename in os.listdir(results_directory):
        if not result_filename.endswith(".txt"): continue
        # Establish a local name for the experiment as a dictionary mapping each
        # property to its value.
        result_name = result_filename[:result_filename.index(".")]
        result_name_parts = result_filename.split("--")
        result_name = []
        for part in result_name_parts:
            if "=" in part:
                k,v = part.split("=",1)
                result_name.append((k,v))
        result_name.sort()
        result_name = frozenset(result_name)
        # Now extract the results
        result_pathname = results_directory + os.sep + result_filename
        with open(result_pathname) as f:
            content = f.read()
            result = {}
            time_in_ms = None
            for regex in RESULT_TIME_REGEXES:
                m = regex.search(content)
                if m:
                    time_in_ms = int(m.group(1))
                    break
            result["time"] = time_in_ms
            if result_name not in data:
                data[result_name] = []
            data[result_name].append(result)
    return data

def aggregate_data(data):
    """
    Aggregates data of the form produced by read_files.  The result produced by
    this routine is a series of nested dictionaries with the following structure
    starting at top level:
        * The key "monovariant" maps to a dictionary containing monovariant
          tests:
            * The key "cases" maps to a list of test case names.
            * The key "analyses" maps to a list of program analysis names.
            * The key "results" maps to a dictionary containing test results:
                * Each key is a test case name.  Each value is a dictionary:
                  * Each key is an analysis name.  Each value is a dictionary:
                    * The key "time" refers to execution time of the analysis in
                      milliseconds.  This time is a linear average of all
                      iterations of this test case.
        * The key "polyvariant" maps to a similar dictionary.
    """
    experiments = {}
    for name in ["monovariant", "polyvariant"]:
        results = {}
        cases = set()
        analyses = set()
        for (result_name, runs) in data.items():
            result_name = dict(result_name)
            if result_name["experiment"] != name: continue
            # Record case and analysis
            case = result_name["case"]
            analysis = result_name["analysis"]
            cases.add(case)
            analyses.add(analysis)
            # Aggregate result
            # Arbitrary policy choice: if any of the results time out, report
            # that instead.
            times = list(map(lambda r: r["time"], runs))
            if None in times:
                time_in_ms = None
            else:
                time_in_ms = int(sum(times)/len(runs))
            result = { "time" : time_in_ms }
            # Construct result dictionary entry
            if case not in results:
                results[case] = {}
            if analysis in results[case]:
                raise ValueError("Duplicate results for {} in {}".format(
                    case, analysis))
            results[case][analysis] = result
        experiments[name] = \
            {"cases": sorted(list(cases)),
             "analyses": sorted(list(analyses)),
             "results": results,
            }
    return experiments

def produce_csv_from(results_directory, name, group_data):
    """
    Produces a CSV file for the provided experiment data.
    """
    with open(results_directory + os.sep + name + "-table.csv", 'w') as csvfile:
        writer = csv.writer(csvfile, quoting=csv.QUOTE_MINIMAL)
        # Extract the data for this case.
        analyses = analyses_in_preferred_order(group_data["analyses"])
        cases = group_data["cases"]
        # Write header
        writer.writerow([""]+analyses)
        # Write results
        for case in cases:
            row = [case]
            for analysis in analyses:
                time = group_data["results"][case][analysis]["time"]
                row.append("timeout" if time is None else time)
            writer.writerow(row)

def main():
    args = parse_args()
    results_directory = args.dir
    data = read_files(results_directory)
    data = aggregate_data(data)
    for group in data:
        if data[group]["cases"] and data[group]["analyses"]:
            produce_csv_from(results_directory, group, data[group])

main()
