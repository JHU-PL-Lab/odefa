#!/usr/bin/env python3

import csv
import matplotlib.pyplot as mplot
import os
import re

P4F_RESULT_TIME = re.compile("Analysis run for: ([0-9]+) milliseconds")
ODEFA_RESULT_TIME = re.compile("Analysis took ([0-9]+) ms")

def read_files():
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
    for result_filename in os.listdir("results"):
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
        result_pathname = "results" + os.sep + result_filename
        with open(result_pathname) as f:
            content = f.read()
            result = {}
            m = P4F_RESULT_TIME.search(content)
            if m:
                result["time"] = int(m.group(1))
            else:
                m = ODEFA_RESULT_TIME.search(content)
                if m:
                    result["time"] = int(m.group(1))
                else:
                    result["time"] = None
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

def analyses_in_preferred_order(input_analyses):
    preferred_analysis_order = ["p4f", "ddpa", "kplume", "splume"]
    analyses = []
    for analysis in preferred_analysis_order:
        if analysis in input_analyses:
            analyses.append(analysis)
    for analysis in input_analyses:
        if analysis not in preferred_analysis_order:
            analyses.append(analysis)
    return analyses

def produce_csv_from(name, group_data):
    """
    Produces a CSV file for the provided experiment data.
    """
    with open("results" + os.sep + name + "-table.csv", 'w') as csvfile:
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

def produce_bar_chart_from(name, group_data):
    """
    Produces a bar chart from the provided experiment data.
    """
    cases = list(reversed(group_data["cases"]))
    analyses = group_data["analyses"]
    analyses = analyses_in_preferred_order(analyses)
    analyses.reverse()
    figure = mplot.figure(figsize=(10,12))
    ax = figure.add_axes([0.2,0.2,0.7,0.7])
    MIN_TIME = 1
    MAX_TIME = 1000000
    ax.set_xlim(MIN_TIME, MAX_TIME)
    bar_group_area = 2
    bar_group_height = 0.7*bar_group_area
    analysis_colors = {"ddpa": "DarkMagenta",
                       "p4f": "DarkGoldenRod",
                       "kplume": "LightSeaGreen",
                       "splume": "ForestGreen"
                      }
    for case_idx, case in enumerate(cases):
        base_position = case_idx * bar_group_area - 0.5*bar_group_height
        for analysis_idx, analysis in enumerate(analyses):
            bar_height = bar_group_height/len(analyses)
            bar_position = base_position + bar_height * (analysis_idx+0.5)
            time = group_data["results"][case][analysis]["time"]
            if time is None:
                bar = ax.barh([bar_position],
                              [MAX_TIME],
                              bar_height,
                              edgecolor='black',
                              label=analysis)[0]
                bar.set_color(analysis_colors.get(analysis) or "Black")
                bar.set_edgecolor("Black")
                bar.set_hatch("//")
            elif time < MIN_TIME:
                print("Time <{}ms reported for {} on {}".format(
                        MIN_TIME, analysis, case))
            else:
                bar = ax.barh([bar_position],
                              [time],
                              bar_height,
                              label=analysis)[0]
                bar.set_color(analysis_colors.get(analysis) or "Black")
                ax.annotate("{:d}".format(int(time)),
                            xy=(bar.get_width(),
                                bar.get_y() + bar.get_height() / 2),
                            xytext=(3,0),
                            textcoords="offset points",
                            ha="left",
                            va="center",
                            fontsize=6)
        ax.set_xlabel("Time (ms)")
        ax.set_xscale("log")
        ax.set_yticks(list(map(lambda n: n * bar_group_area, range(len(cases)))))
        ax.set_yticklabels(cases)
        for tick in ax.yaxis.get_major_ticks()[0::2]:
            tick.set_pad(12)
    ax.legend([
        mplot.Rectangle((0,0),1,1,fc=analysis_colors[analysis])
        for analysis in reversed(analyses)
        ],
        list(reversed(analyses))
    )
    figure.savefig("results" + os.sep + name + "-bars.svg")

def produce_output(data):
    for group in ["monovariant", "polyvariant"]:
        produce_csv_from(group, data[group])
        produce_bar_chart_from(group, data[group])

def main():
    data = read_files()
    aggregated = aggregate_data(data)
    produce_output(aggregated)

main()
