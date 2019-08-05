#!/usr/bin/env python3

from resultsLib import *

import csv
import matplotlib.pyplot as mplot
import os

def read_csvs():
    """
    Reads the result CSVs from the results directory.  Produces a nested
    dictionary data structure of the following form:
        * For each CSV file "foo-table.csv", a key "foo" exists mapping to a
          dictionary of tests for that group:
            * The key "cases" maps to a list of test case names.
            * The key "analyses" maps to a list of program analysis names.
            * The key "results" maps to a dictionary containing test results:
                * Each key is a test case name.  Each value is a dictionary:
                  * Each key is an analysis name.  Each value is a dictionary:
                    * The key "time" refers to execution time of the analysis in
                      milliseconds.  This time is a linear average of all
                      iterations of this test case.
    """
    ret = {}
    for result_csv_filename in os.listdir("results"):
        if not result_csv_filename.endswith("-table.csv"): continue
        with open("results" + os.sep + result_csv_filename) as f:
            rows = list(csv.reader(f))
        group_name = result_csv_filename[:-10]
        analyses = rows[0][1:]
        rows = rows[1:]
        cases = []
        results = {}
        for row in rows:
            case = row[0]
            results[case] = {}
            cases.append(case)
            for i in range(1,len(row)):
                analysis = analyses[i-1]
                try:
                    value = int(row[i])
                except ValueError:
                    value = None if row[i] == "None" or row[i] == "timeout" else row[i]
                results[case][analysis] = { "time": value }
        ret[group_name] = {"cases": cases,
                           "analyses": analyses,
                           "results": results
                           }
    return ret

def produce_bar_chart_from(name, group_data):
    """
    Produces a bar chart from the provided experiment data.
    """
    cases = list(reversed(group_data["cases"]))
    analyses = group_data["analyses"]
    analyses = analyses_in_preferred_order(analyses)
    analyses.reverse()
    figure = mplot.figure(figsize=(10,0.75*len(cases)*len(analyses)))
    ax = figure.add_axes([0.2,0.2,0.7,0.7])
    MIN_TIME = 1
    MAX_TIME = 1000000
    ax.set_xlim(MIN_TIME, MAX_TIME)
    bar_group_area = 2
    bar_group_height = 0.7*bar_group_area
    analysis_colors = {"ddpa":               (0.00,0.62,0.45), # sea green
                       "p4f":                (0.80,0.47,0.65), # reddish purple
                       "kplume":             (0.34,0.71,0.91), # sky blue
                       "splume":             (0.00,0.45,0.70), # medium blue
                       "boomerangSPDS":      (0.84,0.37,0.00), # vermillion
                       "boomerangOriginal":  (0.90,0.62,0.00), # orange
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
        # for tick in ax.yaxis.get_major_ticks()[0::2]:
        #     tick.set_pad(12)
    ax.legend([
        mplot.Rectangle((0,0),1,1,fc=analysis_colors[analysis])
        for analysis in reversed(analyses)
        ],
        list(reversed(analyses))
    )
    figure.savefig("results" + os.sep + name + "-bars.svg")

def main():
    data = read_csvs()
    for group in data:
        if data[group]["cases"] and data[group]["analyses"]:
            produce_bar_chart_from(group, data[group])

main()
