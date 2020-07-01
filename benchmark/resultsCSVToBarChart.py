#!/usr/bin/env python3

from resultsLib import *

import argparse
import csv
import matplotlib.pyplot as mplot
import os

def default_csv_files():
    files = []
    for csv_basename in os.listdir("results"):
        if not csv_basename.endswith("-table.csv"): continue
        files.append("results" + os.sep + csv_basename)
    return files

def parse_args():
    parser = argparse.ArgumentParser(description="Convert a benchmark CSV into a vector bar chart.")
    parser.add_argument("--csv-files", metavar="CSV_FILE", type=str,
                        nargs='+', default=default_csv_files(),
                        help="The CSV files to convert.")
    parser.add_argument("--output-dir", metavar="OUTPUT_DIR", type=str,
                        default="results",
                        help="The directory in which to write the charts.")
    parser.add_argument("--horizontal", action="store_true", default=False,
                        help="Produce a horizontally-oriented bar graph.")
    return parser.parse_args()

def read_csvs(csv_files):
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
    for result_csv_filename in csv_files:
        with open(result_csv_filename) as f:
            rows = list(csv.reader(f))
        group_name = os.path.basename(result_csv_filename)
        group_name = os.path.splitext(group_name)[0]
        if '-' in group_name:
            group_name = group_name[:group_name.find('-')]
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

def produce_bar_chart_from(output_dir, name, group_data, is_horizontal):
    """
    Produces a bar chart from the provided experiment data.
    """
    cases = list(group_data["cases"])
    if not is_horizontal:
        cases.reverse()
    analyses = group_data["analyses"]
    analyses = analyses_in_preferred_order(analyses)
    analyses.reverse()
    bar_size = 6
    area_size = 0.75*len(cases)*len(analyses)
    size = (bar_size,area_size) if not is_horizontal else (area_size,bar_size)
    figure = mplot.figure(figsize=size)
    ax = figure.add_axes([0.2,0.2,0.7,0.7])
    MIN_TIME = 1
    MAX_TIME = 1000 * 60 * 30 # 30 minutes
    if is_horizontal:
        ax.set_ylim(MIN_TIME, MAX_TIME)
    else:
        ax.set_xlim(MIN_TIME, MAX_TIME)
    bar_group_area = 2
    bar_group_size = 0.7*bar_group_area
    analysis_colors = {"ddpa":               (0.00,0.62,0.45), # sea green
                       "p4f":                (0.80,0.47,0.65), # reddish purple
                       "kplume":             (0.34,0.71,0.91), # sky blue
                       "splume":             (0.00,0.45,0.70), # medium blue
                       "boomerangSPDS":      (0.84,0.37,0.00), # vermillion
                       "boomerangOriginal":  (0.90,0.62,0.00), # orange
                      }
    for case_idx, case in enumerate(cases):
        base_position = case_idx * bar_group_area - 0.5*bar_group_size
        for analysis_idx, analysis in enumerate(analyses):
            bar_size = bar_group_size/len(analyses)
            bar_position = base_position + bar_size * (analysis_idx+0.5)
            time = group_data["results"][case][analysis]["time"]
            if time is None:
                if is_horizontal:
                    bar = ax.bar([bar_position],
                                 [MAX_TIME],
                                 bar_size,
                                 edgecolor='black',
                                 label=analysis)[0]
                else:
                    bar = ax.barh([bar_position],
                                  [MAX_TIME],
                                  bar_size,
                                  edgecolor='black',
                                  label=analysis)[0]
                bar.set_color(analysis_colors.get(analysis) or "Black")
                bar.set_edgecolor("Black")
                bar.set_hatch("//")
            elif time < MIN_TIME:
                print("Time <{}ms reported for {} on {}".format(
                        MIN_TIME, analysis, case))
            else:
                if is_horizontal:
                    bar = ax.bar([bar_position],
                                 [time],
                                 bar_size,
                                 label=analysis)[0]
                else:
                    bar = ax.barh([bar_position],
                                  [time],
                                  bar_size,
                                  label=analysis)[0]
                bar.set_color(analysis_colors.get(analysis) or "Black")
                if is_horizontal:
                    xy=(bar.get_x() + bar.get_width() / 2, bar.get_height())
                else:
                    xy=(bar.get_width(), bar.get_y() + bar.get_height() / 2)
                ax.annotate("{:d}".format(int(time)),
                            xy=xy,
                            xytext=(0,3) if is_horizontal else (3,0),
                            textcoords="offset points",
                            ha="center" if is_horizontal else "left",
                            va="bottom" if is_horizontal else "center",
                            fontsize=8)
        if is_horizontal:
            ax.set_ylabel("Time (ms)")
            ax.set_yscale("log")
            ax.set_xticks(list(map(lambda n: n * bar_group_area, range(len(cases)))))
            ax.set_xticklabels(cases)
        else:
            ax.set_xlabel("Time (ms)")
            ax.set_xscale("log")
            ax.set_yticks(list(map(lambda n: n * bar_group_area, range(len(cases)))))
            ax.set_yticklabels(cases)
        # for tick in ax.yaxis.get_major_ticks()[0::2]:
        #     tick.set_pad(12)
    #if not is_horizontal:
        #analyses = list(reversed(analyses))
    ax.legend([
        mplot.Rectangle((0,0),1,1,fc=analysis_colors[analysis])
            for analysis in analyses
        ],
        analyses
    )
    figure.savefig(output_dir + os.sep + name + "-bars.svg")

def main():
    args = parse_args()
    data = read_csvs(args.csv_files)
    for group in data:
        if data[group]["cases"] and data[group]["analyses"]:
            produce_bar_chart_from(args.output_dir, group, data[group],
                                   args.horizontal)

main()
