#!/usr/bin/env python3

def analyses_in_preferred_order(input_analyses):
    preferred_analysis_order = ["p4f", "ddpa", "kplume", "splume", "boomerangSPDS", "boomerangOriginal"]
    analyses = []
    for analysis in preferred_analysis_order:
        if analysis in input_analyses:
            analyses.append(analysis)
    for analysis in input_analyses:
        if analysis not in preferred_analysis_order:
            analyses.append(analysis)
    return analyses
