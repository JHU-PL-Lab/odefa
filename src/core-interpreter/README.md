
To generate graph json file:
run ./toploop_main.native --ddpa-logging=result

This generates ddpa_graphs.log.json file

Rename it to ddpa_graphs.json (yojson doesn't like two periods in the name of the file I think)
cp ddpa_graphs.log.json ddpa_graphs.json

Symbolic_generator.ml will assume the ddpa_graphs.json file is the file is wants to use.
