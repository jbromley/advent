#! /usr/bin/env bash
tail -n +3 input | tr -d '=(,)' | gawk 'BEGIN {printf("digraph {\n");} {printf("  %s -> %s;\n  %s -> %s;\n", $1, $2, $1, $3);} END {printf("}\n"); }' >wasteland.dot
fdp -Tpng -O wasteland.dot
