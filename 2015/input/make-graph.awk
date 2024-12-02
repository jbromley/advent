BEGIN {
  printf("digraph G {\n");
}
/AND|OR/ {
  printf("\t%s -> %s [label = \"%s\"];\n\t%s -> %s [label = \"%s\"];\n", $1, $5, $2, $3, $5, $2);
}
/NOT/ {
  printf("\t%s -> %s [label = \"NOT\"];\n", $2, $4);
}
/LSHIFT|RSHIFT/ {
  printf("\t%s -> %s [label = \"%s %s\"];\n", $1, $5, $2, $3); 
}
/^[a-z]{2} ->/ {
  printf("\t%s -> %s;\n", $1, $3);
 }
/^[0-9]+ ->/ {
  printf("\t%s -> %s;\n", $1, $3);
}
END {
  printf("}\n")
}
