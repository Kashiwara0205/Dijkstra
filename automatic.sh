ocamlopt -c station_variables.ml metoro_network.ml test.ml
ocamlopt -o example station_variables.cmx metoro_network.cmx test.cmx
echo "完了"