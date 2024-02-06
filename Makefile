
.PHONY: processor main test_expression clean

processor: processor.ml
	ocamlc -c processor.ml

main: processor main.ml
	ocamlc -o main processor.cmo main.ml

test_expression: processor test_expression.ml
	ocamlc -o test_expression processor.cmo test_expression.ml
	./test_expression

test_environment: processor test_environment.ml
	ocamlc -o test_environment processor.cmo test_environment.ml
	./test_environment

clean:
	rm -f *.cmo *.cmi main test_expression test_environment

.DEFAULT_GOAL := main
