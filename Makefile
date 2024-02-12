SRC= syntax.ml myparser.mly mylexer.mll eval.ml main.ml 
COMPONENT= syntax.ml myparser.mli myparser.ml mylexer.ml eval.ml main.ml 
TARGET= miniocaml
TESTS= test_expression test_environment test_parse_and_run test_function test_recursive_function test_type_err test_list test_type_checker

.DEFAULT_GOAL := $(TARGET)
.PHONY: clean $(TESTS)

all:	$(TARGET)

$(TARGET): 	$(COMPONENT) 
	ocamlmktop $(COMPONENT) -w -31 -o $(TARGET)

myparser.mly: parser.mly
	cp parser.mly myparser.mly

mylexer.mll: lexer.mll
	sed -e 's/Parser/Myparser/g' lexer.mll > mylexer.mll

myparser.mli:	myparser.mly
	ocamlyacc myparser.mly

myparser.ml:	myparser.mly
	ocamlyacc myparser.mly

mylexer.ml:	mylexer.mll
	ocamllex mylexer.mll

clean:
	/bin/rm -f \
		myparser.ml myparser.mly myparser.mli \
		mylexer.ml mylexer.mll \
		$(TARGET) \
		**/*.cmi **/*.cmo **/*.mli \
		*.cmi *.cmo *.mli \
		$(foreach test, $(TESTS), tests/$(test))
		

test_expression: syntax.ml eval.ml ./tests/test_expression.ml
	ocamlc -o tests/$@ $^

test_environment: syntax.ml eval.ml ./tests/test_environment.ml
	ocamlc -o tests/$@ $^

test_parse_and_run: syntax.ml myparser.mli myparser.ml mylexer.ml eval.ml ./tests/test_parse_and_run.ml
	ocamlc -o tests/$@ $^

test_function: syntax.ml myparser.mli myparser.ml mylexer.ml eval.ml ./tests/test_function.ml
	ocamlc -o tests/$@ $^

test_recursive_function: syntax.ml myparser.mli myparser.ml mylexer.ml eval.ml ./tests/test_recursive_function.ml
	ocamlc -o tests/$@ $^

test_type_err: syntax.ml myparser.mli myparser.ml mylexer.ml eval.ml ./tests/test_type_err.ml
	ocamlc -o tests/$@ $^

test_list: syntax.ml myparser.mli myparser.ml mylexer.ml eval.ml ./tests/test_list.ml
	ocamlc -o tests/$@ $^

test_type_checker: syntax.ml myparser.mli myparser.ml mylexer.ml eval.ml ./tests/test_type_checker.ml
	ocamlc -o tests/$@ $^

test: $(TESTS)
	$(foreach test, $(TESTS), echo "running $(test)" && tests/$(test) &&) echo "OK";
