#
# Makefile
#

SRC= syntax.ml myparser.mly mylexer.mll eval.ml main.ml 
COMPONENT= syntax.ml myparser.mli myparser.ml mylexer.ml eval.ml main.ml 
TARGET= miniocaml

.DEFAULT_GOAL := $(TARGET)
.PHONY: clean test_expression test_environment test_parse_and_run test_function

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
		./tests/test_expression ./tests/test_environment ./tests/test_parse_and_run ./tests/test_function

test_expression: syntax.ml eval.ml ./tests/test_expression.ml
	ocamlc -o tests/$@ $^

test_environment: syntax.ml eval.ml ./tests/test_environment.ml
	ocamlc -o tests/$@ $^

test_parse_and_run: syntax.ml myparser.mli myparser.ml mylexer.ml eval.ml ./tests/test_parse_and_run.ml
	ocamlc -o tests/$@ $^

test_function: syntax.ml myparser.mli myparser.ml mylexer.ml eval.ml ./tests/test_function.ml
	ocamlc -o tests/$@ $^
