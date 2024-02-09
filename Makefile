#
# Makefile
#

SRC= mysyntax.ml myparser.mly mylexer.mll eval.ml main.ml 
COMPONENT= mysyntax.ml myparser.mli myparser.ml mylexer.ml eval.ml main.ml 
TARGET= miniocaml

.DEFAULT_GOAL := $(TARGET)
.PHONY: clean test_expression test_environment

all:	$(TARGET)

$(TARGET): 	$(COMPONENT) 
	ocamlmktop $(COMPONENT) -w -31 -o $(TARGET)

parser.mly:
	wget https://www.logic.cs.tsukuba.ac.jp/jikken/parser.mly

lexer.mll:
	wget https://www.logic.cs.tsukuba.ac.jp/jikken/lexer.mll

syntax.ml:
	wget https://www.logic.cs.tsukuba.ac.jp/jikken/syntax.ml

myparser.mly: parser.mly
	sed -e 's/Syntax/Mysyntax/g' parser.mly > myparser.mly

mylexer.mll: lexer.mll
	sed -e 's/Parser/Myparser/g' lexer.mll > mylexer.mll

mysyntax.ml: syntax.ml
	cp syntax.ml mysyntax.ml

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
		mysyntax.ml \
		$(TARGET) \
		*.cmi *.cmo *.mli \
		test_expression test_environment 

clean_all: clean
	/bin/rm -f \
		parser.mly lexer.mll syntax.ml

test_expression: processor.ml test_expression.ml
	ocamlc -o $@ $^
	./test_expression

test_environment: processor.ml test_environment.ml
	ocamlc -o $@ $^
	./test_environment
