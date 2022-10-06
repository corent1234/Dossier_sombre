all: test_my_list rapport

test_my_list: my_list.ml test_my_list.ml
	ocamlc my_list.mli my_list.ml test_my_list.ml -o test_my_list

rapport: Rapport.tex
	pdflatex -shell-escape Rapport.tex -o rapport.pdf

clean:
	rm -rf test_list *.cmi *.cmo *~

