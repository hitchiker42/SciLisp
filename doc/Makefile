.PHONY: all info pdf clean
all: info pdf
info: manual.texi
	makeinfo manual.texi -o SciLisp.info
pdf: manual.texi
	makeinfo --pdf manual.texi -o SciLisp.pdf
clean: 
	rm SciLisp.*
