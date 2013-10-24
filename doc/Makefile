.PHONY: all info pdf clean
all: info pdf
manual_m4.texi: manual.texi manual.m4
	m4 -P manual.texi > $@
info: manual_m4.texi
	makeinfo manual_m4.texi -o SciLisp.info
pdf: manual_m4.texi
	makeinfo --pdf manual_m4.texi -o SciLisp.pdf
	mv manual_m4.texi temp.texi;rm manual_m4.*;mv temp.texi manual_m4.texi
clean: 
	rm manual_m4.*;rm SciLisp.*
