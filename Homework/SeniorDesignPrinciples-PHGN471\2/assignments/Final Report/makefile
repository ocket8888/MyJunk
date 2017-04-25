all: report_fieck.pdf

report_fieck.pdf: report_fieck.tex
	pdflatex report_fieck.tex -interaction=nonstopmode

%.tex:
	

clean:
	rm -rf ./*.aux ./*.log

.PHONY: all clean
