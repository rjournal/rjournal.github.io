
clean: RJwrapper.pdf

	rm RJwrapper.aux RJwrapper.bbl RJwrapper.blg RJwrapper.brf RJwrapper.log 
	rm RJwrapper.out 

build: 

	pdflatex RJwrapper.tex
	bibtex RJwrapper
	pdflatex RJwrapper.tex
	pdflatex RJwrapper.tex
	pdflatex RJwrapper.tex
