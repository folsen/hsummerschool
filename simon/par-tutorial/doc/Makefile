all : book.pdf lab-exercises.pdf

clean :
	rm book.pdf *.bbl *.aux *.blg *.log *.out *.toc

%.pdf : %.tex %.bib
	pdflatex $<
	bibtex $*
	pdflatex $<
	pdflatex $<

SRCS = \
  book.tex \
  par.tex \
  conc.tex \
  conc-asyncex.tex \
  conc-fork.tex \
  conc-mvar.tex \
  conc-stm.tex \
  conc-ffi.tex \
  conc-higher.tex \
  conc-server.tex \
  conc-distrib.tex \
  accel.tex

book.pdf : $(SRCS)

lab-exercises.pdf : lab-exercises.tex
	pdflatex $<
	pdflatex $<

kmeans-example.png : kmeans-example-points kmeans-example-clusters
	gnuplot -e 'set key off; set terminal png; plot "kmeans-example-points","kmeans-example-clusters" with points pointsize 20 pointtype 6' >$@

kmeans-results.png : kmeans-results
	gnuplot -e 'set key off; set terminal png; plot "kmeans-results" with linespoints' >$@


upload : book.pdf lab-exercises.pdf
	scp book.pdf community:public_html/par-tutorial-cadarache.pdf
	scp lab-exercises.pdf community:public_html/lab-exercises-cadarache.pdf

# -----------------------------------------------------------------------------

GHC = ghc
GHC_OPTS =

SPLICE=utils/splicecode

$(SPLICE) : utils/splicecode.hs
	$(GHC) $(GHC_OPTS) $< -o $@

splice : $(SPLICE)
	$(SPLICE) $(SRCS)
