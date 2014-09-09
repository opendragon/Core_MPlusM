
psfiles = dbDiagram.ps matchSyntaxrails.ps cmDiagram.ps mpmDiagram.ps

imagefiles = ChannelManager.eps

mainsource = MpM_Manual.tex

secondarysources = classes.tex documentationHistory.tex examples.tex frontispiece.tex \
	internalDb.tex matchSyntax.tex overview.tex registrationSequence.tex services.tex \
	utilities.tex

maindvi = $(mainsource:.tex=.dvi)

mainidx = $(mainsource:.tex=.idx)

mainps = $(mainsource:.tex=.ps)

mainpdf = $(mainsource:.tex=.pdf)

target = $(HOME)/$(mainpdf)

#$(psfiles) : maxboxmacros.mp fixedBox.mp

%.ps: %.mp
	mpost -interaction=batchmode $<

#$(maindvi) : $(imagefiles) $(psfiles) $(texfiles) $(mainsource) $(secondarysources)
$(maindvi) : $(imagefiles) $(psfiles) $(mainsource) $(secondarysources)
	latex -interaction=batchmode $(mainsource)
	latex -interaction=batchmode $(mainsource)
	latex -interaction=batchmode $(mainsource)
	makeindex $(mainidx)
	latex -interaction=batchmode $(mainsource)

$(mainps) : $(maindvi)
	dvips $(maindvi) -o $(mainps)

$(mainpdf) : $(mainps)
	gs -dCompatibilityLevel=1.2 -dMaxSubsetPct=100 -dNOPAUSE -dBATCH \
		-sDEVICE=pdfwrite -sPAPERSIZE=letter -sOutputFile="$(mainpdf)" \
		-c save pop -f "$(mainps)" -sFONTPATH=.

$(target) : $(mainpdf)
	cp $(mainpdf) $(target)

.PHONY : clean manual

clean :
	rm $(psfiles) *.log *.pdf *.mpx *.dvi

manual : $(target)

