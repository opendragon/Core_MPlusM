# File: Makefile

MMpsfiles = MMdbDiagram.ps MMmatchSyntaxrails.ps MMcmDiagram.ps MMmpmDiagram.ps

LMpsfiles =

MMimagefiles = ChannelManager.eps

LMimagefiles =

MMmainsource = MpM_Manual.tex

LMmainsource = LeapMotionInput_Manual.tex

MMsecondarysources = MMcaseStudy.tex MMclasses.tex MMdocumentationHistory.tex \
	MMexamples.tex MMexemplars.tex MMfrontispiece.tex MMinternalDb.tex \
	MMmatchSyntax.tex MMoverview.tex MMregistrationSequence.tex MMservices.tex \
	MMutilities.tex sharedSetup.tex

LMsecondarysources = LMdocumentationHistory.tex LMfrontispiece.tex LMmessageFormat.tex \
	LMoverview.tex LMservice.tex sharedSetup.tex

MMmaindvi = $(MMmainsource:.tex=.dvi)

LMmaindvi = $(LMmainsource:.tex=.dvi)

MMmainidx = $(MMmainsource:.tex=.idx)

LMmainidx = $(LMmainsource:.tex=.idx)

MMmainps = $(MMmainsource:.tex=.ps)

LMmainps = $(LMmainsource:.tex=.ps)

MMmainpdf = $(MMmainsource:.tex=.pdf)

LMmainpdf = $(LMmainsource:.tex=.pdf)

MMtarget = $(HOME)/$(MMmainpdf)

LMtarget = $(HOME)/$(LMmainpdf)

%.ps: %.mp
	mpost -interaction=batchmode $<

$(MMmaindvi) : $(MMimagefiles) $(MMpsfiles) $(MMmainsource) $(MMsecondarysources)
	latex -interaction=batchmode $(MMmainsource)
	latex -interaction=batchmode $(MMmainsource)
	latex -interaction=batchmode $(MMmainsource)
	makeindex $(MMmainidx)
	latex -interaction=batchmode $(MMmainsource)

$(LMmaindvi) : $(LMimagefiles) $(LMpsfiles) $(LMmainsource) $(LMsecondarysources)
	latex -interaction=batchmode $(LMmainsource)
	latex -interaction=batchmode $(LMmainsource)
	latex -interaction=batchmode $(LMmainsource)
	makeindex $(LMmainidx)
	latex -interaction=batchmode $(LMmainsource)

$(MMmainps) : $(MMmaindvi)
	dvips $(MMmaindvi) -o $(MMmainps)

$(LMmainps) : $(LMmaindvi)
	dvips $(LMmaindvi) -o $(LMmainps)

$(MMmainpdf) : $(MMmainps)
	gs -dCompatibilityLevel=1.2 -dMaxSubsetPct=100 -dNOPAUSE -dBATCH \
		-sDEVICE=pdfwrite -sPAPERSIZE=letter -sOutputFile="$(MMmainpdf)" \
		-c save pop -f "$(MMmainps)" -sFONTPATH=.

$(LMmainpdf) : $(LMmainps)
	gs -dCompatibilityLevel=1.2 -dMaxSubsetPct=100 -dNOPAUSE -dBATCH \
		-sDEVICE=pdfwrite -sPAPERSIZE=letter -sOutputFile="$(LMmainpdf)" \
		-c save pop -f "$(LMmainps)" -sFONTPATH=.

$(MMtarget) : $(MMmainpdf)
	cp $(MMmainpdf) $(MMtarget)

$(LMtarget) : $(LMmainpdf)
	cp $(LMmainpdf) $(LMtarget)

.PHONY : clean manual

clean :
	rm $(MMpsfiles) $(LMpsfiles) *.log *.pdf *.mpx *.dvi

manual : $(MMtarget) $(LMtarget)

