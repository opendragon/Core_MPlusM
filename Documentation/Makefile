# File: Makefile

# Intermediate Postscript files

IMpsfiles =

KV2psfiles =

LMpsfiles =

MMpsfiles = MMdbDiagram.ps MMmatchSyntaxrails.ps MMcmDiagram.ps MMmpmDiagram.ps

# Input image files

IMimagefiles = Installer01.eps Installer02.eps Installer03.eps Installer04.eps \
	Installer05.eps Installer05a.eps Installer06.eps Installer06a.eps Installer06b.eps \
	Installer07.eps Installer08.eps

KV2imagefiles =

LMimagefiles =

MMimagefiles = ChannelManager.eps

# Main source file

IMmainsource = Installation_Manual.tex

KV2mainsource = KinectV2Input_Manual.tex

LMmainsource = LeapMotionInput_Manual.tex

MMmainsource = MpM_Manual.tex

# Secondary source files

IMsecondarysources = IMdocumentationHistory.tex IMfrontispiece.tex IMoverview.tex \
	IMprocedure.tex sharedSetup.tex

KV2secondarysources = KV2documentationHistory.tex KV2frontispiece.tex \
	KV2messageFormat.tex KV2overview.tex KV2service.tex sharedSetup.tex

LMsecondarysources = LMdocumentationHistory.tex LMfrontispiece.tex LMmessageFormat.tex \
	LMoverview.tex LMservice.tex sharedSetup.tex

MMsecondarysources = MMcaseStudy.tex MMclasses.tex MMdocumentationHistory.tex \
	MMexamples.tex MMexemplars.tex MMfrontispiece.tex MMinternalDb.tex \
	MMmatchSyntax.tex MMoverview.tex MMregistrationSequence.tex MMservices.tex \
	MMutilities.tex sharedSetup.tex

# DVI files

IMmaindvi = $(IMmainsource:.tex=.dvi)

KV2maindvi = $(KV2mainsource:.tex=.dvi)

LMmaindvi = $(LMmainsource:.tex=.dvi)

MMmaindvi = $(MMmainsource:.tex=.dvi)

# Index files

IMmainidx = $(IMmainsource:.tex=.idx)

KV2mainidx = $(KV2mainsource:.tex=.idx)

LMmainidx = $(LMmainsource:.tex=.idx)

MMmainidx = $(MMmainsource:.tex=.idx)

# Intermediate Postscript files

IMmainps = $(IMmainsource:.tex=.ps)

KV2mainps = $(KV2mainsource:.tex=.ps)

LMmainps = $(LMmainsource:.tex=.ps)

MMmainps = $(MMmainsource:.tex=.ps)

# Output PDF files

IMmainpdf = $(IMmainsource:.tex=.pdf)

KV2mainpdf = $(KV2mainsource:.tex=.pdf)

LMmainpdf = $(LMmainsource:.tex=.pdf)

MMmainpdf = $(MMmainsource:.tex=.pdf)

# Target files

IMtarget = $(HOME)/$(IMmainpdf)

KV2target = $(HOME)/$(KV2mainpdf)

LMtarget = $(HOME)/$(LMmainpdf)

MMtarget = $(HOME)/$(MMmainpdf)

# General rules

%.ps: %.mp
	mpost -interaction=batchmode $<

%.ps: %.dvi
	dvips $< -o $@

%.pdf : %.ps
	gs -dCompatibilityLevel=1.2 -dMaxSubsetPct=100 -dNOPAUSE -dBATCH \
		-sDEVICE=pdfwrite -sPAPERSIZE=letter -sOutputFile=$@ \
		-c save pop -f $< -sFONTPATH=.

$(HOME)/%.pdf : %.pdf
	cp $< $@

# Specific rules

$(IMmaindvi) : $(IMimagefiles) $(IMpsfiles) $(IMmainsource) $(IMsecondarysources)
	latex -interaction=batchmode $(IMmainsource)
	latex -interaction=batchmode $(IMmainsource)
	latex -interaction=batchmode $(IMmainsource)
	makeindex $(IMmainidx)
	latex -interaction=batchmode $(IMmainsource)

$(KV2maindvi) : $(KV2imagefiles) $(KV2psfiles) $(KV2mainsource) $(KV2secondarysources)
	latex -interaction=batchmode $(KV2mainsource)
	latex -interaction=batchmode $(KV2mainsource)
	latex -interaction=batchmode $(KV2mainsource)
	makeindex $(KV2mainidx)
	latex -interaction=batchmode $(KV2mainsource)

$(LMmaindvi) : $(LMimagefiles) $(LMpsfiles) $(LMmainsource) $(LMsecondarysources)
	latex -interaction=batchmode $(LMmainsource)
	latex -interaction=batchmode $(LMmainsource)
	latex -interaction=batchmode $(LMmainsource)
	makeindex $(LMmainidx)
	latex -interaction=batchmode $(LMmainsource)

$(MMmaindvi) : $(MMimagefiles) $(MMpsfiles) $(MMmainsource) $(MMsecondarysources)
	latex -interaction=batchmode $(MMmainsource)
	latex -interaction=batchmode $(MMmainsource)
	latex -interaction=batchmode $(MMmainsource)
	makeindex $(MMmainidx)
	latex -interaction=batchmode $(MMmainsource)

# Top-level targets

.PHONY : clean manual

.PRECIOUS : %.ps %.pdf

clean :
	rm $(IMpsfiles) $(KV2psfiles) $(LMpsfiles) $(MMpsfiles) *.log *.pdf *.mpx *.dvi

manual : $(IMtarget) $(KV2target) $(LMtarget) $(MMtarget)
