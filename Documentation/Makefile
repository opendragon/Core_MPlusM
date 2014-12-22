# File: Makefile

# Intermediate Postscript files

CMpsfiles = CMcmDiagram.ps 

IMpsfiles =

KV2psfiles =

LMpsfiles =

MMpsfiles = MMdbDiagram.ps MMmatchSyntaxrails.ps MMmpmDiagram.ps

VDSpsfiles =

# Input image files

CMimagefiles = images/afterAdd.eps images/backgroundMenu.eps images/beforeAdd.eps \
	images/beforeRemove.eps images/ChannelManager.eps images/dragAdd2.eps \
	images/entityMenu.eps images/portMenu.eps \
	images/regularChannelMenuMetricsDisabled.eps \
	images/regularChannelMenuMetricsEnabled.eps \
	images/serviceChannelMenuMetricsDisabled.eps \
	images/serviceChannelMenuMetricsEnabled.eps \
	images/serviceMenuMetricsDisabled.eps \
	images/serviceMenuMetricsEnabled.eps

IMimagefiles = images/Installer01.eps images/Installer02.eps images/Installer03.eps \
	images/Installer04.eps images/Installer05.eps images/Installer05a.eps \
	images/Installer06.eps images/Installer06a.eps images/Installer06b.eps \
	images/Installer07.eps images/Installer08.eps images/RunningChannelManager.eps \
	images/win_install01.eps images/win_install02.eps images/win_install03.eps \
	images/win_install04.eps images/win_install05.eps images/win_install06.eps \
	images/win_install07.eps images/win_install08.eps images/win_install09.eps \
	images/win_install10.eps images/win_install11.eps images/win_run01.eps

KV2imagefiles =

LMimagefiles =

MMimagefiles = 

VDSimagefiles =

# Main source file

CMmainsource = ChannelManagerUtility_Manual.tex

IMmainsource = Installation_Manual.tex

KV2mainsource = KinectV2Input_Manual.tex

LMmainsource = LeapMotionInput_Manual.tex

MMmainsource = MpM_Manual.tex

VDSmainsource = ViconDataStreamInput_Manual.tex

# Secondary source files

Commonsources = boilerPlate.tex sharedSetup.tex

CMsecondarysources = CMdocumentationHistory.tex CMfrontispiece.tex CMoverview.tex \
	CMutility.tex $(Commonsources)
	
IMsecondarysources = IMdocumentationHistory.tex IMfrontispiece.tex IMoverview.tex \
	IMprocedure.tex $(Commonsources)

KV2secondarysources = KV2documentationHistory.tex KV2frontispiece.tex \
	KV2messageFormat.tex KV2overview.tex KV2service.tex $(Commonsources)

LMsecondarysources = LMdocumentationHistory.tex LMfrontispiece.tex LMmessageFormat.tex \
	LMoverview.tex LMservice.tex $(Commonsources)

MMsecondarysources = MMcaseStudy.tex MMclasses.tex MMdocumentationHistory.tex \
	MMexamples.tex MMexemplars.tex MMfrontispiece.tex MMinternalDb.tex \
	MMmatchSyntax.tex MMoverview.tex MMregistrationSequence.tex MMservices.tex \
	MMutilities.tex $(Commonsources)

VDSsecondarysources = VDSdocumentationHistory.tex VDSfrontispiece.tex \
	VDSmessageFormat.tex VDSoverview.tex VDSservice.tex $(Commonsources)

# DVI files

CMmaindvi = $(CMmainsource:.tex=.dvi)

IMmaindvi = $(IMmainsource:.tex=.dvi)

KV2maindvi = $(KV2mainsource:.tex=.dvi)

LMmaindvi = $(LMmainsource:.tex=.dvi)

MMmaindvi = $(MMmainsource:.tex=.dvi)

VDSmaindvi = $(VDSmainsource:.tex=.dvi)

# Index files

CMmainidx = $(CMmainsource:.tex=.idx)

IMmainidx = $(IMmainsource:.tex=.idx)

KV2mainidx = $(KV2mainsource:.tex=.idx)

LMmainidx = $(LMmainsource:.tex=.idx)

MMmainidx = $(MMmainsource:.tex=.idx)

VDSmainidx = $(VDSmainsource:.tex=.idx)

# Intermediate Postscript files

CMmainps = $(CMmainsource:.tex=.ps)

IMmainps = $(IMmainsource:.tex=.ps)

KV2mainps = $(KV2mainsource:.tex=.ps)

LMmainps = $(LMmainsource:.tex=.ps)

MMmainps = $(MMmainsource:.tex=.ps)

VDSmainps = $(VDSmainsource:.tex=.ps)

# Output PDF files

CMmainpdf = $(CMmainsource:.tex=.pdf)

IMmainpdf = $(IMmainsource:.tex=.pdf)

KV2mainpdf = $(KV2mainsource:.tex=.pdf)

LMmainpdf = $(LMmainsource:.tex=.pdf)

MMmainpdf = $(MMmainsource:.tex=.pdf)

VDSmainpdf = $(VDSmainsource:.tex=.pdf)

# Target files

CMtarget = $(HOME)/$(CMmainpdf)

IMtarget = $(HOME)/$(IMmainpdf)

KV2target = $(HOME)/$(KV2mainpdf)

LMtarget = $(HOME)/$(LMmainpdf)

MMtarget = $(HOME)/$(MMmainpdf)

VDStarget = $(HOME)/$(VDSmainpdf)

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

$(CMmaindvi) : $(CMimagefiles) $(CMpsfiles) $(CMmainsource) $(CMsecondarysources)
	latex -interaction=batchmode $(CMmainsource)
	latex -interaction=batchmode $(CMmainsource)
	latex -interaction=batchmode $(CMmainsource)
	makeindex $(CMmainidx)
	latex -interaction=batchmode $(CMmainsource)

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

$(VDSmaindvi) : $(VDSimagefiles) $(VDSpsfiles) $(VDSmainsource) $(VDSsecondarysources)
	latex -interaction=batchmode $(VDSmainsource)
	latex -interaction=batchmode $(VDSmainsource)
	latex -interaction=batchmode $(VDSmainsource)
	makeindex $(VDSmainidx)
	latex -interaction=batchmode $(VDSmainsource)

# Top-level targets

.PHONY : clean all

.PRECIOUS : %.ps %.pdf

clean :
	rm -f $(CMpsfiles) $(IMpsfiles) $(KV2psfiles) $(LMpsfiles) $(MMpsfiles) $(VDSpsfiles)
	rm -f *.log *.pdf *.mpx *.dvi *.aux *.idx *.ilg *.ind *.lof *.out *.ps *.toc

all : $(CMtarget) $(IMtarget) $(KV2target) $(LMtarget) $(MMtarget) $(VDStarget)
