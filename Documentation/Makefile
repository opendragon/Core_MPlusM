# File: Makefile

IMAGE_DIR = mpm_images

# Intermediate Postscript files

CMpsfiles = CMcmDiagram.ps 

IMpsfiles =

JSpsfiles =

KV2psfiles =

LMpsfiles =

MMpsfiles = MMdbDiagram.ps MMmatchSyntaxrails.ps MMmpmDiagram.ps

VDSpsfiles =

# Intermediate Encapsulated Postscript files

CMepsfiles = $(IMAGE_DIR)/CMcmDiagram.eps 

IMepsfiles =

JSepsfiles =

KV2epsfiles =

LMepsfiles =

MMepsfiles = $(IMAGE_DIR)/MMdbDiagram.eps $(IMAGE_DIR)/MMmatchSyntaxrails.eps \
	$(IMAGE_DIR)/MMmpmDiagram.eps

VDSepsfiles =

# Intermediate Portable Network Graphics files

Commonpngfiles = $(IMAGE_DIR)/m+m_tight_logo.png

CMpngfiles = $(IMAGE_DIR)/CMcmDiagram.png $(Commonpngfiles)

IMpngfiles = $(Commonpngfiles)

JSpngfiles = $(Commonpngfiles)

KV2pngfiles = $(Commonpngfiles)

LMpngfiles = $(Commonpngfiles)

MMpngfiles = $(IMAGE_DIR)/MMdbDiagram.png $(IMAGE_DIR)/MMmatchSyntaxrails.png \
	$(IMAGE_DIR)/MMmpmDiagram.png $(Commonpngfiles) 

VDSpngfiles =

# Input bitmap EPS files

CMbitmapepsfiles = $(IMAGE_DIR)/afterAdd.eps $(IMAGE_DIR)/backgroundMenu.eps \
	$(IMAGE_DIR)/beforeAdd.eps $(IMAGE_DIR)/beforeRemove.eps \
	$(IMAGE_DIR)/ChannelManager.eps $(IMAGE_DIR)/clickAdd.eps $(IMAGE_DIR)/dragAdd2.eps \
	$(IMAGE_DIR)/entityMenu.eps $(IMAGE_DIR)/portMenu.eps \
	$(IMAGE_DIR)/regularChannelMenuMetricsDisabled.eps \
	$(IMAGE_DIR)/regularChannelMenuMetricsEnabled.eps \
	$(IMAGE_DIR)/serviceChannelMenuMetricsDisabled.eps \
	$(IMAGE_DIR)/serviceChannelMenuMetricsEnabled.eps \
	$(IMAGE_DIR)/serviceMenuMetricsDisabled.eps $(IMAGE_DIR)/serviceMenuMetricsEnabled.eps

IMbitmapepsfiles = $(IMAGE_DIR)/Installer01.eps $(IMAGE_DIR)/Installer02.eps \
	$(IMAGE_DIR)/Installer03.eps $(IMAGE_DIR)/Installer04.eps \
	$(IMAGE_DIR)/Installer05.eps $(IMAGE_DIR)/Installer05a.eps \
	$(IMAGE_DIR)/Installer06.eps $(IMAGE_DIR)/Installer06a.eps \
	$(IMAGE_DIR)/Installer06b.eps $(IMAGE_DIR)/Installer07.eps \
	$(IMAGE_DIR)/Installer08.eps $(IMAGE_DIR)/RunningChannelManager.eps \
	$(IMAGE_DIR)/win_install01.eps $(IMAGE_DIR)/win_install02.eps \
	$(IMAGE_DIR)/win_install03.eps $(IMAGE_DIR)/win_install04.eps \
	$(IMAGE_DIR)/win_install05.eps $(IMAGE_DIR)/win_install06.eps \
	$(IMAGE_DIR)/win_install07.eps $(IMAGE_DIR)/win_install08.eps \
	$(IMAGE_DIR)/win_install09.eps $(IMAGE_DIR)/win_install10.eps \
	$(IMAGE_DIR)/win_install11.eps $(IMAGE_DIR)/win_run01.eps

JSbitmapepsfiles =

KV2bitmapepsfiles =

LMbitmapepsfiles =

MMbitmapepsfiles = 

VDSbitmapepsfiles =

# Input bitmap PNG files

CMbitmappngfiles = $(CMbitmapepsfiles:.eps=.png)

IMbitmappngfiles = $(IMbitmapepsfiles:.eps=.png)

JSbitmappngfiles = $(JSbitmapepsfiles:.eps=.png)

KV2bitmappngfiles = $(KV2bitmapepsfiles:.eps=.png)

LMbitmappngfiles = $(LMbitmapepsfiles:.eps=.png)

MMbitmappngfiles = $(MMbitmapepsfiles:.eps=.png)

VDSbitmappngfiles = $(VDSbitmapepsfiles:.eps=.png)

# Main source file

CMmainsource = ChannelManagerUtility_Manual.tex

IMmainsource = Installation_Manual.tex

JSmainsource = JavaScript_Manual.tex

KV2mainsource = KinectV2Input_Manual.tex

LMmainsource = LeapMotionInput_Manual.tex

MMmainsource = MpM_Manual.tex

VDSmainsource = ViconDataStreamInput_Manual.tex

# Secondary source files

Commonsources = boilerPlate.tex sharedSetup.tex

CMsecondaryfiles = CMdocumentationHistory.tex CMfrontispiece.tex CMoverview.tex \
	CMutility.tex $(Commonsources)
	
IMsecondaryfiles = IMdocumentationHistory.tex IMfrontispiece.tex IMoverview.tex \
	IMprocedure.tex $(Commonsources)

JSsecondaryfiles = JSdocumentationHistory.tex JSfrontispiece.tex JSoverview.tex \
	JSfileFormat.tex JSservice.tex JSexamples.tex $(Commonsources)
	
KV2secondaryfiles = KV2documentationHistory.tex KV2frontispiece.tex \
	KV2messageFormat.tex KV2overview.tex KV2service.tex $(Commonsources)

LMsecondaryfiles = LMdocumentationHistory.tex LMfrontispiece.tex LMmessageFormat.tex \
	LMoverview.tex LMservice.tex $(Commonsources)

MMsecondaryfiles = MMbuildingFromSource.tex MMcaseStudy.tex MMclasses.tex \
	MMdocumentationHistory.tex MMexamples.tex MMexemplars.tex MMfrontispiece.tex \
	MMinternalDb.tex MMmatchSyntax.tex MMoverview.tex MMregistrationSequence.tex \
	MMservices.tex MMutilities.tex $(Commonsources)

VDSsecondaryfiles = VDSdocumentationHistory.tex VDSfrontispiece.tex \
	VDSmessageFormat.tex VDSoverview.tex VDSservice.tex $(Commonsources)

# DVI files

CMmaindvi = $(CMmainsource:.tex=.dvi)

IMmaindvi = $(IMmainsource:.tex=.dvi)

JSmaindvi = $(JSmainsource:.tex=.dvi)

KV2maindvi = $(KV2mainsource:.tex=.dvi)

LMmaindvi = $(LMmainsource:.tex=.dvi)

MMmaindvi = $(MMmainsource:.tex=.dvi)

VDSmaindvi = $(VDSmainsource:.tex=.dvi)

# Index files

CMmainidx = $(CMmainsource:.tex=.idx)

IMmainidx = $(IMmainsource:.tex=.idx)

JSmainidx = $(JSmainsource:.tex=.idx)

KV2mainidx = $(KV2mainsource:.tex=.idx)

LMmainidx = $(LMmainsource:.tex=.idx)

MMmainidx = $(MMmainsource:.tex=.idx)

VDSmainidx = $(VDSmainsource:.tex=.idx)

# Intermediate Postscript files

CMmainps = $(CMmainsource:.tex=.ps)

IMmainps = $(IMmainsource:.tex=.ps)

JSmainps = $(JSmainsource:.tex=.ps)

KV2mainps = $(KV2mainsource:.tex=.ps)

LMmainps = $(LMmainsource:.tex=.ps)

MMmainps = $(MMmainsource:.tex=.ps)

VDSmainps = $(VDSmainsource:.tex=.ps)

# Output HTML files

CMmainhtml = $(CMmainsource:.tex=.html)

IMmainhtml = $(IMmainsource:.tex=.html)

JSmainhtml = $(JSmainsource:.tex=.html)

KV2mainhtml = $(KV2mainsource:.tex=.html)

LMmainhtml = $(LMmainsource:.tex=.html)

MMmainhtml = $(MMmainsource:.tex=.html)

VDSmainhtml = $(VDSmainsource:.tex=.html)

# Output PDF files

CMmainpdf = $(CMmainsource:.tex=.pdf)

IMmainpdf = $(IMmainsource:.tex=.pdf)

JSmainpdf = $(JSmainsource:.tex=.pdf)

KV2mainpdf = $(KV2mainsource:.tex=.pdf)

LMmainpdf = $(LMmainsource:.tex=.pdf)

MMmainpdf = $(MMmainsource:.tex=.pdf)

VDSmainpdf = $(VDSmainsource:.tex=.pdf)

# Target files

CMtarget = $(HOME)/$(CMmainpdf) $(HOME)/$(CMmainhtml)

IMtarget = $(HOME)/$(IMmainpdf) $(HOME)/$(IMmainhtml)

JStarget = $(HOME)/$(JSmainpdf) $(HOME)/$(JSmainhtml)

KV2target = $(HOME)/$(KV2mainpdf) $(HOME)/$(KV2mainhtml)

LMtarget = $(HOME)/$(LMmainpdf) $(HOME)/$(LMmainhtml)

MMtarget = $(HOME)/$(MMmainpdf) $(HOME)/$(MMmainhtml)

VDStarget = $(HOME)/$(VDSmainpdf) $(HOME)/$(VDSmainhtml)

# General rules

%.eps: %.ps
	ps2eps -f $<

%.png: %.eps
	convert -density 150 -transparent white $< $@

%.ps: %.mp
	mpost -interaction=batchmode -file-line-error $<

%.ps: %.dvi
	dvips -o $@ $<

%.pdf : %.ps
	gs -dCompatibilityLevel=1.2 -dMaxSubsetPct=100 -dNOPAUSE -dBATCH \
		-sDEVICE=pdfwrite -sPAPERSIZE=letter -sOutputFile=$@ \
		-c save pop -f $< -sFONTPATH=.

$(IMAGE_DIR)/%.eps: %.eps
	cp -f $< $@

$(HOME)/%.pdf : %.pdf
	cp -f $< $@

$(HOME)/%.html : %.html
	cp -f $< $@
	-mkdir $(HOME)/$(IMAGE_DIR)
	cp -f $(IMAGE_DIR)/*.png $(HOME)/$(IMAGE_DIR)

# Specific rules (DVI)

$(CMmaindvi) : $(CMbitmapepsfiles) $(CMepsfiles) $(CMmainsource) $(CMsecondaryfiles)
	-latex -interaction=batchmode $(CMmainsource)
	-latex -interaction=batchmode $(CMmainsource)
	-latex -interaction=batchmode $(CMmainsource)
	makeindex $(CMmainidx)
	latex -interaction=batchmode $(CMmainsource)

$(IMmaindvi) : $(IMbitmapepsfiles) $(IMepsfiles) $(IMmainsource) $(IMsecondaryfiles)
	-latex -interaction=batchmode $(IMmainsource)
	-latex -interaction=batchmode $(IMmainsource)
	-latex -interaction=batchmode $(IMmainsource)
	makeindex $(IMmainidx)
	latex -interaction=batchmode $(IMmainsource)

$(JSmaindvi) : $(JSbitmapepsfiles) $(JSepsfiles) $(JSmainsource) $(JSsecondaryfiles)
	-latex -interaction=batchmode $(JSmainsource)
	-latex -interaction=batchmode $(JSmainsource)
	-latex -interaction=batchmode $(JSmainsource)
	makeindex $(JSmainidx)
	latex -interaction=batchmode $(JSmainsource)

$(KV2maindvi) : $(KV2bitmapepsfiles) $(KV2epsfiles) $(KV2mainsource) $(KV2secondaryfiles)
	-latex -interaction=batchmode $(KV2mainsource)
	-latex -interaction=batchmode $(KV2mainsource)
	-latex -interaction=batchmode $(KV2mainsource)
	makeindex $(KV2mainidx)
	latex -interaction=batchmode $(KV2mainsource)

$(LMmaindvi) : $(LMbitmapepsfiles) $(LMepsfiles) $(LMmainsource) $(LMsecondaryfiles)
	-latex -interaction=batchmode $(LMmainsource)
	-latex -interaction=batchmode $(LMmainsource)
	-latex -interaction=batchmode $(LMmainsource)
	makeindex $(LMmainidx)
	latex -interaction=batchmode $(LMmainsource)

$(MMmaindvi) : $(MMbitmapepsfiles) $(MMepsfiles) $(MMmainsource) $(MMsecondaryfiles)
	-latex -interaction=batchmode $(MMmainsource)
	-latex -interaction=batchmode $(MMmainsource)
	-latex -interaction=batchmode $(MMmainsource)
	makeindex $(MMmainidx)
	latex -interaction=batchmode $(MMmainsource)

$(VDSmaindvi) : $(VDSbitmapepsfiles) $(VDSepsfiles) $(VDSmainsource) $(VDSsecondaryfiles)
	-latex -interaction=batchmode $(VDSmainsource)
	-latex -interaction=batchmode $(VDSmainsource)
	-latex -interaction=batchmode $(VDSmainsource)
	makeindex $(VDSmainidx)
	latex -interaction=batchmode $(VDSmainsource)

# Specific rules (HTML)

$(CMmainhtml) : $(CMbitmappngfiles) $(CMpngfiles) $(CMmainsource) $(CMsecondaryfiles)
	-hevea -O -fix -exec xxdate.exe $(CMmainsource) -o $@

$(IMmainhtml) : $(IMbitmappngfiles) $(IMpngfiles) $(IMmainsource) $(IMsecondaryfiles)
	-hevea -O -fix -exec xxdate.exe $(IMmainsource) -o $@

$(JSmainhtml) : $(JSbitmappngfiles) $(JSpngfiles) $(JSmainsource) $(JSsecondaryfiles)
	-hevea -O -fix -exec xxdate.exe $(JSmainsource) -o $@

$(KV2mainhtml) : $(KV2bitmappngfiles) $(KV2pngfiles) $(KV2mainsource) $(KV2secondaryfiles)
	-hevea -O -fix -exec xxdate.exe $(KV2mainsource) -o $@

$(LMmainhtml) : $(LMbitmappngfiles) $(LMpngfiles) $(LMmainsource) $(LMsecondaryfiles)
	-hevea -O -fix -exec xxdate.exe $(LMmainsource) -o $@

$(MMmainhtml) : $(MMbitmappngfiles) $(MMpngfiles) $(MMmainsource) $(MMsecondaryfiles)
	-hevea -O -fix -exec xxdate.exe $(MMmainsource) -o $@

$(VDSmainhtml) : $(VDSbitmappngfiles) $(VDSpngfiles) $(VDSmainsource) $(VDSsecondaryfiles)
	-hevea -O -fix -exec xxdate.exe $(VDSmainsource) -o $@

# Top-level targets

.PHONY : clean all

.PRECIOUS : %.eps %.ps %.pdf

clean :
	-rm -f *.log *.pdf *.mpx *.dvi *.aux *.idx *.ilg *.ind *.lof *.out *.png *.ps *.eps
	-rm -f *.html *.toc *.htoc *.haux *.hind $(IMAGE_DIR)/*.png

all : $(CMtarget) $(IMtarget) $(JStarget) $(KV2target) $(LMtarget) $(MMtarget) \
		$(VDStarget)
