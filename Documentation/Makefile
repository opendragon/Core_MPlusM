# File: Makefile

KV2psfiles =

LMpsfiles =

MMpsfiles = MMdbDiagram.ps MMmatchSyntaxrails.ps MMcmDiagram.ps MMmpmDiagram.ps

KV2imagefiles =

LMimagefiles =

MMimagefiles = ChannelManager.eps

KV2mainsource = KinectV2Input_Manual.tex

LMmainsource = LeapMotionInput_Manual.tex

MMmainsource = MpM_Manual.tex

KV2secondarysources = KV2documentationHistory.tex KV2frontispiece.tex \
	KV2messageFormat.tex KV2overview.tex KV2service.tex sharedSetup.tex

LMsecondarysources = LMdocumentationHistory.tex LMfrontispiece.tex LMmessageFormat.tex \
	LMoverview.tex LMservice.tex sharedSetup.tex

MMsecondarysources = MMcaseStudy.tex MMclasses.tex MMdocumentationHistory.tex \
	MMexamples.tex MMexemplars.tex MMfrontispiece.tex MMinternalDb.tex \
	MMmatchSyntax.tex MMoverview.tex MMregistrationSequence.tex MMservices.tex \
	MMutilities.tex sharedSetup.tex

KV2maindvi = $(KV2mainsource:.tex=.dvi)

LMmaindvi = $(LMmainsource:.tex=.dvi)

MMmaindvi = $(MMmainsource:.tex=.dvi)

KV2mainidx = $(KV2mainsource:.tex=.idx)

LMmainidx = $(LMmainsource:.tex=.idx)

MMmainidx = $(MMmainsource:.tex=.idx)

KV2mainps = $(KV2mainsource:.tex=.ps)

LMmainps = $(LMmainsource:.tex=.ps)

MMmainps = $(MMmainsource:.tex=.ps)

KV2mainpdf = $(KV2mainsource:.tex=.pdf)

LMmainpdf = $(LMmainsource:.tex=.pdf)

MMmainpdf = $(MMmainsource:.tex=.pdf)

KV2target = $(HOME)/$(KV2mainpdf)

LMtarget = $(HOME)/$(LMmainpdf)

MMtarget = $(HOME)/$(MMmainpdf)

%.ps: %.mp
	mpost -interaction=batchmode $<

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

$(KV2mainps) : $(KV2maindvi)
	dvips $(KV2maindvi) -o $(KV2mainps)

$(LMmainps) : $(LMmaindvi)
	dvips $(LMmaindvi) -o $(LMmainps)

$(MMmainps) : $(MMmaindvi)
	dvips $(MMmaindvi) -o $(MMmainps)

$(KV2mainpdf) : $(KV2mainps)
	gs -dCompatibilityLevel=1.2 -dMaxSubsetPct=100 -dNOPAUSE -dBATCH \
		-sDEVICE=pdfwrite -sPAPERSIZE=letter -sOutputFile="$(KV2mainpdf)" \
		-c save pop -f "$(KV2mainps)" -sFONTPATH=.
		
$(LMmainpdf) : $(LMmainps)
	gs -dCompatibilityLevel=1.2 -dMaxSubsetPct=100 -dNOPAUSE -dBATCH \
		-sDEVICE=pdfwrite -sPAPERSIZE=letter -sOutputFile="$(LMmainpdf)" \
		-c save pop -f "$(LMmainps)" -sFONTPATH=.

$(MMmainpdf) : $(MMmainps)
	gs -dCompatibilityLevel=1.2 -dMaxSubsetPct=100 -dNOPAUSE -dBATCH \
		-sDEVICE=pdfwrite -sPAPERSIZE=letter -sOutputFile="$(MMmainpdf)" \
		-c save pop -f "$(MMmainps)" -sFONTPATH=.

$(KV2target) : $(KV2mainpdf)
	cp $(KV2mainpdf) $(KV2target)

$(LMtarget) : $(LMmainpdf)
	cp $(LMmainpdf) $(LMtarget)

$(MMtarget) : $(MMmainpdf)
	cp $(MMmainpdf) $(MMtarget)

.PHONY : clean manual

clean :
	rm $(KV2psfiles) $(LMpsfiles) $(MMpsfiles) *.log *.pdf *.mpx *.dvi

manual : $(KV2target) $(LMtarget) $(MMtarget)
