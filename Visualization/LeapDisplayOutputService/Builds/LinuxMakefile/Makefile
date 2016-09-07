# Automatically generated makefile, created by the Introjucer
# Don't edit this file! Your changes will be overwritten when you re-save the Introjucer project!

# (this disables dependency generation if multiple architectures are set)
DEPFLAGS := $(if $(word 2, $(TARGET_ARCH)), , -MMD)
JUCELIB := ../../../JuceLibraryCode
COMMONVIS := ../../../CommonVisuals
SRCDIR := ../../Source

ifndef CONFIG
  CONFIG=Debug
endif

ifeq ($(CONFIG),Debug)
  BINDIR := build
  LIBDIR := build
  OBJDIR := build/intermediate/Debug
  OUTDIR := build

  ifeq ($(TARGET_ARCH),)
    TARGET_ARCH := -march=native
  endif

  CPPFLAGS := $(DEPFLAGS) -D "LINUX=1" -D "DEBUG=1" -D "_DEBUG=1" -D "JUCER_LINUX_MAKE_6D53C8B4=1" -D "JUCE_APP_VERSION=1.4.25" -D "JUCE_APP_VERSION_HEX=0x10419" -I /usr/include -I /usr/include/freetype2 -I $(JUCELIB) -I $(JUCELIB)/modules -I $(COMMONVIS) -I /opt/m+m/include
  CFLAGS += $(CPPFLAGS) $(TARGET_ARCH) -g -ggdb -O0
  CXXFLAGS += $(CFLAGS) -std=c++11
  LDFLAGS += $(TARGET_ARCH) -L$(BINDIR) -L$(LIBDIR) -L/usr/X11R6/lib -L/opt/m+m/lib -lX11 -lXext -lXinerama -ldl -lfreetype -lpthread -lrt -lYARP_OS -lYARP_init -lm+mCommon -lodLogging -lACE -ldns_sd -lGL
  TARGET := m+mLeapDisplayOutputService
  BLDCMD = $(CXX) -o $(OUTDIR)/$(TARGET) $(OBJECTS) $(LDFLAGS) $(RESOURCES) $(TARGET_ARCH)
  CLEANCMD = rm -rf $(OUTDIR)/$(TARGET) $(OBJDIR)
endif

ifeq ($(CONFIG),Release)
  INDIR := build
  LIBDIR := build
  OBJDIR := build/intermediate/Release
  OUTDIR := build

  ifeq ($(TARGET_ARCH),)
    TARGET_ARCH := -march=native
  endif

  CPPFLAGS := $(DEPFLAGS) -D "LINUX=1" -D "NDEBUG=1" -D "JUCER_LINUX_MAKE_6D53C8B4=1" -D "JUCE_APP_VERSION=1.4.25" -D "JUCE_APP_VERSION_HEX=0x10419" -I /usr/include -I /usr/include/freetype2 -I $(JUCELIB) -I $(JUCELIB)/modules -I $(COMMONVIS) -I /opt/m+m/include
  CFLAGS += $(CPPFLAGS) $(TARGET_ARCH) -O3
  CXXFLAGS += $(CFLAGS) -std=c++11
  LDFLAGS += $(TARGET_ARCH) -L$(BINDIR) -L$(LIBDIR) -fvisibility=hidden -L/usr/X11R6/lib -L/opt/m+m/lib -lX11 -lXext -lXinerama -ldl -lfreetype -lpthread -lrt -lYARP_OS -lYARP_init -lm+mCommon -lodLogging -lACE -ldns_sd -lGL
  TARGET := m+mLeapDisplayOutputService
  BLDCMD = $(CXX) -o $(OUTDIR)/$(TARGET) $(OBJECTS) $(LDFLAGS) $(RESOURCES) $(TARGET_ARCH)
  CLEANCMD = rm -rf $(OUTDIR)/$(TARGET) $(OBJDIR)
endif

OBJECTS := \
  $(OBJDIR)/m+mLeapDisplayApplication_abb6df13.o \
  $(OBJDIR)/m+mLeapDisplayContentPanel_19fbccfb.o \
  $(OBJDIR)/m+mLeapDisplayGraphicsFrame_b0a65b8b.o \
  $(OBJDIR)/m+mLeapDisplayGraphicsPanel_76190d3b.o \
  $(OBJDIR)/m+mLeapDisplayInputHandler_377125ea.o \
  $(OBJDIR)/m+mLeapDisplayMain_f38808a0.o \
  $(OBJDIR)/m+mLeapDisplayOutputService_24261102.o \
  $(OBJDIR)/m+mLeapDisplayWindow_c4f886d7.o \
  $(OBJDIR)/m+mLeapServiceThread_6460918c.o \
  $(OBJDIR)/m+mCaptionedTestField_12345678.o \
  $(OBJDIR)/m+mCheckboxField_23456789.o \
  $(OBJDIR)/m+mCommonVisuals_34567890.o \
  $(OBJDIR)/m+mFormField_45678901.o \
  $(OBJDIR)/m+mFormFieldErrorResponder_56789012.o \
  $(OBJDIR)/m+mSettingsWindow_67890123.o \
  $(OBJDIR)/m+mTextValidator_78901234.o \
  $(OBJDIR)/m+mValidatingTextEditor_89012345.o \
  $(OBJDIR)/m+mVertexBuffer_90123456.o \
  $(OBJDIR)/juce_box2d_ce4232d4.o \
  $(OBJDIR)/juce_core_aff681cc.o \
  $(OBJDIR)/juce_data_structures_bdd6d488.o \
  $(OBJDIR)/juce_events_79b2840.o \
  $(OBJDIR)/juce_graphics_c8f1e7a4.o \
  $(OBJDIR)/juce_gui_basics_a630dd20.o \
  $(OBJDIR)/juce_gui_extra_7767d6a8.o \
  $(OBJDIR)/juce_opengl_8899aabb.o 

.PHONY: clean

$(OUTDIR)/$(TARGET): $(OBJECTS) $(RESOURCES)
	@echo Linking m+mLeapDisplayOutputService
	-@mkdir -p $(BINDIR)
	-@mkdir -p $(LIBDIR)
	-@mkdir -p $(OUTDIR)
	@$(BLDCMD)

clean:
	@echo Cleaning m+mLeapDisplayOutputService
	@$(CLEANCMD)

strip:
	@echo Stripping m+mLeapDisplayOutputService
	-@strip --strip-unneeded $(OUTDIR)/$(TARGET)

$(OBJDIR)/m+mLeapDisplayApplication_abb6df13.o: $(SRCDIR)/m+mLeapDisplayApplication.cpp
	-@mkdir -p $(OBJDIR)
	@echo "Compiling m+mLeapDisplayApplication.cpp"
	@$(CXX) $(CXXFLAGS) -o "$@" -c "$<"

$(OBJDIR)/m+mLeapDisplayContentPanel_19fbccfb.o: $(SRCDIR)/m+mLeapDisplayContentPanel.cpp
	-@mkdir -p $(OBJDIR)
	@echo "Compiling m+mLeapDisplayContentPanel.cpp"
	@$(CXX) $(CXXFLAGS) -o "$@" -c "$<"

$(OBJDIR)/m+mLeapDisplayGraphicsFrame_b0a65b8b.o: $(SRCDIR)/m+mLeapDisplayGraphicsFrame.cpp
	-@mkdir -p $(OBJDIR)
	@echo "Compiling m+mLeapDisplayGraphicsFrame.cpp"
	@$(CXX) $(CXXFLAGS) -o "$@" -c "$<"

$(OBJDIR)/m+mLeapDisplayGraphicsPanel_76190d3b.o: $(SRCDIR)/m+mLeapDisplayGraphicsPanel.cpp
	-@mkdir -p $(OBJDIR)
	@echo "Compiling m+mLeapDisplayGraphicsPanel.cpp"
	@$(CXX) $(CXXFLAGS) -o "$@" -c "$<"

$(OBJDIR)/m+mLeapDisplayInputHandler_377125ea.o: $(SRCDIR)/m+mLeapDisplayInputHandler.cpp
	-@mkdir -p $(OBJDIR)
	@echo "Compiling m+mLeapDisplayInputHandler.cpp"
	@$(CXX) $(CXXFLAGS) -o "$@" -c "$<"

$(OBJDIR)/m+mLeapDisplayMain_f38808a0.o: $(SRCDIR)/m+mLeapDisplayMain.cpp
	-@mkdir -p $(OBJDIR)
	@echo "Compiling m+mLeapDisplayMain.cpp"
	@$(CXX) $(CXXFLAGS) -o "$@" -c "$<"

$(OBJDIR)/m+mLeapDisplayOutputService_24261102.o: $(SRCDIR)/m+mLeapDisplayOutputService.cpp
	-@mkdir -p $(OBJDIR)
	@echo "Compiling m+mLeapDisplayOutputService.cpp"
	@$(CXX) $(CXXFLAGS) -o "$@" -c "$<"

$(OBJDIR)/m+mLeapDisplayWindow_c4f886d7.o: $(SRCDIR)/m+mLeapDisplayWindow.cpp
	-@mkdir -p $(OBJDIR)
	@echo "Compiling m+mLeapDisplayWindow.cpp"
	@$(CXX) $(CXXFLAGS) -o "$@" -c "$<"

$(OBJDIR)/m+mLeapServiceThread_6460918c.o: $(SRCDIR)/m+mLeapServiceThread.cpp
	-@mkdir -p $(OBJDIR)
	@echo "Compiling m+mLeapServiceThread.cpp"
	@$(CXX) $(CXXFLAGS) -o "$@" -c "$<"

$(OBJDIR)/m+mCaptionedTestField_12345678.o: $(COMMONVIS)/m+mCaptionedTextField.cpp
	-@mkdir -p $(OBJDIR)
	@echo "Compiling m+mCaptionedTextField.cpp"
	@$(CXX) $(CXXFLAGS) -o "$@" -c "$<"

$(OBJDIR)/m+mCheckboxField_23456789.o: $(COMMONVIS)/m+mCheckboxField.cpp
	-@mkdir -p $(OBJDIR)
	@echo "Compiling m+mCheckboxField.cpp"
	@$(CXX) $(CXXFLAGS) -o "$@" -c "$<"

$(OBJDIR)/m+mCommonVisuals_34567890.o: $(COMMONVIS)/m+mCommonVisuals.cpp
	-@mkdir -p $(OBJDIR)
	@echo "Compiling m+mCommonVisuals.cpp"
	@$(CXX) $(CXXFLAGS) -o "$@" -c "$<"

$(OBJDIR)/m+mFormField_45678901.o: $(COMMONVIS)/m+mFormField.cpp
	-@mkdir -p $(OBJDIR)
	@echo "Compiling m+mFormField.cpp"
	@$(CXX) $(CXXFLAGS) -o "$@" -c "$<"

$(OBJDIR)/m+mFormFieldErrorResponder_56789012.o: $(COMMONVIS)/m+mFormFieldErrorResponder.cpp
	-@mkdir -p $(OBJDIR)
	@echo "Compiling m+mFormFieldErrorResponder.cpp"
	@$(CXX) $(CXXFLAGS) -o "$@" -c "$<"

$(OBJDIR)/m+mSettingsWindow_67890123.o: $(COMMONVIS)/m+mSettingsWindow.cpp
	-@mkdir -p $(OBJDIR)
	@echo "Compiling m+mSettingsWindow.cpp"
	@$(CXX) $(CXXFLAGS) -o "$@" -c "$<"

$(OBJDIR)/m+mTextValidator_78901234.o: $(COMMONVIS)/m+mTextValidator.cpp
	-@mkdir -p $(OBJDIR)
	@echo "Compiling m+mTextValidator.cpp"
	@$(CXX) $(CXXFLAGS) -o "$@" -c "$<"

$(OBJDIR)/m+mValidatingTextEditor_89012345.o: $(COMMONVIS)/m+mValidatingTextEditor.cpp
	-@mkdir -p $(OBJDIR)
	@echo "Compiling m+mValidatingTextEditor.cpp"
	@$(CXX) $(CXXFLAGS) -o "$@" -c "$<"

$(OBJDIR)/m+mVertexBuffer_90123456.o: $(COMMONVIS)/m+mVertexBuffer.cpp
	-@mkdir -p $(OBJDIR)
	@echo "Compiling m+mVertexBuffer.cpp"
	@$(CXX) $(CXXFLAGS) -o "$@" -c "$<"

$(OBJDIR)/juce_box2d_ce4232d4.o: $(JUCELIB)/modules/juce_box2d/juce_box2d.cpp
	-@mkdir -p $(OBJDIR)
	@echo "Compiling juce_box2d.cpp"
	@$(CXX) $(CXXFLAGS) -o "$@" -c "$<"

$(OBJDIR)/juce_core_aff681cc.o: $(JUCELIB)/modules/juce_core/juce_core.cpp
	-@mkdir -p $(OBJDIR)
	@echo "Compiling juce_core.cpp"
	@$(CXX) $(CXXFLAGS) -o "$@" -c "$<"

$(OBJDIR)/juce_data_structures_bdd6d488.o: $(JUCELIB)/modules/juce_data_structures/juce_data_structures.cpp
	-@mkdir -p $(OBJDIR)
	@echo "Compiling juce_data_structures.cpp"
	@$(CXX) $(CXXFLAGS) -o "$@" -c "$<"

$(OBJDIR)/juce_events_79b2840.o: $(JUCELIB)/modules/juce_events/juce_events.cpp
	-@mkdir -p $(OBJDIR)
	@echo "Compiling juce_events.cpp"
	@$(CXX) $(CXXFLAGS) -o "$@" -c "$<"

$(OBJDIR)/juce_graphics_c8f1e7a4.o: $(JUCELIB)/modules/juce_graphics/juce_graphics.cpp
	-@mkdir -p $(OBJDIR)
	@echo "Compiling juce_graphics.cpp"
	@$(CXX) $(CXXFLAGS) -o "$@" -c "$<"

$(OBJDIR)/juce_gui_basics_a630dd20.o: $(JUCELIB)/modules/juce_gui_basics/juce_gui_basics.cpp
	-@mkdir -p $(OBJDIR)
	@echo "Compiling juce_gui_basics.cpp"
	@$(CXX) $(CXXFLAGS) -o "$@" -c "$<"

$(OBJDIR)/juce_gui_extra_7767d6a8.o: $(JUCELIB)/modules/juce_gui_extra/juce_gui_extra.cpp
	-@mkdir -p $(OBJDIR)
	@echo "Compiling juce_gui_extra.cpp"
	@$(CXX) $(CXXFLAGS) -o "$@" -c "$<"

$(OBJDIR)/juce_opengl_8899aabb.o: $(JUCELIB)/modules/juce_opengl/juce_opengl.cpp
	-@mkdir -p $(OBJDIR)
	@echo "Compiling juce_opengl.cpp"
	@$(CXX) $(CXXFLAGS) -o "$@" -c "$<"

-include $(OBJECTS:%.o=%.d)
