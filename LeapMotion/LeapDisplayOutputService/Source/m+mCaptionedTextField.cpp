//--------------------------------------------------------------------------------------------------
//
//  File:       m+mCaptionedTextField.cpp
//
//  Project:    m+m
//
//  Contains:   The class declaration for a field consisting of a text editor paired with a caption.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2015 by H Plus Technologies Ltd. and Simon Fraser University.
//
//              All rights reserved. Redistribution and use in source and binary forms, with or
//              without modification, are permitted provided that the following conditions are met:
//                * Redistributions of source code must retain the above copyright notice, this list
//                  of conditions and the following disclaimer.
//                * Redistributions in binary form must reproduce the above copyright notice, this
//                  list of conditions and the following disclaimer in the documentation and/or
//                  other materials provided with the distribution.
//                * Neither the name of the copyright holders nor the names of its contributors may
//                  be used to endorse or promote products derived from this software without
//                  specific prior written permission.
//
//              THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY
//              EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
//              OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT
//              SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
//              INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
//              TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
//              BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
//              CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
//              ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
//              DAMAGE.
//
//  Created:    2015-08-31
//
//--------------------------------------------------------------------------------------------------

#include "m+mCaptionedTextField.hpp"

#include "m+mFormFieldErrorResponder.hpp"
#include "m+mManagerApplication.hpp"
#include "m+mTextValidator.hpp"
#include "m+mValidatingTextEditor.hpp"

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)

#if (! MAC_OR_LINUX_)
# include <Windows.h>
#endif //! MAC_OR_LINUX_

/*! @file

 @brief The class declaration for a field consisting of a text editor paired with a caption. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MPlusM_Manager;
using namespace std;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

/*! @brief The amount to add to the height of text editor fields. */
static const int kEditorHeightAdjustment = 4;

/*! @brief The amount of extra space between a field and its label. */
static const int kLabelToFieldGap = 2;

/*! @brief The text of the file popup invocation button. */
static const String kFileButtonText("...");

/*! @brief The width of a 'file' button. */
static int lFileButtonWidth = -1;

#if defined(__APPLE__)
# pragma mark Global constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and Destructors
#endif // defined(__APPLE__)

CaptionedTextField::CaptionedTextField(FormFieldErrorResponder & responder,
                                       Font &                    regularLabelFont,
                                       Font &                    errorLabelFont,
                                       const size_t              index,
                                       const String &            captionTitle,
                                       const int                 top,
                                       const bool                boundsSetLater,
                                       const bool                forFilePath,
                                       ButtonListener *          buttonHandler,
                                       TextValidator *           validator,
                                       const String &            componentName,
                                       juce_wchar                passwordCharacter) :
    inherited(regularLabelFont, index), _textEditor(new ValidatingTextEditor(*this, validator,
                                                                             componentName,
                                                                             passwordCharacter)),
    _validator(validator), _caption(new Label("", captionTitle)), _button(NULL),
    _errorFont(errorLabelFont), _responder(responder)
{
    ODL_ENTER(); //####
    ODL_P4("responder = ", &responder, "regularLabelFont = ", &regularLabelFont, //####
           "errorLabelFont = ", &errorLabelFont, "buttonHandler = ", buttonHandler); //####
    ODL_P1("validator = ", validator); //####
    ODL_S2s("captionTitle = ", captionTitle, "componentName = ", //####
            componentName.toStdString()); //####
    ODL_LL3("index = ", index, "top = ", top, "passwordCharacter = ", passwordCharacter); //####
    ODL_B2("boundsSetLater = ", boundsSetLater, "forFilePath = ", forFilePath); //####
    Point<int> dimensions;
    int        adjustedEditorHeight = static_cast<int>(_regularFont.getHeight() +
                                                       kEditorHeightAdjustment);

    _caption->setFont(_regularFont);
    _textEditor->setFont(_regularFont);
    _textEditor->setEscapeAndReturnKeysConsumed(false);
    _textEditor->setSelectAllWhenFocused(true);
    MPlusM_Manager::CalculateTextArea(dimensions, _regularFont, _caption->getText());
    _caption->setBounds(kLabelInset, top, dimensions.getX() + kLabelInset, dimensions.getY());
    if (forFilePath)
    {
        _button = new TextButton(kFileButtonText);
        _button->setWantsKeyboardFocus(true);
        _button->setMouseClickGrabsKeyboardFocus(false);
        _button->setCommandToTrigger(NULL, kConfigurationFileRequest, false);
        _button->addListener(buttonHandler);
        _button->changeWidthToFitText(ManagerApplication::getButtonHeight());
    }
    if (boundsSetLater)
    {
        _textEditor->setBounds(kFieldInset, 0, 0, adjustedEditorHeight);
    }
    else
    {
        _textEditor->setBounds(kFieldInset, _caption->getY() + _caption->getHeight() +
                               kLabelToFieldGap, _caption->getX() + _caption->getWidth() -
                               kFieldInset, adjustedEditorHeight);
    }
    ODL_EXIT_P(this); //####
} // CaptionedTextField::CaptionedTextField

CaptionedTextField::~CaptionedTextField(void)
{
    ODL_OBJENTER(); //####
    _button = NULL;
    _caption = NULL;
    _textEditor = NULL;
    ODL_OBJEXIT(); //####
} // CaptionedTextField::~CaptionedTextField

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

void
CaptionedTextField::addToComponent(Component * whereToAdd)
{
    ODL_OBJENTER(); //####
    ODL_P1("whereToAdd = ", whereToAdd); //####
    if (whereToAdd)
    {
        whereToAdd->addAndMakeVisible(_caption);
        whereToAdd->addAndMakeVisible(_textEditor);
        if (_button)
        {
            whereToAdd->addAndMakeVisible(_button);
        }
    }
    ODL_OBJEXIT(); //####
} // CaptionedTextField::addToComponent

int
CaptionedTextField::getFileButtonWidth(void)
{
    ODL_ENTER(); //####
    if (lFileButtonWidth < 0)
    {
        ScopedPointer<TextButton> fileButton(new TextButton(kFileButtonText));

        fileButton->changeWidthToFitText(ManagerApplication::getButtonHeight());
        lFileButtonWidth = fileButton->getWidth();
    }
    ODL_EXIT_LL(lFileButtonWidth); //####
    return lFileButtonWidth;
} // CaptionedTextField::getFileButtonWidth

int
CaptionedTextField::getHeight(void)
const
{
    ODL_OBJENTER(); //####
    int result = _textEditor->getHeight() + _caption->getHeight() + kLabelToFieldGap;

    ODL_OBJEXIT_LL(result); //####
    return result;
} // CaptionedTextField::getHeight

int
CaptionedTextField::getMinimumWidth(void)
const
{
    ODL_OBJENTER(); //####
    int result = jmax(_textEditor->getX(), _caption->getWidth() + _caption->getX());

    ODL_OBJEXIT_LL(result); //####
    return result;
} // CaptionedTextField::::getMinimumWidth

const String &
CaptionedTextField::getName(void)
const
{
    ODL_OBJENTER(); //####
    const String & theName = _textEditor->getName();

    ODL_OBJEXIT_s(theName.toStdString()); //####
    return theName;
} // CaptionedTextField::getName

String
CaptionedTextField::getText(void)
const
{
    ODL_OBJENTER(); //####
    String result(_textEditor->getText());

    ODL_OBJEXIT_s(result.toStdString()); //####
    return result;
} // CaptionedTextField::getText

int
CaptionedTextField::getWidth(void)
const
{
    ODL_OBJENTER(); //####
    int firstVal = _textEditor->getWidth() + _textEditor->getX();
    int secondVal = _caption->getWidth() + _caption->getX();
    int result = jmax(firstVal, secondVal) - getX();

    ODL_OBJEXIT_LL(result); //####
    return result;
} // CaptionedTextField::getWidth

int
CaptionedTextField::getX(void)
const
{
    ODL_OBJENTER(); //####
    int result = jmin(_textEditor->getX(), _caption->getX());

    ODL_OBJEXIT_LL(result); //####
    return result;
} // CaptionedTextField::getX

int
CaptionedTextField::getY(void)
const
{
    ODL_OBJENTER(); //####
    int result = _caption->getY();

    ODL_OBJEXIT_LL(result); //####
    return result;
} // CaptionedTextField::getY

void
CaptionedTextField::ignoreNextFocusLoss(void)
{
    ODL_OBJENTER(); //####
    _textEditor->ignoreNextFocusLoss();
    ODL_OBJEXIT(); //####
} // CaptionedTextField::ignoreNextFocusLoss

void
CaptionedTextField::markAsInvalid(void)
{
    ODL_OBJENTER(); //####
    _caption->setFont(_errorFont);
    ODL_OBJEXIT(); //####
} // CaptionedTextField::markAsInvalid

void
CaptionedTextField::markAsValid(void)
{
    ODL_OBJENTER(); //####
    _caption->setFont(_regularFont);
    ODL_OBJEXIT(); //####
} // CaptionedTextField::markAsValid

void
CaptionedTextField::performButtonAction(void)
{
    ODL_OBJENTER(); //####
    if (_validator)
    {
        bool forOutput;

        if (_validator->isForFiles(forOutput))
        {
            ignoreNextFocusLoss();
            if (forOutput)
            {
                FileChooser fc("Choose a file to write to...", File::getCurrentWorkingDirectory(),
                               "*", true);

                if (fc.browseForFileToSave(true))
                {
                    File   chosenFile = fc.getResult();
                    String filePath = chosenFile.getFullPathName();

                    _textEditor->setText(chosenFile.getFullPathName());
                    if (! validateField())
                    {
                        _responder.reportErrorInField(*this);
                    }
                }
            }
            else
            {
                FileChooser fc("Choose a file to open...", File::getCurrentWorkingDirectory(), "*",
                               true);

                if (fc.browseForFileToOpen())
                {
                    File   chosenFile = fc.getResult();
                    String filePath = chosenFile.getFullPathName();

                    _textEditor->setText(chosenFile.getFullPathName());
                    if (! validateField())
                    {
                        _responder.reportErrorInField(*this);
                    }
                }
            }
        }
    }
    ODL_OBJEXIT(); //####
} // CaptionedTextField::performButtonAction

void
CaptionedTextField::removeFromComponent(Component * whereToRemove)
{
    ODL_OBJENTER(); //####
    ODL_P1("whereToRemove = ", whereToRemove); //####
    if (whereToRemove)
    {
        whereToRemove->removeChildComponent(_caption);
        whereToRemove->removeChildComponent(_textEditor);
        if (_button)
        {
            whereToRemove->removeChildComponent(_button);
        }
    }
    ODL_OBJEXIT(); //####
} // CaptionedTextField::removeFromComponent

void
CaptionedTextField::reportErrorInField(void)
{
    ODL_OBJENTER(); //####
    _responder.reportErrorInField(*this);
    ODL_OBJEXIT(); //####
} // CaptionedTextField::reportErrorInField

void
CaptionedTextField::setButton(TextButton * newButton)
{
    ODL_OBJENTER(); //####
    ODL_P1("newButton = ", newButton); //####
    _button = newButton;
    ODL_OBJEXIT(); //####
} // CaptionedTextField::setButton

void
CaptionedTextField::setText(const String & newText)
{
    ODL_OBJENTER(); //####
    ODL_S1s("newText = ", newText.toStdString()); //####
    _textEditor->setText(newText, false);
    ODL_OBJEXIT(); //####
} // CaptionedTextField::setText

void
CaptionedTextField::setWidth(const int ww)
{
    ODL_OBJENTER(); //####
    ODL_LL1("ww = ", ww); //####
    _textEditor->setSize(ww, _textEditor->getHeight());
    if (_button)
    {
        int span = _textEditor->getY() + _textEditor->getHeight() - _caption->getY();
        int offset = span - _button->getHeight();

        _button->setTopLeftPosition(_textEditor->getX() + ww + kButtonGap,
                                    _caption->getY() + (offset / 2));
    }
    ODL_OBJEXIT(); //####
} // CaptionedTextField::setWidth

void
CaptionedTextField::setY(const int yy)
{
    ODL_OBJENTER(); //####
    ODL_LL1("yy = ", yy); //####
    _caption->setTopLeftPosition(kLabelInset, yy);
    _textEditor->setTopLeftPosition(kFieldInset,
                                    _caption->getY() + _caption->getHeight() + kLabelToFieldGap);
    if (_button)
    {
        int span = _textEditor->getY() + _textEditor->getHeight() - _caption->getY();
        int offset = span - _button->getHeight();

        _button->setTopLeftPosition(_textEditor->getX() + _textEditor->getWidth() + kButtonGap,
                                    _caption->getY() + (offset / 2));
    }
    ODL_OBJEXIT(); //####
} // CaptionedTextField::setY

bool
CaptionedTextField::validateField(void)
{
    ODL_OBJENTER(); //####
    bool result = _textEditor->validateField();

    if (result)
    {
        markAsValid();
    }
    else
    {
        markAsInvalid();
    }
    ODL_OBJEXIT_B(result); //####
    return result;
} // CaptionedTextField::validateField

bool
CaptionedTextField::validateField(StringArray & argsToUse)
{
    ODL_OBJENTER(); //####
    ODL_P1("argsToUse = ", &argsToUse); //####
    bool result = _textEditor->validateField(argsToUse);

    if (result)
    {
        markAsValid();
    }
    else
    {
        markAsInvalid();
    }
    ODL_OBJEXIT_B(result); //####
    return result;
} // CaptionedTextField::validateField

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
