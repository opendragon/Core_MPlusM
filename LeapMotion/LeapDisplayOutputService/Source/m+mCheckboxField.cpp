//--------------------------------------------------------------------------------------------------
//
//  File:       m+mCheckboxField.cpp
//
//  Project:    m+m
//
//  Contains:   The class declaration for a field consisting of a checkbox paired with a caption.
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
//  Created:    2015-09-02
//
//--------------------------------------------------------------------------------------------------

#include "m+mCheckboxField.hpp"

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

 @brief The class declaration for a field consisting of a checkbox paired with a caption. */
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

/*! @brief The amount to add to the height of checkbox fields. */
static const int kCheckboxHeightAdjustment = 8;

/*! @brief The amount of extra space between a field and its label. */
static const int kCheckboxToLabelGap = 0;

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

CheckboxField::CheckboxField(Font &         regularLabelFont,
                             const size_t   index,
                             const String & captionTitle,
                             const int      top,
                             const String & componentName) :
    inherited(regularLabelFont, index), _checkbox(new ToggleButton("")),
    _caption(new Label(componentName, captionTitle))
{
    ODL_ENTER(); //####
    ODL_P1("regularLabelFont = ", &regularLabelFont); //####
    ODL_S2s("captionTitle = ", captionTitle.toStdString(), "componentName = ", //####
            componentName.toStdString()); //####
    ODL_LL2("index = ", index, "top = ", top); //####
    Point<int> dimensions;
    int        adjustedEditorHeight = static_cast<int>(_regularFont.getHeight() +
                                                       kCheckboxHeightAdjustment);

    _checkbox->setBounds(kFieldInset, top, adjustedEditorHeight, adjustedEditorHeight);
    MPlusM_Manager::CalculateTextArea(dimensions, _regularFont, captionTitle);
    _caption->setBounds(_checkbox->getX() + _checkbox->getWidth() + kCheckboxToLabelGap,
                        _checkbox->getY(), dimensions.getX(), adjustedEditorHeight);
    _caption->setFont(_regularFont);
    ODL_EXIT_P(this); //####
} // CheckboxField::CheckboxField

CheckboxField::~CheckboxField(void)
{
    ODL_OBJENTER(); //####
    _checkbox = NULL;
    _caption = NULL;
    ODL_OBJEXIT(); //####
} // CheckboxField::~CheckboxField

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

void
CheckboxField::addToComponent(Component * whereToAdd)
{
    ODL_OBJENTER(); //####
    ODL_P1("whereToAdd = ", whereToAdd); //####
    if (whereToAdd)
    {
        whereToAdd->addAndMakeVisible(_checkbox);
        whereToAdd->addAndMakeVisible(_caption);
    }
    ODL_OBJEXIT(); //####
} // CheckboxField::addToComponent

int
CheckboxField::getHeight(void)
const
{
    ODL_OBJENTER(); //####
    int result = _checkbox->getHeight();

    ODL_OBJEXIT_LL(result); //####
    return result;
} // CheckboxField::getHeight

int
CheckboxField::getMinimumWidth(void)
const
{
    ODL_OBJENTER(); //####
    int result = (_checkbox->getX() + _checkbox->getWidth() + kCheckboxToLabelGap +
                  _caption->getWidth());

    ODL_OBJEXIT_LL(result); //####
    return result;
} // CheckboxField::::getMinimumWidth

const String &
CheckboxField::getName(void)
const
{
    ODL_OBJENTER(); //####
    const String & theName = _caption->getName();

    ODL_OBJEXIT_s(theName.toStdString()); //####
    return theName;
} // CheckboxField::getName

String
CheckboxField::getText(void)
const
{
    ODL_OBJENTER(); //####
    String result = (_checkbox->getToggleState() ? "1" : "0");

    ODL_OBJEXIT_s(result.toStdString()); //####
    return result;
} // CheckboxField::getText

int
CheckboxField::getWidth(void)
const
{
    ODL_OBJENTER(); //####
    int result = (_checkbox->getX() + _checkbox->getWidth() + kCheckboxToLabelGap +
                  _caption->getWidth());

    ODL_OBJEXIT_LL(result); //####
    return result;
} // CheckboxField::getWidth

int
CheckboxField::getX(void)
const
{
    ODL_OBJENTER(); //####
    int result = _checkbox->getX();

    ODL_OBJEXIT_LL(result); //####
    return result;
} // CheckboxField::getX

int
CheckboxField::getY(void)
const
{
    ODL_OBJENTER(); //####
    int result = _checkbox->getY();

    ODL_OBJEXIT_LL(result); //####
    return result;
} // CheckboxField::getY

void
CheckboxField::removeFromComponent(Component * whereToRemove)
{
    ODL_OBJENTER(); //####
    ODL_P1("whereToRemove = ", whereToRemove); //####
    if (whereToRemove)
    {
        whereToRemove->removeChildComponent(_checkbox);
        whereToRemove->removeChildComponent(_caption);
    }
    ODL_OBJEXIT(); //####
} // CheckboxField::removeFromComponent

void
CheckboxField::setText(const String & newText)
{
    ODL_OBJENTER(); //####
    ODL_S1s("newText = ", newText.toStdString()); //####
    bool       boolValue;
    juce_wchar firstChar = tolower(newText[0]);

    if (('1' == firstChar) || ('t' == firstChar) || ('y' == firstChar))
    {
        boolValue = true;
    }
    else
    {
        boolValue = false;
    }
    _checkbox->setToggleState(boolValue, dontSendNotification);
    ODL_OBJEXIT(); //####
} // CheckboxField::setText

void
CheckboxField::setWidth(const int ww)
{
    ODL_OBJENTER(); //####
    ODL_LL1("ww = ", ww); //####
    int newWidth = ww - (_checkbox->getX() + _checkbox->getWidth() + kCheckboxToLabelGap);

    _caption->setSize(newWidth, _caption->getHeight());
    ODL_OBJEXIT(); //####
} // CheckboxField::setWidth

void
CheckboxField::setY(const int yy)
{
    ODL_OBJENTER(); //####
    ODL_LL1("yy = ", yy); //####
    _checkbox->setTopLeftPosition(_checkbox->getX(), yy);
    _caption->setTopLeftPosition(_caption->getX(), yy);
    ODL_OBJEXIT(); //####
} // CheckboxField::setY

bool
CheckboxField::validateField(StringArray & argsToUse)
{
    ODL_OBJENTER(); //####
    ODL_P1("argsToUse = ", &argsToUse); //####
    argsToUse.add(getText());
    ODL_OBJEXIT_B(true); //####
    return true;
} // CheckboxField::validateField

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
