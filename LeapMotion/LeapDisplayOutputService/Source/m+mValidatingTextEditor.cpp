//--------------------------------------------------------------------------------------------------
//
//  File:       m+mValidatingTextEditor.cpp
//
//  Project:    m+m
//
//  Contains:   The class declaration for a text editor that performs validation.
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

#include "m+mValidatingTextEditor.hpp"

#include "m+mCaptionedTextField.hpp"
#include "m+mTextValidator.hpp"

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

 @brief The class declaration for a text editor that performs validation. */
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

ValidatingTextEditor::ValidatingTextEditor(CaptionedTextField & owner,
                                           TextValidator *      validator,
                                           const String &       componentName,
                                           juce_wchar           passwordCharacter) :
    inherited(componentName, passwordCharacter), _owner(owner), _validator(validator),
    _ignoreNextFocusLoss(false)
{
    ODL_ENTER(); //####
    ODL_P2("owner = ", &owner, "validator = ", validator); //####
    ODL_S1s("componentName = ", componentName.toStdString()); //####
    ODL_LL1("passwordCharacter = ", passwordCharacter); //####
    ODL_EXIT_P(this); //####
} // ValidatingTextEditor::ValidatingTextEditor

ValidatingTextEditor::~ValidatingTextEditor(void)
{
    ODL_OBJENTER(); //####
    ODL_OBJEXIT(); //####
} // ValidatingTextEditor::~ValidatingTextEditor

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

void
ValidatingTextEditor::addPopupMenuItems(PopupMenu &        menuToAddTo,
                                        const MouseEvent * mouseClickEvent)
{
    ODL_OBJENTER(); //####
    ODL_P2("menuToAddTo = ", &menuToAddTo, "mouseClickEvent = ", mouseClickEvent); //####
    inherited::addPopupMenuItems(menuToAddTo, mouseClickEvent);
    if (_validator)
    {
        bool forOutput;

        if (_validator->isForFiles(forOutput))
        {
            menuToAddTo.addSeparator();
            if (forOutput)
            {
                menuToAddTo.addItem(kPopupSelectFileToSave, "Save...");
            }
            else
            {
                menuToAddTo.addItem(kPopupSelectFileToOpen, "Open...");
            }
        }
    }
    ODL_OBJEXIT(); //####
} // ValidatingTextEditor::addPopupMenuItems

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
void
ValidatingTextEditor::focusGained(FocusChangeType cause)
{
#if MAC_OR_LINUX_
# pragma unused(cause)
#endif // MAC_OR_LINUX_
    ODL_OBJENTER(); //####
    ODL_OBJEXIT(); //####
} // ValidatingTextEditor::focusGained
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
void
ValidatingTextEditor::focusLost(FocusChangeType cause)
{
#if MAC_OR_LINUX_
# pragma unused(cause)
#endif // MAC_OR_LINUX_
    ODL_OBJENTER(); //####
    if (_ignoreNextFocusLoss)
    {
        _ignoreNextFocusLoss = false;
    }
    else if (! validateField())
    {
        _owner.reportErrorInField();
    }
    ODL_OBJEXIT(); //####
} // ValidatingTextEditor::focusLost
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

void
ValidatingTextEditor::ignoreNextFocusLoss(void)
{
    ODL_OBJENTER(); //####
    _ignoreNextFocusLoss = true;
    ODL_OBJEXIT(); //####
} // ValidatingTextEditor::ignoreNextFocusLoss

bool
ValidatingTextEditor::keyPressed(const KeyPress & key)
{
    ODL_OBJENTER(); //####
    ODL_P1("key = ", &key); //####
    bool result;

    if (key == KeyPress::tabKey)
    {
        if (validateField())
        {
            result = inherited::keyPressed(key);
        }
        else
        {
            ignoreNextFocusLoss();
            _owner.reportErrorInField();
            result = true;
        }
    }
    else if (key == KeyPress::escapeKey)
    {
        ignoreNextFocusLoss();
        result = inherited::keyPressed(key);
    }
    else
    {
        result = inherited::keyPressed(key);
    }
    ODL_OBJEXIT_B(result); //####
    return result;
} // ValidatingTextEditor::keyPressed

void
ValidatingTextEditor::performPopupMenuAction(int menuItemID)
{
    ODL_OBJENTER(); //####
    ODL_LL1("menuItemID = ", menuItemID); //####
    switch (menuItemID)
    {
        case kPopupSelectFileToOpen :
        case kPopupSelectFileToSave :
            _owner.performButtonAction();
            break;

        default :
            inherited::performPopupMenuAction(menuItemID);
            break;

    }
    ODL_OBJEXIT(); //####
} // ValidatingTextEditor::performPopupMenuAction

bool
ValidatingTextEditor::validateField(void)
{
    ODL_OBJENTER(); //####
    bool result;

    if (_validator)
    {
        result = _validator->checkValidity(getText());
    }
    else
    {
        result = true;
    }
    ODL_OBJEXIT_B(result); //####
    return result;
} // ValidatingTextEditor::validateField

bool
ValidatingTextEditor::validateField(StringArray & argsToUse)
{
    ODL_OBJENTER(); //####
    ODL_P1("argsToUse = ", &argsToUse); //####
    bool result;

    if (_validator)
    {
        result = _validator->checkValidity(getText(), argsToUse);
    }
    else
    {
        result = true;
        argsToUse.add(getText());
    }
    ODL_OBJEXIT_B(result); //####
    return result;
} // ValidatingTextEditor::validateField

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
