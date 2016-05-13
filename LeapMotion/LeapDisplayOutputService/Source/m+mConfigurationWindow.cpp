//--------------------------------------------------------------------------------------------------
//
//  File:       m+mConfigurationWindow.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for the configuration window of the m+mLeapDisplayOutputService application.
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
//  Created:    2015-07-20
//
//--------------------------------------------------------------------------------------------------

#include "m+mConfigurationWindow.hpp"

#include "m+mCaptionedTextField.hpp"
#include "m+mCheckboxField.hpp"
#include "m+mManagerApplication.hpp"
#include "m+mTextValidator.hpp"

#include <m+m/m+mChannelArgumentDescriptor.hpp>
#include <m+m/m+mEndpoint.hpp>
#include <m+m/m+mPortArgumentDescriptor.hpp>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file

 @brief The class definition for the configuration window of the m+mLeapDisplayOutputService application. */
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

/*! @brief The colour to be used for the window background. */
static const Colour & kWindowBackgroundColour(Colours::whitesmoke);

/*! @brief The extra space around the content in the window. */
static const int kExtraSpaceInWindow = 20;

/*! @brief The internal name for the endpoint text entry field. */
static const String kEndpointFieldName("$$$endpoint$$$");

/*! @brief The internal name for the port text entry field. */
static const String kPortFieldName("$$$port$$$");

/*! @brief The internal name for the tag text entry field. */
static const String kTagFieldName("$$$tag$$$");

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

ConfigurationWindow::ConfigurationWindow(const String &                              title,
                                         const String &                              execType,
                                         const YarpStringVector &                    currentValues,
                                         const MplusM::Utilities::DescriptorVector & descriptors,
                                         yarp::os::Bottle &                          valuesToUse) :
    inherited1(), inherited2(), inherited3(title, kWindowBackgroundColour, 0), inherited4(),
    _topText("topText"), _cancelButton("Cancel"), _okButton("OK"),
    _errorFont(Font::getDefaultMonospacedFontName(), FormField::kFontSize,
               Font::italic + Font::bold), _regularFont(Font::getDefaultMonospacedFontName(),
                                                        FormField::kFontSize, Font::plain),
    _execType(execType), _descriptors(descriptors), _extraArgumentsGroup(NULL),
    _addArgumentsButton(NULL), _removeArgumentsButton(NULL), _valuesToUse(valuesToUse),
    _hasExtraArguments(false), _hasFileField(false)
{
    ODL_ENTER(); //####
    ODL_S2s("title = ", title.toStdString(), "execType = ", execType.toStdString()); //####
    ODL_P2("descriptors = ", &descriptors, "argsToUse = ", &valuesToUse); //####
    ManagerApplication * ourApp = ManagerApplication::getApp();

    if (ourApp)
    {
        ourApp->doScanSoon();
    }
    _contentArea.setSize(100, 100);
    setContentNonOwned(&_contentArea, true);
    BorderSize<int> bt = getBorderThickness();
    BorderSize<int> cb = getContentComponentBorder();
    int             heightSoFar = 0;
    int             widthSoFar = 0;

    _valuesToUse.clear();
    setUpStandardFields(widthSoFar, heightSoFar, currentValues);
    int minW = jmax(widthSoFar,
                    _cancelButton.getWidth() + _okButton.getWidth() + (3 * FormField::kButtonGap));
    int calcW = minW + bt.getLeftAndRight() + cb.getLeftAndRight();
    int calcH = heightSoFar + bt.getTopAndBottom() + cb.getTopAndBottom();

    centreWithSize(calcW + kExtraSpaceInWindow, calcH + kExtraSpaceInWindow);
    adjustFields();
    setOpaque(true);
    setResizable(false, false);
    setVisible(true);
    addKeyListener(ManagerWindow::getApplicationCommandManager().getKeyMappings());
    triggerAsyncUpdate();
    ODL_EXIT_P(this); //####
} // ConfigurationWindow::ConfigurationWindow

ConfigurationWindow::~ConfigurationWindow(void)
{
    ODL_OBJENTER(); //####
    ODL_OBJEXIT(); //####
} // ConfigurationWindow::~ConfigurationWindow

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

void
ConfigurationWindow::addAnExtraField(void)
{
    ODL_ENTER(); //####
    String               compCountAsString(static_cast<int>(_extraFields.size() + 1));
    CaptionedTextField * newField = new CaptionedTextField(*this, _regularFont, _errorFont,
                                                           _extraFields.size(),
                                                           _extraArgRootName + " " +
                                                           compCountAsString, 0, true, false, NULL,
                                                           NULL, _extraArgRootName + "_" +
                                                           compCountAsString);

    _extraFields.add(newField);
    newField->addToComponent(_extraArgumentsGroup);
    recalculateArea();
    adjustFields();
    if (_removeArgumentsButton)
    {
        _removeArgumentsButton->setVisible(true);
    }
    ODL_EXIT(); //####
} // ConfigurationWindow::addAnExtraField

void
ConfigurationWindow::adjustFields(void)
{
    ODL_OBJENTER(); //####
    Component * content = getContentComponent();
    int         newButtonTop = content->getHeight() - (_cancelButton.getHeight() +
                                                       FormField::kButtonGap);
    int         newFieldWidth = content->getWidth() - (2 * FormField::kFieldInset);

    if (_hasFileField)
    {
        newFieldWidth -= (FormField::kButtonGap + CaptionedTextField::getFileButtonWidth());
    }
    _cancelButton.setTopLeftPosition(getWidth() - (_cancelButton.getWidth() +
                                                   FormField::kButtonGap), newButtonTop);
    _okButton.setTopLeftPosition(_cancelButton.getX() - (_okButton.getWidth() +
                                                         FormField::kButtonGap), newButtonTop);
    if (_addArgumentsButton)
    {
        _addArgumentsButton->setTopLeftPosition(_okButton.getX() -
                                                (_addArgumentsButton->getWidth() +
                                                 FormField::kButtonGap), newButtonTop);
    }
    if (_removeArgumentsButton)
    {
        _removeArgumentsButton->setTopLeftPosition(_addArgumentsButton->getX() -
                                                   (_removeArgumentsButton->getWidth() +
                                                    FormField::kButtonGap), newButtonTop);
    }
    for (size_t ii = 0, maxf = _standardFields.size(); maxf > ii; ++ii)
    {
        FormField * aField = _standardFields[static_cast<int>(ii)];

        if (aField)
        {
            aField->setWidth(newFieldWidth);
        }
    }
    if (_extraArgumentsGroup)
    {
        int groupWidth = (_cancelButton.getX() + _cancelButton.getWidth() -
                          (_extraArgumentsGroup->getX() + FormField::kButtonGap));
        int innerWidth = groupWidth - (FormField::kFieldInset + (2 * FormField::kButtonGap));

        for (size_t ii = 0, maxf = _extraFields.size(); maxf > ii; ++ii)
        {
            FormField * aField = _extraFields[static_cast<int>(ii)];

            aField->setWidth(innerWidth);
        }
        _extraArgumentsGroup->setSize(groupWidth, _extraArgumentsGroup->getHeight());
    }
    ODL_OBJEXIT(); //####
} // ConfigurationWindow::adjustFields

void
ConfigurationWindow::buttonClicked(Button * aButton)
{
    ODL_OBJENTER(); //####
    ODL_P1("aButton = ", aButton); //####
    int commandId = aButton->getCommandID();

    tellAllFieldsToIgnoreNextFocusLoss();
    switch (commandId)
    {
        case kConfigurationAddField :
            addAnExtraField();
            break;

        case kConfigurationRemoveField :
            removeMostRecentlyAddedExtraField();
            break;

        case kConfigurationOK :
            if (fieldsAreValid())
            {
                exitModalState(commandId);
            }
            break;

        case kConfigurationFileRequest :
            for (size_t ii = 0, maxf = _standardFields.size(); maxf > ii; ++ii)
            {
                FormField * aField = _standardFields[static_cast<int>(ii)];

                if (aField && (aField->getButton() == aButton))
                {
                    aField->performButtonAction();
                    break;
                }

            }
            break;

        default :
            exitModalState(commandId);
            break;

    }
    ODL_OBJEXIT(); //####
} // ConfigurationWindow::buttonClicked

bool
ConfigurationWindow::fieldsAreValid(void)
{
    ODL_ENTER(); //####
    int    badCount = 0;
    String badArgs;

    for (size_t ii = 0, maxf = _standardFields.size(); maxf > ii; ++ii)
    {
        FormField * aField = _standardFields[static_cast<int>(ii)];

        if (aField)
        {
            if (aField->validateField())
            {
                size_t jj = aField->getIndex();

                if (_descriptors.size() > jj)
                {
                    Utilities::BaseArgumentDescriptor * aDescriptor = _descriptors[jj];

                    if (aDescriptor && aDescriptor->validate(aField->getText().toStdString()))
                    {
                        aDescriptor->addValueToBottle(_valuesToUse);
                    }
                    else
                    {
                        if (0 < badArgs.length())
                        {
                            badArgs += "\n";
                        }
                        badArgs += aField->getName();
                        ++badCount;
                    }
                }
            }
            else
            {
                if (0 < badArgs.length())
                {
                    badArgs += "\n";
                }
                badArgs += aField->getName();
                ++badCount;
            }
        }
    }
    if (0 == badCount)
    {
        // Add the extra arguments here.
        for (size_t ii = 0, maxf = _extraFields.size(); maxf > ii; ++ii)
        {
            FormField * anEditor = _extraFields[static_cast<int>(ii)];

            _valuesToUse.addString(anEditor->getText().toStdString());
        }
    }
    if (0 < badCount)
    {
        String message1((1 < badCount) ? "arguments are" : "argument is");
        String message2((1 < badCount) ? "arguments" : "argument");

        AlertWindow::showMessageBox(AlertWindow::WarningIcon, getName(),
                                    String("The following ") + message1 + " invalid:\n" + badArgs +
                                    "\n" + String("Please correct the ") + message2 + " to the " +
                                    _execType + " and try again.", String::empty, this);
    }
    ODL_EXIT_B(0 == badCount); //####
    return (0 == badCount);
} // ConfigurationWindow::fieldsAreValid

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
void
ConfigurationWindow::focusGained(FocusChangeType cause)
{
#if MAC_OR_LINUX_
# pragma unused(cause)
#endif // MAC_OR_LINUX_
    ODL_OBJENTER(); //####
    ODL_OBJEXIT(); //####
} // ConfigurationWindow::focusGained
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
void
ConfigurationWindow::focusLost(FocusChangeType cause)
{
#if MAC_OR_LINUX_
# pragma unused(cause)
#endif // MAC_OR_LINUX_
    ODL_OBJENTER(); //####
    ODL_OBJEXIT(); //####
} // ConfigurationWindow::focusLost
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

void
ConfigurationWindow::handleAsyncUpdate(void)
{
    ODL_OBJENTER(); //####
    ApplicationCommandManager & commandManager = ManagerWindow::getApplicationCommandManager();

    commandManager.registerAllCommandsForTarget(JUCEApplication::getInstance());
    ODL_OBJEXIT(); //####
} // ConfigurationWindow::handleAsyncUpdate

bool
ConfigurationWindow::keyPressed(const KeyPress & key)
{
    ODL_OBJENTER(); //####
    ODL_P1("key = ", &key); //####
    bool result;

    if (key == KeyPress::escapeKey)
    {
        tellAllFieldsToIgnoreNextFocusLoss();
        exitModalState(kConfigurationCancel);
        result = true;
    }
    else if (key == KeyPress::returnKey)
    {
        tellAllFieldsToIgnoreNextFocusLoss();
        if (fieldsAreValid())
        {
            exitModalState(kConfigurationOK);
        }
        result = true;
    }
    else
    {
        result = inherited3::keyPressed(key);
    }
    ODL_OBJEXIT_B(result); //####
    return result;
} // ConfigurationWindow::keyPressed

void
ConfigurationWindow::recalculateArea(void)
{
    ODL_ENTER(); //####
    int    heightSoFar = 0;
    int    widthSoFar = 0;
    size_t numExtra = _extraFields.size();

    heightSoFar = _topText.getY() + _topText.getHeight() + FormField::kButtonGap;
    widthSoFar = jmax(widthSoFar, _topText.getX() + _topText.getWidth());
    for (size_t ii = 0, numDescriptors = _descriptors.size(), jj = 0; numDescriptors > ii; ++ii)
    {
        Utilities::BaseArgumentDescriptor * aDescriptor = _descriptors[ii];

        if (aDescriptor)
        {
            String argName(aDescriptor->argumentName().c_str());
            String argDescription(aDescriptor->argumentDescription().c_str());

            if (! aDescriptor->isExtra())
            {
                FormField * aField = _standardFields[static_cast<int>(jj)];

                if (aField)
                {
                    widthSoFar = jmax(widthSoFar, aField->getMinimumWidth());
                    heightSoFar = aField->getY() + aField->getHeight() +
                    (FormField::kButtonGap / 2);
                    ++jj;
                }
            }
        }
    }
    if (_extraArgumentsGroup)
    {
        int innerHeight = static_cast<int>(_regularFont.getHeight()) + (FormField::kButtonGap / 2);
        int innerWidth = (2 * jmax(FormField::kFieldInset, FormField::kLabelInset));

        _extraArgumentsGroup->setTopLeftPosition(FormField::kFieldInset, heightSoFar);
        for (size_t ii = 0; numExtra > ii; ++ii)
        {
            FormField * aField = _extraFields[static_cast<int>(ii)];

            aField->setY(innerHeight);
            innerHeight = aField->getY() + aField->getHeight() + (FormField::kButtonGap / 2);
        }
        if (0 < numExtra)
        {
            innerHeight += (3 * FormField::FormField::kButtonGap / 4);
        }
        else
        {
            innerHeight += (FormField::kButtonGap / 2);
        }
        _extraArgumentsGroup->setSize(innerWidth, innerHeight);
        heightSoFar = _extraArgumentsGroup->getY() + _extraArgumentsGroup->getHeight();
        widthSoFar = jmax(widthSoFar, _extraArgumentsGroup->getX() +
                          _extraArgumentsGroup->getWidth());
    }
    BorderSize<int> cb = getContentComponentBorder();
    int             minW = jmax(widthSoFar, _cancelButton.getWidth() + _okButton.getWidth() +
                                (3 * FormField::kButtonGap));

    setContentComponentSize(minW + kExtraSpaceInWindow + cb.getLeftAndRight(),
                            heightSoFar + kExtraSpaceInWindow + cb.getTopAndBottom());
    ODL_EXIT(); //####
} // ConfigurationWindow::recalculateArea

void
ConfigurationWindow::removeMostRecentlyAddedExtraField(void)
{
    ODL_ENTER(); //####
    FormField * lastField = _extraFields.getLast();

    _extraFields.removeLast();
    lastField->removeFromComponent(_extraArgumentsGroup);
    recalculateArea();
    adjustFields();
    if (_removeArgumentsButton)
    {
        _removeArgumentsButton->setVisible(0 < _extraFields.size());
    }
    ODL_EXIT(); //####
} // ConfigurationWindow::removeMostRecentlyAddedExtraField

void
ConfigurationWindow::reportErrorInField(FormField & fieldOfInterest)
{
    ODL_OBJENTER(); //####
    ODL_P1("fieldOfInterest = ", &fieldOfInterest); //####
    AlertWindow::showMessageBox(AlertWindow::WarningIcon, getName(), String("The ") +
                                fieldOfInterest.getName() + " argument is invalid.\n"
                                "Please correct the argument and try again.", String::empty, this);
    ODL_OBJEXIT(); //####
} // ConfigurationWindow::reportErrorInField

void
ConfigurationWindow::reportErrorInField(ValidatingTextEditor & fieldOfInterest)
{
    ODL_OBJENTER(); //####
    ODL_P1("fieldOfInterest = ", &fieldOfInterest); //####
    AlertWindow::showMessageBox(AlertWindow::WarningIcon, getName(), String("The ") +
                                fieldOfInterest.getName() + " argument is invalid.\n"
                                "Please correct the argument and try again.", String::empty, this);
    ODL_OBJEXIT(); //####
} // ConfigurationWindow::reportErrorInField

void
ConfigurationWindow::resized(void)
{
    ODL_OBJENTER(); //####
    Button * close = getCloseButton();

    inherited3::resized();
    if (close)
    {
        const KeyPress esc(KeyPress::escapeKey, 0, 0);

        if (! close->isRegisteredForShortcut(esc))
        {
            close->addShortcut(esc);
        }
    }
    ODL_OBJEXIT(); //####
} // ConfigurationWindow::resized

void
ConfigurationWindow::setUpStandardFields(int &                    widthSoFar,
                                         int &                    heightSoFar,
                                         const YarpStringVector & currentValues)
{
    ODL_OBJENTER(); //####
    ODL_P3("widthSoFar = ", &widthSoFar, "heightSoFar = ", &heightSoFar, //####
           "currentValues = ", &currentValues); //####
    Component * content = getContentComponent();
    int         buttonHeight = ManagerApplication::getButtonHeight();
    Point<int>  dimensions;
    size_t      numDescriptors = _descriptors.size();

    widthSoFar = heightSoFar = 0;
    _topText.setFont(_regularFont);
    if (0 < numDescriptors)
    {
        _topText.setText(String("The ") + _execType +
                         " has one or more values that can be configured.", dontSendNotification);
    }
    else
    {
        _topText.setText(String("The ") + _execType + " has no configurable values.",
                         dontSendNotification);
    }
    MPlusM_Manager::CalculateTextArea(dimensions, _regularFont, _topText.getText());
    _topText.setBounds(FormField::kButtonGap, FormField::kButtonGap + getTitleBarHeight(),
                       dimensions.getX() + FormField::kButtonGap, dimensions.getY());
    content->addAndMakeVisible(&_topText, 0);
    heightSoFar = _topText.getY() + _topText.getHeight() + FormField::kButtonGap;
    widthSoFar = jmax(widthSoFar, _topText.getX() + _topText.getWidth());
    // Check for one or more file descriptors
    for (size_t ii = 0; numDescriptors > ii; ++ii)
    {
        bool                                forOutput;
        Utilities::BaseArgumentDescriptor * aDescriptor = _descriptors[ii];

        if (aDescriptor && (! aDescriptor->isRequired()) && aDescriptor->isForFiles(forOutput))
        {
            _hasFileField = true;
            break;
        }

    }
    for (size_t ii = 0, jj = 0; numDescriptors > ii; ++ii)
    {
        Utilities::BaseArgumentDescriptor * aDescriptor = _descriptors[ii];

        if (aDescriptor && (! aDescriptor->isRequired()))
        {
            String argName(aDescriptor->argumentName().c_str());
            String argDescription(aDescriptor->argumentDescription().c_str());

            if (aDescriptor->isExtra())
            {
                if (! _hasExtraArguments)
                {
                    _hasExtraArguments = true;
                    _extraArgRootName = argName;
                    _extraArgumentsGroup = new GroupComponent("", argDescription);
                    _extraArgumentsGroup->setBounds(FormField::kFieldInset, heightSoFar,
                                                    widthSoFar - (FormField::kButtonGap +
                                                                  FormField::kFieldInset),
                                                    static_cast<int>(_regularFont.getHeight()) +
                                                    FormField::kButtonGap);
                    content->addAndMakeVisible(_extraArgumentsGroup);
                    heightSoFar = (_extraArgumentsGroup->getY() +
                                   _extraArgumentsGroup->getHeight() + (FormField::kButtonGap / 2));
                    widthSoFar = jmax(widthSoFar, _extraArgumentsGroup->getX() +
                                      _extraArgumentsGroup->getWidth());
                    _addArgumentsButton = new TextButton(String("+ ") + argName);
                    _addArgumentsButton->setWantsKeyboardFocus(true);
                    _addArgumentsButton->setMouseClickGrabsKeyboardFocus(false);
                    _addArgumentsButton->setCommandToTrigger(NULL, kConfigurationAddField, false);
                    _addArgumentsButton->addListener(this);
                    _addArgumentsButton->changeWidthToFitText(buttonHeight);
                    content->addAndMakeVisible(_addArgumentsButton, 0);
                    _removeArgumentsButton = new TextButton(String("- ") + argName);
                    _removeArgumentsButton->setWantsKeyboardFocus(true);
                    _removeArgumentsButton->setMouseClickGrabsKeyboardFocus(false);
                    _removeArgumentsButton->setCommandToTrigger(NULL, kConfigurationRemoveField,
                                                                false);
                    _removeArgumentsButton->addListener(this);
                    _removeArgumentsButton->changeWidthToFitText(buttonHeight);
                    content->addChildComponent(_removeArgumentsButton);
                }
            }
            else if (aDescriptor->isModifiable())
            {
                if (aDescriptor->isBoolean())
                {
                    String          descriptionPrefix(aDescriptor->isOptional() ? "(Optional) " :
                                                      "");
                    String          valueToUse;
                    CheckboxField * newField = new CheckboxField(_regularFont, ii,
                                                                 descriptionPrefix +
                                                                 argDescription, heightSoFar,
                                                                 argName);

                    if (jj < currentValues.size())
                    {
                        valueToUse = currentValues[jj].c_str();
                    }
                    else
                    {
                        valueToUse = aDescriptor->getDefaultValue().c_str();
                    }
                    newField->setText(valueToUse);
                    _standardFields.add(newField);
                    newField->addToComponent(content);
                    widthSoFar = jmax(widthSoFar, newField->getMinimumWidth());
                    heightSoFar = (newField->getY() + newField->getHeight() +
                                   (FormField::kButtonGap / 2));
                }
                else
                {
                    bool forFilePath;
                    bool forOutput;

                    if (aDescriptor->isForFiles(forOutput))
                    {
                        forFilePath = true;
                    }
                    else
                    {
                        forFilePath = false;
                    }
                    String               descriptionPrefix(aDescriptor->isOptional() ?
                                                           "(Optional) " : "");
                    String               valueToUse;
                    CaptionedTextField * newField = new CaptionedTextField(*this, _regularFont,
                                                                           _errorFont, ii,
                                                                           descriptionPrefix +
                                                                           argDescription,
                                                                           heightSoFar, false,
                                                                           forFilePath, this,
                                                                   new TextValidator(*aDescriptor),
                                                                           argName,
                                                                       aDescriptor->isPassword() ?
                                                                       CHAR_TO_USE_FOR_PASSWORD_ :
                                                                           0);

                    if (jj < currentValues.size())
                    {
                        valueToUse = currentValues[jj].c_str();
                    }
                    else
                    {
                        valueToUse = aDescriptor->getDefaultValue().c_str();
                    }
                    newField->setText(valueToUse);
                    _standardFields.add(newField);
                    newField->addToComponent(content);
                    widthSoFar = jmax(widthSoFar, newField->getMinimumWidth());
                    heightSoFar = (newField->getY() + newField->getHeight() +
                                   (FormField::kButtonGap / 2));
                }
            }
            ++jj;
        }
    }
    _cancelButton.setWantsKeyboardFocus(true);
    _cancelButton.setMouseClickGrabsKeyboardFocus(false);
    _cancelButton.setCommandToTrigger(NULL, kConfigurationCancel, false);
    _cancelButton.addListener(this);
    _cancelButton.changeWidthToFitText(buttonHeight);
    _cancelButton.setTopLeftPosition(0, heightSoFar + FormField::kButtonGap);
    content->addAndMakeVisible(&_cancelButton, 0);
    _okButton.setWantsKeyboardFocus(true);
    _okButton.setMouseClickGrabsKeyboardFocus(false);
    _okButton.setCommandToTrigger(NULL, kConfigurationOK, false);
    _okButton.addListener(this);
    _okButton.changeWidthToFitText(buttonHeight);
    _okButton.setTopLeftPosition(0, heightSoFar + FormField::kButtonGap);
    content->addAndMakeVisible(&_okButton, 0);
    heightSoFar += buttonHeight;
    ODL_OBJEXIT(); //####
} // ConfigurationWindow::setUpStandardFields

void
ConfigurationWindow::tellAllFieldsToIgnoreNextFocusLoss(void)
{
    ODL_ENTER(); //####
    for (size_t ii = 0, maxf = _standardFields.size(); maxf > ii; ++ii)
    {
        FormField * aField = _standardFields[static_cast<int>(ii)];

        if (aField)
        {
            aField->ignoreNextFocusLoss();
        }
    }
    for (size_t ii = 0, maxf = _extraFields.size(); maxf > ii; ++ii)
    {
        FormField * aField = _extraFields[static_cast<int>(ii)];

        if (aField)
        {
            aField->ignoreNextFocusLoss();
        }
    }
    ODL_EXIT(); //####
} // ConfigurationWindow::tellAllFieldsToIgnoreNextFocusLoss

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
