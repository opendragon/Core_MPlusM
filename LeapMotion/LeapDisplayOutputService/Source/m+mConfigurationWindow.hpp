//--------------------------------------------------------------------------------------------------
//
//  File:       m+mConfigurationWindow.hpp
//
//  Project:    m+m
//
//  Contains:   The class declaration for the configuration window of the m+mLeapDisplayOutputService application.
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

#if (! defined(mpmConfigurationWindow_HPP_))
# define mpmConfigurationWindow_HPP_ /* Header guard */

# include "m+mFormFieldErrorResponder.hpp"
# include "m+mValidatingTextEditor.hpp"

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file

 @brief The class declaration for the configuration window of the m+mLeapDisplayOutputService application. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace MPlusM_Manager
{
    /*! @brief The configuration window of the application. */
    class ConfigurationWindow : private AsyncUpdater,
                                private ButtonListener,
                                public DocumentWindow,
                                public FormFieldErrorResponder
    {
    public :

    protected :

    private :

        /*! @brief The first class that this class is derived from. */
        typedef AsyncUpdater inherited1;

        /*! @brief The second class that this class is derived from. */
        typedef ButtonListener inherited2;

        /*! @brief The third class that this class is derived from. */
        typedef DocumentWindow inherited3;

        /*! @brief The fourth class that this class is derived from. */
        typedef FormFieldErrorResponder inherited4;

    public :

        /*! @brief The constructor.
         @param title The title for the window.
         @param execType The kind of application being configured.
         @param currentValues The current set of values for the fields.
         @param descriptors The provided argument descriptions.
         @param valuesToUse The resulting arguments for the application. */
        ConfigurationWindow(const String &                              title,
                            const String &                              execType,
                            const YarpStringVector &                    currentValues,
                            const MplusM::Utilities::DescriptorVector & descriptors,
                            yarp::os::Bottle &                          valuesToUse);

        /*! @brief The destructor. */
        virtual
        ~ConfigurationWindow(void);

    protected :

    private :

        /*! @brief Add an extra field. */
        void
        addAnExtraField(void);

        /*! @brief Adjust the fields to their proper locations and dimensions. */
        void
        adjustFields(void);

        /*! @brief Called when a button is clicked.
         @param aButton The button that was clicked. */
        virtual void
        buttonClicked(Button * aButton);

        /*! @brief Check the values of the fields against their specifications.
         @returns @c true if all the fields are valid and @c false otherwise. */
        bool
        fieldsAreValid(void);

        /*! @brief Called when this component has just acquired the keyboard focus.
         @param cause The type of event that caused the change in focus. */
        virtual void
        focusGained(FocusChangeType cause);

        /*! @brief Called when this component has just lost the keyboard focus.
         @param cause The type of event that caused the change in focus. */
        virtual void
        focusLost(FocusChangeType cause);

        /*! @brief Called back to perform operations. */
        virtual void
        handleAsyncUpdate(void);

        /*! @brief Called when a key is pressed.
         @param key The key that was pressed.
         @returns @c true if the key was consumed and @c false otherwise. */
        virtual bool
        keyPressed(const KeyPress & key);

        /*! @brief Recalculate the area occupied by the fields and adjust the button positions. */
        void
        recalculateArea(void);

        /*! @brief Remove the most recently added extra field. */
        void
        removeMostRecentlyAddedExtraField(void);

        /*! @brief Report an error in a field.
         @param fieldOfInterest The field to be reported. */
        virtual void
        reportErrorInField(FormField & fieldOfInterest);

        /*! @brief Report an error in a field.
         @param fieldOfInterest The field to be reported. */
        virtual void
        reportErrorInField(ValidatingTextEditor & fieldOfInterest);

        /*! @brief Called when the component size has been changed. */
        virtual void
        resized(void);

        /*! @brief Set up the standard fields.
         @param widthSoFar The minimum width to show the fields.
         @param heightSoFar The minimum height to show the fields.
         @param currentValues The current set of values for the fields. */
        void
        setUpStandardFields(int &                    widthSoFar,
                            int &                    heightSoFar,
                            const YarpStringVector & currentValues);

        /*! @brief Tell all the text editor fields to ignore the next focus loss, so redundant
         validation is not done. */
        void
        tellAllFieldsToIgnoreNextFocusLoss(void);

    public :

    protected :

    private :

        /*! @brief The descriptive text at the top of the window. */
        Label _topText;

        /*! @brief The 'Cancel' button. */
        TextButton _cancelButton;

        /*! @brief The 'OK' button. */
        TextButton _okButton;

        /*! @brief The content area to hold everything. */
        Component _contentArea;

        /*! @brief The monospaced font for error text. */
        Font _errorFont;

        /*! @brief The regular monospaced font to use. */
        Font _regularFont;

        /*! @brief The kind of application being configured. */
        String _execType;

        /*! @brief The root name for extra arguments. */
        String _extraArgRootName;

        /*! @brief The provided argument descriptions. */
        const MplusM::Utilities::DescriptorVector & _descriptors;

        /*! @brief The set of extra fields. */
        OwnedArray<FormField> _extraFields;

        /*! @brief The set of standard fields. */
        OwnedArray<FormField> _standardFields;

        /*! @brief The 'extra arguments' area of the window. */
        ScopedPointer<GroupComponent> _extraArgumentsGroup;

        /*! @brief The '+ arguments' button. */
        ScopedPointer<TextButton> _addArgumentsButton;

        /*! @brief The '- arguments' button. */
        ScopedPointer<TextButton> _removeArgumentsButton;

        /*! @brief Set to the values provided by the user. */
        yarp::os::Bottle & _valuesToUse;

        /*! @brief @c true if extra arguments are present and @c false if they aren't used. */
        bool _hasExtraArguments;

        /*! @brief @c true if one or more of the text fields are for file paths. */
        bool _hasFileField;

        JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR(ConfigurationWindow)

    }; // ConfigurationWindow

} // MPlusM_Manager

#endif // ! defined(mpmConfigurationWindow_HPP_)
