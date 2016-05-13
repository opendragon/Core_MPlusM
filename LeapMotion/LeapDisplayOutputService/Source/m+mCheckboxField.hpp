//--------------------------------------------------------------------------------------------------
//
//  File:       m+mCheckboxField.hpp
//
//  Project:    m+m
//
//  Contains:   The class declaration for a checkbox paired with a caption.
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

#if (! defined(mpmCheckboxField_HPP_))
# define mpmCheckboxField_HPP_ /* Header guard */

# include "m+mFormField.hpp"

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file

 @brief The class declaration for a field consisting of a checkbox paired with a caption. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

/*! @brief The character to use when displaying a 'password' field. */
# define CHAR_TO_USE_FOR_PASSWORD_ 0x02022

namespace MPlusM_Manager
{
    class TextValidator;
    class ValidatingTextEditor;

    /*! @brief A field consisting of a checkbox paired with a caption. */
    class CheckboxField : public FormField
    {
    public :

    protected :

    private :

        /*! @brief The class that this class is derived from. */
        typedef FormField inherited;

    public :

        /*! @brief The constructor.
         @param regularLabelFont The font to use with the label when the text editor data is valid.
         @param index The order of the text editor.
         @param captionTitle The text of the caption.
         @param top The top coordinate of the field.
         @param componentName The name to pass to the component for it to use as its name. */
        CheckboxField(Font &         regularLabelFont,
                      const size_t   index,
                      const String & captionTitle,
                      const int      top,
                      const String & componentName = String::empty);

        /*! @brief The destructor. */
        virtual
        ~CheckboxField(void);

        /*! @brief Add the components of this field to the specified component and make them
         visible.
         @param whereToAdd The component to be added to. */
        virtual void
        addToComponent(Component * whereToAdd);

        /*! @brief Return the height of the field in pixels.
         @return The height of the field in pixels. */
        virtual int
        getHeight(void)
        const;

        /*! @brief Return the minimum width of the field in pixels.
         @return The minimum width of the field in pixels. */
        virtual int
        getMinimumWidth(void)
        const;

        /*! @brief Returns the text value associated with the field.
         @returns The text value associated with the field. */
        virtual String
        getText(void)
        const;

        /*! @brief Return the width of the field in pixels.
         @return The width of the field in pixels. */
        virtual int
        getWidth(void)
        const;

        /*! @brief Return the left coordinate of the field.
         @return The left coordinate of the field. */
        virtual int
        getX(void)
        const;

        /*! @brief Return the top coordinate of the field.
         @return The top coordinate of the field. */
        virtual int
        getY(void)
        const;

        /*! @brief Remove the components of this field from the specified component.
         @param whereToRemove The component to be removed from. */
        virtual void
        removeFromComponent(Component * whereToRemove);

        /*! @brief Set the text value associated with the field.
         @param newText The text to be used. */
        virtual void
        setText(const String & newText);

        /*! @brief Set the width of the field.
         @param ww The new width of the field. */
        virtual void
        setWidth(const int ww);

        /*! @brief Set the top coordinate of the field.
         @param yy The new top coordinate of the field. */
        virtual void
        setY(const int yy);

        /*! @brief Check the field for validity.
         @param argsToUse A set of valid arguments.
         @returns @c true if the validator accepts the field or there's no validation required or
         @c false if the validator rejects the field. */
        virtual bool
        validateField(StringArray & argsToUse);

    protected :

    private :

        /*! @brief Returns the name of the field.
         @returns The name of the field. */
        virtual const String &
        getName(void)
        const;

    public :

    protected :

    private :

        /*! @brief The text editor within the field. */
        ScopedPointer<ToggleButton> _checkbox;

        /*! @brief The caption for the field. */
        ScopedPointer<Label> _caption;

        JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR(CheckboxField)

    }; // CheckboxField

} // MPlusM_Manager

#endif // ! defined(mpmCheckboxField_HPP_)
