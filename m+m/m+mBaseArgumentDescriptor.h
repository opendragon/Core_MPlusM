//--------------------------------------------------------------------------------------------------
//
//  File:       m+m/m+mBaseArgumentDescriptor.h
//
//  Project:    m+m
//
//  Contains:   The class declaration for the minimal functionality required to represent a
//              command-line argument.
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
//                  list of conditions and the following disclaimer in the documentation and / or
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
//  Created:    2015-05-15
//
//--------------------------------------------------------------------------------------------------

#if (! defined(MpMBaseArgumentDescriptor_H_))
# define MpMBaseArgumentDescriptor_H_ /* Header guard */

# include <m+m/m+mCommon.h>
# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wnon-virtual-dtor"
# endif // defined(__APPLE__)
# if (! MAC_OR_LINUX_)
#  pragma warning(push)
#  pragma warning(disable: 4512)
# endif // ! MAC_OR_LINUX_
# include <m+m/optionparser.h>
# if (! MAC_OR_LINUX_)
#  pragma warning(pop)
# endif // ! MAC_OR_LINUX_
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 @brief The class declaration for the minimal functionality required to represent a command-line
 argument. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace MplusM
{
    namespace Utilities
    {
        /*! @brief The mode of an argument. */
        enum ArgumentMode
        {
            /*! @brief The argument is required. */
            kArgModeRequired = 0x00,
            
            /*! @brief The argument is optional. */
            kArgModeOptional = 0x01
            
        }; // ArgumentMode
        
        /*! @brief An argument description.
         
         The external representation of an argument description is:
         
         argFormat ::= argName sep typeTagAndInfo sep default_value sep text_description_for_label;
         
         sep ::= ':';
         
         default_value ::= delimiter text delimiter;
         # use matching pairs of |, <>, (), {}, [], whichever is not present in the text

         where typeTagAndInfo is described with each derived class.
         The following enumerates the standard type tags:
         
         'A' / 'a' => address
         
         'C' / 'c' => channel
         
         'D' / 'd' => double
         
         'E' / 'e' => extra (a placeholder for zero or more trailing arguments)
         
         'F' / 'f' => file path
         
         'I' / 'i' => integer
         
         'P' / 'p' => port number
         
         'S' / 's' => string */
        class BaseArgumentDescriptor
        {
        public :
            
            /*! @brief The constructor.
             @param argName The name of the command-line argument.
             @param argDescription A description of the command-line argument.
             @param argMode The mode of the command-line argument. */
            BaseArgumentDescriptor(const YarpString & argName,
                                   const YarpString & argDescription,
                                   const ArgumentMode argMode);
            
            /*! @brief The destructor. */
            virtual ~BaseArgumentDescriptor(void);
            
            /*! @brief Return the description of the command-line argument.
             @returns The description of the command-line argument. */
            inline const YarpString & argumentDescription(void)
            const
            {
                return _argDescription;
            } // argumentDescription

            /*! @brief Return the name of the command-line argument.
             @returns The name of the command-line argument. */
            inline const YarpString & argumentName(void)
            const
            {
                return _argName;
            } // argumentName
            
            /*! @brief Return the default value.
             @returns The default value. */
            virtual YarpString getDefaultValue(void) = 0;

            /*! @brief Return the processed value.
             @returns The processed value. */
            virtual YarpString getProcessedValue(void) = 0;
            
            /*! @brief Return @c true if the argument is a placeholder for zero or more trailing
             arguments.
             @returns @c true if the argument is a placeholder for zero of more trailing arguments
             and @c false otherwise. */
            virtual bool isExtra(void)
            const
            {
                return false;
            } // isExtra
            
            /*! @brief Return @c true if the argument is for file paths and @c false otherwise.
             @param isForOutput Set to @c true if the argument is for output files and @c false
             otherwise.
             @returns @c true if the argument is for file paths and @c false otherwise. */
            virtual bool isForFiles(bool & isForOutput)
            const
            {
                isForOutput = false;
                return false;
            } // isForFiles
            
            /*! @brief Return @c true if the argument is optional and @c false otherwise.
             @returns @c true if the argument is optional and @c false otherwise. */
            inline bool isOptional(void)
            const
            {
                return (_argMode & kArgModeOptional);
            } // isOptional
          
            /*! @brief Set the associated variable to the default value. */
            virtual void setToDefault(void) = 0;

            /*! @brief Convert to a printable representation.
             @returns A printable representation of the descriptor. */
            virtual YarpString toString(void) = 0;
            
            /*! @brief Check an input value against the constraints of the descriptor.
             @param value The value to be checked.
             @returns @c true if the value is within the domain of the descriptor and @c false
             otherwise. */
            virtual bool validate(const YarpString & value)
            const = 0;
            
        protected :

            /*! @brief Partition a string that is in 'arguments' format into a sequence of strings.
             @param inString The string to be partitioned.
             @param indexOfDefaultValue The position in the input string where the default value
             will appear.
             @param result The partitioned string.
             @returns @c true if the correct number of fields appear within the input string and
             @c false otherwise. */
            static bool partitionString(const YarpString & inString,
                                        const size_t       indexOfDefaultValue,
                                        YarpStringVector & result);

            /*! @brief Returns a string that contains a printable representation of the standard
             prefix fields for a command-line argument.
             @param tagForMandatoryField The tag value to use if the field is mandatory.
             @param tagForOptionalField The tag value to use if the field is optional.
             @returns A string that contains a printable representation of the standard prefix
             fields for a command-line argument. */
            YarpString prefixFields(const YarpString & tagForMandatoryField,
                                    const YarpString & tagForOptionalField)
            const;

            /*! @brief Returns a string that contains a printable representation of the standard
             fields for a command-line argument.
             @param defaultToUse The string to put in the printable representation for the default
             value.
             @returns A string that contains a printable representation of the standard fields for
             a command-line argument. */
            YarpString suffixFields(const YarpString & defaultToUse);
            
        private :
            
            COPY_AND_ASSIGNMENT_(BaseArgumentDescriptor);
            
        public :
        
        protected :
            
            /*! @brief The separator string to use when converting to a string. */
            static YarpString _parameterSeparator;
        
        private :
            
            /*! @brief The description of the command-line argument for the adapter. */
            YarpString _argDescription;

            /*! @brief The name of the command-line argument. */
            YarpString _argName;
            
            /*! @brief The mode of the command-line argument. */
            ArgumentMode _argMode;
            
# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunused-private-field"
# endif // defined(__APPLE__)
            /*! @brief Filler to pad to alignment boundary */
            char _filler[7];
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)
        
        }; // BaseArgumentDescriptor
        
        /*! @brief A sequence of argument descriptors. */
        typedef std::vector<BaseArgumentDescriptor *> DescriptorVector;
        
        /*! @brief Generate the standard 'argument list' description from an argument sequence.
         @param arguments The argument sequence.
         @returns A string containing the standard 'argument list' representation of the argument
         sequence. */
        YarpString ArgumentsToArgString(const DescriptorVector & arguments);
        
        /*! @brief Generate the standard 'argument description' from an argument sequence.
         @param arguments The argument sequence.
         @param output The generated argument descriptions.
         @param minSpace The number of characters between the argument names and their
         descriptions. */
        void ArgumentsToDescriptionArray(const DescriptorVector & arguments,
                                         YarpStringVector &       output,
                                         const int                minSpace);
        
        /*! @brief Return the resulting argument values.
         @param arguments The argument sequence.
         @param sep The separator string between the argument values.
         @returns The argument values, separated by 'sep'. */
        YarpString CombineArguments(const DescriptorVector & arguments,
                                    const YarpString &       sep);

        /*! @brief Convert a string in '--args' format into an argument description.
         @param inString The string to be analyzed.
         @returns A newly allocated argument descriptor or @c NULL if the string is not valid. */
        BaseArgumentDescriptor * ConvertStringToArgument(const YarpString & inString);

        /*! @brief Update the arguments data from the parsed argument list.
         @param arguments The argument sequence.
         @param parseResult The parsed argument list.
         @returns @c true if the parsed argument list matches the argument sequence and @c false
         otherwise. */
        bool ProcessArguments(const DescriptorVector & arguments,
                              Option_::Parser &        parseResult);

        /*! @brief Prompt the user for the value of each of the arguments.
         @param arguments The argument sequence.
         @returns @c true if all arguments are valid and @c false otherwise. */
        bool PromptForValues(const DescriptorVector & arguments);
        
    } // Utilities
    
} // MplusM

#endif // ! defined(MpMBaseArgumentDescriptor_H_)
