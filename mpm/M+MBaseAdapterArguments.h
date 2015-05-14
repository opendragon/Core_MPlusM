//--------------------------------------------------------------------------------------------------
//
//  File:       mpm/M+MBaseAdapterArguments.h
//
//  Project:    M+M
//
//  Contains:   The class declaration for the minimal functionality required to gather the arguments
//              for an M+M adapter.
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
//  Created:    2015-05-14
//
//--------------------------------------------------------------------------------------------------

#if (! defined(MpMBaseAdapterArguments_H_))
# define MpMBaseAdapterArguments_H_ /* Header guard */

# include <mpm/M+MCommon.h>
# include <mpm/optionparser.h>

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 @brief The class declaration for the minimal functionality required to gather the arguments for an
 M+M adapter. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace MplusM
{
    namespace Common
    {
        /*! @brief The arguments for an M+M adapter. */
        class BaseAdapterArguments
        {
        public :
            
            /*! @brief The constructor.
             @param argList The command-line arguments for the adapter.
             @param argDescription A description of the command-line arguments for the adapter.
             */
            BaseAdapterArguments(const char * argList,
                             const char * argDescription);
            
            /*! @brief The destructor. */
            virtual ~BaseAdapterArguments(void);
            
            /*! @brief Return the description of the command-line arguments.
             @returns The description of the command-line arguments. */
            const YarpString & argumentDescription(void)
            const
            {
                return _argDescription;
            } // argumentDescription
            
            /*! @brief Return the command-line arguments.
             @returns The command-line arguments. */
            const YarpString & argumentList(void)
            const
            {
                return _argList;
            } // argumentList
            
            /*! @brief Return the resulting arguments.
             @param sep The separator string between the arguments.
             @returns The arguments, separated by 'sep'. */
            virtual YarpString combineArguments(const YarpString & sep) = 0;

            /*! @brief Update the arguments data from the parsed argument list.
             @param parseResult The parsed argument list. */
            virtual void processArguments(Option_::Parser & parseResult) = 0;
            
        protected :
            
        private :
            
            COPY_AND_ASSIGNMENT_(BaseAdapterArguments);
            
        public :
        
        protected :
        
        private :
            
            /*! @brief The description of the command-line arguments for the adapter. */
            YarpString _argDescription;
            
            /*! @brief The command-line arguments for the adapter. */
            YarpString _argList;
            
        }; // BaseAdapterArguments
        
    } // Common
    
} // MplusM

#endif // ! defined(MpMBaseAdapterArguments_H_)
