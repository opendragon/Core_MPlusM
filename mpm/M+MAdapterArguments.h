//--------------------------------------------------------------------------------------------------
//
//  File:       mpm/M+MAdapterArguments.h
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

#if (! defined(MpMAdapterArguments_H_))
# define MpMAdapterArguments_H_ /* Header guard */

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
        class AdapterArguments
        {
        public :
            
            /*! @brief The constructor.
             @param argList The command-line arguments for the adapter.
             @param argDescription A description of the command-line arguments for the adapter.
             */
            AdapterArguments(const char * argList,
                             const char * argDescription);
            
            /*! @brief The destructor. */
            virtual ~AdapterArguments(void);
            
            /*! @brief Return the description of the command-line arguments.
             @returns The description of the command-line arguments. */
            const yarp::os::ConstString & argumentDescription(void)
            const
            {
                return _argDescription;
            } // argumentDescription
            
            /*! @brief Return the command-line arguments.
             @returns The command-line arguments. */
            const yarp::os::ConstString & argumentList(void)
            const
            {
                return _argList;
            } // argumentList
            
            /*! @brief Return the resulting arguments.
             @param sep The separator string between the arguments.
             @returns The arguments, separated by 'sep'. */
            virtual yarp::os::ConstString combineArguments(const yarp::os::ConstString & sep) = 0;

            /*! @brief Update the arguments data from the parsed argument list.
             @param parseResult The parsed argument list. */
            virtual void processArguments(Option_::Parser & parseResult) = 0;
            
        protected :
            
        private :
            
            COPY_AND_ASSIGNMENT_(AdapterArguments);
            
        public :
        
        protected :
        
        private :
            
            /*! @brief The description of the command-line arguments for the adapter. */
            yarp::os::ConstString _argDescription;
            
            /*! @brief The command-line arguments for the adapter. */
            yarp::os::ConstString _argList;
            
        }; // AdapterArguments
        
        /*! @brief The arguments for an M+M adapter that takes up to one arguments. */
        class AdapterOneArgument : public AdapterArguments
        {
        public :
            
            /*! @brief The constructor.
             @param argList The command-line arguments for the adapter.
             @param argDescription A description of the command-line arguments for the adapter.
             @param defaultFirstArgument The value to be returned if the first argument is not
             present.
             @param firstArgument The resulting first argument.
             */
            AdapterOneArgument(const char *                  argList,
                               const char *                  argDescription,
                               const yarp::os::ConstString & defaultFirstArgument,
                               yarp::os::ConstString &       firstArgument);
            
            /*! @brief The destructor. */
            virtual ~AdapterOneArgument(void);
            
            /*! @brief Return the resulting arguments.
             @param sep The separator string between the arguments.
             @returns The arguments, separated by 'sep'. */
            virtual yarp::os::ConstString combineArguments(const yarp::os::ConstString & sep);

            /*! @brief Update the arguments data from the parsed argument list.
             @param parseResult The parsed argument list. */
            virtual void processArguments(Option_::Parser & parseResult);
            
        protected :
        
        private :
            
            COPY_AND_ASSIGNMENT_(AdapterOneArgument);
            
        public :
        
        protected :
        
        private :
            
            /*! @brief The class that this class is derived from. */
            typedef AdapterArguments inherited;
            
            /*! @brief The first argument. */
            yarp::os::ConstString & _firstArgument;
            
        }; // AdapterOneArgument
        
        /*! @brief The arguments for an M+M adapter that takes up to two arguments. */
        class AdapterTwoArguments : public AdapterArguments
        {
        public :
            
            /*! @brief The constructor.
             @param argList The command-line arguments for the adapter.
             @param argDescription A description of the command-line arguments for the adapter.
             @param defaultFirstArgument The value to be returned if the first argument is not
             present.
             @param defaultSecondArgument The value to be returned if the second argument is not
             present.
             @param firstArgument The resulting first argument.
             @param secondArgument The resulting second argument.
             */
            AdapterTwoArguments(const char *                  argList,
                                const char *                  argDescription,
                                const yarp::os::ConstString & defaultFirstArgument,
                                const yarp::os::ConstString & defaultSecondArgument,
                                yarp::os::ConstString &       firstArgument,
                                yarp::os::ConstString &       secondArgument);
            
            /*! @brief The destructor. */
            virtual ~AdapterTwoArguments(void);
            
            /*! @brief Return the resulting arguments.
             @param sep The separator string between the arguments.
             @returns The arguments, separated by 'sep'. */
            virtual yarp::os::ConstString combineArguments(const yarp::os::ConstString & sep);
            
            /*! @brief Update the arguments data from the parsed argument list.
             @param parseResult The parsed argument list. */
            virtual void processArguments(Option_::Parser & parseResult);
            
        protected :
        
        private :
            
            COPY_AND_ASSIGNMENT_(AdapterTwoArguments);
            
        public :
        
        protected :
        
        private :
            
            /*! @brief The class that this class is derived from. */
            typedef AdapterArguments inherited;
            
            /*! @brief The first argument. */
            yarp::os::ConstString & _firstArgument;
            
            /*! @brief The second argument. */
            yarp::os::ConstString & _secondArgument;
            
        }; // AdapterTwoArguments
        
        /*! @brief The arguments for an M+M adapter that takes up to three arguments. */
        class AdapterThreeArguments : public AdapterArguments
        {
        public :
            
            /*! @brief The constructor.
             @param argList The command-line arguments for the adapter.
             @param argDescription A description of the command-line arguments for the adapter.
             @param defaultFirstArgument The value to be returned if the first argument is not
             present.
             @param defaultSecondArgument The value to be returned if the second argument is not
             present.
             @param defaultThirdArgument The value to be returned if the third argument is not
             present.
             @param firstArgument The resulting first argument.
             @param secondArgument The resulting second argument.
             @param thirdArgument The resulting third argument.
             */
            AdapterThreeArguments(const char *                  argList,
                                  const char *                  argDescription,
                                  const yarp::os::ConstString & defaultFirstArgument,
                                  const yarp::os::ConstString & defaultSecondArgument,
                                  const yarp::os::ConstString & defaultThirdArgument,
                                  yarp::os::ConstString &       firstArgument,
                                  yarp::os::ConstString &       secondArgument,
                                  yarp::os::ConstString &       thirdArgument);
            
            /*! @brief The destructor. */
            virtual ~AdapterThreeArguments(void);
            
            /*! @brief Return the resulting arguments.
             @param sep The separator string between the arguments.
             @returns The arguments, separated by 'sep'. */
            virtual yarp::os::ConstString combineArguments(const yarp::os::ConstString & sep);
            
            /*! @brief Update the arguments data from the parsed argument list.
             @param parseResult The parsed argument list. */
            virtual void processArguments(Option_::Parser & parseResult);
            
        protected :
        
        private :
            
            COPY_AND_ASSIGNMENT_(AdapterThreeArguments);
            
        public :
        
        protected :
        
        private :
            
            /*! @brief The class that this class is derived from. */
            typedef AdapterArguments inherited;
            
            /*! @brief The first argument. */
            yarp::os::ConstString & _firstArgument;
            
            /*! @brief The second argument. */
            yarp::os::ConstString & _secondArgument;
            
            /*! @brief The third argument. */
            yarp::os::ConstString & _thirdArgument;
            
        }; // AdapterThreeArguments
        
        /*! @brief The arguments for an M+M adapter that takes up to four arguments. */
        class AdapterFourArguments : public AdapterArguments
        {
        public :
            
            /*! @brief The constructor.
             @param argList The command-line arguments for the adapter.
             @param argDescription A description of the command-line arguments for the adapter.
             @param defaultFirstArgument The value to be returned if the first argument is not
             present.
             @param defaultSecondArgument The value to be returned if the second argument is not
             present.
             @param defaultThirdArgument The value to be returned if the third argument is not
             present.
             @param defaultFourthArgument The value to be returned if the third argument is not
             present.
             @param firstArgument The resulting first argument.
             @param secondArgument The resulting second argument.
             @param thirdArgument The resulting third argument.
             @param fourthArgument The resulting fourth argument.
             */
            AdapterFourArguments(const char *                  argList,
                                 const char *                  argDescription,
                                 const yarp::os::ConstString & defaultFirstArgument,
                                 const yarp::os::ConstString & defaultSecondArgument,
                                 const yarp::os::ConstString & defaultThirdArgument,
                                 const yarp::os::ConstString & defaultFourthArgument,
                                 yarp::os::ConstString &       firstArgument,
                                 yarp::os::ConstString &       secondArgument,
                                 yarp::os::ConstString &       thirdArgument,
                                 yarp::os::ConstString &       fourthArgument);
            
            /*! @brief The destructor. */
            virtual ~AdapterFourArguments(void);
            
            /*! @brief Return the resulting arguments.
             @param sep The separator string between the arguments.
             @returns The arguments, separated by 'sep'. */
            virtual yarp::os::ConstString combineArguments(const yarp::os::ConstString & sep);
            
            /*! @brief Update the arguments data from the parsed argument list.
             @param parseResult The parsed argument list. */
            virtual void processArguments(Option_::Parser & parseResult);
            
        protected :
        
        private :
            
            COPY_AND_ASSIGNMENT_(AdapterFourArguments);
            
        public :
        
        protected :
        
        private :
            
            /*! @brief The class that this class is derived from. */
            typedef AdapterArguments inherited;
            
            /*! @brief The first argument. */
            yarp::os::ConstString & _firstArgument;
            
            /*! @brief The second argument. */
            yarp::os::ConstString & _secondArgument;
            
            /*! @brief The third argument. */
            yarp::os::ConstString & _thirdArgument;
            
            /*! @brief The fourth argument. */
            yarp::os::ConstString & _fourthArgument;
            
        }; // AdapterFourArguments
        
    } // Common
    
} // MplusM

#endif // ! defined(MpMAdapterArguments_H_)
