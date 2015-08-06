//--------------------------------------------------------------------------------------------------
//
//  File:       m+mJavaScriptService.h
//
//  Project:    m+m
//
//  Contains:   The class declaration for the JavaScript input / output service.
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
//  Created:    2015-01-05
//
//--------------------------------------------------------------------------------------------------

#if (! defined(MpMJavaScriptService_H_))
# define MpMJavaScriptService_H_ /* Header guard */

# include "m+mJavaScriptCommon.h"

# include <m+m/m+mBaseFilterService.h>

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 @brief The class declaration for the %JavaScript input / output service. */

/*! @namespace MplusM::JavaScript
 @brief A set of classes to support executing %JavaScript in an m+m installation. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

/*! @brief The base channel name to use for the service if not provided. */
# define DEFAULT_JAVASCRIPT_SERVICE_NAME_ T_(DEFAULT_SERVICE_NAME_BASE_ "filter/javascript")

/*! @brief The description of the service. */
# define JAVASCRIPTFILTER_SERVICE_DESCRIPTION_ T_("JavaScript filter service")

struct JSContext;

namespace MplusM
{
    namespace JavaScript
    {
        class JavaScriptInputHandler;
        class JavaScriptThread;
        
        /*! @brief The %JavaScript input / output service. */
        class JavaScriptService : public Common::BaseFilterService
        {
        public :
            
            /*! @brief The constructor.
             @param argumentList Descriptions of the arguments to the executable.
             @param context The %JavaScript engine context.
             @param global The %JavaScript global object.
             @param launchPath The command-line name used to launch the service.
             @param argc The number of arguments in 'argv'.
             @param argv The arguments passed to the executable used to launch the service.
             @param tag The modifier for the service name and port names.
             @param description The description from the active script.
             @param loadedInletDescriptions The list of loaded inlet stream descriptions.
             @param loadedOutletDescriptions The list of loaded outlet stream descriptions.
             @param loadedInletHandlers The list of loaded inlet handlers.
             @param loadedStartingFunction The function to execute on starting the service streams.
             @param loadedStoppingFunction The function to execute on stopping the service streams.
             @param sawThread @c true if a thread function was defined.
             @param loadedThreadFunction The function to execute on an output-generating thread.
             @param loadedInterval The interval (in seconds) between executions of the
             output-generating thread.
             @param serviceEndpointName The YARP name to be assigned to the new service.
             @param servicePortNumber The port being used by the service. */
            JavaScriptService(const Utilities::DescriptorVector & argumentList,
                              JSContext *                         context,
                              JS::RootedObject &                  global,
                              const YarpString &                  launchPath,
                              const int                           argc,
                              char * *                            argv,
                              const YarpString &                  tag,
                              const YarpString &                  description,
                              const Common::ChannelVector &       loadedInletDescriptions,
                              const Common::ChannelVector &       loadedOutletDescriptions,
                              const JS::AutoValueVector &         loadedInletHandlers,
                              const JS::RootedValue &             loadedStartingFunction,
                              const JS::RootedValue &             loadedStoppingFunction,
                              const bool                          sawThread,
                              const JS::RootedValue &             loadedThreadFunction,
                              const double                        loadedInterval,
                              const YarpString &                  serviceEndpointName,
                              const YarpString &                  servicePortNumber = "");
            
            /*! @brief The destructor. */
            virtual ~JavaScriptService(void);
            
            DECLARE_CONFIGURE_;
            
            DECLARE_GETCONFIGURATION_;

            /*! @brief Return the %JavaScript execution environment.
             @returns The %JavaScript execution environment. */
            JSContext * getContext(void)
            const
            {
                return _context;
            } // getContext
            
            /*! @brief Return the global object for the %JavaScript execution environment.
             @returns The global object for the %JavaScript execution environment. */
            JS::RootedObject & getGlobal(void)
            const
            {
                return _global;
            } // getGlobal
            
            DECLARE_RESTARTSTREAMS_;
            
            /*! @brief Send a value out a specified channel.
             @param channelSlot The output channel to be used.
             @param theData The value to be sent.
             @returns @c true if the data was successfully sent and @c false otherwise. */
            bool sendToChannel(const int32_t channelSlot,
                               JS::Value     theData);
            
            DECLARE_STARTSERVICE_;
            
            DECLARE_STARTSTREAMS_;
            
            DECLARE_STOPSERVICE_;
            
            DECLARE_STOPSTREAMS_;
            
        protected :
            
        private :
            
            COPY_AND_ASSIGNMENT_(JavaScriptService);
            
            /*! @brief Release all the allocated handlers. */
            void releaseHandlers(void);

            DECLARE_SETUPSTREAMDESCRIPTIONS_;
            
        public :
        
        protected :
        
        private :
            
            /*! @brief The class that this class is derived from. */
            typedef BaseFilterService inherited;
            
            /*! @brief A sequence of input handlers. */
            typedef std::vector<JavaScriptInputHandler *> HandlerVector;

            /*! @brief The handler functions to use for input. */
            JS::AutoValueVector _inletHandlers;
            
            /*! @brief The set of input handlers. */
            HandlerVector _inHandlers;
            
            /*! @brief The output thread to use. */
            JavaScriptThread * _generator;
            
            /*! @brief The %JavaScript execution environment. */
            JSContext * _context;
            
            /*! @brief The %JavaScript global object for this execution environment. */
            JS::RootedObject & _global;
            
            /*! @brief The list of loaded inlet stream descriptions. */
            const Common::ChannelVector & _loadedInletDescriptions;
            
            /*! @brief The list of loaded outlet stream descriptions. */
            const Common::ChannelVector & _loadedOutletDescriptions;
            
            /*! @brief The %JavaScript script starting function. */
            JS::RootedValue _scriptStartingFunc;
            
            /*! @brief The %JavaScript script stopping function. */
            JS::RootedValue _scriptStoppingFunc;
            
            /*! @brief The %JavaScript script thread function. */
            JS::RootedValue _scriptThreadFunc;
            
            /*! @brief The thread interval. */
            double _threadInterval;
            
            /*! @brief @c true if a thread is being used. */
            bool _isThreaded;
            
# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunused-private-field"
# endif // defined(__APPLE__)
            /*! @brief Filler to pad to alignment boundary */
            char _filler1[7];
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

        }; // JavaScriptService
        
        /*! @brief Print out a %JavaScript object.
         @param outStream Where to write the object.
         @param jct The %JavaScript engine context.
         @param anObject The object to be printed.
         @param depth The indentation level to be used.
         @returns The stream that was written to. */
        std::ostream & PrintJavaScriptObject(std::ostream &     outStream,
                                             JSContext *        jct,
                                             JS::RootedObject & anObject,
                                             const int          depth);
        
        /*! @brief Print out a value.
         @param outStream Where to write the value.
         @param jct The %JavaScript engine context.
         @param caption A title for the output.
         @param value The value to be printed.
         @param depth The indentation level to be used.
         @returns The stream that was written to. */
        std::ostream & PrintJavaScriptValue(std::ostream &    outStream,
                                            JSContext *       jct,
                                            const char *      caption,
                                            JS::RootedValue & value,
                                            const int         depth);

    } // JavaScript
    
} // MplusM

#endif // ! defined(MpMJavaScriptService_H_)
