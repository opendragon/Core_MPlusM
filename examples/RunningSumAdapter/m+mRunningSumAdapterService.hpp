//--------------------------------------------------------------------------------------------------
//
//  File:       m+mRunningSumAdapterService.hpp
//
//  Project:    m+m
//
//  Contains:   The class declaration for the Running Sum adapter service.
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
//  Created:    2015-05-27
//
//--------------------------------------------------------------------------------------------------

#if (! defined(MpMRunningSumAdapterService_HPP_))
# define MpMRunningSumAdapterService_HPP_ /* Header guard */

# include <m+m/m+mBaseAdapterService.hpp>

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 @brief The class declaration for the Running Sum adapter service. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

/*! @brief The base channel name to use for the service if not provided. */
# define DEFAULT_RUNNINGSUMADAPTER_SERVICE_NAME_ BUILD_NAME_(MpM_ADAPTER_BASE_NAME_, \
                                                             BUILD_NAME_("examples", "runningsum"))

/*! @brief The description of the service. */
# define RUNNINGSUMADAPTER_SERVICE_DESCRIPTION_ T_("Running Sum adapter")

namespace MplusM
{
    namespace Example
    {
        class RunningSumAdapterData;
        class RunningSumControlInputHandler;
        class RunningSumDataInputHandler;

        /*! @brief The Random Number adapter service. */
        class RunningSumAdapterService : public Common::BaseAdapterService
        {
        public :

        protected :

        private :

            /*! @brief The class that this class is derived from. */
            typedef BaseAdapterService inherited;

        public :

            /*! @brief The constructor.
             @param[in] argumentList Descriptions of the arguments to the executable.
             @param[in] launchPath The command-line name used to launch the service.
             @param[in] argc The number of arguments in 'argv'.
             @param[in] argv The arguments passed to the executable used to launch the service.
             @param[in] tag The modifier for the service name and port names.
             @param[in] serviceEndpointName The YARP name to be assigned to the new service.
             @param[in] servicePortNumber The port being used by the service. */
            RunningSumAdapterService(const Utilities::DescriptorVector & argumentList,
                                     const YarpString &                  launchPath,
                                     const int                           argc,
                                     char * *                            argv,
                                     const YarpString &                  tag,
                                     const YarpString &                  serviceEndpointName,
                                     const YarpString &                  servicePortNumber = "");

            /*! @brief The destructor. */
            virtual
            ~RunningSumAdapterService(void);

            /*! @brief Turn off the send / receive metrics collecting. */
            virtual void
            disableMetrics(void);

            /*! @brief Turn on the send / receive metrics collecting. */
            virtual void
            enableMetrics(void);

            /*! @brief Set up the input handlers.
             @param[in,out] sharedData The shared data for the input handlers. */
            void
            setUpInputHandlers(RunningSumAdapterData & sharedData);

            /*! @brief Start the input / output streams. */
            virtual void
            startStreams(void);

            /*! @brief Stop the input / output streams. */
            virtual void
            stopStreams(void);

        protected :

        private :

            /*! @brief The copy constructor.
             @param[in] other The object to be copied. */
            RunningSumAdapterService(const RunningSumAdapterService & other);

            /*! @brief The assignment operator.
             @param[in] other The object to be copied.
             @return The updated object. */
            RunningSumAdapterService &
            operator =(const RunningSumAdapterService & other);

            /*! @brief Set up the descriptions that will be used to construct the input / output
             streams.
             @return @c true if the descriptions were set up and @c false otherwise. */
            virtual bool
            setUpStreamDescriptions(void);

        public :

        protected :

        private :

            /*! @brief The handler for control input data. */
            RunningSumControlInputHandler * _controlHandler;

            /*! @brief The handler for data input data. */
            RunningSumDataInputHandler * _dataHandler;

        }; // RunningSumAdapterService

    } // Example

} // MplusM

#endif // ! defined(MpMRunningSumAdapterService_HPP_)
