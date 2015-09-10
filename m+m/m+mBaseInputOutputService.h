//--------------------------------------------------------------------------------------------------
//
//  File:       m+m/m+mBaseInputOutputService.h
//
//  Project:    m+m
//
//  Contains:   The class declaration for the minimal functionality required for an m+m input/output
//              service.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2014 by H Plus Technologies Ltd. and Simon Fraser University.
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
//  Created:    2014-06-23
//
//--------------------------------------------------------------------------------------------------

#if (! defined(MpMBaseInputOutputService_H_))
# define MpMBaseInputOutputService_H_ /* Header guard */

# include <m+m/m+mBaseService.h>

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 @brief The class declaration for the minimal functionality required for an m+m input/output
 service. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

/*! @brief Declare the configure method, which updates the service configuration from its single
 argument, details.
 The method returns @c true if the service was successfully configured and @c false otherwise. */
# define DECLARE_CONFIGURE_ \
    virtual bool configure(const yarp::os::Bottle & details)

/*! @brief Declare the doIdle method, which is executed repeatedly once the service has been set
 up. */
# define DECLARE_DOIDLE_ \
    virtual void doIdle(void)

/*! @brief Declare the getConfiguration method, which retrieves the configuration of the input /
 output streams of the service.
 The method returns @c true if the configuration was successfully retrieved and @c false
 otherwise. */
# define DECLARE_GETCONFIGURATION_ \
    virtual bool getConfiguration(yarp::os::Bottle & details)

/*! @brief Declare the restartStreams method, which restarts the input / output streams of the
 service. */
# define DECLARE_RESTARTSTREAMS_ \
    virtual void restartStreams(void)

/*! @brief Declare the setUpClientStreams method, which sets up the client streams of the service.
 The method returns @c true if the channels were set up and @c false otherwise. */
# define DECLARE_SETUPCLIENTSTREAMS_ \
    virtual bool setUpClientStreams(void)

/*! @brief Declare the setUpInputStreams method, which sets up the input streams of the service.
 The method returns @c true if the channels were set up and @c false otherwise. */
# define DECLARE_SETUPINPUTSTREAMS_ \
    virtual bool setUpInputStreams(void)

/*! @brief Declare the setUpOutputStreams method, which sets up the output streams of the service.
 The method returns @c true if the channels were set up and @c false otherwise. */
# define DECLARE_SETUPOUTPUTSTREAMS_ \
    virtual bool setUpOutputStreams(void)

/*! @brief Declare the startStreams method, which sets up the descriptions of the input / output
 streams of the service.
 The method returns @c true if the descriptions were set up and @c false otherwise. */
# define DECLARE_SETUPSTREAMDESCRIPTIONS_ \
    virtual bool setUpStreamDescriptions(void)

/*! @brief Declare the shutDownClientStreams method, which shuts down the client streams of the
 service.
 The method returns @c true if the channels were shut down and @c false otherwise. */
# define DECLARE_SHUTDOWNCLIENTSTREAMS_ \
    virtual bool shutDownClientStreams(void)

/*! @brief Declare the shutDownInputStreams method, which shuts down the input streams of the
 service.
 The method returns @c true if the channels were shut down and @c false otherwise. */
# define DECLARE_SHUTDOWNINPUTSTREAMS_ \
    virtual bool shutDownInputStreams(void)

/*! @brief Declare the shutDownOutputStreams method, which shuts down the output streams of the
 service.
 The method returns @c true if the channels were shut down and @c false otherwise. */
# define DECLARE_SHUTDOWNOUTPUTSTREAMS_ \
    virtual bool shutDownOutputStreams(void)

/*! @brief Declare the startStreams method, which starts the input / output streams of the
 service. */
# define DECLARE_STARTSTREAMS_ \
    virtual void startStreams(void)

/*! @brief Declare the stopStreams method, which stops the input / output streams of the service. */
# define DECLARE_STOPSTREAMS_ \
    virtual void stopStreams(void)

/*! @brief Define the configure method. */
# define DEFINE_CONFIGURE_(class_) \
    bool class_::configure(const yarp::os::Bottle & details)

/*! @brief Define the doIdle method. */
# define DEFINE_DOIDLE_(class_) \
    void class_::doIdle(void)

/*! @brief Define the getConfiguration method. */
# define DEFINE_GETCONFIGURATION_(class_) \
    bool class_::getConfiguration(yarp::os::Bottle & details)

/*! @brief Define the restartStreams method. */
# define DEFINE_RESTARTSTREAMS_(class_) \
    void class_::restartStreams(void)

/*! @brief Define the setUpClientStreams method. */
# define DEFINE_SETUPCLIENTSTREAMS_(class_) \
    bool class_::setUpClientStreams(void)

/*! @brief Define the setUpInputStreams method. */
# define DEFINE_SETUPINPUTSTREAMS_(class_) \
    bool class_::setUpInputStreams(void)

/*! @brief Define the setUpOutputStreams method. */
# define DEFINE_SETUPOUTPUTSTREAMS_(class_) \
    bool class_::setUpOutputStreams(void)

/*! @brief Define the setUpStreamDescriptions method. */
# define DEFINE_SETUPSTREAMDESCRIPTIONS_(class_) \
    bool class_::setUpStreamDescriptions(void)

/*! @brief Define the shutDownClientStreams method. */
# define DEFINE_SHUTDOWNCLIENTSTREAMS_(class_) \
    bool class_::shutDownClientStreams(void)

/*! @brief Define the shutDownInputStreams method. */
# define DEFINE_SHUTDOWNINPUTSTREAMS_(class_) \
    bool class_::shutDownInputStreams(void)

/*! @brief Define the shutDownOutputStreams method. */
# define DEFINE_SHUTDOWNOUTPUTSTREAMS_(class_) \
    bool class_::shutDownOutputStreams(void)

/*! @brief Define the startStreams method. */
# define DEFINE_STARTSTREAMS_(class_) \
    void class_::startStreams(void)

/*! @brief Define the stopStreams method. */
# define DEFINE_STOPSTREAMS_(class_) \
    void class_::stopStreams(void)

namespace MplusM
{
    namespace Common
    {
        class ArgumentDescriptionsRequestHandler;
        class ClientChannel;
        class ConfigurationRequestHandler;
        class ConfigureRequestHandler;
        class GeneralChannel;
        class RestartStreamsRequestHandler;
        class StartStreamsRequestHandler;
        class StopStreamsRequestHandler;
        
        /*! @brief An input/output service. */
        class BaseInputOutputService : public Common::BaseService
        {
        public :
            
            /*! @brief The constructor.
             @param argumentList Descriptions of the arguments to the executable.
             @param theKind The behavioural model for the service.
             @param launchPath The command-line name used to launch the service.
             @param argc The number of arguments in 'argv'.
             @param argv The arguments passed to the executable used to launch the service.
             @param tag The modifier for the service name.
             @param useMultipleHandlers @c true if simultaneous handlers are allowed, @c false if
             one handler is used.
             @param canonicalName The channel-independent name of the service.
             @param description The description of the service.
             @param requestsDescription The description of the requests for the service.
             @param serviceEndpointName The YARP name to be assigned to the new service.
             @param servicePortNumber The channel being used by the service. */
            BaseInputOutputService(const Utilities::DescriptorVector & argumentList,
                                   const ServiceKind                   theKind,
                                   const YarpString &                  launchPath,
                                   const int                           argc,
                                   char * *                            argv,
                                   const YarpString &                  tag,
                                   const bool                          useMultipleHandlers,
                                   const YarpString &                  canonicalName,
                                   const YarpString &                  description,
                                   const YarpString &                  requestsDescription,
                                   const YarpString &                  serviceEndpointName,
                                   const YarpString &                  servicePortNumber = "");
            
            /*! @brief The destructor. */
            virtual ~BaseInputOutputService(void);
            
            /*! @fn virtual bool configure(const yarp::os::Bottle & details)
             @brief Configure the input/output streams.
             @param details The configuration information for the input/output streams.
             @returns @c true if the service was successfully configured and @c false otherwise. */
            DECLARE_CONFIGURE_ = 0;
            
            /*! @fn virtual void doIdle(void)
             @brief Declare the doIdle method, which is executed repeatedly once the service has
             been set up. */
            DECLARE_DOIDLE_;

            DECLARE_DISABLEMETRICS_;
            
            DECLARE_ENABLEMETRICS_;
            
            DECLARE_GATHERMETRICS_;

            /*! @brief Returns the descriptions of the arguments to the application.
             @returns The descriptions of the arguments to the application. */
            inline const Utilities::DescriptorVector & getArgumentDescriptions(void)
            const
            {
                return _argumentList;
            } // getArgumentDescriptions

            /*! @brief Returns the number of client streams.
             @returns The number of client streams. */
            size_t getClientCount(void)
            const;
            
            /*! @brief Returns a specific client stream.
             @param index The zero-origin index of the client stream.
             @returns The client stream at the specified index. */
            ClientChannel * getClientStream(const size_t index)
            const;
            
            /*! @fn virtual bool getConfiguration(yarp::os::Bottle & details)
             @brief Get the configuration of the input/output streams.
             @param details The configuration information for the input/output streams.
             @returns @c true if the configuration was successfully retrieved and @c false
             otherwise. */
            DECLARE_GETCONFIGURATION_ = 0;

            /*! @brief Returns the number of input streams.
             @returns The number of input streams. */
            size_t getInletCount(void)
            const;
  
            /*! @brief Returns a specific input stream.
             @param index The zero-origin index of the input stream.
             @returns The input stream at the specified index. */
            GeneralChannel * getInletStream(const size_t index)
            const;
            
            /*! @brief Returns the number of output streams.
             @returns The number of output streams. */
            size_t getOutletCount(void)
            const;
            
            /*! @brief Returns a specific output stream.
             @param index The zero-origin index of the output stream.
             @returns The output stream at the specified index. */
            GeneralChannel * getOutletStream(const size_t index)
            const;
            
            /*! @brief Return @c true if the streams are processing data and @c false otherwise.
             @returns @c true if the streams are processing data and @c false otherwise. */
            inline bool isActive(void)
            const
            {
                return _active;
            } // isActive
            
            /*! @brief Start the service and set up its configuration.
             @param helpText The help text to be displayed.
             @param goWasSet @c true if the service is to be started immediately.
             @param stdinAvailable @c true if running in the foreground and @c false otherwise.
             @param reportOnExit @c true if service metrics are to be reported on exit and @c false
             otherwise. */
            void performLaunch(const YarpString & helpText,
                               const bool         goWasSet,
                               const bool         stdinAvailable,
                               const bool         reportOnExit);
            
            /*! @fn virtual void restartStreams(void)
             @brief Restart the input / output streams. */
            DECLARE_RESTARTSTREAMS_ = 0;
            
            DECLARE_STARTSERVICE_;
            
            /*! @fn virtual void startStreams(void)
             @brief Start the input / output streams. */
            DECLARE_STARTSTREAMS_ = 0;
            
            DECLARE_STOPSERVICE_;
            
            /*! @fn virtual void stopStreams(void)
             @brief Stop the input / output streams. */
            DECLARE_STOPSTREAMS_ = 0;
            
        protected :
            
            /*! @brief A set of client channels. */
            typedef std::vector<ClientChannel *> ClientChannelVector;

            /*! @brief A set of general channels. */
            typedef std::vector<GeneralChannel *> GeneralChannelVector;
            
            /*! @brief Add a set of client channels from a set of descriptions.
             @param descriptions The descriptions of the channels.
             @returns @c true if the channels were constructed and @c false otherwise. */
            bool addClientStreamsFromDescriptions(const ChannelVector & descriptions);
            
            /*! @brief Add a set of input channels from a set of descriptions.
             @param descriptions The descriptions of the channels.
             @returns @c true if the channels were constructed and @c false otherwise. */
            bool addInStreamsFromDescriptions(const ChannelVector & descriptions);
            
            /*! @brief Add a set of output channels from a set of descriptions.
             @param descriptions The descriptions of the channels.
             @returns @c true if the channels were constructed and @c false otherwise. */
            bool addOutStreamsFromDescriptions(const ChannelVector & descriptions);
            
            /*! @brief Indicate that the streams are not processing data. */
            inline void clearActive(void)
            {
                _active = false;
            } // clearActive
            
            /*! @brief If interactive, prompt for commands and then start the service. Otherwise,
             start the service immediately.
             @param helpText The help text to be displayed.
             @param forAdapter @c true if for an adapter and @c false for a service.
             @param goWasSet @c true if the service is to be started immediately.
             @param stdinAvailable @c true if running in the foreground and @c false otherwise.
             @param reportOnExit @c true if service metrics are to be reported on exit and @c false
             otherwise. */
            void runService(const YarpString & helpText,
                            const bool         forAdapter,
                            const bool         goWasSet,
                            const bool         stdinAvailable,
                            const bool         reportOnExit);

            /*! @brief Indicate that the streams are processing data. */
            inline void setActive(void)
            {
                _active = true;
            } // setActive

            /*! @brief Indicate that the service needs frequent calls to doIdle. */
            inline void setNeedsIdle(void)
            {
                _needsIdle = true;
            } // setNeedsIdle
            
            /*! @fn virtual bool setUpClientStreams(void)
             @brief Set up the client streams.
             @returns @c true if the channels were set up and @c false otherwise. */
            DECLARE_SETUPCLIENTSTREAMS_;

            /*! @fn virtual bool setUpInputStreams(void)
             @brief Set up the input streams.
             @returns @c true if the channels were set up and @c false otherwise. */
            DECLARE_SETUPINPUTSTREAMS_;
            
            /*! @fn virtual bool setUpOutputStreams(void)
             @brief Set up the output streams.
             @returns @c true if the channels were set up and @c false otherwise. */
            DECLARE_SETUPOUTPUTSTREAMS_;
            
            /*! @fn virtual bool setUpStreamDescriptions(void)
             @brief Set up the descriptions that will be used to construct the input / output
             streams.
             @returns @c true if the descriptions were set up and @c false otherwise. */
            DECLARE_SETUPSTREAMDESCRIPTIONS_ = 0;
            
            /*! @fn virtual bool shutDownClientStreams(void)
             @brief Shut down the client streams.
             @returns @c true if the channels were shut down and @c false otherwise. */
            DECLARE_SHUTDOWNCLIENTSTREAMS_;

            /*! @fn virtual bool shutDownInputStreams(void)
             @brief Shut down the input streams.
             @returns @c true if the channels were shut down and @c false otherwise. */
            DECLARE_SHUTDOWNINPUTSTREAMS_;
            
            /*! @fn virtual bool shutDownOutputStreams(void)
             @brief Shut down the output streams.
             @returns @c true if the channels were shut down and @c false otherwise. */
            DECLARE_SHUTDOWNOUTPUTSTREAMS_;
            
        private :
            
            /*! @brief Enable the standard request handlers. */
            void attachRequestHandlers(void);
            
            /*! @brief Disable the standard request handlers. */
            void detachRequestHandlers(void);
            
            DECLARE_FILLINSECONDARYCLIENTCHANNELSLIST_;
            
            DECLARE_FILLINSECONDARYINPUTCHANNELSLIST_;
            
            DECLARE_FILLINSECONDARYOUTPUTCHANNELSLIST_;
            
            COPY_AND_ASSIGNMENT_(BaseInputOutputService);
            
        public :
        
        protected :
        
        private :
            
            /*! @brief The class that this class is derived from. */
            typedef BaseService inherited;

            /*! @brief The set of client channels. */
            ClientChannelVector _clientStreams;
            
            /*! @brief The set of input channels. */
            GeneralChannelVector _inStreams;
            
            /*! @brief The set of output channels. */
            GeneralChannelVector _outStreams;

            /*! @brief The descriptions of the arguments to be filled in by a calling application.
             */
            const Utilities::DescriptorVector & _argumentList;

            /*! @brief The request handler for the 'argumentDescriptions' request. */
            ArgumentDescriptionsRequestHandler * _argumentDescriptionsHandler;

            /*! @brief The request handler for the 'configuration' request. */
            ConfigurationRequestHandler * _configurationHandler;

            /*! @brief The request handler for the 'configure' request. */
            ConfigureRequestHandler * _configureHandler;
            
            /*! @brief The request handler for the 'restartStreams' request. */
            RestartStreamsRequestHandler * _restartStreamsHandler;
            
            /*! @brief The request handler for the 'startStreams' request. */
            StartStreamsRequestHandler * _startStreamsHandler;
            
            /*! @brief The request handler for the 'stopStreams' request. */
            StopStreamsRequestHandler * _stopStreamsHandler;
            
            /*! @brief @c true if the streams are processing data and @c false otherwise. */
            bool _active;

            /*! @brief @c true if the service needs frequent calls to doIdle and @c false
             otherwise. */
            bool _needsIdle;

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunused-private-field"
# endif // defined(__APPLE__)
            /*! @brief Filler to pad to alignment boundary */
            char _filler[6];
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)
            
        }; // BaseInputOutputService

    } // Common
    
} // MplusM

#endif // ! defined(MpMBaseInputOutputService_H_)
