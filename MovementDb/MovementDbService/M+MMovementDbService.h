//--------------------------------------------------------------------------------------------------
//
//  File:       M+MMovementDbService.h
//
//  Project:    M+M
//
//  Contains:   The class declaration for a service that collects statistic on requests.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2014 by HPlus Technologies Ltd. and Simon Fraser University.
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
//  Created:    2014-09-02
//
//--------------------------------------------------------------------------------------------------

#if (! defined(MpMMovementDbService_H_))
# define MpMMovementDbService_H_ /* Header guard */

# include <mpm/M+MBaseService.h>

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 
 @brief The class declaration for a service that collects statistic on requests. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

/*! @brief The channel name to use for the service if not provided. */
# define DEFAULT_MOVEMENTDB_SERVICE_NAME T_(DEFAULT_SERVICE_NAME_BASE "movementdb")

namespace MplusM
{
    namespace MovementDb
    {
        class AddFileRequestHandler;
        class SetDataTrackRequestHandler;
        class SetEmailRequestHandler;
        class StopDbRequestHandler;
        
        /*! @brief The request counter service. */
        class MovementDbService : public Common::BaseService
        {
        public :
            
            /*! @brief The constructor.
             @param launchPath The command-line name used to launch the service.
             @param tag The modifier for the service name and port names.
             @param databaseServerAddress The IP address of the database server.
             @param serviceEndpointName The YARP name to be assigned to the new service.
             @param servicePortNumber The port being used by the service. */
            MovementDbService(const yarp::os::ConstString & launchPath,
                              const yarp::os::ConstString & tag,
                              const yarp::os::ConstString & databaseServerAddress,
                              const yarp::os::ConstString & serviceEndpointName,
                              const yarp::os::ConstString & servicePortNumber = "");
            
            /*! @brief The destructor. */
            virtual ~MovementDbService(void);
            
            /*! @brief Add a file entry to the backend database.
             @param key The client-provided key.
             @param filePath The filesystem path to the file.
             @returns @c true if the file entry was added successfully and @c false otherwise. */
            bool addFileToDb(const yarp::os::ConstString & key,
                             const yarp::os::ConstString & filePath);
            
            /*! @brief Set the active data track.
             @param key The client-provided key.
             @param dataTrack The data track to use with subsequent files.
             @returns @c true if the data track was successfully set and @c false otherwise. */
            bool setDataTrack(const yarp::os::ConstString & key,
                              const yarp::os::ConstString & dataTrack);

            /*! @brief Set the active e-mail address.
             @param key The client-provided key.
             @param emailAddress The e-mail address of the user that will own subsequent files.
             @returns @c true if the e-mail address was successfully set and @c false otherwise. */
            bool setEmailAddress(const yarp::os::ConstString & key,
                                 const yarp::os::ConstString & emailAddress);

            /*! @brief Start processing requests.
             @returns @c true if the service was started and @c false if it was not. */
            virtual bool start(void);
            
            /*! @brief Stop processing requests.
             @returns @c true if the service was stopped and @c false it if was not. */
            virtual bool stop(void);
            
        protected :
            
        private :
            
            COPY_AND_ASSIGNMENT_(MovementDbService);
            
            /*! @brief Enable the standard request handlers. */
            void attachRequestHandlers(void);
            
            /*! @brief Disable the standard request handlers. */
            void detachRequestHandlers(void);
            
        public :
        
        protected :
        
        private :
            
            /*! @brief The class that this class is derived from. */
            typedef BaseService inherited;
            
            /*! @brief The IP address of the backend database server. */
            yarp::os::ConstString _databaseAddress;
            
            /*! @brief The request handler for the 'addfile' request. */
            AddFileRequestHandler * _addFileHandler;
            
            /*! @brief The request handler for the 'setdatatrack' request. */
            SetDataTrackRequestHandler * _setDataTrackHandler;
            
            /*! @brief The request handler for the 'setemail' request. */
            SetEmailRequestHandler * _setEmailHandler;
            
            /*! @brief The request handler for the 'stop' request. */
            StopDbRequestHandler * _stopDbHandler;
            
        }; // MovementDbService
        
    } // MovementDb
    
} // MplusM

#endif // ! defined(MpMMovementDbService_H_)
