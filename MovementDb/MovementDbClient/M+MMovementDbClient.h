//--------------------------------------------------------------------------------------------------
//
//  File:       M+MMovementDbClient.h
//
//  Project:    M+M
//
//  Contains:   The class declaration for the client of the movement database service.
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

#if (! defined(MpMMovementDbClient_H_))
# define MpMMovementDbClient_H_ /* Header guard */

# include <mpm/M+MBaseClient.h>

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 
 @brief The class declaration for the client of the movement database service. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace MplusM
{
    namespace MovementDb
    {
        /*! @brief A client for the request counter service. */
        class MovementDbClient : public Common::BaseClient
        {
        public:
            
            /*! @brief The constructor. */
            MovementDbClient(void);
            
            /*! @brief The destructor. */
            virtual ~MovementDbClient(void);
            
            /*! @brief Add a file entry to the backend database.
             @param emailAddress The e-mail address of the user that owns the file.
             @param dataTrack The data track for the file.
             @param filePath The filesystem path to the file.
             @returns @c true if the file entry was added successfully and @c false otherwise. */
            bool addFileToDb(const yarp::os::ConstString & emailAddress,
                             const yarp::os::ConstString & dataTrack,
                             const yarp::os::ConstString & filePath);
            
        protected:
            
        private:
            
            /*! @brief The class that this class is derived from. */
            typedef BaseClient inherited;
            
            /*! @brief Copy constructor.
             
             Note - not implemented and private, to prevent unexpected copying.
             @param other Another object to construct from. */
            MovementDbClient(const MovementDbClient & other);
            
            /*! @brief Assignment operator.
             
             Note - not implemented and private, to prevent unexpected copying.
             @param other Another object to construct from. */
            MovementDbClient & operator =(const MovementDbClient & other);
            
        }; // MovementDbClient
        
    } // MovementDb
    
} // MplusM

#endif // ! defined(MpMMovementDbClient_H_)
