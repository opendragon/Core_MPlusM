//--------------------------------------------------------------------------------------
//
//  File:       MoMeEchoClient.h
//
//  Project:    MoAndMe
//
//  Contains:   The class declaration for the client of a simple MoAndMe service.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2014 by HPlus Technologies Ltd. and Simon Fraser University.
//
//              All rights reserved. Redistribution and use in source and binary forms,
//              with or without modification, are permitted provided that the following
//              conditions are met:
//                * Redistributions of source code must retain the above copyright
//                  notice, this list of conditions and the following disclaimer.
//                * Redistributions in binary form must reproduce the above copyright
//                  notice, this list of conditions and the following disclaimer in the
//                  documentation and/or other materials provided with the
//                  distribution.
//                * Neither the name of the copyright holders nor the names of its
//                  contributors may be used to endorse or promote products derived
//                  from this software without specific prior written permission.
//
//              THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
//              "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
//              LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
//              PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
//              OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
//              SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
//              LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
//              DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
//              THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
//              (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
//              OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//
//  Created:    2014-02-06
//
//--------------------------------------------------------------------------------------

#if (! defined(MOMEECHOCLIENT_H_))
/*! @brief Header guard. */
# define MOMEECHOCLIENT_H_ /* */

# include "MoMeBaseClient.h"

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 
 @brief The class declaration for the client of a simple MoAndMe service. */

/*! @dir examples
 @brief The set of files that demonstrate features of the MoAndMe framework. */

/*! @namespace MoAndMeExample
 @brief A set of example classes using features from MoAndMe. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace MoAndMeExample
{
    /*! @brief An example MoAndMe client, for the 'echo' service. */
    class EchoClient : public MoAndMe::BaseClient
    {
    public:
        
        /*! @brief The constructor. */
        EchoClient(void);
        
        /*! @brief The destructor. */
        virtual ~EchoClient(void);
        
        /*! @brief Send a string to the service and retrieve it back from the service.
         @param outgoing The string to send to the service.
         @param incoming The returned string from the service.
         @returns @c true if the string was retrieved successfully and @c false otherwise. */
        bool sendAndReceive(const yarp::os::ConstString & outgoing,
                            yarp::os::ConstString &       incoming);

    protected:
        
    private:
        
        /*! @brief The class that this class is derived from. */
        typedef BaseClient inherited;

        /*! @brief Copy constructor.
         
         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        EchoClient(const EchoClient & other);
        
        /*! @brief Assignment operator.
         
         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        EchoClient & operator=(const EchoClient & other);
        
    }; // EchoClient
    
} // MoAndMeExample

#endif // ! defined(MOMEECHOCLIENT_H_)
