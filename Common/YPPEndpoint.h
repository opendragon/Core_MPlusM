//--------------------------------------------------------------------------------------
//
//  File:       YPPEndpoint.h
//
//  Project:    YarpPlusPlus
//
//  Contains:   The class declaration for the connection endpoint for a Yarp++ service.
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
//  Created:    2014-02-07
//
//--------------------------------------------------------------------------------------

#if (! defined(YPPENDPOINT_H_))
# define YPPENDPOINT_H_ /* */

# include "YPPInputHandlerCreator.h"
# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wc++11-extensions"
#  pragma clang diagnostic ignored "-Wdocumentation"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#  pragma clang diagnostic ignored "-Wpadded"
#  pragma clang diagnostic ignored "-Wshadow"
#  pragma clang diagnostic ignored "-Wunused-parameter"
#  pragma clang diagnostic ignored "-Wweak-vtables"
# endif // defined(__APPLE__)
# include <yarp/os/Port.h>
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace YarpPlusPlus
{
    /*! @brief An object that represents an endpoint that provides a bidirectional connection for services and clients.
     */
    class Endpoint
    {
    public:
        
        /*! @brief The constructor.
         @param endpointName The YARP name to be assigned to the new endpoint.
         @param hostName The name or IP address of the machine holding the endpoint.
         @param portNumber The port being used by the endpoint. */
        Endpoint(const yarp::os::ConstString & endpointName,
                 const yarp::os::ConstString & hostName = "",
                 const yarp::os::ConstString & portNumber = "");
        
        /*! @brief The destructor. */
        virtual ~Endpoint(void);
        
        /*! @brief Stop processing input. */
        void close(void);
        
        /*! @brief Return the YARP name for the endpoint.
         @returns The YARP name for the endpoint. */
        yarp::os::ConstString getName(void)
        const;
        
        /*! @brief Return the state of the endpoint.
         @returns @c true if the endpoint is open and @c false otherwise. */
        inline bool isOpen(void)
        const
        {
            return _isOpen;
        } // isOpen
        
        /*! @brief Open the endpoint if it is not already open.
         @returns @c true if the endpoint is open and @c false otherwise. */
        bool open(void);
        
        /*! @brief Set the input handler for the endpoint.
         
         Either an input handler or an input handler creator must be set up before any incoming data will be processed
         and the endpoint cannot be open before set up.
         @param handler The input handler to be used by the endpoint to process incoming data.
         @returns @c true if the input handler was attached to the endpoint. */
        bool setInputHandler(InputHandler & handler);
        
        /*! @brief Set the input handler creator for the endpoint.

         Either an input handler or an input handler creator must be set up before any incoming data will be processed
         and the endpoint cannot be open before set up.
         @param handlerCreator The input handler creator to be used by the endpoint to process incoming data.
         @returns @c true if the input handler creator was attached to the endpoint. */
        bool setInputHandlerCreator(InputHandlerCreator & handlerCreator);

        /*! @brief Set the port activity reporter for the endpoint.
         @param reporter The port activity reporter to be used by the endpoint.
         @param andReportNow @c true if the port activity reporter is to be activated immediately.
         @returns @c true if the port activity reporter was attached to the endpoint. */
        bool setReporter(yarp::os::PortReport & reporter,
                         const bool             andReportNow = false);
        
        /*! @brief Set the endpoint timeout; must be called between creating an endpoint and opening it.
         @param timeout The number of seconds to wait; if negative, wait forever.
         @returns @c true if the timeou was set and @c false otherwise. */
        bool setTimeout(const float timeout);
        
        /*! @brief Check the format of an endpoint name.
         @param portName The name to be checked.
         @returns @c true if the name is a valid endpoint name and @c false otherwise. */
        static bool CheckEndpointName(const yarp::os::ConstString & portName);

    protected:
        
    private:
        
        /*! @brief Copy constructor.

         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        Endpoint(const Endpoint & other);
        
        /*! @brief Assignment operator.

         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        Endpoint & operator=(const Endpoint & other);
        
        /*! @brief The connection details for the endpoint. */
        yarp::os::Contact     _contact;
        
        /*! @brief The input handler for the endpoint. */
        InputHandler *        _handler;
        
        /*! @brief The input handler creator for the endpoint. */
        InputHandlerCreator * _handlerCreator;
        
        /*! @brief The YARP port to be used by the endpoint. */
        yarp::os::Port *      _port;
        
        /*! @brief @c true if the endpoint is open and @c false otherwise. */
        bool                  _isOpen;
        
# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunused-private-field"
# endif // defined(__APPLE__)
        /*! @brief Filler to pad to alignment boundary */
        char                  _filler[7];
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)
        
    }; // Endpoint
    
} // YarpPlusPlus

#endif // ! defined(YPPENDPOINT_H_)
