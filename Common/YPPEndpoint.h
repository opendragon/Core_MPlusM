//
//  YPPEndpoint.h
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-07.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#if (! defined(YPPENDPOINT_H_))
# define YPPENDPOINT_H_ /* */

# include "YPPInputHandlerCreator.h"
# include <yarp/os/Port.h>

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
        
        /*! @brief Generate a random port name.
         @returns A randomly-generated port name. */
        static yarp::os::ConstString getRandomPortName(void);
        
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
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunused-private-field"
        /*! @brief Filler to pad to alignment boundary */
        char                  _filler[7];
# pragma clang diagnostic pop
        
    }; // Endpoint
    
} // YarpPlusPlus

#endif // ! defined(YPPENDPOINT_H_)
