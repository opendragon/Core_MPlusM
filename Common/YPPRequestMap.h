//
//  YPPRequestMap.h
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-28.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#if (! defined(YPPREQUESTMAP_H_))
# define YPPREQUESTMAP_H_ /* */

# include "YPPConfig.h"
# include <map>
# include <string>
# include <yarp/os/Bottle.h>
# include <yarp/os/ConstString.h>

namespace YarpPlusPlus
{
    class BaseService;
    class RequestHandler;
    
    /*! @brief A convenience class to provide distinct exception objects. */
    class RequestMap
    {
    public:
        
        /*! @brief The constructor. */
        RequestMap(BaseService & owner);
        
        /*! @brief The destructor. */
        virtual ~RequestMap(void);
        
        /*! @brief Construct the response to a 'list' request.
         @param reply The Bottle to hold the reply. */
        void fillInListReply(yarp::os::Bottle & reply);
        
        /*! @brief Construct the response to an 'info' request.
         @param reply The Bottle to hold the reply.
         @param requestName The name of the request that is being looked at. */
        void fillInRequestInfo(yarp::os::Bottle &            reply,
                               const yarp::os::ConstString & requestName);

        /*! @brief Return the function corresponding to a particular request.
         @param request The requested operation.
         @returns A pointer to the function to be invoked for the request, or @c NULL if it is not recognized. */
        RequestHandler * lookupRequestHandler(const yarp::os::ConstString & request);

        /*! @brief Remember the function to be used to handle a particular request.
         @param request The requested operation.
         @param handler The function to be called for the operation. */
        void registerRequestHandler(const yarp::os::ConstString & request,
                                    RequestHandler *              handler);
        
        /*! @brief Remember the function to be used to handle unrecognized requests.
         @param handler The function to be called by default. */
        void setDefaultRequestHandler(RequestHandler * handler);

    protected:
        
    private:
        
        typedef std::map<std::string, RequestHandler *> RequestHandlerMap;
        
        typedef RequestHandlerMap::value_type RequestHandlerMapValue;
        
        /*! @brief Copy constructor.
         
         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        RequestMap(const RequestMap & other);
        
        /*! @brief Assignment operator.
         
         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        RequestMap & operator=(const RequestMap & other);
        
        /*! @brief The default handler to use for unrecognized requests. */
        RequestHandler * _defaultHandler;
        
        /*! @brief The map between requests and request handlers. */
        RequestHandlerMap _handlers;
        
        /*! @brief The service that owns this map. */
        BaseService & _owner;
        
    }; // RequestMap
    
} // YarpPlusPlus

#endif // ! defined(YPPREQUESTMAP_H_)
