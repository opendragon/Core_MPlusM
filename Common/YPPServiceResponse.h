//--------------------------------------------------------------------------------------
//
//  File:       YPPServiceResponse.h
//
//  Project:    YarpPlusPlus
//
//  Contains:   The class declaration for the response to a Yarp++ request.
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

#if (! defined(YPPSERVICERESPONSE_H_))
/*! @brief Header guard. */
# define YPPSERVICERESPONSE_H_ /* */

# include "YPPCommon.h"

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 
 @brief The class declaration for the response to a Yarp++ request. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace YarpPlusPlus
{
    /*! @brief The data returned from a service request. */
    class ServiceResponse
    {
    public:
        
        /*! @brief The constructor.
         @param values The (optional) values for the response. */
        ServiceResponse(const Package & values = "");

        /*! @brief The destructor. */
        virtual ~ServiceResponse(void);
        
        /*! @brief The assignment operator.
         @param values The (optional) values for the response. */
        ServiceResponse & operator=(const Package & values);
        
        /*! @brief Return a printable version of the response.
         @returns A printable version of the response. */
        yarp::os::ConstString asString(void)
        const;
        
        /*! @brief The number of values in the response.
         @returns The number of values in the response. */
        inline int count(void)
        const
        {
            return _values.size();
        } // count
        
        /*! @brief Fetch an element from the values.
         @param index The @c 0-based index of the desired element.
         @returns The element corresponding to the provided index. */
        yarp::os::Value element(const int index)
        const;
        
        /*! @brief Return the full set of values.
         @returns All the values in the response. */
        Package values(void)
        const
        {
            return _values;
        } // values
        
    protected:
        
    private:
        
        /*! @brief Copy constructor.
         
         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        ServiceResponse(const ServiceResponse & other);
        
        /*! @brief Assignment operator.
         
         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        ServiceResponse & operator=(const ServiceResponse & other);
        
        /*! @brief The response values. */
        Package _values;

    }; // ServiceResponse
    
} // YarpPlusPlus

#endif // ! defined(YPPSERVICERESPONSE_H_)
