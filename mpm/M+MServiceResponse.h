//--------------------------------------------------------------------------------------------------
//
//  File:       mpm/M+MServiceResponse.h
//
//  Project:    M+M
//
//  Contains:   The class declaration for the response to an M+M request.
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
//  Created:    2014-02-06
//
//--------------------------------------------------------------------------------------------------

#if (! defined(MpMServiceResponse_H_))
# define MpMServiceResponse_H_ /* Header guard */

# include <mpm/M+MCommon.h>

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 
 @brief The class declaration for the response to an M+M request. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace MplusM
{
    namespace Common
    {
        /*! @brief The data returned from a service request. */
        class ServiceResponse final
        {
        public:
            
            /*! @brief The constructor. */
            ServiceResponse(void);
            
            /*! @brief The constructor.
             @param values The (optional) values for the response. */
            ServiceResponse(const yarp::os::Bottle & values);
            
            /*! @brief The destructor. */
            virtual ~ServiceResponse(void);
            
            /*! @brief The assignment operator.
             @param values The (optional) values for the response. */
            ServiceResponse & operator =(const yarp::os::Bottle & values);
            
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
            yarp::os::Bottle values(void)
            const
            {
                return _values;
            } // values
            
        protected:
            
        private:
            
            /*! @brief Copy constructor.
             
             Note - not implemented and private, to prevent unexpected copying.
             @param other Another object to construct from. */
            ServiceResponse(const ServiceResponse & other)
            DISALLOWED_FUNCTION;
            
            /*! @brief Assignment operator.
             
             Note - not implemented and private, to prevent unexpected copying.
             @param other Another object to construct from. */
            ServiceResponse & operator =(const ServiceResponse & other)
            DISALLOWED_FUNCTION;
            
            /*! @brief The response values. */
            yarp::os::Bottle _values;
            
        }; // ServiceResponse
        
    } // Common
    
} // MplusM

#endif // ! defined(MpMServiceResponse_H_)
