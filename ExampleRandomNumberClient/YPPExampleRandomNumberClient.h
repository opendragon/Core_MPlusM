//--------------------------------------------------------------------------------------
//
//  File:       YPPExampleRandomNumberClient.h
//
//  Project:    YarpPlusPlus
//
//  Contains:   The class declaration for the client of a simple Yarp++ service.
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
//  Created:    2014-03-06
//
//--------------------------------------------------------------------------------------

#if (! defined(YPPEXAMPLERANDOMNUMBERCLIENT_H_))
# define YPPEXAMPLERANDOMNUMBERCLIENT_H_ /* */

# include "YPPBaseClient.h"
# include <vector>

namespace YarpPlusPlusExample
{
    /*! @brief An example Yarp++ client, for the 'random' service. */
    class ExampleRandomNumberClient : public YarpPlusPlus::BaseClient
    {
    public:
        
        /*! @brief A sequence of random numbers. */
        typedef std::vector<double>    RandomVector;
        
        /*! @brief An iterator for a sequence of random numbers. */
        typedef RandomVector::iterator RandomVectorIterator;
        
        /*! @brief The constructor. */
        ExampleRandomNumberClient(void);
        
        /*! @brief The destructor. */
        virtual ~ExampleRandomNumberClient(void);
        
        /*! @brief Get one random number from the service.
         @param result Where to return the number.
         @returns @c true if the number was retrieved successfully and @c false otherwise. */
        bool getOneRandomNumber(double & result);
        
        /*! @brief Get a sequence of random numbers from the service.
         @param howMany The number of random numbers to retrieve.
         @param result Where to return the numbers.
         @returns @c true if the numbere were retrieved successfully and @c false otherwise. */
        bool getRandomNumbers(const int      howMany,
                              RandomVector & result);
        
    protected:
        
    private:
        
        /*! @brief The class that this class is derived from. */
        typedef BaseClient inherited;

        /*! @brief Copy constructor.
         
         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        ExampleRandomNumberClient(const ExampleRandomNumberClient & other);
        
        /*! @brief Assignment operator.
         
         Note - not implemented and private, to prevent unexpected copying.
         @param other Another object to construct from. */
        ExampleRandomNumberClient & operator=(const ExampleRandomNumberClient & other);
        
    }; // ExampleRandomNumberClient
    
} // YarpPlusPlusExample

#endif // ! defined(YPPEXAMPLERANDOMNUMBERCLIENT_H_)
