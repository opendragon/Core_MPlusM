//--------------------------------------------------------------------------------------------------
//
//  File:       m+mRunningSumRequests.hpp
//
//  Project:    m+m
//
//  Contains:   The common macro definitions for requests and responses for the Running Sum service.
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
//  Created:    2014-03-18
//
//--------------------------------------------------------------------------------------------------

#if (! defined(MpMRunningSumRequests_HPP_))
# define MpMRunningSumRequests_HPP_ /* Header guard */

# include <m+m/m+mRequests.hpp>

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 @brief The common macro definitions for requests and responses for the Running Sum service. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

/*! @brief The channel-independent name of the Running Sum service. */
# define MpM_RUNNINGSUM_CANONICAL_NAME_ "RunningSum"

/*! @brief The name for the 'addToSum' request. */
# define MpM_ADDTOSUM_REQUEST_ "addToSum"

/*! @brief The name for the 'quit' request. */
# define MpM_QUIT_REQUEST_     "quit"

/*! @brief The name for the 'resetSum' request. */
# define MpM_RESETSUM_REQUEST_ "resetSum"

/*! @brief The name for the 'startSum' request. */
# define MpM_STARTSUM_REQUEST_ "startSum"

/*! @brief The name for the 'stopSum' request. */
# define MpM_STOPSUM_REQUEST_  "stopSum"

/*! @brief The number of elements expected in the output of an 'addToSum' request. */
# define MpM_EXPECTED_ADDTOSUM_RESPONSE_SIZE_ 1

/*! @brief The number of elements expected in the output of a 'quit' request. */
# define MpM_EXPECTED_QUIT_RESPONSE_SIZE_     1

/*! @brief The number of elements expected in the output of a 'resetSum' request. */
# define MpM_EXPECTED_RESETSUM_RESPONSE_SIZE_ 1

/*! @brief The number of elements expected in the output of a 'startSum' request. */
# define MpM_EXPECTED_STARTSUM_RESPONSE_SIZE_ 1

/*! @brief The number of elements expected in the output of a 'stopSum' request. */
# define MpM_EXPECTED_STOPSUM_RESPONSE_SIZE_  1

#endif // ! defined(MpMRunningSumRequests_HPP_)
