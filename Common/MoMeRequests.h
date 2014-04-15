//--------------------------------------------------------------------------------------
//
//  File:       MoMeRequests.h
//
//  Project:    MoAndMe
//
//  Contains:   The common macro definitions for requests and responses.
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
//  Created:    2014-02-25
//
//--------------------------------------------------------------------------------------

#if (! defined(MOMEREQUESTS_H_))
/*! @brief Header guard. */
# define MOMEREQUESTS_H_ /* */

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 
 @brief The common macro definitions for requests and responses. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

/*! @brief The channel-independent name of the service registry service. */
# define MAM_REGISTRY_CANONICAL_NAME "Registry"

/*! @brief The name for the 'clients' request. */
# define MAM_CLIENTS_REQUEST    "clients"
/*! @brief The name for the 'detach' request. */
# define MAM_DETACH_REQUEST     "detach"
/*! @brief The name for the 'echo' request. */
# define MAM_ECHO_REQUEST       "echo"
/*! @brief The standard name for an 'info' request. */
# define MAM_INFO_REQUEST       "info"
/*! @brief The standard name for a 'list' request. */
# define MAM_LIST_REQUEST       "list"
/*! @brief The standard name for a 'match' request. */
# define MAM_MATCH_REQUEST      "match"
/*! @brief The standard name for a 'name' request. */
# define MAM_NAME_REQUEST       "name"
/*! @brief The standard name for a 'register' request. */
# define MAM_REGISTER_REQUEST   "register"
/*! @brief The standard name for a 'unregister' request. */
# define MAM_UNREGISTER_REQUEST "unregister"

/*! @brief The number of elements expected in the output of a 'match' request. */
# define MAM_EXPECTED_MATCH_RESPONSE_SIZE 2
/*! @brief The number of elements expected in the output of a 'name' request. */
# define MAM_EXPECTED_NAME_RESPONSE_SIZE  2

/*! @brief The standard response to an invalid 'register' / 'unregister' request. */
# define MAM_FAILED_RESPONSE "FAILED"
/*! @brief The standard response to a valid 'register' / 'unregister' request. */
# define MAM_OK_RESPONSE     "OK"

/*! @brief Request/response specification character - zero or one repetitions of preceding. */
# define MAM_REQREP_0_OR_1     "?"
/*! @brief Request/response specification character - zero or more repetitions of preceding. */
# define MAM_REQREP_0_OR_MORE  "*"
/*! @brief Request/response specification character - one or more repetitions of preceding. */
# define MAM_REQREP_1_OR_MORE  "+"
/*! @brief Request/response specification character - any element. */
# define MAM_REQREP_ANYTHING   "."
/*! @brief Request/response specification character - end of dictionary specification. */
# define MAM_REQREP_DICT_END   "]"
/*! @brief Request/response specification character - key/value separator for dictionary. */
# define MAM_REQREP_DICT_SEP   ":"
/*! @brief Request/response specification character - start of dictionary specification. */
# define MAM_REQREP_DICT_START "["
/*! @brief Request/response specification character - double value. */
# define MAM_REQREP_DOUBLE     "d"
/*! @brief Request/response specification character - integer value. */
# define MAM_REQREP_INT        "i"
/*! @brief Request/response specification character - end of list specification. */
# define MAM_REQREP_LIST_END   ")"
/*! @brief Request/response specification character - start of list specification. */
# define MAM_REQREP_LIST_START "("
/*! @brief Request/response specification character - numeric value (double or integer). */
# define MAM_REQREP_NUMBER     "n"
/*! @brief Request/response specification character - string value. */
# define MAM_REQREP_STRING     "s"

/*! @brief Request/response dictionary key - channel name. */
# define MAM_REQREP_DICT_CHANNELNAME_KEY "channelname"
/*! @brief Request/response dictionary key - details. */
# define MAM_REQREP_DICT_DETAILS_KEY     "details"
/*! @brief Request/response dictionary key - input specification. */
# define MAM_REQREP_DICT_INPUT_KEY       "input"
/*! @brief Request/response dictionary key - keywords. */
# define MAM_REQREP_DICT_KEYWORDS_KEY    "keywords"
/*! @brief Request/response dictionary key - output specification. */
# define MAM_REQREP_DICT_OUTPUT_KEY      "output"
/*! @brief Request/response dictionary key - request specification. */
# define MAM_REQREP_DICT_REQUEST_KEY     "request"
/*! @brief Request/response dictionary key - version. */
# define MAM_REQREP_DICT_VERSION_KEY     "version"

/*! @brief The standard name for the service registry channel. */
# define MAM_SERVICE_REGISTRY_CHANNEL_NAME "/$ervice"

#endif // ! defined(MOMEREQUESTS_H_)
