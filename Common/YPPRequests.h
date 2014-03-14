//--------------------------------------------------------------------------------------
//
//  File:       YPPRequests.h
//
//  Project:    YarpPlusPlus
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

#if (! defined(YPPREQUESTS_H_))
# define YPPREQUESTS_H_ /* */

/*! @brief The port-independent name of the service registry service. */
# define YPP_REGISTRY_CANONICAL_NAME "Registry"

/*! @brief The name for the 'echo' request. */
# define YPP_ECHO_REQUEST       "echo"
/*! @brief The standard name for an 'info' request. */
# define YPP_INFO_REQUEST       "info"
/*! @brief The standard name for a 'list' request. */
# define YPP_LIST_REQUEST       "list"
/*! @brief The standard name for a 'match' request. */
# define YPP_MATCH_REQUEST      "match"
/*! @brief The standard name for a 'name' request. */
# define YPP_NAME_REQUEST       "name"
/*! @brief The standard name for a 'register' request. */
# define YPP_REGISTER_REQUEST   "register"
/*! @brief The standard name for a 'unregister' request. */
# define YPP_UNREGISTER_REQUEST "unregister"

/*! @brief The standard response to an invalid 'register' / 'unregister' request. */
# define YPP_FAILED_RESPONSE "FAILED"
/*! @brief The standard response to a valid 'register' / 'unregister' request. */
# define YPP_OK_RESPONSE     "OK"

/*! @brief Request/response specification character - integer value. */
# define YPP_REQREP_INT        "i"
/*! @brief Request/response specification character - double value. */
# define YPP_REQREP_DOUBLE     "d"
/*! @brief Request/response specification character - string value. */
# define YPP_REQREP_STRING     "s"
/*! @brief Request/response specification character - start of list specification. */
# define YPP_REQREP_LIST_START "("
/*! @brief Request/response specification character - end of list specification. */
# define YPP_REQREP_LIST_END   ")"
/*! @brief Request/response specification character - start of dictionary specification. */
# define YPP_REQREP_DICT_START "["
/*! @brief Request/response specification character - end of dictionary specification. */
# define YPP_REQREP_DICT_END   "]"
/*! @brief Request/response specification character - key/value separator for dictionary. */
# define YPP_REQREP_DICT_SEP   ":"
/*! @brief Request/response specification character - one or more repetitions of preceding. */
# define YPP_REQREP_1_OR_MORE  "+"
/*! @brief Request/response specification character - zero or more repetitions of preceding. */
# define YPP_REQREP_0_OR_MORE  "*"
/*! @brief Request/response specification character - zero or one repetitions of preceding. */
# define YPP_REQREP_0_OR_1     "?"
/*! @brief Request/response specification character - any element. */
# define YPP_REQREP_ANYTHING   "."

/*! @brief Request/response dictionary key - description. */
# define YPP_REQREP_DICT_DESCRIPTION_KEY "description"
/*! @brief Request/response dictionary key - input specification. */
# define YPP_REQREP_DICT_INPUT_KEY       "input"
/*! @brief Request/response dictionary key - keywords. */
# define YPP_REQREP_DICT_KEYWORDS_KEY    "keywords"
/*! @brief Request/response dictionary key - output specification. */
# define YPP_REQREP_DICT_OUTPUT_KEY      "output"
/*! @brief Request/response dictionary key - request specification. */
# define YPP_REQREP_DICT_REQUEST_KEY     "request"
/*! @brief Request/response dictionary key - version. */
# define YPP_REQREP_DICT_VERSION_KEY     "version"

/*! @brief THe standard name for the service registry port. */
# define YPP_SERVICE_REGISTRY_PORT_NAME "/$ervice"

#endif // ! defined(YPPREQUESTS_H_)
