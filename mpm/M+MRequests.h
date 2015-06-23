//--------------------------------------------------------------------------------------------------
//
//  File:       mpm/M+MRequests.h
//
//  Project:    M+M
//
//  Contains:   The common macro definitions for requests and responses.
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
//  Created:    2014-02-25
//
//--------------------------------------------------------------------------------------------------

#if (! defined(MpMRequests_H_))
# define MpMRequests_H_ /* Header guard */

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 @brief The common macro definitions for requests and responses. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

/*! @brief The standard name for the %Registry Service request channel. */
# define MpM_REGISTRY_ENDPOINT_NAME_  "/$ervice"

/*! @brief The channel-independent name of the %Registry Service. */
# define MpM_REGISTRY_CANONICAL_NAME_ "Registry"

/*! @brief The name of the secondary port for the %Registry Service. */
# define MpM_REGISTRY_STATUS_NAME_    T_(MpM_REGISTRY_ENDPOINT_NAME_ "/status")

/*! @brief The name for an 'arguments' request. */
# define MpM_ARGUMENTS_REQUEST_       "arguments"

/*! @brief The standard name for a 'channels' request. */
# define MpM_CHANNELS_REQUEST_        "channels"

/*! @brief The standard name for the 'clients' request. */
# define MpM_CLIENTS_REQUEST_         "clients"

/*! @brief The standard name for the 'configure' request. */
# define MpM_CONFIGURE_REQUEST_       "configure"

/*! @brief The standard name for the 'detach' request. */
# define MpM_DETACH_REQUEST_          "detach"

/*! @brief The name for the 'echo' request. */
# define MpM_ECHO_REQUEST_            "echo"

/*! @brief The standard name for a 'getMetrics' request. */
# define MpM_GETMETRICS_REQUEST_      "getMetrics"

/*! @brief The name for a 'getMetricsState' request. */
# define MpM_GETMETRICSSTATE_REQUEST_ "getMetricsState"

/*! @brief The standard name for an 'info' request. */
# define MpM_INFO_REQUEST_            "info"

/*! @brief The standard name for a 'list' request. */
# define MpM_LIST_REQUEST_            "list"

/*! @brief The name for a 'match' request. */
# define MpM_MATCH_REQUEST_           "match"

/*! @brief The standard name for a 'name' request. */
# define MpM_NAME_REQUEST_            "name"

/*! @brief The standard name for a 'ping' request. */
# define MpM_PING_REQUEST_            "ping"

/*! @brief The name for a 'register' request. */
# define MpM_REGISTER_REQUEST_        "register"

/*! @brief The name for a 'restartStreams' request. */
# define MpM_RESTARTSTREAMS_REQUEST_  "restartStreams"

/*! @brief The name for a 'setMetricsState' request. */
# define MpM_SETMETRICSSTATE_REQUEST_ "setMetricsState"

/*! @brief The name for a 'startStreams' request. */
# define MpM_STARTSTREAMS_REQUEST_    "startStreams"

/*! @brief The name for a 'stop' request. */
# define MpM_STOP_REQUEST_            "stop"

/*! @brief The name for a 'stopStreams' request. */
# define MpM_STOPSTREAMS_REQUEST_     "stopStreams"

/*! @brief The name for an 'unregister' request. */
# define MpM_UNREGISTER_REQUEST_      "unregister"

/*! @brief The name for a 'where' request. */
# define MpM_WHERE_REQUEST_           "where"

/*! @brief The number of elements expected in a channel description. */
# define MpM_EXPECTED_CHANNEL_DESCRIPTOR_SIZE_ 3

/*! @brief The number of elements expected in the output of a 'channels' request. */
# define MpM_EXPECTED_CHANNELS_RESPONSE_SIZE_        3

/*! @brief The number of elements expected in the output of a 'configure' request. */
# define MpM_EXPECTED_CONFIGURE_RESPONSE_SIZE_       1

/*! @brief The number of elements expected in the output of a 'detach' request. */
# define MpM_EXPECTED_DETACH_RESPONSE_SIZE_          1

/*! @brief The number of elements expected in the output of a 'getMetricsState' request. */
# define MpM_EXPECTED_GETMETRICSSTATE_RESPONSE_SIZE_ 1

/*! @brief The number of elements expected in the output of a 'match' request. */
# define MpM_EXPECTED_MATCH_RESPONSE_SIZE_           2

/*! @brief The number of elements expected in the output of a 'name' request. */
# define MpM_EXPECTED_NAME_RESPONSE_SIZE_            6

/*! @brief The number of elements expected in the output of a 'ping' request. */
# define MpM_EXPECTED_PING_RESPONSE_SIZE_            1

/*! @brief The number of elements expected in the output of a 'register' request. */
# define MpM_EXPECTED_REGISTER_RESPONSE_SIZE_        1

/*! @brief The number of elements expected in the output of a 'restartStreams' request. */
# define MpM_EXPECTED_RESTARTSTREAMS_RESPONSE_SIZE_  1

/*! @brief The number of elements expected in the output of a 'setMetricsState' request. */
# define MpM_EXPECTED_SETMETRICSSTATE_RESPONSE_SIZE_ 1

/*! @brief The number of elements expected in the output of a 'startStreams' request. */
# define MpM_EXPECTED_STARTSTREAMS_RESPONSE_SIZE_    1

/*! @brief The number of elements expected in the output of a 'stop' request. */
# define MpM_EXPECTED_STOP_RESPONSE_SIZE_            1

/*! @brief The number of elements expected in the output of a 'stopStreams' request. */
# define MpM_EXPECTED_STOPSTREAMS_RESPONSE_SIZE_     1

/*! @brief The number of elements expected in the output of an 'unregister' request. */
# define MpM_EXPECTED_UNREGISTER_RESPONSE_SIZE_      1

/*! @brief The number of elements expected in the output of an 'where' request. */
# define MpM_EXPECTED_WHERE_RESPONSE_SIZE_           2

/*! @brief The standard response to an invalid %Registry Service request. */
# define MpM_FAILED_RESPONSE_ "FAILED"

/*! @brief The standard response to a valid %Registry Service request. */
# define MpM_OK_RESPONSE_     "OK"

/*! @brief The number of elements expected in the %Registry Service status message. */
# define MpM_EXPECTED_REGISTRY_STATUS_SIZE_ 4

/*! @brief A service is being added to the registry. */
# define MpM_REGISTRY_STATUS_ADDING_        "adding"

/*! @brief A service has pinged the registry. */
# define MpM_REGISTRY_STATUS_PINGED_        "pinged"

/*! @brief A service or request could not be added to the registry. */
# define MpM_REGISTRY_STATUS_PROBLEM_       "problem"

/*! @brief A service is being registered in the registry. */
# define MpM_REGISTRY_STATUS_REGISTERING_   "registering"

/*! @brief A service is being removed from the registry. */
# define MpM_REGISTRY_STATUS_REMOVING_      "removing"

/*! @brief A service has not pinged the registry recently. */
# define MpM_REGISTRY_STATUS_STALE_         "stale"

/*! @brief The registry has just started. */
# define MpM_REGISTRY_STATUS_STARTING_      "starting"

/*! @brief The registry is stopping. */
# define MpM_REGISTRY_STATUS_STOPPING_      "stopping"

/*! @brief The registry status is unknown. */
# define MpM_REGISTRY_STATUS_UNKNOWN_       "<unknown>"

/*! @brief The service is not recognized. */
# define MpM_REGISTRY_STATUS_UNRECOGNIZED_  "unrecognized"

/*! @brief A service is being unregistered from the registry. */
# define MpM_REGISTRY_STATUS_UNREGISTERING_ "unregistering"

/*! @brief Request/response specification character - zero or one repetitions of preceding. */
# define MpM_REQREP_0_OR_1_     "?"

/*! @brief Request/response specification character - zero or more repetitions of preceding. */
# define MpM_REQREP_0_OR_MORE_  "*"

/*! @brief Request/response specification character - one or more repetitions of preceding. */
# define MpM_REQREP_1_OR_MORE_  "+"

/*! @brief Request/response specification character - any element. */
# define MpM_REQREP_ANYTHING_   "."

/*! @brief Request/response specification character - binary blob value. */
# define MpM_REQREP_BLOB_       "b"

/*! @brief Request/response specification character - end of dictionary specification. */
# define MpM_REQREP_DICT_END_   "]"

/*! @brief Request/response specification character - key/value separator for dictionary. */
# define MpM_REQREP_DICT_SEP_   ":"

/*! @brief Request/response specification character - start of dictionary specification. */
# define MpM_REQREP_DICT_START_ "["

/*! @brief Request/response specification character - double value. */
# define MpM_REQREP_DOUBLE_     "d"

/*! @brief Request/response specification character - integer value. */
# define MpM_REQREP_INT_        "i"

/*! @brief Request/response specification character - end of list specification. */
# define MpM_REQREP_LIST_END_   ")"

/*! @brief Request/response specification character - start of list specification. */
# define MpM_REQREP_LIST_START_ "("

/*! @brief Request/response specification character - numeric value (double or integer). */
# define MpM_REQREP_NUMBER_     "n"

/*! @brief Request/response specification character - string value. */
# define MpM_REQREP_STRING_     "s"

/*! @brief Request/response dictionary key - channel name. */
# define MpM_REQREP_DICT_CHANNELNAME_KEY_ "channelname"

/*! @brief Request/response dictionary key - details. */
# define MpM_REQREP_DICT_DETAILS_KEY_     "details"

/*! @brief Request/response dictionary key - input specification. */
# define MpM_REQREP_DICT_INPUT_KEY_       "input"

/*! @brief Request/response dictionary key - keywords. */
# define MpM_REQREP_DICT_KEYWORDS_KEY_    "keywords"

/*! @brief Request/response dictionary key - name. */
# define MpM_REQREP_DICT_NAME_KEY_        "name"

/*! @brief Request/response dictionary key - output specification. */
# define MpM_REQREP_DICT_OUTPUT_KEY_      "output"

/*! @brief Request/response dictionary key - request specification. */
# define MpM_REQREP_DICT_REQUEST_KEY_     "request"

/*! @brief Request/response dictionary key - version. */
# define MpM_REQREP_DICT_VERSION_KEY_     "version"

/*! @brief The name server reporting service name. */
# define MpM_MDNS_NAMESERVER_REPORT_  "_yarpns._tcp";

/*! @brief The name server reporting service version number. */
# define MpM_MDNS_NAMESERVER_VERSION_ "1"

/*! @brief The name server reporting service version number key. */
# define MpM_MDNS_NAMESERVER_KEY_     "txtvers"

#endif // ! defined(MpMRequests_H_)
