//--------------------------------------------------------------------------------------------------
//
//  File:       m+m/m+mConfig.h
//
//  Project:    m+m
//
//  Contains:   The common macro definitions for m+m clients and services.
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
//  Created:    2014-02-06
//
//--------------------------------------------------------------------------------------------------

#if (! defined(MpMConfig_H_))
# define MpMConfig_H_ /* Header guard */

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
   @brief The common macro definitions for m+m clients and services. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

/*! @brief The major part of the version number. */
# define MpM_VERSION_MAJOR_ 1

/*! @brief The minor part of the version number. */
# define MpM_VERSION_MINOR_ 5

/*! @brief The patch part of the version number. */
# define MpM_VERSION_PATCH_ 6

/*! @brief The version number as a string. */
# define MpM_VERSION_ "1.5.6"

/*! @brief The base of the channel name to use for an adapter. */
# define MpM_ADAPTER_BASE_NAME_        "/_adapter_/"

/*! @brief The base of the channel name to use for a client. */
# define MpM_CLIENT_BASE_NAME_         "/_client_/"

/*! @brief The base name of the channel name to use for an input. */
# define MpM_INPUT_BASE_NAME_          "/_input_/"

/*! @brief The base name of the channel name to use for an output. */
# define MpM_OUTPUT_BASE_NAME_         "/_output_/"

/*! @brief The name of the %Registry service executable. */
# define MpM_REGISTRY_EXECUTABLE_NAME_ "m+mRegistryService"

/*! @brief The base of the channel name to use for a service. */
# define MpM_SERVICE_BASE_NAME_        "/_service_/"

/* #undef MpM_ChattyStart */

/* #undef MpM_DoExplicitCheckForOK */

/* #undef MpM_DoExplicitClose */

/* #undef MpM_DoExplicitDisconnect */

/* #undef MpM_DontUseTimeouts */

/* #undef MpM_LogIncludesYarpTrace */

/* #undef MpM_MainDoesDelayNotYield */

/* #undef MpM_MetricsInitiallyOn */

/* #undef MpM_ReportContactDetails */

/* #undef MpM_ReportOnConnections */

#define MpM_ServicesLogToStandardError /* Enable logging to stderr as well as the system log. */

/* #undef MpM_StallOnSendProblem */

/* #undef MpM_UseDiskDatabase */

/* #undef MpM_UseTestDatabase */

/* #undef MpM_UseTimeoutsInRetryLoops */

#endif // ! defined(MpMConfig_H_)
