//--------------------------------------------------------------------------------------------------
//
//  File:       mpm/M+MAdapterOneArgument.cpp
//
//  Project:    M+M
//
//  Contains:   The class definition for the minimal functionality required to gather the arguments
//              for an M+M adapter.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2015 by H Plus Technologies Ltd. and Simon Fraser University.
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
//  Created:    2015-05-14
//
//--------------------------------------------------------------------------------------------------

#include <mpm/M+MAdapterOneArgument.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for the minimal functionality required to gather the arguments for an
 M+M adapter. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and Destructors
#endif // defined(__APPLE__)

AdapterOneArgument::AdapterOneArgument(const char *       argList,
                                       const char *       argDescription,
                                       const YarpString & defaultFirstArgument,
                                       YarpString &       firstArgument) :
    inherited(argList, argDescription), _firstArgument(firstArgument)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S2("argList = ", argList, "argDescription = ", argDescription); //####
    OD_LOG_S1s("defaultFirstArgument = ", defaultFirstArgument); //####
    OD_LOG_P1("firstArgument = ", &firstArgument); //####
    _firstArgument = defaultFirstArgument;
    OD_LOG_EXIT_P(this); //####
} // AdapterOneArgument::AdapterOneArgument

AdapterOneArgument::~AdapterOneArgument(void)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_OBJEXIT(); //####
} // AdapterOneArgument::~AdapterOneArgument

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

YarpString AdapterOneArgument::combineArguments(const YarpString & sep)
{
    OD_LOG_OBJENTER(); //####
    YarpString result(_firstArgument);

    OD_LOG_EXIT_s(result); //####
    return result;
} // AdapterOneArgument::combineArguments

void AdapterOneArgument::processArguments(Option_::Parser & parseResult)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("parseResult = ", parseResult); //####
    if (0 < parseResult.nonOptionsCount())
    {
        _firstArgument = parseResult.nonOption(0);
    }
    OD_LOG_EXIT(); //####
} // AdapterOneArgument::processArguments

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
