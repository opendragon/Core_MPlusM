//--------------------------------------------------------------------------------------------------
//
//  File:       m+mServiceLaunchThread.cpp
//
//  Project:    m+m
//
//  Contains:   The class declaration for the background service launcher.
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
//  Created:    2015-05-12
//
//--------------------------------------------------------------------------------------------------

#include "m+mServiceLaunchThread.hpp"

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)

#if (! MAC_OR_LINUX_)
# include <Windows.h>
#endif //! MAC_OR_LINUX_

/*! @file

 @brief The class declaration for the background service launcher. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MPlusM_Manager;
using namespace std;

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

ServiceLaunchThread::ServiceLaunchThread(const String &      pathToExecutable,
                                         const String &      endpointName,
                                         const String &      tag,
                                         const String &      portNumber,
                                         const int           tagModifierCount,
                                         const StringArray & arguments,
                                         const bool          needsGo) :
    inherited("General service launcher"), _serviceProcess(NULL), _serviceEndpoint(endpointName),
    _servicePath(pathToExecutable), _servicePort(portNumber), _serviceTag(tag),
    _arguments(arguments), _tagModifierCount(tagModifierCount), _needsGo(needsGo)
{
    ODL_ENTER(); //####
    ODL_S4s("pathToExecutable = ", pathToExecutable.toStdString(), "endpointName = ", //####
            endpointName.toStdString(), "tag = ", tag.toStdString(), "portNumber = ", //####
            portNumber.toStdString()); //####
    ODL_P1("arguments = ", &arguments); //####
    ODL_B1("needsGo = ", needsGo); //####
#if defined(OD_ENABLE_LOGGING_)
    String allArgs(arguments.joinIntoString(", "));

    ODL_S1s("allArgs <- ", allArgs.toStdString()); //####
#endif // defined(OD_ENABLE_LOGGING_)
    ODL_EXIT_P(this); //####
} // ServiceLaunchThread::ServiceLaunchThread

ServiceLaunchThread::~ServiceLaunchThread(void)
{
    ODL_OBJENTER(); //####
    killChildProcess();
    _serviceProcess = NULL;
    ODL_OBJEXIT(); //####
} // ServiceLaunchThread::~ServiceLaunchThread

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

void
ServiceLaunchThread::killChildProcess(void)
{
    ODL_OBJENTER(); //####
    if (_serviceProcess)
    {
        _serviceProcess->kill();
    }
    ODL_OBJEXIT(); //####
} // ServiceLaunchThread::killChildProcess

void
ServiceLaunchThread::run(void)
{
    ODL_OBJENTER(); //####
    _serviceProcess = new ChildProcess;
    if (_serviceProcess)
    {
        ODL_LOG("(_serviceProcess)"); //####
        StringArray nameAndArgs(_servicePath);

        if (0 < _servicePort.length())
        {
            ODL_LOG("(0 < _servicePort.length())"); //####
            nameAndArgs.add("--port");
            nameAndArgs.add(_servicePort);
        }
        if (0 < _serviceTag.length())
        {
            ODL_LOG("(0 < _serviceTag())"); //####
            nameAndArgs.add("--tag");
            nameAndArgs.add(_serviceTag);
        }
        if (0 < _serviceEndpoint.length())
        {
            ODL_LOG("(0 < _serviceEndpoint())"); //####
            nameAndArgs.add("--endpoint");
            nameAndArgs.add(_serviceEndpoint);
        }
        if (0 < _tagModifierCount)
        {
            ODL_LOG("(0 < tagModifierCount)"); //####
            nameAndArgs.add("--mod");
            nameAndArgs.add(String(_tagModifierCount));
        }
        if (_needsGo)
        {
            ODL_LOG("(_needsGo)"); //####
            nameAndArgs.add("--go");
        }
        if (0 < _arguments.size())
        {
            nameAndArgs.addArray(_arguments);
        }
        if (_serviceProcess->start(nameAndArgs, 0))
        {
            ODL_LOG("(_serviceProcess->start(nameAndArgs, 0))"); //####
            const String childOutput(_serviceProcess->readAllProcessOutput());

            LazyLaunchProcess(*_serviceProcess, -1);
            ODL_S1s("childOutput = ", childOutput.toStdString()); //####
        }
        else
        {
            ODL_LOG("! (_serviceProcess->start(nameAndArgs, 0))"); //####
        }
    }
    ODL_OBJEXIT(); //####
} // ServiceLaunchThread::run

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
