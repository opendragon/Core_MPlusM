//--------------------------------------------------------------------------------------------------
//
//  File:       mpm/M+MBaseAdapterService.cpp
//
//  Project:    M+M
//
//  Contains:   The class definition for the minimal functionality required for an M+M
//              adapter service.
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
//  Created:    2015-05-27
//
//--------------------------------------------------------------------------------------------------

#include <mpm/M+MBaseAdapterService.h>
#include <mpm/M+MRequests.h>

#include <mpm/M+MUtilities.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for the minimal functionality required for an M+M adapter service. */
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

BaseAdapterService::BaseAdapterService(const YarpString & launchPath,
                                       const int          argc,
                                       char * *           argv,
                                       const YarpString & tag,
                                       const bool         useMultipleHandlers,
                                       const YarpString & canonicalName,
                                       const YarpString & description,
                                       const YarpString & requestsDescription,
                                       const YarpString & serviceEndpointName,
                                       const YarpString & servicePortNumber) :
    inherited(kServiceKindAdapter, launchPath, argc, argv, tag, useMultipleHandlers, canonicalName,
              description, requestsDescription, serviceEndpointName, servicePortNumber)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S4s("launchPath = ", launchPath, "tag = ", tag, "canonicalName = ", canonicalName, //####
               "description = ", description); //####
    OD_LOG_S3s("requestsDescription = ", requestsDescription, "serviceEndpointName = ", //####
               serviceEndpointName, "servicePortNumber = ", servicePortNumber); //####
    OD_LOG_LL1("argc = ", argc); //####
    OD_LOG_P1("argv = ", argv); //####
    OD_LOG_B1("useMultipleHandlers = ", useMultipleHandlers); //####
    OD_LOG_EXIT_P(this); //####
} // BaseAdapterService::BaseAdapterService

BaseAdapterService::~BaseAdapterService(void)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_OBJEXIT(); //####
} // BaseAdapterService::~BaseAdapterService

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

bool BaseAdapterService::setUpClientStreams(void)
{
    OD_LOG_OBJENTER(); //####
    bool result = inherited::setUpClientStreams();
    
    if (result)
    {
        result = addClientStreamsFromDescriptions(_clientDescriptions);
    }
    OD_LOG_EXIT_B(result); //####
    return result;
} // BaseAdapterService::setUpClientStreams

bool BaseAdapterService::setUpInputStreams(void)
{
    OD_LOG_OBJENTER(); //####
    bool result = inherited::setUpInputStreams();
    
    if (result)
    {
        result = addInStreamsFromDescriptions(_inDescriptions);
    }
    OD_LOG_EXIT_B(result); //####
    return result;
} // BaseAdapterService::setUpInputStreams

bool BaseAdapterService::setUpOutputStreams(void)
{
    OD_LOG_OBJENTER(); //####
    bool result = inherited::setUpOutputStreams();
    
    if (result)
    {
        result = addOutStreamsFromDescriptions(_outDescriptions);
    }
    OD_LOG_EXIT_B(result); //####
    return result;
} // BaseAdapterService::setUpOutputStreams

bool BaseAdapterService::shutDownClientStreams(void)
{
    OD_LOG_OBJENTER(); //####
    bool result = inherited::shutDownClientStreams();
    
    OD_LOG_EXIT_B(result); //####
    return result;
} // BaseAdapterService::shutDownClientStreams

bool BaseAdapterService::shutDownInputStreams(void)
{
    OD_LOG_OBJENTER(); //####
    bool result = inherited::shutDownInputStreams();
    
    OD_LOG_EXIT_B(result); //####
    return result;
} // BaseAdapterService::shutDownInputStreams

bool BaseAdapterService::shutDownOutputStreams(void)
{
    OD_LOG_OBJENTER(); //####
    bool result = inherited::shutDownOutputStreams();
    
    OD_LOG_EXIT_B(result); //####
    return result;
} // BaseAdapterService::shutDownOutputStreams

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

bool Common::ProcessStandardAdapterOptions(const int                     argc,
                                           char * *                      argv,
                                           Utilities::DescriptorVector & argumentDescriptions,
                                           const YarpString &            defaultEndpointNameRoot,
                                           const YarpString &            adapterDescription,
                                           const YarpString &            matchingCriteria,
                                           const int                     year,
                                           const char *                  copyrightHolder,
                                           bool &                        goWasSet,
                                           bool &                        reportOnExit,
                                           YarpString &                  tag,
                                           YarpString &                  serviceEndpointName,
                                           YarpString &                  servicePortNumber,
                                           const OptionsMask             skipOptions,
                                           YarpStringVector *            arguments)
{
    OD_LOG_ENTER(); //####
    OD_LOG_L2("argc = ", argc, "year = ", year); //####
    OD_LOG_P4("argv = ", argv, "argumentDescriptions = ", &argumentDescriptions, //####
              "reportOnExit = ", &reportOnExit, "arguments = ", arguments); //####
    OD_LOG_S2s("defaultEndpointNameRoot = ", defaultEndpointNameRoot, //####
               "adapterDescription = ", adapterDescription); //####
    OD_LOG_S1("copyrightHolder = ", copyrightHolder); //####
    enum optionIndex
    {
        kOptionUNKNOWN,
        kOptionARGS,
        kOptionCHANNEL,
        kOptionGO,
        kOptionHELP,
        kOptionINFO,
        kOptionPORT,
        kOptionREPORT,
        kOptionTAG,
        kOptionVERSION
    }; // optionIndex
    
    bool                  keepGoing = true;
    bool                  reportEndpoint = false;
    Option_::Descriptor   firstDescriptor(kOptionUNKNOWN, 0, "", "", Option_::Arg::None, NULL);
    Option_::Descriptor   argsDescriptor(kOptionARGS, 0, "a", "args", Option_::Arg::None,
                                         T_("  --args, -a        Report the argument formats"));
    Option_::Descriptor   channelDescriptor(kOptionCHANNEL, 0, "c", "channel", Option_::Arg::None,
                                            T_("  --channel, -c     Report the actual endpoint "
                                               "name"));
    Option_::Descriptor   goDescriptor(kOptionGO, 0, "g", "go", Option_::Arg::None,
                                       T_("  --go, -g          Start the service immediately"));
    Option_::Descriptor   helpDescriptor(kOptionHELP, 0, "h", "help", Option_::Arg::None,
                                         T_("  --help, -h        Print usage and exit"));
    Option_::Descriptor   infoDescriptor(kOptionINFO, 0, "i", "info", Option_::Arg::None,
                                         T_("  --info, -i        Print executable type, supported "
                                            "service options and description and exit"));
    Option_::Descriptor   portDescriptor(kOptionPORT, 0, "p", "port", Option_::Arg::Required,
                                         T_("  --port, -p        Specify a non-default port to be "
                                            "used"));
    Option_::Descriptor   reportDescriptor(kOptionREPORT, 0, "r", "report", Option_::Arg::None,
                                           T_("  --report, -r      Report the service metrics when "
                                              "the application exits"));
    Option_::Descriptor   tagDescriptor(kOptionTAG, 0, "t", "tag", Option_::Arg::Required,
                                        T_("  --tag, -t         Specify the tag to be used as part "
                                           "of the service name"));
    Option_::Descriptor   versionDescriptor(kOptionVERSION, 0, "v", "vers", Option_::Arg::None,
                                            T_("  --vers, -v        Print version information and "
                                               "exit"));
    Option_::Descriptor   lastDescriptor(0, 0, NULL, NULL, NULL, NULL);
    Option_::Descriptor   usage[11];
    Option_::Descriptor * usageWalker = usage;
    int                   argcWork = argc;
    char * *              argvWork = argv;
    YarpString            usageString("USAGE: ");
    YarpString            argList(ArgumentsToArgString(argumentDescriptions));
    
    reportOnExit = goWasSet = false;
    tag = serviceEndpointName = serviceEndpointName = "";
    if (arguments)
    {
        arguments->clear();
    }
    usageString += *argv;
    usageString += " [options]";
    if (0 < argList.length())
    {
        YarpStringVector descriptions;
        
        Utilities::ArgumentsToDescriptionArray(argumentDescriptions, descriptions, 2);
        usageString += " ";
        usageString += argList + "\n\n";
        for (int ii = 0, mm = descriptions.size(); mm > ii; ++ii)
        {
            if (0 < ii)
            {
                usageString += "\n";
            }
            usageString += "  ";
            usageString += descriptions[ii];
        }
    }
    usageString += "\n\nOptions:";
#if MAC_OR_LINUX_
    firstDescriptor.help = strdup(usageString.c_str());
#else // ! MAC_OR_LINUX_
    firstDescriptor.help = _strdup(usageString.c_str());
#endif // ! MAC_OR_LINUX_
    memcpy(usageWalker++, &firstDescriptor, sizeof(firstDescriptor));
    if (! (skipOptions & kSkipArgsOption))
    {
        memcpy(usageWalker++, &argsDescriptor, sizeof(argsDescriptor));
    }
    if (! (skipOptions & kSkipChannelOption))
    {
        memcpy(usageWalker++, &channelDescriptor, sizeof(channelDescriptor));
    }
    if (! (skipOptions & kSkipGoOption))
    {
        memcpy(usageWalker++, &goDescriptor, sizeof(goDescriptor));
    }
    memcpy(usageWalker++, &helpDescriptor, sizeof(helpDescriptor));
    if (! (skipOptions & kSkipInfoOption))
    {
        memcpy(usageWalker++, &infoDescriptor, sizeof(infoDescriptor));
    }
    if (! (skipOptions & kSkipPortOption))
    {
        memcpy(usageWalker++, &portDescriptor, sizeof(portDescriptor));
    }
    if (! (skipOptions & kSkipReportOption))
    {
        memcpy(usageWalker++, &reportDescriptor, sizeof(reportDescriptor));
    }
    if (! (skipOptions & kSkipTagOption))
    {
        memcpy(usageWalker++, &tagDescriptor, sizeof(tagDescriptor));
    }
    memcpy(usageWalker++, &versionDescriptor, sizeof(versionDescriptor));
    memcpy(usageWalker++, &lastDescriptor, sizeof(lastDescriptor));
    argcWork -= (argc > 0);
    argvWork += (argc > 0); // skip program name argv[0] if present
    Option_::Stats    stats(usage, argcWork, argvWork);
    Option_::Option * options = new Option_::Option[stats.options_max];
    Option_::Option * buffer = new Option_::Option[stats.buffer_max];
    Option_::Parser   parse(usage, argcWork, argvWork, options, buffer, 1);
    
    if (parse.error())
    {
        keepGoing = false;
    }
    else if (options[kOptionHELP] || options[kOptionUNKNOWN])
    {
        Option_::printUsage(cout, usage, HELP_LINE_LENGTH);
        keepGoing = false;
    }
    else if (options[kOptionVERSION])
    {
        YarpString mpmVersionString(SanitizeString(MpM_VERSION, true));
        
        cout << "Version " << mpmVersionString.c_str() << ": Copyright (c) " << year << " by " <<
        copyrightHolder << "." << endl;
        keepGoing = false;
    }
    else if (options[kOptionARGS])
    {
        for (int ii = 0, mm = argumentDescriptions.size(); mm > ii; ++ii)
        {
            Utilities::BaseArgumentDescriptor * anArg = argumentDescriptions[ii];
            
            if (0 < ii)
            {
                cout << "\t";
            }
            if (anArg)
            {
                cout << anArg->toString().c_str();
            }
        }
        cout << endl;
        keepGoing = false;
    }
    else if (options[kOptionINFO])
    {
        bool needTab = true;
        
        // Note that we don't report the 'h' and 'v' options, as they are not involved in
        // determining what choices to offer when launching a service.
        cout << "Adapter";
        if (! (skipOptions & kSkipArgsOption))
        {
            if (needTab)
            {
                cout << "\t";
                needTab = false;
            }
            cout << "a";
        }
        if (! (skipOptions & kSkipChannelOption))
        {
            if (needTab)
            {
                cout << "\t";
                needTab = false;
            }
            cout << "c";
        }
        if (! (skipOptions & kSkipEndpointOption))
        {
            if (needTab)
            {
                cout << "\t";
                needTab = false;
            }
            cout << "e";
        }
        if (! (skipOptions & kSkipGoOption))
        {
            if (needTab)
            {
                cout << "\t";
                needTab = false;
            }
            cout << "g";
        }
        if (! (skipOptions & kSkipInfoOption))
        {
            if (needTab)
            {
                cout << "\t";
                needTab = false;
            }
            cout << "i";
        }
        if (! (skipOptions & kSkipPortOption))
        {
            if (needTab)
            {
                cout << "\t";
                needTab = false;
            }
            cout << "p";
        }
        if (! (skipOptions & kSkipReportOption))
        {
            if (needTab)
            {
                cout << "\t";
                needTab = false;
            }
            cout << "r";
        }
        if (! (skipOptions & kSkipTagOption))
        {
            if (needTab)
            {
                cout << "\t";
                needTab = false;
            }
            cout << "t";
        }
        if (needTab)
        {
            cout << "\t";
        }
        cout << "\t" << matchingCriteria.c_str() << "\t" << adapterDescription.c_str() << endl;
        keepGoing = false;
    }
    else if (ProcessArguments(argumentDescriptions, parse))
    {
        if (options[kOptionGO])
        {
            goWasSet = true;
        }
        if (options[kOptionCHANNEL])
        {
            reportEndpoint = true;
        }
        if (options[kOptionREPORT])
        {
            reportOnExit = true;
        }
        if (options[kOptionPORT])
        {
            servicePortNumber = options[kOptionPORT].arg;
            OD_LOG_S1s("servicePortNumber <- ", servicePortNumber); //####
            if (0 < servicePortNumber.length())
            {
                const char * startPtr = servicePortNumber.c_str();
                char *       endPtr;
                int          aPort = static_cast<int>(strtol(startPtr, &endPtr, 10));
                
                if ((startPtr == endPtr) || *endPtr || (! Utilities::ValidPortNumber(aPort)))
                {
                    cout << "Bad port number." << endl;
                    keepGoing = false;
                }
            }
        }
        if (options[kOptionTAG])
        {
            tag = options[kOptionTAG].arg;
            OD_LOG_S1s("tag <- ", tag); //####
        }
        if (arguments)
        {
            for (int ii = 0; ii < parse.nonOptionsCount(); ++ii)
            {
                arguments->push_back(parse.nonOption(ii));
            }
        }
    }
    else
    {
        cout << "One or more invalid or missing arguments." << endl;
        keepGoing = false;
    }
    delete[] options;
    delete[] buffer;
    if (0 < tag.size())
    {
        serviceEndpointName = defaultEndpointNameRoot + "/" + tag;
    }
    else
    {
        serviceEndpointName = defaultEndpointNameRoot;
    }
    if (reportEndpoint)
    {
        cout << serviceEndpointName.c_str() << endl;
        keepGoing = false;
    }
    OD_LOG_EXIT_B(keepGoing); //####
    return keepGoing;
} // Common::ProcessStandardAdapterOptions
