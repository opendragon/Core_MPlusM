//--------------------------------------------------------------------------------------------------
//
//  File:       m+m/m+mUtilities.cpp
//
//  Project:    m+m
//
//  Contains:   The function and variable declarations for utilities for m+m clients and services.
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
//  Created:    2014-03-19
//
//--------------------------------------------------------------------------------------------------

#include "m+mUtilities.h"

#include <m+m/m+mBaseClient.h>
#include <m+m/m+mClientChannel.h>
#include <m+m/m+mRequests.h>
#include <m+m/m+mServiceRequest.h>
#include <m+m/m+mServiceResponse.h>
#include <m+m/m+mStringBuffer.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if (! MAC_OR_LINUX_)
# pragma comment(lib, "ws2_32.lib")
#endif // ! MAC_OR_LINUX_

#if MAC_OR_LINUX_
# include <libgen.h>
#else // ! MAC_OR_LINUX_
# include <stdlib.h>
# include <sys/timeb.h>
#endif // ! MAC_OR_LINUX_

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wc++11-extensions"
# pragma clang diagnostic ignored "-Wconversion"
# pragma clang diagnostic ignored "-Wdeprecated-declarations"
# pragma clang diagnostic ignored "-Wdocumentation"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# pragma clang diagnostic ignored "-Wextern-c-compat"
# pragma clang diagnostic ignored "-Wextra-semi"
# pragma clang diagnostic ignored "-Wpadded"
# pragma clang diagnostic ignored "-Wshadow"
# pragma clang diagnostic ignored "-Wsign-conversion"
# pragma clang diagnostic ignored "-Wunused-parameter"
# pragma clang diagnostic ignored "-Wweak-vtables"
#endif // defined(__APPLE__)
#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4267)
# pragma warning(disable: 4996)
#endif // ! MAC_OR_LINUX_
#include <yarp/os/impl/BufferedConnectionWriter.h>
#include <yarp/os/impl/NameConfig.h>
#include <yarp/os/impl/PortCommand.h>
#include <yarp/os/impl/Protocol.h>
#include <yarp/os/impl/String.h>
#include <yarp/os/impl/TcpFace.h>
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

// Note that the following must be after the YARP includes or else Windows compiles break.
#include <dns_sd.h>

#if defined(__APPLE__)
# include "TargetConditionals.h"
# if (! TARGET_OS_IPHONE)
#  define CONFIG_FILE_AVAILABLE_
# endif // ! TARGET_OS_IPHONE
#else // ! defined(__APPLE__)
# define CONFIG_FILE_AVAILABLE_
#endif // ! defined(__APPLE__)

#if MAC_OR_LINUX_
# include <netdb.h>
# include <pwd.h>
# include <uuid/uuid.h>
#endif // MAC_OR_LINUX_

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The function and variable definitions for utilities for m+m clients and services. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::Utilities;
using std::cerr;
using std::cout;
using std::endl;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

#if (! MAC_OR_LINUX_)
# define strtok_r strtok_s /* Equivalent routine for Windows. */
#endif // ! MAC_OR_LINUX_

/*! @brief The number of seconds to wait on a select() for mDNS operatios. */
static const int kDNSWaitTime = 3;

/*! @brief @c true once the random number generator is seeded. */
static bool lRandomSeeded = false;

/*! @brief @c true once the first browse 'add' for the NameServerReporter has been seen. */
static volatile bool lSawBrowseAdd = false;

/*! @brief @c true once the resolve for the NameServerReporter has been seen. */
static volatile bool lSawResolve = false;

/*! @brief The global channel status reporter. */
static ChannelStatusReporter * lReporter = NULL;

/*! @brief The indicator string for the beginning of new information received. */
static const char * kLineMarker = "registration name ";

/*! @brief The part name being used for probing connections. */
static const char * kMagicName = "<$probe>";

#if defined(__APPLE__)
# pragma mark Global constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

/*@ brief Get the port names from the YARP name server.
 @param response The list returned from the YARP name server.
 @returns @c true if the list was successfully retrieved and @c false otherwise. */
static bool getNameServerPortList(yarp::os::Bottle & response)
{
    OD_LOG_ENTER(); //####
    bool                   okSoFar;
    yarp::os::Bottle       request;
    yarp::os::ContactStyle contactInfo;
    
    request.addString("list");
    contactInfo.timeout = 5.0;
    if (yarp::os::Network::writeToNameServer(request, response, contactInfo))
    {
        okSoFar = (1 == response.size());
        OD_LOG_B1("okSoFar <- ", okSoFar); //####
        OD_LOG_S1s("response = ", response.toString()); //####
    }
    else
    {
        OD_LOG("! (yarp::os::Network::writeToNameServer(request, response, contactInfo))"); //####
        okSoFar = false;
    }
    if (! okSoFar)
    {
        // Try again, in case of a network 'glitch'.
        response.clear();
        if (yarp::os::Network::writeToNameServer(request, response, contactInfo))
        {
            okSoFar = (1 == response.size());
            OD_LOG_B1("okSoFar <- ", okSoFar); //####
            OD_LOG_S1s("response = ", response.toString()); //####
        }
        else
        {
            OD_LOG("! (yarp::os::Network::writeToNameServer(request, response, " //####
                   "contactInfo))"); //####
        }
    }
    OD_LOG_EXIT_B(okSoFar); //####
    return okSoFar;
} // getNameServerPortList

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
/* @brief The mDNS resolve callback.
 @param service The DNSServiceRef initialized by DNSServiceResolve.
 @param flags The resolve result - @c kDNSServiceFlagsMoreComing.
 @param interfaceIndex The interface on which the service was resolved.
 @param errorCode @c kDNSServiceErr_NoError on success.
 @param fullname The full service domain name.
 @param hostTarget The target hostname of the machine providing the service.
 @param port The port, in network byte order, on which connections are accepted for this service.
 @param txtLen The length of the txt record, in bytes.
 @param txtRecord The service's primary txt record, in standard txt record format.
 @param context The context pointer that was passed by DNSServiceResolve. */
static void DNSSD_API resolveCallback(DNSServiceRef         service,
                                      DNSServiceFlags       flags,
                                      uint32_t              interfaceIndex,
                                      DNSServiceErrorType   errorCode,
                                      const char *          fullname,
                                      const char *          hostTarget,
                                      uint16_t              port, /* In network byte order */
                                      uint16_t              txtLen,
                                      const unsigned char * txtRecord,
                                      void *                context)
{
#if (! defined(OD_ENABLE_LOGGING_))
# if MAC_OR_LINUX_
//#  pragma unused(service,flags,context)
# endif // MAC_OR_LINUX_
#endif // ! defined(OD_ENABLE_LOGGING_)
    OD_LOG_ENTER(); //####
    OD_LOG_P3("service = ", service, "txtRecord = ", txtRecord, "context = ", context); //####
    OD_LOG_L4("flags = ", flags, "interfaceIndex = ", interfaceIndex, "errorCode = ",//####
              errorCode, "port = ", port); //####
    OD_LOG_L1("txtLen = ", txtLen); //####
    OD_LOG_S2("fullname = ", fullname, "hostTarget = ", hostTarget); //####
    bool okToUse = false;

    if (TXTRecordContainsKey(txtLen, txtRecord, MpM_MDNS_NAMESERVER_KEY_))
    {
        uint8_t      valueLen = 0;
        const void * valuePtr = TXTRecordGetValuePtr(txtLen, txtRecord, MpM_MDNS_NAMESERVER_KEY_,
                                                     &valueLen);
        
        if (valuePtr && (0 < valueLen))
        {
            std::stringstream buff1(MpM_MDNS_NAMESERVER_VERSION_);
            std::string       inString(reinterpret_cast<const char *>(valuePtr), valueLen);
            std::stringstream buff2(inString);
            int               thisVersion;
            int               otherVersion;
            
            buff1 >> thisVersion;
            buff2 >> otherVersion;
            okToUse = (thisVersion <= otherVersion);
        }
    }
    lSawResolve = true;
    if ((kDNSServiceErr_NoError == errorCode) && okToUse)
    {
        addrinfo   hints;
        addrinfo * res = NULL;
        
        memset(&hints, 0, sizeof(hints));
        hints.ai_family = AF_INET;
        hints.ai_socktype = SOCK_STREAM;
        hints.ai_flags = 0;
        hints.ai_protocol = 0;
        hints.ai_canonname = NULL;
        hints.ai_addr = NULL;
        hints.ai_next = NULL;
        if (! getaddrinfo(hostTarget, NULL, &hints, &res))
        {
            // Just use the first entry, as we aren't asking for a specific service.
            sockaddr_in *              asIP4 = reinterpret_cast<sockaddr_in *>(res->ai_addr);
            char                       buffer[INET_ADDRSTRLEN + 1];
            const char *               addressAsString = inet_ntop(res->ai_family, &asIP4->sin_addr,
                                                                   buffer, sizeof(buffer));
            yarp::os::impl::NameConfig nc;
#if defined(CONFIG_FILE_AVAILABLE_)
            yarp::os::Contact          address(addressAsString, ntohs(port));
#endif // ! defined(CONFIG_FILE_AVAILABLE_)
            
#if defined(CONFIG_FILE_AVAILABLE_)
            nc.setAddress(address);
            nc.toFile();
#else // ! defined(CONFIG_FILE_AVAILABLE_)
            nc.setManualConfig(addressAsString, ntohs(port));
#endif // ! defined(CONFIG_FILE_AVAILABLE_)
            freeaddrinfo(res);
        }
    }
    DNSServiceRefDeallocate(service);
    OD_LOG_EXIT(); //####
} // resolveCallback
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
/* @brief The mDNS browse callback.
 @param service The DNSServiceRef initialized by DNSServiceBrowse.
 @param flags The browse result - @c kDNSServiceFlagsAdd or @c kDNSServiceFlagsMoreComing.
 @param interfaceIndex The interface on which the service is advertised.
 @param errorCode @c kDNSServiceErr_NoError on success.
 @param name The service name that was registered.
 @param type The type of service that was registered.
 @param domain The domain on which the service was registered.
 @param context The context pointer that was passed by DNSServiceBrowse. */
static void DNSSD_API browseCallBack(DNSServiceRef       service,
                                     DNSServiceFlags     flags,
                                     uint32_t            interfaceIndex,
                                     DNSServiceErrorType errorCode,
                                     const char *        name,
                                     const char *        type,
                                     const char *        domain,
                                     void *              context)
{
#if (! defined(OD_ENABLE_LOGGING_))
# if MAC_OR_LINUX_
#  pragma unused(service,flags,context)
# endif // MAC_OR_LINUX_
#endif // ! defined(OD_ENABLE_LOGGING_)
    OD_LOG_ENTER(); //####
    OD_LOG_P2("service = ", service, "context = ", context); //####
    OD_LOG_L3("flags = ", flags, "interfaceIndex = ", interfaceIndex, "errorCode = ",//####
              errorCode); //####
    if (kDNSServiceErr_NoError == errorCode)
    {
        if (flags & kDNSServiceFlagsAdd)
        {
            DNSServiceRefDeallocate(service);
            lSawResolve = false;
            DNSServiceRef       service2 = NULL;
            DNSServiceErrorType err = DNSServiceResolve(&service2, 0, interfaceIndex, name, type,
                                                        domain, resolveCallback, NULL);
            
            if (kDNSServiceErr_NoError == err)
            {
                // Wait for the resolver?
                int            dns_sd_fd = DNSServiceRefSockFD(service2);
                int            nfds = dns_sd_fd + 1;
                fd_set         readfds;
                struct timeval tv;
                int            result;
                
                for ( ; ! lSawResolve; )
                {
                    FD_ZERO(&readfds);
                    FD_SET(dns_sd_fd, &readfds);
                    tv.tv_sec = kDNSWaitTime; // We don't want to wait forever.
                    tv.tv_usec = 0;
                    result = select(nfds, &readfds, NULL, NULL, &tv);
                    if (0 < result)
                    {
                        DNSServiceErrorType err = kDNSServiceErr_NoError;
                        
                        if (FD_ISSET(dns_sd_fd, &readfds))
                        {
                            err = DNSServiceProcessResult(service2);
                        }
                        if (err)
                        {
                            cerr << "DNSServiceProcessResult returned " << err << endl;
                            break;
                        }
                        
                    }
                    else if (0 != result)
                    {
                        int  actErrno = errno;
                        char errBuff[256];
                        
#if MAC_OR_LINUX_
                        strerror_r(actErrno, errBuff, sizeof(errBuff));
#else // ! MAC_OR_LINUX_
                        strerror_s(errBuff, sizeof(errBuff), actErrno);
#endif // ! MAC_OR_LINUX_
                        cerr << "select() returned " << result << " errno " << actErrno << " " <<
                                errBuff << endl;
                        if (EINTR != actErrno)
                        {
                            break;
                        }
                        
                    }
                    else
                    {
                        // Ran out of time - maybe there is no NameServerReporter running...
                        break;
                    }
                    
                }
                DNSServiceRefDeallocate(service2);
            }
            else
            {
                cerr << "DNSServiceResolve returned " << err << endl;
            }
            lSawBrowseAdd = true;
        }
    }
    else
    {
        cerr << "browseCallBack returned " << errorCode << endl;
    }
    OD_LOG_EXIT(); //####
} // browseCallBack
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

/*! @brief Check if the response is for an input connection.
 @param response The response from the port that is being checked.
 @param inputs The collected inputs for the port. */
static void checkForInputConnection(const yarp::os::Bottle & response,
                                    ChannelVector &          inputs)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S1s("response = ", response.toString()); //####
    OD_LOG_P1("inputs = ", &inputs); //####
    const char * matchString[] =
    {
        "There", "is", "an", "input", "connection", "from", NULL, "to", NULL, "using", NULL
    };
    int          respLen = response.size();
    int          matchLen = (sizeof(matchString) / sizeof(*matchString));
    
    if (respLen >= matchLen)
    {
        bool matched = true;
        
        for (int ii = 0; matched && (ii < matchLen); ++ii)
        {
            YarpString element(response.get(ii).asString());
            
            if (matchString[ii])
            {
                if (element != matchString[ii])
                {
                    matched = false;
                }
            }
        }
        if (matched)
        {
            YarpString mode(response.get(matchLen - 1).asString());
            YarpString destination(response.get(matchLen - 3).asString());
            YarpString source(response.get(matchLen - 5).asString());
            
            if ((source != kMagicName) && (destination != kMagicName))
            {
                ChannelDescription connection;
                
                connection._portName = source;
                if (mode == "tcp")
                {
                    connection._portMode = kChannelModeTCP;
                }
                else if (mode == "udp")
                {
                    connection._portMode = kChannelModeUDP;
                }
                else
                {
                    connection._portMode = kChannelModeOther;
                }
                inputs.push_back(connection);
            }
        }
    }
    OD_LOG_EXIT(); //####
} // checkForInputConnection

/*! @brief Check if the response is for an output connection.
 @param response The response from the port that is being checked.
 @param outputs The collected outputs for the port. */
static void checkForOutputConnection(const yarp::os::Bottle & response,
                                     ChannelVector &          outputs)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S1s("response = ", response.toString()); //####
    OD_LOG_P1("outputs = ", &outputs); //####
    const char * matchString[] =
    {
        "There", "is", "an", "output", "connection", "from", NULL, "to", NULL, "using",
        NULL
    };
    int          respLen = response.size();
    int          matchLen = (sizeof(matchString) / sizeof(*matchString));
    
    if (respLen >= matchLen)
    {
        bool matched = true;
        
        for (int ii = 0; matched && (ii < matchLen); ++ii)
        {
            YarpString element(response.get(ii).asString());
            
            if (matchString[ii])
            {
                if (element != matchString[ii])
                {
                    matched = false;
                }
            }
        }
        if (matched)
        {
            YarpString mode(response.get(matchLen - 1).asString());
            YarpString destination(response.get(matchLen - 3).asString());
            YarpString source(response.get(matchLen - 5).asString());
            
            if ((source != kMagicName) && (destination != kMagicName))
            {
                ChannelDescription connection;
                
                connection._portName = destination;
                if (mode == "tcp")
                {
                    connection._portMode = kChannelModeTCP;
                }
                else if (mode == "udp")
                {
                    connection._portMode = kChannelModeUDP;
                }
                else
                {
                    connection._portMode = kChannelModeOther;
                }
                outputs.push_back(connection);
            }
        }
    }
    OD_LOG_EXIT(); //####
} // checkForOutputConnection

/*! @brief Add a service metrics property to a string.
 @param propList The dictionary to process.
 @param flavour The output format to be used.
 @param channelWidth The width used for output alignment.
 @param sawSome @c true if some output has been generated and @c false otherwise.
 @param result The string to be added to. */
static void convertMetricPropertyToString(yarp::os::Property & propList,
                                          const OutputFlavour  flavour,
                                          const size_t         channelWidth,
                                          bool &               sawSome,
                                          std::stringstream &  result)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P3("propList = ", &propList, "sawSome = ", &sawSome, "result = ", &result); //####
    OD_LOG_L1("channelWidth = ", channelWidth); //####
    yarp::os::Bottle * theInBytesAsList = NULL;
    yarp::os::Bottle * theInMessagesAsList = NULL;
    yarp::os::Bottle * theOutBytesAsList = NULL;
    yarp::os::Bottle * theOutMessagesAsList = NULL;
    YarpString         theChannelAsString;
    YarpString         theDateAsString;
    YarpString         theTimeAsString;
    int64_t            inByteCount = 0;
    int64_t            inMessageCount = 0;
    int64_t            outByteCount = 0;
    int64_t            outMessageCount = 0;
    
    if (propList.check(MpM_SENDRECEIVE_CHANNEL_))
    {
        yarp::os::Value theChannel = propList.find(MpM_SENDRECEIVE_CHANNEL_);
        
        if (theChannel.isString())
        {
            theChannelAsString = theChannel.toString();
        }
    }
    if (propList.check(MpM_SENDRECEIVE_DATE_))
    {
        yarp::os::Value theDate = propList.find(MpM_SENDRECEIVE_DATE_);
        
        if (theDate.isString())
        {
            theDateAsString = theDate.toString();
        }
    }
    if (propList.check(MpM_SENDRECEIVE_TIME_))
    {
        yarp::os::Value theTime = propList.find(MpM_SENDRECEIVE_TIME_);
        
        if (theTime.isString())
        {
            theTimeAsString = theTime.toString();
        }
    }
    if (propList.check(MpM_SENDRECEIVE_INBYTES_))
    {
        yarp::os::Value theInBytes = propList.find(MpM_SENDRECEIVE_INBYTES_);
        
        if (theInBytes.isList())
        {
            theInBytesAsList = theInBytes.asList();
            if (theInBytesAsList && (2 == theInBytesAsList->size()))
            {
                yarp::os::Value firstValue(theInBytesAsList->get(0));
                yarp::os::Value secondValue(theInBytesAsList->get(1));
                
                if (firstValue.isInt() && secondValue.isInt())
                {
                    inByteCount = (static_cast<int64_t>(firstValue.asInt()) << 32) +
                    secondValue.asInt();
                }
            }
        }
    }
    if (propList.check(MpM_SENDRECEIVE_INMESSAGES_))
    {
        yarp::os::Value theInMessages = propList.find(MpM_SENDRECEIVE_INMESSAGES_);
        
        if (theInMessages.isList())
        {
            theInMessagesAsList = theInMessages.asList();
            if (theInMessagesAsList && (2 == theInMessagesAsList->size()))
            {
                yarp::os::Value firstValue(theInMessagesAsList->get(0));
                yarp::os::Value secondValue(theInMessagesAsList->get(1));
                
                if (firstValue.isInt() && secondValue.isInt())
                {
                    inMessageCount = (static_cast<int64_t>(firstValue.asInt()) << 32) +
                    secondValue.asInt();
                }
            }
        }
    }
    if (propList.check(MpM_SENDRECEIVE_OUTBYTES_))
    {
        yarp::os::Value theOutBytes = propList.find(MpM_SENDRECEIVE_OUTBYTES_);
        
        if (theOutBytes.isList())
        {
            theOutBytesAsList = theOutBytes.asList();
            if (theOutBytesAsList && (2 == theOutBytesAsList->size()))
            {
                yarp::os::Value firstValue(theOutBytesAsList->get(0));
                yarp::os::Value secondValue(theOutBytesAsList->get(1));
                
                if (firstValue.isInt() && secondValue.isInt())
                {
                    outByteCount = (static_cast<int64_t>(firstValue.asInt()) << 32) +
                    secondValue.asInt();
                }
            }
        }
    }
    if (propList.check(MpM_SENDRECEIVE_OUTMESSAGES_))
    {
        yarp::os::Value theOutMessages = propList.find(MpM_SENDRECEIVE_OUTMESSAGES_);
        
        if (theOutMessages.isList())
        {
            theOutMessagesAsList = theOutMessages.asList();
            if (theOutMessagesAsList && (2 == theOutMessagesAsList->size()))
            {
                yarp::os::Value firstValue(theOutMessagesAsList->get(0));
                yarp::os::Value secondValue(theOutMessagesAsList->get(1));
                
                if (firstValue.isInt() && secondValue.isInt())
                {
                    outMessageCount = (static_cast<int64_t>(firstValue.asInt()) << 32) +
                    secondValue.asInt();
                }
            }
        }
    }
    if (theInBytesAsList && theInMessagesAsList && theOutBytesAsList && theOutMessagesAsList)
    {
        switch (flavour)
        {
            case kOutputFlavourTabs :
                if (sawSome)
                {
                    result << endl;
                }
                result << theChannelAsString.c_str() << "\t" << theDateAsString.c_str() << "\t" <<
                        theTimeAsString.c_str() << "\t" << inByteCount << "\t" <<
                        outByteCount << "\t" << inMessageCount << "\t" << outMessageCount;
                break;
                
            case kOutputFlavourJSON :
                if (sawSome)
                {
                    result << ", ";
                }
                result << T_("{ " CHAR_DOUBLEQUOTE_ "channel" CHAR_DOUBLEQUOTE_ ": "
                             CHAR_DOUBLEQUOTE_) << SanitizeString(theChannelAsString).c_str() <<
                        T_(CHAR_DOUBLEQUOTE_ ", " CHAR_DOUBLEQUOTE_ "date" CHAR_DOUBLEQUOTE_ ": "
                           CHAR_DOUBLEQUOTE_) << theDateAsString.c_str() <<
                        T_(CHAR_DOUBLEQUOTE_ ", " CHAR_DOUBLEQUOTE_ "time" CHAR_DOUBLEQUOTE_ ": "
                           CHAR_DOUBLEQUOTE_) << theTimeAsString.c_str() <<
                        T_(CHAR_DOUBLEQUOTE_ ", " CHAR_DOUBLEQUOTE_ "inBytes" CHAR_DOUBLEQUOTE_
                           ": " CHAR_DOUBLEQUOTE_) << inByteCount <<
                        T_(CHAR_DOUBLEQUOTE_ ", " CHAR_DOUBLEQUOTE_ "inMessages" CHAR_DOUBLEQUOTE_
                           ": " CHAR_DOUBLEQUOTE_) << inMessageCount <<
                        T_(CHAR_DOUBLEQUOTE_ ", " CHAR_DOUBLEQUOTE_ "outBytes" CHAR_DOUBLEQUOTE_
                           ": " CHAR_DOUBLEQUOTE_) << outByteCount <<
                        T_(CHAR_DOUBLEQUOTE_ ", " CHAR_DOUBLEQUOTE_ "outMessages" CHAR_DOUBLEQUOTE_
                           ": " CHAR_DOUBLEQUOTE_) << outMessageCount <<
                        T_(CHAR_DOUBLEQUOTE_ " }");
                break;
                
            case kOutputFlavourNormal :
                if (sawSome)
                {
                    result << endl;
                }
                result.width(channelWidth);
                result << theChannelAsString.c_str() << ": [date: " << theDateAsString.c_str() <<
                        ", time: " << theTimeAsString.c_str() << ", bytes in: " <<
                        inByteCount << ", out: " << outByteCount << ", messages in: " <<
                        inMessageCount << ", out: " << outMessageCount << "]";
                break;
                
            default :
                break;
                
        }
        sawSome = true;
    }
} // convertMetricPropertyToString

/*! @brief Process the response from the name server.
 
 Note that each line of the response, except the last, is started with 'registration name'. This is
 followed by the port name, 'ip', the IP address, 'port' and the port number.
 @param received The response to be processed.
 @param includeHiddenPorts @c true if all ports are returned and @c false is 'hidden' ports are
 ignored.
 @param ports The list of non-default ports/ipaddress/portnumber found. */
static void processNameServerResponse(const YarpString & received,
                                      const bool         includeHiddenPorts,
                                      PortVector &       ports)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S1s("received = ", received); //####
    OD_LOG_B1("includeHiddenPorts = ", includeHiddenPorts); //####
    size_t     lineMakerLength = strlen(kLineMarker);
    YarpString nameServerName(yarp::os::Network::getNameServerName());
    YarpString workingCopy(received);
    
    OD_LOG_S1s("nameServerName = ", nameServerName); //####
    for (size_t nextPos = 0; YarpString::npos != nextPos; )
    {
        nextPos = workingCopy.find(kLineMarker);
        if (YarpString::npos != nextPos)
        {
            workingCopy = workingCopy.substr(nextPos + lineMakerLength);
            size_t chopPos = workingCopy.find(kLineMarker);
            
            if (YarpString::npos != chopPos)
            {
                char *     channelName = NULL;
                YarpString chopped(workingCopy.substr(0, chopPos));
#if MAC_OR_LINUX_
                char *     choppedAsChars = strdup(chopped.c_str());
#else // ! MAC_OR_LINUX_
                char *     choppedAsChars = _strdup(chopped.c_str());
#endif // ! MAC_OR_LINUX_
                char *     ipAddress = NULL;
                char *     saved;
                char *     pp = strtok_r(choppedAsChars, " ", &saved);
                
                if (pp)
                {
                    // Port name
                    if ('/' == *pp)
                    {
                        channelName = pp;
                        if (nameServerName == channelName)
                        {
                            pp = NULL;
                        }
                        else
                        {
                            pp = strtok_r(NULL, " ", &saved);
                        }
                    }
                    else
                    {
                        pp = NULL;
                    }
                }
                if (pp)
                {
                    // 'ip'
                    if (strcmp(pp, "ip"))
                    {
                        pp = NULL;
                    }
                    else
                    {
                        pp = strtok_r(NULL, " ", &saved);
                    }
                }
                if (pp)
                {
                    ipAddress = pp;
                    pp = strtok_r(NULL, " ", &saved);
                }
                if (pp)
                {
                    // 'port'
                    if (strcmp(pp, "port"))
                    {
                        pp = NULL;
                    }
                    else
                    {
                        pp = strtok_r(NULL, " ", &saved);
                    }
                }
                // Check if this is a 'hidden' port:
                if (pp && (! includeHiddenPorts) && channelName)
                {
                    if (! strncmp(channelName, HIDDEN_CHANNEL_PREFIX_,
                                  sizeof(HIDDEN_CHANNEL_PREFIX_) - 1))
                    {
                        // Skip this one.
                        pp = NULL;
                    }
                }
                if (pp)
                {
                    PortDescriptor aDescriptor;
                    
                    aDescriptor._portName = channelName;
                    aDescriptor._portIpAddress = ipAddress;
                    aDescriptor._portPortNumber = pp;
                    ports.push_back(aDescriptor);
                }
                free(choppedAsChars);
            }
        }
    }
    OD_LOG_EXIT(); //####
} // processNameServerResponse

/*! @brief Convert a YARP value into a JSON element.
 @param outBuffer The buffer to be written to.
 @param inputValue The value to be processed. */
#if defined(MpM_UseCustomStringBuffer)
static void processValue(Common::StringBuffer &  outBuffer,
                         const yarp::os::Value & inputValue);
#else // ! defined(MpM_UseCustomStringBuffer)
static void processValue(std::stringstream &     outBuffer,
                         const yarp::os::Value & inputValue);
#endif // ! defined(MpM_UseCustomStringBuffer)

/*! @brief Convert a YARP string into a JSON string.
 @param outBuffer The buffer to be written to.
 @param inputString The string to be processed. */
#if defined(MpM_UseCustomStringBuffer)
static void processString(Common::StringBuffer & outBuffer,
                          const YarpString &     inputString)
#else // ! defined(MpM_UseCustomStringBuffer)
static void processString(std::stringstream & outBuffer,
                          const YarpString &  inputString)
#endif // ! defined(MpM_UseCustomStringBuffer)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("outBuffer = ", &outBuffer, "inputString = ", &inputString); //####
#if defined(MpM_UseCustomStringBuffer)
    outBuffer.addChar('"');
#else // ! defined(MpM_UseCustomStringBuffer)
    outBuffer << '"';
#endif // ! defined(MpM_UseCustomStringBuffer)
    for (size_t ii = 0, mm = inputString.length(); mm > ii; ++ii)
    {
        char aChar = inputString[ii];
        
        switch (aChar)
        {
            case '\\' :
            case '"' :
            case '/' :
#if defined(MpM_UseCustomStringBuffer)
                outBuffer.addChar(kEscapeChar).addChar(aChar);
#else // ! defined(MpM_UseCustomStringBuffer)
                outBuffer << kEscapeChar << aChar;
#endif // ! defined(MpM_UseCustomStringBuffer)
                break;
                
            case '\b' :
#if defined(MpM_UseCustomStringBuffer)
                outBuffer.addChar(kEscapeChar).addChar('b');
#else // ! defined(MpM_UseCustomStringBuffer)
                outBuffer << kEscapeChar << 'b';
#endif // ! defined(MpM_UseCustomStringBuffer)
                break;
                
            case '\f' :
#if defined(MpM_UseCustomStringBuffer)
                outBuffer.addChar(kEscapeChar).addChar('f');
#else // ! defined(MpM_UseCustomStringBuffer)
                outBuffer << kEscapeChar << 'f';
#endif // ! defined(MpM_UseCustomStringBuffer)
                break;
                
            case '\n' :
#if defined(MpM_UseCustomStringBuffer)
                outBuffer.addChar(kEscapeChar).addChar('n');
#else // ! defined(MpM_UseCustomStringBuffer)
                outBuffer << kEscapeChar << 'n';
#endif // ! defined(MpM_UseCustomStringBuffer)
                break;
                
            case '\r' :
#if defined(MpM_UseCustomStringBuffer)
                outBuffer.addChar(kEscapeChar).addChar('r');
#else // ! defined(MpM_UseCustomStringBuffer)
                outBuffer << kEscapeChar << 'r';
#endif // ! defined(MpM_UseCustomStringBuffer)
                break;
                
            case '\t' :
#if defined(MpM_UseCustomStringBuffer)
                outBuffer.addChar(kEscapeChar).addChar('t');
#else // ! defined(MpM_UseCustomStringBuffer)
                outBuffer << kEscapeChar << 't';
#endif // ! defined(MpM_UseCustomStringBuffer)
                break;
                
            default :
#if defined(MpM_UseCustomStringBuffer)
                outBuffer.addChar(aChar);
#else // ! defined(MpM_UseCustomStringBuffer)
                outBuffer << aChar;
#endif // ! defined(MpM_UseCustomStringBuffer)
                break;
                
        }
    }
#if defined(MpM_UseCustomStringBuffer)
    outBuffer.addChar('"');
#else // ! defined(MpM_UseCustomStringBuffer)
    outBuffer << '"';
#endif // ! defined(MpM_UseCustomStringBuffer)
    OD_LOG_EXIT(); //####
} // processString

/*! @brief Convert a YARP dictionary into a JSON object.
 @param outBuffer The buffer to be written to.
 @param inputDictionary The dictionary to be processed. */
#if defined(MpM_UseCustomStringBuffer)
static void processDictionary(Common::StringBuffer &     outBuffer,
                              const yarp::os::Property & inputDictionary)
#else // ! defined(MpM_UseCustomStringBuffer)
static void processDictionary(std::stringstream &        outBuffer,
                              const yarp::os::Property & inputDictionary)
#endif // ! defined(MpM_UseCustomStringBuffer)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("outBuffer = ", &outBuffer, "inputDictionary = ", &inputDictionary); //####
    yarp::os::Bottle asList(inputDictionary.toString());
    
    // A dictionary converted to a string is a list of two-element lists, with the key as the first
    // entry and the value as the second.
#if defined(MpM_UseCustomStringBuffer)
    outBuffer.addString("{ ");
#else // ! defined(MpM_UseCustomStringBuffer)
    outBuffer << "{ ";
#endif // ! defined(MpM_UseCustomStringBuffer)
    for (int ii = 0, mm = asList.size(); mm > ii; ++ii)
    {
        yarp::os::Value anEntry(asList.get(ii));
        
        if (anEntry.isList())
        {
            yarp::os::Bottle * entryAsList = anEntry.asList();
            
            if (entryAsList && (2 == entryAsList->size()))
            {
                if (0 < ii)
                {
#if defined(MpM_UseCustomStringBuffer)
                    outBuffer.addString(", ");
#else // ! defined(MpM_UseCustomStringBuffer)
                    outBuffer << ", ";
#endif // ! defined(MpM_UseCustomStringBuffer)
                }
                processString(outBuffer, entryAsList->get(0).toString());
#if defined(MpM_UseCustomStringBuffer)
                outBuffer.addString(" : ");
#else // ! defined(MpM_UseCustomStringBuffer)
                outBuffer << " : ";
#endif // ! defined(MpM_UseCustomStringBuffer)
                processValue(outBuffer, entryAsList->get(1));
            }
        }
    }
#if defined(MpM_UseCustomStringBuffer)
    outBuffer.addString(" }");
#else // ! defined(MpM_UseCustomStringBuffer)
    outBuffer << " }";
#endif // ! defined(MpM_UseCustomStringBuffer)
    OD_LOG_EXIT(); //####
} // processDictionary

/*! @brief Convert a YARP list into a JSON array.
 @param outBuffer The buffer to be written to.
 @param inputList The list to be processed. */
#if defined(MpM_UseCustomStringBuffer)
static void processList(Common::StringBuffer &   outBuffer,
                        const yarp::os::Bottle & inputList)
#else // ! defined(MpM_UseCustomStringBuffer)
static void processList(std::stringstream &      outBuffer,
                        const yarp::os::Bottle & inputList)
#endif // ! defined(MpM_UseCustomStringBuffer)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("outBuffer = ", &outBuffer, "inputList = ", &inputList); //####
#if defined(MpM_UseCustomStringBuffer)
    outBuffer.addString("[ ");
#else // ! defined(MpM_UseCustomStringBuffer)
    outBuffer << "( ";
#endif // ! defined(MpM_UseCustomStringBuffer)
    for (int ii = 0, mm = inputList.size(); mm > ii; ++ii)
    {
        yarp::os::Value aValue(inputList.get(ii));
        
        if (0 < ii)
        {
#if defined(MpM_UseCustomStringBuffer)
            outBuffer.addString(", ");
#else // ! defined(MpM_UseCustomStringBuffer)
            outBuffer << ", ";
#endif // ! defined(MpM_UseCustomStringBuffer)
        }
        processValue(outBuffer, aValue);
    }
#if defined(MpM_UseCustomStringBuffer)
    outBuffer.addString(" ]");
#else // ! defined(MpM_UseCustomStringBuffer)
    outBuffer << " ]";
#endif // ! defined(MpM_UseCustomStringBuffer)
    OD_LOG_EXIT(); //####
} // processList

#if defined(MpM_UseCustomStringBuffer)
static void processValue(Common::StringBuffer &  outBuffer,
                         const yarp::os::Value & inputValue)
#else // ! defined(MpM_UseCustomStringBuffer)
static void processValue(std::stringstream &     outBuffer,
                         const yarp::os::Value & inputValue)
#endif // ! defined(MpM_UseCustomStringBuffer)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("outBuffer = ", &outBuffer, "inputValue = ", &inputValue); //####
    if (inputValue.isBool())
    {
        bool value = inputValue.asBool();
        
#if defined(MpM_UseCustomStringBuffer)
        outBuffer.addString(value ? "true" : "false");
#else // ! defined(MpM_UseCustomStringBuffer)
        outBuffer << (value ? "true" : "false");
#endif // ! defined(MpM_UseCustomStringBuffer)
    }
    else if (inputValue.isInt())
    {
        int value = inputValue.asInt();
        
#if defined(MpM_UseCustomStringBuffer)
        outBuffer.addLong(value);
#else // ! defined(MpM_UseCustomStringBuffer)
        outBuffer << value;
#endif // ! defined(MpM_UseCustomStringBuffer)
    }
    else if (inputValue.isString())
    {
        YarpString value = inputValue.asString();
        
        processString(outBuffer, value);
    }
    else if (inputValue.isDouble())
    {
        double value = inputValue.asDouble();
        
#if defined(MpM_UseCustomStringBuffer)
        outBuffer.addDouble(value);
#else // ! defined(MpM_UseCustomStringBuffer)
        outBuffer << value;
#endif // ! defined(MpM_UseCustomStringBuffer)
    }
    else if (inputValue.isDict())
    {
        yarp::os::Property * value = inputValue.asDict();
        
        if (value)
        {
            processDictionary(outBuffer, *value);
        }
    }
    else if (inputValue.isList())
    {
        yarp::os::Bottle * value = inputValue.asList();
        
        if (value)
        {
            yarp::os::Property asDict;
            
            if (ListIsReallyDictionary(*value, asDict))
            {
                processDictionary(outBuffer, asDict);
            }
            else
            {
                processList(outBuffer, *value);
            }
        }
    }
    else
    {
        // We don't know what to do with this...
#if defined(MpM_UseCustomStringBuffer)
        outBuffer.addString("null");
#else // ! defined(MpM_UseCustomStringBuffer)
        outBuffer << "null";
#endif // ! defined(MpM_UseCustomStringBuffer)
    }
    OD_LOG_EXIT(); //####
} // processValue

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

bool Utilities::AddConnection(const YarpString & fromPortName,
                              const YarpString & toPortName,
                              const double       timeToWait,
                              const bool         isUDP,
                              CheckFunction      checker,
                              void *             checkStuff)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S2s("fromPortName = ", fromPortName, "toPortName = ", toPortName); //####
    OD_LOG_D1("timeToWait = ", timeToWait); //####
    OD_LOG_B1("isUDP = ", isUDP); //####
    OD_LOG_P1("checkStuff = ", checkStuff); //####
    bool result = NetworkConnectWithRetries(fromPortName, toPortName, timeToWait, isUDP, checker,
                                            checkStuff);
    
    OD_LOG_EXIT_B(result); //####
    return result;
} // Utilities::AddConnection

bool Utilities::CheckConnection(const YarpString & fromPortName,
                                const YarpString & toPortName)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S2s("fromPortName = ", fromPortName, "toPortName = ", toPortName); //####
    bool result = yarp::os::Network::isConnected(fromPortName, toPortName);
    
    OD_LOG_EXIT_B(result); //####
    return result;
} // Utilities::CheckConnection

bool Utilities::CheckForChannel(const YarpString & channelName)
{
    OD_LOG_ENTER(); //####
    bool result = yarp::os::Network::exists(channelName);

    OD_LOG_EXIT_B(result); //####
    return result;
} // Utilities::CheckForChannel

void Utilities::CheckForNameServerReporter(void)
{
    OD_LOG_ENTER(); //####
	bool                       skipNameServerScan = false;
    yarp::os::impl::NameConfig nc;

    // First, see if there is a configuration file and it points to a real server.
#if defined(CONFIG_FILE_AVAILABLE_)
    if (nc.fromFile())
    {
        OD_LOG("(nc.fromFile())"); //####
        yarp::os::Contact address = nc.getAddress();
        
        if (address.isValid())
        {
            OD_LOG("(address.isValid())"); //####
            yarp::os::impl::TcpFace    aFace;
            yarp::os::OutputProtocol * outp = aFace.write(address);

            if (outp)
            {
                OD_LOG("(outp)"); //####
                delete outp;
                skipNameServerScan = true;
            }
        }
    }
#endif // defined(CONFIG_FILE_AVAILABLE_)
    if (! skipNameServerScan)
    {
        OD_LOG("(! skipNameServerScan)"); //####
        lSawBrowseAdd = false;
        DNSServiceRef       serviceRef = NULL;
        static const char * regType = MpM_MDNS_NAMESERVER_REPORT_;
        DNSServiceErrorType err = DNSServiceBrowse(&serviceRef, 0, 0, regType, NULL /* domain */,
                                                   browseCallBack, NULL);
        
        if (kDNSServiceErr_NoError == err)
        {
            OD_LOG("(kDNSServiceErr_NoError == err)"); //####
            // handle events.
            int            dns_sd_fd = DNSServiceRefSockFD(serviceRef);
            int            nfds = dns_sd_fd + 1;
            fd_set         readfds;
            struct timeval tv;
            int            result;
            
            for ( ; ! lSawBrowseAdd; )
            {
                FD_ZERO(&readfds);
                FD_SET(dns_sd_fd, &readfds);
                tv.tv_sec = kDNSWaitTime; // We don't want to wait forever.
                tv.tv_usec = 0;
                result = select(nfds, &readfds, NULL, NULL, &tv);
                if (0 < result)
                {
                    DNSServiceErrorType err = kDNSServiceErr_NoError;
                    
                    if (FD_ISSET(dns_sd_fd, &readfds))
                    {
                        err = DNSServiceProcessResult(serviceRef);
                    }
                    if (err)
                    {
                        cerr << "DNSServiceProcessResult returned " << err << endl;
                        break;
                    }
                    
                }
                else if (0 != result)
                {
                    int  actErrno = errno;
                    char errBuff[256];
                    
#if MAC_OR_LINUX_
                    strerror_r(actErrno, errBuff, sizeof(errBuff));
#else // ! MAC_OR_LINUX_
                    strerror_s(errBuff, sizeof(errBuff), actErrno);
#endif // ! MAC_OR_LINUX_
                    cerr << "select() returned " << result << " errno " << actErrno << " " <<
                            errBuff << endl;
                    if (EINTR != actErrno)
                    {
                        break;
                    }
                    
                }
                else
                {
                    // Ran out of time - maybe there is no NameServerReporter running...
                    break;
                }
                
            }
            DNSServiceRefDeallocate(serviceRef);
        }
    }
    OD_LOG_EXIT(); //####
} // Utilities::CheckForNameServerReporter

bool Utilities::CheckForRegistryService(void)
{
    OD_LOG_ENTER(); //####
    bool result = CheckForChannel(MpM_REGISTRY_ENDPOINT_NAME_);
    
    OD_LOG_EXIT_B(result); //####
    return result;
} // Utilities::CheckForRegistryService

bool Utilities::CheckForValidNetwork(const bool quiet)
{
    OD_LOG_ENTER(); //####
    OD_LOG_B1("quiet =", quiet); //####
    bool result;
    
    if (yarp::os::Network::checkNetwork(NETWORK_CHECK_TIMEOUT_))
    {
        result = true;
    }
    else
    {
        OD_LOG("! (yarp::os::Network::checkNetwork(NETWORK_CHECK_TIMEOUT_))"); //####
        result = false;
    }
    if (result)
    {
        // Make a test probe of the network, just to double-check
        yarp::os::Bottle  response;
        yarp::os::Network yarp; // This is necessary to establish any connections to the YARP
                                // infrastructure
        
        // Ask the YARP name server for a list of ports - we don't care about the actual list, this
        // is a secondary check to see if there really is a YARP network available.
        result = getNameServerPortList(response);
    }
    if ((! result) && (! quiet))
    {
#if MAC_OR_LINUX_
        GetLogger().fail("YARP network not running.");
#else // ! MAC_OR_LINUX_
        cerr << "YARP network not running." << endl;
#endif // ! MAC_OR_LINUX_
    }
    OD_LOG_EXIT_B(result); //####
    return result;
} // Utilities::CheckForValidNetwork

bool Utilities::CheckListForRegistryService(const PortVector & ports)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P1("ports = ", &ports); //####
    bool result = false;
    
    if (0 < ports.size())
    {
        for (PortVector::const_iterator walker(ports.begin()); ports.end() != walker; ++walker)
        {
            if (walker->_portName == MpM_REGISTRY_ENDPOINT_NAME_)
            {
                result = true;
                break;
            }
            
        }
    }
    OD_LOG_EXIT_B(result); //####
    return result;
} // Utilities::CheckListForRegistryService

#if defined(MpM_UseCustomStringBuffer)
void Utilities::ConvertMessageToJSON(Common::StringBuffer &   outBuffer,
                                     const yarp::os::Bottle & input)
#else // ! defined(MpM_UseCustomStringBuffer)
void Utilities::ConvertMessageToJSON(std::stringstream &      outBuffer,
                                     const yarp::os::Bottle & input)
#endif // ! defined(MpM_UseCustomStringBuffer)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("outBuffer = ", &outBuffer, "input = ", &input); //####
    int     mm = input.size();
    int64_t now = GetCurrentTimeInMilliseconds();
    
#if defined(MpM_UseCustomStringBuffer)
    outBuffer.reset();
    outBuffer.addString("{ \"time\" : ");
    outBuffer.addLong(now).addString(", \"value\" : ");
#else // ! defined(MpM_UseCustomStringBuffer)
    outBuffer.seekp(0);
    outBuffer << "{ \"time\" : " << now << ", \"value\" : ";
#endif // ! defined(MpM_UseCustomStringBuffer)
    if (1 == mm)
    {
        processValue(outBuffer, input.get(0));
    }
    else if (1 < mm)
    {
        processList(outBuffer, input);
    }
    else
    {
#if defined(MpM_UseCustomStringBuffer)
        outBuffer.addString("null");
#else // ! defined(MpM_UseCustomStringBuffer)
        outBuffer << "null";
#endif // ! defined(MpM_UseCustomStringBuffer)
    }
#if defined(MpM_UseCustomStringBuffer)
    outBuffer.addString(" }\n");
#else // ! defined(MpM_UseCustomStringBuffer)
    outBuffer << " }\n";
#endif // ! defined(MpM_UseCustomStringBuffer)
    OD_LOG_EXIT(); //####
} // Utilities::ConvertMessageToJSON

YarpString Utilities::ConvertMetricsToString(const yarp::os::Bottle & metrics,
                                             const OutputFlavour      flavour)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P1("metrics = ", &metrics); //####
    bool                  sawSome = false;
    size_t                channelWidth = 0;
    std::stringstream     result;
    
    // First, calculate the tag width:
    if (kOutputFlavourNormal == flavour)
    {
        for (int ii = 0, maxm = metrics.size(); maxm > ii; ++ii)
        {
            yarp::os::Value & aValue(metrics.get(ii));
            
            if (aValue.isDict())
            {
                yarp::os::Property * propList = aValue.asDict();
                
                if (propList)
                {
                    YarpString theTagAsString;
                    
                    if (propList->check(MpM_SENDRECEIVE_CHANNEL_))
                    {
                        yarp::os::Value theChannel = propList->find(MpM_SENDRECEIVE_CHANNEL_);
                        
                        if (theChannel.isString())
                        {
                            size_t ww = theChannel.toString().length();
                            
                            if (channelWidth < ww)
                            {
                                channelWidth = ww;
                            }
                        }
                    }
                }
            }
            else if (aValue.isList())
            {
                yarp::os::Bottle * asList = aValue.asList();
                
                if (asList)
                {
                    yarp::os::Property propList;
                    
                    if (ListIsReallyDictionary(*asList, propList))
                    {
                        YarpString theTagAsString;
                        
                        if (propList.check(MpM_SENDRECEIVE_CHANNEL_))
                        {
                            yarp::os::Value theChannel = propList.find(MpM_SENDRECEIVE_CHANNEL_);
                            
                            if (theChannel.isString())
                            {
                                size_t ww = theChannel.toString().length();
                                
                                if (channelWidth < ww)
                                {
                                    channelWidth = ww;
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    if (kOutputFlavourJSON == flavour)
    {
        result << "[ ";
    }
    for (int ii = 0, maxm = metrics.size(); maxm > ii; ++ii)
    {
        yarp::os::Value & aValue(metrics.get(ii));
        
        if (aValue.isDict())
        {
            yarp::os::Property * propList = aValue.asDict();
            
            if (propList)
            {
                convertMetricPropertyToString(*propList, flavour, channelWidth, sawSome, result);
            }
        }
        else if (aValue.isList())
        {
            yarp::os::Bottle * asList = aValue.asList();
            
            if (asList)
            {
                yarp::os::Property propList;
                
                if (ListIsReallyDictionary(*asList, propList))
                {
                    convertMetricPropertyToString(propList, flavour, channelWidth, sawSome, result);
                }
            }
        }
    }
    if (kOutputFlavourJSON == flavour)
    {
        result << " ]";
    }
    OD_LOG_EXIT_s(result.str()); //####
    return result.str();
} // Utilities::ConvertMetricsToString

void Utilities::GatherPortConnections(const YarpString &    portName,
                                      ChannelVector &       inputs,
                                      ChannelVector &       outputs,
                                      const InputOutputFlag which,
                                      const bool            quiet,
                                      CheckFunction         checker,
                                      void *                checkStuff)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S1s("portName = ", portName); //####
    OD_LOG_P3("inputs = ", &inputs, "outputs = ", &outputs, "checkStuff = ", checkStuff); //####
    OD_LOG_L1("which = ", static_cast<int>(which)); //####
    OD_LOG_B1("quiet = ", quiet); //####
    inputs.clear();
    outputs.clear();
    try
    {
        yarp::os::Contact address = yarp::os::Network::queryName(portName.c_str());
    
        if (address.isValid())
        {
            if ((address.getCarrier() == "tcp") || (address.getCarrier() == "fast_tcp") ||
                (address.getCarrier() == "xmlrpc"))
            {
                // Note that the following connect() call will hang indefinitely if the address
                // given is for an 'output' port that is connected to another 'output' port.
                // 'yarp ping /port' will hang as well.
                yarp::os::OutputProtocol * out = yarp::os::impl::Carriers::connect(address);
            
                if (out)
                {
                    yarp::os::Route rr(kMagicName, portName.c_str(), "text_ack");
                
                    if (out->open(rr))
                    {
                        yarp::os::Bottle                         resp;
                        yarp::os::impl::BufferedConnectionWriter
                                                            bw(out->getConnection().isTextMode());
                        yarp::os::InputStream &                  is = out->getInputStream();
                        yarp::os::OutputStream &                 os = out->getOutputStream();
                        yarp::os::impl::PortCommand              pc(0, "*");
                        yarp::os::impl::StreamConnectionReader   reader;
                    
                        pc.write(bw);
                        bw.write(os);
                        reader.reset(is, NULL, rr, 0, true);
                        for (bool done = false; ! done; )
                        {
                            if (checker && checker(checkStuff))
                            {
                                break;
                            }
                            
                            resp.read(reader);
                            YarpString checkString(resp.get(0).asString());
                        
                            if (checkString == "<ACK>")
                            {
                                done = true;
                            }
                            else if (checkString == "There")
                            {
                                if (static_cast<int>(which) &
                                    static_cast<int>(kInputAndOutputInput))
                                {
                                    checkForInputConnection(resp, inputs);
                                }
                                if (static_cast<int>(which) &
                                    static_cast<int>(kInputAndOutputOutput))
                                {
                                    checkForOutputConnection(resp, outputs);
                                }
                            }
                        }
                    }
                    else if (! quiet)
                    {
#if MAC_OR_LINUX_
                        GetLogger().fail("Could not open route to port.");
#endif // MAC_OR_LINUX_
                    }
                    delete out;
                }
                else if (! quiet)
                {
#if MAC_OR_LINUX_
                    GetLogger().fail("Could not connect to port.");
#endif // MAC_OR_LINUX_
                }
            }
            else if (! quiet)
            {
#if MAC_OR_LINUX_
                GetLogger().fail("Port not using recognized connection type.");
#endif // MAC_OR_LINUX_
            }
        }
        else if (! quiet)
        {
#if MAC_OR_LINUX_
            GetLogger().fail("Port name not recognized.");
#endif // MAC_OR_LINUX_
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_EXIT(); //####
} // Utilities::GatherPortConnections

bool Utilities::GetConfigurationForService(const YarpString & serviceChannelName,
                                           YarpStringVector & values,
                                           const double       timeToWait,
                                           CheckFunction      checker,
                                           void *             checkStuff)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S1s("serviceChannelName = ", serviceChannelName); //####
    OD_LOG_P2("values = ", &values, "checkStuff = ", checkStuff); //####
    OD_LOG_D1("timeToWait = ", timeToWait); //####
    bool            result = false;
    YarpString      aName(GetRandomChannelName(HIDDEN_CHANNEL_PREFIX_ "configuration_/"
                                               DEFAULT_CHANNEL_ROOT_));
    ClientChannel * newChannel = new ClientChannel;
    
    values.clear();
    if (newChannel)
    {
        if (newChannel->openWithRetries(aName, timeToWait))
        {
            if (NetworkConnectWithRetries(aName, serviceChannelName, timeToWait, false, checker,
                                          checkStuff))
            {
                yarp::os::Bottle parameters;
                ServiceRequest   request(MpM_CONFIGURATION_REQUEST_, parameters);
                ServiceResponse  response;
                
                if (request.send(*newChannel, response))
                {
                    OD_LOG_S1s("response <- ", response.asString()); //####
                    // Note that only input / output services will respond to this request.
                    if (response.asString() != MpM_UNRECOGNIZED_REQUEST_)
                    {
                        for (int ii = 0, howMany = response.count(); howMany > ii; ++ii)
                        {
                            yarp::os::Value aValue(response.element(ii));
                            
                            values.push_back(aValue.toString());
                        }
                        result = true;
                    }
                }
                else
                {
                    OD_LOG("! (request.send(*newChannel, response))"); //####
                }
#if defined(MpM_DoExplicitDisconnect)
                if (! NetworkDisconnectWithRetries(aName, serviceChannelName, timeToWait, checker,
                                                   checkStuff))
                {
                    OD_LOG("(! NetworkDisconnectWithRetries(aName, destinationName, " //####
                           "timeToWait, checker, checkStuff))"); //####
                }
#endif // defined(MpM_DoExplicitDisconnect)
            }
            else
            {
                OD_LOG("! (NetworkConnectWithRetries(aName, serviceChannelName, timetoWait, " //####
                       "false, checker, checkStuff))"); //####
            }
#if defined(MpM_DoExplicitClose)
            newChannel->close();
#endif // defined(MpM_DoExplicitClose)
        }
        else
        {
            OD_LOG("! (newChannel->openWithRetries(aName, timeToWait))"); //####
        }
        delete newChannel;
    }
    else
    {
        OD_LOG("! (newChannel)"); //####
    }
    OD_LOG_EXIT_B(result); //####
    return result;
} // Utilities::GetConfigurationForService

int64_t Utilities::GetCurrentTimeInMilliseconds(void)
{
    OD_LOG_ENTER(); //####
    int64_t        result;
#if MAC_OR_LINUX_
    struct timeval tv;
#else // ! MAC_OR_LINUX_
    struct _timeb  tt;
#endif // ! MAC_OR_LINUX_
    
#if MAC_OR_LINUX_
    gettimeofday(&tv, NULL);
    result = (static_cast<int64_t>(tv.tv_sec) * 1000) + (tv.tv_usec / 1000);
#else // ! MAC_OR_LINUX_
    _ftime_s(&tt);
    result = (static_cast<int64_t>(tt.time) * 1000) + tt.millitm;
#endif // ! MAC_OR_LINUX_
    OD_LOG_EXIT_LL(result); //####
    return result;
} // Utilities::GetCurrentTimeInMilliseconds

bool Utilities::GetCurrentYarpConfiguration(struct in_addr & serverAddress,
                                            int &            serverPort)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("serverAddress = ", &serverAddress, "serverPort = ", &serverPort); //####
    bool                       okSoFar = false;
    yarp::os::impl::NameConfig nc;

    if (nc.fromFile())
    {
        yarp::os::Contact aContact(nc.getAddress());
        YarpString        hostName(aContact.getHost());

		if (hostName == SELF_ADDRESS_NAME_)
		{
			hostName = SELF_ADDRESS_IPADDR_;
		}
#if MAC_OR_LINUX_
        int res = inet_pton(AF_INET, hostName.c_str(), &serverAddress);
#else // ! MAC_OR_LINUX_
        int res = InetPton(AF_INET, hostName.c_str(), &serverAddress);
#endif // ! MAC_OR_LINUX_

        serverPort = aContact.getPort();
        okSoFar = (0 < res);
    }
    OD_LOG_EXIT_B(okSoFar); //####
    return okSoFar;
} // Utilities::GetCurrentYarpConfiguration

void Utilities::GetDateAndTime(char *       dateBuffer,
                               const size_t dateBufferSize,
                               char *       timeBuffer,
                               const size_t timeBufferSize)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("dateBuffer = ", dateBuffer, "timeBuffer = ", timeBuffer); //####
    OD_LOG_L2("dateBufferSize = ", dateBufferSize, "timeBufferSize = ", timeBufferSize); //####
    time_t    rawtime;
    struct tm locTime;
    
    time(&rawtime);
    memset(&locTime, 0, sizeof(locTime));
#if MAC_OR_LINUX_
    localtime_r(&rawtime, &locTime);
    strftime(dateBuffer, dateBufferSize, "%F", &locTime);
    strftime(timeBuffer, timeBufferSize, "%T", &locTime);
#else // ! MAC_OR_LINUX_
    localtime_s(&locTime, &rawtime);
    strftime(dateBuffer, dateBufferSize, "%x", &locTime);
    strftime(timeBuffer, timeBufferSize, "%X", &locTime);
#endif // ! MAC_OR_LINUX_
    OD_LOG_EXIT(); //####
} // Utilities::GetDateAndTime

bool Utilities::GetDetectedPortList(PortVector & ports,
                                    const bool   includeHiddenPorts)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P1("ports = ", &ports); //####
    OD_LOG_B1("includeHiddenPorts = ", includeHiddenPorts); //####
    yarp::os::Bottle response;
    bool             okSoFar = getNameServerPortList(response);
    
    ports.clear();
    if (okSoFar)
    {
        yarp::os::Value responseValue(response.get(0));
        
        if (responseValue.isString())
        {
            processNameServerResponse(responseValue.asString(), includeHiddenPorts, ports);
        }
        else
        {
            OD_LOG("! (responseValue.isString())"); //####
            okSoFar = false;
        }
    }
    if (! okSoFar)
    {
        char buffer1[DATE_TIME_BUFFER_SIZE_];
        char buffer2[DATE_TIME_BUFFER_SIZE_];
        
        GetDateAndTime(buffer1, sizeof(buffer1), buffer2, sizeof(buffer2));
        cerr << buffer1 << " " << buffer2 << "Problem getting list of ports." << endl;
    }
    OD_LOG_EXIT_B(okSoFar); //####
    return okSoFar;
} // Utilities::GetDetectedPortList

bool Utilities::GetExtraInformationForService(const YarpString &  serviceChannelName,
                                              YarpString &        extraInfo,
                                              const double        timeToWait,
                                              CheckFunction       checker,
                                              void *              checkStuff)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S1s("serviceChannelName = ", serviceChannelName); //####
    OD_LOG_P2("extraInfo = ", &extraInfo, "checkStuff = ", checkStuff); //####
    OD_LOG_D1("timeToWait = ", timeToWait); //####
    bool result = false;
    
    try
    {
        YarpString      aName(GetRandomChannelName(HIDDEN_CHANNEL_PREFIX_ "extraInfo_/"
                                                   DEFAULT_CHANNEL_ROOT_));
        ClientChannel * newChannel = new ClientChannel;
        
        if (newChannel)
        {
            if (newChannel->openWithRetries(aName, timeToWait))
            {
                if (NetworkConnectWithRetries(aName, serviceChannelName, timeToWait, false, checker,
                                              checkStuff))
                {
                    yarp::os::Bottle parameters1;
                    ServiceRequest   request1(MpM_EXTRAINFO_REQUEST_, parameters1);
                    ServiceResponse  response1;
                    
                    if (request1.send(*newChannel, response1))
                    {
                        OD_LOG_S1s("response1 <- ", response1.asString()); //####
                        if (MpM_EXPECTED_EXTRAINFO_RESPONSE_SIZE_ == response1.count())
                        {
                            yarp::os::Value theExtraInfo(response1.element(0));
                            
                            OD_LOG_S1s("theExtraInfo <- ", theExtraInfo.toString()); //####
                            if (theExtraInfo.isString())
                            {
                                extraInfo = theExtraInfo.toString();
                                result = true;
                            }
                            else
                            {
                                OD_LOG("! (theExtraInfo.isString())"); //####
                            }
                        }
                        else
                        {
                            OD_LOG("! (MpM_EXPECTED_EXTRAINFO_RESPONSE_SIZE_ == " //####
                                   "response1.count())"); //####
                            OD_LOG_S1s("response1 = ", response1.asString()); //####
                        }
                    }
                    else
                    {
                        OD_LOG("! (request1.send(*newChannel, response1))"); //####
                    }
#if defined(MpM_DoExplicitDisconnect)
                    if (! NetworkDisconnectWithRetries(aName, serviceChannelName, timeToWait,
                                                       checker, checkStuff))
                    {
                        OD_LOG("(! NetworkDisconnectWithRetries(aName, destinationName, " //####
                               "timeToWait, checker, checkStuff))"); //####
                    }
#endif // defined(MpM_DoExplicitDisconnect)
                }
                else
                {
                    OD_LOG("! (NetworkConnectWithRetries(aName, serviceChannelName, "
                           "timetoWait, false, checker, checkStuff))"); //####
                }
#if defined(MpM_DoExplicitClose)
                newChannel->close();
#endif // defined(MpM_DoExplicitClose)
            }
            else
            {
                OD_LOG("! (newChannel->openWithRetries(aName, timeToWait))"); //####
            }
            delete newChannel;
        }
        else
        {
            OD_LOG("! (newChannel)"); //####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_EXIT_B(result); //####
    return result;
} // Utilities::GetExtraInformationForService

YarpString Utilities::GetFileNameBase(const YarpString & inFileName)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S1s("inFileName = ", inFileName);
    YarpString result;
    size_t     index = inFileName.rfind('.');
    
    if (YarpString::npos == index)
    {
        result = inFileName;
    }
    else
    {
        result = inFileName.substr(0, index);
    }
    OD_LOG_EXIT_s(result); //####
    return result;
} // Utilities::GetFileNameBase

YarpString Utilities::GetFileNamePart(const YarpString & inFileName)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S1s("inFileName = ", inFileName); //####
    YarpString result;
#if MAC_OR_LINUX_
    char *     nameCopy = strdup(inFileName.c_str());
#else // ! MAC_OR_LINUX_
    char       baseFileName[_MAX_FNAME + 10];
    char       baseExtension[_MAX_EXT + 10];
#endif // ! MAC_OR_LINUX_
    
#if MAC_OR_LINUX_
    result = basename(nameCopy);
    free(nameCopy);
#else // ! MAC_OR_LINUX_
    _splitpath_s(inFileName.c_str(), NULL, 0, NULL, 0, baseFileName, sizeof(baseFileName),
                 baseExtension, sizeof(baseExtension));
    result = baseFileName;
    result += ".";
    result += baseExtension;
#endif // ! MAC_OR_LINUX_
    OD_LOG_EXIT_s(result); //####
    return result;
} // Utilities::GetFileNamePart

ChannelStatusReporter * Utilities::GetGlobalStatusReporter(void)
{
	OD_LOG_ENTER(); //####
	OD_LOG_EXIT_P(lReporter); //####
    return lReporter;
} // Utilities::GetGlobalStatusReporter

void Utilities::GetMachineIPs(YarpStringVector & result)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P1("result = ", &result); //####
    YarpString ipString(yarp::os::impl::NameConfig::getIps());
    
    OD_LOG_S1s("ipString <- ", ipString); //####
    result.clear();
    for ( ; 0 < ipString.length(); )
    {
        size_t indx = ipString.find(" ");
        
        if (YarpString::npos == indx)
        {
            result.push_back(ipString);
            ipString = "";
        }
        else
        {
            result.push_back(ipString.substr(0, indx));
            ipString = ipString.substr(indx + 1);
        }
    }
    OD_LOG_EXIT(); //####
} // Utilities::GetMachineIPs

bool Utilities::GetMetricsForService(const YarpString & serviceChannelName,
                                     yarp::os::Bottle & metrics,
                                     const double       timeToWait,
                                     CheckFunction      checker,
                                     void *             checkStuff)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S1s("serviceChannelName = ", serviceChannelName); //####
    OD_LOG_P2("metrics = ", &metrics, "checkStuff = ", checkStuff); //####
    OD_LOG_D1("timeToWait = ", timeToWait); //####
    bool            result = false;
    YarpString      aName(GetRandomChannelName(HIDDEN_CHANNEL_PREFIX_ "servicemetrics_/"
                                               DEFAULT_CHANNEL_ROOT_));
    ClientChannel * newChannel = new ClientChannel;
    
    if (newChannel)
    {
        if (newChannel->openWithRetries(aName, timeToWait))
        {
            if (NetworkConnectWithRetries(aName, serviceChannelName, timeToWait, false, checker,
                                          checkStuff))
            {
                yarp::os::Bottle parameters;
                ServiceRequest   request(MpM_METRICS_REQUEST_, parameters);
                ServiceResponse  response;
                
                if (request.send(*newChannel, response))
                {
                    OD_LOG_S1s("response <- ", response.asString()); //####
                    metrics = response.values();
                    result = (0 < response.count());
                }
                else
                {
                    OD_LOG("! (request.send(*newChannel, response))"); //####
                }
#if defined(MpM_DoExplicitDisconnect)
                if (! NetworkDisconnectWithRetries(aName, serviceChannelName, timeToWait, checker,
                                                   checkStuff))
                {
                    OD_LOG("(! NetworkDisconnectWithRetries(aName, destinationName, " //####
                           "timeToWait, checker, checkStuff))"); //####
                }
#endif // defined(MpM_DoExplicitDisconnect)
            }
            else
            {
                OD_LOG("! (NetworkConnectWithRetries(aName, serviceChannelName, timetoWait, " //####
                       "false, checker, checkStuff))"); //####
            }
#if defined(MpM_DoExplicitClose)
            newChannel->close();
#endif // defined(MpM_DoExplicitClose)
        }
        else
        {
            OD_LOG("! (newChannel->openWithRetries(aName, timeToWait))"); //####
        }
        delete newChannel;
    }
    else
    {
        OD_LOG("! (newChannel)"); //####
    }
    OD_LOG_EXIT_B(result); //####
    return result;
} // Utilities::GetMetricsForService

bool Utilities::GetMetricsStateForService(const YarpString & serviceChannelName,
                                          bool &             metricsState,
                                          const double       timeToWait,
                                          CheckFunction      checker,
                                          void *             checkStuff)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S1s("serviceChannelName = ", serviceChannelName); //####
    OD_LOG_P2("metrics = ", &metricsState, "checkStuff = ", checkStuff); //####
    OD_LOG_D1("timeToWait = ", timeToWait); //####
    bool            result = false;
    YarpString      aName(GetRandomChannelName(HIDDEN_CHANNEL_PREFIX_ "servicemetrics_/"
                                               DEFAULT_CHANNEL_ROOT_));
    ClientChannel * newChannel = new ClientChannel;
    
    if (newChannel)
    {
        if (newChannel->openWithRetries(aName, timeToWait))
        {
            if (NetworkConnectWithRetries(aName, serviceChannelName, timeToWait, false, checker,
                                          checkStuff))
            {
                yarp::os::Bottle parameters;
                ServiceRequest   request(MpM_METRICSSTATE_REQUEST_, parameters);
                ServiceResponse  response;
                
                if (request.send(*newChannel, response))
                {
                    OD_LOG_S1s("response <- ", response.asString()); //####
                    if (MpM_EXPECTED_METRICSSTATE_RESPONSE_SIZE_ == response.count())
                    {
                        yarp::os::Value responseValue = response.element(0);
                        
                        if (responseValue.isInt())
                        {
                            metricsState = (0 != responseValue.asInt());
                        }
                        else
                        {
                            metricsState = false;
                        }
                        result = true;
                    }
                    else
                    {
                        result = false;
                    }
                }
                else
                {
                    OD_LOG("! (request.send(*newChannel, response))"); //####
                }
#if defined(MpM_DoExplicitDisconnect)
                if (! NetworkDisconnectWithRetries(aName, serviceChannelName, timeToWait, checker,
                                                   checkStuff))
                {
                    OD_LOG("(! NetworkDisconnectWithRetries(aName, destinationName, " //####
                           "timeToWait, checker, checkStuff))"); //####
                }
#endif // defined(MpM_DoExplicitDisconnect)
            }
            else
            {
                OD_LOG("! (NetworkConnectWithRetries(aName, serviceChannelName, timetoWait, " //####
                       "false, checker, checkStuff))"); //####
            }
#if defined(MpM_DoExplicitClose)
            newChannel->close();
#endif // defined(MpM_DoExplicitClose)
        }
        else
        {
            OD_LOG("! (newChannel->openWithRetries(aName, timeToWait))"); //####
        }
        delete newChannel;
    }
    else
    {
        OD_LOG("! (newChannel)"); //####
    }
    OD_LOG_EXIT_B(result); //####
    return result;
} // Utilities::GetMetricsStateForService

bool Utilities::GetNameAndDescriptionForService(const YarpString &  serviceChannelName,
                                                ServiceDescriptor & descriptor,
                                                const double        timeToWait,
                                                CheckFunction       checker,
                                                void *              checkStuff)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S1s("serviceChannelName = ", serviceChannelName); //####
    OD_LOG_P2("descriptor = ", &descriptor, "checkStuff = ", checkStuff); //####
    OD_LOG_D1("timeToWait = ", timeToWait); //####
    bool result = false;
    
    try
    {
        YarpString      aName(GetRandomChannelName(HIDDEN_CHANNEL_PREFIX_ "servicelister_/"
                                                   DEFAULT_CHANNEL_ROOT_));
        ClientChannel * newChannel = new ClientChannel;
        
        if (newChannel)
        {
            if (newChannel->openWithRetries(aName, timeToWait))
            {
                if (NetworkConnectWithRetries(aName, serviceChannelName, timeToWait, false, checker,
                                              checkStuff))
                {
                    yarp::os::Bottle parameters1;
                    ServiceRequest   request1(MpM_NAME_REQUEST_, parameters1);
                    ServiceResponse  response1;
                    
                    if (request1.send(*newChannel, response1))
                    {
                        OD_LOG_S1s("response1 <- ", response1.asString()); //####
                        if (MpM_EXPECTED_NAME_RESPONSE_SIZE_ == response1.count())
                        {
                            yarp::os::Value theCanonicalName(response1.element(0));
                            yarp::os::Value theDescription(response1.element(1));
                            yarp::os::Value theExtraInfo(response1.element(2));
                            yarp::os::Value theKind(response1.element(3));
                            yarp::os::Value thePath(response1.element(4));
                            yarp::os::Value theRequestsDescription(response1.element(5));
                            yarp::os::Value theTag(response1.element(6));
                            
                            OD_LOG_S4s("theCanonicalName <- ", theCanonicalName.toString(), //####
                                       "theDescription <- ", theDescription.toString(), //####
                                       "theExtraInfo <- ", theExtraInfo.toString(), //####
                                       "theKind <- ", theKind.toString()); //####
                            OD_LOG_S3s("thePath <- ", thePath.toString(), //####
                                       "theRequestsDescription = ", //####
                                       theRequestsDescription.toString(), "theTag = ", //####
                                       theTag.toString()); //####
                            if (theCanonicalName.isString() && theDescription.isString() &&
                                theExtraInfo.isString() && theKind.isString() &&
                                thePath.isString() && theRequestsDescription.isString() &&
                                theTag.isString())
                            {
                                descriptor._channelName = serviceChannelName;
                                descriptor._serviceName = theCanonicalName.toString();
                                descriptor._description = theDescription.toString();
                                descriptor._extraInfo = theExtraInfo.toString();
                                descriptor._kind = theKind.toString();
                                descriptor._path = thePath.toString();
                                descriptor._requestsDescription = theRequestsDescription.toString();
                                descriptor._tag = theTag.toString();
                                result = true;
                            }
                            else
                            {
                                OD_LOG("! (theCanonicalName.isString() && " //####
                                       "theDescription.isString() && " //####
                                       "theExtraInfo.isString() && theKind.isString() && " //####
                                       "thePath.isString() && " //####
                                       "theRequestsDescription.isString() && " //####
                                       "theTag.isString())"); //####
                            }
                        }
                        else
                        {
                            OD_LOG("! (MpM_EXPECTED_NAME_RESPONSE_SIZE_ == " //####
                                   "response1.count())"); //####
                            OD_LOG_S1s("response1 = ", response1.asString()); //####
                        }
                    }
                    else
                    {
                        OD_LOG("! (request1.send(*newChannel, response1))"); //####
                    }
                    if (result)
                    {
                        yarp::os::Bottle parameters2;
                        ServiceRequest   request2(MpM_CHANNELS_REQUEST_, parameters2);
                        ServiceResponse  response2;
                        
                        if (request2.send(*newChannel, response2))
                        {
                            OD_LOG_S1s("response2 <- ", response2.asString()); //####
                            if (MpM_EXPECTED_CHANNELS_RESPONSE_SIZE_ == response2.count())
                            {
                                yarp::os::Value theInputChannels(response2.element(0));
                                yarp::os::Value theOutputChannels(response2.element(1));
                                yarp::os::Value theClientChannels(response2.element(2));
                                
                                OD_LOG_S3s("theInputChannels <- ", //####
                                           theInputChannels.toString(), //####
                                           "theOutputChannels <- ", //####
                                           theOutputChannels.toString(), //####
                                           "theClientChannels <- ", //####
                                           theClientChannels.toString()); //####
                                if (theInputChannels.isList() && theOutputChannels.isList() &&
                                    theClientChannels.isList())
                                {
                                    yarp::os::Bottle * inputChannelsAsList =
                                                                        theInputChannels.asList();
                                    yarp::os::Bottle * outputChannelsAsList =
                                                                        theOutputChannels.asList();
                                    yarp::os::Bottle * clientChannelsAsList =
                                                                        theClientChannels.asList();
                                    
                                    for (int ii = 0, howMany = inputChannelsAsList->size();
                                         result && (howMany > ii); ++ii)
                                    {
                                        yarp::os::Value element(inputChannelsAsList->get(ii));
                                        
                                        if (element.isList())
                                        {
                                            yarp::os::Bottle * inputChannelAsList =
                                                                                element.asList();
                                            
                                            if (MpM_EXPECTED_CHANNEL_DESCRIPTOR_SIZE_ ==
                                                                        inputChannelAsList->size())
                                            {
                                                yarp::os::Value firstValue =
                                                                        inputChannelAsList->get(0);
                                                yarp::os::Value secondValue =
                                                                        inputChannelAsList->get(1);
                                                yarp::os::Value thirdValue =
                                                                        inputChannelAsList->get(2);
                                                
                                                if (firstValue.isString() &&
                                                    secondValue.isString() && thirdValue.isString())
                                                {
                                                    ChannelDescription aChannel;
                                                    
                                                    aChannel._portName = firstValue.asString();
                                                    aChannel._portProtocol = secondValue.asString();
                                                    aChannel._portMode = kChannelModeOther;
                                                    aChannel._protocolDescription =
                                                                            thirdValue.asString();
                                                    descriptor._inputChannels.push_back(aChannel);
                                                }
                                                else
                                                {
                                                    OD_LOG("! (firstValue.isString() && " //####
                                                           "secondValue.isString() && " //####
                                                           "thirdValue.isString())"); //####
                                                    result = false;
                                                }
                                            }
                                            else
                                            {
                                                OD_LOG("! (MpM_EXPECTED_CHANNEL_DESCRIPTOR_" //####
                                                       "SIZE_ == inputChannelAsList->" //####
                                                       "size())"); //####
                                                result = false;
                                            }
                                        }
                                        else
                                        {
                                            OD_LOG("! (element.isList())"); //####
                                            result = false;
                                        }
                                    }
                                    for (int ii = 0, howMany = outputChannelsAsList->size();
                                         result && (howMany > ii); ++ii)
                                    {
                                        yarp::os::Value element(outputChannelsAsList->get(ii));
                                        
                                        if (element.isList())
                                        {
                                            yarp::os::Bottle * outputChannelAsList =
                                                                                element.asList();
                                            
                                            if (MpM_EXPECTED_CHANNEL_DESCRIPTOR_SIZE_ ==
                                                                        outputChannelAsList->size())
                                            {
                                                yarp::os::Value firstValue =
                                                                        outputChannelAsList->get(0);
                                                yarp::os::Value secondValue =
                                                                        outputChannelAsList->get(1);
                                                yarp::os::Value thirdValue =
                                                                        outputChannelAsList->get(2);
                                                
                                                if (firstValue.isString() &&
                                                    secondValue.isString() && thirdValue.isString())
                                                {
                                                    ChannelDescription aChannel;
                                                    
                                                    aChannel._portName = firstValue.asString();
                                                    aChannel._portProtocol = secondValue.asString();
                                                    aChannel._portMode = kChannelModeOther;
                                                    aChannel._protocolDescription =
                                                                            thirdValue.asString();
                                                    descriptor._outputChannels.push_back(aChannel);
                                                }
                                                else
                                                {
                                                    OD_LOG("! (firstValue.isString() && " //####
                                                           "secondValue.isString() && " //####
                                                           "thirdValue.isString())"); //####
                                                    result = false;
                                                }
                                            }
                                            else
                                            {
                                                OD_LOG("! (MpM_EXPECTED_CHANNEL_DESCRIPTOR_" //####
                                                       "SIZE_ == outputChannelAsList->" //####
                                                       "size())"); //####
                                                result = false;
                                            }
                                        }
                                        else
                                        {
                                            OD_LOG("! (element.isList())"); //####
                                            result = false;
                                        }
                                    }
                                    for (int ii = 0, howMany = clientChannelsAsList->size();
                                         result && (howMany > ii); ++ii)
                                    {
                                        yarp::os::Value element(clientChannelsAsList->get(ii));
                                        
                                        if (element.isList())
                                        {
                                            yarp::os::Bottle * clientChannelAsList =
                                                                                element.asList();
                                            
                                            if (MpM_EXPECTED_CHANNEL_DESCRIPTOR_SIZE_ ==
                                                                        clientChannelAsList->size())
                                            {
                                                yarp::os::Value firstValue =
                                                                        clientChannelAsList->get(0);
                                                yarp::os::Value secondValue =
                                                                        clientChannelAsList->get(1);
                                                yarp::os::Value thirdValue =
                                                                        clientChannelAsList->get(2);
                                                
                                                if (firstValue.isString() &&
                                                    secondValue.isString() && thirdValue.isString())
                                                {
                                                    ChannelDescription aChannel;
                                                    
                                                    aChannel._portName = firstValue.asString();
                                                    aChannel._portProtocol = secondValue.asString();
                                                    aChannel._portMode = kChannelModeOther;
                                                    aChannel._protocolDescription =
                                                                            thirdValue.asString();
                                                    descriptor._clientChannels.push_back(aChannel);
                                                }
                                                else
                                                {
                                                    OD_LOG("! (firstValue.isString() && " //####
                                                           "secondValue.isString() && " //####
                                                           "thirdValue.isString())"); //####
                                                    result = false;
                                                }
                                            }
                                            else
                                            {
                                                OD_LOG("! (MpM_EXPECTED_CHANNEL_DESCRIPTOR_" //####
                                                       "SIZE_ == clientChannelAsList->" //####
                                                       "size())"); //####
                                                result = false;
                                            }
                                        }
                                        else
                                        {
                                            OD_LOG("! (element.isList())"); //####
                                            result = false;
                                        }
                                    }
                                }
                                else
                                {
                                    OD_LOG("! (theInputChannels.isList() && " //####
                                           "theOutputChannels.isList() && " //####
                                           "theClientChannels.isList())");
                                    result = false;
                                }
                            }
                            else
                            {
                                OD_LOG("! (MpM_EXPECTED_CHANNELS_RESPONSE_SIZE_ == " //####
                                       "response2.count())"); //####
                                OD_LOG_S1s("response2 = ", response2.asString()); //####
                                result = false;
                            }
                        }
                        else
                        {
                            OD_LOG("! (request2.send(*newChannel, response2))"); //####
                            result = false;
                        }
                    }
                    if (result)
                    {
                        yarp::os::Bottle parameters3;
                        ServiceRequest   request3(MpM_ARGUMENTDESCRIPTIONS_REQUEST_, parameters3);
                        ServiceResponse  response3;

                        if (request3.send(*newChannel, response3))
                        {
                            OD_LOG_S1s("response3 <- ", response3.asString()); //####
                            // Note that only input / output services will respond to this request.
                            if (response3.asString() != MpM_UNRECOGNIZED_REQUEST_)
                            {
                                for (int ii = 0, howMany = response3.count();
                                     result && (howMany > ii); ++ii)
                                {
                                    yarp::os::Value anArgAsValue(response3.element(ii));

                                    if (anArgAsValue.isString())
                                    {
                                        YarpString               argAsString =
                                                                            anArgAsValue.asString();
                                        OD_LOG_S1s("argAsString <- ", argAsString); //####
                                        BaseArgumentDescriptor * argDesc =
                                                            ConvertStringToArgument(argAsString);

                                        if (argDesc)
                                        {
                                            descriptor._argumentList.push_back(argDesc);
                                        }
                                        else
                                        {
                                            OD_LOG("! (argDesc)"); //####
                                            result = false;
                                        }
                                    }
                                    else
                                    {
                                        OD_LOG("! (anArgAsValue.isString())"); //####
                                        result = false;
                                    }
                                }
                            }
                        }
                        else
                        {
                            OD_LOG("! (request3.send(*newChannel, response3))"); //####
                            result = false;
                        }
                    }
#if defined(MpM_DoExplicitDisconnect)
                    if (! NetworkDisconnectWithRetries(aName, serviceChannelName, timeToWait,
                                                       checker, checkStuff))
                    {
                        OD_LOG("(! NetworkDisconnectWithRetries(aName, destinationName, " //####
                               "timeToWait, checker, checkStuff))"); //####
                    }
#endif // defined(MpM_DoExplicitDisconnect)
                }
                else
                {
                    OD_LOG("! (NetworkConnectWithRetries(aName, serviceChannelName, "
                           "timetoWait, false, checker, checkStuff))"); //####
                }
#if defined(MpM_DoExplicitClose)
                newChannel->close();
#endif // defined(MpM_DoExplicitClose)
            }
            else
            {
                OD_LOG("! (newChannel->openWithRetries(aName, timeToWait))"); //####
            }
            delete newChannel;
        }
        else
        {
            OD_LOG("! (newChannel)"); //####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_EXIT_B(result); //####
    return result;
} // Utilities::GetNameAndDescriptionForService

Utilities::PortKind Utilities::GetPortKind(const YarpString & portName)
{
    const char * portNameChars = portName.c_str();
    const size_t kAdapterPortNameBaseLen = sizeof(DEFAULT_ADAPTER_NAME_BASE_) - 1;
    const size_t kClientPortNameBaseLen = sizeof(CLIENT_PORT_NAME_BASE_) - 1;
    const size_t kDefaultServiceNameBaseLen = sizeof(DEFAULT_SERVICE_NAME_BASE_) - 1;
    PortKind     result;
    
    if (! strcmp(MpM_REGISTRY_ENDPOINT_NAME_, portNameChars))
    {
        result = kPortKindRegistryService;
    }
    else if (! strncmp(DEFAULT_SERVICE_NAME_BASE_, portNameChars, kDefaultServiceNameBaseLen))
    {
        result = kPortKindService;
    }
    else if (! strncmp(DEFAULT_ADAPTER_NAME_BASE_, portNameChars, kAdapterPortNameBaseLen))
    {
        result = kPortKindAdapter;
    }
    else if (! strncmp(CLIENT_PORT_NAME_BASE_, portNameChars, kClientPortNameBaseLen))
    {
        result = kPortKindClient;
    }
    else
    {
        result = kPortKindStandard;
    }
    return result;
} // Utilities::GetPortKind

YarpString Utilities::GetPortLocation(const YarpString & portName)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S1s("portName = ", portName); //####
    YarpString        result;
    yarp::os::Contact whereItIs = yarp::os::NetworkBase::queryName(portName);

    if (whereItIs.isValid())
    {
        std::stringstream buff;

        buff << ":" << whereItIs.getPort();
        result = whereItIs.getHost() + buff.str();
    }
    else
    {
        result = "<unknown>";
    }
    OD_LOG_EXIT_s(result); //####
    return result;
} // Utilities::GetPortLocation

YarpString Utilities::GetRandomHexString(void)
{
    OD_LOG_ENTER(); //####
    int               randNumb;
    YarpString        result;
    std::stringstream buff;
    
    if (! lRandomSeeded)
    {
#if defined(__APPLE__)
        sranddev();
#else // ! defined(__APPLE__)
        srand(clock());
#endif // ! defined(__APPLE__)
        lRandomSeeded = true;
    }
    randNumb = (rand() % 10000);
    buff << std::hex << randNumb;
    result = buff.str();
    OD_LOG_EXIT_s(result); //####
    return result;
} // Utilities::GetRandomHexString

bool Utilities::GetServiceNames(YarpStringVector & services,
                                const bool         quiet,
                                CheckFunction      checker,
                                void *             checkStuff)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("services = ", &services, "checkStuff = ", checkStuff); //####
    OD_LOG_B1("quiet = ", quiet); //####
    bool okSoFar = GetServiceNamesFromCriteria(MpM_REQREP_DICT_REQUEST_KEY_ ":*", services, quiet,
                                               checker, checkStuff);
    
    OD_LOG_EXIT_B(okSoFar); //####
    return okSoFar;
} // Utilities::GetServiceNames

bool Utilities::GetServiceNamesFromCriteria(const YarpString & criteria,
                                            YarpStringVector & services,
                                            const bool         quiet,
                                            CheckFunction      checker,
                                            void *             checkStuff)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S1s("criteria = ", criteria); //####
    OD_LOG_P2("services = ", &services, "checkStuff = ", checkStuff); //####
    OD_LOG_B1("quiet = ", quiet); //####
    bool             okSoFar = false;
    yarp::os::Bottle matches(FindMatchingServices(criteria, false, checker, checkStuff));
    
    if (MpM_EXPECTED_MATCH_RESPONSE_SIZE_ == matches.size())
    {
        // First, check if the search succeeded.
        YarpString matchesFirstString(matches.get(0).toString());
        
        if (strcmp(MpM_OK_RESPONSE_, matchesFirstString.c_str()))
        {
            OD_LOG("(strcmp(MpM_OK_RESPONSE_, matchesFirstString.c_str()))"); //####
            if (! quiet)
            {
#if MAC_OR_LINUX_
                YarpString reason(matches.get(1).toString());
                
                GetLogger().fail(YarpString("Failed: ") + reason + ".");
#endif // MAC_OR_LINUX_
            }
        }
        else
        {
            // Now, process the second element.
            yarp::os::Bottle * matchesList = matches.get(1).asList();
            
            if (matchesList)
            {
                for (int ii = 0, matchesCount = matchesList->size(); ii < matchesCount; ++ii)
                {
                    services.push_back(matchesList->get(ii).toString());
                }
                okSoFar = true;
            }
        }
    }
    else
    {
        OD_LOG("! (MpM_EXPECTED_MATCH_RESPONSE_SIZE_ == matches.size())"); //####
        if (! quiet)
        {
#if MAC_OR_LINUX_
            GetLogger().fail("Problem getting information from the Registry Service.");
#endif // MAC_OR_LINUX_
        }
    }
    if ((! okSoFar) && (! quiet))
    {
        char buffer1[DATE_TIME_BUFFER_SIZE_];
        char buffer2[DATE_TIME_BUFFER_SIZE_];
        
        GetDateAndTime(buffer1, sizeof(buffer1), buffer2, sizeof(buffer2));
        cerr << buffer1 << " " << buffer2 << " Problem getting list of service names." << endl;
    }
    OD_LOG_EXIT_B(okSoFar); //####
    return okSoFar;
} // Utilities::GetServiceNamesFromCriteria

void Utilities::GoToSleep(const int milliseconds)
{
    OD_LOG_ENTER(); //####
#if MAC_OR_LINUX_
    usleep(milliseconds * 1000);
#else // ! MAC_OR_LINUX_
    Sleep(milliseconds);
#endif // ! MAC_OR_LINUX_
    OD_LOG_EXIT(); //####
} // Utilities::GoToSleep

const char * Utilities::MapServiceKindToString(const ServiceKind kind)
{
    OD_LOG_ENTER(); //####
    OD_LOG_L1("kind = ", static_cast<int>(kind)); //####
    const char * result;
    
    switch (kind)
    {
        case kServiceKindAdapter :
            result = "Adapter";
            break;
            
        case kServiceKindFilter :
            result = "Filter";
            break;
            
	    case kServiceKindInput :
            result = "Input";
            break;
            
	    case kServiceKindOutput :
            result = "Output";
            break;
            
	    case kServiceKindRegistry :
            result = "Registry";
            break;
            
        case kServiceKindNormal :
            result = "Normal";
            break;
            
        default :
            result = "unknown";
            break;
            
    }
    OD_LOG_EXIT_S(result); //####
    return result;
} // Utilities::MapServiceKindToString

ServiceKind Utilities::MapStringToServiceKind(const YarpString & kindString)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S1s("kindString = ", kindString); //####
    ServiceKind  result;
    const char * kindStringChars = kindString.c_str();
    
    if (! strcmp("Adapter", kindStringChars))
    {
        result = kServiceKindAdapter;
    }
    else if (! strcmp("Filter", kindStringChars))
    {
        result = kServiceKindFilter;
    }
    else if (! strcmp("Input", kindStringChars))
    {
        result = kServiceKindInput;
    }
    else if (! strcmp("Output", kindStringChars))
    {
        result = kServiceKindOutput;
    }
    else if (! strcmp("Registry", kindStringChars))
    {
        result = kServiceKindRegistry;
    }
    else
    {
        result = kServiceKindNormal;
    }
    OD_LOG_EXIT_L(static_cast<int>(result)); //####
    return result;
} // Utilities::MapStringToServiceKind

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
bool Utilities::NetworkConnectWithRetries(const YarpString & sourceName,
                                          const YarpString & destinationName,
                                          const double       timeToWait,
                                          const bool         isUDP,
                                          CheckFunction      checker,
                                          void *             checkStuff)
{
#if ((! RETRY_LOOPS_USE_TIMEOUTS) && (! defined(OD_ENABLE_LOGGING_)))
# if MAC_OR_LINUX_
#  pragma unused(timeToWait)
# endif // MAC_OR_LINUX_
#endif // (! RETRY_LOOPS_USE_TIMEOUTS) && (! defined(OD_ENABLE_LOGGING_))
    OD_LOG_ENTER(); //####
    OD_LOG_S2s("sourceName = ", sourceName, "destinationName = ", destinationName); //####
    OD_LOG_D1("timeToWait = ", timeToWait); //####
    OD_LOG_B1("isUDP = ", isUDP); //####
    OD_LOG_P1("checkStuff = ", checkStuff); //####
    bool result = false;
    
    if (yarp::os::Network::exists(sourceName) && yarp::os::Network::exists(destinationName))
    {
        double retryTime = INITIAL_RETRY_INTERVAL_;
        int    retriesLeft = MAX_RETRIES_;
        
#if RETRY_LOOPS_USE_TIMEOUTS
        SetUpCatcher();
#endif // RETRY_LOOPS_USE_TIMEOUTS
        try
        {
#if RETRY_LOOPS_USE_TIMEOUTS
            BailOut      bailer(timeToWait);
#endif // RETRY_LOOPS_USE_TIMEOUTS
            const char * carrier;
            
            if (isUDP)
            {
                carrier = "udp";
            }
            else
            {
                carrier = "tcp";
            }
            do
            {
                if (checker && checker(checkStuff))
                {
                    break;
                }
                
                OD_LOG("about to connect"); //####
#if (defined(OD_ENABLE_LOGGING_) && defined(MpM_LogIncludesYarpTrace))
                result = yarp::os::Network::connect(sourceName, destinationName, carrier, false);
#else // ! (defined(OD_ENABLE_LOGGING_) && defined(MpM_LogIncludesYarpTrace))
                result = yarp::os::Network::connect(sourceName, destinationName, carrier, true);
#endif // ! (defined(OD_ENABLE_LOGGING_) && defined(MpM_LogIncludesYarpTrace))
                OD_LOG("connected?"); //####
                if (! result)
                {
                    if (0 < --retriesLeft)
                    {
                        OD_LOG("%%retry%%"); //####
                        yarp::os::Time::delay(retryTime);
                        retryTime *= RETRY_MULTIPLIER_;
                    }
                }
            }
            while ((! result) && (0 < retriesLeft));
        }
        catch (...)
        {
            OD_LOG("Exception caught"); //####
            throw;
        }
#if RETRY_LOOPS_USE_TIMEOUTS
        ShutDownCatcher();
#endif // RETRY_LOOPS_USE_TIMEOUTS
    }
    else
    {
        OD_LOG("! (yarp::os::Network::exists(sourceName) && " //####
               "yarp::os::Network::exists(destinationName))"); //####
    }
    OD_LOG_EXIT_B(result); //####
    return result;
} // Utilities::NetworkConnectWithRetries
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
bool Utilities::NetworkDisconnectWithRetries(const YarpString & sourceName,
                                             const YarpString & destinationName,
                                             const double       timeToWait,
                                             CheckFunction      checker,
                                             void *             checkStuff)
{
#if ((! RETRY_LOOPS_USE_TIMEOUTS) && (! defined(OD_ENABLE_LOGGING_)))
# if MAC_OR_LINUX_
#  pragma unused(timeToWait)
# endif // MAC_OR_LINUX_
#endif // (! RETRY_LOOPS_USE_TIMEOUTS) && (! defined(OD_ENABLE_LOGGING_))
    OD_LOG_ENTER(); //####
    OD_LOG_S2s("sourceName = ", sourceName, "destinationName = ", destinationName); //####
    OD_LOG_D1("timeToWait = ", timeToWait); //####
    OD_LOG_P1("checkStuff = ", checkStuff); //####
    bool result = false;
    
    if (yarp::os::Network::exists(sourceName) && yarp::os::Network::exists(destinationName))
    {
        double retryTime = INITIAL_RETRY_INTERVAL_;
        int    retriesLeft = MAX_RETRIES_;
        
#if RETRY_LOOPS_USE_TIMEOUTS
        SetUpCatcher();
#endif // RETRY_LOOPS_USE_TIMEOUTS
        try
        {
#if RETRY_LOOPS_USE_TIMEOUTS
            BailOut bailer(timeToWait);
#endif // RETRY_LOOPS_USE_TIMEOUTS
            
            do
            {
                if (checker && checker(checkStuff))
                {
                    break;
                }
                
                OD_LOG("about to disconnect"); //####
#if (defined(OD_ENABLE_LOGGING_) && defined(MpM_LogIncludesYarpTrace))
                result = yarp::os::Network::disconnect(sourceName, destinationName, false);
#else // ! (defined(OD_ENABLE_LOGGING_) && defined(MpM_LogIncludesYarpTrace))
                result = yarp::os::Network::disconnect(sourceName, destinationName, true);
#endif // ! (defined(OD_ENABLE_LOGGING_) && defined(MpM_LogIncludesYarpTrace))
                OD_LOG("disconnected?"); //####
                if (! result)
                {
                    if (0 < --retriesLeft)
                    {
                        OD_LOG("%%retry%%"); //####
                        yarp::os::Time::delay(retryTime);
                        retryTime *= RETRY_MULTIPLIER_;
                    }
                }
            }
            while ((! result) && (0 < retriesLeft));
        }
        catch (...)
        {
            OD_LOG("Exception caught"); //####
            throw;
        }
#if RETRY_LOOPS_USE_TIMEOUTS
        ShutDownCatcher();
#endif // RETRY_LOOPS_USE_TIMEOUTS
    }
    else
    {
        OD_LOG("! (yarp::os::Network::exists(sourceName) && " //####
               "yarp::os::exists(destinationName))"); //####
    }
    OD_LOG_EXIT_B(result); //####
    return result;
} // Utilities::NetworkDisconnectWithRetries
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

bool Utilities::ProcessStandardClientOptions(const int          argc,
                                             char * *           argv,
                                             DescriptorVector & argumentDescriptions,
                                             const YarpString & clientDescription,
                                             const int          year,
                                             const char *       copyrightHolder,
                                             OutputFlavour &    flavour,
                                             const bool         ignoreFlavours,
                                             YarpStringVector * arguments)
{
    OD_LOG_ENTER(); //####
    OD_LOG_L2("argc = ", argc, "year = ", year); //####
    OD_LOG_P4("argv = ", argv, "argumentDescriptions = ", &argumentDescriptions, //####
              "flavour = ", &flavour, "arguments = ", arguments); //####
    OD_LOG_S1s("clientDescription = ", clientDescription); //####
    OD_LOG_S1("copyrightHolder = ", copyrightHolder); //####
    OD_LOG_B1("ignoreFlavours = ", ignoreFlavours); //####
    enum optionIndex
    {
        kOptionUNKNOWN,
        kOptionHELP,
        kOptionINFO,
        kOptionJSON,
        kOptionTABS,
        kOptionVERSION
    }; // optionIndex
    
    bool                  keepGoing = true;
    Option_::Descriptor   firstDescriptor(kOptionUNKNOWN, 0, "", "", Option_::Arg::None, NULL);
    Option_::Descriptor   helpDescriptor(kOptionHELP, 0, "h", "help", Option_::Arg::None,
                                         T_("  --help, -h    Print usage and exit"));
    Option_::Descriptor   infoDescriptor(kOptionINFO, 0, "i", "info", Option_::Arg::None,
                                         T_("  --info, -i    Print type and description and exit"));
    Option_::Descriptor   jsonDescriptor(kOptionJSON, 0, "j", "json", Option_::Arg::None,
                                         T_("  --json, -j    Generate output in JSON format") );
    Option_::Descriptor   tabsDescriptor(kOptionTABS, 0, "t", "tabs", Option_::Arg::None,
                                         T_("  --tabs, -t    Generate output in tab-delimited "
                                            "format"));
    Option_::Descriptor   versionDescriptor(kOptionVERSION, 0, "v", "vers", Option_::Arg::None,
                                            T_("  --vers, -v    Print version information and "
                                               "exit"));
    Option_::Descriptor   lastDescriptor(0, 0, NULL, NULL, NULL, NULL);
    int                   argcWork = argc;
    char * *              argvWork = argv;
    YarpString            usageString("USAGE: ");
    YarpString            argList(ArgumentsToArgString(argumentDescriptions));
    
    flavour = kOutputFlavourNormal;
    usageString += *argv;
    usageString += " [options]";
    if (0 < argList.length())
    {
        YarpStringVector descriptions;
        
        ArgumentsToDescriptionArray(argumentDescriptions, descriptions, 2);
        usageString += " ";
        usageString += argList + "\n\n";
        for (size_t ii = 0, mm = descriptions.size(); mm > ii; ++ii)
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
    // firstDescriptor, helpDescriptor, infoDescriptor, versionDescriptor, lastDescriptor
    size_t descriptorCount = 5;
    
    if (! ignoreFlavours)
    {
        descriptorCount += 2;
    }
    Option_::Descriptor * usage = new Option_::Descriptor[descriptorCount];
    Option_::Descriptor * usageWalker = usage;
    
#if MAC_OR_LINUX_
    firstDescriptor.help = strdup(usageString.c_str());
#else // ! MAC_OR_LINUX_
    firstDescriptor.help = _strdup(usageString.c_str());
#endif // ! MAC_OR_LINUX_
    memcpy(usageWalker++, &firstDescriptor, sizeof(firstDescriptor));
    memcpy(usageWalker++, &helpDescriptor, sizeof(helpDescriptor));
    memcpy(usageWalker++, &infoDescriptor, sizeof(infoDescriptor));
    if (! ignoreFlavours)
    {
        memcpy(usageWalker++, &jsonDescriptor, sizeof(jsonDescriptor));
    }
    if (! ignoreFlavours)
    {
        memcpy(usageWalker++, &tabsDescriptor, sizeof(tabsDescriptor));
    }
    memcpy(usageWalker++, &versionDescriptor, sizeof(versionDescriptor));
    memcpy(usageWalker++, &lastDescriptor, sizeof(lastDescriptor));
    argcWork -= (argc > 0);
    argvWork += (argc > 0); // skip program name argv[0] if present
    Option_::Stats    stats(usage, argcWork, argvWork);
    Option_::Option * options = new Option_::Option[stats.options_max];
    Option_::Option * buffer = new Option_::Option[stats.buffer_max];
    Option_::Parser   parse(usage, argcWork, argvWork, options, buffer, 1);
    YarpString        badArgs;
    
    if (parse.error())
    {
        keepGoing = false;
    }
    else if (options[kOptionHELP] || options[kOptionUNKNOWN])
    {
        Option_::printUsage(cout, usage, HELP_LINE_LENGTH_);
        keepGoing = false;
    }
    else if (options[kOptionVERSION])
    {
        YarpString mpmVersionString(SanitizeString(MpM_VERSION_, true));
        
        cout << "Version " << mpmVersionString.c_str() << ": Copyright (c) " << year << " by " <<
        copyrightHolder << "." << endl;
        keepGoing = false;
    }
    else if (options[kOptionINFO])
    {
        cout << "Client\t" << clientDescription.c_str() << endl;
        keepGoing = false;
    }
    else if (ProcessArguments(argumentDescriptions, parse, badArgs))
    {
        if (options[kOptionJSON])
        {
            flavour = kOutputFlavourJSON;
        }
        else if (options[kOptionTABS])
        {
            flavour = kOutputFlavourTabs;
        }
        if (arguments)
        {
            arguments->clear();
            for (int ii = 0; ii < parse.nonOptionsCount(); ++ii)
            {
                arguments->push_back(parse.nonOption(ii));
            }
        }
    }
    else
    {
        cout << "One or more invalid or missing arguments (" << badArgs.c_str() << ")." << endl;
        keepGoing = false;
    }
    delete[] options;
    delete[] buffer;
    delete[] usage;
    OD_LOG_EXIT_B(keepGoing); //####
    return keepGoing;
} // Utilities::ProcessStandardClientOptions

bool Utilities::ProcessStandardUtilitiesOptions(const int          argc,
                                                char * *           argv,
                                                DescriptorVector & argumentDescriptions,
                                                const YarpString & utilityDescription,
                                                const int          year,
                                                const char *       copyrightHolder,
                                                OutputFlavour &    flavour,
                                                const bool         ignoreFlavours,
                                                YarpStringVector * arguments)
{
    OD_LOG_ENTER(); //####
    OD_LOG_L2("argc = ", argc, "year = ", year); //####
    OD_LOG_P4("argv = ", argv, "argumentDescriptions = ", &argumentDescriptions, //####
              "flavour = ", &flavour, "arguments = ", arguments); //####
    OD_LOG_S1s("utilityDescription = ", utilityDescription); //####
    OD_LOG_S1("copyrightHolder = ", copyrightHolder); //####
    OD_LOG_B1("ignoreFlavours = ", ignoreFlavours); //####
    enum optionIndex
    {
        kOptionUNKNOWN,
        kOptionHELP,
        kOptionINFO,
        kOptionJSON,
        kOptionTABS,
        kOptionVERSION
    }; // optionIndex
    
    bool                  keepGoing = true;
    Option_::Descriptor   firstDescriptor(kOptionUNKNOWN, 0, "", "", Option_::Arg::None, NULL);
    Option_::Descriptor   helpDescriptor(kOptionHELP, 0, "h", "help", Option_::Arg::None,
                                         T_("  --help, -h    Print usage and exit"));
    Option_::Descriptor   infoDescriptor(kOptionINFO, 0, "i", "info", Option_::Arg::None,
                                         T_("  --info, -i    Print type and description and exit"));
    Option_::Descriptor   jsonDescriptor(kOptionJSON, 0, "j", "json", Option_::Arg::None,
                                         T_("  --json, -j    Generate output in JSON format") );
    Option_::Descriptor   tabsDescriptor(kOptionTABS, 0, "t", "tabs", Option_::Arg::None,
                                         T_("  --tabs, -t    Generate output in tab-delimited "
                                            "format"));
    Option_::Descriptor   versionDescriptor(kOptionVERSION, 0, "v", "vers", Option_::Arg::None,
                                            T_("  --vers, -v    Print version information and "
                                               "exit"));
    Option_::Descriptor   lastDescriptor(0, 0, NULL, NULL, NULL, NULL);
    Option_::Descriptor   usage[7];
    Option_::Descriptor * usageWalker = usage;
    int                   argcWork = argc;
    char * *              argvWork = argv;
    YarpString            usageString("USAGE: ");
    YarpString            argList(ArgumentsToArgString(argumentDescriptions));
    
    flavour = kOutputFlavourNormal;
    usageString += *argv;
    usageString += " [options]";
    if (0 < argList.length())
    {
        YarpStringVector descriptions;
        
        ArgumentsToDescriptionArray(argumentDescriptions, descriptions, 2);
        usageString += " ";
        usageString += argList + "\n\n";
        for (size_t ii = 0, mm = descriptions.size(); mm > ii; ++ii)
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
    memcpy(usageWalker++, &helpDescriptor, sizeof(helpDescriptor));
    memcpy(usageWalker++, &infoDescriptor, sizeof(infoDescriptor));
    if (! ignoreFlavours)
    {
        memcpy(usageWalker++, &jsonDescriptor, sizeof(jsonDescriptor));        
    }
    if (! ignoreFlavours)
    {
        memcpy(usageWalker++, &tabsDescriptor, sizeof(tabsDescriptor));
    }
    memcpy(usageWalker++, &versionDescriptor, sizeof(versionDescriptor));
    memcpy(usageWalker++, &lastDescriptor, sizeof(lastDescriptor));
    argcWork -= (argc > 0);
    argvWork += (argc > 0); // skip program name argv[0] if present
    Option_::Stats    stats(usage, argcWork, argvWork);
    Option_::Option * options = new Option_::Option[stats.options_max];
    Option_::Option * buffer = new Option_::Option[stats.buffer_max];
    Option_::Parser   parse(usage, argcWork, argvWork, options, buffer, 1);
    YarpString        badArgs;
    
    if (parse.error())
    {
        keepGoing = false;
    }
    else if (options[kOptionHELP] || options[kOptionUNKNOWN])
    {
        Option_::printUsage(cout, usage, HELP_LINE_LENGTH_);
        keepGoing = false;
    }
    else if (options[kOptionVERSION])
    {
        YarpString mpmVersionString(SanitizeString(MpM_VERSION_, true));
        
        cout << "Version " << mpmVersionString.c_str() << ": Copyright (c) " << year << " by " <<
                copyrightHolder << "." << endl;
        keepGoing = false;
    }
    else if (options[kOptionINFO])
    {
        cout << "Utility\t" << utilityDescription.c_str() << endl;
        keepGoing = false;
    }
    else if (ProcessArguments(argumentDescriptions, parse, badArgs))
    {
        if (options[kOptionJSON])
        {
            flavour = kOutputFlavourJSON;
        }
        else if (options[kOptionTABS])
        {
            flavour = kOutputFlavourTabs;
        }
        if (arguments)
        {
            arguments->clear();
            for (int ii = 0; ii < parse.nonOptionsCount(); ++ii)
            {
                arguments->push_back(parse.nonOption(ii));
            }
        }
    }
    else
    {
        cout << "One or more invalid or missing arguments (" << badArgs.c_str() << ")." << endl;
        keepGoing = false;
    }
    delete[] options;
    delete[] buffer;
    OD_LOG_EXIT_B(keepGoing); //####
    return keepGoing;
} // Utilities::ProcessStandardUtilitiesOptions

bool Utilities::RemoveConnection(const YarpString & fromPortName,
                                 const YarpString & toPortName,
                                 CheckFunction      checker,
                                 void *             checkStuff)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S2s("fromPortName = ", fromPortName, "toPortName = ", toPortName); //####
    OD_LOG_P1("checkStuff = ", checkStuff); //####
    bool result = NetworkDisconnectWithRetries(fromPortName, toPortName, STANDARD_WAIT_TIME_,
                                               checker, checkStuff);
    
    OD_LOG_EXIT_B(result); //####
    return result;
} // Utilities::RemoveConnection

void Utilities::RemoveStalePorts(const float timeout)
{
    OD_LOG_ENTER(); //####
    OD_LOG_D1("timeout = ", timeout); //####
    bool                       okSoFar;
    yarp::os::impl::NameConfig nc;
    yarp::os::impl::String     name = nc.getNamespace();
    yarp::os::Bottle           msg;
    yarp::os::Bottle           reply;
    
    msg.addString("bot");
    msg.addString("list");
    okSoFar = yarp::os::NetworkBase::write(name.c_str(), msg, reply);
    if (okSoFar)
    {
        okSoFar = (0 < reply.size());
    }
    if (! okSoFar)
    {
        // Try again, in case of a network 'glitch'.
        okSoFar = yarp::os::NetworkBase::write(name.c_str(), msg, reply);
        if (okSoFar)
        {
            okSoFar = (0 < reply.size());
        }
    }
    if (okSoFar)
    {
        for (int ii = 1; reply.size() > ii; ++ii)
        {
            yarp::os::Bottle * entry = reply.get(ii).asList();
            
            if (entry)
            {
                YarpString port = entry->check("name", yarp::os::Value("")).asString();
                
                OD_LOG_S1s("port = ", port); //####
                if ((port != "") && (port != "fallback") && (port != name.c_str()))
                {
                    yarp::os::Contact cc = yarp::os::Contact::byConfig(*entry);
                    
                    if (cc.getCarrier() == "mcast")
                    {
                        OD_LOG("Skipping mcast port."); //####
                    }
                    else
                    {
                        OD_LOG("! (cc.getCarrier() == \"mcast\")"); //####
                        yarp::os::Contact addr = cc;
                        
                        OD_LOG_S1s("Testing at ", addr.toURI()); //####
                        if (addr.isValid())
                        {
                            if (0 <= timeout)
                            {
                                addr.setTimeout(timeout);
                            }
                            yarp::os::OutputProtocol * out =
                                                            yarp::os::impl::Carriers::connect(addr);
                            
                            if (out)
                            {
                                delete out;
                            }
                            else
                            {
                                OD_LOG("No response, removing port."); //####
                                char buffer1[DATE_TIME_BUFFER_SIZE_];
                                char buffer2[DATE_TIME_BUFFER_SIZE_];
                                
                                GetDateAndTime(buffer1, sizeof(buffer1), buffer2, sizeof(buffer2));
                                yarp::os::NetworkBase::unregisterName(port);
                                cerr << buffer1 << " " << buffer2 << " Removing stale port '" <<
                                        port.c_str() << "'." << endl;
                            }
                        }
                    }
                }
                else if (port != "")
                {
                    OD_LOG("Ignoring port"); //####
                }
            }
        }
    }
    OD_LOG("Giving name server a chance to do garbage collection."); //####
    YarpString       serverName = yarp::os::NetworkBase::getNameServerName();
    yarp::os::Bottle cmd2("gc");
    yarp::os::Bottle reply2;
    
    if (yarp::os::NetworkBase::write(serverName, cmd2, reply2))
    {
        OD_LOG_S1s("Name server says: ", reply2.toString()); //####
    }
    else
    {
        OD_LOG("! (yarp::os::NetworkBase::write(serverName, cmd2, reply2))"); //####
    }
    OD_LOG_EXIT(); //####
} // Utilities::RemoveStalePorts

bool Utilities::RestartAService(const YarpString & serviceChannelName,
                                const double       timeToWait,
                                CheckFunction      checker,
                                void *             checkStuff)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S1s("serviceChannelName = ", serviceChannelName); //####
    OD_LOG_P1("checkStuff = ", checkStuff); //####
    OD_LOG_D1("timeToWait = ", timeToWait); //####
    bool result = false;

    try
    {
        YarpString      aName(GetRandomChannelName(HIDDEN_CHANNEL_PREFIX_ "restart_/"
                                                   DEFAULT_CHANNEL_ROOT_));
        ClientChannel * newChannel = new ClientChannel;

        if (newChannel)
        {
#if defined(MpM_ReportOnConnections)
            newChannel->setReporter(*GetGlobalStatusReporter());
#endif // defined(MpM_ReportOnConnections)
            if (newChannel->openWithRetries(aName, timeToWait))
            {
                if (NetworkConnectWithRetries(aName, serviceChannelName, timeToWait, false, checker,
                                              checkStuff))
                {
                    yarp::os::Bottle parameters;
                    ServiceRequest   request(MpM_RESTARTSTREAMS_REQUEST_, parameters);
#if defined(MpM_DoExplicitCheckForOK)
                    ServiceResponse  response;
#endif // defined(MpM_DoExplicitCheckForOK)

#if defined(MpM_DoExplicitCheckForOK)
                    if (! request.send(*newChannel, response))
                    {
                        OD_LOG("! (request.send(*newChannel, response))"); //####
                    }
#else // ! defined(MpM_DoExplicitCheckForOK)
                    if (! request.send(*newChannel))
                    {
                        OD_LOG("! (request.send(*newChannel))"); //####
                    }
#endif // ! defined(MpM_DoExplicitCheckForOK)
#if defined(MpM_DoExplicitDisconnect)
                    if (! NetworkDisconnectWithRetries(aName, serviceChannelName, timeToWait,
                                                       checker, checkStuff))
                    {
                        OD_LOG("(! NetworkDisconnectWithRetries(aName, serviceChannelName, " //####
                               "timeToWait, checker, checkStuff))"); //####
                    }
#endif // defined(MpM_DoExplicitDisconnect)
                }
                else
                {
                    OD_LOG("! (NetworkConnectWithRetries(aName, serviceChannelName, " //####
                           "timeToWait, false, checker, checkStuff))"); //####
                }
#if defined(MpM_DoExplicitClose)
                newChannel->close();
#endif // defined(MpM_DoExplicitClose)
            }
            else
            {
                OD_LOG("! (newChannel->openWithRetries(aName, timeToWait))"); //####
            }
            BaseChannel::RelinquishChannel(newChannel);
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_EXIT_B(result); //####
    return result;
} // Utilities::RestartAService

bool Utilities::SetConfigurationForService(const YarpString &       serviceChannelName,
                                           const yarp::os::Bottle & newValues,
                                           const double             timeToWait,
                                           Common::CheckFunction    checker,
                                           void *                   checkStuff)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S1s("serviceChannelName = ", serviceChannelName); //####
    OD_LOG_P2("newValues = ", &newValues, "checkStuff = ", checkStuff); //####
    OD_LOG_D1("timeToWait = ", timeToWait); //####
    bool            result = false;
    YarpString      aName(GetRandomChannelName(HIDDEN_CHANNEL_PREFIX_ "configure_/"
                                               DEFAULT_CHANNEL_ROOT_));
    ClientChannel * newChannel = new ClientChannel;
    
    if (newChannel)
    {
        if (newChannel->openWithRetries(aName, timeToWait))
        {
            if (NetworkConnectWithRetries(aName, serviceChannelName, timeToWait, false, checker,
                                          checkStuff))
            {
                ServiceRequest  request(MpM_CONFIGURE_REQUEST_, newValues);
                ServiceResponse response;
                
                if (request.send(*newChannel, response))
                {
                    OD_LOG_S1s("response <- ", response.asString()); //####
                    result = true;
                }
                else
                {
                    OD_LOG("! (request.send(*newChannel, response))"); //####
                }
                if (result)
                {
                }
#if defined(MpM_DoExplicitDisconnect)
                if (! NetworkDisconnectWithRetries(aName, serviceChannelName, timeToWait, checker,
                                                   checkStuff))
                {
                    OD_LOG("(! NetworkDisconnectWithRetries(aName, destinationName, " //####
                           "timeToWait, checker, checkStuff))"); //####
                }
#endif // defined(MpM_DoExplicitDisconnect)
            }
            else
            {
                OD_LOG("! (NetworkConnectWithRetries(aName, serviceChannelName, timetoWait, " //####
                       "false, checker, checkStuff))"); //####
            }
#if defined(MpM_DoExplicitClose)
            newChannel->close();
#endif // defined(MpM_DoExplicitClose)
        }
        else
        {
            OD_LOG("! (newChannel->openWithRetries(aName, timeToWait))"); //####
        }
        delete newChannel;
    }
    else
    {
        OD_LOG("! (newChannel)"); //####
    }
    OD_LOG_EXIT_B(result); //####
    return result;
} // Utilities::SetConfigurationForService

bool Utilities::SetMetricsStateForService(const YarpString & serviceChannelName,
                                          const bool         newMetricsState,
                                          const double       timeToWait,
                                          CheckFunction      checker,
                                          void *             checkStuff)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S1s("serviceChannelName = ", serviceChannelName); //####
    OD_LOG_B1("newMetricsState = ", newMetricsState); //####
    OD_LOG_P1("checkStuff = ", checkStuff); //####
    OD_LOG_D1("timeToWait = ", timeToWait); //####
    bool            result = false;
    YarpString      aName(GetRandomChannelName(HIDDEN_CHANNEL_PREFIX_ "servicemetrics_/"
                                               DEFAULT_CHANNEL_ROOT_));
    ClientChannel * newChannel = new ClientChannel;
    
    if (newChannel)
    {
        if (newChannel->openWithRetries(aName, timeToWait))
        {
            if (NetworkConnectWithRetries(aName, serviceChannelName, timeToWait, false, checker,
                                          checkStuff))
            {
                yarp::os::Bottle parameters;
                
                parameters.addInt(newMetricsState ? 1 : 0);
                ServiceRequest  request(MpM_SETMETRICSSTATE_REQUEST_, parameters);
                ServiceResponse response;
                
                if (request.send(*newChannel, response))
                {
                    OD_LOG_S1s("response <- ", response.asString()); //####
                    result = true;
                }
                else
                {
                    OD_LOG("! (request.send(*newChannel, response))"); //####
                }
                if (result)
                {
                }
#if defined(MpM_DoExplicitDisconnect)
                if (! NetworkDisconnectWithRetries(aName, serviceChannelName, timeToWait, checker,
                                                   checkStuff))
                {
                    OD_LOG("(! NetworkDisconnectWithRetries(aName, destinationName, " //####
                           "timeToWait, checker, checkStuff))"); //####
                }
#endif // defined(MpM_DoExplicitDisconnect)
            }
            else
            {
                OD_LOG("! (NetworkConnectWithRetries(aName, serviceChannelName, timetoWait, " //####
                       "false, checker, checkStuff))"); //####
            }
#if defined(MpM_DoExplicitClose)
            newChannel->close();
#endif // defined(MpM_DoExplicitClose)
        }
        else
        {
            OD_LOG("! (newChannel->openWithRetries(aName, timeToWait))"); //####
        }
        delete newChannel;
    }
    else
    {
        OD_LOG("! (newChannel)"); //####
    }
    OD_LOG_EXIT_B(result); //####
    return result;
} // Utilities::SetMetricsStateForService

void Utilities::SetUpGlobalStatusReporter(void)
{
	OD_LOG_ENTER(); //####
    if (! lReporter)
    {
        lReporter = new ChannelStatusReporter;
    }
	OD_LOG_ENTER(); //####
} // Utilities::SetUpGlobalStatusReporter

void Utilities::ShutDownGlobalStatusReporter(void)
{
	OD_LOG_ENTER(); //####
    delete lReporter;
    lReporter = NULL;
	OD_LOG_EXIT(); //####
} // Utilities::ShutDownGlobalStatusReporter

bool Utilities::StopAService(const YarpString & serviceChannelName,
                             const double       timeToWait,
                             CheckFunction      checker,
                             void *             checkStuff)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S1s("serviceChannelName = ", serviceChannelName); //####
    OD_LOG_P1("checkStuff = ", checkStuff); //####
    OD_LOG_D1("timeToWait = ", timeToWait); //####
    bool result = false;
    
    try
    {
        YarpString      aName(GetRandomChannelName(HIDDEN_CHANNEL_PREFIX_ "stop_/"
                                                   DEFAULT_CHANNEL_ROOT_));
        ClientChannel * newChannel = new ClientChannel;
        
        if (newChannel)
        {
#if defined(MpM_ReportOnConnections)
            newChannel->setReporter(*GetGlobalStatusReporter());
#endif // defined(MpM_ReportOnConnections)
            if (newChannel->openWithRetries(aName, timeToWait))
            {
                if (NetworkConnectWithRetries(aName, serviceChannelName, timeToWait, false, checker,
                                              checkStuff))
                {
                    yarp::os::Bottle parameters;
                    ServiceRequest   request(MpM_STOP_REQUEST_, parameters);
#if defined(MpM_DoExplicitCheckForOK)
                    ServiceResponse  response;
#endif // defined(MpM_DoExplicitCheckForOK)
                    
#if defined(MpM_DoExplicitCheckForOK)
                    if (! request.send(*newChannel, response))
                    {
                        OD_LOG("! (request.send(*newChannel, response))"); //####
                    }
#else // ! defined(MpM_DoExplicitCheckForOK)
                    if (! request.send(*newChannel))
                    {
                        OD_LOG("! (request.send(*newChannel))"); //####
                    }
#endif // ! defined(MpM_DoExplicitCheckForOK)
#if defined(MpM_DoExplicitDisconnect)
                    if (! NetworkDisconnectWithRetries(aName, serviceChannelName, timeToWait,
                                                       checker, checkStuff))
                    {
                        OD_LOG("(! NetworkDisconnectWithRetries(aName, serviceChannelName, " //####
                               "timeToWait, checker, checkStuff))"); //####
                    }
#endif // defined(MpM_DoExplicitDisconnect)
                }
                else
                {
                    OD_LOG("! (NetworkConnectWithRetries(aName, serviceChannelName, " //####
                           "timeToWait, false, checker, checkStuff))"); //####
                }
#if defined(MpM_DoExplicitClose)
                newChannel->close();
#endif // defined(MpM_DoExplicitClose)
            }
            else
            {
                OD_LOG("! (newChannel->openWithRetries(aName, timeToWait))"); //####
            }
            BaseChannel::RelinquishChannel(newChannel);
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_EXIT_B(result); //####
    return result;
} // Utilities::StopAService
