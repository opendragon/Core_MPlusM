//--------------------------------------------------------------------------------------------------
//
//  File:       mpm/M+MUtilities.cpp
//
//  Project:    M+M
//
//  Contains:   The function and variable declarations for utilities for M+M clients and services.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2014 by HPlus Technologies Ltd. and Simon Fraser University.
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
//  Created:    2014-03-19
//
//--------------------------------------------------------------------------------------------------

#include <mpm/M+MUtilities.h>
#include <mpm/M+MBaseClient.h>
#include <mpm/M+MClientChannel.h>
#include <mpm/M+MRequests.h>
#include <mpm/M+MSendReceiveCounters.h>
#include <mpm/M+MServiceRequest.h>
#include <mpm/M+MServiceResponse.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#include <dns_sd.h>
#include <netdb.h>
#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wc++11-extensions"
# pragma clang diagnostic ignored "-Wconversion"
# pragma clang diagnostic ignored "-Wdocumentation"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# pragma clang diagnostic ignored "-Wextra-semi"
# pragma clang diagnostic ignored "-Wpadded"
# pragma clang diagnostic ignored "-Wshadow"
# pragma clang diagnostic ignored "-Wsign-conversion"
# pragma clang diagnostic ignored "-Wunused-parameter"
# pragma clang diagnostic ignored "-Wweak-vtables"
#endif // defined(__APPLE__)
#include <yarp/os/Face.h>
#include <yarp/os/impl/BufferedConnectionWriter.h>
#include <yarp/os/impl/NameConfig.h>
#include <yarp/os/impl/PortCommand.h>
#include <yarp/os/impl/Protocol.h>
#include <yarp/os/impl/String.h>
#include <yarp/os/impl/TcpFace.h>
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 
 @brief The function and variable definitions for utilities for M+M clients and services. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::Utilities;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

/*! @brief The number of seconds to wait on a select() for mDNS operatios. */
static const int kDNSWaitTime = 2;

/*! @brief @c true once the first browse 'add' for the NameServerReporter has been seen. */
static volatile bool lSawBrowseAdd = false;

/*! @brief @c true once the resolve for the NameServerReporter has been seen. */
static volatile bool lSawResolve = false;

/*! @brief The global channel status reporter. */
static ChannelStatusReporter * lReporter = NULL;

/*! @brief The indicator string for the beginning of new information received. */
static const char * kLineMarker = "registration name ";

/*! @brief The part name being used for probing connections. */
static const char * kMagicName = "<!!!>";

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if (! MAC_OR_LINUX_)
//ASSUME WINDOWS
# define strtok_r strtok_s /* Equivalent routine for Windows. */
#endif // defined (! MAC_OR_LINUX_)

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
 @param hosttarget The target hostname of the machine providing the service.
 @param port The port, in network byte order, on which connections are accepted for this service.
 @param txtLen The length of the txt record, in bytes.
 @param txtRecord The service's primary txt record, in standard txt record format.
 @param context The context pointer that was passed by DNSServiceResolve. */
static void resolveCallback(DNSServiceRef         service,
                            DNSServiceFlags       flags,
                            uint32_t              interfaceIndex,
                            DNSServiceErrorType   errorCode,
                            const char *          fullname,
                            const char *          hosttarget,
                            uint16_t              port, /* In network byte order */
                            uint16_t              txtLen,
                            const unsigned char * txtRecord,
                            void *                context)
{
#if (! defined(OD_ENABLE_LOGGING))
# if MAC_OR_LINUX_
    //#  pragma unused(service,flags,context)
# endif // MAC_OR_LINUX_
#endif // ! defined(OD_ENABLE_LOGGING)
    OD_LOG_ENTER(); //####
    OD_LOG_P3("service = ", service, "txtRecord = ", txtRecord, "context = ", context); //####
    OD_LOG_L4("flags = ", flags, "interfaceIndex = ", interfaceIndex, "errorCode = ",//####
              errorCode, "port = ", port); //####
    OD_LOG_L1("txtLen = ", txtLen); //####
    OD_LOG_S2("fullname = ", fullname, "hosttarget = ", hosttarget); //####
    lSawResolve = true;
    if (kDNSServiceErr_NoError == errorCode)
    {
        hostent * hostAddress = gethostbyname(hosttarget);

        if (hostAddress)
        {
            in_addr * anAddress = reinterpret_cast<in_addr *>(hostAddress->h_addr_list[0]);
            
            if (anAddress)
            {
                const char *               addressAsString = inet_ntoa(*anAddress);
                yarp::os::impl::NameConfig nc;
                yarp::os::Contact          address(addressAsString, ntohs(port));
                
//                nc.setManualConfig(addressAsString, ntohs(port));
                nc.setAddress(address);
                nc.toFile();
            }
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
static void browseCallBack(DNSServiceRef       service,
                           DNSServiceFlags     flags,
                           uint32_t            interfaceIndex,
                           DNSServiceErrorType errorCode,
                           const char *        name,
                           const char *        type,
                           const char *        domain,
                           void *              context)
{
#if (! defined(OD_ENABLE_LOGGING))
# if MAC_OR_LINUX_
#  pragma unused(service,flags,context)
# endif // MAC_OR_LINUX_
#endif // ! defined(OD_ENABLE_LOGGING)
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
                            std::cerr << "DNSServiceProcessResult returned " << err << std::endl;
                            break;
                        }
                        
                    }
                    else if (0 != result)
                    {
                        int actErrno = errno;
                        
                        std::cerr << "select() returned " << result << " errno " << actErrno <<
                                    " " << strerror(actErrno) << std::endl;
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
                std::cerr << "DNSServiceResolve returned " << err << std::endl;
            }
            lSawBrowseAdd = true;
        }
    }
    else
    {
        std::cerr << "browseCallBack returned " << errorCode << std::endl;
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
            yarp::os::ConstString element(response.get(ii).asString());
            
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
            yarp::os::ConstString mode(response.get(matchLen - 1).asString());
            yarp::os::ConstString destination(response.get(matchLen - 3).asString());
            yarp::os::ConstString source(response.get(matchLen - 5).asString());
            
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
        "There", "is", "an", "output", "connection", "from", NULL, "to", NULL, "using", NULL
    };
    int          respLen = response.size();
    int          matchLen = (sizeof(matchString) / sizeof(*matchString));
    
    if (respLen >= matchLen)
    {
        bool matched = true;
        
        for (int ii = 0; matched && (ii < matchLen); ++ii)
        {
            yarp::os::ConstString element(response.get(ii).asString());
            
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
            yarp::os::ConstString mode(response.get(matchLen - 1).asString());
            yarp::os::ConstString destination(response.get(matchLen - 3).asString());
            yarp::os::ConstString source(response.get(matchLen - 5).asString());
            
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

/*! @brief Check the response to the 'getAssociates' request for validity.
 @param response The response to be checked.
 @param associates The associated ports for the port.
 @returns @c true if the response was valid and @c false otherwise. */
static bool processGetAssociatesResponse(const yarp::os::Bottle & response,
                                         PortAssociation &        associates)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S1s("response = ", response.toString()); //####
    bool result = false;
    
    try
    {
        if (MpM_EXPECTED_GETASSOCIATES_RESPONSE_SIZE <= response.size())
        {
            // The first element of the response should be 'OK' or 'FAILED'.
            yarp::os::Value responseFirst(response.get(0));
            
            if (responseFirst.isString())
            {
                yarp::os::ConstString responseFirstAsString(responseFirst.toString());
                
                if (! strcmp(MpM_OK_RESPONSE, responseFirstAsString.c_str()))
                {
                    yarp::os::Value responseSecond(response.get(1));
                    yarp::os::Value responseThird(response.get(2));
                    yarp::os::Value responseFourth(response.get(3));
                    
                    if (responseSecond.isInt() && responseThird.isList() && responseFourth.isList())
                    {
                        associates._valid = true;
                        associates._primary = (0 != responseSecond.asInt());
                        yarp::os::Bottle * thirdAsList = responseThird.asList();
                        yarp::os::Bottle * fourthAsList = responseFourth.asList();
                        
                        for (int ii = 0, mm = thirdAsList->size(); mm > ii; ++ii)
                        {
                            yarp::os::Value aPort(thirdAsList->get(ii));
                            
                            if (aPort.isString())
                            {
                                associates._inputs.push_back(aPort.toString());
                            }
                        }
                        for (int ii = 0, mm = fourthAsList->size(); mm > ii; ++ii)
                        {
                            yarp::os::Value aPort(fourthAsList->get(ii));
                            
                            if (aPort.isString())
                            {
                                associates._outputs.push_back(aPort.toString());
                            }
                        }
                        result = true;
                    }
                    else
                    {
                        OD_LOG("! (responseSecond.isInt() && responseThird.isList() && " //####
                               "responseFourth.isList())"); //####
                    }
                }
                else if (strcmp(MpM_FAILED_RESPONSE, responseFirstAsString.c_str()))
                {
                    OD_LOG("strcmp(MpM_FAILED_RESPONSE, responseFirstAsString.c_str())"); //####
                }
            }
            else
            {
                OD_LOG("! (responseFirst.isString())"); //####
            }
        }
        else
        {
            OD_LOG("! (MpM_EXPECTED_GETASSOCIATES_RESPONSE_SIZE <= response.size())"); //####
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_EXIT_B(result); //####
    return result;
} // processGetAssociatesResponse

/*! @brief Process the response from the name server.
 
 Note that each line of the response, except the last, is started with 'registration name'. This is
 followed by the port name, 'ip', the IP address, 'port' and the port number.
 @param received The response to be processed.
 @param includeHiddenPorts @c true if all ports are returned and @c false is 'hidden' ports are
 ignored.
 @param ports The list of non-default ports/ipaddress/portnumber found. */
static void processNameServerResponse(const yarp::os::ConstString & received,
                                      const bool                    includeHiddenPorts,
                                      PortVector &                  ports)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S1s("received = ", received); //####
    OD_LOG_B1("includeHiddenPorts = ", includeHiddenPorts); //####
    size_t                lineMakerLength = strlen(kLineMarker);
    yarp::os::ConstString nameServerName(yarp::os::Network::getNameServerName());
    yarp::os::ConstString workingCopy(received);
    
    OD_LOG_S1s("nameServerName = ", nameServerName); //####
    for (size_t nextPos = 0; yarp::os::ConstString::npos != nextPos; )
    {
        nextPos = workingCopy.find(kLineMarker);
        if (yarp::os::ConstString::npos != nextPos)
        {
            workingCopy = workingCopy.substr(nextPos + lineMakerLength);
            size_t chopPos = workingCopy.find(kLineMarker);
            
            if (yarp::os::ConstString::npos != chopPos)
            {
                char *                channelName = NULL;
                yarp::os::ConstString chopped(workingCopy.substr(0, chopPos));
                char *                choppedAsChars = strdup(chopped.c_str());
                char *                ipAddress = NULL;
                char *                saved;
                char *                pp = strtok_r(choppedAsChars, " ", &saved);
                
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
                    if (! strncmp(channelName, HIDDEN_CHANNEL_PREFIX,
                                  sizeof(HIDDEN_CHANNEL_PREFIX) - 1))
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

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

bool Utilities::AddConnection(const yarp::os::ConstString & fromPortName,
                              const yarp::os::ConstString & toPortName,
                              const double                  timeToWait,
                              const bool                    isUDP,
                              Common::CheckFunction         checker,
                              void *                        checkStuff)
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

void Utilities::CheckForNameServerReporter(void)
{
    OD_LOG_ENTER(); //####
    bool                       skipNameServerScan = false;
    yarp::os::impl::NameConfig nc;

    // First, see if there is a configuration file and it points to a real server.
    if (nc.fromFile())
    {
        yarp::os::Contact address = nc.getAddress();
        
        if (address.isValid())
        {
            yarp::os::impl::TcpFace    aFace;
            yarp::os::OutputProtocol * outp = aFace.write(address);
            
            if (outp)
            {
                delete outp;
                skipNameServerScan = true;
            }
        }
    }
    if (! skipNameServerScan)
    {
        lSawBrowseAdd = false;
        DNSServiceRef       serviceRef = NULL;
        static const char * regType = MpM_MDNS_NAMESERVER_REPORT;
        DNSServiceErrorType err = DNSServiceBrowse(&serviceRef, 0, 0, regType, NULL /* domain */,
                                                   browseCallBack, NULL);
        
        if (kDNSServiceErr_NoError == err)
        {
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
                        std::cerr << "DNSServiceProcessResult returned " << err << std::endl;
                        break;
                    }
                    
                }
                else if (0 != result)
                {
                    int actErrno = errno;
                    
                    std::cerr << "select() returned " << result << " errno " << actErrno << " " <<
                    strerror(actErrno) << std::endl;
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

bool Utilities::CheckForRegistryService(const PortVector & ports)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P1("ports = ", &ports); //####
    bool result = false;
    
    if (0 < ports.size())
    {
        for (PortVector::const_iterator walker(ports.begin()); ports.end() != walker; ++walker)
        {
            if (walker->_portName == MpM_REGISTRY_CHANNEL_NAME)
            {
                result = true;
                break;
            }
            
        }
    }
    OD_LOG_EXIT_B(result); //####
    return result;
} // Utilities::CheckForRegistryService

yarp::os::ConstString Utilities::ConvertMetricsToString(const yarp::os::Bottle & metrics,
                                                        Common::OutputFlavour    flavour)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P1("metrics = ", &metrics); //####
    bool                  sawSome = false;
    int                   channelWidth = 0;
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
                    yarp::os::ConstString theTagAsString;
                    
                    if (propList->check(MpM_SENDRECEIVE_CHANNEL_))
                    {
                        yarp::os::Value theChannel = propList->find(MpM_SENDRECEIVE_CHANNEL_);
                        
                        if (theChannel.isString())
                        {
                            int ww = theChannel.toString().size();
                            
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
                yarp::os::Bottle *    theInBytesAsList = NULL;
                yarp::os::Bottle *    theInMessagesAsList = NULL;
                yarp::os::Bottle *    theOutBytesAsList = NULL;
                yarp::os::Bottle *    theOutMessagesAsList = NULL;
                yarp::os::ConstString theChannelAsString;
                yarp::os::ConstString theDateAsString;
                yarp::os::ConstString theTimeAsString;
                int64_t               inByteCount = 0;
                int64_t               inMessageCount = 0;
                int64_t               outByteCount = 0;
                int64_t               outMessageCount = 0;
                
                if (propList->check(MpM_SENDRECEIVE_CHANNEL_))
                {
                    yarp::os::Value theChannel = propList->find(MpM_SENDRECEIVE_CHANNEL_);
                    
                    if (theChannel.isString())
                    {
                        theChannelAsString = theChannel.toString();
                    }
                }
                if (propList->check(MpM_SENDRECEIVE_DATE_))
                {
                    yarp::os::Value theDate = propList->find(MpM_SENDRECEIVE_DATE_);
                    
                    if (theDate.isString())
                    {
                        theDateAsString = theDate.toString();
                    }
                }
                if (propList->check(MpM_SENDRECEIVE_TIME_))
                {
                    yarp::os::Value theTime = propList->find(MpM_SENDRECEIVE_TIME_);
                    
                    if (theTime.isString())
                    {
                        theTimeAsString = theTime.toString();
                    }
                }
                if (propList->check(MpM_SENDRECEIVE_INBYTES_))
                {
                    yarp::os::Value theInBytes = propList->find(MpM_SENDRECEIVE_INBYTES_);
                    
                    if (theInBytes.isList())
                    {
                        theInBytesAsList = theInBytes.asList();
                        if (2 == theInBytesAsList->size())
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
                if (propList->check(MpM_SENDRECEIVE_INMESSAGES_))
                {
                    yarp::os::Value theInMessages = propList->find(MpM_SENDRECEIVE_INMESSAGES_);
                    
                    if (theInMessages.isList())
                    {
                        theInMessagesAsList = theInMessages.asList();
                        if (2 == theInMessagesAsList->size())
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
                if (propList->check(MpM_SENDRECEIVE_OUTBYTES_))
                {
                    yarp::os::Value theOutBytes = propList->find(MpM_SENDRECEIVE_OUTBYTES_);
                    
                    if (theOutBytes.isList())
                    {
                        theOutBytesAsList = theOutBytes.asList();
                        if (2 == theOutBytesAsList->size())
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
                if (propList->check(MpM_SENDRECEIVE_OUTMESSAGES_))
                {
                    yarp::os::Value theOutMessages = propList->find(MpM_SENDRECEIVE_OUTMESSAGES_);
                    
                    if (theOutMessages.isList())
                    {
                        theOutMessagesAsList = theOutMessages.asList();
                        if (2 == theOutMessagesAsList->size())
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
                if (theInBytesAsList && theInMessagesAsList && theOutBytesAsList &&
                    theOutMessagesAsList)
                {
                    switch (flavour)
                    {
                        case kOutputFlavourTabs :
                            if (sawSome)
                            {
                                result << std::endl;
                            }
                            result << theChannelAsString.c_str() << "\t" <<
                                    theDateAsString.c_str() << "\t" << theTimeAsString.c_str() <<
                                    "\t" << inByteCount << "\t" << outByteCount << "\t" <<
                                    inMessageCount << "\t" << outMessageCount;
                            break;
                            
                        case kOutputFlavourJSON :
                            if (sawSome)
                            {
                                result << ", ";
                            }
                            result << T_("{ " CHAR_DOUBLEQUOTE "channel" CHAR_DOUBLEQUOTE ": "
                                         CHAR_DOUBLEQUOTE) <<
                                    SanitizeString(theChannelAsString).c_str() <<
                                    T_(CHAR_DOUBLEQUOTE ", " CHAR_DOUBLEQUOTE "date"
                                       CHAR_DOUBLEQUOTE ": " CHAR_DOUBLEQUOTE) <<
                                    theDateAsString.c_str() << T_(CHAR_DOUBLEQUOTE ", "
                                                                  CHAR_DOUBLEQUOTE "time"
                                                                  CHAR_DOUBLEQUOTE ": "
                                                                  CHAR_DOUBLEQUOTE) <<
                                    theTimeAsString.c_str() << T_(CHAR_DOUBLEQUOTE ", "
                                                                  CHAR_DOUBLEQUOTE "inBytes"
                                                                  CHAR_DOUBLEQUOTE ": "
                                                                  CHAR_DOUBLEQUOTE) <<
                                    inByteCount << T_(CHAR_DOUBLEQUOTE ", " CHAR_DOUBLEQUOTE
                                                      "inMessages" CHAR_DOUBLEQUOTE ": "
                                                      CHAR_DOUBLEQUOTE) << inMessageCount <<
                                    T_(CHAR_DOUBLEQUOTE ", " CHAR_DOUBLEQUOTE "outBytes"
                                       CHAR_DOUBLEQUOTE ": " CHAR_DOUBLEQUOTE) << outByteCount <<
                                    T_(CHAR_DOUBLEQUOTE ", " CHAR_DOUBLEQUOTE "outMessages"
                                       CHAR_DOUBLEQUOTE ": " CHAR_DOUBLEQUOTE) <<
                                    outMessageCount << T_(CHAR_DOUBLEQUOTE " }");
                            break;
                            
                        case kOutputFlavourNormal :
                            if (sawSome)
                            {
                                result << std::endl;
                            }
                            result.width(channelWidth);
                            result << theChannelAsString.c_str() << ": [date: " <<
                                    theDateAsString.c_str() << ", time: " <<
                                    theTimeAsString.c_str() << ", bytes in: " << inByteCount <<
                                    ", out: " << outByteCount << ", messages in: " <<
                                    inMessageCount << ", out: " << outMessageCount << "]";
                            break;
                            
                        default :
                            break;
                            
                    }
                    sawSome = true;
                }
            }
        }
    }
    if (kOutputFlavourJSON == flavour)
    {
        result << " ]";
    }
    OD_LOG_EXIT_S(result.str()); //####
    return result.str();
} // Utilities::ConvertMetricsToString

void Utilities::GatherPortConnections(const yarp::os::ConstString & portName,
                                      ChannelVector &               inputs,
                                      ChannelVector &               outputs,
                                      const InputOutputFlag         which,
                                      const bool                    quiet,
                                      Common::CheckFunction         checker,
                                      void *                        checkStuff)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P3("inputs = ", &inputs, "outputs = ", &outputs, "checkStuff = ", checkStuff); //####
    OD_LOG_L1("which = ", static_cast<int>(which)); //####
    OD_LOG_B1("quiet = ", quiet); //####
    yarp::os::Contact address = yarp::os::Network::queryName(portName.c_str());
    
    inputs.clear();
    outputs.clear();
    if (address.isValid())
    {
        if ((address.getCarrier() == "tcp") || (address.getCarrier() == "fast_tcp") ||
            (address.getCarrier() == "xmlrpc"))
        {
            // Note that the following connect() call will hang indefinitely if the address given is
            // for an 'output' port that is connected to another 'output' port. 'yarp ping /port'
            // will hang as well.
            yarp::os::OutputProtocol * out = yarp::os::impl::Carriers::connect(address);
            
            if (out)
            {
                yarp::os::Route rr(kMagicName, portName.c_str(), "text_ack");
                
                if (out->open(rr))
                {
                    yarp::os::Bottle                         resp;
                    yarp::os::impl::BufferedConnectionWriter bw(out->getConnection().isTextMode());
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
                        yarp::os::ConstString checkString(resp.get(0).asString());
                        
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
    OD_LOG_EXIT(); //####
} // Utilities::GatherPortConnections

bool Utilities::GetAssociatedPorts(const yarp::os::ConstString & portName,
                                   PortAssociation &             associates,
                                   const double                  timeToWait,
                                   Common::CheckFunction         checker,
                                   void *                        checkStuff)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S1s("portName = ", portName); //####
    OD_LOG_D1("timeToWait = ", timeToWait); //####
    OD_LOG_P1("checkStuff = ", checkStuff); //####
    bool result = false;
    
    associates._inputs.clear();
    associates._outputs.clear();
    associates._primary = associates._valid = false;
    try
    {
        yarp::os::ConstString   aName(GetRandomChannelName(HIDDEN_CHANNEL_PREFIX "getassociates_/"
                                                           DEFAULT_CHANNEL_ROOT));
        ClientChannel *         newChannel = new ClientChannel;
#if defined(MpM_ReportOnConnections)
        ChannelStatusReporter * reporter = GetGlobalStatusReporter();
#endif // defined(MpM_ReportOnConnections)
        
        if (newChannel)
        {
#if defined(MpM_ReportOnConnections)
            newChannel->setReporter(reporter);
#endif // defined(MpM_ReportOnConnections)
            if (newChannel->openWithRetries(aName, timeToWait))
            {
                if (NetworkConnectWithRetries(aName, MpM_REGISTRY_CHANNEL_NAME, timeToWait, false,
                                              checker, checkStuff))
                {
                    yarp::os::Bottle parameters;
                    
                    parameters.addString(portName);
                    ServiceRequest  request(MpM_GETASSOCIATES_REQUEST, parameters);
                    ServiceResponse response;
                    
                    if (request.send(*newChannel, &response))
                    {
                        OD_LOG_S1s("response <- ", response.asString()); //####
                        result = processGetAssociatesResponse(response.values(), associates);
                    }
                    else
                    {
                        OD_LOG("! (request.send(*newChannel, &response))"); //####
                    }
#if defined(MpM_DoExplicitDisconnect)
                    if (! NetworkDisconnectWithRetries(aName, MpM_REGISTRY_CHANNEL_NAME,
                                                       timeToWait, checker, checkStuff))
                    {
                        OD_LOG("(! NetworkDisconnectWithRetries(aName, " //####
                               "MpM_REGISTRY_CHANNEL_NAME, timeToWait, checker, " //####
                               "checkStuff))"); //####
                    }
#endif // defined(MpM_DoExplicitDisconnect)
                }
                else
                {
                    OD_LOG("! (NetworkConnectWithRetries(aName, MpM_REGISTRY_CHANNEL_NAME, " //####
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
} // Utilities::GetAssociatedPorts

void Utilities::GetDateAndTime(char *       dateBuffer,
                               const size_t dateBufferSize,
                               char *       timeBuffer,
                               const size_t timeBufferSize)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("dateBuffer = ", dateBuffer, "timeBuffer = ", timeBuffer); //####
    OD_LOG_L2("dateBufferSize = ", dateBufferSize, "timeBufferSize = ", timeBufferSize); //####
    time_t rawtime;
    
    time(&rawtime);
    struct tm * locTime = localtime(&rawtime);
    
#if MAC_OR_LINUX_
    strftime(dateBuffer, dateBufferSize, "%F", locTime);
    strftime(timeBuffer, timeBufferSize, "%T", locTime);
#else // ! MAC_OR_LINUX_
    strftime(dateBuffer, dateBufferSize, "%x", locTime);
    strftime(timeBuffer, timeBufferSize, "%X", locTime);
#endif // ! MAC_OR_LINUX_
    OD_LOG_EXIT(); //####
} // Utilities::GetDateAndTime

bool Utilities::GetDetectedPortList(PortVector & ports,
                                    const bool   includeHiddenPorts)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P1("ports = ", &ports); //####
    OD_LOG_B1("includeHiddenPorts = ", includeHiddenPorts); //####
    bool                   okSoFar = false;
    yarp::os::Bottle       request;
    yarp::os::Bottle       response;
    yarp::os::ContactStyle contactInfo;
    
    ports.clear();
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
        char buffer1[DATE_TIME_BUFFER_SIZE];
        char buffer2[DATE_TIME_BUFFER_SIZE];
        
        GetDateAndTime(buffer1, sizeof(buffer1), buffer2, sizeof(buffer2));
        std::cerr << buffer1 << " " << buffer2 << "Problem getting list of ports." << std::endl;
    }
    OD_LOG_EXIT_B(okSoFar); //####
    return okSoFar;
} // Utilities::GetDetectedPortList

ChannelStatusReporter * Utilities::GetGlobalStatusReporter(void)
{
    return lReporter;
} // Utilities::GetGlobalStatusReporter

bool Utilities::GetMetricsForService(const yarp::os::ConstString & serviceChannelName,
                                     yarp::os::Bottle &            metrics,
                                     const double                  timeToWait,
                                     Common::CheckFunction         checker,
                                     void *                        checkStuff)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S1s("serviceChannelName = ", serviceChannelName); //####
    OD_LOG_P2("metrics = ", &metrics, "checkStuff = ", checkStuff); //####
    OD_LOG_D1("timeToWait = ", timeToWait); //####
    bool                  result = false;
    yarp::os::ConstString aName(GetRandomChannelName(HIDDEN_CHANNEL_PREFIX "servicemetrics_/"
                                                     DEFAULT_CHANNEL_ROOT));
    ClientChannel *       newChannel = new ClientChannel;
    
    if (newChannel)
    {
        if (newChannel->openWithRetries(aName, timeToWait))
        {
            if (NetworkConnectWithRetries(aName, serviceChannelName, timeToWait, false, checker,
                                          checkStuff))
            {
                yarp::os::Bottle parameters;
                ServiceRequest   request(MpM_METRICS_REQUEST, parameters);
                ServiceResponse  response;
                
                if (request.send(*newChannel, &response))
                {
                    OD_LOG_S1s("response <- ", response.asString()); //####
                    metrics = response.values();
                    result = (0 < response.count());
                }
                else
                {
                    OD_LOG("! (request.send(*newChannel, &response))"); //####
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
} // Utilities::GetMetricsForService

bool Utilities::GetNameAndDescriptionForService(const yarp::os::ConstString & serviceChannelName,
                                                ServiceDescriptor &           descriptor,
                                                const double                  timeToWait,
                                                Common::CheckFunction         checker,
                                                void *                        checkStuff)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S1s("serviceChannelName = ", serviceChannelName); //####
    OD_LOG_P2("descriptor = ", &descriptor, "checkStuff = ", checkStuff); //####
    OD_LOG_D1("timeToWait = ", timeToWait); //####
    bool                  result = false;
    yarp::os::ConstString aName(GetRandomChannelName(HIDDEN_CHANNEL_PREFIX "servicelister_/"
                                                     DEFAULT_CHANNEL_ROOT));
    ClientChannel *       newChannel = new ClientChannel;
    
    if (newChannel)
    {
        if (newChannel->openWithRetries(aName, timeToWait))
        {
            if (NetworkConnectWithRetries(aName, serviceChannelName, timeToWait, false, checker,
                                          checkStuff))
            {
                yarp::os::Bottle parameters1;
                ServiceRequest   request1(MpM_NAME_REQUEST, parameters1);
                ServiceResponse  response1;
                
                if (request1.send(*newChannel, &response1))
                {
                    OD_LOG_S1s("response1 <- ", response1.asString()); //####
                    if (MpM_EXPECTED_NAME_RESPONSE_SIZE == response1.count())
                    {
                        yarp::os::Value theCanonicalName(response1.element(0));
                        yarp::os::Value theDescription(response1.element(1));
                        yarp::os::Value theKind(response1.element(2));
                        yarp::os::Value thePath(response1.element(3));
                        yarp::os::Value theRequestsDescription(response1.element(4));
                        yarp::os::Value theTag(response1.element(5));
                        
                        OD_LOG_S4s("theCanonicalName <- ", theCanonicalName.toString(), //####
                                   "theDescription <- ", theDescription.toString(), //####
                                   "theKind <- ", theKind.toString(), "thePath <- ", //####
                                   thePath.toString()); //####
                        OD_LOG_S2s("theRequestsDescription = ", //####
                                   theRequestsDescription.toString(), "theTag = ", //####
                                   theTag.toString()); //####
                        if (theCanonicalName.isString() && theDescription.isString() &&
                            theKind.isString() && thePath.isString() &&
                            theRequestsDescription.isString() && theTag.isString())
                        {
                            descriptor._channelName = serviceChannelName;
                            descriptor._serviceName = theCanonicalName.toString();
                            descriptor._description = theDescription.toString();
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
                                   "theKind.isString() && thePath.isString() && " //####
                                   "theRequestsDescription.isString() && " //####
                                   "theTag.isString())"); //####
                        }
                    }
                    else
                    {
                        OD_LOG("! (MpM_EXPECTED_NAME_RESPONSE_SIZE == response1.count())"); //####
                        OD_LOG_S1s("response1 = ", response1.asString()); //####
                    }
                }
                else
                {
                    OD_LOG("! (request1.send(*newChannel, &response1))"); //####
                }
                if (result)
                {
                    yarp::os::Bottle parameters2;
                    ServiceRequest   request2(MpM_CHANNELS_REQUEST, parameters2);
                    ServiceResponse  response2;
                    
                    if (request2.send(*newChannel, &response2))
                    {
                        if (MpM_EXPECTED_CHANNELS_RESPONSE_SIZE == response2.count())
                        {
                            yarp::os::Value theInputChannels(response2.element(0));
                            yarp::os::Value theOutputChannels(response2.element(1));
                            
                            OD_LOG_S2s("theInputChannels <- ", theInputChannels.toString(), //####
                                       "theOutputChannels <- ", //####
                                       theOutputChannels.toString()); //####
                            if (theInputChannels.isList() && theOutputChannels.isList())
                            {
                                yarp::os::Bottle * inputChannelsAsList = theInputChannels.asList();
                                yarp::os::Bottle * outputChannelsAsList =
                                                                        theOutputChannels.asList();
                                
                                for (int ii = 0, howMany = inputChannelsAsList->size();
                                     ii < howMany; ++ii)
                                {
                                    yarp::os::Value element(inputChannelsAsList->get(ii));
                                    
                                    if (element.isList())
                                    {
                                        yarp::os::Bottle * inputChannelAsList = element.asList();
                                        
                                        if (MpM_EXPECTED_CHANNEL_DESCRIPTOR_SIZE ==
                                                                        inputChannelAsList->size())
                                        {
                                            yarp::os::Value firstValue(inputChannelAsList->get(0));
                                            yarp::os::Value secondValue(inputChannelAsList->get(1));
                                            yarp::os::Value thirdValue(inputChannelAsList->get(2));
                                            
                                            if (firstValue.isString() && secondValue.isString() &&
                                                thirdValue.isString())
                                            {
                                                ChannelDescription aChannel;
                                                
                                                aChannel._portName = firstValue.asString();
                                                aChannel._portProtocol = secondValue.asString();
                                                aChannel._portMode = kChannelModeOther;
                                                aChannel._protocolDescription =
                                                                            thirdValue.asString();
                                                descriptor._inputChannels.push_back(aChannel);
                                            }
                                        }
                                    }
                                }
                                for (int ii = 0, howMany = outputChannelsAsList->size();
                                     ii < howMany; ++ii)
                                {
                                    yarp::os::Value element(outputChannelsAsList->get(ii));
                                    
                                    if (element.isList())
                                    {
                                        yarp::os::Bottle * outputChannelAsList = element.asList();
                                        
                                        if (MpM_EXPECTED_CHANNEL_DESCRIPTOR_SIZE ==
                                                                    outputChannelAsList->size())
                                        {
                                            yarp::os::Value firstValue(outputChannelAsList->get(0));
                                            yarp::os::Value secondValue =
                                                                        outputChannelAsList->get(1);
                                            yarp::os::Value thirdValue(outputChannelAsList->get(2));
                                            
                                            if (firstValue.isString() && secondValue.isString() &&
                                                thirdValue.isString())
                                            {
                                                ChannelDescription aChannel;
                                                
                                                aChannel._portName = firstValue.asString();
                                                aChannel._portProtocol = secondValue.asString();
                                                aChannel._portMode = kChannelModeOther;
                                                aChannel._protocolDescription =
                                                                            thirdValue.asString();
                                                descriptor._outputChannels.push_back(aChannel);
                                            }
                                        }
                                    }
                                }
                            }
                            else
                            {
                                OD_LOG("! (theInputChannels.isList() && " //####
                                       "theOutputChannels.isList())");
                            }
                        }
                        else
                        {
                            OD_LOG("! (MpM_EXPECTED_CHANNELS_RESPONSE_SIZE == " //####
                                   "response2.count())"); //####
                            OD_LOG_S1s("response2 = ", response2.asString()); //####
                        }
                    }
                    else
                    {
                        OD_LOG("! (request2.send(*newChannel, &response2))"); //####
                        result = false;
                    }
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
} // Utilities::GetNameAndDescriptionForService

Utilities::PortKind Utilities::GetPortKind(const yarp::os::ConstString & portName)
{
    const char * portNameChars = portName.c_str();
    const size_t kAdapterPortNameBaseLen = sizeof(ADAPTER_PORT_NAME_BASE) - 1;
    const size_t kClientPortNameBaseLen = sizeof(CLIENT_PORT_NAME_BASE) - 1;
    const size_t kDefaultServiceNameBaseLen = sizeof(DEFAULT_SERVICE_NAME_BASE) - 1;
    PortKind     result;
    
    if (! strcmp(MpM_REGISTRY_CHANNEL_NAME, portNameChars))
    {
        result = kPortKindServiceRegistry;
    }
    else if (! strncmp(DEFAULT_SERVICE_NAME_BASE, portNameChars, kDefaultServiceNameBaseLen))
    {
        result = kPortKindService;
    }
    else if (! strncmp(ADAPTER_PORT_NAME_BASE, portNameChars, kAdapterPortNameBaseLen))
    {
        result = kPortKindAdapter;
    }
    else if (! strncmp(CLIENT_PORT_NAME_BASE, portNameChars, kClientPortNameBaseLen))
    {
        result = kPortKindClient;
    }
    else
    {
        result = kPortKindStandard;
    }
    return result;
} // Utilities::GetPortKind

yarp::os::ConstString Utilities::GetPortLocation(const yarp::os::ConstString & portName)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S1s("portName = ", portName); //####
    yarp::os::ConstString result;
    yarp::os::Contact     whereItIs = yarp::os::NetworkBase::queryName(portName);

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
    OD_LOG_EXIT_S(result.c_str()); //####
    return result;
} // Utilities::GetPortLocation

bool Utilities::GetServiceNames(StringVector &        services,
                                const bool            quiet,
                                Common::CheckFunction checker,
                                void *                checkStuff)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("services = ", &services, "checkStuff = ", checkStuff); //####
    OD_LOG_B1("quiet = ", quiet); //####
    bool okSoFar = GetServiceNamesFromCriteria(MpM_REQREP_DICT_REQUEST_KEY ":*", services, quiet,
                                               checker, checkStuff);
    
    OD_LOG_EXIT_B(okSoFar); //####
    return okSoFar;
} // Utilities::GetServiceNames

bool Utilities::GetServiceNamesFromCriteria(const yarp::os::ConstString & criteria,
                                            Common::StringVector &        services,
                                            const bool                    quiet,
                                            Common::CheckFunction         checker,
                                            void *                        checkStuff)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S1s("criteria = ", criteria); //####
    OD_LOG_P2("services = ", &services, "checkStuff = ", checkStuff); //####
    OD_LOG_B1("quiet = ", quiet); //####
    bool             okSoFar = false;
    yarp::os::Bottle matches(FindMatchingServices(criteria, false, checker, checkStuff));
    
    if (MpM_EXPECTED_MATCH_RESPONSE_SIZE == matches.size())
    {
        // First, check if the search succeeded.
        yarp::os::ConstString matchesFirstString(matches.get(0).toString());
        
        if (strcmp(MpM_OK_RESPONSE, matchesFirstString.c_str()))
        {
            OD_LOG("(strcmp(MpM_OK_RESPONSE, matchesFirstString.c_str()))"); //####
            if (! quiet)
            {
#if MAC_OR_LINUX_
                yarp::os::ConstString reason(matches.get(1).toString());
                
                GetLogger().fail(yarp::os::ConstString("Failed: ") + reason + ".");
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
        OD_LOG("! (MpM_EXPECTED_MATCH_RESPONSE_SIZE == matches.size())"); //####
        if (! quiet)
        {
#if MAC_OR_LINUX_
            GetLogger().fail("Problem getting information from the Service Registry.");
#endif // MAC_OR_LINUX_
        }
    }
    if ((! okSoFar) && (! quiet))
    {
        char buffer1[DATE_TIME_BUFFER_SIZE];
        char buffer2[DATE_TIME_BUFFER_SIZE];
        
        GetDateAndTime(buffer1, sizeof(buffer1), buffer2, sizeof(buffer2));
        std::cerr << buffer1 << " " << buffer2 << " Problem getting list of service names." <<
        std::endl;
    }
    OD_LOG_EXIT_B(okSoFar); //####
    return okSoFar;
} // Utilities::GetServiceNamesFromCriteria

const char * Utilities::MapServiceKindToString(const ServiceKind kind)
{
    OD_LOG_ENTER(); //####
    OD_LOG_L1("kind = ", static_cast<int>(kind)); //####
    const char * result;
    
    switch (kind)
    {
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

ServiceKind Utilities::MapStringToServiceKind(const yarp::os::ConstString & kindString)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S1s("kindString = ", kindString); //####
    ServiceKind  result;
    const char * kindStringChars = kindString.c_str();
    
    if (! strcmp("Filter", kindStringChars))
    {
        result =kServiceKindFilter;
    }
    else if (! strcmp("Input", kindStringChars))
    {
        result =kServiceKindInput;
    }
    else if (! strcmp("Output", kindStringChars))
    {
        result =kServiceKindOutput;
    }
    else if (! strcmp("Registry", kindStringChars))
    {
        result =kServiceKindRegistry;
    }
    else
    {
        result =kServiceKindNormal;
    }
    OD_LOG_EXIT_L(static_cast<int>(result)); //####
    return result;
} // Utilities::MapStringToServiceKind

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
bool Utilities::NetworkConnectWithRetries(const yarp::os::ConstString & sourceName,
                                          const yarp::os::ConstString & destinationName,
                                          const double                  timeToWait,
                                          const bool                    isUDP,
                                          Common::CheckFunction         checker,
                                          void *                        checkStuff)
{
#if ((! RETRY_LOOPS_USE_TIMEOUTS) && (! defined(OD_ENABLE_LOGGING)))
# if MAC_OR_LINUX_
#  pragma unused(timeToWait)
# endif // MAC_OR_LINUX_
#endif // (! RETRY_LOOPS_USE_TIMEOUTS) && (! defined(OD_ENABLE_LOGGING))
    OD_LOG_ENTER(); //####
    OD_LOG_S2s("sourceName = ", sourceName, "destinationName = ", destinationName); //####
    OD_LOG_D1("timeToWait = ", timeToWait); //####
    OD_LOG_B1("isUDP = ", isUDP); //####
    OD_LOG_P1("checkStuff = ", checkStuff); //####
    bool result = false;
    
    if (yarp::os::Network::exists(sourceName) && yarp::os::Network::exists(destinationName))
    {
        double retryTime = INITIAL_RETRY_INTERVAL;
        int    retriesLeft = MAX_RETRIES;
        
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
#if (defined(OD_ENABLE_LOGGING) && defined(MpM_LogIncludesYarpTrace))
                result = yarp::os::Network::connect(sourceName, destinationName, carrier, false);
#else // ! (defined(OD_ENABLE_LOGGING) && defined(MpM_LogIncludesYarpTrace))
                result = yarp::os::Network::connect(sourceName, destinationName, carrier, true);
#endif // ! (defined(OD_ENABLE_LOGGING) && defined(MpM_LogIncludesYarpTrace))
                OD_LOG("connected?"); //####
                if (! result)
                {
                    if (0 < --retriesLeft)
                    {
                        OD_LOG("%%retry%%"); //####
                        yarp::os::Time::delay(retryTime);
                        retryTime *= RETRY_MULTIPLIER;
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
bool Utilities::NetworkDisconnectWithRetries(const yarp::os::ConstString & sourceName,
                                             const yarp::os::ConstString & destinationName,
                                             const double                  timeToWait,
                                             Common::CheckFunction         checker,
                                             void *                        checkStuff)
{
#if ((! RETRY_LOOPS_USE_TIMEOUTS) && (! defined(OD_ENABLE_LOGGING)))
# if MAC_OR_LINUX_
#  pragma unused(timeToWait)
# endif // MAC_OR_LINUX_
#endif // (! RETRY_LOOPS_USE_TIMEOUTS) && (! defined(OD_ENABLE_LOGGING))
    OD_LOG_ENTER(); //####
    OD_LOG_S2s("sourceName = ", sourceName, "destinationName = ", destinationName); //####
    OD_LOG_D1("timeToWait = ", timeToWait); //####
    OD_LOG_P1("checkStuff = ", checkStuff); //####
    bool result = false;
    
    if (yarp::os::Network::exists(sourceName) && yarp::os::Network::exists(destinationName))
    {
        double retryTime = INITIAL_RETRY_INTERVAL;
        int    retriesLeft = MAX_RETRIES;
        
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
#if (defined(OD_ENABLE_LOGGING) && defined(MpM_LogIncludesYarpTrace))
                result = yarp::os::Network::disconnect(sourceName, destinationName, false);
#else // ! (defined(OD_ENABLE_LOGGING) && defined(MpM_LogIncludesYarpTrace))
                result = yarp::os::Network::disconnect(sourceName, destinationName, true);
#endif // ! (defined(OD_ENABLE_LOGGING) && defined(MpM_LogIncludesYarpTrace))
                OD_LOG("disconnected?"); //####
                if (! result)
                {
                    if (0 < --retriesLeft)
                    {
                        OD_LOG("%%retry%%"); //####
                        yarp::os::Time::delay(retryTime);
                        retryTime *= RETRY_MULTIPLIER;
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

bool Utilities::RemoveConnection(const yarp::os::ConstString & fromPortName,
                                 const yarp::os::ConstString & toPortName,
                                 Common::CheckFunction         checker,
                                 void *                        checkStuff)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S2s("fromPortName = ", fromPortName, "toPortName = ", toPortName); //####
    OD_LOG_P1("checkStuff = ", checkStuff); //####
    bool result = NetworkDisconnectWithRetries(fromPortName, toPortName, STANDARD_WAIT_TIME,
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
                yarp::os::ConstString port = entry->check("name", yarp::os::Value("")).asString();
                
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
                                char buffer1[DATE_TIME_BUFFER_SIZE];
                                char buffer2[DATE_TIME_BUFFER_SIZE];
                                
                                GetDateAndTime(buffer1, sizeof(buffer1), buffer2, sizeof(buffer2));
                                yarp::os::NetworkBase::unregisterName(port);
                                std::cerr << buffer1 << " " << buffer2 <<
                                            " Removing stale port '" << port.c_str() << "'." <<
                                            std::endl;
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
    yarp::os::ConstString serverName = yarp::os::NetworkBase::getNameServerName();
    yarp::os::Bottle      cmd2("gc");
    yarp::os::Bottle      reply2;
    
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

void Utilities::SetUpGlobalStatusReporter(void)
{
    if (! lReporter)
    {
        lReporter = new ChannelStatusReporter;
    }
} // Utilities::SetUpGlobalStatusReporter

void Utilities::ShutDownGlobalStatusReporter(void)
{
    delete lReporter;
    lReporter = NULL;
} // Utilities::ShutDownGlobalStatusReporter
