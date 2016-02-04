//--------------------------------------------------------------------------------------------------
//
//  File:       m+mNameServerReportingThread.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for a name server reporting for m+m.
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
//  Created:    2014-06-18
//
//--------------------------------------------------------------------------------------------------

#include "m+mNameServerReportingThread.h"

#include <m+m/m+mRequests.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for a registry check thread for m+m. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::Registry;
using std::cerr;
using std::endl;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
/*! @brief The mDNS registration callback.
 @param service The DNSServiceRef initialized by DNSServiceRegister.
 @param flags The flags for the registration.
 @param errorCode @c kDNSServiceErr_NoError on success.
 @param name The service name that was registered.
 @param type The type of service that was registered.
 @param domain The domain on which the service was registered.
 @param context The context pointer that was passed by DSNServiceRegister. */
static void DNSSD_API
registrationCallback(DNSServiceRef       service,
                     DNSServiceFlags     flags,
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
    OD_LOG_L2("flags = ", flags, "errorCode = ", errorCode); //####
    OD_LOG_S3("name = ", name, "type = ", type, "domain = ", domain); //####
    if (kDNSServiceErr_NoError == errorCode)
    {
        cerr << "registered " << name << "." << type << domain << endl;
    }
    else
    {
        cerr << "not registered -> " << errorCode << endl;
    }
    OD_LOG_EXIT(); //####
} // registrationCallback
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and Destructors
#endif // defined(__APPLE__)

NameServerReportingThread::NameServerReportingThread(void) :
    inherited(), _serviceRef(NULL)
{
    OD_LOG_ENTER(); //####
    OD_LOG_EXIT_P(this); //####
} // NameServerReportingThread::NameServerReportingThread

NameServerReportingThread::~NameServerReportingThread(void)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_OBJEXIT(); //####
} // NameServerReportingThread::~NameServerReportingThread

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

DEFINE_RUN_(NameServerReportingThread)
{
    OD_LOG_OBJENTER(); //####
    int            dns_sd_fd = DNSServiceRefSockFD(_serviceRef);
    int            nfds = dns_sd_fd + 1;
    fd_set         readfds;
    struct timeval tv;
    int            result;
    
    for ( ; ! isStopping(); )
    {
        FD_ZERO(&readfds);
        FD_SET(dns_sd_fd, &readfds);
        tv.tv_sec = 2; // We want to make sure that we see a stop request!
        tv.tv_usec = 0;
        result = select(nfds, &readfds, NULL, NULL, &tv);
        if (0 < result)
        {
            DNSServiceErrorType err = kDNSServiceErr_NoError;
            
            if (FD_ISSET(dns_sd_fd, &readfds))
            {
                err = DNSServiceProcessResult(_serviceRef);
            }
            if (err)
            {
                cerr << "DNSServiceProcessResult returned " << err << endl;
                break;
            }
            
        }
        else if (0 != result)
        {
            int    actErrno = errno;
            char   errBuff[256];
#if MAC_OR_LINUX_
# if LINUX_
            char * res;
# else // ! LINUX_
            int    res;
# endif // ! LINUX_
#endif // MAC_OR_LINUX_
            
#if MAC_OR_LINUX_
            res = strerror_r(actErrno, errBuff, sizeof(errBuff));
#else // ! MAC_OR_LINUX_
            strerror_s(errBuff, sizeof(errBuff), actErrno);
#endif // ! MAC_OR_LINUX_
            cerr << "select() returned " << result << " errno " << actErrno << " " << errBuff <<
                    endl;
            if (EINTR != actErrno)
            {
                break;
            }
            
        }
    }
    OD_LOG_OBJEXIT(); //####
} // NameServerReportingThread::run

DEFINE_THREADINIT_(NameServerReportingThread)
{
    OD_LOG_OBJENTER(); //####
    yarp::os::Contact   nsContact = yarp::os::Network::getNameServerContact();
    YarpString          serverAddress = nsContact.getHost();
    int                 serverPort = nsContact.getPort();
    const char *        serverString = NULL;
    static const char * regType = MpM_MDNS_NAMESERVER_REPORT_;
    const uint16_t      maxTXTSize = 256;
    char                txtBuffer[maxTXTSize];
    TXTRecordRef        txtRecord;
    
    if (nsContact.isValid())
    {
        if (serverAddress != SELF_ADDRESS_IPADDR_)
        {
            serverString = serverAddress.c_str();
        }
    }
    TXTRecordCreate(&txtRecord, sizeof(txtBuffer), txtBuffer);
    TXTRecordSetValue(&txtRecord, MpM_MDNS_NAMESERVER_KEY_,
                      sizeof(MpM_MDNS_NAMESERVER_VERSION_) - 1, MpM_MDNS_NAMESERVER_VERSION_);
    DNSServiceErrorType err = DNSServiceRegister(&_serviceRef, kDNSServiceFlagsNoAutoRename, 0,
                                                 NULL /* name */, regType, NULL /* domain */,
                                                 serverString /* host */, htons(serverPort),
                                                 TXTRecordGetLength(&txtRecord),
                                                 TXTRecordGetBytesPtr(&txtRecord),
                                                 registrationCallback, NULL);
    bool                result = (kDNSServiceErr_NoError == err);

    TXTRecordDeallocate(&txtRecord);
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // NameServerReportingThread::threadInit

DEFINE_THREADRELEASE_(NameServerReportingThread)
{
    OD_LOG_OBJENTER(); //####
    DNSServiceRefDeallocate(_serviceRef);
    OD_LOG_OBJEXIT(); //####
} // NameServerReportingThread::threadRelease

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
