//--------------------------------------------------------------------------------------
//
//  File:       YPPCommon.cpp
//
//  Project:    YarpPlusPlus
//
//  Contains:   The definitions for common functions.
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
//  Created:    2014-03-19
//
//--------------------------------------------------------------------------------------

#include "YPPCommon.h"
//#define ENABLE_OD_SYSLOG /* */
#include "ODSyslog.h"
#include <ace/Version.h>
#include <cmath>
#include <cstdlib>
#include <ctime>
#include <iostream>
#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wc++11-extensions"
# pragma clang diagnostic ignored "-Wdocumentation"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# pragma clang diagnostic ignored "-Wpadded"
# pragma clang diagnostic ignored "-Wshadow"
# pragma clang diagnostic ignored "-Wunused-parameter"
# pragma clang diagnostic ignored "-Wweak-vtables"
#endif // defined(__APPLE__)
#include <yarp/conf/version.h>
#include <yarp/os/Random.h>
#include <yarp/os/Time.h>
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace YarpPlusPlus;
using std::cerr;
using std::cout;
using std::endl;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

#define CHATTY_START_ /* Report the version numbers. */

/*! @brief The maximum integer that we wish to use for generated random values. */
static const int kMaxRandom = 123456789;

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

/*! @brief Returns a printable string, even for null strings.
 @param aString The string to be checked.
 @returns The input string, if non-@c NULL, or a fixed string if it is @c NULL. */
static const char * nullOrString(const char * aString)
{
    const char * result;
    
    if (aString)
    {
        result = aString;
    }
    else
    {
        result = "<>";
    }
    return result;
} // nullOrString

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

void YarpPlusPlus::DumpContact(const char *              tag,
                               const yarp::os::Contact & aContact)
{
    OD_SYSLOG_S4("tag = ", tag, "contact.name = ", aContact.getName().c_str(),//####
                 "contact.host = ", aContact.getHost().c_str(), "contact.carrier = ",//####
                 aContact.getCarrier().c_str());//####
    OD_SYSLOG_LL1("contact.port = ", aContact.getPort());//####
    OD_SYSLOG_S1("contact.toString = ", aContact.toString().c_str());//####
    OD_SYSLOG_B1("contact.isValid = ", aContact.isValid());//####
    cout << "tag = '" << nullOrString(tag) << "', contact.name = '" << nullOrString(aContact.getName().c_str()) <<
            "'" << endl;
    cout << "contact.host = '" << nullOrString(aContact.getHost().c_str()) << "', contact.carrier = '" <<
            nullOrString(aContact.getCarrier().c_str()) << "'" << endl;
    cout << "contact.port = " << aContact.getPort() << endl;
    cout << "contact.toString = '" << nullOrString(aContact.toString().c_str()) << "'" << endl;
    cout << "contact.isValid = " << (aContact.isValid() ? "true" : "false") << endl;
    cout.flush();
} // DumpContact

yarp::os::ConstString YarpPlusPlus::GetRandomPortName(const char * portRoot)
{
    OD_SYSLOG_ENTER();//####
    OD_SYSLOG_S1("portRoot = ", portRoot);//####
    yarp::os::ConstString result;

    try
    {
        const char * stringToUse;
        size_t       buffLen;
        
        if (portRoot)
        {
            stringToUse = portRoot;
            buffLen = strlen(portRoot);
        }
        else
        {
            stringToUse = "_";
            buffLen = 1;
        }
        buffLen += 32; // allow for a big number...
        char * buff = new char[buffLen];
        int    randNumb = yarp::os::Random::uniform(0, kMaxRandom);
        
        snprintf(buff, buffLen, "/%s%x", stringToUse, randNumb);
        result = buff;
        delete[] buff;
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    OD_SYSLOG_EXIT_S(result.c_str());//####
    return result;
} // Endpoint::GetRandomPortName

void YarpPlusPlus::Initialize(void)
{
    OD_SYSLOG_ENTER();//####
    try
    {
        double intPart;
        double now = yarp::os::Time::now();
        double fraction = modf(now, &intPart);
        int    seed = static_cast<int>(ceil(fraction * kMaxRandom));
        
#if defined(CHATTY_START_)
        cerr << "YARP++ Version " << YPP_VERSION << ", YARP Version " << YARP_VERSION_STRING << ", ACE Version = " <<
                ACE_VERSION << endl;
#endif // defined(CHATTY_START_)
        OD_SYSLOG_D2("time = ", now, "fraction = ", fraction);//####
        OD_SYSLOG_LL1("seed = ", seed);//####
        yarp::os::Random::seed(seed);
    }
    catch (...)
    {
        OD_SYSLOG("Exception caught");//####
        throw;
    }
    OD_SYSLOG_EXIT();//####
} // YarpPlusPlus::Initialized
