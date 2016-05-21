//--------------------------------------------------------------------------------------------------
//
//  File:       m+m/m+mCommon.cpp
//
//  Project:    m+m
//
//  Contains:   The function and variable declarations for common entities for m+m clients and
//              services.
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

#include "m+mCommon.hpp"

#include <m+m/m+mBailOut.hpp>
#include <m+m/m+mRequests.hpp>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#include <cmath>
#include <cstdio>
#include <ctime>
#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wc++11-extensions"
# pragma clang diagnostic ignored "-Wdeprecated-declarations"
# pragma clang diagnostic ignored "-Wdocumentation"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# pragma clang diagnostic ignored "-Wextern-c-compat"
# pragma clang diagnostic ignored "-Wpadded"
# pragma clang diagnostic ignored "-Wshadow"
# pragma clang diagnostic ignored "-Wunused-parameter"
# pragma clang diagnostic ignored "-Wweak-vtables"
#endif // defined(__APPLE__)
#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4267)
# pragma warning(disable: 4996)
#endif // ! MAC_OR_LINUX_
#include <ace/Version.h>
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if (! MAC_OR_LINUX_)
# include <Windows.h>
#endif //! MAC_OR_LINUX_

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The function and variable definitions for common entities for m+m clients and services. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using std::endl;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

/*! @brief The maximum integer that we wish to use for generated random values. */
static const int kMaxRandom = 123456789;

/*! @brief @c true if the executable is running or ready-to-run and @c false otherwise. */
static bool lKeepRunning = false;

#if MAC_OR_LINUX_
/*! @brief The logger to use for reporting problems. */
static yarp::os::impl::Logger * lLogger = NULL;
#endif // MAC_OR_LINUX_

#if defined(__APPLE__)
# pragma mark Global constants and variables
#endif // defined(__APPLE__)

const char MplusM::kEscapeChar = '\\';

// Note that this MUST be a single-character string!!!
#if MAC_OR_LINUX_
const YarpString MplusM::kDirectorySeparator = "/";
#else // ! MAC_OR_LINUX_
const YarpString MplusM::kDirectorySeparator = "\\";
#endif // ! MAC_OR_LINUX_

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if MAC_OR_LINUX_
/*! @brief The signal handler to catch requests to stop the service.
 @param[in] signal The signal being handled. */
static void
localCatcher(int signal)
{
    ODL_ENTER(); //####
    ODL_LL1("signal = ", signal); //####
    if (lLogger)
    {
        std::stringstream buff;
        YarpString        message("Exiting due to signal ");

        buff << signal;
        message += buff.str();
        message += " = ";
        message += NameOfSignal(signal);
# if USE_YARP_FATAL_NOT_FAIL_
        lLogger->error("%s", message.c_str());
# else // ! USE_YARP_FATAL_NOT_FAIL_
        lLogger->error(message.c_str());
# endif // ! USE_YARP_FATAL_NOT_FAIL_
    }
    ODL_EXIT_EXIT(1); //####
    yarp::os::exit(1);
} // localCatcher
#endif // MAC_OR_LINUX_

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

void
Common::DumpContactToLog(const char *              tag,
                         const yarp::os::Contact & aContact)
{
#if MAC_OR_LINUX_
    if (lLogger)
    {
        std::stringstream buff;
        YarpString        message("tag = ");

        buff << aContact.getPort();
        message += tag;
# if USE_YARP_FATAL_NOT_FAIL_
        lLogger->info("%s", message.c_str());
# else // ! USE_YARP_FATAL_NOT_FAIL_
        lLogger->info(message.c_str());
# endif // ! USE_YARP_FATAL_NOT_FAIL_
        message = "contact.carrier = ";
        message += aContact.getCarrier();
# if USE_YARP_FATAL_NOT_FAIL_
        lLogger->info("%s", message.c_str());
# else // ! USE_YARP_FATAL_NOT_FAIL_
        lLogger->info(message.c_str());
# endif // ! USE_YARP_FATAL_NOT_FAIL_
        message = "contact.host = ";
        message += aContact.getHost();
# if USE_YARP_FATAL_NOT_FAIL_
        lLogger->info("%s", message.c_str());
# else // ! USE_YARP_FATAL_NOT_FAIL_
        lLogger->info(message.c_str());
# endif // ! USE_YARP_FATAL_NOT_FAIL_
        message = "contact.isValid = ";
        message += (aContact.isValid() ? "true" : "false");
# if USE_YARP_FATAL_NOT_FAIL_
        lLogger->info("%s", message.c_str());
# else // ! USE_YARP_FATAL_NOT_FAIL_
        lLogger->info(message.c_str());
# endif // ! USE_YARP_FATAL_NOT_FAIL_
        message = "contact.name = ";
        message += aContact.getName();
# if USE_YARP_FATAL_NOT_FAIL_
        lLogger->info("%s", message.c_str());
# else // ! USE_YARP_FATAL_NOT_FAIL_
        lLogger->info(message.c_str());
# endif // ! USE_YARP_FATAL_NOT_FAIL_
        message = "contact.port = ";
        message += buff.str();
# if USE_YARP_FATAL_NOT_FAIL_
        lLogger->info("%s", message.c_str());
# else // ! USE_YARP_FATAL_NOT_FAIL_
        lLogger->info(message.c_str());
# endif // ! USE_YARP_FATAL_NOT_FAIL_
        message = "contact.toString = ";
        message += aContact.toString();
# if USE_YARP_FATAL_NOT_FAIL_
        lLogger->info("%s", message.c_str());
# else // ! USE_YARP_FATAL_NOT_FAIL_
        lLogger->info(message.c_str());
# endif // ! USE_YARP_FATAL_NOT_FAIL_
    }
#endif // MAC_OR_LINUX_
} // Common::DumpContactToLog

#if MAC_OR_LINUX_
yarp::os::impl::Logger &
Common::GetLogger(void)
{
    ODL_ENTER(); //####
    ODL_EXIT_P(lLogger);
    return *lLogger;
} // Common::GetLogger
#endif // MAC_OR_LINUX_

YarpString
Common::GetRandomChannelName(const char * channelRoot)
{
    ODL_ENTER(); //####
    ODL_S1("channelRoot = ", channelRoot); //####
    YarpString result;

    try
    {
        bool              hasLeadingSlash = false;
        const char *      stringToUse;
        int               randNumb = static_cast<int>(yarp::os::Random::uniform() * kMaxRandom);
        std::stringstream buff;

        if (channelRoot)
        {
            stringToUse = channelRoot;
            if ('/' == *channelRoot)
            {
                hasLeadingSlash = true;
            }
        }
        else
        {
            stringToUse = "_";
        }
        if (! hasLeadingSlash)
        {
            buff << "/" ;
        }
        buff << stringToUse << std::hex << randNumb;
        result = buff.str();
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_EXIT_s(result); //####
    return result;
} // Common::GetRandomChannelName

YarpString
Common::GetRandomChannelName(const YarpString & channelRoot)
{
    return GetRandomChannelName(channelRoot.c_str());
} // Common::GetRandomChannelName

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
void
Common::Initialize(const YarpString & progName)
{
#if ((! defined(MpM_ChattyStart)) && (! defined(OD_ENABLE_LOGGING_)))
# if MAC_OR_LINUX_
#  pragma unused(progName)
# endif // MAC_OR_LINUX_
#endif // (! defined(MpM_ChattyStart)) && (! defined(OD_ENABLE_LOGGING_))
    ODL_ENTER(); //####
    ODL_S1s("progName = ", progName); //####
    try
    {
#if (defined(OD_ENABLE_LOGGING_) && defined(MpM_LogIncludesYarpTrace))
        yarp::os::Network::setVerbosity(1);
#else // ! (defined(OD_ENABLE_LOGGING_) && defined(MpM_LogIncludesYarpTrace))
        yarp::os::Network::setVerbosity(-1);
#endif // ! (defined(OD_ENABLE_LOGGING_) && defined(MpM_LogIncludesYarpTrace))
        double intPart;
        double now = yarp::os::Time::now();
        double fraction = modf(now, &intPart);
        int    seed = static_cast<int>(ceil(fraction * kMaxRandom));

#if defined(MpM_ChattyStart)
# if MAC_OR_LINUX_
        if (lLogger)
        {
            YarpString message("Program ");

            message += progName;
#  if USE_YARP_FATAL_NOT_FAIL_
            lLogger->info("%s", message.c_str());
#  else // ! USE_YARP_FATAL_NOT_FAIL_
            lLogger->info(message.c_str());
#  endif // ! USE_YARP_FATAL_NOT_FAIL_
            lLogger->info("m+m Version: " MpM_VERSION_ ", YARP Version: " YARP_VERSION_STRING
                          ", ACE Version: " ACE_VERSION);
        }
# endif // MAC_OR_LINUX_
#endif // defined(MpM_ChattyStart)
        ODL_D2("time = ", now, "fraction = ", fraction); //####
        ODL_LL1("seed = ", seed); //####
        yarp::os::Random::seed(seed);
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_EXIT(); //####
} // Common::Initialize
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

void
Common::SetSignalHandlers(yarp::os::YarpSignalHandler theHandler)
{
    ODL_ENTER(); //####
#if MAC_OR_LINUX_
    sigset_t         blocking;
    struct sigaction act;
#endif // MAC_OR_LINUX_

#if MAC_OR_LINUX_
    act.sa_handler = theHandler;
    sigemptyset(&act.sa_mask);
    act.sa_flags = 0;
# if (defined(SIGABRT) && (SIGABRT != STANDARD_SIGNAL_TO_USE_))
    sigaction(SIGABRT, &act, NULL);
# endif // defined(SIGABRT) && (SIGABRT != STANDARD_SIGNAL_TO_USE_)
# if (defined(SIGHUP) && (SIGHUP != STANDARD_SIGNAL_TO_USE_))
    sigaction(SIGHUP, &act, NULL);
# endif // defined(SIGHUP) && (SIGABRT != STANDARD_SIGNAL_TO_USE_)
# if (defined(SIGINT) && (SIGINT != STANDARD_SIGNAL_TO_USE_))
    sigaction(SIGINT, &act, NULL);
# endif // defined(SIGINT) && (SIGABRT != STANDARD_SIGNAL_TO_USE_)
# if (defined(SIGQUIT) && (SIGQUIT != STANDARD_SIGNAL_TO_USE_))
    sigaction(SIGQUIT, &act, NULL);
# endif // defined(SIGQUIT) && (SIGABRT != STANDARD_SIGNAL_TO_USE_)
# if (defined(SIGUSR1) && (SIGUSR1 != STANDARD_SIGNAL_TO_USE_))
    sigaction(SIGUSR1, &act, NULL);
# endif // defined(SIGUSR1) && (SIGABRT != STANDARD_SIGNAL_TO_USE_)
# if (defined(SIGUSR2) && (SIGUSR2 != STANDARD_SIGNAL_TO_USE_))
    sigaction(SIGUSR2, &act, NULL);
# endif // defined(SIGUSR2) && (SIGABRT != STANDARD_SIGNAL_TO_USE_)
    sigemptyset(&blocking);
    sigaddset(&blocking, STANDARD_SIGNAL_TO_USE_);
    pthread_sigmask(SIG_BLOCK, &blocking, NULL);
#else // ! MAC_OR_LINUX_
# if (defined(SIGABRT) && (SIGABRT != STANDARD_SIGNAL_TO_USE_))
    //yarp::os::signal(SIGABRT, theHandler);
    signal(SIGABRT, theHandler); //windows doesn't like the yarp signals for some reason
# endif // defined(SIGABRT) && (SIGABRT != STANDARD_SIGNAL_TO_USE_)
# if (defined(SIGHUP) && (SIGHUP != STANDARD_SIGNAL_TO_USE_))
    yarp::os::signal(SIGHUP, theHandler);
# endif // defined(SIGHUP) && (SIGABRT != STANDARD_SIGNAL_TO_USE_)
# if (defined(SIGINT) && (SIGINT != STANDARD_SIGNAL_TO_USE_))
    //yarp::os::signal(SIGINT, theHandler);
    signal(SIGINT, theHandler); //windows doesn't like the yarp signals for some reason
# endif // defined(SIGINT) && (SIGABRT != STANDARD_SIGNAL_TO_USE_)
# if (defined(SIGQUIT) && (SIGQUIT != STANDARD_SIGNAL_TO_USE_))
    yarp::os::signal(SIGQUIT, theHandler);
# endif // defined(SIGQUIT) && (SIGABRT != STANDARD_SIGNAL_TO_USE_)
# if (defined(SIGUSR1) && (SIGUSR1 != STANDARD_SIGNAL_TO_USE_))
    yarp::os::signal(SIGUSR1, theHandler);
# endif // defined(SIGUSR1) && (SIGABRT != STANDARD_SIGNAL_TO_USE_)
# if (defined(SIGUSR2) && (SIGUSR2 != STANDARD_SIGNAL_TO_USE_))
    yarp::os::signal(SIGUSR2, theHandler);
# endif // defined(SIGUSR2) && (SIGABRT != STANDARD_SIGNAL_TO_USE_)
    yarp::os::signal(SIGTERM, theHandler);
#endif // ! MAC_OR_LINUX_
    ODL_EXIT(); //####
} // Common::SetSignalHandlers

void
Common::SetUpCatcher(void)
{
    ODL_ENTER(); //####
#if MAC_OR_LINUX_
    sigset_t         unblocking;
    struct sigaction act;
#endif // MAC_OR_LINUX_

#if MAC_OR_LINUX_
    sigemptyset(&unblocking);
    sigaddset(&unblocking, STANDARD_SIGNAL_TO_USE_);
    pthread_sigmask(SIG_UNBLOCK, &unblocking, NULL);
    act.sa_handler = localCatcher;
    sigemptyset(&act.sa_mask);
    act.sa_flags = 0;
    sigaction(STANDARD_SIGNAL_TO_USE_, &act, NULL);
#else // ! MAC_OR_LINUX_
#endif // ! MAC_OR_LINUX_
    ODL_EXIT(); //####
} // Common::SetUpCatcher

#if MAC_OR_LINUX_
void
Common::SetUpLogger(const YarpString & progName)
{
    ODL_ENTER(); //####
    lLogger = new yarp::os::impl::Logger(progName.c_str());
    if (lLogger)
    {
        lLogger->setVerbosity(1);
    }
    ODL_EXIT(); //####
} // Common::SetUpLogger
#endif // MAC_OR_LINUX_

void
Common::ShutDownCatcher(void)
{
    ODL_ENTER(); //####
#if MAC_OR_LINUX_
    sigset_t         blocking;
    struct sigaction act;
#endif // MAC_OR_LINUX_

#if MAC_OR_LINUX_
    sigemptyset(&blocking);
    sigaddset(&blocking, STANDARD_SIGNAL_TO_USE_);
    pthread_sigmask(SIG_BLOCK, &blocking, NULL);
    act.sa_handler = SIG_DFL;
    sigemptyset(&act.sa_mask);
    act.sa_flags = 0;
    sigaction(STANDARD_SIGNAL_TO_USE_, &act, NULL);
#else // ! MAC_OR_LINUX_
#endif // ! MAC_OR_LINUX_
    ODL_EXIT(); //####
} // Common::ShutDownCatcher

void
Common::Stall(void)
{
    for ( ; ; )
    {
        ConsumeSomeTime();
    }
} // Common::Stall

bool
MplusM::CanReadFromStandardInput(void)
{
    ODL_ENTER(); //####
#if MAC_OR_LINUX_
    pid_t fg = tcgetpgrp(STDIN_FILENO);
#else // ! MAC_OR_LINUX_
    HWND  wind = GetConsoleWindow();
#endif // ! MAC_OR_LINUX_
    bool  result = false;

#if MAC_OR_LINUX_
    if (-1 == fg)
    {
        // Piped
        result = true;
    }
    else if (getpgrp() == fg)
    {
        // Foreground
        result = true;
    }
    else
    {
        // Background
        result = false;
    }
#else // ! MAC_OR_LINUX_
    result = (NULL != wind);
#endif // ! MAC_OR_LINUX_
    ODL_EXIT_B(result); //####
    return result;
} // MplusM::CanReadFromStandardInput

void
MplusM::ConsumeSomeTime(const double factor)
{
    ODL_ENTER(); //####
    yarp::os::Time::delay(ONE_SECOND_DELAY_ / factor);
    yarp::os::Time::yield();
    ODL_EXIT(); //####
} // MplusM::ConsumeSomeTime

void
MplusM::IdleUntilNotRunning(void)
{
    ODL_ENTER(); //####
    for ( ; IsRunning(); )
    {
        ConsumeSomeTime();
    }
    ODL_EXIT(); //####
} // MplusM::IdleUntilNotRunning

bool
MplusM::IsRunning(void)
{
    ODL_ENTER(); //####
    ODL_EXIT_B(lKeepRunning); //####
    return lKeepRunning;
} // MplusM::IsRunning

bool
MplusM::ListIsReallyDictionary(const yarp::os::Bottle & aList,
                               yarp::os::Property &     aDictionary)
{
    ODL_ENTER(); //####
    ODL_P2("aList = ", &aList, "aDictionary = ", &aDictionary); //####
    int  mm = aList.size();
    bool isDictionary = (0 < mm);

    aDictionary.clear();
    for (int ii = 0; isDictionary && (mm > ii); ++ii)
    {
        yarp::os::Value anEntry(aList.get(ii));

        if (anEntry.isList())
        {
            yarp::os::Bottle * entryAsList = anEntry.asList();

            if (entryAsList)
            {
                if (2 == entryAsList->size())
                {
                    yarp::os::Value key(entryAsList->get(0));
                    yarp::os::Value data(entryAsList->get(1));

                    if (key.isString())
                    {
                        YarpString keyAsString(key.toString());

                        if (aDictionary.check(keyAsString))
                        {
                            isDictionary = false;
                        }
                        else
                        {
                            aDictionary.put(keyAsString, data);
                        }
                    }
                    else
                    {
                        isDictionary = false;
                    }
                }
                else
                {
                    isDictionary = false;
                }
            }
        }
        else
        {
            isDictionary = false;
        }
    }
    ODL_EXIT_B(isDictionary); //####
    return isDictionary;
} // MplusM::ListIsReallyDictionary

const char *
MplusM::NameOfSignal(const int theSignal)
{
    const char * result;

#if MAC_OR_LINUX_
    switch (theSignal)
    {
        case SIGHUP :
            result = "SIGHUP[hangup]";
            break;

        case SIGINT :
            result = "SIGINT[interrupt]";
            break;

        case SIGQUIT :
            result = "SIGQUIT[quit]";
            break;

        case SIGILL :
            result = "SIGILL[illegal instruction]";
            break;

        case SIGTRAP :
            result = "SIGTRAP[trace trap]";
            break;

        case SIGABRT :
            result = "SIGABRT[abort()]";
            break;

# if (defined(_POSIX_C_SOURCE) && (! defined(_DARWIN_C_SOURCE)))
        case SIGPOLL :
            result = "SIGPOLL[pollable evebt]";
            break;
# else // (! defined(_POSIX_C_SOURCE)) || defined(_DARWIN_C_SOURCE)
        case SIGEMT :
            result = "SIGEMT[EMT instruction]";
            break;
# endif // (! defined(_POSIX_C_SOURCE)) || defined(_DARWIN_C_SOURCE)

        case SIGFPE :
            result = "SIGFPE[floating point exception]";
            break;

        case SIGKILL :
            result = "SIGKILL[kill]";
            break;

        case SIGBUS :
            result = "SIGBUS[bus error]";
            break;

        case SIGSEGV :
            result = "SIGSEGV[segmentation violation]";
            break;

        case SIGSYS :
            result = "SIGSYS[bad argument to system call]";
            break;

        case SIGPIPE :
            result = "SIGPIPE[write on a pipe with no one to read it]";
            break;

        case SIGALRM :
            result = "SIGALRM[alarm clock]";
            break;

        case SIGTERM :
            result = "SIGTERM[software termination signal from kill]";
            break;

        case SIGURG :
            result = "SIGURG[urgent condition on IO channel]";
            break;

        case SIGSTOP :
            result = "SIGSTOP[sendable stop signal not from tty]";
            break;

        case SIGTSTP :
            result = "SIGTSTP[stop signal from tty]";
            break;

        case SIGCONT :
            result = "SIGCONT[continue a stopped process]";
            break;

        case SIGCHLD :
            result = "SIGCHLD[to parent on child stop or exit]";
            break;

        case SIGTTIN :
            result = "SIGTTIN[to readers pgrp upon background tty read]";
            break;

        case SIGTTOU :
            result = "SIGTTOU[like TTIN for output if (tp->t_local&LTOSTOP)]";
            break;

# if ((! defined(_POSIX_C_SOURCE)) || defined(_DARWIN_C_SOURCE))
        case SIGIO :
            result = "SIGIO[input/output possible signal]";
            break;
# endif // (! defined(_POSIX_C_SOURCE)) || defined(_DARWIN_C_SOURCE)

        case SIGXCPU :
            result = "SIGXCPU[exceeded CPU time limit]";
            break;

        case SIGXFSZ :
            result = "SIGXFSZ[exceeded file size limit]";
            break;

        case SIGVTALRM :
            result = "SIGVTALRM[virtual time alarm]";
            break;

        case SIGPROF :
            result = "SIGPROF[profiling time alarm]";
            break;

# if ((! defined(_POSIX_C_SOURCE)) || defined(_DARWIN_C_SOURCE))
        case SIGWINCH :
            result = "SIGWINCH[window size changes]";
            break;
# endif // (! defined(_POSIX_C_SOURCE)) || defined(_DARWIN_C_SOURCE)

# if ((! defined(_POSIX_C_SOURCE)) || defined(_DARWIN_C_SOURCE))
        case SIGINFO :
            result = "SIGINFO[information request]";
            break;
# endif // (! defined(_POSIX_C_SOURCE)) || defined(_DARWIN_C_SOURCE)

        case SIGUSR1 :
            result = "SIGUSR1[user defined signal 1]";
            break;

        case SIGUSR2 :
            result = "SIGUSR2[user defined signal 2]";
            break;

        default :
            result = "unknown";
            break;

    }
#else // ! MAC_OR_LINUX_
    switch (theSignal)
    {
        case SIGINT :
            result = "SIGINT[interrupt]";
            break;

        case SIGABRT :
            result = "SIGABRT[abort()]";
            break;

        default :
            result = "unknown";
            break;

    }
#endif // ! MAC_OR_LINUX_
    return result;
} // MplusM::NameOfSignal

void
MplusM::OutputDescription(std::ostream &     outStream,
                          const char *       heading,
                          const YarpString & description)
{
    size_t     descriptionLength = description.length();
    size_t     indentSize = strlen(heading);
    size_t     pieceStart = 0;
    YarpString blanks(indentSize, ' ');
    YarpString indent(heading);

    for (size_t ii = 0; ii < descriptionLength; ++ii)
    {
        if ('\n' == description[ii])
        {
            YarpString piece(description.substr(pieceStart, ii - pieceStart));

            outStream << indent << piece.c_str() << endl;
            pieceStart = ii + 1;
            indent = blanks;
        }
    }
    YarpString piece(description.substr(pieceStart, descriptionLength - pieceStart));

    outStream << indent << piece.c_str() << endl;
} // MplusM::OutputDescription

YarpString
MplusM::SanitizeString(const YarpString & inString,
                       const bool         allowDoubleQuotes)
{
    ODL_ENTER(); //####
    ODL_S1s("channelRoot = ", inString); //####
    ODL_B1("allowDoubleQuotes = ", allowDoubleQuotes); //####
    YarpString outString;

    try
    {
        for (size_t ii = 0, mm = inString.length(); mm > ii; )
        {
            char cc = inString[ii++];

            switch (cc)
            {
                case '\t' :
                    outString += kEscapeChar;
                    cc = 't';
                    break;

                case '\n' :
                    outString += kEscapeChar;
                    cc = 'n';
                    break;

                case '\\' :
                    outString += kEscapeChar;
                    break;

                case '"' :
                    if (! allowDoubleQuotes)
                    {
                        outString += kEscapeChar;
                    }
                    break;

                default :
                    break;

            }
            outString += cc;
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_EXIT_s(outString); //####
    return outString;
} // MplusM::SanitizeString

void
MplusM::StartRunning(void)
{
    ODL_ENTER(); //####
    lKeepRunning = true;
    ODL_EXIT(); //####
} // MplusM::StartRunning

void
MplusM::StopRunning(void)
{
    ODL_ENTER(); //####
    lKeepRunning = false;
    ODL_EXIT(); //####
} // MplusM::StopRunning

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
void
MplusM::SignalRunningStop(const int signal)
{
#if (! defined(OD_ENABLE_LOGGING_))
# if MAC_OR_LINUX_
#  pragma unused(signal)
# endif // MAC_OR_LINUX_
#endif // ! defined(OD_ENABLE_LOGGING_)
    ODL_ENTER(); //####
    ODL_LL1("signal = ", signal); //####
    StopRunning();
    ODL_EXIT(); //####
} // MplusM::SignalRunningStop
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_
