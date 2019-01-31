//--------------------------------------------------------------------------------------------------
//
//  File:       m+mCommonVisuals.cpp
//
//  Project:    m+m
//
//  Contains:   The definitions of common data types for the common visualization code.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2016 by OpenDragon.
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
//  Created:    2016-06-05
//
//--------------------------------------------------------------------------------------------------

#include "m+mCommonVisuals.hpp"

//#include <odlEnable.h>
#include <odlInclude.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)

#if (! MAC_OR_LINUX_)
# include <Windows.h>
#endif //! MAC_OR_LINUX_

/*! @file

 @brief The definitions of common data types for the common visualization code. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace CommonVisuals;
using namespace MplusM;
using namespace std;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

/*! @brief The command manager object used to dispatch command events. */
static ScopedPointer<ApplicationCommandManager> lApplicationCommandManager;

/*! @brief @c true if an exit has been requested and @c false otherwise. */
static bool lExitRequested = false;

///*! @brief The name to be used for logging and display purposes. */
//static const char * kApplicationName = "m+mLeapDisplayOutputService";

/*! @brief The number of milliseconds to sleep while waiting for a process to finish. */
static const int kProcessSleepSlice = 5;

///*! @brief The number of milliseconds before a thread is force-killed. */
//static const int kThreadKillTime = 3000;

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

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

void
CommonVisuals::CalculateTextArea(Point<int> &   dimensions,
                                 const Font &   aFont,
                                 const String & aString)
{
    ODL_ENTER(); //####
    ODL_P2("dimensions = ", &dimensions, "aFont = ", &aFont); //####
    ODL_S1s("aString = ", aString.toStdString()); //####
    float       maxWidth = 0;
    StringArray asLines;

    asLines.addLines(aString);
    int numRows = asLines.size();

    for (int ii = 0; ii < numRows; ++ii)
    {
        const String & aRow = asLines[ii];
        float          aWidth = aFont.getStringWidthFloat(aRow);

        if (maxWidth < aWidth)
        {
            maxWidth = aWidth;
        }
    }
    dimensions = Point<int>(static_cast<int>(maxWidth + 0.5),
                            static_cast<int>((numRows * aFont.getHeight()) + 0.5));
    ODL_EXIT(); //####
} // CommonVisuals::CalculateTextArea

ApplicationCommandManager &
CommonVisuals::GetApplicationCommandManager(void)
{
    ODL_ENTER(); //####
    if (! lApplicationCommandManager)
    {
        lApplicationCommandManager = new ApplicationCommandManager;
    }
    ODL_EXIT_P(lApplicationCommandManager); //####
    return *lApplicationCommandManager;
} // CommonVisuals::GetApplicationCommandManager

int
CommonVisuals::GetButtonHeight(void)
{
    ODL_ENTER(); //####
    int result = LookAndFeel::getDefaultLookAndFeel().getAlertWindowButtonHeight();

    ODL_EXIT_I(result);
    return result;
} // CommonVisuals::GetButtonHeight

void
CommonVisuals::ReleaseApplicationCommandManager(void)
{
    ODL_ENTER(); //####
    lApplicationCommandManager = NULL;
    ODL_EXIT(); //####
} // CommonVisuals::ReleaseApplicationCommandManager

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
bool
CheckForExit(void * stuff)
{
#if (! defined(ODL_ENABLE_LOGGING_))
# if MAC_OR_LINUX_
#  pragma unused(stuff)
# endif // MAC_OR_LINUX_
#endif // ! defined(ODL_ENABLE_LOGGING_)
    ODL_ENTER(); //####
    ODL_P1("stuff = ", stuff); //####
    ODL_EXIT_B(lExitRequested); //####
    return lExitRequested;
} // CheckForExit
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

bool
LazyLaunchProcess(ChildProcess & aProcess,
                  const int      timeout)
{
    ODL_ENTER(); //####
    ODL_P1("aProcess = ", &aProcess); //####
    ODL_I1("timeout = ", timeout); //####
    bool result = false;

    if (0 < timeout)
    {
        uint32_t       now = Time::getMillisecondCounter();
        const uint32_t timeoutTime = now + static_cast<uint32_t>(timeout);

        for ( ; (! result) && (now < timeoutTime); )
        {
            if (aProcess.isRunning())
            {
                Thread::sleep(kProcessSleepSlice);
                now = Time::getMillisecondCounter();
            }
            else
            {
                result = true;
            }
        }
    }
    else
    {
        for ( ; ! result; )
        {
            if (aProcess.isRunning())
            {
                Thread::sleep(kProcessSleepSlice);
            }
            else
            {
                result = true;
            }
        }
    }
    ODL_EXIT_B(result); //####
    return result;
} // LazyLaunchProcess

void
SetExitRequest(void)
{
    ODL_ENTER(); //####
    lExitRequested = true;
    ODL_EXIT(); //####
} // SetExitRequest

