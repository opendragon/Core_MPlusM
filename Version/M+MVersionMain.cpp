//--------------------------------------------------------------------------------------------------
//
//  File:       M+MVersionMain.cpp
//
//  Project:    M+M
//
//  Contains:   A utility application to report the version numbers of M+M and its dependencies.
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
//  Created:    2014-04-16
//
//--------------------------------------------------------------------------------------------------

#include <mpm/M+MCommon.h>
#include <mpm/M+MUtilities.h>

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
#include <ace/Version.h>
#include <yarp/conf/version.h>
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)
		
#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief A utility application to report the version numbers of M+M and its dependencies. */

/*! @dir Version
 @brief The set of files that implement the Version application. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using std::cout;
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

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

/*! @brief The entry point for reporting the version numbers.
 
 Standard output will receive a list of the version numbers.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used with the application.
 @returns @c 0 on a successful test and @c 1 on failure. */
int main(int      argc,
         char * * argv)
{
    OutputFlavour         flavour;
    yarp::os::ConstString aceVersionString;
    yarp::os::ConstString mpmVersionString;
    yarp::os::ConstString yarpVersionString;
    
    if (Utilities::ProcessStandardUtilitiesOptions(argc, argv, "", "", 2014,
                                                   STANDARD_COPYRIGHT_NAME, flavour))
    {
        switch (flavour)
        {
            case kOutputFlavourTabs :
                aceVersionString = SanitizeString(ACE_VERSION, true);
                mpmVersionString = SanitizeString(MpM_VERSION, true);
                yarpVersionString = SanitizeString(YARP_VERSION_STRING, true);
                cout << mpmVersionString.c_str() << "\t" << yarpVersionString.c_str() << "\t" <<
                        aceVersionString.c_str() << endl;
                break;
                
            case kOutputFlavourJSON :
                aceVersionString = SanitizeString(ACE_VERSION);
                mpmVersionString = SanitizeString(MpM_VERSION);
                yarpVersionString = SanitizeString(YARP_VERSION_STRING);
                cout << T_("{ " CHAR_DOUBLEQUOTE "M+M" CHAR_DOUBLEQUOTE ": " CHAR_DOUBLEQUOTE) <<
                        mpmVersionString.c_str() << T_(CHAR_DOUBLEQUOTE ", " CHAR_DOUBLEQUOTE "YARP"
                                                       CHAR_DOUBLEQUOTE ": " CHAR_DOUBLEQUOTE) <<
                        yarpVersionString.c_str() << T_(CHAR_DOUBLEQUOTE ", " CHAR_DOUBLEQUOTE "ACE"
                                                        CHAR_DOUBLEQUOTE ": " CHAR_DOUBLEQUOTE) <<
                        aceVersionString.c_str() << T_(CHAR_DOUBLEQUOTE " }") << endl;
                break;
                
            case kOutputFlavourNormal :
                aceVersionString = SanitizeString(ACE_VERSION, true);
                mpmVersionString = SanitizeString(MpM_VERSION, true);
                yarpVersionString = SanitizeString(YARP_VERSION_STRING, true);
                cout << "Movement And Meaning Version: " << mpmVersionString.c_str() <<
                        ", YARP Version: " << yarpVersionString.c_str() << ", ACE Version: " <<
                aceVersionString.c_str() << endl;
                break;
                
            default :
                break;
                
        }
    }
    return 0;
} // main
