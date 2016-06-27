//--------------------------------------------------------------------------------------------------
//
//  File:       m+mPlatonicDisplayApplication.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for the application object of the platonic display output
//              service application.
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
//  Created:    2016-06-04
//
//--------------------------------------------------------------------------------------------------

#include "m+mPlatonicDisplayApplication.hpp"
#include "m+mPlatonicDisplayGraphicsPanel.hpp"
#include "m+mPlatonicDisplayOutputService.hpp"
#include "m+mPlatonicServiceThread.hpp"
#include "m+mSettingsWindow.hpp"

#include <m+m/m+mEndpoint.hpp>
#include <m+m/m+mRequests.hpp>

#if (! MAC_OR_LINUX_)
# include <io.h>
#endif // ! MAC_OR_LINUX_

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

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
# pragma warning(disable: 4996)
# pragma warning(disable: 4458)
#endif // ! MAC_OR_LINUX_
#if YARP_SYSTEM_INFO_MOVED_
# include <yarp/os/SystemInfo.h>
#else // ! YARP_SYSTEM_INFO_MOVED_
# include <yarp/os/impl/SystemInfo.h>
#endif // ! YARP_SYSTEM_INFO_MOVED_
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)
#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file

 @brief The class definition for the application object of the platonic display output service
 application. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace CommonVisuals;
using namespace MplusM;
using namespace PlatonicDisplay;
using namespace std;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

/*! @brief The name to be used for logging and display purposes. */
static const char * kApplicationName = "m+mPlatonicDisplayOutputService";

/*! @brief The number of milliseconds before a thread is force-killed. */
static const int kThreadKillTime = 3000;

#if defined(__APPLE__)
# pragma mark Global constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

/*! @brief DO a case-insensitive match.
 @param[in] string1 The first string to compare.
 @param[in] string2 The second string to compare.
 @returns @c true if both strings are identical, ignoring case and @c false if the strings are of
 different length or contain at least one character that is different. */
static bool
caseInsensitiveMatch(const char * string1,
                     const char * string2)
{
    ODL_ENTER(); //####
    ODL_S2("string1 = ", string1, "string2 = ", string2); //####
    bool matched = true;

    if (! string1)
    {
        PlatonicDisplayWindow * mainWindow = PlatonicDisplayApplication::getMainWindow();

        AlertWindow::showMessageBox(AlertWindow::WarningIcon, "Bad first string pointer",
                                    String::empty, String::empty, mainWindow);
        mainWindow->toFront(true);
    }
    else if (! string2)
    {
        PlatonicDisplayWindow * mainWindow = PlatonicDisplayApplication::getMainWindow();

        AlertWindow::showMessageBox(AlertWindow::WarningIcon, "Bad second string pointer",
                                    String::empty, String::empty, mainWindow);
        mainWindow->toFront(true);
    }
    else
    {
        for ( ; matched; ++string1, ++string2)
        {
            if (*string1)
            {
                if (*string2)
                {
                    matched = (toupper(*string1) == toupper(*string2));
                }
                else
                {
                    // First string is longer.
                    matched = false;
                }
            }
            else if (*string2)
            {
                // Second string is longer.
                matched = false;
            }
            else
            {
                // Reached end-of-string on both strings.
                break;
            }

        }
    }
    ODL_EXIT_B(matched); //####
    return matched;
} // caseInsensitiveMatch

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and Destructors
#endif // defined(__APPLE__)

PlatonicDisplayApplication::PlatonicDisplayApplication(void) :
    inherited(), _mainWindow(NULL), _yarp(NULL)
{
#if defined(MpM_ServicesLogToStandardError)
    ODL_INIT(kApplicationName, kODLoggingOptionIncludeProcessID | //####
             kODLoggingOptionIncludeThreadID | kODLoggingOptionWriteToStderr | //####
             kODLoggingOptionEnableThreadSupport); //####
#else // ! defined(MpM_ServicesLogToStandardError)
    ODL_INIT(kApplicationName, kODLoggingOptionIncludeProcessID | //####
             kODLoggingOptionIncludeThreadID | kODLoggingOptionEnableThreadSupport); //####
#endif // ! defined(MpM_ServicesLogToStandardError)
    ODL_ENTER(); //####
    ODL_EXIT_P(this); //####
} // PlatonicDisplayApplication::PlatonicDisplayApplication

PlatonicDisplayApplication::~PlatonicDisplayApplication(void)
{
    ODL_OBJENTER(); //####
    stopAssociatedService();
    ODL_OBJEXIT(); //####
} // PlatonicDisplayApplication::~PlatonicDisplayApplication

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
void
PlatonicDisplayApplication::anotherInstanceStarted(const String & commandLine)
{
#if (! defined(OD_ENABLE_LOGGING_))
# if MAC_OR_LINUX_
#  pragma unused(commandLine)
# endif // MAC_OR_LINUX_
#endif // ! defined(OD_ENABLE_LOGGING_)
    ODL_OBJENTER(); //####
    ODL_S1s("commandLine = ", commandLine.toStdString()); //####
    // When another instance of the app is launched while this one is running, this method is
    // invoked, and the commandLine parameter tells you what the other instance's command-line
    // arguments were.
    ODL_OBJEXIT(); //####
} // PlatonicDisplayApplication::anotherInstanceStarted
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

PlatonicDisplayOutputService *
PlatonicDisplayApplication::configureAndCreateService(void)
{
    ODL_OBJENTER(); //####
    PlatonicDisplayOutputService * result = NULL;
    ApplicationInfo            appInfo;
    int                        tagModifierCount;
    String                     endpointToUse;
    String                     portToUse;
    String                     tagToUse;
    StringArray                argsToUse;
    
    // There are no arguments to the service, so the '_argDescriptions' field need not be filled in.
    // Fill in the remaining fields:
    appInfo._applicationPath = findPathToExecutable(getApplicationName());
    appInfo._criteria = "";
    appInfo._description = PLATONICDISPLAY_SERVICE_DESCRIPTION_;
    appInfo._options = ALL_OPTIONS_STRING_;
    appInfo._kind = kApplicationService;
    appInfo._shortName = getApplicationName();
    ScopedPointer<SettingsWindow> settings(new SettingsWindow(String("Launching the ") +
                                                              appInfo._description, "service",
                                                              appInfo, endpointToUse, tagToUse,
                                                              portToUse, tagModifierCount,
                                                              argsToUse));
    
    if (CommonVisuals::kConfigurationOK == settings->runModalLoop())
    {
        Common::AddressTagModifier  modFlag = Common::kModificationNone;
        Utilities::DescriptorVector argumentList;
        int                         argc = 0;
        char * *                    argv = NULL;
        YarpString                  progName(appInfo._shortName.toStdString());
        YarpString                  tag(tagToUse.toStdString());
        YarpString                  serviceEndpointName(endpointToUse.toStdString());
        YarpString                  servicePortNumber(portToUse.toStdString());
        
        switch (tagModifierCount)
        {
            case 0 :
                modFlag = Common::kModificationNone;
                break;
                
            case 1 :
                modFlag = Common::kModificationBottomByte;
                break;
                
            case 2 :
                modFlag = Common::kModificationBottomTwoBytes;
                break;
                
            case 3 :
                modFlag = Common::kModificationBottomThreeBytes;
                break;
                
            case 4 :
                modFlag = Common::kModificationAllBytes;
                break;
                
            default :
                break;
                
        }
        AdjustEndpointName(DEFAULT_PLATONICDISPLAY_SERVICE_NAME_, modFlag, tag,
                           serviceEndpointName);
        result = new PlatonicDisplayOutputService(argumentList, progName, argc, argv, tag,
                                              serviceEndpointName, servicePortNumber);
    }
    ODL_OBJEXIT_P(result); //####
    return result;
} // PlatonicDisplayApplication::configureAndCreateService

#if 0
void
PlatonicDisplayApplication::configureAssociatedService(void)
{
    ODL_OBJENTER(); //####
    ODL_OBJEXIT(); //####
} // PlatonicDisplayApplication::configureAssociatedService
#endif//0

String
PlatonicDisplayApplication::findPathToExecutable(const String & execName)
{
    ODL_ENTER(); //####
    ODL_S1s("execName = ", execName.toStdString()); //####
    String result;

    if (juce::File::isAbsolutePath(execName))
    {
        bool       doCheck = false;
        juce::File aFile(juce::File::createFileWithoutCheckingPath(execName));

        if (aFile.existsAsFile())
        {
            doCheck = true;
        }
#if (! MAC_OR_LINUX_)
        else
        {
            String temp(aFile.getFullPathName() + ".exe");

            aFile = juce::File::createFileWithoutCheckingPath(temp);
            if (aFile.existsAsFile())
            {
                doCheck = true;
            }
        }
#endif // ! MAC_OR_LINUX_
        if (doCheck)
        {
            String temp(aFile.getFullPathName());

#if MAC_OR_LINUX_
            if (! access(temp.toStdString().c_str(), X_OK))
#else // ! MAC_OR_LINUX_
            if (! _access(temp.toStdString().c_str(), 0))
                // Note that there's no explicit check for execute permission in Windows.
#endif // ! MAC_OR_LINUX_
            {
                // We've found an executable that we can use!
                result = temp;
            }
        }
    }
    else
    {
        String pathVar(getEnvironmentVar("PATH"));

        if (pathVar.isNotEmpty())
        {
            for ( ; pathVar.isNotEmpty(); )
            {
                bool       doCheck = false;
                juce::File aFile;
                String     pathToCheck;
#if MAC_OR_LINUX_
                int        indx = pathVar.indexOfChar(':');
#else // ! MAC_OR_LINUX_
                int        indx = pathVar.indexOfChar(';');
#endif // ! MAC_OR_LINUX_

                if (-1 == indx)
                {
                    pathToCheck = pathVar;
                    pathVar.clear();
                }
                else
                {
                    pathToCheck = pathVar.substring(0, indx);
                    pathVar = pathVar.substring(indx + 1);
                }
                pathToCheck = juce::File::addTrailingSeparator(pathToCheck);
                aFile = juce::File::createFileWithoutCheckingPath(pathToCheck + execName);
                if (aFile.existsAsFile())
                {
                    doCheck = true;
                }
#if (! MAC_OR_LINUX_)
                else
                {
                    String temp(aFile.getFullPathName() + ".exe");

                    aFile = juce::File::createFileWithoutCheckingPath(temp);
                    if (aFile.existsAsFile())
                    {
                        doCheck = true;
                    }
                }
#endif // ! MAC_OR_LINUX_
                if (doCheck)
                {
                    String temp(aFile.getFullPathName());

#if MAC_OR_LINUX_
                    if (! access(temp.toStdString().c_str(), X_OK))
#else // ! MAC_OR_LINUX_
                    if (! _access(temp.toStdString().c_str(), 0))
                        // Note that there's no explicit check for execute permission in Windows.
#endif // ! MAC_OR_LINUX_
                    {
                        // We've found an executable that we can use!
                        result = temp;
                        pathVar = "";
                    }
                }
            }
        }
    }
    ODL_EXIT_s(result.toStdString()); //####
    return result;
} // PlatonicDisplayApplication::findPathToExecutable

PlatonicDisplayApplication *
PlatonicDisplayApplication::getApp(void)
{
    ODL_ENTER(); //####
    PlatonicDisplayApplication * result =
                        static_cast<PlatonicDisplayApplication *>(JUCEApplication::getInstance());

    ODL_EXIT_P(result); //####
    return result;
} // PlatonicDisplayApplication::getApp

const String
PlatonicDisplayApplication::getApplicationName(void)
{
    ODL_OBJENTER(); //####
    ODL_OBJEXIT_S(kApplicationName); //####
    return kApplicationName;
} // LeapDisplayApplication::getApplicationName

const String
PlatonicDisplayApplication::getApplicationVersion(void)
{
    ODL_OBJENTER(); //####
    ODL_OBJEXIT_S(ProjectInfo::versionString); //####
    return ProjectInfo::versionString;
} // PlatonicDisplayApplication::getApplicationVersion

String
PlatonicDisplayApplication::getEnvironmentVar(const char * varName)
{
    ODL_ENTER(); //####
    ODL_S1("varName = ", varName); //####
    String           result;
    YarpStringVector keys;
    YarpStringVector values;
    YarpString       match(varName);

    getEnvironmentVars(keys, values);
    for (size_t ii = 0, mm = keys.size(); mm > ii; ++ii)
    {
        const char * keysAsChars = keys[ii].c_str();

        if (caseInsensitiveMatch(varName, keysAsChars))
        {
            result = values[ii].c_str();
            break;
        }

    }
    ODL_EXIT_s(result.toStdString()); //####
    return result;
} // PlatonicDisplayApplication::getEnvironmentVar

void
PlatonicDisplayApplication::getEnvironmentVars(YarpStringVector & keys,
                                           YarpStringVector & values)
{
    ODL_ENTER(); //####
    ODL_P2("keys = ", &keys, "values = ", &values);
#if (! defined(__APPLE__))
# if YARP_SYSTEM_INFO_MOVED_
    yarp::os::Property vars(yarp::os::SystemInfo::getPlatformInfo().environmentVars);
# else // ! YARP_SYSTEM_INFO_MOVED_
    yarp::os::Property vars(yarp::os::impl::SystemInfo::getPlatformInfo().environmentVars);
# endif // ! YARP_SYSTEM_INFO_MOVED_
    YarpString         varsAsString(vars.toString());
    yarp::os::Bottle   varsAsBottle(varsAsString);
#endif // ! defined(__APPLE__)

    keys.clear();
    values.clear();
#if defined(__APPLE__)
    // Since neither the 'environ' global variable nor the 'getenv()' function contain a useable
    // set of environment variables, we will cheat - launch a command shell, indicating that it is
    // an interactive, login shell, with a single command 'set'; this will cause various files to
    // be loaded that set the shell environment... giving us a useable set of environment variables.
    String       execPath("/bin/sh");
    ChildProcess runApplication;
    StringArray  nameAndArgs(execPath);

    nameAndArgs.add("-i");
    nameAndArgs.add("-l");
    nameAndArgs.add("-c");
    nameAndArgs.add("set");
    if (runApplication.start(nameAndArgs))
    {
        const String childOutput(runApplication.readAllProcessOutput());

        LazyLaunchProcess(runApplication, kThreadKillTime);
        ODL_S1s("childOutput = ", childOutput.toStdString()); //####
        if (0 < childOutput.length())
        {
            StringArray aRecord(StringArray::fromTokens(childOutput, "\n", ""));

            for (int ii = 0, mm = aRecord.size(); mm > ii; ++ii)
            {
                std::string tmpVariable(aRecord[ii].toStdString());
                size_t      equalsSign = tmpVariable.find("=");

                if (std::string::npos != equalsSign)
                {
                    keys.push_back(tmpVariable.substr(0, equalsSign));
                    values.push_back(tmpVariable.substr(equalsSign + 1));
                }
            }
        }
    }
    else
    {
        ODL_LOG("! (runApplication.start(nameAndArgs))"); //####
    }
#else // ! defined(__APPLE__)
    for (int ii = 0, numVars = varsAsBottle.size(); numVars > ii; ++ii)
    {
        yarp::os::Value & aValue = varsAsBottle.get(ii);

        if (aValue.isList())
        {
            yarp::os::Bottle * asList = aValue.asList();

            if (asList && (2 == asList->size()))
            {
                yarp::os::Value & keyValue = asList->get(0);
                yarp::os::Value & valueValue = asList->get(1);

                if (keyValue.isString() && valueValue.isString())
                {
                    YarpString keyString = keyValue.asString();
                    YarpString valueString = valueValue.asString();

                    if ((0 < keyString.length()) && (0 < valueString.length()))
                    {
                        keys.push_back(keyString);
                        values.push_back(valueString);
                    }
                }
            }
        }
    }
#endif // ! defined(__APPLE__)
    ODL_EXIT(); //####
} // PlatonicDisplayApplication::getEnvironmentVars

PlatonicDisplayWindow *
PlatonicDisplayApplication::getMainWindow(void)
{
    ODL_ENTER(); //####
    PlatonicDisplayWindow * result = getApp()->_mainWindow;

    ODL_EXIT_P(result);
    return result;
} // PlatonicDisplayApplication::getMainWindow

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
void
PlatonicDisplayApplication::initialise(const String & commandLine)
{
#if (! defined(OD_ENABLE_LOGGING_))
# if MAC_OR_LINUX_
#  pragma unused(commandLine)
# endif // MAC_OR_LINUX_
#endif // ! defined(OD_ENABLE_LOGGING_)
    ODL_OBJENTER(); //####
    ODL_S1s("commandLine = ", commandLine.toStdString()); //####
#if MAC_OR_LINUX_
    Common::SetUpLogger(kApplicationName);
#endif // MAC_OR_LINUX_
    Common::Initialize(kApplicationName);
    Utilities::SetUpGlobalStatusReporter();
    Utilities::CheckForNameServerReporter();
    _mainWindow = new PlatonicDisplayWindow(kApplicationName);
    if (Utilities::CheckForValidNetwork(true))
    {
        _yarp = new yarp::os::Network; // This is necessary to establish any connections to the YARP
                                       // infrastructure.
    }
    else
    {
        ODL_LOG("! (yarp::os::Network::checkNetwork())"); //####
        MpM_WARNING_("YARP network not running.");
        String yarpPath = findPathToExecutable("yarp");

        // No running YARP server was detected - first check if YARP is actually available:
        if (0 < yarpPath.length())
        {
            validateYarp(yarpPath);
        }
        else
        {
            // If YARP isn't installed, say so and leave.
            AlertWindow::showMessageBox(AlertWindow::WarningIcon, getApplicationName(),
                                        T_("No YARP network was detected and a YARP executable "
                                           "could not be found in the PATH system environment "
                                           "variable. "
                                           "Execution is not possible."), String::empty,
                                        _mainWindow);
            _mainWindow->toFront(true);
            systemRequestedQuit();
        }
    }
    if (_yarp)
    {
        if (! Utilities::CheckForRegistryService())
        {
            String registryServicePath = findPathToExecutable(MpM_REGISTRY_EXECUTABLE_NAME_);
            
            if (0 < registryServicePath.length())
            {
                // If the Registry Service is installed, give the option of running it.
                validateRegistryService(registryServicePath);
            }
            else
            {
                // If YARP isn't installed, say so and leave.
                AlertWindow::showMessageBox(AlertWindow::WarningIcon, getApplicationName(),
                                            T_("A Registry Service executable could not be found "
                                               "in the PATH system environment variable. "
                                               "Launching the Registry Service is not possible."),
                                            String::empty, _mainWindow);
                _mainWindow->toFront(true);
                systemRequestedQuit();
            }
        }
    }
    ODL_OBJEXIT(); //####
} // PlatonicDisplayApplication::initialise
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

bool
PlatonicDisplayApplication::moreThanOneInstanceAllowed(void)
{
    ODL_OBJENTER(); //####
    ODL_OBJEXIT_B(true); //####
    return true;
} // PlatonicDisplayApplication::moreThanOneInstanceAllowed

bool
PlatonicDisplayApplication::serviceIsRunning(void)
const
{
    ODL_OBJENTER(); //####
    bool isRunning;
    
    if (NULL == _serviceThread)
    {
        ODL_LOG("(NULL == _serviceThread)"); //####
        isRunning = false;
    }
    else
    {
        isRunning = _serviceThread->isThreadRunning();
    }
    ODL_OBJEXIT_B(isRunning); //####
    return isRunning;
} // PlatonicDisplayApplication::serviceIsRunning

void
PlatonicDisplayApplication::setServiceThread(PlatonicServiceThread * newThread)
{
    ODL_OBJENTER(); //####
    ODL_P1("newThread = ", newThread); //####
    _serviceThread = newThread;
    ODL_P1("_serviceThread <- ", _serviceThread); //####
    ODL_OBJEXIT(); //####
} // PlatonicDisplayApplication::setServiceThread

void
PlatonicDisplayApplication::shutdown(void)
{
    ODL_OBJENTER(); //####
    SetExitRequest();
    _mainWindow = NULL; // (deletes our window)
    yarp::os::Network::fini();
    _yarp = NULL;
    Utilities::ShutDownGlobalStatusReporter();
    ODL_OBJEXIT(); //####
} // PlatonicDisplayApplication::shutdown

void
PlatonicDisplayApplication::startAssociatedService(void)
{
    ODL_OBJENTER(); //####
    PlatonicDisplayOutputService * aService;

    if (NULL == _serviceThread)
    {
        ODL_LOG("(NULL == _serviceThread)"); //####
        aService = configureAndCreateService();
        ODL_P1("aService <- ", aService); //####
        if (NULL == aService)
        {
            ODL_LOG("(NULL == aService)"); //####
        }
        else
        {
            PlatonicServiceThread * aThread = new PlatonicServiceThread(aService, "");

            ODL_P1("aThread <- ", aThread); //####
            if (NULL == aThread)
            {
                ODL_LOG("(NULL == aThread)"); //####
                delete aService;
            }
            else
            {
                ODL_LOG("! (NULL == aThread)"); //####
                setServiceThread(aThread);
                _serviceThread->startThread();
            }
        }
    }
    else
    {
        ODL_LOG("! (NULL == _serviceThread)"); //####
        aService = _serviceThread->getService();
        if (NULL == aService)
        {
            ODL_LOG("(NULL == aService)"); //####
            aService = configureAndCreateService();
            ODL_P1("aService <- ", aService); //####
            if (NULL == aService)
            {
                ODL_LOG("(NULL == aService)"); //####
            }
            else
            {
                ODL_LOG("! (NULL == aService)"); //####
                _serviceThread->setService(aService);
                _serviceThread->startThread();
            }
        }
        else
        {
            ODL_LOG("! (NULL == aService)"); //####
        }
    }
    ODL_OBJEXIT(); //####
} // PlatonicDisplayApplication::startAssociatedService

void
PlatonicDisplayApplication::stopAssociatedService(void)
{
    ODL_OBJENTER(); //####
    if (NULL == _serviceThread)
    {
        ODL_LOG("(NULL == _serviceThread)"); //####
    }
    else
    {
        ODL_LOG("! (NULL == _serviceThread)"); //####
        if (_serviceThread->isThreadRunning())
        {
            ODL_LOG("(_serviceThread->isThreadRunning())"); //####
            PlatonicDisplayOutputService * aService = _serviceThread->getService();
            
            if (NULL == aService)
            {
                ODL_LOG("(NULL == aService)"); //####
            }
            else if (! Utilities::StopAService(aService->getEndpoint().getName(),
                                               STANDARD_WAIT_TIME_))
            {
                ODL_LOG("(! Utilities::StopAService(aService->getEndpoint().getName(), " //####
                        "STANDARD_WAIT_TIME_))"); //####
                aService->requestServiceStop();
                _serviceThread->stopThread(5000 /* milliseconds */);
            }
        }
        setServiceThread(NULL);
    }
    ODL_OBJEXIT(); //####
} // PlatonicDisplayApplication::stopAssociatedService

void
PlatonicDisplayApplication::systemRequestedQuit(void)
{
    ODL_OBJENTER(); //####
    // This is called when the app is being asked to quit: you can ignore this request and let the
    // app carry on running, or call quit() to allow the app to close.
    quit();
    ODL_OBJEXIT(); //####
} // PlatonicDisplayApplication::systemRequestedQuit

void
PlatonicDisplayApplication::validateRegistryService(const String & registryServicePath)
{
    ODL_OBJENTER(); //####
    ODL_S1s("registryServicePath = ", registryServicePath.toStdString()); //####
    ChildProcess runRegistryService;
    String       appName(JUCEApplication::getInstance()->getApplicationName());
    StringArray  nameAndArgs(registryServicePath);

    nameAndArgs.add("--vers");
    if (runRegistryService.start(nameAndArgs))
    {
        const String childOutput(runRegistryService.readAllProcessOutput());

        LazyLaunchProcess(runRegistryService, kThreadKillTime);
        ODL_S1s("childOutput = ", childOutput.toStdString()); //####
        if (0 < childOutput.length())
        {
            // We have a useable Registry Service executable - ask what the user wants to do.
            AlertWindow::showMessageBox(AlertWindow::WarningIcon, appName,
                                        T_("No running Registry Service was "
                                           "detected - you will need to launch "
                                           "the Registry Service executable. "
                                           "Execution is not possible until the Registry Service "
                                           "is running."), String::empty, _mainWindow);
        }
        else
        {
            // The Registry Service executable can't actually be launched!
            AlertWindow::showMessageBox(AlertWindow::WarningIcon, appName,
                                        T_("The Registry Service executable found in the PATH "
                                           "system environment variable did not return valid data. "
                                           "Launching the Registry Service is not possible."),
                                        String::empty, _mainWindow);
        }
    }
    else
    {
        ODL_LOG("! (runRegistryService.start(nameAndArgs))"); //####
        AlertWindow::showMessageBox(AlertWindow::WarningIcon, appName,
                                    T_("The Registry Service executable found in the PATH system "
                                       "environment variable could not be started. "
                                       "Launching the Registry Service is not possible."),
                                    String::empty, _mainWindow);
    }
    _mainWindow->toFront(true);
    systemRequestedQuit();
    ODL_OBJEXIT(); //####
} // PlatonicDisplayApplication::validateRegistryService

void
PlatonicDisplayApplication::validateYarp(const String & yarpPath)
{
    ODL_OBJENTER(); //####
    ODL_S1s("yarpPath ", yarpPath.toStdString()); //####
    ChildProcess runYarp;
    String       appName(JUCEApplication::getInstance()->getApplicationName());
    StringArray  nameAndArgs(yarpPath);

    nameAndArgs.add("version");
    if (runYarp.start(nameAndArgs))
    {
        const String childOutput(runYarp.readAllProcessOutput());

        LazyLaunchProcess(runYarp, kThreadKillTime);
        ODL_S1s("childOutput = ", childOutput.toStdString()); //####
        if (0 < childOutput.length())
        {
            // We have a useable YARP executable.
            AlertWindow::showMessageBox(AlertWindow::WarningIcon, appName,
                                        T_("No YARP network was detected - you will need to launch "
                                           "the YARP executable. "
                                           "Execution is not possible until the YARP executable is "
                                           "running."), String::empty, _mainWindow);
        }
        else
        {
            // The YARP executable can't actually be launched!
            AlertWindow::showMessageBox(AlertWindow::WarningIcon, appName,
                                        T_("No YARP network was detected and the YARP executable "
                                           "found in the PATH system environment variable did not "
                                           "return valid data. "
                                           "Execution is not possible."), String::empty,
                                        _mainWindow);
        }
    }
    else
    {
        ODL_LOG("! (runYarp.start(nameAndArgs))"); //####
        AlertWindow::showMessageBox(AlertWindow::WarningIcon, appName,
                                    T_("No YARP network was detected and the YARP executable found "
                                       "in the PATH system environment variable could not be "
                                       "started. "
                                       "Execution is not possible."), String::empty, _mainWindow);
    }
    _mainWindow->toFront(true);
    systemRequestedQuit();
    ODL_OBJEXIT(); //####
} // validateYarp

void
PlatonicDisplayApplication::zoomIn(void)
{
    ODL_OBJENTER(); //####
    if (_graphicsPanel)
    {
        _graphicsPanel->zoomIn();
    }
    ODL_OBJEXIT(); //####
} // PlatonicDisplayApplication::zoomIn

void
PlatonicDisplayApplication::zoomOut(void)
{
    ODL_OBJENTER(); //####
    if (_graphicsPanel)
    {
        _graphicsPanel->zoomOut();
    }
    ODL_OBJEXIT(); //####
} // PlatonicDisplayApplication::zoomOut

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
