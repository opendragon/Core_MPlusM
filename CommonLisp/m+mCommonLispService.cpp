//--------------------------------------------------------------------------------------------------
//
//  File:       m+mCommonLispService.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for the Common Lisp input / output service.
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
//  Created:    2015-08-05
//
//--------------------------------------------------------------------------------------------------

#include "m+mCommonLispService.h"

#include "m+mCommonLispInputHandler.h"
#include "m+mCommonLispRequests.h"
#include "m+mCommonLispThread.h"

#include <m+m/m+mEndpoint.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for the Common Lisp input / output service. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::CommonLisp;
using std::cerr;
using std::cout;
using std::endl;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

/*! @brief The name of the 'hash2assoc' function. */
#define HASH2ASSOC_NAME_ "HASH2ASSOC"

#if defined(__APPLE__)
# pragma mark Global constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

/*! @brief Fill a bottle with the contents of an object.
 @param aBottle The bottle to be filled.
 @param theData The value to be sent.
 @param hashMapFunction The function to be applied to hash tables to get values that can be placed
 in a bottle.
 @param topLevel @c true if this is the outermost list of an object. */
static void fillBottleFromValue(yarp::os::Bottle & aBottle,
                                cl_object          theData,
                                cl_object          hashMapFunction,
                                const bool         topLevel)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P3("aBottle = ", &aBottle, "theData = ", theData, "hashMapFunction = ", //####
              hashMapFunction); //####
    OD_LOG_B1("topLevel = ", topLevel); //####
    if (ECL_NIL != cl_integerp(theData))
    {
        aBottle.addInt(ecl_to_fixnum(theData));
    }
    else if (ECL_NIL != cl_realp(theData))
    {
        aBottle.addDouble(ecl_to_double(theData));
    }
    else if (ECL_NIL != cl_stringp(theData))
    {
        cl_object aValue = si_coerce_to_base_string(theData);

        if (ECL_NIL == aValue)
        {
            aBottle.addString("<unconvertible string>");
        }
        else
        {
            aBottle.addString(reinterpret_cast<char *>(aValue->base_string.self));
        }
    }
    else if (ECL_NIL != cl_symbolp(theData))
    {
        cl_object aName = cl_symbol_name(theData);

        if (ECL_NIL == aName)
        {
            aBottle.addString("<problematic symbol>");
        }
        else
        {
            if (ECL_NIL == cl_stringp(aName))
            {
                aName = cl_string(aName);
            }
            aName = si_coerce_to_base_string(aName);
            if (ECL_NIL == aName)
            {
                aBottle.addString("<unconvertible symbol>");
            }
            else
            {
                aBottle.addString(reinterpret_cast<char *>(aName->base_string.self));
            }
        }
    }
    else if (ECL_NIL != cl_characterp(theData))
    {
        cl_object asString = cl_string(theData);

        if (ECL_NIL == asString)
        {
            aBottle.addString("<unconvertible character>");
        }
        else
        {
            aBottle.addString(reinterpret_cast<char *>(asString->base_string.self));
        }
    }
    else if (ECL_NIL != cl_hash_table_p(theData))
    {
        cl_env_ptr env = ecl_process_env();
        cl_object  aList;
        cl_object  errorSymbol = ecl_make_symbol("ERROR", "CL");

        ECL_RESTART_CASE_BEGIN(env, ecl_list1(errorSymbol))
        {
            /* This form is evaluated with bound handlers. */
            aList = cl_funcall(1, hashMapFunction, theData);
        }
        ECL_RESTART_CASE(1, condition)
        {
            /* This code is executed when an error happens. */
            aList = ECL_NIL;
#if MAC_OR_LINUX_
            GetLogger().fail("Script aborted during load.");
#else // ! MAC_OR_LINUX_
            cerr << "Script aborted during load." << endl;
#endif // ! MAC_OR_LINUX_
        }
        ECL_RESTART_CASE_END;
        if (ECL_NIL != aList)
        {
            yarp::os::Property & innerDict(aBottle.addDict());

            for ( ; ECL_NIL != aList; aList = cl_cdr(aList))
            {
                cl_object  aPair = cl_car(aList);
                cl_object  aKey = cl_car(aPair);
                cl_object  aValue = cl_cdr(aPair);
                YarpString keyToUse;

                if (ECL_NIL != cl_stringp(aKey))
                {
                    cl_object keyValue = si_coerce_to_base_string(aKey);

                    if (ECL_NIL != aValue)
                    {
                        keyToUse = reinterpret_cast<char *>(aValue->base_string.self);
                    }
                }
                else if (ECL_NIL != cl_symbolp(aKey))
                {
                    cl_object aName = cl_symbol_name(theData);

                    if (ECL_NIL != aName)
                    {
                        if (ECL_NIL == cl_stringp(aName))
                        {
                            aName = cl_string(aName);
                        }
                        aName = si_coerce_to_base_string(aName);
                        if (ECL_NIL != aName)
                        {
                            keyToUse = reinterpret_cast<char *>(aName->base_string.self);
                        }
                    }
                }
                else if (ECL_NIL != cl_characterp(aKey))
                {
                    cl_object asString = cl_string(aKey);
                    
                    if (ECL_NIL != asString)
                    {
                        keyToUse = reinterpret_cast<char *>(asString->base_string.self);
                    }
                }
                if (0 < keyToUse.length())
                {
                    yarp::os::Bottle convertedResult;

                    fillBottleFromValue(convertedResult, aValue, hashMapFunction, false);
                    if (1 == convertedResult.size())
                    {
                        yarp::os::Value anElement(convertedResult.get(0));

                        if (anElement.isInt())
                        {
                            innerDict.put(keyToUse, anElement.asInt());
                        }
                        else if (anElement.isDouble())
                        {
                            innerDict.put(keyToUse, anElement.asDouble());
                        }
                        else if (anElement.isString())
                        {
                            innerDict.put(keyToUse, anElement.asString());
                        }
                        else
                        {
                            innerDict.put(keyToUse, anElement);
                        }
                    }
                }
            }
        }
    }
    else if (ECL_NIL != cl_listp(theData))
    {
        for ( ; ECL_NIL != theData; theData = cl_cdr(theData))
        {
            cl_object anElement = cl_car(theData);

            if (ECL_NIL != anElement)
            {
                fillBottleFromValue(aBottle, anElement, hashMapFunction, false);
            }
        }
    }
    else if (ECL_NIL != cl_arrayp(theData))
    {
        if (1 == ecl_fixnum(cl_array_rank(theData)))
        {
            cl_fixnum numElements = ecl_fixnum(cl_array_dimension(theData, ecl_make_fixnum(0)));

            // Treat as a list
            if (topLevel)
            {
                for (cl_fixnum ii = 0; numElements > ii; ++ii)
                {
                    cl_object anElement = cl_aref(2, theData, ecl_make_fixnum(ii));

                    if (ECL_NIL != anElement)
                    {
                        fillBottleFromValue(aBottle, anElement, hashMapFunction, false);
                    }
                }
            }
            else
            {
                yarp::os::Bottle & innerList(aBottle.addList());

                for (cl_fixnum ii = 0; numElements > ii; ++ii)
                {
                    cl_object anElement = cl_aref(2, theData, ecl_make_fixnum(ii));

                    if (ECL_NIL != anElement)
                    {
                        fillBottleFromValue(innerList, anElement, hashMapFunction, false);
                    }
                }
            }
        }
    }
    else
    {
        aBottle.addString("<untranslatable>");
    }
    OD_LOG_EXIT(); //####
} // fillBottleFromValue

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and Destructors
#endif // defined(__APPLE__)

CommonLispService::CommonLispService(const Utilities::DescriptorVector & argumentList,
                                     const YarpString &                  launchPath,
                                     const int                           argc,
                                     char * *                            argv,
                                     const YarpString &                  tag,
                                     const YarpString &                  description,
                                     const Common::ChannelVector &       loadedInletDescriptions,
                                     const Common::ChannelVector &       loadedOutletDescriptions,
                                     const ObjectVector &                loadedInletHandlers,
                                     cl_object                           loadedStartingFunction,
                                     cl_object                           loadedStoppingFunction,
                                     const bool                          sawThread,
                                     cl_object                           loadedThreadFunction,
                                     const double                        loadedInterval,
                                     const YarpString &                  serviceEndpointName,
                                     const YarpString &                  servicePortNumber) :
    inherited(argumentList, launchPath, argc, argv, tag, true, MpM_COMMONLISP_CANONICAL_NAME_,
              description, "", serviceEndpointName, servicePortNumber),
    _inletHandlers(loadedInletHandlers), _inHandlers(), _generator(NULL),
    _loadedInletDescriptions(loadedInletDescriptions),
    _loadedOutletDescriptions(loadedOutletDescriptions), _interlock(0),
    _scriptStartingFunc(loadedStartingFunction), _scriptStoppingFunc(loadedStoppingFunction),
    _scriptThreadFunc(loadedThreadFunction), _hash2assocFunc(ECL_NIL),
    _threadInterval(loadedInterval), _isThreaded(sawThread)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P4("argumentList = ", &argumentList, "argv = ", argv, //####
              "loadedInletDescriptions = ", &loadedInletDescriptions, //####
              "loadedOutletDescriptions = ", &loadedOutletDescriptions); //####
    OD_LOG_P4("loadedInletHandlers = ", &loadedInletHandlers, "loadedStartingFunction = ", //####
              loadedStartingFunction, "loadedStoppingFunction = ", loadedStoppingFunction, //####
              "loadedThreadFunction = ", loadedThreadFunction); //####
    OD_LOG_LL1("argc = ", argc); //####
    OD_LOG_S4s("launchPath = ", launchPath, "tag = ", tag, "description = ", description, //####
               "serviceEndpointName = ", serviceEndpointName); //####
    OD_LOG_S1s("servicePortNumber = ", servicePortNumber); //####
    OD_LOG_B1("sawThread = ", sawThread); //####
    OD_LOG_D1("loadedInterval = ", loadedInterval); //####
    if (_isThreaded && (ECL_NIL != _scriptThreadFunc))
    {
        setNeedsIdle();
    }
    try
    {
        // The following can't be directly expressed in C/C++, but is better described as Common
        // Lisp -
        cl_object definition = c_string_to_object("((aTable) "
                                                  "(let (asList) "
                                                  "(maphash #'(lambda (key val) "
                                                  "(setq asList (acons key val asList))) aTable)"
                                                  " asList))");
        cl_object aName = cl_find_symbol(1,
                                 ecl_make_simple_base_string(const_cast<char *>(HASH2ASSOC_NAME_),
                                                             sizeof(HASH2ASSOC_NAME_) - 1));

        _hash2assocFunc = si_make_lambda(aName, definition);
    }
    catch (...)
    {
    }
    OD_LOG_EXIT_P(this); //####
} // CommonLispService::CommonLispService

CommonLispService::~CommonLispService(void)
{
    OD_LOG_OBJENTER(); //####
    stopStreams();
    releaseHandlers();
    OD_LOG_OBJEXIT(); //####
} // CommonLispService::~CommonLispService

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
DEFINE_CONFIGURE_(CommonLispService)
{
#if (! defined(MpM_DoExplicitDisconnect))
# if MAC_OR_LINUX_
#  pragma unused(details)
# endif // MAC_OR_LINUX_
#endif // ! defined(MpM_DoExplicitDisconnect)
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("details = ", &details); //####
    bool result = false;
    
    try
    {
        if (ECL_NIL == _scriptStartingFunc)
        {
            result = true;
        }
        else
        {
            cl_env_ptr env = ecl_process_env();
            cl_object  aValue;
            cl_object  errorSymbol = ecl_make_symbol("ERROR", "CL");

            ECL_RESTART_CASE_BEGIN(env, ecl_list1(errorSymbol))
            {
                /* This form is evaluated with bound handlers. */
                aValue = cl_funcall(1, _scriptStartingFunc);
            }
            ECL_RESTART_CASE(1, condition)
            {
                /* This code is executed when an error happens. */
                aValue = ECL_NIL;
#if MAC_OR_LINUX_
                GetLogger().fail("Script aborted during load.");
#else // ! MAC_OR_LINUX_
                cerr << "Script aborted during load." << endl;
#endif // ! MAC_OR_LINUX_
            }
            ECL_RESTART_CASE_END;
            if (ECL_T == aValue)
            {
                result = true;
            }
            else if (ECL_NIL == aValue)
            {
                // Script rejected starting, but gave no reason.
                cout << "Could not configure -> unknown reason." << endl;
            }
            else
            {
                if (ECL_NIL == cl_stringp(aValue))
                {
                    aValue = cl_string(aValue);
                }
                aValue = si_coerce_to_base_string(aValue);
                if (ECL_NIL != aValue)
                {
                    cout << "Could not configure -> " <<
                            reinterpret_cast<char *>(aValue->base_string.self) << "." << endl;
                }
            }
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // CommonLispService::configure
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

DEFINE_DISABLEMETRICS_(CommonLispService)
{
    OD_LOG_OBJENTER(); //####
    inherited::disableMetrics();
    for (HandlerVector::const_iterator walker(_inHandlers.begin()); _inHandlers.end() != walker;
         ++walker)
    {
        CommonLispInputHandler * aHandler = *walker;
        
        if (aHandler)
        {
            aHandler->disableMetrics();
        }
    }
    OD_LOG_OBJEXIT(); //####
} // CommonLispService::disableMetrics

DEFINE_DOIDLE_(CommonLispService)
{
    OD_LOG_OBJENTER(); //####
    if (isActive())
    {
        OD_LOG("(isActive())"); //####
        if (_interlock.check())
        {
            OD_LOG("(_interlock.check())"); //####
            if (ECL_NIL != _scriptThreadFunc)
            {
                OD_LOG("(ECL_NIL != _scriptThreadFunc)"); //####
                try
                {
                    cl_env_ptr env = ecl_process_env();
                    cl_object  errorSymbol = ecl_make_symbol("ERROR", "CL");

                    ECL_RESTART_CASE_BEGIN(env, ecl_list1(errorSymbol))
                    {
                        /* This form is evaluated with bound handlers. */
                        cl_funcall(1, _scriptThreadFunc);
                    }
                    ECL_RESTART_CASE(1, condition)
                    {
                        /* This code is executed when an error happens. */
#if MAC_OR_LINUX_
                        GetLogger().fail("Script aborted during load.");
#else // ! MAC_OR_LINUX_
                        cerr << "Script aborted during load." << endl;
#endif // ! MAC_OR_LINUX_
                    }
                    ECL_RESTART_CASE_END;
                }
                catch (...)
                {
                    OD_LOG("Exception caught"); //####
                    throw;
                }
            }
        }
    }
    OD_LOG_OBJEXIT(); //####
} // CommonLispService::doIdle

DEFINE_ENABLEMETRICS_(CommonLispService)
{
    OD_LOG_OBJENTER(); //####
    inherited::enableMetrics();
    for (HandlerVector::const_iterator walker(_inHandlers.begin()); _inHandlers.end() != walker;
         ++walker)
    {
        CommonLispInputHandler * aHandler = *walker;
        
        if (aHandler)
        {
            aHandler->enableMetrics();
        }
    }
    OD_LOG_OBJEXIT(); //####
} // CommonLispService::enableMetrics

DEFINE_GETCONFIGURATION_(CommonLispService)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_P1("details = ", &details); //####
    bool result = true;

    details.clear();
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // CommonLispService::getConfiguration

void CommonLispService::releaseHandlers(void)
{
    OD_LOG_OBJENTER(); //####
    if (0 < _inHandlers.size())
    {
        for (HandlerVector::const_iterator walker(_inHandlers.begin()); _inHandlers.end() != walker;
             ++walker)
        {
            CommonLispInputHandler * aHandler = *walker;
            
            if (aHandler)
            {
                OD_LOG_P1("aHandler = ", aHandler); //####
                delete aHandler;
            }
        }
        _inHandlers.clear();
    }
    OD_LOG_OBJEXIT(); //####
} // CommonLispService::releaseHandlers

DEFINE_RESTARTSTREAMS_(CommonLispService)
{
    OD_LOG_OBJENTER(); //####
    try
    {
        // No special processing needed.
        stopStreams();
        startStreams();
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT(); //####
} // CommonLispService::restartStreams

bool CommonLispService::sendToChannel(const cl_fixnum channelSlot,
                                      cl_object       theData)
{
    OD_LOG_OBJENTER();
    OD_LOG_LL1("channelSlot = ", channelSlot); //####
    bool okSoFar = false;

    if ((0 <= channelSlot) && (channelSlot < static_cast<cl_fixnum>(getOutletCount())))
    {
        Common::GeneralChannel * outChannel = getOutletStream(channelSlot);
        yarp::os::Bottle         outBottle;

        fillBottleFromValue(outBottle, theData, _hash2assocFunc, true);
        if ((0 < outBottle.size()) && outChannel)
        {
            if (outChannel->write(outBottle))
            {
                okSoFar = true;
            }
            else
            {
                OD_LOG("! (outChannel->write(message))"); //####
#if defined(MpM_StallOnSendProblem)
                Stall();
#endif // defined(MpM_StallOnSendProblem)
            }
        }
        else
        {
            // If there's nothing to write, or the channel is gone, continue as if everything is
            // fine.
            okSoFar = true;
        }
    }
    OD_LOG_OBJEXIT_B(okSoFar); //####
    return okSoFar;
} // CommonLispService::sendToChannel

DEFINE_SETUPSTREAMDESCRIPTIONS_(CommonLispService)
{
    OD_LOG_OBJENTER(); //####
    bool               result = true;
    ChannelDescription description;
    YarpString         rootName(getEndpoint().getName() + "/");
    
    _inDescriptions.clear();
    for (ChannelVector::const_iterator walker(_loadedInletDescriptions.begin());
         _loadedInletDescriptions.end() != walker; ++walker)
    {
        description._portName = rootName + walker->_portName;
        description._portProtocol = walker->_portProtocol;
        description._protocolDescription = walker->_protocolDescription;
        _inDescriptions.push_back(description);
    }
    _outDescriptions.clear();
    for (ChannelVector::const_iterator walker(_loadedOutletDescriptions.begin());
         _loadedOutletDescriptions.end() != walker; ++walker)
    {
        description._portName = rootName + walker->_portName;
        description._portProtocol = walker->_portProtocol;
        description._protocolDescription = walker->_protocolDescription;
        _outDescriptions.push_back(description);
    }
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // CommonLispService::setUpStreamDescriptions

void CommonLispService::signalRunFunction(void)
{
    OD_LOG_OBJENTER(); //####
    _interlock.post();
    OD_LOG_OBJEXIT(); //####
} // CommonLispService::signalRunFunction

DEFINE_STARTSERVICE_(CommonLispService)
{
    OD_LOG_OBJENTER(); //####
    try
    {
        if (! isStarted())
        {
            inherited::startService();
            if (isStarted())
            {
                
            }
            else
            {
                OD_LOG("! (isStarted())"); //####
            }
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT_B(isStarted()); //####
    return isStarted();
} // CommonLispService::startService

DEFINE_STARTSTREAMS_(CommonLispService)
{
    OD_LOG_OBJENTER(); //####
    try
    {
        if (! isActive())
        {
            if (_isThreaded)
            {
                _generator = new CommonLispThread(*this, _threadInterval);
				if (! _generator->start())
				{
					OD_LOG("(! _generator->start())"); //####
                    cerr << "Could not start auxiliary thread." << endl;
					delete _generator;
					_generator = NULL;
				}
            }
            else
            {
                releaseHandlers();
                for (size_t ii = 0, mm = getInletCount(); mm > ii; ++ii)
                {
                    cl_object                handlerFunc = _inletHandlers[ii];
                    CommonLispInputHandler * aHandler = new CommonLispInputHandler(this, ii,
                                                                                   handlerFunc);
                    
                    if (aHandler)
                    {
                        _inHandlers.push_back(aHandler);
                        aHandler->setChannel(getInletStream(ii));
                        getInletStream(ii)->setReader(*aHandler);
                        aHandler->activate();
                    }
                }
            }
            setActive();
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT(); //####
} // CommonLispService::startStreams

DEFINE_STOPSERVICE_(CommonLispService)
{
    OD_LOG_OBJENTER(); //####
    bool result;
    
    try
    {
        result = inherited::stopService();
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // CommonLispService::stopService

DEFINE_STOPSTREAMS_(CommonLispService)
{
    OD_LOG_OBJENTER(); //####
    try
    {
        if (isActive())
        {
            if (_isThreaded)
            {
				if (_generator)
				{
					_generator->stop();
					for ( ; _generator->isRunning(); )
					{
						yarp::os::Time::delay(_threadInterval / 3.9);
					}
					delete _generator;
					_generator = NULL;
				}
            }
            else
            {
                for (size_t ii = 0, mm = getInletCount(); mm > ii; ++ii)
                {
                    CommonLispInputHandler * aHandler = _inHandlers.at(ii);
                    
                    if (aHandler)
                    {
                        aHandler->deactivate();
                    }
                }
            }
            clearActive();
            // Tell the script that we're done for now.
            if (ECL_NIL != _scriptStoppingFunc)
            {
                cl_env_ptr env = ecl_process_env();
                cl_object  errorSymbol = ecl_make_symbol("ERROR", "CL");

                ECL_RESTART_CASE_BEGIN(env, ecl_list1(errorSymbol))
                {
                    /* This form is evaluated with bound handlers. */
                    cl_funcall(1, _scriptStoppingFunc);
                }
                ECL_RESTART_CASE(1, condition)
                {
                    /* This code is executed when an error happens. */
#if MAC_OR_LINUX_
                    GetLogger().fail("Script aborted during load.");
#else // ! MAC_OR_LINUX_
                    cerr << "Script aborted during load." << endl;
#endif // ! MAC_OR_LINUX_
                }
                ECL_RESTART_CASE_END;
            }
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT(); //####
} // CommonLispService::stopStreams

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

#if 0
std::ostream & CommonLisp::PrintCommonLispObject(std::ostream &     outStream,
                                                 JSContext *        jct,
                                                 JS::RootedObject & anObject,
                                                 const int          depth)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("jct = ", jct, "anObject = ", &anObject); //####
    OD_LOG_L1("depth = ", depth); //####
    JS::AutoIdArray ids(jct, JS_Enumerate(jct, anObject));
    
    // Note that only operator! is defined, so we need to do a 'double-negative'.
    if (!! ids)
    {
        bool okSoFar = true;
        
        for (size_t ii = 0, len = ids.length(); (len > ii) && okSoFar; ++ii)
        {
            JS::RootedValue key(jct);
            
            if (JS_IdToValue(jct, ids[ii], &key))
            {
                PrintCommonLispValue(outStream, jct, "id = ", key, depth);
            }
            else
            {
                okSoFar = false;
            }
            if (okSoFar)
            {
                JS::RootedValue result(jct);
                JS::RootedId    aRootedId(jct);
                
                aRootedId = ids[ii];
                if (JS_GetPropertyById(jct, anObject, aRootedId, &result))
                {
                    PrintCommonLispValue(outStream, jct, ", property = ", result, 0);
                }
                else
                {
                    okSoFar = false;
                }
                if (okSoFar)
                {
                    if (result.isObject())
                    {
                        JS::RootedObject asObject(jct);
                        
                        if (JS_ValueToObject(jct, result, &asObject))
                        {
                            if (JS_IsArrayObject(jct, result))
                            {
                                outStream << "array";
                                uint32_t arrayLength;
                                
                                if (JS_GetArrayLength(jct, asObject, &arrayLength))
                                {
                                    outStream << ", size = " << arrayLength;
                                }
                            }
                            else if (JS_ObjectIsFunction(jct, asObject))
                            {
                                outStream << "function";
                                JSFunction * asFunction = JS_ValueToFunction(jct, result);
                                
                                if (asFunction)
                                {
                                    outStream << ", arity = " << JS_GetFunctionArity(asFunction);
                                    if (! JS::IsCallable(asObject))
                                    {
                                        outStream << ", not callable";
                                    }
                                }
                            }
                            else
                            {
                                outStream << "object";
                            }
                        }
                        else
                        {
                            okSoFar = false;
                        }
                        outStream << endl;
                        if (okSoFar)
                        {
                            PrintCommonLispObject(outStream, jct, asObject, depth + 1);
                        }
                    }
                    else
                    {
                        outStream << endl;
                    }
                }
            }
        }
    }
    OD_LOG_EXIT_P(&outStream); //####
    return outStream;
} // CommonLisp::PrintCommonLispObject

std::ostream & CommonLisp::PrintCommonLispValue(std::ostream &    outStream,
                                                JSContext *       jct,
                                                const char *      caption,
                                                JS::RootedValue & value,
                                                const int         depth)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P3("outStream = ", &outStream, "jct = ", jct, "value = ", &value); //####
    OD_LOG_S1("caption = ", caption); //####
    if (0 < depth)
    {
        outStream.width(depth);
        outStream << " ";
    }
    cout << caption;
    if (value.isString())
    {
        JSString * asString = value.toString();
        char *     asChars = JS_EncodeString(jct, asString);
        
        outStream << "string(" << asChars << ")";
        JS_free(jct, asChars);
    }
    else if (value.isObject())
    {
        // Objects will be processed separately.
    }
    else if (value.isInt32())
    {
        outStream << "int32(" << value.toInt32() << ")";
    }
    else if (value.isBoolean())
    {
        outStream << "boolean(" << (value.toBoolean() ? "true" : "false") << ")";
    }
    else if (value.isDouble())
    {
        outStream << "double(" << value.toDouble() << ")";
    }
    else if (value.isNullOrUndefined())
    {
        outStream << "null or undefined";
    }
    else
    {
        outStream << "other";
    }
    OD_LOG_EXIT_P(&outStream); //####
    return outStream;
} // CommonLisp::PrintCommonLispValue
#endif//0
