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

#include <odl/ODEnableLogging.h>
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

/*! @brief The name of the 'hash-to-assoc' function. */
#define HASH_TO_ASSOC_NAME_ "hash-to-assoc"

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
        OD_LOG("(ECL_NIL != cl_integerp(theData))"); //####
        aBottle.addInt(ecl_to_fixnum(theData));
    }
    else if (ECL_NIL != cl_realp(theData))
    {
        OD_LOG("(ECL_NIL != cl_realp(theData))"); //####
        aBottle.addDouble(ecl_to_double(theData));
    }
    else if (ECL_NIL != cl_stringp(theData))
    {
        OD_LOG("(ECL_NIL != cl_stringp(theData))"); //####
        cl_object aValue = si_coerce_to_base_string(theData);

        if (ECL_NIL == aValue)
        {
            OD_LOG("(ECL_NIL == aValue)"); //####
            aBottle.addString("<unconvertible string>");
        }
        else
        {
            OD_LOG("! (ECL_NIL == aValue)"); //####
            aBottle.addString(reinterpret_cast<char *>(aValue->base_string.self));
        }
    }
    else if (ECL_NIL != cl_symbolp(theData))
    {
        OD_LOG("(ECL_NIL != cl_symbolp(theData))"); //####
        cl_object aName = cl_symbol_name(theData);

        if (ECL_NIL == aName)
        {
            OD_LOG("(ECL_NIL == aName)"); //####
            aBottle.addString("<problematic symbol>");
        }
        else
        {
            OD_LOG("! (ECL_NIL == aName)"); //####
            if (ECL_NIL == cl_stringp(aName))
            {
                OD_LOG("(ECL_NIL == cl_stringp(aName))"); //####
                aName = cl_string(aName);
            }
            aName = si_coerce_to_base_string(aName);
            if (ECL_NIL == aName)
            {
                OD_LOG("(ECL_NIL == aName)"); //####
                aBottle.addString("<unconvertible symbol>");
            }
            else
            {
                OD_LOG("! (ECL_NIL == aName)"); //####
                aBottle.addString(reinterpret_cast<char *>(aName->base_string.self));
            }
        }
    }
    else if (ECL_NIL != cl_characterp(theData))
    {
        OD_LOG("(ECL_NIL != cl_characterp(theData))"); //####
        cl_object asString = cl_string(theData);

        if (ECL_NIL == asString)
        {
            OD_LOG("(ECL_NIL == asString)"); //####
            aBottle.addString("<unconvertible character>");
        }
        else
        {
            OD_LOG("! (ECL_NIL == asString)"); //####
            aBottle.addString(reinterpret_cast<char *>(asString->base_string.self));
        }
    }
    else if (ECL_NIL != cl_hash_table_p(theData))
    {
        OD_LOG("(ECL_NIL != cl_hash_table_p(theData))"); //####
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
            GetLogger().fail("The 'hashMap' function failed.");
#else // ! MAC_OR_LINUX_
            cerr << "The 'hashMap' function failed." << endl;
#endif // ! MAC_OR_LINUX_
        }
        ECL_RESTART_CASE_END;
        if (ECL_NIL != aList)
        {
            OD_LOG("(ECL_NIL != aList)"); //####
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
                        OD_LOG("(ECL_NIL != aValue)"); //####
                        keyToUse = reinterpret_cast<char *>(aValue->base_string.self);
                    }
                }
                else if (ECL_NIL != cl_symbolp(aKey))
                {
                    OD_LOG("(ECL_NIL != cl_symbolp(aKey))"); //####
                    cl_object aName = cl_symbol_name(theData);

                    if (ECL_NIL != aName)
                    {
                        OD_LOG("(ECL_NIL != aName)"); //####
                        if (ECL_NIL == cl_stringp(aName))
                        {
                            OD_LOG("(ECL_NIL == cl_stringp(aName))"); //####
                            aName = cl_string(aName);
                        }
                        aName = si_coerce_to_base_string(aName);
                        if (ECL_NIL != aName)
                        {
                            OD_LOG("(ECL_NIL != aName)"); //####
                            keyToUse = reinterpret_cast<char *>(aName->base_string.self);
                        }
                    }
                }
                else if (ECL_NIL != cl_characterp(aKey))
                {
                    OD_LOG("(ECL_NIL != cl_characterp(aKey))"); //####
                    cl_object asString = cl_string(aKey);
                    
                    if (ECL_NIL != asString)
                    {
                        OD_LOG("(ECL_NIL != asString)"); //####
                        keyToUse = reinterpret_cast<char *>(asString->base_string.self);
                    }
                }
                if (0 < keyToUse.length())
                {
                    OD_LOG("(0 < keyToUse.length())"); //####
                    yarp::os::Bottle convertedResult;

                    fillBottleFromValue(convertedResult, aValue, hashMapFunction, false);
                    if (1 == convertedResult.size())
                    {
                        OD_LOG("(1 == convertedResult.size())"); //####
                        yarp::os::Value anElement(convertedResult.get(0));

                        if (anElement.isInt())
                        {
                            OD_LOG("(anElement.isInt())"); //####
                            innerDict.put(keyToUse, anElement.asInt());
                        }
                        else if (anElement.isDouble())
                        {
                            OD_LOG("(anElement.isDouble())"); //####
                            innerDict.put(keyToUse, anElement.asDouble());
                        }
                        else if (anElement.isString())
                        {
                            OD_LOG("(anElement.isString())"); //####
                            innerDict.put(keyToUse, anElement.asString());
                        }
                        else
                        {
                            OD_LOG("! (anElement.isString())"); //####
                            innerDict.put(keyToUse, anElement);
                        }
                    }
                }
            }
        }
    }
    else if (ECL_NIL != cl_listp(theData))
    {
        OD_LOG("(ECL_NIL != cl_listp(theData))"); //####
        for ( ; ECL_NIL != theData; theData = cl_cdr(theData))
        {
            cl_object anElement = cl_car(theData);

            if (ECL_NIL != anElement)
            {
                OD_LOG("(ECL_NIL != anElement)"); //####
                fillBottleFromValue(aBottle, anElement, hashMapFunction, false);
            }
        }
    }
    else if (ECL_NIL != cl_arrayp(theData))
    {
        OD_LOG("(ECL_NIL != cl_arrayp(theData))"); //####
        if (1 == ecl_fixnum(cl_array_rank(theData)))
        {
            OD_LOG("(1 == ecl_fixnum(cl_array_rank(theData)))"); //####
            cl_fixnum numElements = ecl_fixnum(cl_array_dimension(theData, ecl_make_fixnum(0)));

            // Treat as a list
            OD_LOG_LL1("numElements <- ", numElements); //####
            if (topLevel)
            {
                OD_LOG("(topLevel)"); //####
                for (cl_fixnum ii = 0; numElements > ii; ++ii)
                {
                    cl_object anElement = cl_aref(2, theData, ecl_make_fixnum(ii));

                    if (ECL_NIL != anElement)
                    {
                        OD_LOG("(ECL_NIL != anElement)"); //####
                        fillBottleFromValue(aBottle, anElement, hashMapFunction, false);
                    }
                }
            }
            else
            {
                OD_LOG("! (topLevel)"); //####
                yarp::os::Bottle & innerList(aBottle.addList());

                for (cl_fixnum ii = 0; numElements > ii; ++ii)
                {
                    cl_object anElement = cl_aref(2, theData, ecl_make_fixnum(ii));

                    if (ECL_NIL != anElement)
                    {
                        OD_LOG("(ECL_NIL != anElement)"); //####
                        fillBottleFromValue(innerList, anElement, hashMapFunction, false);
                    }
                }
            }
        }
    }
    else
    {
        OD_LOG("! (ECL_NIL != cl_arrayp(theData))"); //####
        aBottle.addString("<untranslatable>");
    }
    OD_LOG_EXIT(); //####
} // fillBottleFromValue

/*! @brief Convert a YARP value into a Common Lisp object.
 @param setHashFunction The function object to use when setting a hash table entry.
 @param inputValue The value to be processed.
 @returns The output object. */
static cl_object convertValue(cl_object               setHashFunction,
                              const yarp::os::Value & inputValue);

/*! @brief Convert a YARP dictionary into a Common Lisp object.
 @param setHashFunction The function object to use when setting a hash table entry.
 @param inputAsList The input dictionary as a list.
 @returns The output object. */
static cl_object convertDictionary(cl_object                setHashFunction,
                                   const yarp::os::Bottle & inputAsList)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("setHashFunction = ", setHashFunction, "inputAsList = ", &inputAsList); //####
    cl_object result = cl_make_hash_table(0);
    OD_LOG_P1("result <- ", result); //####
    
    for (int ii = 0, mm = inputAsList.size(); mm > ii; ++ii)
    {
        yarp::os::Value anEntry(inputAsList.get(ii));
        
        if (anEntry.isList())
        {
            OD_LOG("(anEntry.isList())"); //####
            yarp::os::Bottle * entryAsList = anEntry.asList();
            
            if (entryAsList && (2 == entryAsList->size()))
            {
                OD_LOG("(entryAsList && (2 == entryAsList->size()))"); //####
                YarpString      aKey(entryAsList->get(0).toString());
                yarp::os::Value aValue(entryAsList->get(1));
                cl_object       anElement = convertValue(setHashFunction, aValue);
                
                OD_LOG_P1("anElement <- ", anElement); //####
                cl_object       elementKey = CreateBaseString(aKey.c_str(), aKey.length());
                cl_env_ptr      env = ecl_process_env();
                cl_object       errorSymbol = ecl_make_symbol("ERROR", "CL");
                
                ECL_RESTART_CASE_BEGIN(env, ecl_list1(errorSymbol))
                {
                    /* This form is evaluated with bound handlers. */
                    cl_funcall(4, setHashFunction, result, elementKey, anElement);
                }
                ECL_RESTART_CASE(1, condition)
                {
                    /* This code is executed when an error happens. */
#if MAC_OR_LINUX_
                    GetLogger().fail("Function 'setHash' failed.");
#else // ! MAC_OR_LINUX_
                    cerr << "Function 'setHash' failed." << endl;
#endif // ! MAC_OR_LINUX_
                }
                ECL_RESTART_CASE_END;
            }
        }
    }
    OD_LOG_EXIT_P(result); //####
    return result;
} // convertDictionary

/*! @brief Convert a YARP list into a Common Lisp object.
 @param setHashFunction The function object to use when setting a hash table entry.
 @param inputValue The value to be processed.
 @returns The result object. */
static cl_object convertList(cl_object                setHashFunction,
                             const yarp::os::Bottle & inputValue)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("setHashFunction = ", setHashFunction, "inputValue = ", &inputValue); //####
    cl_object result = ecl_alloc_simple_vector(inputValue.size(), ecl_aet_object);
    
    OD_LOG_P1("result <- ", result); //####
    for (int ii = 0, mm = inputValue.size(); mm > ii; ++ii)
    {
        yarp::os::Value aValue(inputValue.get(ii));
        cl_object       anElement = convertValue(setHashFunction, aValue);
        OD_LOG_P1("anElement <- ", anElement); //####
        
        ecl_aset1(result, ii, anElement);
    }
    OD_LOG_EXIT_P(result); //####
    return result;
} // convertList

static cl_object convertValue(cl_object               setHashFunction,
                              const yarp::os::Value & inputValue)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("setHashFunction = ", setHashFunction, "inputValue = ", &inputValue); //####
    cl_object result = ECL_NIL;
    
    if (inputValue.isBool())
    {
        OD_LOG("(inputValue.isBool())"); //####
        result = ecl_make_fixnum(inputValue.asBool() ? 1 : 0);
        OD_LOG_P1("result <- ", result); //####
    }
    else if (inputValue.isInt())
    {
        OD_LOG("(inputValue.isInt())"); //####
        result = ecl_make_fixnum(inputValue.asInt());
        OD_LOG_P1("result <- ", result); //####
    }
    else if (inputValue.isString())
    {
        OD_LOG("(inputValue.isString())"); //####
        YarpString value = inputValue.asString();
        
        result = CreateBaseString(value.c_str(), value.length());
        OD_LOG_P1("result <- ", result); //####
    }
    else if (inputValue.isDouble())
    {
        OD_LOG("(inputValue.isDouble())"); //####
        result = ecl_make_double_float(inputValue.asDouble());
        OD_LOG_P1("result <- ", result); //####
    }
    else if (inputValue.isDict())
    {
        OD_LOG("(inputValue.isDict())"); //####
        yarp::os::Property * value = inputValue.asDict();
        
        if (value)
        {
            OD_LOG("(value)"); //####
            yarp::os::Bottle asList(value->toString());
            
            result = convertDictionary(setHashFunction, asList);
            OD_LOG_P1("result <- ", result); //####
        }
    }
    else if (inputValue.isList())
    {
        OD_LOG("(inputValue.isList())"); //####
        yarp::os::Bottle * value = inputValue.asList();
        
        if (value)
        {
            OD_LOG("(value)"); //####
            yarp::os::Property asDict;
            
            if (ListIsReallyDictionary(*value, asDict))
            {
                OD_LOG("(ListIsReallyDictionary(*value, asDict))"); //####
                result = convertDictionary(setHashFunction, *value);
                OD_LOG_P1("result <- ", result); //####
            }
            else
            {
                OD_LOG("! (ListIsReallyDictionary(*value, asDict))"); //####
                result = convertList(setHashFunction, *value);
                OD_LOG_P1("result <- ", result); //####
            }
        }
    }
    else
    {
        OD_LOG("! (inputValue.isList())"); //####
        // We don't know what to do with this...
        result = ECL_NIL;
        OD_LOG_P1("result <- ", result); //####
    }
    OD_LOG_EXIT_P(result); //####
    return result;
} // convertValue

/*! @brief Create a Common Lisp structure with the contents of a bottle.
 @param setHashFunction The function object to use when setting a hash table entry.
 @param aBottle The bottle to be used.
 @returns The bottle as a Common Lisp structure. */
static cl_object createObjectFromBottle(cl_object                setHashFunction,
                                        const yarp::os::Bottle & aBottle)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("setHashFunction = ", setHashFunction, "aBottle = ", &aBottle); //####
    cl_object result;
    
//    cerr << "'" << aBottle.toString().c_str() << "'" << endl;
    result = convertList(setHashFunction, aBottle);
    OD_LOG_EXIT_P(result); //####
    return result;
} // createObjectFromBottle

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
    _loadedOutletDescriptions(loadedOutletDescriptions), _goAhead(0), _staller(1),
    _scriptStartingFunc(loadedStartingFunction), _scriptStoppingFunc(loadedStoppingFunction),
    _scriptThreadFunc(loadedThreadFunction), _hash2assocFunc(ECL_NIL), _setHashFunc(ECL_NIL),
    _threadInterval(loadedInterval), _mostRecentSlot(0), _isThreaded(sawThread)
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
    else if (0 < loadedInletHandlers.size())
    {
        setNeedsIdle();
    }
    try
    {
        // The following can't be directly expressed in C/C++, but is better described as Common
        // Lisp -
        cl_object form = c_string_to_object("(defun hash-to-assoc (aTable) "
                                            "(let (asList) "
                                            "(maphash #'(lambda (key val) "
                                            "(setq asList (acons key val asList))) aTable)"
                                            " asList))");
        
        _hash2assocFunc = cl_safe_eval(form, ECL_NIL, ECL_NIL);
        if (ECL_NIL == _hash2assocFunc)
        {
#if MAC_OR_LINUX_
            GetLogger().fail("Could not create 'hash-to-assoc' function.");
#else // ! MAC_OR_LINUX_
            cerr << "Could not create 'hash-to-assoc' function." << endl;
#endif // ! MAC_OR_LINUX_
        }
        // The following can't be directly expressed in C/C++, but is better described as Common
        // Lisp -
        form = c_string_to_object("(defun setHash (table key value) "
                                  "(setf (gethash key table) value))");
        
        _setHashFunc = cl_safe_eval(form, ECL_NIL, ECL_NIL);
        if (ECL_NIL == _setHashFunc)
        {
#if MAC_OR_LINUX_
            GetLogger().fail("Could not create 'setHash' function.");
#else // ! MAC_OR_LINUX_
            cerr << "Could not create 'setHash' function." << endl;
#endif // ! MAC_OR_LINUX_
        }
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

#include <odl/ODDisableLogging.h>
#include <odl/ODLogging.h>

DEFINE_DOIDLE_(CommonLispService)
{
    OD_LOG_OBJENTER(); //####
    if (isActive())
    {
        OD_LOG("(isActive())"); //####
        if (_goAhead.check())
        {
#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>
            OD_LOG("(_goAhead.check())"); //####
            if (ECL_NIL == _scriptThreadFunc)
            {
                OD_LOG("(ECL_NIL == _scriptThreadFunc)"); //####
                // We have a request from an input handler.
                if (_inletHandlers.size() > _mostRecentSlot)
                {
                    OD_LOG("(_inletHandlers.size() > _mostRecentSlot)"); //####
                    cl_object                handlerFunc = _inletHandlers[_mostRecentSlot];
                    CommonLispInputHandler * aHandler = _inHandlers.at(_mostRecentSlot);
                    
                    if (aHandler && (ECL_NIL != handlerFunc))
                    {
                        OD_LOG("(aHandler && (ECL_NIL != handlerFunc))"); //####
                        cl_object incoming = createObjectFromBottle(_setHashFunc,
                                                                    aHandler->getReceivedData());
                        
                        if (ECL_NIL != incoming)
                        {
                            OD_LOG("(ECL_NIL != incoming)"); //####
                            cl_env_ptr env = ecl_process_env();
                            cl_object  errorSymbol = ecl_make_symbol("ERROR", "CL");
                            
                            ECL_RESTART_CASE_BEGIN(env, ecl_list1(errorSymbol))
                            {
                                /* This form is evaluated with bound handlers. */
                                cl_funcall(3, handlerFunc, ecl_make_fixnum(_mostRecentSlot),
                                           incoming);
                            }
                            ECL_RESTART_CASE(1, condition)
                            {
                                /* This code is executed when an error happens. */
#if MAC_OR_LINUX_
                                GetLogger().fail("Input handler function failed.");
#else // ! MAC_OR_LINUX_
                                cerr << "Input handler function failed." << endl;
#endif // ! MAC_OR_LINUX_
                            }
                            ECL_RESTART_CASE_END;
                        }
                    }
                }
                _staller.post();
            }
            else
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
#include <odl/ODDisableLogging.h>
#include <odl/ODLogging.h>
        }
    }
    OD_LOG_OBJEXIT(); //####
} // CommonLispService::doIdle

#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

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
            OD_LOG("((0 < outBottle.size()) && outChannel)"); //####
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
            OD_LOG("! ((0 < outBottle.size()) && outChannel)"); //####
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
    _goAhead.post();
    OD_LOG_OBJEXIT(); //####
} // CommonLispService::signalRunFunction

void CommonLispService::stallUntilIdle(const size_t slotNumber)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_LL1("slotNumber = ", slotNumber); //####
    _staller.wait();
    _mostRecentSlot = slotNumber;
    OD_LOG_OBJEXIT(); //####
} // CommonLispService::stallUntilIdle

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
                    CommonLispInputHandler * aHandler = new CommonLispInputHandler(this, ii);
                    
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

cl_object CommonLisp::CreateBaseString(const char * inString,
                                       const size_t inLength)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S1("inString = ", inString); //####
    OD_LOG_LL1("inLength = ", inLength); //####
    cl_object result;
    
    if (inString)
    {
        result = ecl_alloc_simple_base_string(inLength);
        if (ECL_NIL != result)
        {
            strcpy(reinterpret_cast<char *>(result->base_string.self), inString);
        }
    }
    else
    {
        result = ECL_NIL;
    }
    OD_LOG_EXIT_P(result); //####
    return result;
} // CreateBaseString

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
