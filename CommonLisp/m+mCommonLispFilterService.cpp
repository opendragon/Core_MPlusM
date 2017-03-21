//--------------------------------------------------------------------------------------------------
//
//  File:       m+mCommonLispFilterService.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for the CommonLisp filter service.
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

#include "m+mCommonLispFilterService.hpp"
#include "m+mCommonLispFilterInputHandler.hpp"
#include "m+mCommonLispFilterRequests.hpp"
#include "m+mCommonLispFilterThread.hpp"

#include <m+m/m+mEndpoint.hpp>

//#include <ODEnableLogging.h>
#include <ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for the %CommonLisp filter service. */
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
 @param[in,out] aBottle The bottle to be filled.
 @param[in] theData The value to be sent.
 @param[in] hashMapFunction The function to be applied to hash tables to get values that can be
 placed in a bottle.
 @param[in] topLevel @c true if this is the outermost list of an object. */
static void
fillBottleFromValue(yarp::os::Bottle & aBottle,
                    cl_object          theData,
                    cl_object          hashMapFunction,
                    const bool         topLevel)
{
    ODL_ENTER(); //####
    ODL_P3("aBottle = ", &aBottle, "theData = ", theData, "hashMapFunction = ", //####
           hashMapFunction); //####
    ODL_B1("topLevel = ", topLevel); //####
    if (ECL_NIL != cl_integerp(theData))
    {
        ODL_LOG("(ECL_NIL != cl_integerp(theData))"); //####
        aBottle.addInt(ecl_to_fixnum(theData));
    }
    else if (ECL_NIL != cl_realp(theData))
    {
        ODL_LOG("(ECL_NIL != cl_realp(theData))"); //####
        aBottle.addDouble(ecl_to_double(theData));
    }
    else if (ECL_NIL != cl_stringp(theData))
    {
        ODL_LOG("(ECL_NIL != cl_stringp(theData))"); //####
        cl_object aValue = si_coerce_to_base_string(theData);

        if (ECL_NIL == aValue)
        {
            ODL_LOG("(ECL_NIL == aValue)"); //####
            aBottle.addString("<unconvertible string>");
        }
        else
        {
            ODL_LOG("! (ECL_NIL == aValue)"); //####
            aBottle.addString(reinterpret_cast<char *>(aValue->base_string.self));
        }
    }
    else if (ECL_NIL != cl_symbolp(theData))
    {
        ODL_LOG("(ECL_NIL != cl_symbolp(theData))"); //####
        cl_object aName = cl_symbol_name(theData);

        if (ECL_NIL == aName)
        {
            ODL_LOG("(ECL_NIL == aName)"); //####
            aBottle.addString("<problematic symbol>");
        }
        else
        {
            ODL_LOG("! (ECL_NIL == aName)"); //####
            if (ECL_NIL == cl_stringp(aName))
            {
                ODL_LOG("(ECL_NIL == cl_stringp(aName))"); //####
                aName = cl_string(aName);
            }
            aName = si_coerce_to_base_string(aName);
            if (ECL_NIL == aName)
            {
                ODL_LOG("(ECL_NIL == aName)"); //####
                aBottle.addString("<unconvertible symbol>");
            }
            else
            {
                ODL_LOG("! (ECL_NIL == aName)"); //####
                aBottle.addString(reinterpret_cast<char *>(aName->base_string.self));
            }
        }
    }
    else if (ECL_NIL != cl_characterp(theData))
    {
        ODL_LOG("(ECL_NIL != cl_characterp(theData))"); //####
        cl_object asString = cl_string(theData);

        if (ECL_NIL == asString)
        {
            ODL_LOG("(ECL_NIL == asString)"); //####
            aBottle.addString("<unconvertible character>");
        }
        else
        {
            ODL_LOG("! (ECL_NIL == asString)"); //####
            aBottle.addString(reinterpret_cast<char *>(asString->base_string.self));
        }
    }
    else if (ECL_NIL != cl_hash_table_p(theData))
    {
        ODL_LOG("(ECL_NIL != cl_hash_table_p(theData))"); //####
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
            MpM_FAIL_("The 'hashMap' function failed.");
        }
        ECL_RESTART_CASE_END;
        if (ECL_NIL != aList)
        {
            ODL_LOG("(ECL_NIL != aList)"); //####
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
                        ODL_LOG("(ECL_NIL != aValue)"); //####
                        keyToUse = reinterpret_cast<char *>(aValue->base_string.self);
                    }
                }
                else if (ECL_NIL != cl_symbolp(aKey))
                {
                    ODL_LOG("(ECL_NIL != cl_symbolp(aKey))"); //####
                    cl_object aName = cl_symbol_name(theData);

                    if (ECL_NIL != aName)
                    {
                        ODL_LOG("(ECL_NIL != aName)"); //####
                        if (ECL_NIL == cl_stringp(aName))
                        {
                            ODL_LOG("(ECL_NIL == cl_stringp(aName))"); //####
                            aName = cl_string(aName);
                        }
                        aName = si_coerce_to_base_string(aName);
                        if (ECL_NIL != aName)
                        {
                            ODL_LOG("(ECL_NIL != aName)"); //####
                            keyToUse = reinterpret_cast<char *>(aName->base_string.self);
                        }
                    }
                }
                else if (ECL_NIL != cl_characterp(aKey))
                {
                    ODL_LOG("(ECL_NIL != cl_characterp(aKey))"); //####
                    cl_object asString = cl_string(aKey);

                    if (ECL_NIL != asString)
                    {
                        ODL_LOG("(ECL_NIL != asString)"); //####
                        keyToUse = reinterpret_cast<char *>(asString->base_string.self);
                    }
                }
                if (0 < keyToUse.length())
                {
                    ODL_LOG("(0 < keyToUse.length())"); //####
                    yarp::os::Bottle convertedResult;

                    fillBottleFromValue(convertedResult, aValue, hashMapFunction, false);
                    if (1 == convertedResult.size())
                    {
                        ODL_LOG("(1 == convertedResult.size())"); //####
                        yarp::os::Value anElement(convertedResult.get(0));

                        if (anElement.isInt())
                        {
                            ODL_LOG("(anElement.isInt())"); //####
                            innerDict.put(keyToUse, anElement.asInt());
                        }
                        else if (anElement.isDouble())
                        {
                            ODL_LOG("(anElement.isDouble())"); //####
                            innerDict.put(keyToUse, anElement.asDouble());
                        }
                        else if (anElement.isString())
                        {
                            ODL_LOG("(anElement.isString())"); //####
                            innerDict.put(keyToUse, anElement.asString());
                        }
                        else
                        {
                            ODL_LOG("! (anElement.isString())"); //####
                            innerDict.put(keyToUse, anElement);
                        }
                    }
                }
            }
        }
    }
    else if (ECL_NIL != cl_listp(theData))
    {
        ODL_LOG("(ECL_NIL != cl_listp(theData))"); //####
        for ( ; ECL_NIL != theData; theData = cl_cdr(theData))
        {
            cl_object anElement = cl_car(theData);

            if (ECL_NIL != anElement)
            {
                ODL_LOG("(ECL_NIL != anElement)"); //####
                fillBottleFromValue(aBottle, anElement, hashMapFunction, false);
            }
        }
    }
    else if (ECL_NIL != cl_arrayp(theData))
    {
        ODL_LOG("(ECL_NIL != cl_arrayp(theData))"); //####
        if (1 == ecl_fixnum(cl_array_rank(theData)))
        {
            ODL_LOG("(1 == ecl_fixnum(cl_array_rank(theData)))"); //####
            cl_fixnum numElements = ecl_fixnum(cl_array_dimension(theData, ecl_make_fixnum(0)));

            // Treat as a list
            ODL_LL1("numElements <- ", numElements); //####
            if (topLevel)
            {
                ODL_LOG("(topLevel)"); //####
                for (cl_fixnum ii = 0; numElements > ii; ++ii)
                {
                    cl_object anElement = cl_aref(2, theData, ecl_make_fixnum(ii));

                    if (ECL_NIL != anElement)
                    {
                        ODL_LOG("(ECL_NIL != anElement)"); //####
                        fillBottleFromValue(aBottle, anElement, hashMapFunction, false);
                    }
                }
            }
            else
            {
                ODL_LOG("! (topLevel)"); //####
                yarp::os::Bottle & innerList(aBottle.addList());

                for (cl_fixnum ii = 0; numElements > ii; ++ii)
                {
                    cl_object anElement = cl_aref(2, theData, ecl_make_fixnum(ii));

                    if (ECL_NIL != anElement)
                    {
                        ODL_LOG("(ECL_NIL != anElement)"); //####
                        fillBottleFromValue(innerList, anElement, hashMapFunction, false);
                    }
                }
            }
        }
    }
    else
    {
        ODL_LOG("! (ECL_NIL != cl_arrayp(theData))"); //####
        aBottle.addString("<untranslatable>");
    }
    ODL_EXIT(); //####
} // fillBottleFromValue

/*! @brief Convert a YARP value into a Common Lisp object.
 @param[in] setHashFunction The function object to use when setting a hash table entry.
 @param[in] inputValue The value to be processed.
 @returns The output object. */
static cl_object
convertValue(cl_object               setHashFunction,
             const yarp::os::Value & inputValue);

/*! @brief Convert a YARP dictionary into a Common Lisp object.
 @param[in] setHashFunction The function object to use when setting a hash table entry.
 @param[in] inputAsList The input dictionary as a list.
 @returns The output object. */
static cl_object
convertDictionary(cl_object                setHashFunction,
                  const yarp::os::Bottle & inputAsList)
{
    ODL_ENTER(); //####
    ODL_P2("setHashFunction = ", setHashFunction, "inputAsList = ", &inputAsList); //####
    cl_object result = cl_make_hash_table(0);
    ODL_P1("result <- ", result); //####

    for (int ii = 0, mm = inputAsList.size(); mm > ii; ++ii)
    {
        yarp::os::Value anEntry(inputAsList.get(ii));

        if (anEntry.isList())
        {
            ODL_LOG("(anEntry.isList())"); //####
            yarp::os::Bottle * entryAsList = anEntry.asList();

            if (entryAsList && (2 == entryAsList->size()))
            {
                ODL_LOG("(entryAsList && (2 == entryAsList->size()))"); //####
                YarpString      aKey(entryAsList->get(0).toString());
                yarp::os::Value aValue(entryAsList->get(1));
                cl_object       anElement = convertValue(setHashFunction, aValue);

                ODL_P1("anElement <- ", anElement); //####
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
                    MpM_FAIL_("Function 'setHash' failed.");
                }
                ECL_RESTART_CASE_END;
            }
        }
    }
    ODL_EXIT_P(result); //####
    return result;
} // convertDictionary

/*! @brief Convert a YARP list into a Common Lisp object.
 @param[in] setHashFunction The function object to use when setting a hash table entry.
 @param[in] inputValue The value to be processed.
 @returns The result object. */
static cl_object
convertList(cl_object                setHashFunction,
            const yarp::os::Bottle & inputValue)
{
    ODL_ENTER(); //####
    ODL_P2("setHashFunction = ", setHashFunction, "inputValue = ", &inputValue); //####
    cl_object result = ecl_alloc_simple_vector(inputValue.size(), ecl_aet_object);

    ODL_P1("result <- ", result); //####
    for (int ii = 0, mm = inputValue.size(); mm > ii; ++ii)
    {
        yarp::os::Value aValue(inputValue.get(ii));
        cl_object       anElement = convertValue(setHashFunction, aValue);
        ODL_P1("anElement <- ", anElement); //####

        ecl_aset1(result, ii, anElement);
    }
    ODL_EXIT_P(result); //####
    return result;
} // convertList

static cl_object
convertValue(cl_object               setHashFunction,
             const yarp::os::Value & inputValue)
{
    ODL_ENTER(); //####
    ODL_P2("setHashFunction = ", setHashFunction, "inputValue = ", &inputValue); //####
    cl_object result = ECL_NIL;

    if (inputValue.isBool())
    {
        ODL_LOG("(inputValue.isBool())"); //####
        result = ecl_make_fixnum(inputValue.asBool() ? 1 : 0);
        ODL_P1("result <- ", result); //####
    }
    else if (inputValue.isInt())
    {
        ODL_LOG("(inputValue.isInt())"); //####
        result = ecl_make_fixnum(inputValue.asInt());
        ODL_P1("result <- ", result); //####
    }
    else if (inputValue.isString())
    {
        ODL_LOG("(inputValue.isString())"); //####
        YarpString value = inputValue.asString();

        result = CreateBaseString(value.c_str(), value.length());
        ODL_P1("result <- ", result); //####
    }
    else if (inputValue.isDouble())
    {
        ODL_LOG("(inputValue.isDouble())"); //####
        result = ecl_make_double_float(inputValue.asDouble());
        ODL_P1("result <- ", result); //####
    }
    else if (inputValue.isDict())
    {
        ODL_LOG("(inputValue.isDict())"); //####
        yarp::os::Property * value = inputValue.asDict();

        if (value)
        {
            ODL_LOG("(value)"); //####
            yarp::os::Bottle asList(value->toString());

            result = convertDictionary(setHashFunction, asList);
            ODL_P1("result <- ", result); //####
        }
    }
    else if (inputValue.isList())
    {
        ODL_LOG("(inputValue.isList())"); //####
        yarp::os::Bottle * value = inputValue.asList();

        if (value)
        {
            ODL_LOG("(value)"); //####
            yarp::os::Property asDict;

            if (ListIsReallyDictionary(*value, asDict))
            {
                ODL_LOG("(ListIsReallyDictionary(*value, asDict))"); //####
                result = convertDictionary(setHashFunction, *value);
                ODL_P1("result <- ", result); //####
            }
            else
            {
                ODL_LOG("! (ListIsReallyDictionary(*value, asDict))"); //####
                result = convertList(setHashFunction, *value);
                ODL_P1("result <- ", result); //####
            }
        }
    }
    else
    {
        ODL_LOG("! (inputValue.isList())"); //####
        // We don't know what to do with this...
        result = ECL_NIL;
        ODL_P1("result <- ", result); //####
    }
    ODL_EXIT_P(result); //####
    return result;
} // convertValue

/*! @brief Create a Common Lisp structure with the contents of a bottle.
 @param[in] setHashFunction The function object to use when setting a hash table entry.
 @param[in] aBottle The bottle to be used.
 @returns The bottle as a Common Lisp structure. */
static cl_object
createObjectFromBottle(cl_object                setHashFunction,
                       const yarp::os::Bottle & aBottle)
{
    ODL_ENTER(); //####
    ODL_P2("setHashFunction = ", setHashFunction, "aBottle = ", &aBottle); //####
    cl_object result;

//    cerr << "'" << aBottle.toString().c_str() << "'" << endl;
    result = convertList(setHashFunction, aBottle);
    ODL_EXIT_P(result); //####
    return result;
} // createObjectFromBottle

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and Destructors
#endif // defined(__APPLE__)

CommonLispFilterService::CommonLispFilterService(const Utilities::DescriptorVector & argumentList,
                                                 const YarpString &                  launchPath,
                                                 const int                           argc,
                                                 char * *                            argv,
                                                 const YarpString &                  tag,
                                                 const YarpString &                  description,
                                                 const Common::ChannelVector &
                                                                            loadedInletDescriptions,
                                                 const Common::ChannelVector &
                                                                        loadedOutletDescriptions,
                                                 const ObjectVector &
                                                                                loadedInletHandlers,
                                                 cl_object
                                                                            loadedStartingFunction,
                                                 cl_object
                                                                            loadedStoppingFunction,
                                                 const bool                          sawThread,
                                                 cl_object
                                                                            loadedThreadFunction,
                                                 const double                        loadedInterval,
                                                 const YarpString &
                                                                                serviceEndpointName,
                                                 const YarpString &
                                                                                servicePortNumber) :
    inherited(argumentList, launchPath, argc, argv, tag, true, MpM_COMMONLISPFILTER_CANONICAL_NAME_,
              description, "", serviceEndpointName, servicePortNumber),
    _inletHandlers(loadedInletHandlers), _inHandlers(), _generator(NULL),
    _loadedInletDescriptions(loadedInletDescriptions),
    _loadedOutletDescriptions(loadedOutletDescriptions), _goAhead(0), _staller(1),
    _scriptStartingFunc(loadedStartingFunction), _scriptStoppingFunc(loadedStoppingFunction),
    _scriptThreadFunc(loadedThreadFunction), _hash2assocFunc(ECL_NIL), _setHashFunc(ECL_NIL),
    _threadInterval(loadedInterval), _mostRecentSlot(0), _isThreaded(sawThread)
{
    ODL_ENTER(); //####
    ODL_P4("argumentList = ", &argumentList, "argv = ", argv, //####
           "loadedInletDescriptions = ", &loadedInletDescriptions, //####
           "loadedOutletDescriptions = ", &loadedOutletDescriptions); //####
    ODL_P4("loadedInletHandlers = ", &loadedInletHandlers, "loadedStartingFunction = ", //####
           loadedStartingFunction, "loadedStoppingFunction = ", loadedStoppingFunction, //####
           "loadedThreadFunction = ", loadedThreadFunction); //####
    ODL_LL1("argc = ", argc); //####
    ODL_S4s("launchPath = ", launchPath, "tag = ", tag, "description = ", description, //####
            "serviceEndpointName = ", serviceEndpointName); //####
    ODL_S1s("servicePortNumber = ", servicePortNumber); //####
    ODL_B1("sawThread = ", sawThread); //####
    ODL_D1("loadedInterval = ", loadedInterval); //####
    if (_isThreaded && (ECL_NIL != _scriptThreadFunc))
    {
        ODL_LOG("(_isThreaded && (ECL_NIL != _scriptThreadFunc))"); //####
        setNeedsIdle();
    }
    else if (0 < loadedInletHandlers.size())
    {
        ODL_LOG("(0 < loadedInletHandlers.size())"); //####
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
            MpM_FAIL_("Could not create 'hash-to-assoc' function.");
        }
        // The following can't be directly expressed in C/C++, but is better described as Common
        // Lisp -
        form = c_string_to_object("(defun setHash (table key value) "
                                  "(setf (gethash key table) value))");

        _setHashFunc = cl_safe_eval(form, ECL_NIL, ECL_NIL);
        if (ECL_NIL == _setHashFunc)
        {
            MpM_FAIL_("Could not create 'setHash' function.");
        }
    }
    catch (...)
    {
    }
    ODL_EXIT_P(this); //####
} // CommonLispFilterService::CommonLispFilterService

CommonLispFilterService::~CommonLispFilterService(void)
{
    ODL_OBJENTER(); //####
    stopStreams();
    releaseHandlers();
    ODL_OBJEXIT(); //####
} // CommonLispFilterService::~CommonLispFilterService

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
bool
CommonLispFilterService::configure(const yarp::os::Bottle & details)
{
#if (! defined(MpM_DoExplicitDisconnect))
# if MAC_OR_LINUX_
#  pragma unused(details)
# endif // MAC_OR_LINUX_
#endif // ! defined(MpM_DoExplicitDisconnect)
    ODL_OBJENTER(); //####
    ODL_P1("details = ", &details); //####
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
                MpM_FAIL_("Script aborted during load.");
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
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT_B(result); //####
    return result;
} // CommonLispFilterService::configure
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

void
CommonLispFilterService::disableMetrics(void)
{
    ODL_OBJENTER(); //####
    inherited::disableMetrics();
    for (HandlerVector::const_iterator walker(_inHandlers.begin()); _inHandlers.end() != walker;
         ++walker)
    {
        CommonLispFilterInputHandler * aHandler = *walker;

        if (aHandler)
        {
            aHandler->disableMetrics();
        }
    }
    ODL_OBJEXIT(); //####
} // CommonLispFilterService::disableMetrics

void
CommonLispFilterService::doIdle(void)
{
    ODL_OBJENTER(); //####
    if (isActive())
    {
        ODL_LOG("(isActive())"); //####
        if (_goAhead.check())
        {
            ODL_LOG("(_goAhead.check())"); //####
            if (ECL_NIL == _scriptThreadFunc)
            {
                ODL_LOG("(ECL_NIL == _scriptThreadFunc)"); //####
                // We have a request from an input handler.
                if (_inletHandlers.size() > _mostRecentSlot)
                {
                    ODL_LOG("(_inletHandlers.size() > _mostRecentSlot)"); //####
                    cl_object                      handlerFunc = _inletHandlers[_mostRecentSlot];
                    CommonLispFilterInputHandler * aHandler = _inHandlers.at(_mostRecentSlot);

                    if (aHandler && (ECL_NIL != handlerFunc))
                    {
                        ODL_LOG("(aHandler && (ECL_NIL != handlerFunc))"); //####
                        cl_object incoming = createObjectFromBottle(_setHashFunc,
                                                                    aHandler->getReceivedData());

                        if (ECL_NIL != incoming)
                        {
                            ODL_LOG("(ECL_NIL != incoming)"); //####
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
                                MpM_FAIL_("Input handler function failed.");
                            }
                            ECL_RESTART_CASE_END;
                        }
                    }
                }
                _staller.post();
            }
            else
            {
                ODL_LOG("(ECL_NIL != _scriptThreadFunc)"); //####
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
                        MpM_FAIL_("Script aborted during load.");
                    }
                    ECL_RESTART_CASE_END;
                }
                catch (...)
                {
                    ODL_LOG("Exception caught"); //####
                    throw;
                }
            }
        }
    }
    ODL_OBJEXIT(); //####
} // CommonLispFilterService::doIdle

void
CommonLispFilterService::enableMetrics(void)
{
    ODL_OBJENTER(); //####
    inherited::enableMetrics();
    for (HandlerVector::const_iterator walker(_inHandlers.begin()); _inHandlers.end() != walker;
         ++walker)
    {
        CommonLispFilterInputHandler * aHandler = *walker;

        if (aHandler)
        {
            aHandler->enableMetrics();
        }
    }
    ODL_OBJEXIT(); //####
} // CommonLispFilterService::enableMetrics

void
CommonLispFilterService::releaseHandlers(void)
{
    ODL_OBJENTER(); //####
    if (0 < _inHandlers.size())
    {
        for (HandlerVector::const_iterator walker(_inHandlers.begin()); _inHandlers.end() != walker;
             ++walker)
        {
            CommonLispFilterInputHandler * aHandler = *walker;

            if (aHandler)
            {
                ODL_P1("aHandler = ", aHandler); //####
                delete aHandler;
            }
        }
        _inHandlers.clear();
    }
    ODL_OBJEXIT(); //####
} // CommonLispFilterService::releaseHandlers

bool
CommonLispFilterService::sendToChannel(const cl_fixnum channelSlot,
                                       cl_object       theData)
{
    ODL_OBJENTER();
    ODL_LL1("channelSlot = ", channelSlot); //####
    bool okSoFar = false;

    if ((0 <= channelSlot) && (channelSlot < static_cast<cl_fixnum>(getOutletCount())))
    {
        Common::GeneralChannel * outChannel = getOutletStream(channelSlot);
        yarp::os::Bottle         outBottle;

        fillBottleFromValue(outBottle, theData, _hash2assocFunc, true);
        if ((0 < outBottle.size()) && outChannel)
        {
            ODL_LOG("((0 < outBottle.size()) && outChannel)"); //####
            if (outChannel->writeBottle(outBottle))
            {
                okSoFar = true;
            }
            else
            {
                ODL_LOG("! (outChannel->writeBottle(message))"); //####
#if defined(MpM_StallOnSendProblem)
                Stall();
#endif // defined(MpM_StallOnSendProblem)
            }
        }
        else
        {
            ODL_LOG("! ((0 < outBottle.size()) && outChannel)"); //####
            // If there's nothing to write, or the channel is gone, continue as if everything is
            // fine.
            okSoFar = true;
        }
    }
    ODL_OBJEXIT_B(okSoFar); //####
    return okSoFar;
} // CommonLispFilterService::sendToChannel

bool
CommonLispFilterService::setUpStreamDescriptions(void)
{
    ODL_OBJENTER(); //####
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
    ODL_OBJEXIT_B(result); //####
    return result;
} // CommonLispFilterService::setUpStreamDescriptions

void
CommonLispFilterService::signalRunFunction(void)
{
    ODL_OBJENTER(); //####
    _goAhead.post();
    ODL_OBJEXIT(); //####
} // CommonLispFilterService::signalRunFunction

void
CommonLispFilterService::stallUntilIdle(const size_t slotNumber)
{
    ODL_OBJENTER(); //####
    ODL_LL1("slotNumber = ", slotNumber); //####
    _staller.wait();
    _mostRecentSlot = slotNumber;
    ODL_OBJEXIT(); //####
} // CommonLispFilterService::stallUntilIdle

void
CommonLispFilterService::startStreams(void)
{
    ODL_OBJENTER(); //####
    try
    {
        if (! isActive())
        {
            if (_isThreaded)
            {
                _generator = new CommonLispFilterThread(*this, _threadInterval);
                if (! _generator->start())
                {
                    ODL_LOG("(! _generator->start())"); //####
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
                    cl_object                      handlerFunc = _inletHandlers[ii];
                    CommonLispFilterInputHandler * aHandler = new CommonLispFilterInputHandler(this,
                                                                                               ii);

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
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT(); //####
} // CommonLispFilterService::startStreams

void
CommonLispFilterService::stopStreams(void)
{
    ODL_OBJENTER(); //####
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
                        yarp::os::Time::delay(_threadInterval / IO_SERVICE_DELAY_FACTOR_);
                    }
                    delete _generator;
                    _generator = NULL;
                }
            }
            else
            {
                for (size_t ii = 0, mm = getInletCount(); mm > ii; ++ii)
                {
                    CommonLispFilterInputHandler * aHandler = _inHandlers.at(ii);

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
                    MpM_FAIL_("Script aborted during load.");
                }
                ECL_RESTART_CASE_END;
            }
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT(); //####
} // CommonLispFilterService::stopStreams

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

cl_object
CommonLisp::CreateBaseString(const char * inString,
                             const size_t inLength)
{
    ODL_ENTER(); //####
    ODL_S1("inString = ", inString); //####
    ODL_LL1("inLength = ", inLength); //####
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
    ODL_EXIT_P(result); //####
    return result;
} // CreateBaseString
