//--------------------------------------------------------------------------------------------------
//
//  File:       m+mCommonLispInputHandler.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for the input channel input handler used by the Common Lisp
//              input / output service.
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

#include "m+mCommonLispInputHandler.h"
#include "m+mCommonLispService.h"

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for the input channel input handler used by the Common Lisp
 input / output service. */
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
using std::endl;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

/*! @brief The name of the 'setHash' function. */
#define SETHASH_NAME_ "SETHASH"

#if defined(__APPLE__)
# pragma mark Global constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

/*! @brief Convert a YARP value into a Common Lisp object.
 @param setHashFunction The function object to use when setting a hash table entry.
 @param theData The output object.
 @param inputValue The value to be processed. */
static void convertValue(cl_object               setHashFunction,
                         cl_object &             theData,
                         const yarp::os::Value & inputValue);

/*! @brief Convert a YARP dictionary into a Common Lisp object.
 @param setHashFunction The function object to use when setting a hash table entry.
 @param theData The output object.
 @param inputAsList The input dictionary as a list. */
static void convertDictionary(cl_object                setHashFunction,
                              cl_object &              theData,
                              const yarp::os::Bottle & inputAsList)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P3("setHashFunction = ", setHashFunction, "theData = ", &theData, //####
              "inputAsList = ", &inputAsList); //####
    theData = cl_make_hash_table(0);
    for (int ii = 0, mm = inputAsList.size(); mm > ii; ++ii)
    {
        yarp::os::Value anEntry(inputAsList.get(ii));

        if (anEntry.isList())
        {
            yarp::os::Bottle * entryAsList = anEntry.asList();

            if (entryAsList && (2 == entryAsList->size()))
            {
                cl_object       anElement;
                YarpString      aKey(entryAsList->get(0).toString());
                yarp::os::Value aValue(entryAsList->get(1));
                cl_object       elementKey;

                convertValue(setHashFunction, anElement, aValue);
                elementKey = ecl_make_simple_base_string(const_cast<char *>(aKey.c_str()),
                                                         aKey.length());
                cl_env_ptr env = ecl_process_env();
                cl_object  errorSymbol = ecl_make_symbol("ERROR", "CL");

                ECL_RESTART_CASE_BEGIN(env, ecl_list1(errorSymbol))
                {
                    /* This form is evaluated with bound handlers. */
                    cl_funcall(4, setHashFunction, theData, elementKey, anElement);
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
    OD_LOG_EXIT(); //####
} // convertDictionary

/*! @brief Convert a YARP list into a Common Lisp object.
 @param setHashFunction The function object to use when setting a hash table entry.
 @param theData The output object.
 @param inputValue The value to be processed. */
static void convertList(cl_object                setHashFunction,
                        cl_object &              theData,
                        const yarp::os::Bottle & inputValue)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P3("setHashFunction = ", setHashFunction, "theData = ", &theData, "inputValue = ", //####
              &inputValue); //####
    theData = ecl_alloc_simple_vector(inputValue.size(), ecl_aet_object);
    for (int ii = 0, mm = inputValue.size(); mm > ii; ++ii)
    {
        cl_object       anElement;
        yarp::os::Value aValue(inputValue.get(ii));

        convertValue(setHashFunction, anElement, aValue);
        ecl_aset1(theData, ii, anElement);
    }
    OD_LOG_EXIT(); //####
} // convertList

static void convertValue(cl_object               setHashFunction,
                         cl_object &             theData,
                         const yarp::os::Value & inputValue)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P3("setHashFunction = ", setHashFunction, "theData = ", &theData, "inputValue = ", //####
              &inputValue); //####
    if (inputValue.isBool())
    {
        theData = ecl_make_fixnum(inputValue.asBool() ? 1 : 0);
    }
    else if (inputValue.isInt())
    {
        theData = ecl_make_fixnum(inputValue.asInt());
    }
    else if (inputValue.isString())
    {
        YarpString value = inputValue.asString();

        theData = ecl_make_simple_base_string(const_cast<char *>(value.c_str()), value.length());
    }
    else if (inputValue.isDouble())
    {
        theData = ecl_make_double_float(inputValue.asDouble());
    }
    else if (inputValue.isDict())
    {
        yarp::os::Property * value = inputValue.asDict();

        if (value)
        {
            yarp::os::Bottle asList(value->toString());

            convertDictionary(setHashFunction, theData, asList);
        }
    }
    else if (inputValue.isList())
    {
        yarp::os::Bottle * value = inputValue.asList();

        if (value)
        {
            yarp::os::Property asDict;

            if (ListIsReallyDictionary(*value, asDict))
            {
                convertDictionary(setHashFunction, theData, *value);
            }
            else
            {
                convertList(setHashFunction, theData, *value);
            }
        }
    }
    else
    {
        // We don't know what to do with this...
        theData = ECL_NIL;
    }
    OD_LOG_EXIT(); //####
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
    cl_object result = ECL_NIL;

//        cerr << "'" << aBottle.toString().c_str() << "'" << endl << endl;
    convertList(setHashFunction, result, aBottle);
    OD_LOG_EXIT_P(result); //####
    return result;
} // createObjectFromBottle

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and Destructors
#endif // defined(__APPLE__)

CommonLispInputHandler::CommonLispInputHandler(CommonLispService * owner,
                                               const size_t        slotNumber,
                                               cl_object           handlerFunc) :
    inherited(), _owner(owner), _handlerFunc(handlerFunc), _setHashFunc(ECL_NIL),
    _slotNumber(slotNumber), _active(false)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("owner = ", owner, "handlerFunc = ", handlerFunc); //####
    OD_LOG_L1("slotNumber = ", slotNumber); //####

    try
    {
        // The following can't be directly expressed in C/C++, but is better described as Common
        // Lisp -
        cl_object definition = c_string_to_object("((table key value) "
                                                  "(setf (gethash key table) value))");
        cl_object aName = cl_find_symbol(1,
                                     ecl_make_simple_base_string(const_cast<char *>(SETHASH_NAME_),
                                                                 sizeof(SETHASH_NAME_) - 1));

        _setHashFunc = si_make_lambda(aName, definition);
    }
    catch (...)
    {
    }

    OD_LOG_EXIT_P(this); //####
} // CommonLispInputHandler::CommonLispInputHandler

CommonLispInputHandler::~CommonLispInputHandler(void)
{
    OD_LOG_OBJENTER(); //####
    OD_LOG_OBJEXIT(); //####
} // CommonLispInputHandler::~CommonLispInputHandler

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
DEFINE_HANDLE_INPUT_(CommonLispInputHandler)
{
#if (! defined(OD_ENABLE_LOGGING_))
# if MAC_OR_LINUX_
#  pragma unused(senderChannel,replyMechanism)
# endif // MAC_OR_LINUX_
#endif // ! defined(OD_ENABLE_LOGGING_)
    OD_LOG_OBJENTER(); //####
    OD_LOG_S2s("senderChannel = ", senderChannel, "got ", input.toString()); //####
    OD_LOG_P1("replyMechanism = ", replyMechanism); //####
    OD_LOG_L1("numBytes = ", numBytes); //####
    bool result = true;
    
    try
    {
        if (_active && _owner && _handlerFunc)
        {
            // need to pass port number and incoming data to function; ignore result
            cl_object  incoming = createObjectFromBottle(_setHashFunc, input); //!!!!
            cl_env_ptr env = ecl_process_env();
            cl_object  errorSymbol = ecl_make_symbol("ERROR", "CL");

            ECL_RESTART_CASE_BEGIN(env, ecl_list1(errorSymbol))
            {
                /* This form is evaluated with bound handlers. */
                cl_funcall(1, _handlerFunc, ecl_make_fixnum(_slotNumber), incoming);
            }
            ECL_RESTART_CASE(1, condition)
            {
                /* This code is executed when an error happens. */
                result = false;
#if MAC_OR_LINUX_
                GetLogger().fail("Script aborted during load.");
#else // ! MAC_OR_LINUX_
                cerr << "Script aborted during load." << endl;
#endif // ! MAC_OR_LINUX_
            }
            ECL_RESTART_CASE_END;
        }
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
        throw;
    }
    OD_LOG_OBJEXIT_B(result); //####
    return result;
} // CommonLispInputHandler::handleInput
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
