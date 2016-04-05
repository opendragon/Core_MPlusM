//--------------------------------------------------------------------------------------------------
//
//  File:       m+mCommonLispFilterServiceMain.cpp
//
//  Project:    m+m
//
//  Contains:   The main application for the CommonLisp filter service.
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

#include "m+mCommonLispFilterService.h"

#include <m+m/m+mEndpoint.h>
#include <m+m/m+mExtraArgumentDescriptor.h>
#include <m+m/m+mFilePathArgumentDescriptor.h>
#include <m+m/m+mUtilities.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The main application for the %CommonLisp filter service. */

/*! @dir CommonLisp
 @brief The set of files that implement the %CommonLisp filter service. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::CommonLisp;
using std::cin;
using std::cout;
using std::endl;
using std::cerr;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

/*! @brief The name of the 'argv' object. */
#define ARGV_NAME_ "ARGV"

/*! @brief The name of the Common Lisp function to create an inlet entry. */
#define CREATE_INLET_ENTRY_NAME_ "CREATE-INLET-ENTRY"

/*! @brief The name of the Common Lisp function to create an outlet entry. */
#define CREATE_OUTLET_ENTRY_NAME_ "CREATE-OUTLET-ENTRY"

/*! @brief A macro to create a DEFUN abstraction in C++.

 Credit: https://gist.github.com/vwood/662109
 @param name The string name for the function.
 @param fun A pointer to the implementing C++ function.
 @param args The number of arguments to the function. */
#define DEFUN_(name,fun,args) \
    cl_def_c_function(c_string_to_object(name), (cl_objectfn_fixed) fun, args)

/*! @brief The name of the 'handler' field. */
#define HANDLER_NAME_ "HANDLER"

/*! @brief The name of the 'name' field. */
#define NAME_NAME_ "NAME"

/*! @brief The name of the 'protocol' field. */
#define PROTOCOL_NAME_ "PROTOCOL"

/*! @brief The name of the 'protocolDescription' field. */
#define PROTOCOLDESCRIPTION_NAME_ "PROTOCOLDESCRIPTION"

/*! @brief The name of the 'scriptTag' object. */
#define SCRIPTTAG_NAME_ "SCRIPTTAG"

/*! @brief A pointer to the active service, for use in callbacks. */
static CommonLispFilterService * lActiveService = NULL;

#if defined(__APPLE__)
# pragma mark Global constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

/*! @brief A C-callback function for Common Lisp to get the current time in seconds. */
static cl_object
getTimeNowForCl(void)
{
    ODL_ENTER(); //####
    cl_env_ptr env = ecl_process_env();
    cl_object  result = ecl_make_double_float(yarp::os::Time::now());

    ODL_EXIT_P(result); //####
    ecl_return1(env, result);
} // getTimeNowForCl

/*! @brief A C-callback function for Common Lisp to stop the service. */
static cl_object
requestStopForCl(void)
{
    ODL_ENTER(); //####
    cl_env_ptr env = ecl_process_env();

    if (lActiveService)
    {
        lActiveService->requestServiceStop();
    }
    ODL_EXIT_P(ECL_NIL); //####
    ecl_return0(env);
} // requestStopForCl

/*! @brief A C-callback function for Common Lisp to send an object to a channel.
 @param channelIndex The number of the channel to be used.
 @param message The message to send to the channel. */
static cl_object
sendToChannelForCl(cl_object channelIndex,
                   cl_object message)
{
    ODL_ENTER(); //####
    ODL_P2("channelIndex = ", channelIndex, "message = ", message); //####
    cl_env_ptr env = ecl_process_env();

    if (lActiveService && (ECL_NIL != cl_integerp(channelIndex)))
    {
        lActiveService->sendToChannel(ecl_to_fixnum(channelIndex), message);
    }
    ODL_EXIT_P(ECL_NIL); //####
    ecl_return0(env);
} // sendToChannelForCl

/*! @brief Add custom functions to the Common Lisp environment.
 @returns @c true if the custom functions were addeded successfully and @c false otherwise. */
static void
addCustomFunctions(void)
{
    ODL_ENTER(); //####
    DEFUN_("getTimeNow", getTimeNowForCl, 0);
    DEFUN_("requestStop", requestStopForCl, 0);
    DEFUN_("sendToChannel", sendToChannelForCl, 2);
    // The following can't be directly expressed in C/C++, but is better described as Common
    // Lisp -
    cl_object form = c_string_to_object("(defun create-inlet-entry "
                                        "(name protocol protocolDescription handler) "
                                        "(let ((entry (make-hash-table))) "
                                        "(psetf (gethash 'name entry) name "
                                        "(gethash 'protocol entry) protocol "
                                        "(gethash 'protocolDescription entry) protocolDescription "
                                        "(gethash 'handler entry) handler) entry))");
    cl_object aFunction = cl_safe_eval(form, ECL_NIL, ECL_NIL);

    if (ECL_NIL == aFunction)
    {
        MpM_FAIL_("Could not create 'create-inlet-entry' function.");
    }
    form = c_string_to_object("(defun create-outlet-entry (name protocol protocolDescription) "
                              "(let ((entry (make-hash-table))) "
                              "(psetf (gethash 'name entry) name "
                              "(gethash 'protocol entry) protocol "
                              "(gethash 'protocolDescription entry) protocolDescription) entry))");
    aFunction = cl_safe_eval(form, ECL_NIL, ECL_NIL);
    if (ECL_NIL == aFunction)
    {
        MpM_FAIL_("Could not create 'create-outlet-entry' function.");
    }
    ODL_EXIT(); //####
} // addCustomFunctions

/*! @brief Add custom classes to the Common Lisp environment.
 @param ourPackage The package to be used with the new classes.
 @returns @c true if the custom classes were addeded successfully and @c false otherwise. */
static bool
addCustomClasses(cl_object ourPackage)
{
    ODL_ENTER(); //####
    ODL_P1("ourPackage = ", ourPackage); //####
    bool okSoFar = true;

    ODL_EXIT_B(okSoFar); //####
    return okSoFar;
} // addCustomClasses

/*! @brief Add an array containing the command-line arguments to the Common Lisp environment.
 @param ourPackage The package to be used with the new object.
 @param argv The arguments to be used with the %CommonLisp filter service. */
static void
addArgvObject(cl_object                ourPackage,
              const YarpStringVector & argv)
{
    ODL_ENTER(); //####
    ODL_P2("ourPackage = ", ourPackage, "argv = ", &argv); //####
    cl_env_ptr env = ecl_process_env();
    cl_object  argvObject = cl_intern(2, CreateBaseString(ARGV_NAME_, sizeof(ARGV_NAME_) - 1),
                                      ourPackage);
    cl_object  argvValue = ecl_alloc_simple_vector(argv.size(), ecl_aet_object);

    cl_export(2, argvObject, ourPackage);
    ecl_setq(env, argvObject, argvValue);
    for (size_t ii = 0, argc = argv.size(); argc > ii; ++ii)
    {
        ecl_aset1(argvValue, ii, CreateBaseString(argv[ii].c_str(), argv[ii].length()));
    }
    ODL_EXIT(); //####
} // addArgvObject

/*! @brief Add a custom string object to the Common Lisp environment.
 @param ourPackage The package to be used with the new object.
 @param tag The modifier for the service name and port names. */
static void
addScriptTagObject(cl_object          ourPackage,
                   const YarpString & tag)
{
    ODL_ENTER(); //####
    ODL_P1("ourPackage = ", ourPackage); //####
    ODL_S1s("tag = ", tag); //####
    cl_env_ptr env = ecl_process_env();
    cl_object  scriptTagObject = cl_intern(2, CreateBaseString(SCRIPTTAG_NAME_,
                                                               sizeof(SCRIPTTAG_NAME_) - 1),
                                           ourPackage);

    cl_export(2, scriptTagObject, ourPackage);
    ecl_setq(env, scriptTagObject, CreateBaseString(tag.c_str(), tag.length()));
    ODL_EXIT(); //####
} // addScriptTagObject

/*! @brief Add custom classes, functions and variables to the Common Lisp environment.
 @param tag The modifier for the service name and port names.
 @param argv The arguments to be used with the %CommonLisp filter service. */
static cl_object
addCustomObjects(const YarpString &       tag,
                 const YarpStringVector & argv)
{
    ODL_ENTER(); //####
    ODL_P1("argv = ", &argv); //####
    ODL_S1s("tag = ", tag); //####
    cl_object ourPackage = cl_make_package(5, c_string_to_object(MpM_COMMONLISP_PACKAGE_NAME_),
                                           c_string_to_object(":nicknames"),
                                           cl_list(1,
                                               c_string_to_object(MpM_COMMONLISP_PACKAGE_ABBREV_)),
                                           c_string_to_object(":use"),
                                           cl_list(1, c_string_to_object(":common-lisp")));

    addCustomFunctions();
    addCustomClasses(ourPackage);
    addArgvObject(ourPackage, argv);
    addScriptTagObject(ourPackage, tag);
    ODL_EXIT_P(ourPackage); //####
    return ourPackage;
} // addCustomObjects

/*! @brief Check the arity of a function.
 @param objectFunction The function to be checked.
 @param arityRequired The required arity for the function.
 @returns @c true if the function has the required arity or @c false otherwise. */
static bool
checkArity(cl_object      objectFunction,
           const uint32_t arityRequired)
{
    ODL_ENTER(); //####
    ODL_P1("objectFunction = ", objectFunction); //####
    ODL_LL1("arityRequired = ", arityRequired); //####
    bool      okSoFar;
    cl_object lambdaExpr = cl_function_lambda_expression(objectFunction);

    if (ECL_NIL == lambdaExpr)
    {
        okSoFar = true;
    }
    else
    {
        // The lambda expression is in the form (EXT:LAMBDA-BLOCK functionName functioArgs ...)
        cl_object argList = cl_caddr(lambdaExpr);

        if (ECL_NIL == argList)
        {
            okSoFar = (0 == arityRequired);
        }
        else
        {
            argList = cl_list_length(argList);
            okSoFar = (ecl_fixnum(argList) == arityRequired);
        }
    }
    ODL_EXIT_B(okSoFar);
    return okSoFar;
} // checkArity

/*! @brief Check an object for a specific numeric property.
 @param propertyName The name of the property being searched for.
 @param canBeFunction @c true if the property can be a function rather than a string and @c false if
 the property must be a string.
 @param isOptional @c true if the property does not have to be present.
 @param result The value of the number, if located.
 @returns @c true on success and @c false otherwise. */
static bool
getLoadedDouble(const char * propertyName,
                const bool   canBeFunction,
                const bool   isOptional,
                double &     result)
{
    ODL_ENTER(); //####
    ODL_S1("propertyName = ", propertyName); //####
    ODL_B2("canBeFunction = ", canBeFunction, "isOptional = ", isOptional); //####
    ODL_P1("result = ", &result); //####
    bool okSoFar = false;

    try
    {
        cl_object aSymbol = cl_find_symbol(1, CreateBaseString(propertyName, strlen(propertyName)));

        if (ECL_NIL != aSymbol)
        {
            if (ECL_NIL != cl_boundp(aSymbol))
            {
                cl_object aValue = cl_symbol_value(aSymbol);

                if (ECL_NIL != cl_realp(aValue))
                {
                    result = ecl_to_double(aValue);
                    okSoFar = true;
                }
            }
            else if (canBeFunction && (ECL_NIL != cl_fboundp(aSymbol)))
            {
                cl_object aFunction = cl_symbol_function(aSymbol);

                if (ECL_NIL != aFunction)
                {
                    if (checkArity(aFunction, 0))
                    {
                        cl_env_ptr env = ecl_process_env();
                        cl_object  aValue;
                        cl_object  errorSymbol = ecl_make_symbol("ERROR", "CL");

                        ECL_RESTART_CASE_BEGIN(env, ecl_list1(errorSymbol))
                        {
                            /* This form is evaluated with bound handlers. */
                            aValue = cl_funcall(1, aFunction);
                        }
                        ECL_RESTART_CASE(1, condition)
                        {
                            /* This code is executed when an error happens. */
                            aValue = ECL_NIL;
                            MpM_FAIL_("Script aborted during load.");
                        }
                        ECL_RESTART_CASE_END;
                        if (ECL_NIL != aValue)
                        {
                            if (ECL_NIL != cl_realp(aValue))
                            {
                                result = ecl_to_double(aValue);
                                okSoFar = true;
                            }
                        }
                    }
                    else
                    {
                        YarpString message("Function (");

                        message += propertyName;
                        message += ") has the incorrect number of arguments.";
                        MpM_FAIL_(message.c_str());
                    }
                }
            }
            else if (isOptional)
            {
                okSoFar = true;
            }
            else
            {
                MpM_FAIL_("Problem searching for a property.");
            }
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_EXIT_B(okSoFar); //####
    return okSoFar;
} // getLoadedDouble

/*! @brief Check  script for a specific string property.
 @param propertyName The name of the property being searched for.
 @param canBeFunction @c true if the property can be a function rather than a string and @c false if
 the property must be a string.
 @param isOptional @c true if the property does not have to be present.
 @param result The value of the string, if located.
 @returns @c true on success and @c false otherwise. */
static bool
getLoadedString(const char * propertyName,
                const bool   canBeFunction,
                const bool   isOptional,
                YarpString & result)
{
    ODL_ENTER(); //####
    ODL_S1("propertyName = ", propertyName); //####
    ODL_B2("canBeFunction = ", canBeFunction, "isOptional = ", isOptional); //####
    ODL_P1("result = ", &result); //####
    bool okSoFar = false;

    try
    {
        cl_object aSymbol = cl_find_symbol(1, CreateBaseString(propertyName, strlen(propertyName)));

        if (ECL_NIL == aSymbol)
        {
            if (isOptional)
            {
                result = "";
                okSoFar = true;
            }
        }
        else
        {
            if (ECL_NIL != cl_boundp(aSymbol))
            {
                cl_object aValue = cl_symbol_value(aSymbol);

                if (ECL_NIL == cl_stringp(aValue))
                {
                    aValue = cl_string(aValue);
                }
                aValue = si_coerce_to_base_string(aValue);
                if (ECL_NIL != aValue)
                {
                    result = reinterpret_cast<char *>(aValue->base_string.self);
                    okSoFar = true;
                }
            }
            else if (canBeFunction && (ECL_NIL != cl_fboundp(aSymbol)))
            {
                cl_object aFunction = cl_symbol_function(aSymbol);

                if (ECL_NIL != aFunction)
                {
                    if (checkArity(aFunction, 0))
                    {
                        cl_env_ptr env = ecl_process_env();
                        cl_object  aValue;
                        cl_object  errorSymbol = ecl_make_symbol("ERROR", "CL");

                        ECL_RESTART_CASE_BEGIN(env, ecl_list1(errorSymbol))
                        {
                            /* This form is evaluated with bound handlers. */
                            aValue = cl_funcall(1, aFunction);
                        }
                        ECL_RESTART_CASE(1, condition)
                        {
                            /* This code is executed when an error happens. */
                            aValue = ECL_NIL;
                            MpM_FAIL_("Script aborted during load.");
                        }
                        ECL_RESTART_CASE_END;
                        if (ECL_NIL != aValue)
                        {
                            if (ECL_NIL == cl_stringp(aValue))
                            {
                                aValue = cl_string(aValue);
                            }
                            aValue = si_coerce_to_base_string(aValue);
                            if (ECL_NIL != aValue)
                            {
                                result = reinterpret_cast<char *>(aValue->base_string.self);
                                okSoFar = true;
                            }
                        }
                    }
                    else
                    {
                        YarpString message("Function (");

                        message += propertyName;
                        message += ") has the incorrect number of arguments.";
                        MpM_FAIL_(message.c_str());
                    }
                }
            }
            else if (isOptional)
            {
                okSoFar = true;
            }
            else
            {
                MpM_FAIL_("Problem searching for a property.");
            }
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_EXIT_B(okSoFar); //####
    return okSoFar;
} // getLoadedString

/*! @brief Check a script for a specific function property.
 @param propertyName The name of the property being searched for.
 @param arity The expected number of arguments for the function.
 @param isOptional @c true if the property does not have to be present.
 @param result The value of the string, if located.
 @returns @c true on success and @c false otherwise. */
static bool
getLoadedFunctionRef(const char *   propertyName,
                     const uint32_t arity,
                     const bool     isOptional,
                     cl_object &    result)
{
    ODL_ENTER(); //####
    ODL_S1("propertyName = ", propertyName); //####
    ODL_LL1("arity = ", arity); //####
    ODL_B1("isOptional = ", isOptional); //####
    ODL_P1("result = ", &result); //####
    bool      okSoFar = false;
    cl_object aSymbol = cl_find_symbol(1, CreateBaseString(propertyName, strlen(propertyName)));

    if (ECL_NIL == aSymbol)
    {
        if (isOptional)
        {
            result = ECL_NIL;
            okSoFar = true;
        }
    }
    else
    {
        if (ECL_NIL == cl_fboundp(aSymbol))
        {
            MpM_FAIL_("Problem searching for a property.");
        }
        else
        {
            result = cl_symbol_function(aSymbol);
            if (ECL_NIL != result)
            {
                if (checkArity(result, arity))
                {
                    okSoFar = true;
                }
                else
                {
                    YarpString message("Function (");

                    message += propertyName;
                    message += ") has the incorrect number of arguments.";
                    MpM_FAIL_(message.c_str());
                }
            }
        }
    }
    ODL_EXIT_B(okSoFar); //####
    return okSoFar;
} // getLoadedFunctionRef

/*! @brief Check a stream description.
 @param anElement The stream description object to be checked.
 @param inletHandlers non-@c NULL if there must be a handler for the stream description.
 @param description The validated stream description.
 @returns @c true on success and @c false otherwise. */
static bool
processStreamDescription(cl_object            anElement,
                         ObjectVector *       inletHandlers,
                         ChannelDescription & description)
{
    ODL_ENTER(); //####
    ODL_P3("anElement = ", &anElement, "inletHandlers = ", inletHandlers, //####
              "description = ", &description); //####
    bool okSoFar;

    if (ECL_NIL == cl_hash_table_p(anElement))
    {
        okSoFar = false;
    }
    else
    {
        cl_env_ptr env = ecl_process_env();
        cl_object  aSymbol = cl_find_symbol(1, CreateBaseString(NAME_NAME_,
                                                                sizeof(NAME_NAME_) - 1));
        cl_object  aValue = cl_gethash(2, aSymbol, anElement);
        cl_object  present = ecl_nth_value(env, 1);

        if (ECL_NIL == present)
        {
            okSoFar = false;
        }
        else
        {
            okSoFar = true;
            if (ECL_NIL == cl_stringp(aValue))
            {
                aValue = cl_string(aValue);
            }
            aValue = si_coerce_to_base_string(aValue);
            if (ECL_NIL != aValue)
            {
                description._portName = reinterpret_cast<char *>(aValue->base_string.self);
            }
        }
        if (okSoFar)
        {
            aSymbol = cl_find_symbol(1, CreateBaseString(PROTOCOL_NAME_,
                                                         sizeof(PROTOCOL_NAME_) - 1));
            aValue = cl_gethash(2, aSymbol, anElement);
            present = ecl_nth_value(env, 1);
            if (ECL_NIL == present)
            {
                okSoFar = false;
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
                    description._portProtocol = reinterpret_cast<char *>(aValue->base_string.self);
                }
            }
        }
        if (okSoFar)
        {
            aSymbol = cl_find_symbol(1, CreateBaseString(PROTOCOLDESCRIPTION_NAME_,
                                                         sizeof(PROTOCOLDESCRIPTION_NAME_) - 1));
            aValue = cl_gethash(2, aSymbol, anElement);
            present = ecl_nth_value(env, 1);
            if (ECL_NIL == present)
            {
                okSoFar = false;
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
                    description._protocolDescription =
                                                reinterpret_cast<char *>(aValue->base_string.self);
                }
            }
        }
        if (okSoFar && inletHandlers)
        {
            aSymbol = cl_find_symbol(1, CreateBaseString(HANDLER_NAME_, sizeof(HANDLER_NAME_) - 1));
            aValue = cl_gethash(2, aSymbol, anElement);
            present = ecl_nth_value(env, 1);
            if (ECL_NIL == present)
            {
                okSoFar = false;
            }
            else
            {
                if (ECL_NIL == cl_symbolp(aValue))
                {
                    okSoFar = false;
                }
                else if (ECL_NIL == cl_fboundp(aValue))
                {
                    okSoFar = false;
                }
                else
                {
                    aValue = cl_symbol_function(aValue);
                    if (ECL_NIL == aValue)
                    {
                        okSoFar = false;
                    }
                    else if (checkArity(aValue, 2))
                    {
                        inletHandlers->push_back(aValue);
                    }
                    else
                    {
                        okSoFar = false;
                    }
                }
            }
        }
    }
    ODL_EXIT_B(okSoFar); //####
    return okSoFar;
} // processStreamDescription

/*! @brief Check the Common Lisp environment for a specific array variable containing stream
 descriptions.
 @param arrayName The name of the array variable being searched for.
 @param inletHandlers non-@c NULL if there must be a handler for each stream description.
 @param streamDescriptions The list of loaded stream descriptions.
 @returns @c true on success and @c false otherwise. */
static bool
getLoadedStreamDescriptions(const char *    arrayName,
                            ObjectVector *  inletHandlers,
                            ChannelVector & streamDescriptions)
{
    ODL_ENTER(); //####
    ODL_P2("inletHandlers = ", inletHandlers, "streamDescriptions = ", //####
              &streamDescriptions); //####
    ODL_S1("arrayName = ", arrayName); //####
    bool okSoFar = false;

    try
    {
        cl_object descriptionArray = ECL_NIL;
        cl_object aSymbol = cl_find_symbol(1, CreateBaseString(arrayName, strlen(arrayName)));

        streamDescriptions.clear();
        if (ECL_NIL == aSymbol)
        {
            okSoFar = true;
        }
        else
        {
            if (ECL_NIL != cl_boundp(aSymbol))
            {
                descriptionArray = cl_symbol_value(aSymbol);
            }
            else if (ECL_NIL != cl_fboundp(aSymbol))
            {
                cl_object aFunction = cl_symbol_function(aSymbol);

                if (ECL_NIL != aFunction)
                {
                    if (checkArity(aFunction, 0))
                    {
                        cl_env_ptr env = ecl_process_env();
                        cl_object  errorSymbol = ecl_make_symbol("ERROR", "CL");

                        ECL_RESTART_CASE_BEGIN(env, ecl_list1(errorSymbol))
                        {
                            /* This form is evaluated with bound handlers. */
                            descriptionArray = cl_funcall(1, aFunction);
                        }
                        ECL_RESTART_CASE(1, condition)
                        {
                            /* This code is executed when an error happens. */
                            descriptionArray = ECL_NIL;
                            MpM_FAIL_("Script aborted during load.");
                        }
                        ECL_RESTART_CASE_END;
                    }
                    else
                    {
                        YarpString message("Function (");

                        message += arrayName;
                        message += ") has the incorrect number of arguments.";
                        MpM_FAIL_(message.c_str());
                    }
                }
            }
            else
            {
                MpM_FAIL_("Problem searching for a property.");
            }
            if (descriptionArray)
            {
                if (ECL_NIL != cl_arrayp(descriptionArray))
                {
                    if (1 == ecl_fixnum(cl_array_rank(descriptionArray)))
                    {
                        cl_fixnum numElements = ecl_fixnum(cl_array_dimension(descriptionArray,
                                                                              ecl_make_fixnum(0)));

                        okSoFar = true;
                        for (cl_fixnum ii = 0; okSoFar && (numElements > ii); ++ii)
                        {
                            cl_object anElement = cl_aref(2, descriptionArray, ecl_make_fixnum(ii));

                            if (ECL_NIL == anElement)
                            {
                                okSoFar = false;
                            }
                            else
                            {
                                ChannelDescription description;

                                okSoFar = processStreamDescription(anElement, inletHandlers,
                                                                   description);
                                if (okSoFar)
                                {
                                    streamDescriptions.push_back(description);
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_EXIT_B(okSoFar); //####
    return okSoFar;
} // getLoadedStreamDescriptions

/*! @brief Check the Common Lisp environment after loading a script.
 @param sawThread @c true if a thread function was defined.
 @param description The descriptive text from the script.
 @param helpString The help text from the script.
 @param loadedInletDescriptions The list of loaded inlet stream descriptions.
 @param loadedOutletDescriptions The list of loaded outlet stream descriptions.
 @param loadedInletHandlers The list of loaded inlet handlers.
 @param loadedStartingFunction The function to execute on starting the service streams.
 @param loadedStoppingFunction The function to execute on stopping the service streams.
 @param loadedThreadFunction The function to execute on an output-generating thread.
 @param loadedInterval The interval (in seconds) between executions of the output-generating thread.
 @returns @c true on success and @c false otherwise.
 @param missingStuff A list of the missing functions or variables. */
static bool
validateLoadedScript(bool &          sawThread,
                     YarpString &    description,
                     YarpString &    helpString,
                     ChannelVector & loadedInletDescriptions,
                     ChannelVector & loadedOutletDescriptions,
                     ObjectVector &  loadedInletHandlers,
                     cl_object &     loadedStartingFunction,
                     cl_object &     loadedStoppingFunction,
                     cl_object &     loadedThreadFunction,
                     double &        loadedInterval,
                     YarpString &    missingStuff)
{
    ODL_ENTER();
    ODL_P4("sawThread = ", &sawThread, "description = ", &description, "helpString = ", //####
              &helpString, "loadedInletDescriptions = ", &loadedInletDescriptions);
    ODL_P4("loadedOutletDescriptions = ", &loadedOutletDescriptions, //####
              "loadedInletHandlers = ", &loadedInletHandlers, "loadedStartingFunction = ", //####
              &loadedStartingFunction, "loadedStoppingFunction = ", &loadedStoppingFunction); //####
    ODL_P3("loadedThreadFunction = ", &loadedThreadFunction, "loadedInterval = ", //####
              &loadedInterval, "missingStuff = ", &missingStuff); //####
    bool okSoFar;

    sawThread = false;
    loadedInterval = 1.0;
    loadedThreadFunction = ECL_NIL;
    loadedStartingFunction = ECL_NIL;
    loadedStoppingFunction = ECL_NIL;
    missingStuff = "";
    if (getLoadedString("SCRIPTDESCRIPTION", true, false, description))
    {
        okSoFar = true;
    }
    else
    {
        missingStuff = "scriptDescription";
        okSoFar = false;
    }
    if (! getLoadedString("SCRIPTHELP", false, true, helpString))
    {
        if (0 < missingStuff.length())
        {
            missingStuff += ", ";
        }
        missingStuff += "scriptHelp";
        okSoFar = false;
    }
    if (getLoadedFunctionRef("SCRIPTTHREAD", 0, true, loadedThreadFunction))
    {
        sawThread = (ECL_NIL != loadedThreadFunction);
    }
    else
    {
        if (0 < missingStuff.length())
        {
            missingStuff += ", ";
        }
        missingStuff += "scriptThread";
        okSoFar = false;
    }
    if (! sawThread)
    {
        if (! getLoadedStreamDescriptions("SCRIPTINLETS", &loadedInletHandlers,
                                          loadedInletDescriptions))
        {
            if (0 < missingStuff.length())
            {
                missingStuff += ", ";
            }
            missingStuff += "scriptInlets";
            okSoFar = false;
        }
    }
    if (! getLoadedStreamDescriptions("SCRIPTOUTLETS", NULL, loadedOutletDescriptions))
    {
        if (0 < missingStuff.length())
        {
            missingStuff += ", ";
        }
        missingStuff += "scriptOutlets";
        okSoFar = false;
    }
    if (sawThread)
    {
        if (! getLoadedDouble("SCRIPTINTERVAL", true, true, loadedInterval))
        {
            if (0 < missingStuff.length())
            {
                missingStuff += ", ";
            }
            missingStuff += "scriptInterval";
            okSoFar = false;
        }
    }
    if (okSoFar)
    {
        if (getLoadedFunctionRef("SCRIPTSTARTING", 0, true, loadedStartingFunction))
        {
//            cout << "function scriptStarting defined" << endl;
        }
        if (getLoadedFunctionRef("SCRIPTSTOPPING", 0, true, loadedStoppingFunction))
        {
//            cout << "function scriptStopping defined" << endl;
        }
    }
    ODL_EXIT_B(okSoFar);
    return okSoFar;
} // validateLoadedScript

/*! @brief Set up the environment and start the %CommonLisp filter service.
 @param argumentList Descriptions of the arguments to the executable.
 @param scriptPath The script file to be processed.
 @param arguments The arguments for the service.
 @param progName The path to the executable.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used with the %CommonLisp filter service.
 @param tag The modifier for the service name and port names.
 @param serviceEndpointName The YARP name to be assigned to the new service.
 @param servicePortNumber The port being used by the service.
 @param goWasSet @c true if the service is to be started immediately.
 @param nameWasSet @c true if the endpoint name was set and @c false otherwise.
 @param reportOnExit @c true if service metrics are to be reported on exit and @c false otherwise.
 @param stdinAvailable @c true if running in the foreground and @c false otherwise. */
static void
setUpAndGo(const Utilities::DescriptorVector & argumentList,
           YarpString &                        scriptPath,
           const YarpStringVector &            arguments,
           const YarpString &                  progName,
           const int                           argc,
           char * *                            argv,
           YarpString &                        tag,
           YarpString &                        serviceEndpointName,
           const YarpString &                  servicePortNumber,
           const bool                          goWasSet,
           const bool                          nameWasSet,
           const bool                          reportOnExit,
           const bool                          stdinAvailable)
{
    ODL_ENTER(); //####
    ODL_P3("argumentList = ", &argumentList, "arguments = ", &arguments, "argv = ", argv); //####
    ODL_S4s("scriptPath = ", scriptPath, "progName = ", progName, "tag = ", tag, //####
               "serviceEndpointName = ", serviceEndpointName); //####
    ODL_S1s("servicePortNumber = ", servicePortNumber); //####
    ODL_LL1("argc = ", argc); //####
    ODL_B4("goWasSet = ", goWasSet, "nameWasSet = ", nameWasSet, //####
              "reportOnExit = ", reportOnExit, "stdinAvailable = ", stdinAvailable); //####
    bool          sawThread;
    ChannelVector loadedInletDescriptions;
    ChannelVector loadedOutletDescriptions;
    double        loadedInterval;
    YarpString    description;
    YarpString    helpText;
    YarpString    missingStuff;
    ObjectVector  loadedInletHandlers;
    cl_object     loadedStartingFunction = ECL_NIL;
    cl_object     loadedStoppingFunction = ECL_NIL;
    cl_object     loadedThreadFunction = ECL_NIL;

    cl_boot(argc, argv);
    atexit(cl_shutdown);
    // Set up our functions and objects before loading the script.
    try
    {
        bool       okSoFar = true;
        cl_env_ptr env = ecl_process_env();
        cl_object  ourPackage = addCustomObjects(tag, arguments);
        cl_object  errorSymbol = ecl_make_symbol("ERROR", "CL");

        // Load the script!
        loadedInletHandlers.clear();
        ECL_RESTART_CASE_BEGIN(env, ecl_list1(errorSymbol))
        {
            /* This form is evaluated with bound handlers. */
            cl_object pathToUse = CreateBaseString(scriptPath.c_str(), scriptPath.length());

            if (ECL_NIL != pathToUse)
            {
                cl_load(1, pathToUse);
                okSoFar = true;
            }
        }
        ECL_RESTART_CASE(1, condition)
        {
            /* This code is executed when an error happens. */
            okSoFar = false;
            MpM_FAIL_("Script aborted during load.");
        }
        ECL_RESTART_CASE_END;
        if (okSoFar)
        {
            // Check for the functions / strings that we need.
            if (validateLoadedScript(sawThread, description, helpText, loadedInletDescriptions,
                                     loadedOutletDescriptions, loadedInletHandlers,
                                     loadedStartingFunction, loadedStoppingFunction,
                                     loadedThreadFunction, loadedInterval, missingStuff))
            {
                CommonLispFilterService * aService = new CommonLispFilterService(argumentList,
                                                                                 scriptPath, argc,
                                                                                 argv, tag,
                                                                                 description,
                                                                         loadedInletDescriptions,
                                                                         loadedOutletDescriptions,
                                                                             loadedInletHandlers,
                                                                             loadedStartingFunction,
                                                                             loadedStoppingFunction,
                                                                                 sawThread,
                                                                             loadedThreadFunction,
                                                                                 loadedInterval,
                                                                             serviceEndpointName,
                                                                                 servicePortNumber);

                if (aService)
                {
                    lActiveService = aService;
                    aService->performLaunch(helpText, goWasSet, stdinAvailable, reportOnExit);
                    lActiveService = NULL;
                    delete aService;
                }
                else
                {
                    ODL_LOG("! (aService)"); //####
                }
            }
            else
            {
                ODL_LOG("! (validateLoadedScript(sawThread, description, helpText, " //####
                       "loadedInletDescriptions, loadedOutletDescriptions, " //####
                       "loadedInletHandlers, loadedStartingFunction, " //####
                       "loadedStoppingFunction, loadedThreadFunction, loadedInterval))"); //####
                YarpString message("Script is missing one or more functions or variables (");

                okSoFar = false;
                message += missingStuff + ").";
                MpM_FAIL_(message.c_str());
            }
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_EXIT(); //####
} // setUpAndGo

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

/*! @brief The entry point for running the %CommonLisp filter service.

 The first argument is the path of the script to be run by the service.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used with the %CommonLisp filter service.
 @returns @c 0 on a successful test and @c 1 on failure. */
int
main(int      argc,
     char * * argv)
{
    YarpString progName(*argv);

#if defined(MpM_ServicesLogToStandardError)
    ODL_INIT(progName.c_str(), kODLoggingOptionIncludeProcessID | //####
             kODLoggingOptionIncludeThreadID | kODLoggingOptionWriteToStderr | //####
             kODLoggingOptionEnableThreadSupport); //####
#else // ! defined(MpM_ServicesLogToStandardError)
    ODL_INIT(progName.c_str(), kODLoggingOptionIncludeProcessID | //####
             kODLoggingOptionIncludeThreadID | kODLoggingOptionEnableThreadSupport); //####
#endif // ! defined(MpM_ServicesLogToStandardError)
    ODL_ENTER(); //####
#if MAC_OR_LINUX_
    SetUpLogger(progName);
#endif // MAC_OR_LINUX_
    try
    {
        AddressTagModifier                    modFlag = kModificationNone;
        bool                                  goWasSet = false;
        bool                                  reportEndpoint = false;
        bool                                  reportOnExit = false;
        bool                                  stdinAvailable = CanReadFromStandardInput();
        YarpString                            serviceEndpointName;
        YarpString                            servicePortNumber;
        YarpString                            tag;
        YarpStringVector                      arguments;
        Utilities::FilePathArgumentDescriptor firstArg("filePath", T_("Path to script file to use"),
                                                       Utilities::kArgModeRequired, "", "", false,
                                                       false);
        Utilities::ExtraArgumentDescriptor    secondArg("scriptArgument",
                                                        T_("Additional script arguments"));
        Utilities::DescriptorVector           argumentList;

        argumentList.push_back(&firstArg);
        argumentList.push_back(&secondArg);
        if (ProcessStandardServiceOptions(argc, argv, argumentList,
                                          COMMONLISPFILTER_SERVICE_DESCRIPTION_, "", 2015,
                                          STANDARD_COPYRIGHT_NAME_, goWasSet, reportEndpoint,
                                          reportOnExit, tag, serviceEndpointName, servicePortNumber,
                                          modFlag, kSkipNone, &arguments))
        {
            Utilities::SetUpGlobalStatusReporter();
            Utilities::CheckForNameServerReporter();
            if (Utilities::CheckForValidNetwork())
            {
                yarp::os::Network yarp; // This is necessary to establish any connections to the
                                        // YARP infrastructure

                Initialize(progName);
                YarpString scriptPath(firstArg.getCurrentValue());
                YarpString tagModifier =
                                Utilities::GetFileNameBase(Utilities::GetFileNamePart(scriptPath));
                bool       nameWasSet = AdjustEndpointName(DEFAULT_COMMONLISP_SERVICE_NAME_,
                                                           modFlag, tag, serviceEndpointName,
                                                           tagModifier);

                if (reportEndpoint)
                {
                    cout << serviceEndpointName.c_str() << endl;
                }
                else if (Utilities::CheckForRegistryService())
                {
                    setUpAndGo(argumentList, scriptPath, arguments, progName, argc, argv, tag,
                               serviceEndpointName, servicePortNumber, goWasSet, nameWasSet,
                               reportOnExit, stdinAvailable);
                }
                else
                {
                    ODL_LOG("! (Utilities::CheckForRegistryService())"); //####
                    MpM_FAIL_(MSG_REGISTRY_NOT_RUNNING);
                }
            }
            else
            {
                ODL_LOG("! (Utilities::CheckForValidNetwork())"); //####
                MpM_FAIL_(MSG_YARP_NOT_RUNNING);
            }
            Utilities::ShutDownGlobalStatusReporter();
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
    }
    yarp::os::Network::fini();
    ODL_EXIT_L(0); //####
    return 0;
} // main
