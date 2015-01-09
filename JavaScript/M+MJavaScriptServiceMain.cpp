//--------------------------------------------------------------------------------------------------
//
//  File:       M+MJavaScriptServiceMain.cpp
//
//  Project:    M+M
//
//  Contains:   The main application for the JavaScript input / output service.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2015 by HPlus Technologies Ltd. and Simon Fraser University.
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
//  Created:    2015-01-05
//
//--------------------------------------------------------------------------------------------------

#include "M+MJavaScriptService.h"

#include <mpm/M+MEndpoint.h>
#include <mpm/M+MUtilities.h>

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if (! MAC_OR_LINUX_) //ASSUME WINDOWS
# include <mpm/getopt.h>
#endif //(! MAC_OR_LINUX_)

#if defined(MAC_OR_LINUX_)
# include <libgen.h>
#else  // ! defined(MAC_OR_LINUX_)
# include <stdlib.h>
#endif // ! defined(MAC_OR_LINUX_)

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Winvalid-offsetof"
#endif // defined(__APPLE__)
#include <js/RequiredDefines.h>
#include <jsapi.h>
#include <js/CallArgs.h>
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The main application for the %JavaScript input / output service. */

/*! @dir JavaScript
 @brief The set of files that implement the %JavaScript input / output service. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::JavaScript;
using std::cin;
using std::cout;
using std::endl;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

/*! @brief The number of megabytes before the %JavaScript engine triggers a garbage collection. */
#define JAVASCRIPT_GC_SIZE 16

/*! @brief The number of bytes for each %JavaScript 'stack chunk'. */
#define JAVASCRIPT_STACKCHUNK_SIZE 8192

/*! @brief The class of the global object. */
static JSClass lGlobalClass =
{
    "global",
    JSCLASS_GLOBAL_FLAGS,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    JS_GlobalObjectTraceHook
}; // lGlobalClass

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

/*! @brief Display the available commands. */
static void displayCommands(void)
{
    OD_LOG_ENTER(); //####
    cout << "Commands:" << endl;
    cout << "  ? - display this list" << endl;
    cout << "  b - start (begin) the input and output streams" << endl;
    cout << "  c - configure the service" << endl;
    cout << "  e - stop (end) the input and output streams" << endl;
    cout << "  q - quit the application" << endl;
    cout << "  r - restart the input and output streams" << endl;
    cout << "  u - reset the configuration (unconfigure) so that it will be reprocessed" << endl;
    OD_LOG_EXIT(); //####
} // displayCommands

/*! @brief The error reporter callback for the %JavaScript engine.
 @param cx The context in which the error happened.
 @param message An error message.
 @param report An error report record containing additional details about the error. */
static void reportJavaScriptError(JSContext *     cx,
                                  const char *    message,
                                  JSErrorReport * report)
{
    // Note that, since this is a callback for the JavaScript engine, it must NOT throw any C++
    // exceptions!
    try
    {
        yarp::os::ConstString errMessage(report->filename ? report->filename : "[no filename]");
        std::stringstream     buff;
        
        buff << report->lineno << ":" << message;
        errMessage += buff.str();
#if MAC_OR_LINUX_
        GetLogger().fail(errMessage);
#else // ! MAC_OR_LINUX_
        std::cerr << errMessage.c_str() << std::endl;
#endif // ! MAC_OR_LINUX_
    }
    catch (...)
    {
        // Suppress any C++ exception caused by this function.
    }
} // reportJavaScriptError

/*! @brief A C-callback function for %JavaScript to write out a string.
 @param jct The context in which the native function is being called.
 @param argc The number of arguments supplied to the function by the caller.
 @param vp The arguments to the function.
 @returns @c true on success and @c false otherwise. */
static bool writeStringForJs(JSContext * jct,
                             unsigned    argc,
                             JS::Value * vp)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("jct = ", jct, "vp = ", vp); //####
    OD_LOG_L1("argc = ", argc); //####
    bool         result;
    JS::CallArgs args = JS::CallArgsFromVp(argc, vp);
    
    if (0 == args.length())
    {
        JS_ReportError(jct, "Missing argument to writeString");
        result = false;
    }
    else if (1 < args.length())
    {
        JS_ReportError(jct, "Extra arguments to writeString");
        result = false;
    }
    else if (args[0].isString())
    {
        JSString * asString = args[0].toString();
        char *     asChars = JS_EncodeString(jct, asString);

        std::cout << asChars << std::endl;
        JS_free(jct, asChars);
        result = true;
    }
    else
    {
        JS_ReportError(jct, "Non-string argument to writeString");
        result = false;
    }
    OD_LOG_EXIT_B(result); //####
    return result;
} // writeStringForJs

/*! @brief The table of supplied functions for the service. */
static JSFunctionSpec lServiceFunctions[] =
{
    JS_FS("writeStringToStdout", writeStringForJs, 1, 0),
//    JS_FS("rand",   myjs_rand,   0, 0),
//    JS_FS("srand",  myjs_srand,  0, 0),
//    JS_FS("system", myjs_system, 1, 0),
    JS_FS_END
}; // lServiceFunctions

/*! @brief Add custom functions and variables to the %JavaScript environment.
 @param jct The %JavaScript engine context.
 @param global The %JavaScript global object.
 @param argv The arguments to be used with the %JavaScript input / output service.
 @param argc The number of arguments in 'argv'.
 @returns @c true if the functions were addeded successfully and @c false otherwise. */
static bool addCustomFunctionsAndVariables(JSContext *        jct,
                                           JS::RootedObject & global,
                                           char * *           argv,
                                           const int          argc)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("jct = ", jct, "global = ", &global); //####
    bool okSoFar = JS_DefineFunctions(jct, global, lServiceFunctions);

    if (okSoFar)
    {
        JSObject * argArray = JS_NewArrayObject(jct, 0);
        
        if (argArray)
        {
            JS::RootedObject argObject(jct);
            JS::RootedValue  argValue(jct);
            
            argObject = argArray;
            argValue.setObject(*argArray);
            if (JS_SetProperty(jct, global, "argv", argValue))
            {
                char *          endPtr;
                int32_t         tempInt;
                JS::RootedValue anElement(jct);
                JS::RootedId    aRootedId(jct);
                
                for (int ii = optind; okSoFar && (argc > ii); ++ii)
                {
                    char * anArg = argv[ii];
                    
                    // Check for an integer value
                    tempInt = static_cast<int32_t>(strtol(anArg, &endPtr, 10));
                    if ((anArg == endPtr) || *endPtr)
                    {
                        // Check for an floating-point value
                        double tempDouble = strtod(anArg, &endPtr);
                        
                        if ((anArg == endPtr) || *endPtr)
                        {
                            // Otherwise, treat as a string
                            JSString * aString = JS_NewStringCopyZ(jct, anArg);
                            
                            if (aString)
                            {
                                anElement.setString(aString);
                            }
                            else
                            {
                                okSoFar = false;
                            }
                        }
                        else
                        {
                            anElement.setDouble(tempDouble);
                        }
                    }
                    else
                    {
                        anElement.setInt32(tempInt);
                    }
                    if (okSoFar)
                    {
                        if (JS_IndexToId(jct, ii - optind, &aRootedId))
                        {
                            JS_SetPropertyById(jct, argObject, aRootedId, anElement);
                        }
                        else
                        {
                            okSoFar = false;
                        }
                    }
                }
            }
            else
            {
                okSoFar = false;
            }
        }
        else
        {
            okSoFar = false;
        }
    }
    OD_LOG_EXIT_B(okSoFar); //####
    return okSoFar;
} // addCustomFunctionsAndVariables

/*! @brief Load a script into the %JavaScript environment.
 @param jct The %JavaScript engine context.
 @param global The %JavaScript global object.
 @param options The compile options used to retain the compiled script.
 @param script The %JavaScript source code to be executed.
 @param scriptPath The path to the script file.
 @returns @c true on success and @c false otherwise. */
static bool loadScript(JSContext *                   jct,
                       JS::RootedObject &            global,
                       JS::OwningCompileOptions &    options,
                       const yarp::os::ConstString & script,
                       const yarp::os::ConstString & scriptPath)
{
    OD_LOG_ENTER();
    OD_LOG_P2("jct = ", jct, "global = ", &global); //####
    OD_LOG_S1s("scriptPath = ", scriptPath); //####
    bool            okSoFar;
    JS::RootedValue result(jct);
    
    options.setFileAndLine(jct, scriptPath.c_str(), 1);
    // We can ignore the returned result, since we are only interested in setting up the functions
    // and variables in the environment. The documentation states that NULL can be passed as the
    // last argument, but compiles fail if this is done.
    okSoFar = JS::Evaluate(jct, global, options, script.c_str(), script.size(), &result);
    OD_LOG_EXIT_B(okSoFar);
    return okSoFar;
} // loadScript

/*! @brief Print out a value.
 @param jct The %JavaScript engine context.
 @param caption A title for the output.
 @param value The value to be printed.
 @param depth The indentation level to be used. */
static void printRootedValue(JSContext *       jct,
                             const char *      caption,
                             JS::RootedValue & value,
                             const int         depth)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("jct = ", jct, "value = ", value); //####
    OD_LOG_S1("caption = ", caption); //####
    if (0 < depth)
    {
        std::cout.width(depth);
        std::cout << " ";
    }
    std::cout << caption;
    if (value.isString())
    {
        JSString * asString = value.toString();
        char *     asChars = JS_EncodeString(jct, asString);
        
        std::cout << "string(" << asChars << ")";
        JS_free(jct, asChars);
    }
    else if (value.isObject())
    {
        // Objects will be processed separately.
    }
    else if (value.isInt32())
    {
        std::cout << "int32(" << value.toInt32() << ")";
    }
    else if (value.isBoolean())
    {
        std::cout << "boolean(" << (value.toBoolean() ? "true" : "false") << ")";
    }
    else if (value.isDouble())
    {
        std::cout << "double(" << value.toDouble() << ")";
    }
    else if (value.isNullOrUndefined())
    {
        std::cout << "null or undefined";
    }
    else
    {
        std::cout << "other";
    }
    OD_LOG_EXIT(); //####
} // printRootedValue

/*! @brief Print out an object.
 @param jct The %JavaScript engine context.
 @param anObject The object to be printed.
 @param depth The indentation level to be used. */
static void printObject(JSContext *        jct,
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
        
        for (int ii = 0, len = ids.length(); (len > ii) && okSoFar; ++ii)
        {
            JS::RootedValue key(jct);
            
            if (JS_IdToValue(jct, ids[ii], &key))
            {
                printRootedValue(jct, "id = ", key, depth);
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
                    printRootedValue(jct, ", property = ", result, 0);
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
                                std::cout << "array";
                                uint32_t arrayLength;
                                
                                if (JS_GetArrayLength(jct, asObject, &arrayLength))
                                {
                                    std::cout << ", size = " << arrayLength;
                                }
                            }
                            else if (JS_ObjectIsFunction(jct, asObject))
                            {
                                std::cout << "function";
                                JSFunction * asFunction = JS_ValueToFunction(jct, result);
                                
                                if (asFunction)
                                {
                                    std::cout << ", arity = " << JS_GetFunctionArity(asFunction);
                                    if (! JS::IsCallable(asObject))
                                    {
                                        std::cout << ", not callable";
                                    }
                                }
                            }
                            else
                            {
                                std::cout << "object";
                            }
                        }
                        else
                        {
                            okSoFar = false;
                        }
                        std::cout << std::endl;
                        if (okSoFar)
                        {
                            printObject(jct, asObject, depth + 1);
                        }
                    }
                    else
                    {
                        std::cout << std::endl;
                    }
                }
            }
        }
    }
    OD_LOG_EXIT(); //####
} // printObject

/*! @brief Check an object for a specific string property.
 @param jct The %JavaScript engine context.
 @param anObject The object to check.
 @param propertyName The name of the property being searched for.
 @param canBeFunction @c true if the property can be a function rather than a string and @c false if
 the property must be a string.
 @param result The value of the string, if located.
 @returns @c true on success and @c false otherwise. */
static bool getLoadedString(JSContext *             jct,
                            JS::RootedObject &      anObject,
                            const char *            propertyName,
                            const bool              canBeFunction,
                            yarp::os::ConstString & result)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P3("jct = ", jct, "anObject = ", &anObject, "result = ", &result); //####
    OD_LOG_S1("propertyName = ", propertyName); //####
    bool found = false;
    bool okSoFar;
    
    if (JS_HasProperty(jct, anObject, propertyName, &found))
    {
        okSoFar = found;
    }
    else
    {
        OD_LOG("! (JS_HasProperty(jct, anObject, propertyName, &found))"); //####
        okSoFar = false;
#if MAC_OR_LINUX_
        GetLogger().fail("Problem searching for a property.");
#else // ! MAC_OR_LINUX_
        std::cerr << "Problem searching for a property." << std::endl;
#endif // ! MAC_OR_LINUX_
    }
    if (okSoFar)
    {
        JS::RootedValue value(jct);

        if (JS_GetProperty(jct, anObject, propertyName, &value))
        {
            okSoFar = false;
            if (value.isString())
            {
                JSString * asString = value.toString();
                char *     asChars = JS_EncodeString(jct, asString);
                
                result = asChars;
                JS_free(jct, asChars);
                okSoFar = true;
            }
            else if (canBeFunction)
            {
                if (value.isObject())
                {
                    JS::RootedObject asObject(jct);
                    
                    if (JS_ValueToObject(jct, value, &asObject))
                    {
                        if (JS_ObjectIsFunction(jct, asObject))
                        {
                            JS::RootedValue funcResult(jct);
                            
                            if (JS_CallFunctionValue(jct, anObject, value,
                                                     JS::HandleValueArray::empty(), &funcResult))
                            {
                                if (funcResult.isString())
                                {
                                    JSString * asString = funcResult.toString();
                                    char *     asChars = JS_EncodeString(jct, asString);
                                    
                                    result = asChars;
                                    JS_free(jct, asChars);
                                    okSoFar = true;
                                }
                            }
                        }
                    }
                }
            }
            if (! okSoFar)
            {
                OD_LOG("! (canBeFunction)"); //####
                okSoFar = false;
                yarp::os::ConstString message("Property '");
                
                message += propertyName;
                message += "' has the wrong type.";
#if MAC_OR_LINUX_
                GetLogger().fail(message.c_str());
#else // ! MAC_OR_LINUX_
                std::cerr << message.c_str() << std::endl;
#endif // ! MAC_OR_LINUX_
            }
        }
        else
        {
            OD_LOG("! (JS_GetProperty(jct, anObject, propertyName, &value))"); //####
            okSoFar = false;
#if MAC_OR_LINUX_
            GetLogger().fail("Problem retrieving a property.");
#else // ! MAC_OR_LINUX_
            std::cerr << "Problem retrieving a property." << std::endl;
#endif // ! MAC_OR_LINUX_
        }
    }
    OD_LOG_EXIT_B(okSoFar); //####
    return okSoFar;
} // getLoadedString

/*! @brief Check a stream description.
 @param jct The %JavaScript engine context.
 @param anElement The stream description object to be checked.
 @param description The validated stream description.
 @returns @c true on success and @c false otherwise. */
static bool processStreamDescription(JSContext *          jct,
                                     JS::RootedValue &    anElement,
                                     ChannelDescription & description)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P3("jct = ", jct, "anElement = ", &anElement, "description = ", &description); //####
    bool okSoFar = true;
    
    if (! anElement.isObject())
    {
        OD_LOG("(! anElement.isObject())"); //####
        okSoFar = false;
#if MAC_OR_LINUX_
        GetLogger().fail("Array element has the wrong type.");
#else // ! MAC_OR_LINUX_
        std::cerr << "Array element has the wrong type." << std::endl;
#endif // ! MAC_OR_LINUX_
    }
    JS::RootedObject asObject(jct);
    
    if (okSoFar)
    {
        if (! JS_ValueToObject(jct, anElement, &asObject))
        {
            OD_LOG("(! JS_ValueToObject(jct, anElement, &asObject))"); //####
            okSoFar = false;
#if MAC_OR_LINUX_
            GetLogger().fail("Problem converting array element to object.");
#else // ! MAC_OR_LINUX_
            std::cerr << "Problem converting array element to object." << std::endl;
#endif // ! MAC_OR_LINUX_
        }
    }
    if (okSoFar)
    {
        okSoFar = getLoadedString(jct, asObject, "name", false, description._portName);
    }
    if (okSoFar)
    {
        okSoFar = getLoadedString(jct, asObject, "protocol", false, description._portProtocol);
    }
    if (okSoFar)
    {
        okSoFar = getLoadedString(jct, asObject, "protocolDescription", false,
                                  description._protocolDescription);
    }
    OD_LOG_EXIT_B(okSoFar); //####
    return okSoFar;
} // processStreamDescription

/*! @brief Check the %JavaScript environment for a specific array variable containing stream
 descriptions.
 @param jct The %JavaScript engine context.
 @param global The %JavaScript global object.
 @param arrayName The name of the array variable being searched for.
 @param streamDescriptions The list of loaded stream descriptions.
 @returns @c true on success and @c false otherwise. */
static bool getLoadedStreamDescriptions(JSContext *        jct,
                                        JS::RootedObject & global,
                                        const char *       arrayName,
                                        ChannelVector &    streamDescriptions)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P3("jct = ", jct, "global = ", &global, "streamDescriptions = ", //####
              &streamDescriptions); //####
    OD_LOG_S1("arrayName = ", arrayName); //####
    bool found = false;
    bool okSoFar;
    
    if (JS_HasProperty(jct, global, arrayName, &found))
    {
        okSoFar = found;
    }
    else
    {
        OD_LOG("! (JS_HasProperty(jct, global, arrayName, &found))"); //####
        okSoFar = false;
#if MAC_OR_LINUX_
        GetLogger().fail("Problem searching for a global property.");
#else // ! MAC_OR_LINUX_
        std::cerr << "Problem searching for a global property." << std::endl;
#endif // ! MAC_OR_LINUX_
    }
    if (okSoFar)
    {
        JS::RootedValue  value(jct);
        JS::RootedObject asObject(jct);
        
        if (JS_GetProperty(jct, global, arrayName, &value))
        {
            okSoFar = false;
            if (value.isObject())
            {
                if (JS_ValueToObject(jct, value, &asObject))
                {
                    okSoFar = true;
                }
                else
                {
                    OD_LOG("(! JS_ValueToObject(jct, value, &asObject))"); //####
#if MAC_OR_LINUX_
                    GetLogger().fail("Problem converting value to object.");
#else // ! MAC_OR_LINUX_
                    std::cerr << "Problem converting value to object." << std::endl;
#endif // ! MAC_OR_LINUX_
                }
            }
            if (okSoFar)
            {
                if (JS_ObjectIsFunction(jct, asObject))
                {
                    JS::RootedValue funcResult(jct);
                    
                    okSoFar = false;
                    if (JS_CallFunctionValue(jct, global, value,
                                             JS::HandleValueArray::empty(), &funcResult))
                    {
                        if (funcResult.isObject())
                        {
                            if (JS_ValueToObject(jct, funcResult, &asObject))
                            {
                                value = funcResult;
                                okSoFar = true;
                            }
                            else
                            {
                                OD_LOG("(! JS_ValueToObject(jct, funcResult, &asObject))"); //####
#if MAC_OR_LINUX_
                                GetLogger().fail("Problem converting value to object.");
#else // ! MAC_OR_LINUX_
                                std::cerr << "Problem converting value to object." << std::endl;
#endif // ! MAC_OR_LINUX_
                            }
                        }
                    }
                }
            }
            if (! okSoFar)
            {
                OD_LOG("(! okSoFar)"); //####
                yarp::os::ConstString message("Property '");
                
                message += arrayName;
                message += "' has the wrong type.";
#if MAC_OR_LINUX_
                GetLogger().fail(message.c_str());
#else // ! MAC_OR_LINUX_
                std::cerr << message.c_str() << std::endl;
#endif // ! MAC_OR_LINUX_
            }
        }
        else
        {
            OD_LOG("! (JS_GetProperty(jct, global, arrayName, &value))"); //####
            okSoFar = false;
#if MAC_OR_LINUX_
            GetLogger().fail("Problem retrieving a global property.");
#else // ! MAC_OR_LINUX_
            std::cerr << "Problem retrieving a global property." << std::endl;
#endif // ! MAC_OR_LINUX_
        }
        uint32_t arrayLength;
        
        if (okSoFar)
        {
            if (! JS_GetArrayLength(jct, asObject, &arrayLength))
            {
                OD_LOG("(! JS_GetArrayLength(jct, asObject, &arrayLength))"); //####
                okSoFar = false;
#if MAC_OR_LINUX_
                GetLogger().fail("Problem getting the array length.");
#else // ! MAC_OR_LINUX_
                std::cerr << "Problem getting the array length." << std::endl;
#endif // ! MAC_OR_LINUX_
            }
        }
        if (okSoFar)
        {
            for (uint32_t ii = 0; okSoFar && (arrayLength > ii); ++ii)
            {
                JS::RootedValue anElement(jct);
                
                if (JS_GetElement(jct, asObject, ii, &anElement))
                {
                    ChannelDescription description;

                    okSoFar = processStreamDescription(jct, anElement, description);
                    if (okSoFar)
                    {
                        streamDescriptions.push_back(description);
                    }
                }
                else
                {
                    OD_LOG("! (JS_GetElement(jct, asObject, ii, &anElement))"); //####
                    okSoFar = false;
#if MAC_OR_LINUX_
                    GetLogger().fail("Problem getting an array element.");
#else // ! MAC_OR_LINUX_
                    std::cerr << "Problem getting an array element." << std::endl;
#endif // ! MAC_OR_LINUX_
                }
            }
        }
    }
    OD_LOG_EXIT_B(okSoFar); //####
    return okSoFar;
} // getLoadedStreamDescriptions

/*! @brief Check the %JavaScript environment after loading a script.
 @param jct The %JavaScript engine context.
 @param global The %JavaScript global object.
 @param description The descriptive text from the script.
 @param loadedInletDescriptions The list of loaded inlet stream descriptions.
 @param loadedOutletDescriptions The list of loaded outlet stream descriptions.
 @returns @c true on success and @c false otherwise. */
static bool validateLoadedScript(JSContext *             jct,
                                 JS::RootedObject &      global,
                                 yarp::os::ConstString & description,
                                 ChannelVector &         loadedInletDescriptions,
                                 ChannelVector &         loadedOutletDescriptions)
{
    OD_LOG_ENTER();
    OD_LOG_P4("jct = ", jct, "global = ", &global, "description = ", &description, //####
              "loadedInletDescriptions = ", &loadedInletDescriptions); //####
    OD_LOG_P1("loadedOutletDescriptions = ", &loadedOutletDescriptions); //####
    bool okSoFar = true;

#if 0
    printObject(jct, global, 0);
#endif //0
    okSoFar = getLoadedString(jct, global, "scriptDescription", true, description);
    if (okSoFar)
    {
        okSoFar = getLoadedStreamDescriptions(jct, global, "scriptInlets", loadedInletDescriptions);
    }
    if (okSoFar)
    {
        okSoFar = getLoadedStreamDescriptions(jct, global, "scriptOutlets",
                                              loadedOutletDescriptions);
    }
    if (okSoFar)
    {
        
    }
    OD_LOG_EXIT_B(okSoFar);
    return okSoFar;
} // validateLoadedScript

/*! @brief Set up the %JavaScript environment and start the %JavaScript input / output service.
 @param jct The %JavaScript engine context.
 @param script The %JavaScript source code to be executed.
 @param scriptPath The path to the script file.
 @param argv The arguments to be used with the %JavaScript input / output service.
 @param argc The number of arguments in 'argv'.
 @param tag The modifier for the service name and port names.
 @param serviceEndpointName The YARP name to be assigned to the new service.
 @param servicePortNumber The port being used by the service.
 @param stdinAvailable @c true if running in the foreground and @c false otherwise.
 @param reportOnExit @c true if service metrics are to be reported on exit and @c false otherwise.
 */
static void setUpAndGo(JSContext *                   jct,
                       const yarp::os::ConstString & script,
                       const yarp::os::ConstString & scriptPath,
                       char * *                      argv,
                       const int                     argc,
                       const yarp::os::ConstString & tag,
                       const yarp::os::ConstString & serviceEndpointName,
                       const yarp::os::ConstString & servicePortNumber,
                       const bool                    stdinAvailable,
                       const bool                    reportOnExit)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("jct = ", jct, "argv = ", argv); //####
    OD_LOG_S4s("scriptPath = ", scriptPath, "tag = ", tag, "serviceEndpointName = ", //####
               serviceEndpointName, "servicePortNumber = ", servicePortNumber); //####
    OD_LOG_L1("argc = ", argc); //####
    OD_LOG_B2("stdinAvailable = ", stdinAvailable, "reportOnExit = ", reportOnExit); //####
    // Enter a request before running anything in the context. In particular, the request is needed
    // in order for JS_InitStandardClasses to work properly.
    JSAutoRequest    ar(jct);
    JS::RootedObject global(jct, JS_NewGlobalObject(jct, &lGlobalClass, NULL,
                                                    JS::FireOnNewGlobalHook));
    
    if (global)
    {
        // Enter the new global object's compartment.
        bool                     okSoFar;
        JSAutoCompartment        ac(jct, global);
        JS::OwningCompileOptions options(jct); // this is used so that script objects persist
        yarp::os::ConstString    description;
        
        // Populate the global object with the standard globals, like Object and Array.
        if (JS_InitStandardClasses(jct, global))
        {
            okSoFar = true;
        }
        else
        {
            OD_LOG("! (JS_InitStandardClasses(jct, global))"); //####
            okSoFar = false;
#if MAC_OR_LINUX_
            GetLogger().fail("JavaScript global object could not be initialized.");
#else // ! MAC_OR_LINUX_
            std::cerr << "JavaScript global object could not be initialized." << std::endl;
#endif // ! MAC_OR_LINUX_
        }
        if (okSoFar)
        {
            if (! addCustomFunctionsAndVariables(jct, global, argv, argc))
            {
                OD_LOG("(! addCustomFunctionsAndVariables(jct, global, argv, argc))"); //####
                okSoFar = false;
#if MAC_OR_LINUX_
                GetLogger().fail("Custom functions and variables could not be added to the "
                                 "JavaScript global object.");
#else // ! MAC_OR_LINUX_
                std::cerr << "Custom functions and variables could not be added to the "
                                "JavaScript global object." << std::endl;
#endif // ! MAC_OR_LINUX_
            }
        }
        if (okSoFar)
        {
            if (! loadScript(jct, global, options, script, scriptPath))
            {
                OD_LOG("(! loadScript(jct, global, script, scriptPath))"); //####
                okSoFar = false;
#if MAC_OR_LINUX_
                GetLogger().fail("Script could not be loaded.");
#else // ! MAC_OR_LINUX_
                std::cerr << "Script could not be loaded." << std::endl;
#endif // ! MAC_OR_LINUX_
            }
        }
        ChannelVector loadedInletDescriptions;
        ChannelVector loadedOutletDescriptions;
        
        if (okSoFar)
        {
            if (! validateLoadedScript(jct, global, description, loadedInletDescriptions,
                                       loadedOutletDescriptions))
            {
                OD_LOG("(! validateLoadedScript(jct, global, description, " //####
                       "inStreamDescriptions, outStreamDescriptions))"); //####
                okSoFar = false;
#if MAC_OR_LINUX_
                GetLogger().fail("Script is missing one or more functions or variables.");
#else // ! MAC_OR_LINUX_
                std::cerr << "Script is missing one or more functions or variables." << std::endl;
#endif // ! MAC_OR_LINUX_
            }
        }
        if (okSoFar)
        {
            JavaScriptService * stuff = new JavaScriptService(jct, *argv, tag, description,
                                                              loadedInletDescriptions,
                                                              loadedOutletDescriptions,
                                                              serviceEndpointName,
                                                              servicePortNumber);
            
            if (stuff)
            {
                if (stuff->start())
                {
                    yarp::os::ConstString channelName(stuff->getEndpoint().getName());
                    
                    OD_LOG_S1s("channelName = ", channelName); //####
                    if (RegisterLocalService(channelName, *stuff))
                    {
                        bool             configured = false;
                        yarp::os::Bottle configureData;
                        
                        StartRunning();
                        SetSignalHandlers(SignalRunningStop);
                        stuff->startPinger();
                        if (! stdinAvailable)
                        {
                            if (stuff->configure(configureData))
                            {
                                stuff->startStreams();
                            }
                        }
                        for ( ; IsRunning(); )
                        {
                            if (stdinAvailable)
                            {
                                char inChar;
                                
                                cout << "Operation: [? b c e q r u]? ";
                                cout.flush();
                                cin >> inChar;
                                switch (inChar)
                                {
                                    case '?' :
                                        // Help
                                        displayCommands();
                                        break;
                                        
                                    case 'b' :
                                    case 'B' :
                                        // Start streams
                                        if (! configured)
                                        {
                                            if (stuff->configure(configureData))
                                            {
                                                configured = true;
                                            }
                                        }
                                        if (configured)
                                        {
                                            stuff->startStreams();
                                        }
                                        break;
                                        
                                    case 'c' :
                                    case 'C' :
                                        // Configure - nothing to do for the JavaScript input /
                                        // output service.
                                        if (stuff->configure(configureData))
                                        {
                                            configured = true;
                                        }
                                        break;
                                        
                                    case 'e' :
                                    case 'E' :
                                        // Stop streams
                                        stuff->stopStreams();
                                        break;
                                        
                                    case 'q' :
                                    case 'Q' :
                                        // Quit
                                        StopRunning();
                                        break;
                                        
                                    case 'r' :
                                    case 'R' :
                                        // Restart streams
                                        if (! configured)
                                        {
                                            if (stuff->configure(configureData))
                                            {
                                                configured = true;
                                            }
                                        }
                                        if (configured)
                                        {
                                            stuff->restartStreams();
                                        }
                                        break;
                                        
                                    case 'u' :
                                    case 'U' :
                                        // Unconfigure
                                        configured = false;
                                        break;
                                        
                                    default :
                                        cout << "Unrecognized request '" << inChar << "'." << endl;
                                        break;
                                        
                                }
                            }
                            else
                            {
#if defined(MpM_MainDoesDelayNotYield)
                                yarp::os::Time::delay(ONE_SECOND_DELAY / 10.0);
#else // ! defined(MpM_MainDoesDelayNotYield)
                                yarp::os::Time::yield();
#endif // ! defined(MpM_MainDoesDelayNotYield)
                            }
                        }
                        UnregisterLocalService(channelName, *stuff);
                        if (reportOnExit)
                        {
                            yarp::os::Bottle metrics;
                            
                            stuff->gatherMetrics(metrics);
                            yarp::os::ConstString converted =
                                                        Utilities::ConvertMetricsToString(metrics);
                            
                            cout << converted.c_str() << endl;
                        }
                        stuff->stop();
                    }
                    else
                    {
                        OD_LOG("! (RegisterLocalService(channelName, *stuff))"); //####
#if MAC_OR_LINUX_
                        GetLogger().fail("Service could not be registered.");
#else // ! MAC_OR_LINUX_
                        std::cerr << "Service could not be registered." << std::endl;
#endif // ! MAC_OR_LINUX_
                    }
                }
                else
                {
                    OD_LOG("! (stuff->start())"); //####
#if MAC_OR_LINUX_
                    GetLogger().fail("Service could not be started.");
#else // ! MAC_OR_LINUX_
                    std::cerr << "Service could not be started." << std::endl;
#endif // ! MAC_OR_LINUX_
                }
                delete stuff;
            }
            else
            {
                OD_LOG("! (stuff)"); //####
            }
        }
    }
    else
    {
        OD_LOG("! (global)"); //####
#if MAC_OR_LINUX_
        GetLogger().fail("JavaScript global object could not be created.");
#else // ! MAC_OR_LINUX_
        std::cerr << "JavaScript global object could not be created." << std::endl;
#endif // ! MAC_OR_LINUX_
    }
    OD_LOG_EXIT(); //####
} // setUpAndGo

/*! @brief Return the file name part of a path.
 @param inFileName The file path to be processed.
 @returns The file name part of a path. */
static yarp::os::ConstString getFileNamePart(const yarp::os::ConstString & inFileName)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S1s("inFileName = ", inFileName); //####
    yarp::os::ConstString result;
#if defined(MAC_OR_LINUX_)
    char * nameCopy = strdup(inFileName.c_str());
#else // ! defined(MAC_OR_LINUX_)
    char   baseFileName[_MAX_FNAME + 10];
    char   baseExtension[_MAX_EXT + 10];
#endif // ! defined(MAC_OR_LINUX_)
    
#if defined(MAC_OR_LINUX_)
    result = basename(nameCopy);
    free(nameCopy);
#else // ! defined(MAC_OR_LINUX_)
    _splitpath(inFileName.c_str(), NULL, NULL, baseFileName, baseExtension);
    result = baseFileName;
    result += ".";
    result += baseExtension;
#endif // ! defined(MAC_OR_LINUX_)
    OD_LOG_EXIT_S(result.c_str()); //####
    return result;
} // getFileNamePart

/*! @brief Return the base name of a file name.
 @param inFileName The file name to be processed.
 @returns The base name of a file name. */
static yarp::os::ConstString getFileNameBase(const yarp::os::ConstString & inFileName)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S1s("inFileName = ", inFileName);
    yarp::os::ConstString result;
    size_t                index = inFileName.rfind('.');
    
    if (yarp::os::ConstString::npos == index)
    {
        result = inFileName;
    }
    else
    {
        result = inFileName.substr(0, index);
    }
    OD_LOG_EXIT_S(result.c_str()); //####
    return result;
} // getFileNameBase

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

/*! @brief The entry point for running the %JavaScript input / output service.
 
 The first argument is the path of the script to be run by the service.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used with the %JavaScript input / output service.
 @returns @c 0 on a successful test and @c 1 on failure. */
int main(int      argc,
         char * * argv)
{
#if defined(MpM_ServicesLogToStandardError)
    OD_LOG_INIT(*argv, kODLoggingOptionIncludeProcessID | kODLoggingOptionIncludeThreadID | //####
                kODLoggingOptionWriteToStderr | kODLoggingOptionEnableThreadSupport); //####
#else // ! defined(MpM_ServicesLogToStandardError)
    OD_LOG_INIT(*argv, kODLoggingOptionIncludeProcessID | kODLoggingOptionIncludeThreadID | //####
                kODLoggingOptionEnableThreadSupport); //####
#endif // ! defined(MpM_ServicesLogToStandardError)
    OD_LOG_ENTER(); //####
#if MAC_OR_LINUX_
    SetUpLogger(*argv);
#endif // MAC_OR_LINUX_
    try
    {
        bool                  reportOnExit = false;
        bool                  nameWasSet;
        bool                  stdinAvailable = CanReadFromStandardInput();
        yarp::os::ConstString serviceEndpointName;
        yarp::os::ConstString servicePortNumber;
        yarp::os::ConstString tag;
        
        nameWasSet = ProcessStandardServiceOptions(argc, argv, DEFAULT_JAVASCRIPT_SERVICE_NAME,
                                                   reportOnExit, tag, serviceEndpointName,
                                                   servicePortNumber);
        Utilities::CheckForNameServerReporter();
#if CheckNetworkWorks_
        if (yarp::os::Network::checkNetwork(NETWORK_CHECK_TIMEOUT))
#endif // CheckNetworkWorks_
        {
            yarp::os::Network yarp; // This is necessary to establish any connections to the YARP
                                    // infrastructure
            
            Initialize(*argv);
            if (optind < argc)
            {
                yarp::os::ConstString scriptPath(argv[optind]);
                yarp::os::ConstString scriptSource;
                yarp::os::ConstString tagModifier(getFileNameBase(getFileNamePart(scriptPath)));
                
                if (! nameWasSet)
                {
                    serviceEndpointName += "/" + tagModifier;
                }
                if (0 < tag.length())
                {
                    tag += ":" + tagModifier;
                }
                else
                {
                    tag = tagModifier;
                }
                // Make sure that the scriptPath is valid and construct the modified 'tag' and
                // (optional) endpoint name.
                FILE * scratch = fopen(scriptPath.c_str(), "r");
                
                if (scratch)
                {
                    // The path given is a readable file, so read it in and prepare to start the
                    // service by filling in the scriptSource.
                    char   buffer[10240];
                    size_t numRead;
                    
                    for ( ; ! feof(scratch); )
                    {
                        numRead = fread(buffer, 1, sizeof(buffer) - 1, scratch);
                        if (numRead)
                        {
                            buffer[numRead] = '\0';
                            scriptSource += buffer;
                        }
                    }
                    fclose(scratch);
                }
                if (0 < scriptSource.size())
                {
                    if (JS_Init())
                    {
                        JSContext * jct = NULL;
                        JSRuntime * jrt = JS_NewRuntime(JAVASCRIPT_GC_SIZE * 1024 * 1024);
                        
                        if (jrt)
                        {
                            // Avoid ambiguity between 'var x = ...' and 'x = ...'.
                            JS::RuntimeOptionsRef(jrt).setVarObjFix(true);
                            JS::RuntimeOptionsRef(jrt).setExtraWarnings(true);
                            jct = JS_NewContext(jrt, JAVASCRIPT_STACKCHUNK_SIZE);
                            if (jct)
                            {
                                JS_SetErrorReporter(jrt, reportJavaScriptError);
                            }
                            else
                            {
                                OD_LOG("! (jct)"); //####
#if MAC_OR_LINUX_
                                GetLogger().fail("JavaScript context could not be allocated.");
#else // ! MAC_OR_LINUX_
                                std::cerr << "JavaScript context could not be allocated." <<
                                            std::endl;
#endif // ! MAC_OR_LINUX_
                                JS_DestroyRuntime(jrt);
                                jrt = NULL;
                            }
                        }
                        else
                        {
                            OD_LOG("! (jrt)"); //####
#if MAC_OR_LINUX_
                            GetLogger().fail("JavaScript runtime could not be allocated.");
#else // ! MAC_OR_LINUX_
                            std::cerr << "JavaScript runtime could not be allocated." << std::endl;
#endif // ! MAC_OR_LINUX_
                        }
                        if (jrt && jct)
                        {
                            setUpAndGo(jct, scriptSource, scriptPath, argv, argc, tag,
                                       serviceEndpointName, servicePortNumber, stdinAvailable,
                                       reportOnExit);
                            JS_DestroyContext(jct);
                            JS_DestroyRuntime(jrt);
                        }
                        JS_ShutDown();
                    }
                    else
                    {
                        OD_LOG("! (JS_Init())"); //####
#if MAC_OR_LINUX_
                        GetLogger().fail("JavaScript engine could not be started.");
#else // ! MAC_OR_LINUX_
                        std::cerr << "JavaScript engine could not be started." << std::endl;
#endif // ! MAC_OR_LINUX_
                    }
                }
                else
                {
                    OD_LOG("! (0 < scriptSource.size())"); //####
#if MAC_OR_LINUX_
                    GetLogger().fail("Empty script file.");
#else // ! MAC_OR_LINUX_
                    std::cerr << "Empty script file." << std::endl;
#endif // ! MAC_OR_LINUX_
                }
            }
            else
            {
# if MAC_OR_LINUX_
                GetLogger().fail("Missing script file path.");
# else // ! MAC_OR_LINUX_
                std::cerr << "Missing script file path." << std::endl;
# endif // ! MAC_OR_LINUX_
            }
        }
#if CheckNetworkWorks_
        else
        {
            OD_LOG("! (yarp::os::Network::checkNetwork(NETWORK_CHECK_TIMEOUT))"); //####
# if MAC_OR_LINUX_
            GetLogger().fail("YARP network not running.");
# else // ! MAC_OR_LINUX_
            std::cerr << "YARP network not running." << std::endl;
# endif // ! MAC_OR_LINUX_
        }
#endif // CheckNetworkWorks_
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
    }
    yarp::os::Network::fini();
    OD_LOG_EXIT_L(0); //####
    return 0;
} // main
