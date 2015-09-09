//--------------------------------------------------------------------------------------------------
//
//  File:       m+mCommonLispServiceMain.cpp
//
//  Project:    m+m
//
//  Contains:   The main application for the Common Lisp input / output service.
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
 @brief The main application for the Common Lisp input / output service. */

/*! @dir CommonLisp
 @brief The set of files that implement the Common Lisp input / output service. */
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

#if defined(__APPLE__)
# pragma mark Global constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

/*! @brief Run an arbitrary Lisp expression.
 @param call The expression to be executed.
 @returns The result of the execution. */
static cl_object doLisp(const YarpString & call)
{
    OD_LOG_ENTER(); //####
    OD_LOG_S1s("call = ", call);
    cl_env_ptr env = ecl_process_env();
    cl_object  result = cl_safe_eval(c_string_to_object(call.c_str()), ECL_NIL, ECL_NIL);

    OD_LOG_EXIT_P(result); //####
    ecl_return1(env, result);
} // doLisp

#if 0
/*! @brief The error reporter callback for the Common Lisp engine.
 @param cx The context in which the error happened.
 @param message An error message.
 @param report An error report record containing additional details about the error. */
static void reportCommonLispError(JSContext *     cx,
                                  const char *    message,
                                  JSErrorReport * report)
{
    // Note that, since this is a callback for the Common Lisp engine, it must NOT throw any C++
    // exceptions!
    try
    {
        YarpString        errMessage(report->filename ? report->filename : "[no filename]");
        std::stringstream buff;
        
        buff << report->lineno << ":" << message;
        errMessage += buff.str();
#if MAC_OR_LINUX_
        GetLogger().fail(errMessage);
#else // ! MAC_OR_LINUX_
        cerr << errMessage.c_str() << endl;
#endif // ! MAC_OR_LINUX_
    }
    catch (...)
    {
        // Suppress any C++ exception caused by this function.
    }
} // reportCommonLispError
#endif//0

/*! @brief A C-callback function for Common Lisp to send an object to a channel.
 @param channelIndex The number of the channel to be used.
 @param message The message to send to the channel. */
static cl_object sendToChannelForCl(cl_object channelIndex,
                                    cl_object message)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("channelIndex = ", channelIndex, "message = ", message); //####
    cl_env_ptr env = ecl_process_env();

    //TBD
    OD_LOG_EXIT_P(ECL_NIL); //####
    ecl_return0(env);
} // sendToChannelForCl

#if 0
/*! @brief A C-callback function for Common Lisp to send an object to a channel.
 @param jct The context in which the native function is being called.
 @param argc The number of arguments supplied to the function by the caller.
 @param vp The arguments to the function.
 @returns @c true on success and @c false otherwise. */
static bool sendToChannelForCl(JSContext * jct,
                               unsigned    argc,
                               JS::Value * vp)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("jct = ", jct, "vp = ", vp); //####
    OD_LOG_L1("argc = ", argc); //####
    bool         result = false;
    JS::CallArgs args = JS::CallArgsFromVp(argc, vp);
    
    if (2 == args.length())
    {
        // Check that the first argument is a valid integer.
        if (args[0].isInt32())
        {
            int32_t             channelSlot = args[0].toInt32();
            CommonLispService * theService =
                                reinterpret_cast<CommonLispService *>(JS_GetContextPrivate(jct));
            
            if (theService)
            {
                result = theService->sendToChannel(channelSlot, args[1]);
            }
        }
    }
    else if (2 < args.length())
    {
        JS_ReportError(jct, "Extra arguments to sendToChannel");
    }
    else
    {
        JS_ReportError(jct, "Missing argument(s) to sendToChannel");
    }
    OD_LOG_EXIT_B(result); //####
    return result;
} // sendToChannelForCl
#endif//0

/*! @brief Add custom functions to the Common Lisp environment.
 @returns @c true if the custom functions were addeded successfully and @c false otherwise. */
static void addCustomFunctions(void)
{
    OD_LOG_ENTER(); //####
    DEFUN_("sendToChannel", sendToChannelForCl, 2);
    OD_LOG_EXIT(); //####
} // addCustomFunctions

#if 0
// The following forward reference is needed since lStreamClass refers to this function and the
// function uses lStreamClass.
/*! @brief Release resources used by a Stream object.
 @param freeOp The environment in which this is being performed.
 @param obj The Stream object being released. */
static void cleanupStreamObject(JSFreeOp * freeOp,
                                JSObject * obj);

/*! @brief The class of the global object. */
static JSClass lStreamClass =
{
    "Stream",            // name
    JSCLASS_HAS_PRIVATE, // flags
    NULL,             // addProperty
    NULL,             // delProperty
    NULL,             // getProperty
    NULL,             // setProperty
    NULL,             // enumerate
    NULL,             // resolve
#if (39 < MOZJS_MAJOR_VERSION)
    NULL,             // mayResolve
#endif // 39 < MOZJS_MAJOR_VERSION
    NULL,             // convert
    cleanupStreamObject  // finalize
}; // lStreamClass

static void cleanupStreamObject(JSFreeOp * freeOp,
                                JSObject * obj)
{
    OD_LOG_ENTER(); //####
    if (&lStreamClass == JS_GetClass(obj))
    {
        FILE * aFile = reinterpret_cast<FILE *>(JS_GetPrivate(obj));
        
        if (aFile)
        {
            fclose(aFile);
            JS_SetPrivate(obj, NULL);
        }
    }
    OD_LOG_EXIT(); //####
} // cleanupStreamObject

/*! @brief A C-callback function for Common Lisp to create a Stream object.
 @param jct The context in which the native function is being called.
 @param argc The number of arguments supplied to the function by the caller.
 @param vp The arguments to the function.
 @returns @c true on success and @c false otherwise. */
static bool CreateStreamObject(JSContext * jct,
                               unsigned    argc,
                               JS::Value * vp)
{
    bool         result = false;
    JS::CallArgs args = JS::CallArgsFromVp(argc, vp);
    
    if (args.length())
    {
        cerr << "Extra arguments to Stream constructor" << endl;
    }
    else
    {
        JSObject * obj = JS_NewObjectForConstructor(jct, &lStreamClass, args);
        
        if (obj)
        {
            JS_SetPrivate(obj, NULL);
            args.rval().setObject(*obj);
            result = true;
        }
    }
    return result;
} // CreateStreamObject

/*! @brief A C-callback function for Common Lisp to check if a Stream object is at EOF.
 @param jct The context in which the native function is being called.
 @param argc The number of arguments supplied to the function by the caller.
 @param vp The arguments to the function.
 @returns @c true on success and @c false otherwise. */
static bool streamAtEofForCl(JSContext * jct,
                             unsigned    argc,
                             JS::Value * vp)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("jct = ", jct, "vp = ", vp); //####
    OD_LOG_L1("argc = ", argc); //####
    bool         result = false;
    JS::CallArgs args = JS::CallArgsFromVp(argc, vp);
    JSObject &   theThis = args.thisv().toObject();
    
    if (args.length())
    {
        JS_ReportError(jct, "Extra arguments to Stream.atEof");
    }
    else
    {
        FILE * aFile = reinterpret_cast<FILE *>(JS_GetPrivate(&theThis));
        
        if (aFile)
        {
            args.rval().setBoolean(0 != feof(aFile));
        }
        else
        {
            // If it isn't open, treat it as if it was at EOF
            args.rval().setBoolean(true);
        }
        result = true;
    }
    OD_LOG_EXIT_B(result); //####
    return result;
} // streamAtEofForCl

/*! @brief A C-callback function for Common Lisp to clear the error state of a Stream object.
 @param jct The context in which the native function is being called.
 @param argc The number of arguments supplied to the function by the caller.
 @param vp The arguments to the function.
 @returns @c true on success and @c false otherwise. */
static bool streamClearErrorForCl(JSContext * jct,
                                  unsigned    argc,
                                  JS::Value * vp)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("jct = ", jct, "vp = ", vp); //####
    OD_LOG_L1("argc = ", argc); //####
    bool         result = false;
    JS::CallArgs args = JS::CallArgsFromVp(argc, vp);
    JSObject &   theThis = args.thisv().toObject();
    
    if (args.length())
    {
        JS_ReportError(jct, "Extra arguments to Stream.clearError");
    }
    else
    {
        FILE * aFile = reinterpret_cast<FILE *>(JS_GetPrivate(&theThis));
        
        if (aFile)
        {
            clearerr(aFile);
        }
        result = true;
    }
    OD_LOG_EXIT_B(result); //####
    return result;
} // streamClearErrorForCl

/*! @brief A C-callback function for Common Lisp to close a Stream object.
 @param jct The context in which the native function is being called.
 @param argc The number of arguments supplied to the function by the caller.
 @param vp The arguments to the function.
 @returns @c true on success and @c false otherwise. */
static bool streamCloseForCl(JSContext * jct,
                             unsigned    argc,
                             JS::Value * vp)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("jct = ", jct, "vp = ", vp); //####
    OD_LOG_L1("argc = ", argc); //####
    bool         result = false;
    JS::CallArgs args = JS::CallArgsFromVp(argc, vp);
    JSObject &   theThis = args.thisv().toObject();
    
    if (args.length())
    {
        JS_ReportError(jct, "Extra arguments to Stream.close");
    }
    else
    {
        FILE * aFile = reinterpret_cast<FILE *>(JS_GetPrivate(&theThis));
        
        if (aFile)
        {
            fclose(aFile);
            JS_SetPrivate(&theThis, NULL);
        }
        result = true;
    }
    OD_LOG_EXIT_B(result); //####
    return result;
} // streamCloseForCl

/*! @brief A C-callback function for Common Lisp to check if a Stream object is in an error state.
 @param jct The context in which the native function is being called.
 @param argc The number of arguments supplied to the function by the caller.
 @param vp The arguments to the function.
 @returns @c true on success and @c false otherwise. */
static bool streamHasErrorForCl(JSContext * jct,
                                unsigned    argc,
                                JS::Value * vp)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("jct = ", jct, "vp = ", vp); //####
    OD_LOG_L1("argc = ", argc); //####
    bool         result = false;
    JS::CallArgs args = JS::CallArgsFromVp(argc, vp);
    JSObject &   theThis = args.thisv().toObject();
    
    if (args.length())
    {
        JS_ReportError(jct, "Extra arguments to Stream.hasError");
    }
    else
    {
        FILE * aFile = reinterpret_cast<FILE *>(JS_GetPrivate(&theThis));
        
        if (aFile)
        {
            args.rval().setBoolean(0 != feof(aFile));
        }
        else
        {
            // If it isn't open, treat it as if it was at EOF
            args.rval().setBoolean(true);
        }
        result = true;
    }
    OD_LOG_EXIT_B(result); //####
    return result;
} // streamHasErrorForCl

/*! @brief A C-callback function for Common Lisp to check if a Stream object is open.
 @param jct The context in which the native function is being called.
 @param argc The number of arguments supplied to the function by the caller.
 @param vp The arguments to the function.
 @returns @c true on success and @c false otherwise. */
static bool streamIsOpenForCl(JSContext * jct,
                             unsigned    argc,
                             JS::Value * vp)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("jct = ", jct, "vp = ", vp); //####
    OD_LOG_L1("argc = ", argc); //####
    bool         result = false;
    JS::CallArgs args = JS::CallArgsFromVp(argc, vp);
    JSObject &   theThis = args.thisv().toObject();
    
    if (args.length())
    {
        JS_ReportError(jct, "Extra arguments to Stream.isOpen");
    }
    else
    {
        FILE * aFile = reinterpret_cast<FILE *>(JS_GetPrivate(&theThis));
        
        args.rval().setBoolean(NULL != aFile);
        result = true;
    }
    OD_LOG_EXIT_B(result); //####
    return result;
} // streamIsOpenForCl

/*! @brief A C-callback function for Common Lisp to open a Stream object.
 @param jct The context in which the native function is being called.
 @param argc The number of arguments supplied to the function by the caller.
 @param vp The arguments to the function.
 @returns @c true on success and @c false otherwise. */
static bool streamOpenForCl(JSContext * jct,
                            unsigned    argc,
                            JS::Value * vp)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("jct = ", jct, "vp = ", vp); //####
    OD_LOG_L1("argc = ", argc); //####
    bool         result = false;
    JS::CallArgs args = JS::CallArgsFromVp(argc, vp);
    JSObject &   theThis = args.thisv().toObject();
    
    if (2 == args.length())
    {
        // Check if the Stream is already open and close it.
        FILE * aFile = reinterpret_cast<FILE *>(JS_GetPrivate(&theThis));

        if (aFile)
        {
            fclose(aFile);
            JS_SetPrivate(&theThis, NULL);
        }
        if (args[0].isString() && args[1].isString())
        {
            JSString * asString1 = args[0].toString();
            char *     asChars1 = JS_EncodeString(jct, asString1);
            JSString * asString2 = args[1].toString();
            char *     asChars2 = JS_EncodeString(jct, asString2);
            
            aFile = fopen(asChars1, asChars2);
            JS_free(jct, asChars1);
            JS_free(jct, asChars2);
            if (aFile)
            {
                JS_SetPrivate(&theThis, aFile);
                result = true;
            }
        }
    }
    else if (2 < args.length())
    {
        JS_ReportError(jct, "Extra arguments to Stream.open");
    }
    else
    {
        JS_ReportError(jct, "Missing argument(s) to Stream.open");
    }
    OD_LOG_EXIT_B(result); //####
    return result;
} // streamOpenForCl

/*! @brief A C-callback function for Common Lisp to read the next non-blank character from a Stream
 object.
 @param jct The context in which the native function is being called.
 @param argc The number of arguments supplied to the function by the caller.
 @param vp The arguments to the function.
 @returns @c true on success and @c false otherwise. */
static bool streamReadCharacterForCl(JSContext * jct,
                                     unsigned    argc,
                                     JS::Value * vp)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("jct = ", jct, "vp = ", vp); //####
    OD_LOG_L1("argc = ", argc); //####
    bool         result = false;
    JS::CallArgs args = JS::CallArgsFromVp(argc, vp);
    JSObject &   theThis = args.thisv().toObject();
    
    if (args.length())
    {
        JS_ReportError(jct, "Extra arguments to Stream.readCharacter");
    }
    else
    {
        FILE * aFile = reinterpret_cast<FILE *>(JS_GetPrivate(&theThis));
        
        if (aFile)
        {
            char aChar = '\0';
            
            if (1 == fscanf(aFile, " %c", &aChar))
            {
                JSString * outString = JS_NewStringCopyN(jct, &aChar, 1);
                
                args.rval().setString(outString);
            }
            result = true;
        }
    }
    OD_LOG_EXIT_B(result); //####
    return result;
} // streamReadCharacterForCl

/*! @brief A C-callback function for Common Lisp to read a line from a Stream object.
 @param jct The context in which the native function is being called.
 @param argc The number of arguments supplied to the function by the caller.
 @param vp The arguments to the function.
 @returns @c true on success and @c false otherwise. */
static bool streamReadLineForCl(JSContext * jct,
                                unsigned    argc,
                                JS::Value * vp)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("jct = ", jct, "vp = ", vp); //####
    OD_LOG_L1("argc = ", argc); //####
    bool         result = false;
    JS::CallArgs args = JS::CallArgsFromVp(argc, vp);
    JSObject &   theThis = args.thisv().toObject();
    
    if (args.length())
    {
        JS_ReportError(jct, "Extra arguments to Stream.readLine");
    }
    else
    {
        FILE * aFile = reinterpret_cast<FILE *>(JS_GetPrivate(&theThis));
        
        if (aFile)
        {
            bool       keepGoing = true;
            char       inBuffer[200];
            JSString * outString = JS_NewStringCopyZ(jct, "");
            
            for ( ; keepGoing; )
            {
                char * inPtr = fgets(inBuffer, sizeof(inBuffer), aFile);
            
                if (inPtr)
                {
                    JSString * thisChunk;
                    size_t     len = strlen(inBuffer);
                    
                    if ('\n' == inBuffer[len - 1])
                    {
                        // We don't want to copy the newline into the buffer.
                        thisChunk = JS_NewStringCopyN(jct, inBuffer, len - 1);
                        keepGoing = false;
                    }
                    else
                    {
                        thisChunk = JS_NewStringCopyZ(jct, inBuffer);
                    }
                    JS::RootedString leftString(jct);
                    JS::RootedString rightString(jct);
                    
                    leftString = outString;
                    rightString = thisChunk;
                    outString = JS_ConcatStrings(jct, leftString, rightString);
                }
                else
                {
                    keepGoing = false;
                }
            }
            args.rval().setString(outString);
            result = true;
        }
    }
    OD_LOG_EXIT_B(result); //####
    return result;
} // streamReadLineForCl

/*! @brief A C-callback function for Common Lisp to read the next number from a Stream object.
 @param jct The context in which the native function is being called.
 @param argc The number of arguments supplied to the function by the caller.
 @param vp The arguments to the function.
 @returns @c true on success and @c false otherwise. */
static bool streamReadNumberForCl(JSContext * jct,
                                  unsigned    argc,
                                  JS::Value * vp)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("jct = ", jct, "vp = ", vp); //####
    OD_LOG_L1("argc = ", argc); //####
    bool         result = false;
    JS::CallArgs args = JS::CallArgsFromVp(argc, vp);
    JSObject &   theThis = args.thisv().toObject();
    
    if (args.length())
    {
        JS_ReportError(jct, "Extra arguments to Stream.readNumber");
    }
    else
    {
        FILE * aFile = reinterpret_cast<FILE *>(JS_GetPrivate(&theThis));
        
        if (aFile)
        {
            double aDouble = 0;
            
            if (1 == fscanf(aFile, " %lg", &aDouble))
            {
                args.rval().setDouble(aDouble);
            }
            result = true;
        }
    }
    OD_LOG_EXIT_B(result); //####
    return result;
} // streamReadNumberForCl

/*! @brief A C-callback function for Common Lisp to read a string from a Stream object.
 @param jct The context in which the native function is being called.
 @param argc The number of arguments supplied to the function by the caller.
 @param vp The arguments to the function.
 @returns @c true on success and @c false otherwise. */
static bool streamReadStringForCl(JSContext * jct,
                                  unsigned    argc,
                                  JS::Value * vp)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("jct = ", jct, "vp = ", vp); //####
    OD_LOG_L1("argc = ", argc); //####
    bool         result = false;
    JS::CallArgs args = JS::CallArgsFromVp(argc, vp);
    JSObject &   theThis = args.thisv().toObject();
    
    if (args.length())
    {
        JS_ReportError(jct, "Extra arguments to Stream.readString");
    }
    else
    {
        FILE * aFile = reinterpret_cast<FILE *>(JS_GetPrivate(&theThis));
        
        if (aFile)
        {
            char aChar = '\0';
            
            if (1 == fscanf(aFile, " %c", &aChar))
            {
                bool       keepGoing = true;
                int        outLen = 0;
                char       outBuff[100];
                char       matchChar;
                JSString * outString = NULL;
                JSString * thisChunk = NULL;
                
                if (('"' == aChar) || ('\'' == aChar))
                {
                    matchChar = aChar;
                }
                else
                {
                    matchChar = '\0';
                    outBuff[0] = aChar;
                    outLen = 1;
                }
                for ( ; keepGoing; )
                {
                    aChar = fgetc(aFile);
                    if (EOF == aChar)
                    {
                        // Something happened.
                        keepGoing = false;
                    }
                    else if (matchChar == aChar)
                    {
                        // Reached the end-of-string.
                        keepGoing = false;
                    }
                    else if (outLen < sizeof(outBuff))
                    {
                        outBuff[outLen++] = aChar;
                    }
                    else
                    {
                        // The buffer is full.
                        if (outString)
                        {
                            // We need to concenate
                            thisChunk = JS_NewStringCopyN(jct, outBuff, outLen);
                            JS::RootedString leftString(jct);
                            JS::RootedString rightString(jct);
                            
                            leftString = outString;
                            rightString = thisChunk;
                            outString = JS_ConcatStrings(jct, leftString, rightString);
                        }
                        else
                        {
                            // This is the first chunk
                            outString = JS_NewStringCopyN(jct, outBuff, outLen);
                        }
                        outLen = 0;
                    }
                }
                // Add the remaining characters to the string.
                if (outLen)
                {
                    if (outString)
                    {
                        // We need to concenate
                        thisChunk = JS_NewStringCopyN(jct, outBuff, outLen);
                        JS::RootedString leftString(jct);
                        JS::RootedString rightString(jct);
                        
                        leftString = outString;
                        rightString = thisChunk;
                        outString = JS_ConcatStrings(jct, leftString, rightString);
                    }
                    else
                    {
                        // This is the first chunk
                        outString = JS_NewStringCopyN(jct, outBuff, outLen);
                    }
                }
                else if (! outString)
                {
                    outString = JS_NewStringCopyZ(jct, "");
                }
                args.rval().setString(outString);
            }
            result = true;
        }
    }
    OD_LOG_EXIT_B(result); //####
    return result;
} // streamReadStringForCl

/*! @brief A C-callback function for Common Lisp to reposition a Stream object to its beginning.
 @param jct The context in which the native function is being called.
 @param argc The number of arguments supplied to the function by the caller.
 @param vp The arguments to the function.
 @returns @c true on success and @c false otherwise. */
static bool streamRewindForCl(JSContext * jct,
                              unsigned    argc,
                              JS::Value * vp)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("jct = ", jct, "vp = ", vp); //####
    OD_LOG_L1("argc = ", argc); //####
    bool         result = false;
    JS::CallArgs args = JS::CallArgsFromVp(argc, vp);
    JSObject &   theThis = args.thisv().toObject();
    
    if (args.length())
    {
        JS_ReportError(jct, "Extra arguments to Stream.close");
    }
    else
    {
        FILE * aFile = reinterpret_cast<FILE *>(JS_GetPrivate(&theThis));
        
        if (aFile)
        {
            rewind(aFile);
        }
        result = true;
    }
    OD_LOG_EXIT_B(result); //####
    return result;
} // streamRewindForCl

/*! @brief A C-callback function for Common Lisp to write a value to a Stream object.
 @param jct The context in which the native function is being called.
 @param argc The number of arguments supplied to the function by the caller.
 @param vp The arguments to the function.
 @returns @c true on success and @c false otherwise. */
static bool streamWriteForCl(JSContext * jct,
                             unsigned    argc,
                             JS::Value * vp)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("jct = ", jct, "vp = ", vp); //####
    OD_LOG_L1("argc = ", argc); //####
    bool         result;
    JS::CallArgs args = JS::CallArgsFromVp(argc, vp);
    JSObject &   theThis = args.thisv().toObject();
    FILE *       aFile = reinterpret_cast<FILE *>(JS_GetPrivate(&theThis));
    
    if (aFile)
    {
        for (int ii = 0, mm = args.length(); mm > ii; ++ii)
        {
            JSString * asString = JS::ToString(jct, args[ii]);
            
            if (asString && JS_GetStringLength(asString))
            {
                char * asChars = JS_EncodeString(jct, asString);
                
                fputs(asChars, aFile);
                JS_free(jct, asChars);
            }
        }
        result = true;
    }
    else
    {
        result = false;
    }
    OD_LOG_EXIT_B(result); //####
    return result;
} // streamWriteForCl

/*! @brief A C-callback function for Common Lisp to write a value to a Stream object,
 followed by a newline.
 @param jct The context in which the native function is being called.
 @param argc The number of arguments supplied to the function by the caller.
 @param vp The arguments to the function.
 @returns @c true on success and @c false otherwise. */
static bool streamWriteLineForCl(JSContext * jct,
                                 unsigned    argc,
                                 JS::Value * vp)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("jct = ", jct, "vp = ", vp); //####
    OD_LOG_L1("argc = ", argc); //####
    bool         result;
    JS::CallArgs args = JS::CallArgsFromVp(argc, vp);
    JSObject &   theThis = args.thisv().toObject();
    FILE *       aFile = reinterpret_cast<FILE *>(JS_GetPrivate(&theThis));
    
    if (aFile)
    {
        for (int ii = 0, mm = args.length(); mm > ii; ++ii)
        {
            JSString * asString = JS::ToString(jct, args[ii]);
            
            if (asString && JS_GetStringLength(asString))
            {
                char * asChars = JS_EncodeString(jct, asString);
                
                fputs(asChars, aFile);
                JS_free(jct, asChars);
            }
        }
        fputc('\n', aFile);
        fflush(aFile);
        result = true;
    }
    else
    {
        result = false;
    }
    OD_LOG_EXIT_B(result); //####
    return result;
} // streamWriteLineForCl

/*! @brief The table of supplied functions for the %Stream class. */
static const JSFunctionSpec lStreamFunctions[] =
{
    JS_FS("atEof", streamAtEofForCl, 0, JSPROP_ENUMERATE),
    JS_FS("clearError", streamClearErrorForCl, 0, JSPROP_ENUMERATE),
    JS_FS("close", streamCloseForCl, 0, JSPROP_ENUMERATE),
    JS_FS("hasError", streamHasErrorForCl, 0, JSPROP_ENUMERATE),
    JS_FS("isOpen", streamIsOpenForCl, 0, JSPROP_ENUMERATE),
    JS_FS("open", streamOpenForCl, 2, JSPROP_ENUMERATE),
    JS_FS("readCharacter", streamReadCharacterForCl, 0, JSPROP_ENUMERATE),
    JS_FS("readLine", streamReadLineForCl, 0, JSPROP_ENUMERATE),
    JS_FS("readNumber", streamReadNumberForCl, 0, JSPROP_ENUMERATE),
    JS_FS("readString", streamReadStringForCl, 0, JSPROP_ENUMERATE),
    JS_FS("rewind", streamRewindForCl, 0, JSPROP_ENUMERATE),
    JS_FS("write", streamWriteForCl, 1, JSPROP_ENUMERATE),
    JS_FS("writeLine", streamWriteLineForCl, 1, JSPROP_ENUMERATE),
    JS_FS_END
}; // lStreamFunctions

/*! @brief Add custom classes to the Common Lisp environment.
 @param jct The Common Lisp engine context.
 @param global The Common Lisp global object.
 @returns @c true if the custom classes were addeded successfully and @c false otherwise. */
static bool addCustomClasses(JSContext *        jct,
                             JS::RootedObject & global)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("jct = ", jct, "global = ", &global); //####
    bool okSoFar = false;
    
#if (40 < MOZJS_MAJOR_VERSION)
    if (JS_InitClass(jct, global, NULL, &lStreamClass, CreateStreamObject, 0, NULL,
                     lStreamFunctions, NULL, NULL))
#else // 40 >= MOZJS_MAJOR_VERSION
    if (JS_InitClass(jct, global, JS::NullPtr(), &lStreamClass, CreateStreamObject, 0, NULL,
                     lStreamFunctions, NULL, NULL))
#endif // 40 >= MOZJS_MAJOR_VERSION
    {
        okSoFar = true;
    }
    OD_LOG_EXIT_B(okSoFar); //####
    return okSoFar;
} // addCustomClasses
#endif//0

/*! @brief Add an array containing the command-line arguments to the Common Lisp environment.
 @param ourPackage The package to be used with the new object.
 @param argv The arguments to be used with the Common Lisp input / output service. */
static void addArgvObject(cl_object                ourPackage,
                          const YarpStringVector & argv)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("ourPackage = ", ourPackage, "argv = ", &argv); //####
    cl_env_ptr env = ecl_process_env();
    cl_object  argvObject = cl_intern(2, ecl_make_simple_base_string(const_cast<char *>(ARGV_NAME_),
                                                                     sizeof(ARGV_NAME_) - 1),
                                      ourPackage);


    cl_object  argvValue = ecl_alloc_simple_vector(argv.size(), ecl_aet_object);

    cl_export(2, argvObject, ourPackage);
    ecl_setq(env, argvObject, argvValue);
    for (size_t ii = 0, argc = argv.size(); argc > ii; ++ii)
    {
        char * anArg = const_cast<char *>(argv[ii].c_str());

        ecl_aset1(argvValue, ii, ecl_make_simple_base_string(anArg, argv[ii].length()));
    }
    OD_LOG_EXIT(); //####
} // addArgvObject

/*! @brief Add a custom string object to the Common Lisp environment.
 @param ourPackage The package to be used with the new object.
 @param tag The modifier for the service name and port names. */
static void addScriptTagObject(cl_object          ourPackage,
                               const YarpString & tag)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P1("ourPackage = ", ourPackage); //####
    OD_LOG_S1s("tag = ", tag); //####
    cl_env_ptr env = ecl_process_env();
    cl_object  scriptTagObject = cl_intern(2,
                                   ecl_make_simple_base_string(const_cast<char *>(SCRIPTTAG_NAME_),
                                                               sizeof(SCRIPTTAG_NAME_) - 1),
                                           ourPackage);

    cl_export(2, scriptTagObject, ourPackage);
    ecl_setq(env, scriptTagObject, ecl_make_simple_base_string(const_cast<char *>(tag.c_str()),
                                                               tag.length()));
    OD_LOG_EXIT(); //####
} // addScriptTagObject

/*! @brief Add custom classes, functions and variables to the Common Lisp environment.
 @param tag The modifier for the service name and port names.
 @param argv The arguments to be used with the Common Lisp input / output service. */
static cl_object addCustomObjects(const YarpString &       tag,
                                  const YarpStringVector & argv)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P1("argv = ", &argv); //####
    OD_LOG_S1s("tag = ", tag); //####
    cl_object ourPackage = cl_make_package(5, c_string_to_object(MpM_COMMONLISP_PACKAGE_NAME_),
                                           c_string_to_object(":nicknames"),
                                           cl_list(1,
                                               c_string_to_object(MpM_COMMONLISP_PACKAGE_ABBREV_)),
                                           c_string_to_object(":use"),
                                           cl_list(1, c_string_to_object(":common-lisp")));
    
    addCustomFunctions();
    //addCustomClasses()
    addArgvObject(ourPackage, argv);
    addScriptTagObject(ourPackage, tag);
    OD_LOG_EXIT_P(ourPackage); //####
    return ourPackage;
} // addCustomObjects

/*! @brief Check the arity of a function.
 @param objectFunction The function to be checked.
 @param arityRequired The required arity for the function.
 @returns @c true if the function has the required arity or @c false otherwise. */
static bool checkArity(cl_object      objectFunction,
                       const uint32_t arityRequired)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P1("objectFunction = ", objectFunction); //####
    OD_LOG_LL1("arityRequired = ", arityRequired); //####
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
    OD_LOG_EXIT_B(okSoFar);
    return okSoFar;
} // checkArity

/*! @brief Check an object for a specific numeric property.
 @param propertyName The name of the property being searched for.
 @param canBeFunction @c true if the property can be a function rather than a string and @c false if
 the property must be a string.
 @param isOptional @c true if the property does not have to be present.
 @param result The value of the number, if located.
 @returns @c true on success and @c false otherwise. */
static bool getLoadedDouble(const char * propertyName,
                            const bool   canBeFunction,
                            const bool   isOptional,
                            double &     result)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P3("jct = ", jct, "anObject = ", &anObject, "result = ", &result); //####
    OD_LOG_S1("propertyName = ", propertyName); //####
    OD_LOG_B2("canBeFunction = ", canBeFunction, "isOptional = ", isOptional); //####
    bool      okSoFar = false;
    cl_object aSymbol = cl_find_symbol(1,
                                       ecl_make_simple_base_string(const_cast<char *>(propertyName),
                                                                   strlen(propertyName)));

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
                    cl_object aValue = cl_funcall(1, aFunction);

                    if (ECL_NIL != cl_realp(aValue))
                    {
                        result = ecl_to_double(aValue);
                        okSoFar = true;
                    }
                }
                else
                {
#if MAC_OR_LINUX_
                    GetLogger().fail(YarpString("Function (") + YarpString(propertyName) +
                                     ") has the incorrect number of arguments.");
#else // ! MAC_OR_LINUX_
                    cerr << "Function (" << propertyName <<
                            ") has the incorrect number of arguments." << endl;
#endif // ! MAC_OR_LINUX_
                }
            }
        }
        else if (isOptional)
        {
            okSoFar = true;
        }
        else
        {
#if MAC_OR_LINUX_
            GetLogger().fail("Problem searching for a property.");
#else // ! MAC_OR_LINUX_
            cerr << "Problem searching for a property." << endl;
#endif // ! MAC_OR_LINUX_
        }
    }
    OD_LOG_EXIT_B(okSoFar); //####
    return okSoFar;
} // getLoadedDouble

/*! @brief Check an object for a specific string property.
 @param propertyName The name of the property being searched for.
 @param canBeFunction @c true if the property can be a function rather than a string and @c false if
 the property must be a string.
 @param isOptional @c true if the property does not have to be present.
 @param result The value of the string, if located.
 @returns @c true on success and @c false otherwise. */
static bool getLoadedString(const char * propertyName,
                            const bool   canBeFunction,
                            const bool   isOptional,
                            YarpString & result)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P1("result = ", &result); //####
    OD_LOG_S1("propertyName = ", propertyName); //####
    OD_LOG_B2("canBeFunction = ", canBeFunction, "isOptional = ", isOptional); //####
    bool      okSoFar = false;
    cl_object aSymbol = cl_find_symbol(1,
                                       ecl_make_simple_base_string(const_cast<char *>(propertyName),
                                                                   strlen(propertyName)));

    if (ECL_NIL != aSymbol)
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
                    cl_object aValue = cl_funcall(1, aFunction);

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
                else
                {
#if MAC_OR_LINUX_
                    GetLogger().fail(YarpString(YarpString("Function (") +
                                                YarpString(propertyName) +
                                                ") has the incorrect number of arguments."));
#else // ! MAC_OR_LINUX_
                    cerr << "Function (" << propertyName <<
                            ") has the incorrect number of arguments." << endl;
#endif // ! MAC_OR_LINUX_
                }
            }
        }
        else if (isOptional)
        {
            okSoFar = true;
        }
        else
        {
#if MAC_OR_LINUX_
            GetLogger().fail("Problem searching for a property.");
#else // ! MAC_OR_LINUX_
            cerr << "Problem searching for a property." << endl;
#endif // ! MAC_OR_LINUX_
        }
    }
    OD_LOG_EXIT_B(okSoFar); //####
    return okSoFar;
} // getLoadedString

/*! @brief Check an object for a specific string property.
 @param propertyName The name of the property being searched for.
 @param canBeFunction @c true if the property can be a function rather than a string and @c false if
 the property must be a string.
 @param isOptional @c true if the property does not have to be present.
 @param result The value of the string, if located.
 @returns @c true on success and @c false otherwise. */
static bool getLoadedFunctionRef(const char *   propertyName,
                                 const uint32_t arity,
                                 cl_object &    result)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P1("result = ", &result); //####
    OD_LOG_S1("propertyName = ", propertyName); //####
    OD_LOG_LL1("arity = ", arity); //####
    bool      okSoFar = false;
    cl_object aSymbol = cl_find_symbol(1,
                                       ecl_make_simple_base_string(const_cast<char *>(propertyName),
                                                                   strlen(propertyName)));

    if (ECL_NIL != aSymbol)
    {
        if (ECL_NIL == cl_fboundp(aSymbol))
        {
#if MAC_OR_LINUX_
            GetLogger().fail("Problem searching for a property.");
#else // ! MAC_OR_LINUX_
            cerr << "Problem searching for a property." << endl;
#endif // ! MAC_OR_LINUX_
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
#if MAC_OR_LINUX_
                    GetLogger().fail(YarpString(YarpString("Function (") +
                                                YarpString(propertyName) +
                                                ") has the incorrect number of arguments."));
#else // ! MAC_OR_LINUX_
                    cerr << "Function (" << propertyName <<
                            ") has the incorrect number of arguments." << endl;
#endif // ! MAC_OR_LINUX_
                }
            }
        }
    }
    OD_LOG_EXIT_B(okSoFar); //####
    return okSoFar;
} // getLoadedFunctionRef

/*! @brief Check a stream description.
 @param anElement The stream description object to be checked.
 @param inletHandlers non-@c NULL if there must be a handler for the stream description.
 @param description The validated stream description.
 @returns @c true on success and @c false otherwise. */
static bool processStreamDescription(cl_object            anElement,
                                     ObjectVector *       inletHandlers,
                                     ChannelDescription & description)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P3("anElement = ", &anElement, "inletHandlers = ", inletHandlers, //####
              "description = ", &description); //####
    bool okSoFar;

    if (ECL_NIL == cl_hash_table_p(anElement))
    {
        okSoFar = false;
    }
    else
    {
        cl_env_ptr env = ecl_process_env();
        cl_object  aSymbol = cl_find_symbol(1,
                                        ecl_make_simple_base_string(const_cast<char *>(NAME_NAME_),
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
            aSymbol = cl_find_symbol(1,
                                     ecl_make_simple_base_string(const_cast<char *>(PROTOCOL_NAME_),
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
            aSymbol = cl_find_symbol(1,
                                     ecl_make_simple_base_string(const_cast<char *>(PROTOCOL_NAME_),
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
            aSymbol = cl_find_symbol(1,
                         ecl_make_simple_base_string(const_cast<char *>(PROTOCOLDESCRIPTION_NAME_),
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
                    description._portProtocol = reinterpret_cast<char *>(aValue->base_string.self);
                }
            }
        }
        if (okSoFar && inletHandlers)
        {
            aSymbol = cl_find_symbol(1,
                                     ecl_make_simple_base_string(const_cast<char *>(HANDLER_NAME_),
                                                                 sizeof(HANDLER_NAME_) - 1));
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
    OD_LOG_EXIT_B(okSoFar); //####
    return okSoFar;
} // processStreamDescription

/*! @brief Check the Common Lisp environment for a specific array variable containing stream
 descriptions.
 @param arrayName The name of the array variable being searched for.
 @param inletHandlers non-@c NULL if there must be a handler for each stream description.
 @param streamDescriptions The list of loaded stream descriptions.
 @returns @c true on success and @c false otherwise. */
static bool getLoadedStreamDescriptions(const char *    arrayName,
                                        ObjectVector *  inletHandlers,
                                        ChannelVector & streamDescriptions)
{
    OD_LOG_ENTER(); //####
    OD_LOG_P2("inletHandlers = ", inletHandlers, "streamDescriptions = ", //####
              &streamDescriptions); //####
    OD_LOG_S1("arrayName = ", arrayName); //####
    bool      okSoFar = false;
    cl_object descriptionArray = ECL_NIL;
    cl_object aSymbol = cl_find_symbol(1,
                                       ecl_make_simple_base_string(const_cast<char *>(arrayName),
                                                                   strlen(arrayName)));

    streamDescriptions.clear();
    if (ECL_NIL != aSymbol)
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
                    descriptionArray = cl_funcall(1, aFunction);
                }
                else
                {
#if MAC_OR_LINUX_
                    GetLogger().fail(YarpString(YarpString("Function (") + YarpString(arrayName) +
                                                ") has the incorrect number of arguments."));
#else // ! MAC_OR_LINUX_
                    cerr << "Function (" << arrayName <<
                            ") has the incorrect number of arguments." << endl;
#endif // ! MAC_OR_LINUX_
                }
            }
        }
        else
        {
#if MAC_OR_LINUX_
            GetLogger().fail("Problem searching for a property.");
#else // ! MAC_OR_LINUX_
            cerr << "Problem searching for a property." << endl;
#endif // ! MAC_OR_LINUX_
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
    OD_LOG_EXIT_B(okSoFar); //####
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
 @returns @c true on success and @c false otherwise. */
static bool validateLoadedScript(bool &          sawThread,
                                 YarpString &    description,
                                 YarpString &    helpString,
                                 ChannelVector & loadedInletDescriptions,
                                 ChannelVector & loadedOutletDescriptions,
                                 ObjectVector &  loadedInletHandlers,
                                 cl_object &     loadedStartingFunction,
                                 cl_object &     loadedStoppingFunction,
                                 cl_object &     loadedThreadFunction,
                                 double &        loadedInterval)
{
    OD_LOG_ENTER();
    OD_LOG_P2("sawThread = ", &sawThread, "description = ", &description); //####
    OD_LOG_P4("helpString = ", &helpString, "loadedInletDescriptions = ", //####
              &loadedInletDescriptions, "loadedOutletDescriptions = ", //####
              &loadedOutletDescriptions, "loadedInletHandlers = ", &loadedInletHandlers); //####
    OD_LOG_P4("loadedStartingFunction = ", &loadedStartingFunction, //####
              "loadedStoppingFunction = ", &loadedStoppingFunction, //####
              "loadedThreadFunction = ", &loadedThreadFunction, "loadedInterval = ", //####
              &loadedInterval); //####
    bool okSoFar;

    sawThread = false;
    loadedInterval = 1.0;
    loadedThreadFunction = ECL_NIL;
    okSoFar = getLoadedString("SCRIPTDESCRIPTION", true, false, description);
    if (okSoFar)
    {
        okSoFar = getLoadedString("SCRIPTHELP", false, true, helpString);
    }
    if (okSoFar)
    {
        if (getLoadedFunctionRef("SCRIPTTHREAD", 0, loadedThreadFunction))
        {
//            cout << "function scriptThread defined" << endl;
            sawThread = true;
        }
    }
    if (okSoFar && (! sawThread))
    {
        okSoFar = getLoadedStreamDescriptions("SCRIPTINLETS", &loadedInletHandlers,
                                              loadedInletDescriptions);
    }
    if (okSoFar)
    {
        okSoFar = getLoadedStreamDescriptions("SCRIPTOUTLETS", NULL, loadedOutletDescriptions);
    }
    if (okSoFar)
    {
        loadedStartingFunction = ECL_NIL;
        loadedStoppingFunction = ECL_NIL;
        if (getLoadedFunctionRef("SCRIPTSTARTING", 0, loadedStartingFunction))
        {
//            cout << "function scriptStarting defined" << endl;
        }
        if (getLoadedFunctionRef("SCRIPTSTOPPING", 0, loadedStoppingFunction))
        {
//            cout << "function scriptStopping defined" << endl;
        }
    }
    if (okSoFar && sawThread)
    {
        okSoFar = getLoadedDouble("SCRIPTINTERVAL", true, true, loadedInterval);
    }
    OD_LOG_EXIT_B(okSoFar);
    return okSoFar;
} // validateLoadedScript

/*! @brief Set up the environment and start the Common Lisp service.
 @param argumentList Descriptions of the arguments to the executable.
 @param scriptPath The script file to be processed.
 @param arguments The arguments for the service.
 @param progName The path to the executable.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used with the Common Lisp service.
 @param tag The modifier for the service name and port names.
 @param serviceEndpointName The YARP name to be assigned to the new service.
 @param servicePortNumber The port being used by the service. 
 @param goWasSet @c true if the service is to be started immediately.
 @param nameWasSet @c true if the endpoint name was set and @c false otherwise.
 @param reportOnExit @c true if service metrics are to be reported on exit and @c false otherwise.
 @param stdinAvailable @c true if running in the foreground and @c false otherwise. */
static void setUpAndGo(const Utilities::DescriptorVector & argumentList,
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
    OD_LOG_ENTER(); //####
    OD_LOG_P3("argumentList = ", &argumentList, "arguments = ", &arguments, "argv = ", argv); //####
    OD_LOG_S4s("scriptPath = ", scriptPath, "progName = ", progName, "tag = ", tag, //####
               "serviceEndpointName = ", serviceEndpointName); //####
    OD_LOG_S1s("servicePortNumber = ", servicePortNumber); //####
    OD_LOG_LL1("argc = ", argc); //####
    OD_LOG_B4("goWasSet = ", goWasSet, "nameWasSet = ", nameWasSet, //####
              "reportOnExit = ", reportOnExit, "stdinAvailable = ", stdinAvailable); //####
    bool          okSoFar = true;
    bool          sawThread;
    ChannelVector loadedInletDescriptions;
    ChannelVector loadedOutletDescriptions;
    double        loadedInterval;
    YarpString    description;
    YarpString    helpText;
    ObjectVector  loadedInletHandlers;
    cl_object     loadedStartingFunction = ECL_NIL;
    cl_object     loadedStoppingFunction = ECL_NIL;
    cl_object     loadedThreadFunction = ECL_NIL;

    cl_boot(argc, argv);
    atexit(cl_shutdown);
    // Set up our functions and objects before loading the script.
    cl_env_ptr env = ecl_process_env();
    cl_object  ourPackage = addCustomObjects(tag, arguments);
    cl_object  errorSymbol = ecl_make_symbol("ERROR", "CL");

    // Load the script!
    loadedInletHandlers.clear();
    ECL_RESTART_CASE_BEGIN(env, ecl_list1(errorSymbol))
    {
        /* This form is evaluated with bound handlers. */
        cl_object pathToUse = ecl_make_simple_base_string(const_cast<char *>(scriptPath.c_str()),
                                                          scriptPath.length());

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
#if MAC_OR_LINUX_
        GetLogger().fail("Script aborted during load.");
#else // ! MAC_OR_LINUX_
        cerr << "Script aborted during load." << endl;
#endif // ! MAC_OR_LINUX_
    }
    ECL_RESTART_CASE_END;
    if (okSoFar)
    {
        // Check for the functions / strings that we need.
        if (validateLoadedScript(sawThread, description, helpText, loadedInletDescriptions,
                                 loadedOutletDescriptions, loadedInletHandlers,
                                 loadedStartingFunction, loadedStoppingFunction,
                                 loadedThreadFunction, loadedInterval))
        {
            CommonLispService * aService = new CommonLispService(argumentList, scriptPath, argc,
                                                                 argv, tag, description,
                                                                 loadedInletDescriptions,
                                                                 loadedOutletDescriptions,
                                                                 loadedInletHandlers,
                                                                 loadedStartingFunction,
                                                                 loadedStoppingFunction,
                                                                 sawThread, loadedThreadFunction,
                                                                 loadedInterval,
                                                                 serviceEndpointName,
                                                                 servicePortNumber);

            if (aService)
            {
                aService->performLaunch(helpText, goWasSet, stdinAvailable, reportOnExit);
                delete aService;
            }
            else
            {
                OD_LOG("! (aService)"); //####
            }
        }
        else
        {
            OD_LOG("! (validateLoadedScript(sawThread, description, helpText, " //####
                   "loadedInletDescriptions, loadedOutletDescriptions, " //####
                   "loadedInletHandlers, loadedStartingFunction, loadedStoppingFunction, " //####
                   "loadedThreadFunction, loadedInterval))"); //####
            okSoFar = false;
#if MAC_OR_LINUX_
            GetLogger().fail("Script is missing one or more functions or variables.");
#else // ! MAC_OR_LINUX_
            cerr << "Script is missing one or more functions or variables." << endl;
#endif // ! MAC_OR_LINUX_
        }
    }
    OD_LOG_EXIT(); //####
} // setUpAndGo

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

/*! @brief The entry point for running the Common Lisp input / output service.

 The first argument is the path of the script to be run by the service.
 @param argc The number of arguments in 'argv'.
 @param argv The arguments to be used with the Common Lisp input / output service.
 @returns @c 0 on a successful test and @c 1 on failure. */
int main(int      argc,
         char * * argv)
{
    YarpString progName(*argv);

#if defined(MpM_ServicesLogToStandardError)
    OD_LOG_INIT(progName.c_str(), kODLoggingOptionIncludeProcessID | //####
                kODLoggingOptionIncludeThreadID | kODLoggingOptionWriteToStderr | //####
                kODLoggingOptionEnableThreadSupport); //####
#else // ! defined(MpM_ServicesLogToStandardError)
    OD_LOG_INIT(progName.c_str(), kODLoggingOptionIncludeProcessID | //####
                kODLoggingOptionIncludeThreadID | kODLoggingOptionEnableThreadSupport); //####
#endif // ! defined(MpM_ServicesLogToStandardError)
    OD_LOG_ENTER(); //####
#if MAC_OR_LINUX_
    SetUpLogger(progName);
#endif // MAC_OR_LINUX_
    try
    {
        bool                                  goWasSet = false;
        bool                                  nameWasSet = false;
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
                                          DEFAULT_COMMONLISP_SERVICE_NAME_,
                                          COMMONLISPFILTER_SERVICE_DESCRIPTION_, "", 2015,
                                          STANDARD_COPYRIGHT_NAME_, goWasSet, nameWasSet,
                                          reportOnExit, tag, serviceEndpointName, servicePortNumber,
                                          kSkipNone, &arguments))
        {
			Utilities::SetUpGlobalStatusReporter();
			Utilities::CheckForNameServerReporter();
            if (Utilities::CheckForValidNetwork())
            {
                yarp::os::Network yarp; // This is necessary to establish any connections to the
                                        // YARP infrastructure
                
                Initialize(progName);
                if (Utilities::CheckForRegistryService())
                {
                    YarpString scriptPath(firstArg.getCurrentValue());
                    YarpString tagModifier =
                                Utilities::GetFileNameBase(Utilities::GetFileNamePart(scriptPath));
                    
                    if (0 < tagModifier.length())
                    {
                        char lastChar = tagModifier[tagModifier.length() - 1];
                        
                        // Drop a trailing period, if present.
                        if ('.' == lastChar)
                        {
                            tagModifier = tagModifier.substr(0, tagModifier.length() - 1);
                        }
                    }
                    if (! nameWasSet)
                    {
                        serviceEndpointName += YarpString("/") + tagModifier;
                    }
                    if (0 < tag.length())
                    {
                        tag += YarpString(":") + tagModifier;
                    }
                    else
                    {
                        tag = tagModifier;
                    }
                    setUpAndGo(argumentList, scriptPath, arguments, progName, argc, argv, tag,
                               serviceEndpointName, servicePortNumber, goWasSet, nameWasSet,
                               reportOnExit, stdinAvailable);
                }
                else
                {
                    OD_LOG("! (Utilities::CheckForRegistryService())"); //####
#if MAC_OR_LINUX_
                    GetLogger().fail("Registry Service not running.");
#else // ! MAC_OR_LINUX_
                    cerr << "Registry Service not running." << endl;
#endif // ! MAC_OR_LINUX_
                }
            }
            else
            {
                OD_LOG("! (Utilities::CheckForValidNetwork())"); //####
#if MAC_OR_LINUX_
                GetLogger().fail("YARP network not running.");
#else // ! MAC_OR_LINUX_
                cerr << "YARP network not running." << endl;
#endif // ! MAC_OR_LINUX_
            }
			Utilities::ShutDownGlobalStatusReporter();
		}
    }
    catch (...)
    {
        OD_LOG("Exception caught"); //####
    }
    yarp::os::Network::fini();
    OD_LOG_EXIT_L(0); //####
    return 0;
} // main
