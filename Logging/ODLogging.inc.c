//--------------------------------------------------------------------------------------
//
//  File:       ODLogging.inc.c
//
//  Project:    MoAndMe
//
//  Contains:   The function definitions for the logging facility.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2013 by OpenDragon.
//
//              All rights reserved. Redistribution and use in source and binary forms,
//              with or without modification, are permitted provided that the following
//              conditions are met:
//                * Redistributions of source code must retain the above copyright
//                  notice, this list of conditions and the following disclaimer.
//                * Redistributions in binary form must reproduce the above copyright
//                  notice, this list of conditions and the following disclaimer in the
//                  documentation and/or other materials provided with the
//                  distribution.
//                * Neither the name of the copyright holders nor the names of its
//                  contributors may be used to endorse or promote products derived
//                  from this software without specific prior written permission.
//
//              THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
//              "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
//              LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
//              PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
//              OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
//              SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
//              LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
//              DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
//              THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
//              (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
//              OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//
//  Created:    2013-04-07
//
//--------------------------------------------------------------------------------------

#if (! defined(ODLOGGING_INC_C_))
/*! @brief Header guard. */
# define ODLOGGING_INC_C_ /* */

# include "ODLogging.h"
# if (defined(__APPLE__) || defined(__linux))
#  include <pthread.h>
#  include <unistd.h>
# endif // defined(__APPLE__) || defined(__linux)
# include <stdlib.h>
# include <string.h>
# include <ctype.h>
# include <errno.h>
# include <stdio.h>
# if defined(__OBJC__)
#  import "Foundation/NSObjCRuntime.h"
# elif (defined(__APPLE__) || defined(__linux))
#  include <syslog.h>
# endif // defined(__APPLE__) || defined(__linux)

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 
 @brief The function definitions for the logging facility. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

# if defined(__cplusplus)
/*! @brief Make sure that the function will have a 'C'-style external name. */
#  define EXTERN_C extern "C"
# else // ! defined(__cpluplus)
/*! @brief Make sure that the function will have a 'C'-style external name. */
#  define EXTERN_C /* */
# endif // ! defined(__cplusplus)

# if defined(OD_ENABLE_LOGGING)
/*! @brief The number of bytes to be displayed per line when logging a memory region. */
#  define OD_MAX_BYTES_IN_LINE_ 16
/*! @brief The maximum size of the output displayed per line when logging a memory region. */
#  define OD_MAX_CHARS_IN_LINE_ (OD_MAX_BYTES_IN_LINE_ * 4)
/*! @brief The initial indentation level. */
#  define OD_LOG_INIT_VALUE_ 1
/*! @brief The @c syslog facility to be used for logging. */
#  define OD_LOG_LOG_TARGET_ LOG_LOCAL7
/*! @brief The @c syslog level to be used for logging. */
#  define OD_LOG_LOG_LEVEL_  LOG_NOTICE

/*! @brief The format to be used to log the prefix and indentation strings with functions. */
#  define OD_FUNC_FORMAT_ \
            "%s%s" OD_FUNC_WHERE_
/*! @brief The standard parameters to be used with each logged message for functions. */
#  define OD_FUNC_PREFIX_(file_) \
            prefixString_, indentString_, funcName, file_, lineNumber
/*! @brief The format to be used to log the function name, file name and line number. */
#  define OD_FUNC_WHERE_ \
            "%s{%s@%04d}"

/*! @brief The format to be used to log the prefix and indentation strings with methods. */
#  define OD_METHOD_FORMAT_ \
            "%s%s" OD_METHOD_WHERE_
/*! @brief The standard parameters to be used with each logged message for methods. */
#  define OD_METHOD_PREFIX_(file_) \
            prefixString_, indentString_, funcName, file_, lineNumber, objPtr
/*! @brief The format to be used to log the method name, file name and line number. */
#  define OD_METHOD_WHERE_ \
            "%s{%s@%04d}[%p]"

/*! @brief The standard variable used to hold the indentation string. */
#  define OD_CREATE_INDENT_() \
            char * indentString_ = odBuildIndent_()
/*! @brief The standard variable used to hold the prefix string. */
#  define OD_CREATE_PREFIX_() \
            char * prefixString_ = odBuildPrefix_()
/*! @brief The standard code to release the memory associated with the indentation string. */
#  define OD_FREE_INDENT_() \
            free(indentString_)
/*! @brief The standard code to release the memory associated with the prefix string. */
#  define OD_FREE_PREFIX_() \
            free(prefixString_)

#  if defined(WIN32)
/*! @brief The output stream to use for logging with Windows. */
#   define OD_LOG_STREAM_ stderr
#  endif // defined(WIN32)

/*! @brief The data associated with each thread for logging. */
typedef struct tOdThreadData_
{
    /*! @brief The thread indentation level. */
    int _indentLevel;
} tOdThreadData;

#  if (! defined(WIN32))
/*! @brief @c true if thread support is enabled and @c false if all threads share data. */
static bool lOdEnableThreadSupport_ = false;
/*! @brief @c true if the process identifier is to be logged and @c false otherwise. */
static bool lOdIncludeProcessID_ = false;
/*! @brief @c true if the thread identifier is to be logged and @c false otherwise. */
static bool lOdIncludeThreadID_ = false;
#  endif // ! defined(WIN32)

/*! @brief The stream to be used when logging to a file. */
static FILE * lOdLogFile_ = NULL;

#  if (! defined(WIN32))
/*! @brief The thread key to be used. */
static pthread_key_t  lOdThreadSpecificKey_;
/*! @brief The once-only variable to be used with threading. */
static pthread_once_t lOdThreadSpecificKeyOnce_ = PTHREAD_ONCE_INIT;
#  endif // ! defined(WIN32)

/*! @brief The thread data to be used when thread support is not enabled. */
static tOdThreadData lOdThreadData_ = { OD_LOG_INIT_VALUE_ };

/*! @brief Return a string corresponding to each @c bool value.
 @param val The input value.
 @returns Either "true" or "false", depending on the input value. */
inline static const char * odBoolToString_(const bool val)
{
    return (val ? "true" : "false");
} // odBoolToString_

#  if defined(__OBJC__)
/*! @brief Return either the @c description string for an object or a fixed string.
 @param value The input object.
 @returns The description of the object or "<>". */
static const char * odNullOrDescription(id value)
{
    const char * result;
    
    if (value)
    {
        result = [[value description] UTF8String];
    }
    else
    {
        result = "<>";
    }
    return result;
} // odNullOrDescription
#  endif // defined(__OBJC__)

/*! @brief Return either the input string or a fixed string, if the input is @c NULL.
 @param aString The input string.
 @returns The input string or "<>". */
static const char * odNullOrString(const char * aString)
{
    const char * result;
    
    if (aString)
    {
        result = aString;
    }
    else
    {
        result = "<>";
    }
    return result;
} // odNullOrString

#  if (! defined(WIN32))
/*! @brief Release the data associated with a thread.
 @param data A pointer to the data to be released. */
static void odReleaseThreadSpecificData_(void * data)
{
    tOdThreadData * stuff = (tOdThreadData *) data;
    
    pthread_setspecific(lOdThreadSpecificKey_, NULL);
    free(stuff);
} // odReleaseThreadSpecificData_
#  endif // ! defined(WIN32)

#  if (! defined(WIN32))
/*! @brief Create the thread key and record the 'release' function for thread-specific data. */
static void odSetUpThreadKey_(void)
{
    if (pthread_key_create(&lOdThreadSpecificKey_, odReleaseThreadSpecificData_))
    {
#   if defined(__OBJC__)
        NSLog(@"problem creating thread-specific key => %d", errno);
#   else // ! defined(__OBJC__)
        syslog(OD_LOG_LOG_LEVEL_, "problem creating thread-specific key => %d", errno);
#   endif // ! defined(__OBJC__)
    }
} // odSetUpThreadKey_
#  endif // ! defined(WIN32)

/*! @brief Get a pointer to the thread-specific data.
 
 If this is the first call for a thread, create the data. If threading is not supported, return the address of the
 shared thread data.
 @returns A pointer to the thread-specific data. */
inline static tOdThreadData * odGetThreadData_(void)
{
    tOdThreadData * stuff;
    
#  if (! defined(WIN32))
    if (lOdEnableThreadSupport_)
    {
        pthread_once(&lOdThreadSpecificKeyOnce_, odSetUpThreadKey_);
        stuff = (tOdThreadData *) pthread_getspecific(lOdThreadSpecificKey_);
        if (! stuff)
        {
            stuff = (tOdThreadData *) malloc(sizeof(tOdThreadData));
            stuff->_indentLevel = OD_LOG_INIT_VALUE_;
            pthread_setspecific(lOdThreadSpecificKey_, stuff);
        }
    }
    else
#  endif // ! defined(WIN32)
    {
        stuff = &lOdThreadData_;
    }
    return stuff;
} // odGetThreadData_

/*! @brief Return the portion of a file name that does not include the path to the file.
 @param fileName The input file path.
 @returns The file name part of a file path. */
static const char * odFileNameRoot_(const char * fileName)
{
    char * result = strrchr(fileName, '/');
    
    return (result ? (result + 1) : fileName);
} // odFileNameRoot_

/*! @brief Return the current indentation level for the active thread.
 @returns The current indentation level for the active thread. */
inline static int odGetIndent_(void)
{
    tOdThreadData * stuff = odGetThreadData_();
    
    return stuff->_indentLevel;
} // odGetIndent_

/*! @brief Set the current indentation level for the active thread.
 @param value The new indentation level. */
inline static void odSetIndent_(const int value)
{
    tOdThreadData * stuff = odGetThreadData_();
    
    stuff->_indentLevel = value;
} // odSetIndent_

/*! @brief Generate an indentation string.
 @returns A string of alternating spaces and periods that whose length matches the current indentation level for the
 active thread. */
static char * odBuildIndent_(void)
{
    int    level = odGetIndent_();
    int    length = ((level > 0) ? level : 1);
    char * result = (char *) malloc((size_t) (length + 1));
    
    for (int ii = 0; ii < length; ++ii)
    {
        result[ii] = ((ii & 1) ? '.' : ' ');
    }
    result[length] = '\0';
    return result;
} // odBuildIndent_

/*! @brief Generate a prefix string.
 @returns A string containing the process identifier and / or the thread identifier, if they are enabled for logging. */
static char * odBuildPrefix_(void)
{
    char *              result;
    size_t              length;
#  if (! defined(WIN32))
    static const size_t lengthHexString = 20; // Enough digits for an 8-byte value in hexadecimal, plus a little more.
#  endif // ! defined(WIN32)
    
#  if (! defined(WIN32))
    if (lOdIncludeProcessID_)
    {
        if (lOdIncludeThreadID_)
        {
            length = (2 * lengthHexString) + 3; // '<', ':', '>'
            result = (char *) malloc(length + 1);
            snprintf(result, length, "<%#lx:%#lx>", (long unsigned) getpid(), (long unsigned) pthread_self());
        }
        else
        {
            length = lengthHexString + 2; // '<', '>'
            result = (char *) malloc(length + 1);
            snprintf(result, length, "<%#lx>", (long unsigned) getpid());
        }
    }
    else if (lOdIncludeThreadID_)
    {
        length = lengthHexString + 2; // '<', '>'
        result = (char *) malloc(length + 1);
        snprintf(result, length, "<%#lx>", (long unsigned) pthread_self());
    }
    else
#  endif // ! defined(WIN32)
    {
        length = 0;
        result = (char *) malloc(length + 1);
    }
    result[length] = '\0';
    return result;
} // odBuildPrefix_

/*! @brief Reduce the current indentation level for the active thread. */
inline static void odDecreaseIndent_(void)
{
    odSetIndent_(odGetIndent_() - 1);
} // odDecreaseIndent_

/*! @brief Increase the current indentation level for the active thread. */
inline static void odIncreaseIndent_(void)
{
    odSetIndent_(odGetIndent_() + 1);
} // odIncreaseIndent_

/*! @brief Write the date and time to a file stream. */
static void odWriteTime_(FILE * outFile)
{
    char   buffer[80];
    time_t rawtime;
    
    time(&rawtime);
    strftime(buffer, sizeof(buffer), "%F %T ", localtime(&rawtime));
    fputs(buffer, outFile);
} // odWriteTime_

/*! @brief The value prefix string to be used when exiting a function or method. */
#  define OD_EXIT_VALUE_       "exit -> "
/*! @brief The format string to be used with a single boolean value. */
#  define OD_FORMAT_B1_        " %s%s"
/*! @brief The format string to be used with a pair of boolean values. */
#  define OD_FORMAT_B2_        " %s%s, %s%s"
/*! @brief The format string to be used with a single character value. */
#  define OD_FORMAT_C1_        " %s'%c'(%#02X)"
/*! @brief The format string to be used with a pair of character values. */
#  define OD_FORMAT_C2_        " %s'%c'(%#02X), %s'%c'(%#02X)"
/*! @brief The format string to be used with a single double value. */
#  define OD_FORMAT_D1_        " %s%g"
/*! @brief The format string to be used with a pair of double values. */
#  define OD_FORMAT_D2_        " %s%g, %s%g"
/*! @brief The message string to be used when entering a function or method. */
#  define OD_FORMAT_ENTER_     " enter"
/*! @brief The message string to be used when exiting a function or method. */
#  define OD_FORMAT_EXIT_      " exit"
/*! @brief The format string to be used when exiting a function or method via @c exit. */
#  define OD_FORMAT_EXIT_CALL_ " call exit(%ld(%#lx))"
/*! @brief The format string to be used when exiting a function or method via @c throw of an integer. */
#  define OD_FORMAT_EXIT_T_L_  " throw(%d(%#x))"
/*! @brief The format string to be used when exiting a function or method via @c throw of a string. */
#  define OD_FORMAT_EXIT_T_S_  " throw(%s)"
/*! @brief The format string to be used with an IP address and port value. */
#  define OD_FORMAT_IP_        " %s%d.%d.%d.%d:%d"
/*! @brief The format string to be used with a single long value. */
#  define OD_FORMAT_L1_        " %s%d(%#x)"
/*! @brief The format string to be used with a pair of long values. */
#  define OD_FORMAT_L2_        " %s%d(%#x), %s%d(%#x)"
/*! @brief The format string to be used with a single long long value. */
#  define OD_FORMAT_LL1_       " %s%lld(%#llx)"
/*! @brief The format string to be used with a pair of long long values. */
#  define OD_FORMAT_LL2_       " %s%lld(%#llx), %s%lld(%#llx)"
/*! @brief The format string to be used with a message. */
#  define OD_FORMAT_LOG_       " %s"
/*! @brief The format string to be used with a long string value. */
#  define OD_FORMAT_LS         " %s | %s |"
/*! @brief The format string to be used with a single object value. */
#  define OD_FORMAT_O1_        " %s%s"
/*! @brief The format string to be used with a pair of object values. */
#  define OD_FORMAT_O2_        " %s%s, %s%s"
/*! @brief The format string to be used with a single pointer value. */
#  define OD_FORMAT_P1_        " %s%p"
/*! @brief The format string to be used with a pair of pointer values. */
#  define OD_FORMAT_P2_        " %s%p, %s%p"
/*! @brief The format string to be used for the heading when logging a memory region. */
#  define OD_FORMAT_PACKET_1_  " address = %p, size = %d"
/*! @brief The format string to be used when logging a section of a memory region. */
#  define OD_FORMAT_PACKET_2_  " %s | %04X : %s| %s"
/*! @brief The format string to be used with a rectangle value. */
#  define OD_FORMAT_RECT_      " %s[l: %g, t: %g, h: %g, w: %g]"
/*! @brief The format string to be used with a single string value. */
#  define OD_FORMAT_S1_        " %s'%s'"
/*! @brief The format string to be used with a pair of string values. */
#  define OD_FORMAT_S2_        " %s'%s', %s'%s'"
/*! @brief The format string to be used with a (possibly unterminated) string value. */
#  define OD_FORMAT_SP_        " %s'%.*s'"
/*! @brief The format string to be used with a time value. */
#  define OD_FORMAT_TI_        " %s%ld:%ld"
/*! @brief The message string to be used when setting up logging for the first time. */
#  define OD_INIT_FORMAT_      "* %s%s" OD_FUNC_WHERE_ " started *"

EXTERN_C void ODLog_(const char * fileName,
                     const char * funcName,
                     const int    lineNumber,
                     const char * text)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_LOG_ "\n", OD_FUNC_PREFIX_(rootName), text ? text : "");
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(WIN32)
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_FUNC_FORMAT_ OD_FORMAT_LOG_ "\n", OD_FUNC_PREFIX_(rootName), text ? text : "");
        fflush(OD_LOG_STREAM_);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_LOG_, OD_FUNC_PREFIX_(rootName), text ? text : "");
#   else // ! defined(__OBJC__)
        syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_LOG_, OD_FUNC_PREFIX_(rootName), text ? text : "");
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLog_

EXTERN_C void ODLogB1_(const char * fileName,
                       const char * funcName,
                       const int    lineNumber,
                       const char * text1,
                       const bool   val1)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_B1_ "\n", OD_FUNC_PREFIX_(rootName), text1,
                odBoolToString_(val1));
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(WIN32)
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_FUNC_FORMAT_ OD_FORMAT_B1_ "\n", OD_FUNC_PREFIX_(rootName), text1,
                odBoolToString_(val1));
        fflush(OD_LOG_STREAM_);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_B1_, OD_FUNC_PREFIX_(rootName), text1, odBoolToString_(val1));
#   else // ! defined(__OBJC__)
        syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_B1_, OD_FUNC_PREFIX_(rootName), text1,
               odBoolToString_(val1));
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLogB1_

EXTERN_C void ODLogB2_(const char * fileName,
                       const char * funcName,
                       const int    lineNumber,
                       const char * text1,
                       const bool   val1,
                       const char * text2,
                       const bool   val2)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_B2_ "\n", OD_FUNC_PREFIX_(rootName), text1,
                odBoolToString_(val1), text2, odBoolToString_(val2));
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(WIN32)
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_FUNC_FORMAT_ OD_FORMAT_B2_ "\n", OD_FUNC_PREFIX_(rootName), text1,
                odBoolToString_(val1), text2, odBoolToString_(val2));
        fflush(OD_LOG_STREAM_);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_B2_, OD_FUNC_PREFIX_(rootName), text1, odBoolToString_(val1), text2,
              odBoolToString_(val2));
#   else // ! defined(__OBJC__)
        syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_B2_, OD_FUNC_PREFIX_(rootName), text1,
               odBoolToString_(val1), text2, odBoolToString_(val2));
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLogB2_

EXTERN_C void ODLogB3_(const char * fileName,
                       const char * funcName,
                       const int    lineNumber,
                       const char * text1,
                       const bool   val1,
                       const char * text2,
                       const bool   val2,
                       const char * text3,
                       const bool   val3)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_B2_ "\n", OD_FUNC_PREFIX_(rootName), text1,
                odBoolToString_(val1), text2, odBoolToString_(val2));
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_B1_ "\n", OD_FUNC_PREFIX_(rootName), text3,
                odBoolToString_(val3));
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(WIN32)
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_FUNC_FORMAT_ OD_FORMAT_B2_ "\n", OD_FUNC_PREFIX_(rootName), text1,
                odBoolToString_(val1), text2, odBoolToString_(val2));
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_FUNC_FORMAT_ OD_FORMAT_B1_ "\n", OD_FUNC_PREFIX_(rootName), text3,
                odBoolToString_(val3));
        fflush(OD_LOG_STREAM_);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_B2_, OD_FUNC_PREFIX_(rootName), text1, odBoolToString_(val1), text2,
              odBoolToString_(val2));
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_B1_, OD_FUNC_PREFIX_(rootName), text3, odBoolToString_(val3));
#   else // ! defined(__OBJC__)
        syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_B2_, OD_FUNC_PREFIX_(rootName), text1,
               odBoolToString_(val1), text2, odBoolToString_(val2));
        syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_B1_, OD_FUNC_PREFIX_(rootName), text3,
               odBoolToString_(val3));
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLogB3_

EXTERN_C void ODLogB4_(const char * fileName,
                       const char * funcName,
                       const int    lineNumber,
                       const char * text1,
                       const bool   val1,
                       const char * text2,
                       const bool   val2,
                       const char * text3,
                       const bool   val3,
                       const char * text4,
                       const bool   val4)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_B2_ "\n", OD_FUNC_PREFIX_(rootName), text1,
                odBoolToString_(val1), text2, odBoolToString_(val2));
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_B2_ "\n", OD_FUNC_PREFIX_(rootName), text3,
                odBoolToString_(val3), text4, odBoolToString_(val4));
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(WIN32)
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_FUNC_FORMAT_ OD_FORMAT_B2_ "\n", OD_FUNC_PREFIX_(rootName), text1,
                odBoolToString_(val1), text2, odBoolToString_(val2));
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_FUNC_FORMAT_ OD_FORMAT_B2_ "\n", OD_FUNC_PREFIX_(rootName), text3,
                odBoolToString_(val3), text4, odBoolToString_(val4));
        fflush(OD_LOG_STREAM_);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_B2_, OD_FUNC_PREFIX_(rootName), text1, odBoolToString_(val1), text2,
              odBoolToString_(val2));
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_B2_, OD_FUNC_PREFIX_(rootName), text3, odBoolToString_(val3), text4,
              odBoolToString_(val4));
#   else // ! defined(__OBJC__)
        syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_B2_, OD_FUNC_PREFIX_(rootName), text1,
               odBoolToString_(val1), text2, odBoolToString_(val2));
        syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_B2_, OD_FUNC_PREFIX_(rootName), text3,
               odBoolToString_(val3), text4, odBoolToString_(val4));
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLogB4_

EXTERN_C void ODLogC1_(const char * fileName,
                       const char * funcName,
                       const int    lineNumber,
                       const char * text1,
                       const char   val1)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_C1_ "\n", OD_FUNC_PREFIX_(rootName), text1, val1, val1);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(WIN32)
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_FUNC_FORMAT_ OD_FORMAT_C1_ "\n", OD_FUNC_PREFIX_(rootName), text1, val1, val1);
        fflush(OD_LOG_STREAM_);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_C1_, OD_FUNC_PREFIX_(rootName), text1, val1, val1);
#   else // ! defined(__OBJC__)
        syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_C1_, OD_FUNC_PREFIX_(rootName), text1, val1, val1);
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLogC1_

EXTERN_C void ODLogC2_(const char * fileName,
                       const char * funcName,
                       const int    lineNumber,
                       const char * text1,
                       const char   val1,
                       const char * text2,
                       const char   val2)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_C2_ "\n", OD_FUNC_PREFIX_(rootName), text1, val1, val1, text2,
                val2, val2);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(WIN32)
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_FUNC_FORMAT_ OD_FORMAT_C2_ "\n", OD_FUNC_PREFIX_(rootName), text1, val1, val1, text2,
                val2, val2);
        fflush(OD_LOG_STREAM_);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_C2_, OD_FUNC_PREFIX_(rootName), text1, val1, val1, text2, val2, val2);
#   else // ! defined(__OBJC__)
        syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_C2_, OD_FUNC_PREFIX_(rootName), text1, val1, val1, text2,
               val2, val2);
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLogC2_

EXTERN_C void ODLogC3_(const char * fileName,
                       const char * funcName,
                       const int    lineNumber,
                       const char * text1,
                       const char   val1,
                       const char * text2,
                       const char   val2,
                       const char * text3,
                       const char   val3)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_C2_ "\n", OD_FUNC_PREFIX_(rootName), text1, val1, val1, text2,
                val2, val2);
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_C1_ "\n", OD_FUNC_PREFIX_(rootName), text3, val3, val3);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(WIN32)
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_FUNC_FORMAT_ OD_FORMAT_C2_ "\n", OD_FUNC_PREFIX_(rootName), text1, val1, val1, text2,
                val2, val2);
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_FUNC_FORMAT_ OD_FORMAT_C1_ "\n", OD_FUNC_PREFIX_(rootName), text3, val3, val3);
        fflush(OD_LOG_STREAM_);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_C2_, OD_FUNC_PREFIX_(rootName), text1, val1, val1, text2, val2, val2);
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_C1_, OD_FUNC_PREFIX_(rootName), text3, val3, val3);
#   else // ! defined(__OBJC__)
        syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_C2_, OD_FUNC_PREFIX_(rootName), text1, val1, val1, text2,
               val2, val2);
        syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_C1_, OD_FUNC_PREFIX_(rootName), text3, val3, val3);
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLogC3_

EXTERN_C void ODLogC4_(const char * fileName,
                       const char * funcName,
                       const int    lineNumber,
                       const char * text1,
                       const char   val1,
                       const char * text2,
                       const char   val2,
                       const char * text3,
                       const char   val3,
                       const char * text4,
                       const char   val4)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_C2_ "\n", OD_FUNC_PREFIX_(rootName), text1, val1, val1, text2,
                val2, val2);
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_C2_ "\n", OD_FUNC_PREFIX_(rootName), text3, val3, val3, text4,
                val4, val4);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(WIN32)
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_FUNC_FORMAT_ OD_FORMAT_C2_ "\n", OD_FUNC_PREFIX_(rootName), text1, val1, val1, text2,
                val2, val2);
        odWriteTime_(lOdLogFile_);
        fprintf(OD_LOG_STREAM_, OD_FUNC_FORMAT_ OD_FORMAT_C2_ "\n", OD_FUNC_PREFIX_(rootName), text3, val3, val3, text4,
                val4, val4);
        fflush(OD_LOG_STREAM_);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_C2_, OD_FUNC_PREFIX_(rootName), text1, val1, val1, text2, val2, val2);
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_C2_, OD_FUNC_PREFIX_(rootName), text3, val3, val3, text4, val4, val4);
#   else // ! defined(__OBJC__)
        syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_C2_, OD_FUNC_PREFIX_(rootName), text1, val1, val1, text2,
               val2, val2);
        syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_C2_, OD_FUNC_PREFIX_(rootName), text3, val3, val3, text4,
               val4, val4);
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLogC4_

EXTERN_C void ODLogD1_(const char * fileName,
                       const char * funcName,
                       const int    lineNumber,
                       const char * text1,
                       const double val1)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_D1_ "\n", OD_FUNC_PREFIX_(rootName), text1, val1);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(WIN32)
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_FUNC_FORMAT_ OD_FORMAT_D1_ "\n", OD_FUNC_PREFIX_(rootName), text1, val1);
        fflush(OD_LOG_STREAM_);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_D1_, OD_FUNC_PREFIX_(rootName), text1, val1);
#   else // ! defined(__OBJC__)
        syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_D1_, OD_FUNC_PREFIX_(rootName), text1, val1);
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLogD1_

EXTERN_C void ODLogD2_(const char * fileName,
                       const char * funcName,
                       const int    lineNumber,
                       const char * text1,
                       const double val1,
                       const char * text2,
                       const double val2)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_D2_ "\n", OD_FUNC_PREFIX_(rootName), text1, val1, text2, val2);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(WIN32)
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_FUNC_FORMAT_ OD_FORMAT_D2_ "\n", OD_FUNC_PREFIX_(rootName), text1, val1, text2,
                val2);
        fflush(OD_LOG_STREAM_);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_D2_, OD_FUNC_PREFIX_(rootName), text1, val1, text2, val2);
#   else // ! defined(__OBJC__)
        syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_D2_, OD_FUNC_PREFIX_(rootName), text1, val1, text2,
               val2);
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLogD2_

EXTERN_C void ODLogD3_(const char * fileName,
                       const char * funcName,
                       const int    lineNumber,
                       const char * text1,
                       const double   val1,
                       const char * text2,
                       const double   val2,
                       const char * text3,
                       const double   val3)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_D2_ "\n", OD_FUNC_PREFIX_(rootName), text1, val1, text2, val2);
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_D1_ "\n", OD_FUNC_PREFIX_(rootName), text3, val3);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(WIN32)
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_FUNC_FORMAT_ OD_FORMAT_D2_ "\n", OD_FUNC_PREFIX_(rootName), text1, val1, text2,
                val2);
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_FUNC_FORMAT_ OD_FORMAT_D1_ "\n", OD_FUNC_PREFIX_(rootName), text3, val3);
        fflush(OD_LOG_STREAM_);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_D2_, OD_FUNC_PREFIX_(rootName), text1, val1, text2, val2);
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_D1_, OD_FUNC_PREFIX_(rootName), text3, val3);
#   else // ! defined(__OBJC__)
        syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_D2_, OD_FUNC_PREFIX_(rootName), text1, val1, text2,
               val2);
        syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_D1_, OD_FUNC_PREFIX_(rootName), text3, val3);
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLogD3_

EXTERN_C void ODLogD4_(const char * fileName,
                       const char * funcName,
                       const int    lineNumber,
                       const char * text1,
                       const double   val1,
                       const char * text2,
                       const double   val2,
                       const char * text3,
                       const double   val3,
                       const char * text4,
                       const double   val4)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_D2_ "\n", OD_FUNC_PREFIX_(rootName), text1, val1, text2, val2);
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_D2_ "\n", OD_FUNC_PREFIX_(rootName), text3, val3, text4, val4);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(WIN32)
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_FUNC_FORMAT_ OD_FORMAT_D2_ "\n", OD_FUNC_PREFIX_(rootName), text1, val1, text2,
                val2);
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_FUNC_FORMAT_ OD_FORMAT_D2_ "\n", OD_FUNC_PREFIX_(rootName), text3, val3, text4,
                val4);
        fflush(OD_LOG_STREAM_);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_D2_, OD_FUNC_PREFIX_(rootName), text1, val1, text2, val2);
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_D2_, OD_FUNC_PREFIX_(rootName), text3, val3, text4, val4);
#   else // ! defined(__OBJC__)
        syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_D2_, OD_FUNC_PREFIX_(rootName), text1, val1, text2,
               val2);
        syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_D2_, OD_FUNC_PREFIX_(rootName), text3, val3, text4,
               val4);
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLogD4_

EXTERN_C void ODLogEnter_(const char * fileName,
                          const char * funcName,
                          const int    lineNumber)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_ENTER_ "\n", OD_FUNC_PREFIX_(rootName));
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(WIN32)
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_FUNC_FORMAT_ OD_FORMAT_ENTER_ "\n", OD_FUNC_PREFIX_(rootName));
        fflush(OD_LOG_STREAM_);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_ENTER_, OD_FUNC_PREFIX_(rootName));
#   else // ! defined(__OBJC__)
        syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_ENTER_, OD_FUNC_PREFIX_(rootName));
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
    odIncreaseIndent_();
} // ODLogEnter_

EXTERN_C void ODLogExit_(const char * fileName,
                         const char * funcName,
                         const int    lineNumber)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    odDecreaseIndent_();
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_EXIT_ "\n", OD_FUNC_PREFIX_(rootName));
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(WIN32)
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_FUNC_FORMAT_ OD_FORMAT_EXIT_ "\n", OD_FUNC_PREFIX_(rootName));
        fflush(OD_LOG_STREAM_);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_EXIT_, OD_FUNC_PREFIX_(rootName));
#   else // ! defined(__OBJC__)
        syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_EXIT_, OD_FUNC_PREFIX_(rootName));
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLogExit_

EXTERN_C void ODLogExitB_(const char * fileName,
                          const char * funcName,
                          const int    lineNumber,
                          const bool   val)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    odDecreaseIndent_();
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_B1_ "\n", OD_FUNC_PREFIX_(rootName), OD_EXIT_VALUE_,
                odBoolToString_(val));
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(WIN32)
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_FUNC_FORMAT_ OD_FORMAT_B1_ "\n", OD_FUNC_PREFIX_(rootName), OD_EXIT_VALUE_,
                odBoolToString_(val));
        fflush(OD_LOG_STREAM_);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_B1_, OD_FUNC_PREFIX_(rootName), OD_EXIT_VALUE_, odBoolToString_(val));
#   else // ! defined(__OBJC__)
        syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_B1_, OD_FUNC_PREFIX_(rootName), OD_EXIT_VALUE_,
               odBoolToString_(val));
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLogExitB_

EXTERN_C void ODLogExitC_(const char * fileName,
                          const char * funcName,
                          const int    lineNumber,
                          const char   val)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    odDecreaseIndent_();
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_C1_ "\n", OD_FUNC_PREFIX_(rootName), OD_EXIT_VALUE_, val, val);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(WIN32)
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_FUNC_FORMAT_ OD_FORMAT_C1_ "\n", OD_FUNC_PREFIX_(rootName), OD_EXIT_VALUE_, val,
                val);
        fflush(OD_LOG_STREAM_);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_C1_, OD_FUNC_PREFIX_(rootName), OD_EXIT_VALUE_, val, val);
#   else // ! defined(__OBJC__)
        syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_C1_, OD_FUNC_PREFIX_(rootName), OD_EXIT_VALUE_, val, val);
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLogExitC_

EXTERN_C void ODLogExitD_(const char * fileName,
                          const char * funcName,
                          const int    lineNumber,
                          const double val)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    odDecreaseIndent_();
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_D1_ "\n", OD_FUNC_PREFIX_(rootName), OD_EXIT_VALUE_, val);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(WIN32)
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_FUNC_FORMAT_ OD_FORMAT_D1_ "\n", OD_FUNC_PREFIX_(rootName), OD_EXIT_VALUE_, val);
        fflush(OD_LOG_STREAM_);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_D1_, OD_FUNC_PREFIX_(rootName), OD_EXIT_VALUE_, val);
#   else // ! defined(__OBJC__)
        syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_D1_, OD_FUNC_PREFIX_(rootName), OD_EXIT_VALUE_, val);
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLogExitD_

EXTERN_C void ODLogExitExit_(const char * fileName,
                             const char * funcName,
                             const int    lineNumber,
                             const long   val)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    odDecreaseIndent_();
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_EXIT_CALL_ "\n", OD_FUNC_PREFIX_(rootName), val, val);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(WIN32)
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_FUNC_FORMAT_ OD_FORMAT_EXIT_CALL_ "\n", OD_FUNC_PREFIX_(rootName), val, val);
        fflush(OD_LOG_STREAM_);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_EXIT_CALL_, OD_FUNC_PREFIX_(rootName), val, val);
#   else // ! defined(__OBJC__)
        syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_EXIT_CALL_, OD_FUNC_PREFIX_(rootName), val, val);
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLogExitExit_

EXTERN_C void ODLogExitL_(const char *  fileName,
                          const char *  funcName,
                          const int     lineNumber,
                          const int32_t val)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    odDecreaseIndent_();
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_L1_ "\n", OD_FUNC_PREFIX_(rootName), OD_EXIT_VALUE_, val, val);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(WIN32)
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_FUNC_FORMAT_ OD_FORMAT_L1_ "\n", OD_FUNC_PREFIX_(rootName), OD_EXIT_VALUE_, val,
                val);
        fflush(OD_LOG_STREAM_);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_L1_, OD_FUNC_PREFIX_(rootName), OD_EXIT_VALUE_, val, val);
#   else // ! defined(__OBJC__)
        syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_L1_, OD_FUNC_PREFIX_(rootName), OD_EXIT_VALUE_, val,
               val);
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLogExitL_

EXTERN_C void ODLogExitLL_(const char *  fileName,
                           const char *  funcName,
                           const int     lineNumber,
                           const int64_t val)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    odDecreaseIndent_();
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_LL1_ "\n", OD_FUNC_PREFIX_(rootName), OD_EXIT_VALUE_, val, val);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(WIN32)
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_FUNC_FORMAT_ OD_FORMAT_LL1_ "\n", OD_FUNC_PREFIX_(rootName), OD_EXIT_VALUE_, val,
                val);
        fflush(OD_LOG_STREAM_);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_LL1_, OD_FUNC_PREFIX_(rootName), OD_EXIT_VALUE_, val, val);
#   else // ! defined(__OBJC__)
        syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_LL1_, OD_FUNC_PREFIX_(rootName), OD_EXIT_VALUE_, val,
               val);
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLogExitLL_

#  if defined(__OBJC__)
EXTERN_C void ODLogExitO_(const char * fileName,
                          const char * funcName,
                          const int    lineNumber,
                          const id     val)
{
    const char * rootName = odFileNameRoot_(fileName);
    const char * valString = odNullOrDescription(val);
    
    odDecreaseIndent_();
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_O1_ "\n", OD_FUNC_PREFIX_(rootName), OD_EXIT_VALUE_, valString);
        fflush(lOdLogFile_);
    }
    else
    {
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_O1_, OD_FUNC_PREFIX_(rootName), OD_EXIT_VALUE_, valString);
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLogExitO_
#  endif // defined(__OBJC__)

EXTERN_C void ODLogExitP_(const char * fileName,
                          const char * funcName,
                          const int    lineNumber,
                          const void * val)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    odDecreaseIndent_();
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_P1_ "\n", OD_FUNC_PREFIX_(rootName), OD_EXIT_VALUE_, val);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(WIN32)
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_FUNC_FORMAT_ OD_FORMAT_P1_ "\n", OD_FUNC_PREFIX_(rootName), OD_EXIT_VALUE_, val);
        fflush(OD_LOG_STREAM_);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_P1_, OD_FUNC_PREFIX_(rootName), OD_EXIT_VALUE_, val);
#   else // ! defined(__OBJC__)
        syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_P1_, OD_FUNC_PREFIX_(rootName), OD_EXIT_VALUE_, val);
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLogExitP_

#  if defined(__APPLE__)
EXTERN_C void ODLogExitR_(const char * fileName,
                          const char * funcName,
                          const int    lineNumber,
                          const CGRect val)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    odDecreaseIndent_();
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_RECT_ "\n", OD_FUNC_PREFIX_(rootName), OD_EXIT_VALUE_,
                val.origin.x, val.origin.y, val.size.height, val.size.width);
        fflush(lOdLogFile_);
    }
    else
    {
#   if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_RECT_, OD_FUNC_PREFIX_(rootName), OD_EXIT_VALUE_, val.origin.x, val.origin.y,
              val.size.height, val.size.width);
#   else // ! defined(__OBJC__)
        syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_RECT_, OD_FUNC_PREFIX_(rootName), OD_EXIT_VALUE_,
               val.origin.x, val.origin.y, val.size.height, val.size.width);
#   endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLogExitR_
#  endif // defined(__APPLE__)

EXTERN_C void ODLogExitS_(const char * fileName,
                          const char * funcName,
                          const int    lineNumber,
                          const char * val)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    odDecreaseIndent_();
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_S1_ "\n", OD_FUNC_PREFIX_(rootName), OD_EXIT_VALUE_,
                odNullOrString(val));
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(WIN32)
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_FUNC_FORMAT_ OD_FORMAT_S1_ "\n", OD_FUNC_PREFIX_(rootName), OD_EXIT_VALUE_,
                odNullOrString(val));
        fflush(OD_LOG_STREAM_);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_S1_, OD_FUNC_PREFIX_(rootName), OD_EXIT_VALUE_, odNullOrString(val));
#   else // ! defined(__OBJC__)
        syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_S1_, OD_FUNC_PREFIX_(rootName), OD_EXIT_VALUE_,
               odNullOrString(val));
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLogExitS_

EXTERN_C void ODLogExitThrowL_(const char *  fileName,
                               const char *  funcName,
                               const int     lineNumber,
                               const int32_t val)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    odDecreaseIndent_();
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_EXIT_T_L_ "\n", OD_FUNC_PREFIX_(rootName), val, val);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(WIN32)
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_FUNC_FORMAT_ OD_FORMAT_EXIT_T_L_ "\n", OD_FUNC_PREFIX_(rootName), val, val);
        fflush(OD_LOG_STREAM_);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_EXIT_T_L_, OD_FUNC_PREFIX_(rootName), val, val);
#   else // ! defined(__OBJC__)
        syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_EXIT_T_L_, OD_FUNC_PREFIX_(rootName), val, val);
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLogExitThrowL_

EXTERN_C void ODLogExitThrowS_(const char * fileName,
                               const char * funcName,
                               const int    lineNumber,
                               const char * val)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    odDecreaseIndent_();
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_EXIT_T_S_ "\n", OD_FUNC_PREFIX_(rootName), odNullOrString(val));
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(WIN32)
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_FUNC_FORMAT_ OD_FORMAT_EXIT_T_S_ "\n", OD_FUNC_PREFIX_(rootName),
                odNullOrString(val));
        fflush(OD_LOG_STREAM_);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_EXIT_T_S_, OD_FUNC_PREFIX_(rootName), odNullOrString(val));
#   else // ! defined(__OBJC__)
        syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_EXIT_T_S_, OD_FUNC_PREFIX_(rootName),
               odNullOrString(val));
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLogExitThrowS_

EXTERN_C void ODLogInit_(const char * prefix,
                         const int    options,
                         const char * fileName,
                         const char * funcName,
                         const int    lineNumber)
{
#  if (defined(__OBJC__) || defined(WIN32))
#   pragma unused(prefix)
#  endif // defined(__OBJC__) || defined(WIN32)
    bool         odWriteToFile = (options & kODLoggingOptionWriteToFile);
    const char * rootName = odFileNameRoot_(fileName);
    const char * suffixString;
    char *       stars;
    size_t       starsLength = strlen(rootName) + strlen(funcName) + (sizeof(OD_INIT_FORMAT_) - 1) - 8; // 4 %s
    
#  if (! defined(WIN32))
    lOdEnableThreadSupport_ = (options & kODLoggingOptionEnableThreadSupport);
    lOdIncludeProcessID_ = (options & kODLoggingOptionIncludeProcessID);
    lOdIncludeThreadID_ = (options & kODLoggingOptionIncludeThreadID);
#  endif // ! defined(WIN32)
    OD_CREATE_PREFIX_();
    size_t prefixLength = strlen(prefixString_);
    
    if (prefixLength)
    {
        suffixString = " ";
    }
    else
    {
        suffixString = "";
    }
    starsLength += prefixLength + strlen(suffixString);
    stars = (char *) malloc(starsLength + 1);
    memset(stars, '*', starsLength);
    stars[starsLength] = '\0';
#  if ((! defined(WIN32)) && (! defined(__OBJC__)))
    if (options & kODLoggingOptionWriteToStderr)
    {
        openlog(prefix, LOG_PID | LOG_CONS | LOG_PERROR, OD_LOG_LOG_TARGET_);
    }
    else
    {
        openlog(prefix, LOG_PID | LOG_CONS, OD_LOG_LOG_TARGET_);
    }
#  endif // (! defined(WIN32)) && (! defined(__OBJC__))
    if (odWriteToFile)
    {
        char pidString[100];
        
#  if defined(WIN32)
        snprintf(pidString, sizeof(pidString), "od_temp.log");
#  else // ! defined(WIN32)
        snprintf(pidString, sizeof(pidString), "/var/log/pid-%lX.log", (long unsigned) getpid());
#  endif // ! defined(WIN32)
        lOdLogFile_ = fopen(pidString, "w");
        if (lOdLogFile_)
        {
            odWriteTime_(lOdLogFile_);
            fprintf(lOdLogFile_, "%s\n", stars);
        }
        else
        {
#  if defined(WIN32)
            fprintf(OD_LOG_STREAM_, "problem opening %s => %d\n", pidString, errno);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
            NSLog(@"problem opening %s => %d", pidString, errno);
#   else // ! defined(__OBJC__)
            syslog(OD_LOG_LOG_LEVEL_, "problem opening %s => %d", pidString, errno);
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
        }
    }
    else
    {
#  if defined(WIN32)
        fprintf(OD_LOG_STREAM_, "%s\n", stars);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
        NSLog(@"%s", stars);
#   else // ! defined(__OBJC__)
        syslog(OD_LOG_LOG_LEVEL_, "%s", stars);
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
    }
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_INIT_FORMAT_ "\n", prefixString_, suffixString, funcName, rootName, lineNumber);
    }
    else
    {
#  if defined(WIN32)
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_INIT_FORMAT_ "\n", prefixString_, suffixString, funcName, rootName, lineNumber);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
        NSLog(@OD_INIT_FORMAT_, prefixString_, suffixString, funcName, rootName, lineNumber);
#   else // ! defined(__OBJC__)
        syslog(OD_LOG_LOG_LEVEL_, OD_INIT_FORMAT_, prefixString_, suffixString, funcName, rootName, lineNumber);
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
    }
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, "%s\n", stars);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(WIN32)
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, "%s\n", stars);
        fflush(OD_LOG_STREAM_);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
        NSLog(@"%s", stars);
#   else // ! defined(__OBJC__)
        syslog(OD_LOG_LOG_LEVEL_, "%s", stars);
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
    }
    free(stars);
    OD_FREE_PREFIX_();
    odSetIndent_(OD_LOG_INIT_VALUE_);
} // ODLogInit_

EXTERN_C void ODLogIP_(const char *  fileName,
                       const char *  funcName,
                       const int     lineNumber,
                       const char *  text1,
                       const int32_t val1,
                       const int     val2)
{
    const char * rootName = odFileNameRoot_(fileName);
    union IP_TYPE_
    {
        uint8_t asBytes[4];
        int32_t asInt32;
    } address;
    
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    address.asInt32 = val1;
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_IP_ "\n", OD_FUNC_PREFIX_(rootName), text1,
                address.asBytes[0], address.asBytes[1], address.asBytes[2], address.asBytes[3], val2);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(WIN32)
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_FUNC_FORMAT_ OD_FORMAT_IP_ "\n", OD_FUNC_PREFIX_(rootName), text1,
                address.asBytes[0], address.asBytes[1], address.asBytes[2], address.asBytes[3], val2);
        fflush(OD_LOG_STREAM_);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_IP_, OD_FUNC_PREFIX_(rootName), text1, address.asBytes[0],
              address.asBytes[1], address.asBytes[2], address.asBytes[3], val2);
#   else // ! defined(__OBJC__)
        syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_IP_, OD_FUNC_PREFIX_(rootName), text1,
               address.asBytes[0], address.asBytes[1], address.asBytes[2], address.asBytes[3], val2);
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLogIP_

EXTERN_C void ODLogL1_(const char *  fileName,
                       const char *  funcName,
                       const int     lineNumber,
                       const char *  text1,
                       const int32_t val1)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_L1_ "\n", OD_FUNC_PREFIX_(rootName), text1, val1, val1);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(WIN32)
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_FUNC_FORMAT_ OD_FORMAT_L1_ "\n", OD_FUNC_PREFIX_(rootName), text1, val1, val1);
        fflush(OD_LOG_STREAM_);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_L1_, OD_FUNC_PREFIX_(rootName), text1, val1, val1);
#   else // ! defined(__OBJC__)
        syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_L1_, OD_FUNC_PREFIX_(rootName), text1, val1, val1);
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLogL1_

EXTERN_C void ODLogL2_(const char *  fileName,
                       const char *  funcName,
                       const int     lineNumber,
                       const char *  text1,
                       const int32_t val1,
                       const char *  text2,
                       const int32_t val2)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_L2_ "\n", OD_FUNC_PREFIX_(rootName), text1, val1, val1, text2,
                val2, val2);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(WIN32)
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_FUNC_FORMAT_ OD_FORMAT_L2_ "\n", OD_FUNC_PREFIX_(rootName), text1, val1, val1, text2,
                val2, val2);
        fflush(OD_LOG_STREAM_);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_L2_, OD_FUNC_PREFIX_(rootName), text1, val1, val1, text2, val2, val2);
#   else // ! defined(__OBJC__)
        syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_L2_, OD_FUNC_PREFIX_(rootName), text1, val1, val1, text2,
               val2, val2);
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLogL2_

EXTERN_C void ODLogL3_(const char *  fileName,
                       const char *  funcName,
                       const int     lineNumber,
                       const char *  text1,
                       const int32_t val1,
                       const char *  text2,
                       const int32_t val2,
                       const char *  text3,
                       const int32_t val3)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_L2_ "\n", OD_FUNC_PREFIX_(rootName), text1, val1, val1, text2,
                val2, val2);
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_L1_ "\n", OD_FUNC_PREFIX_(rootName), text3, val3, val3);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(WIN32)
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_FUNC_FORMAT_ OD_FORMAT_L2_ "\n", OD_FUNC_PREFIX_(rootName), text1, val1, val1, text2,
                val2, val2);
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_FUNC_FORMAT_ OD_FORMAT_L1_ "\n", OD_FUNC_PREFIX_(rootName), text3, val3, val3);
        fflush(OD_LOG_STREAM_);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_L2_, OD_FUNC_PREFIX_(rootName), text1, val1, val1, text2, val2, val2);
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_L1_, OD_FUNC_PREFIX_(rootName), text3, val3, val3);
#   else // ! defined(__OBJC__)
        syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_L2_, OD_FUNC_PREFIX_(rootName), text1, val1, val1, text2,
               val2, val2);
        syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_L1_, OD_FUNC_PREFIX_(rootName), text3, val3, val3);
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLogL3_

EXTERN_C void ODLogL4_(const char *  fileName,
                       const char *  funcName,
                       const int     lineNumber,
                       const char *  text1,
                       const int32_t val1,
                       const char *  text2,
                       const int32_t val2,
                       const char *  text3,
                       const int32_t val3,
                       const char *  text4,
                       const int32_t val4)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_L2_ "\n", OD_FUNC_PREFIX_(rootName), text1, val1, val1, text2,
                val2, val2);
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_L2_ "\n", OD_FUNC_PREFIX_(rootName), text3, val3, val3, text4,
                val4, val4);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(WIN32)
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_FUNC_FORMAT_ OD_FORMAT_L2_ "\n", OD_FUNC_PREFIX_(rootName), text1, val1, val1, text2,
                val2, val2);
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_FUNC_FORMAT_ OD_FORMAT_L2_ "\n", OD_FUNC_PREFIX_(rootName), text3, val3, val3, text4,
                val4, val4);
        fflush(OD_LOG_STREAM_);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_L2_, OD_FUNC_PREFIX_(rootName), text1, val1, val1, text2, val2, val2);
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_L2_, OD_FUNC_PREFIX_(rootName), text3, val3, val3, text4, val4, val4);
#   else // ! defined(__OBJC__)
        syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_L2_, OD_FUNC_PREFIX_(rootName), text1, val1, val1, text2,
               val2, val2);
        syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_L2_, OD_FUNC_PREFIX_(rootName), text3, val3, val3, text4,
               val4, val4);
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLogL4_

EXTERN_C void ODLogLL1_(const char *  fileName,
                        const char *  funcName,
                        const int     lineNumber,
                        const char *  text1,
                        const int64_t val1)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_LL1_ "\n", OD_FUNC_PREFIX_(rootName), text1, val1, val1);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(WIN32)
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_FUNC_FORMAT_ OD_FORMAT_LL1_ "\n", OD_FUNC_PREFIX_(rootName), text1, val1, val1);
        fflush(OD_LOG_STREAM_);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_LL1_, OD_FUNC_PREFIX_(rootName), text1, val1, val1);
#   else // ! defined(__OBJC__)
        syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_LL1_, OD_FUNC_PREFIX_(rootName), text1, val1, val1);
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLogLL1_

EXTERN_C void ODLogLL2_(const char *  fileName,
                        const char *  funcName,
                        const int     lineNumber,
                        const char *  text1,
                        const int64_t val1,
                        const char *  text2,
                        const int64_t val2)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_LL2_ "\n", OD_FUNC_PREFIX_(rootName), text1, val1, val1, text2,
                val2, val2);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(WIN32)
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_FUNC_FORMAT_ OD_FORMAT_LL2_ "\n", OD_FUNC_PREFIX_(rootName), text1, val1, val1,
                text2, val2, val2);
        fflush(OD_LOG_STREAM_);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_LL2_, OD_FUNC_PREFIX_(rootName), text1, val1, val1, text2, val2, val2);
#   else // ! defined(__OBJC__)
        syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_LL2_, OD_FUNC_PREFIX_(rootName), text1, val1, val1,
               text2, val2, val2);
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLogLL2_

EXTERN_C void ODLogLL3_(const char *  fileName,
                        const char *  funcName,
                        const int     lineNumber,
                        const char *  text1,
                        const int64_t val1,
                        const char *  text2,
                        const int64_t val2,
                        const char *  text3,
                        const int64_t val3)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_LL2_ "\n", OD_FUNC_PREFIX_(rootName), text1, val1, val1, text2,
                val2, val2);
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_LL1_ "\n", OD_FUNC_PREFIX_(rootName), text3, val3, val3);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(WIN32)
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_FUNC_FORMAT_ OD_FORMAT_LL2_ "\n", OD_FUNC_PREFIX_(rootName), text1, val1, val1,
                text2, val2, val2);
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_FUNC_FORMAT_ OD_FORMAT_LL1_ "\n", OD_FUNC_PREFIX_(rootName), text3, val3, val3);
        fflush(OD_LOG_STREAM_);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_LL2_, OD_FUNC_PREFIX_(rootName), text1, val1, val1, text2, val2, val2);
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_LL1_, OD_FUNC_PREFIX_(rootName), text3, val3, val3);
#   else // ! defined(__OBJC__)
        syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_LL2_, OD_FUNC_PREFIX_(rootName), text1, val1, val1,
               text2, val2, val2);
        syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_LL1_, OD_FUNC_PREFIX_(rootName), text3, val3, val3);
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLogLL3_

EXTERN_C void ODLogLL4_(const char *  fileName,
                        const char *  funcName,
                        const int     lineNumber,
                        const char *  text1,
                        const int64_t val1,
                        const char *  text2,
                        const int64_t val2,
                        const char *  text3,
                        const int64_t val3,
                        const char *  text4,
                        const int64_t val4)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_LL2_ "\n", OD_FUNC_PREFIX_(rootName), text1, val1, val1, text2,
                val2, val2);
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_LL2_ "\n", OD_FUNC_PREFIX_(rootName), text3, val3, val3, text4,
                val4, val4);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(WIN32)
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_FUNC_FORMAT_ OD_FORMAT_LL2_ "\n", OD_FUNC_PREFIX_(rootName), text1, val1, val1,
                text2, val2, val2);
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_FUNC_FORMAT_ OD_FORMAT_LL2_ "\n", OD_FUNC_PREFIX_(rootName), text3, val3, val3,
                text4, val4, val4);
        fflush(OD_LOG_STREAM_);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_LL2_, OD_FUNC_PREFIX_(rootName), text1, val1, val1, text2, val2, val2);
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_LL2_, OD_FUNC_PREFIX_(rootName), text3, val3, val3, text4, val4, val4);
#   else // ! defined(__OBJC__)
        syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_LL2_, OD_FUNC_PREFIX_(rootName), text1, val1, val1,
               text2, val2, val2);
        syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_LL2_, OD_FUNC_PREFIX_(rootName), text3, val3, val3,
               text4, val4, val4);
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLogLL4_

EXTERN_C void ODLogLS_(const char * fileName,
                       const char * funcName,
                       const int    lineNumber,
                       const char * text1,
                       const char * val1)
{
    const char * heading = text1;
    const char * rootName = odFileNameRoot_(fileName);
    size_t       captionLength = strlen(text1);
    int          size = (int) strlen(val1);
    int          jj = 0;
    char         lineBuffer[OD_MAX_CHARS_IN_LINE_ + 1];
    char *       blankCaption = (char *) malloc(captionLength + 1);
    
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    memset(blankCaption, ' ', captionLength);
    blankCaption[captionLength] = '\0';
    // Prefill the line buffer
    memset(lineBuffer, ' ', sizeof(lineBuffer) - 1);
    lineBuffer[sizeof(lineBuffer) - 1] = '\0';
    for (int ii = 0; ii < size; )
    {
        char bb = val1[ii++];
        
        if (('\n' == bb) || ('\r' == bb) || ('\t' == bb))
        {
            lineBuffer[jj++] = '\\';
            if (OD_MAX_CHARS_IN_LINE_ <= jj)
            {
                if (lOdLogFile_)
                {
                    odWriteTime_(lOdLogFile_);
                    fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_LS "\n", OD_FUNC_PREFIX_(rootName), heading,
                            lineBuffer);
                }
                else
                {
#  if defined(WIN32)
                    odWriteTime_(OD_LOG_STREAM_);
                    fprintf(OD_LOG_STREAM_, OD_FUNC_FORMAT_ OD_FORMAT_LS "\n", OD_FUNC_PREFIX_(rootName), heading,
                            lineBuffer);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
                    NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_LS, OD_FUNC_PREFIX_(rootName), heading, lineBuffer);
#   else // ! defined(__OBJC__)
                    syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_LS, OD_FUNC_PREFIX_(rootName), heading,
                           lineBuffer);
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
                }
                heading = blankCaption;
                memset(lineBuffer, ' ', sizeof(lineBuffer) - 1);
                jj = 0;
            }
            bb = (('\n' == bb) ? 'n' : (('\r' == bb) ? 'r' : 't'));
        }
        lineBuffer[jj++] = (isprint(bb) ? bb : '.');
        if (OD_MAX_CHARS_IN_LINE_ <= jj)
        {
            if (lOdLogFile_)
            {
                odWriteTime_(lOdLogFile_);
                fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_LS "\n", OD_FUNC_PREFIX_(rootName), heading,
                        lineBuffer);
            }
            else
            {
#  if defined(WIN32)
                odWriteTime_(OD_LOG_STREAM_);
                fprintf(OD_LOG_STREAM_, OD_FUNC_FORMAT_ OD_FORMAT_LS "\n", OD_FUNC_PREFIX_(rootName), heading,
                        lineBuffer);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
                NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_LS, OD_FUNC_PREFIX_(rootName), heading, lineBuffer);
#   else // ! defined(__OBJC__)
                syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_LS, OD_FUNC_PREFIX_(rootName), heading,
                       lineBuffer);
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
            }
            heading = blankCaption;
            memset(lineBuffer, ' ', sizeof(lineBuffer) - 1);
            jj = 0;
        }
    }
    if (jj)
    {
        if (lOdLogFile_)
        {
            odWriteTime_(lOdLogFile_);
            fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_LS "\n", OD_FUNC_PREFIX_(rootName), heading, lineBuffer);
        }
        else
        {
#  if defined(WIN32)
            odWriteTime_(OD_LOG_STREAM_);
            fprintf(OD_LOG_STREAM_, OD_FUNC_FORMAT_ OD_FORMAT_LS "\n", OD_FUNC_PREFIX_(rootName), heading, lineBuffer);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
            NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_LS, OD_FUNC_PREFIX_(rootName), heading, lineBuffer);
#   else // ! defined(__OBJC__)
            syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_LS, OD_FUNC_PREFIX_(rootName), heading,
                   lineBuffer);
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
        }
    }
    if (lOdLogFile_)
    {
        fflush(lOdLogFile_);
    }
#  if defined(WIN32)
    else
    {
        fflush(OD_LOG_STREAM_);
    }
#  endif // defined(WIN32)
    free(blankCaption);
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLogLS_

#  if defined(__OBJC__)
EXTERN_C void ODLogO1_(const char * fileName,
                       const char * funcName,
                       const int    lineNumber,
                       const char * text1,
                       const id     obj1)
{
    const char * obj1String = odNullOrDescription(obj1);
    const char * rootName = odFileNameRoot_(fileName);
    
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_O1_ "\n", OD_FUNC_PREFIX_(rootName), text1, obj1String);
        fflush(lOdLogFile_);
    }
    else
    {
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_O1_, OD_FUNC_PREFIX_(rootName), text1, obj1String);
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLogO1_
#  endif // defined(__OBJC__)

#  if defined(__OBJC__)
EXTERN_C void ODLogO2_(const char * fileName,
                       const char * funcName,
                       const int    lineNumber,
                       const char * text1,
                       const id     obj1,
                       const char * text2,
                       const id     obj2)
{
    const char * obj1String = odNullOrDescription(obj1);
    const char * obj2String = odNullOrDescription(obj2);
    const char * rootName = odFileNameRoot_(fileName);
    
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_O2_ "\n", OD_FUNC_PREFIX_(rootName), text1, obj1String, text2,
                obj2String);
        fflush(lOdLogFile_);
    }
    else
    {
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_O2_, OD_FUNC_PREFIX_(rootName), text1, obj1String, text2, obj2String);
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLogO2_
#  endif // defined(__OBJC__)

#  if defined(__OBJC__)
EXTERN_C void ODLogO3_(const char * fileName,
                       const char * funcName,
                       const int    lineNumber,
                       const char * text1,
                       const id     obj1,
                       const char * text2,
                       const id     obj2,
                       const char * text3,
                       const id     obj3)
{
    const char * obj1String = odNullOrDescription(obj1);
    const char * obj2String = odNullOrDescription(obj2);
    const char * obj3String = odNullOrDescription(obj3);
    const char * rootName = odFileNameRoot_(fileName);
    
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_O2_ "\n", OD_FUNC_PREFIX_(rootName), text1, obj1String, text2,
                obj2String);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_O1_ "\n", OD_FUNC_PREFIX_(rootName), text3, obj3String);
        fflush(lOdLogFile_);
    }
    else
    {
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_O2_, OD_FUNC_PREFIX_(rootName), text1, obj1String, text2, obj2String);
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_O1_, OD_FUNC_PREFIX_(rootName), text3, obj3String);
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLogO3_
#  endif // defined(__OBJC__)

#  if defined(__OBJC__)
EXTERN_C void ODLogO4_(const char * fileName,
                       const char * funcName,
                       const int    lineNumber,
                       const char * text1,
                       const id     obj1,
                       const char * text2,
                       const id     obj2,
                       const char * text3,
                       const id     obj3,
                       const char * text4,
                       const id     obj4)
{
    const char * obj1String = odNullOrDescription(obj1);
    const char * obj2String = odNullOrDescription(obj2);
    const char * obj3String = odNullOrDescription(obj3);
    const char * obj4String = odNullOrDescription(obj4);
    const char * rootName = odFileNameRoot_(fileName);
    
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_O2_ "\n", OD_FUNC_PREFIX_(rootName), text1, obj1String, text2,
                obj2String);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_O2_ "\n", OD_FUNC_PREFIX_(rootName), text3, obj3String, text4,
                obj4String);
        fflush(lOdLogFile_);
    }
    else
    {
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_O2_, OD_FUNC_PREFIX_(rootName), text1, obj1String, text2, obj2String);
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_O2_, OD_FUNC_PREFIX_(rootName), text3, obj3String, text4, obj4String);
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLogO4_
#  endif // defined(__OBJC__)

EXTERN_C void ODLogObjEnter_(const char * fileName,
                             const char * funcName,
                             const int    lineNumber,
                             const void * objPtr)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_METHOD_FORMAT_ OD_FORMAT_ENTER_ "\n", OD_METHOD_PREFIX_(rootName));
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(WIN32)
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_METHOD_FORMAT_ OD_FORMAT_ENTER_ "\n", OD_METHOD_PREFIX_(rootName));
        fflush(OD_LOG_STREAM_);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
        NSLog(@OD_METHOD_FORMAT_ OD_FORMAT_ENTER_, OD_METHOD_PREFIX_(rootName));
#   else // ! defined(__OBJC__)
        syslog(OD_LOG_LOG_LEVEL_, OD_METHOD_FORMAT_ OD_FORMAT_ENTER_, OD_METHOD_PREFIX_(rootName));
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
    odIncreaseIndent_();
} // ODLogObjEnter_

EXTERN_C void ODLogObjExit_(const char * fileName,
                            const char * funcName,
                            const int    lineNumber,
                            const void * objPtr)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    odDecreaseIndent_();
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_METHOD_FORMAT_ OD_FORMAT_EXIT_ "\n", OD_METHOD_PREFIX_(rootName));
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(WIN32)
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_METHOD_FORMAT_ OD_FORMAT_EXIT_ "\n", OD_METHOD_PREFIX_(rootName));
        fflush(OD_LOG_STREAM_);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
        NSLog(@OD_METHOD_FORMAT_ OD_FORMAT_EXIT_, OD_METHOD_PREFIX_(rootName));
#   else // ! defined(__OBJC__)
        syslog(OD_LOG_LOG_LEVEL_, OD_METHOD_FORMAT_ OD_FORMAT_EXIT_, OD_METHOD_PREFIX_(rootName));
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLogObjExit_

EXTERN_C void ODLogObjExitB_(const char * fileName,
                             const char * funcName,
                             const int    lineNumber,
                             const void * objPtr,
                             const bool   val)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    odDecreaseIndent_();
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_METHOD_FORMAT_ OD_FORMAT_B1_ "\n", OD_METHOD_PREFIX_(rootName), OD_EXIT_VALUE_,
                odBoolToString_(val));
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(WIN32)
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_METHOD_FORMAT_ OD_FORMAT_B1_ "\n", OD_METHOD_PREFIX_(rootName), OD_EXIT_VALUE_,
                odBoolToString_(val));
        fflush(OD_LOG_STREAM_);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
        NSLog(@OD_METHOD_FORMAT_ OD_FORMAT_B1_, OD_METHOD_PREFIX_(rootName), OD_EXIT_VALUE_, odBoolToString_(val));
#   else // ! defined(__OBJC__)
        syslog(OD_LOG_LOG_LEVEL_, OD_METHOD_FORMAT_ OD_FORMAT_B1_, OD_METHOD_PREFIX_(rootName), OD_EXIT_VALUE_,
               odBoolToString_(val));
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLogObjExitB_

EXTERN_C void ODLogObjExitC_(const char * fileName,
                             const char * funcName,
                             const int    lineNumber,
                             const void * objPtr,
                             const char   val)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    odDecreaseIndent_();
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_METHOD_FORMAT_ OD_FORMAT_C1_ "\n", OD_METHOD_PREFIX_(rootName), OD_EXIT_VALUE_, val,
                val);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(WIN32)
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_METHOD_FORMAT_ OD_FORMAT_C1_ "\n", OD_METHOD_PREFIX_(rootName), OD_EXIT_VALUE_,
                val, val);
        fflush(OD_LOG_STREAM_);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
        NSLog(@OD_METHOD_FORMAT_ OD_FORMAT_C1_, OD_METHOD_PREFIX_(rootName), OD_EXIT_VALUE_, val, val);
#   else // ! defined(__OBJC__)
        syslog(OD_LOG_LOG_LEVEL_, OD_METHOD_FORMAT_ OD_FORMAT_C1_, OD_METHOD_PREFIX_(rootName), OD_EXIT_VALUE_,
               val, val);
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLogObjExitC_

EXTERN_C void ODLogObjExitD_(const char * fileName,
                             const char * funcName,
                             const int    lineNumber,
                             const void * objPtr,
                             const double val)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    odDecreaseIndent_();
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_METHOD_FORMAT_ OD_FORMAT_D1_ "\n", OD_METHOD_PREFIX_(rootName), OD_EXIT_VALUE_, val);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(WIN32)
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_METHOD_FORMAT_ OD_FORMAT_D1_ "\n", OD_METHOD_PREFIX_(rootName), OD_EXIT_VALUE_,
                val);
        fflush(OD_LOG_STREAM_);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
        NSLog(@OD_METHOD_FORMAT_ OD_FORMAT_D1_, OD_METHOD_PREFIX_(rootName), OD_EXIT_VALUE_, val);
#   else // ! defined(__OBJC__)
        syslog(OD_LOG_LOG_LEVEL_, OD_METHOD_FORMAT_ OD_FORMAT_D1_, OD_METHOD_PREFIX_(rootName), OD_EXIT_VALUE_,
               val);
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLogObjExitD_

EXTERN_C void ODLogObjExitExit_(const char * fileName,
                                const char * funcName,
                                const int    lineNumber,
                                const void * objPtr,
                                const long   val)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    odDecreaseIndent_();
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_METHOD_FORMAT_ OD_FORMAT_EXIT_CALL_ "\n", OD_METHOD_PREFIX_(rootName), val, val);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(WIN32)
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_METHOD_FORMAT_ OD_FORMAT_EXIT_CALL_ "\n", OD_METHOD_PREFIX_(rootName), val, val);
        fflush(OD_LOG_STREAM_);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
        NSLog(@OD_METHOD_FORMAT_ OD_FORMAT_EXIT_CALL_, OD_METHOD_PREFIX_(rootName), val, val);
#   else // ! defined(__OBJC__)
        syslog(OD_LOG_LOG_LEVEL_, OD_METHOD_FORMAT_ OD_FORMAT_EXIT_CALL_, OD_METHOD_PREFIX_(rootName), val, val);
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLogObjExitExit_

EXTERN_C void ODLogObjExitL_(const char *  fileName,
                             const char *  funcName,
                             const int     lineNumber,
                             const void *  objPtr,
                             const int32_t val)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    odDecreaseIndent_();
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_METHOD_FORMAT_ OD_FORMAT_L1_ "\n", OD_METHOD_PREFIX_(rootName), OD_EXIT_VALUE_, val,
                val);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(WIN32)
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_METHOD_FORMAT_ OD_FORMAT_L1_ "\n", OD_METHOD_PREFIX_(rootName), OD_EXIT_VALUE_,
                val, val);
        fflush(OD_LOG_STREAM_);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
        NSLog(@OD_METHOD_FORMAT_ OD_FORMAT_L1_, OD_METHOD_PREFIX_(rootName), OD_EXIT_VALUE_, val, val);
#   else // ! defined(__OBJC__)
        syslog(OD_LOG_LOG_LEVEL_, OD_METHOD_FORMAT_ OD_FORMAT_L1_, OD_METHOD_PREFIX_(rootName), OD_EXIT_VALUE_,
               val, val);
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLogObjExitL_

EXTERN_C void ODLogObjExitLL_(const char *  fileName,
                              const char *  funcName,
                              const int     lineNumber,
                              const void *  objPtr,
                              const int64_t val)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    odDecreaseIndent_();
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_METHOD_FORMAT_ OD_FORMAT_LL1_ "\n", OD_METHOD_PREFIX_(rootName), OD_EXIT_VALUE_, val,
                val);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(WIN32)
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_METHOD_FORMAT_ OD_FORMAT_LL1_ "\n", OD_METHOD_PREFIX_(rootName), OD_EXIT_VALUE_,
                val, val);
        fflush(OD_LOG_STREAM_);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
        NSLog(@OD_METHOD_FORMAT_ OD_FORMAT_LL1_, OD_METHOD_PREFIX_(rootName), OD_EXIT_VALUE_, val, val);
#   else // ! defined(__OBJC__)
        syslog(OD_LOG_LOG_LEVEL_, OD_METHOD_FORMAT_ OD_FORMAT_LL1_, OD_METHOD_PREFIX_(rootName), OD_EXIT_VALUE_,
               val, val);
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLogObjExitLL_

#  if defined(__OBJC__)
EXTERN_C void ODLogObjExitO_(const char * fileName,
                             const char * funcName,
                             const int    lineNumber,
                             const void * objPtr,
                             const id     val)
{
    const char * rootName = odFileNameRoot_(fileName);
    const char * valString = odNullOrDescription(val);
    
    odDecreaseIndent_();
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_METHOD_FORMAT_ OD_FORMAT_O1_ "\n", OD_METHOD_PREFIX_(rootName), OD_EXIT_VALUE_,
                valString);
        fflush(lOdLogFile_);
    }
    else
    {
        NSLog(@OD_METHOD_FORMAT_ OD_FORMAT_O1_, OD_METHOD_PREFIX_(rootName), OD_EXIT_VALUE_, valString);
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLogObjExitO_
#  endif // defined(__OBJC__)

EXTERN_C void ODLogObjExitP_(const char * fileName,
                             const char * funcName,
                             const int    lineNumber,
                             const void * objPtr,
                             const void * val)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    odDecreaseIndent_();
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_METHOD_FORMAT_ OD_FORMAT_P1_ "\n", OD_METHOD_PREFIX_(rootName), OD_EXIT_VALUE_, val);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(WIN32)
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_METHOD_FORMAT_ OD_FORMAT_P1_ "\n", OD_METHOD_PREFIX_(rootName), OD_EXIT_VALUE_,
                val);
        fflush(OD_LOG_STREAM_);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
        NSLog(@OD_METHOD_FORMAT_ OD_FORMAT_P1_, OD_METHOD_PREFIX_(rootName), OD_EXIT_VALUE_, val);
#   else // ! defined(__OBJC__)
        syslog(OD_LOG_LOG_LEVEL_, OD_METHOD_FORMAT_ OD_FORMAT_P1_, OD_METHOD_PREFIX_(rootName), OD_EXIT_VALUE_,
               val);
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLogObjExitP_

#  if defined(__APPLE__)
EXTERN_C void ODLogObjExitR_(const char * fileName,
                             const char * funcName,
                             const int    lineNumber,
                             const void * objPtr,
                             const CGRect val)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    odDecreaseIndent_();
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_METHOD_FORMAT_ OD_FORMAT_RECT_ "\n", OD_METHOD_PREFIX_(rootName), OD_EXIT_VALUE_,
                val.origin.x, val.origin.y, val.size.height, val.size.width);
        fflush(lOdLogFile_);
    }
    else
    {
#   if defined(__OBJC__)
        NSLog(@OD_METHOD_FORMAT_ OD_FORMAT_RECT_, OD_METHOD_PREFIX_(rootName), OD_EXIT_VALUE_, val.origin.x,
              val.origin.y, val.size.height, val.size.width);
#   else // ! defined(__OBJC__)
        syslog(OD_LOG_LOG_LEVEL_, OD_METHOD_FORMAT_ OD_FORMAT_RECT_, OD_METHOD_PREFIX_(rootName), OD_EXIT_VALUE_,
               val.origin.x, val.origin.y, val.size.height, val.size.width);
#   endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLogObjExitR_
#  endif // defined(__APPLE__)

EXTERN_C void ODLogObjExitS_(const char * fileName,
                             const char * funcName,
                             const int    lineNumber,
                             const void * objPtr,
                             const char * val)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    odDecreaseIndent_();
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_METHOD_FORMAT_ OD_FORMAT_S1_ "\n", OD_METHOD_PREFIX_(rootName), OD_EXIT_VALUE_,
                odNullOrString(val));
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(WIN32)
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_METHOD_FORMAT_ OD_FORMAT_S1_ "\n", OD_METHOD_PREFIX_(rootName), OD_EXIT_VALUE_,
                odNullOrString(val));
        fflush(OD_LOG_STREAM_);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
        NSLog(@OD_METHOD_FORMAT_ OD_FORMAT_S1_, OD_METHOD_PREFIX_(rootName), OD_EXIT_VALUE_, odNullOrString(val));
#   else // ! defined(__OBJC__)
        syslog(OD_LOG_LOG_LEVEL_, OD_METHOD_FORMAT_ OD_FORMAT_S1_, OD_METHOD_PREFIX_(rootName), OD_EXIT_VALUE_,
               odNullOrString(val));
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLogObjExitS_

EXTERN_C void ODLogObjExitThrowL_(const char *  fileName,
                                  const char *  funcName,
                                  const int     lineNumber,
                                  const void *  objPtr,
                                  const int32_t val)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    odDecreaseIndent_();
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_METHOD_FORMAT_ OD_FORMAT_EXIT_T_L_ "\n", OD_METHOD_PREFIX_(rootName), val, val);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(WIN32)
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_METHOD_FORMAT_ OD_FORMAT_EXIT_T_L_ "\n", OD_METHOD_PREFIX_(rootName), val, val);
        fflush(OD_LOG_STREAM_);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
        NSLog(@OD_METHOD_FORMAT_ OD_FORMAT_EXIT_T_L_, OD_METHOD_PREFIX_(rootName), val, val);
#   else // ! defined(__OBJC__)
        syslog(OD_LOG_LOG_LEVEL_, OD_METHOD_FORMAT_ OD_FORMAT_EXIT_T_L_, OD_METHOD_PREFIX_(rootName), val, val);
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLogObjExitThrowL_

EXTERN_C void ODLogObjExitThrowS_(const char * fileName,
                                  const char * funcName,
                                  const int    lineNumber,
                                  const void * objPtr,
                                  const char * val)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    odDecreaseIndent_();
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_METHOD_FORMAT_ OD_FORMAT_EXIT_T_S_ "\n", OD_METHOD_PREFIX_(rootName),
                odNullOrString(val));
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(WIN32)
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_METHOD_FORMAT_ OD_FORMAT_EXIT_T_S_ "\n", OD_METHOD_PREFIX_(rootName),
                odNullOrString(val));
        fflush(OD_LOG_STREAM_);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
        NSLog(@OD_METHOD_FORMAT_ OD_FORMAT_EXIT_T_S_, OD_METHOD_PREFIX_(rootName), odNullOrString(val));
#   else // ! defined(__OBJC__)
        syslog(OD_LOG_LOG_LEVEL_, OD_METHOD_FORMAT_ OD_FORMAT_EXIT_T_S_, OD_METHOD_PREFIX_(rootName),
               odNullOrString(val));
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLogObjExitThrowS_

EXTERN_C void ODLogP1_(const char * fileName,
                       const char * funcName,
                       const int    lineNumber,
                       const char * text1,
                       const void * ptr1)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_P1_ "\n", OD_FUNC_PREFIX_(rootName), text1, ptr1);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(WIN32)
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_FUNC_FORMAT_ OD_FORMAT_P1_ "\n", OD_FUNC_PREFIX_(rootName), text1, ptr1);
        fflush(OD_LOG_STREAM_);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_P1_, OD_FUNC_PREFIX_(rootName), text1, ptr1);
#   else // ! defined(__OBJC__)
        syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_P1_, OD_FUNC_PREFIX_(rootName), text1, ptr1);
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLogP1_

EXTERN_C void ODLogP2_(const char * fileName,
                       const char * funcName,
                       const int    lineNumber,
                       const char * text1,
                       const void * ptr1,
                       const char * text2,
                       const void * ptr2)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_P2_ "\n", OD_FUNC_PREFIX_(rootName), text1, ptr1, text2, ptr2);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(WIN32)
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_FUNC_FORMAT_ OD_FORMAT_P2_ "\n", OD_FUNC_PREFIX_(rootName), text1, ptr1, text2,
                ptr2);
        fflush(OD_LOG_STREAM_);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_P2_, OD_FUNC_PREFIX_(rootName), text1, ptr1, text2, ptr2);
#   else // ! defined(__OBJC__)
        syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_P2_, OD_FUNC_PREFIX_(rootName), text1, ptr1, text2,
               ptr2);
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLogP2_

EXTERN_C void ODLogP3_(const char * fileName,
                       const char * funcName,
                       const int    lineNumber,
                       const char * text1,
                       const void * ptr1,
                       const char * text2,
                       const void * ptr2,
                       const char * text3,
                       const void * ptr3)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_P2_ "\n", OD_FUNC_PREFIX_(rootName), text1, ptr1, text2, ptr2);
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_P1_ "\n", OD_FUNC_PREFIX_(rootName), text3, ptr3);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(WIN32)
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_FUNC_FORMAT_ OD_FORMAT_P2_ "\n", OD_FUNC_PREFIX_(rootName), text1, ptr1, text2,
                ptr2);
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_FUNC_FORMAT_ OD_FORMAT_P1_ "\n", OD_FUNC_PREFIX_(rootName), text3, ptr3);
        fflush(OD_LOG_STREAM_);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_P2_, OD_FUNC_PREFIX_(rootName), text1, ptr1, text2, ptr2);
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_P1_, OD_FUNC_PREFIX_(rootName), text3, ptr3);
#   else // ! defined(__OBJC__)
        syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_P2_, OD_FUNC_PREFIX_(rootName), text1, ptr1, text2,
               ptr2);
        syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_P1_, OD_FUNC_PREFIX_(rootName), text3, ptr3);
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLogP3_

EXTERN_C void ODLogP4_(const char * fileName,
                       const char * funcName,
                       const int    lineNumber,
                       const char * text1,
                       const void * ptr1,
                       const char * text2,
                       const void * ptr2,
                       const char * text3,
                       const void * ptr3,
                       const char * text4,
                       const void * ptr4)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_P2_ "\n", OD_FUNC_PREFIX_(rootName), text1, ptr1, text2, ptr2);
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_P2_ "\n", OD_FUNC_PREFIX_(rootName), text3, ptr3, text4, ptr4);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(WIN32)
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_FUNC_FORMAT_ OD_FORMAT_P2_ "\n", OD_FUNC_PREFIX_(rootName), text1, ptr1, text2,
                ptr2);
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_FUNC_FORMAT_ OD_FORMAT_P2_ "\n", OD_FUNC_PREFIX_(rootName), text3, ptr3, text4,
                ptr4);
        fflush(OD_LOG_STREAM_);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_P2_, OD_FUNC_PREFIX_(rootName), text1, ptr1, text2, ptr2);
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_P2_, OD_FUNC_PREFIX_(rootName), text3, ptr3, text4, ptr4);
#   else // ! defined(__OBJC__)
        syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_P2_, OD_FUNC_PREFIX_(rootName), text1, ptr1, text2,
               ptr2);
        syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_P2_, OD_FUNC_PREFIX_(rootName), text3, ptr3, text4,
               ptr4);
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLogP4_

EXTERN_C void ODLogPacket_(const char * fileName,
                           const char * funcName,
                           const int    lineNumber,
                           const char * caption,
                           const char * buffer,
                           const int    size)
{
    const char * heading = caption;
    const char * rootName = odFileNameRoot_(fileName);
    char         lineBuffer[(OD_MAX_BYTES_IN_LINE_ * 3) + 1];
    char         charBuffer[OD_MAX_BYTES_IN_LINE_ + 1];
    static char  hexDigits[] = "0123456789ABCDEF";
    size_t       captionLength = strlen(caption);
    char *       blankCaption = (char *) malloc(captionLength + 1);
    
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_PACKET_1_ "\n", OD_FUNC_PREFIX_(rootName), buffer, size);
    }
    else
    {
#  if defined(WIN32)
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_FUNC_FORMAT_ OD_FORMAT_PACKET_1_ "\n", OD_FUNC_PREFIX_(rootName), buffer, size);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_PACKET_1_, OD_FUNC_PREFIX_(rootName), buffer, size);
#   else // ! defined(__OBJC__)
        syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_PACKET_1_, OD_FUNC_PREFIX_(rootName), buffer, size);
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
    }
    memset(blankCaption, ' ', captionLength);
    blankCaption[captionLength] = '\0';
    memset(lineBuffer, ' ', sizeof(lineBuffer) - 1);
    lineBuffer[sizeof(lineBuffer) - 1] = '\0';
    for (int left = size, ii = 0; left > 0; left -= OD_MAX_BYTES_IN_LINE_, ii += OD_MAX_BYTES_IN_LINE_)
    {
        int ww = ((left > OD_MAX_BYTES_IN_LINE_) ? OD_MAX_BYTES_IN_LINE_ : left);
        
        for (int jj = 0; jj < ww; ++jj)
        {
            char bb = buffer[ii + jj];
            
            lineBuffer[jj * 3] = hexDigits[(bb >> 4) & 0x0F];
            lineBuffer[(jj * 3) + 1] = hexDigits[bb & 0x0F];
            charBuffer[jj] = (isprint(bb) ? bb : '.');
        }
        charBuffer[ww] = '\0';
        if (lOdLogFile_)
        {
            odWriteTime_(lOdLogFile_);
            fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_PACKET_2_ "\n", OD_FUNC_PREFIX_(rootName), heading, ii,
                    lineBuffer, charBuffer);
        }
        else
        {
#  if defined(WIN32)
            odWriteTime_(OD_LOG_STREAM_);
            fprintf(OD_LOG_STREAM_, OD_FUNC_FORMAT_ OD_FORMAT_PACKET_2_ "\n", OD_FUNC_PREFIX_(rootName), heading, ii,
                    lineBuffer, charBuffer);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
            NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_PACKET_2_, OD_FUNC_PREFIX_(rootName), heading, ii, lineBuffer,
                  charBuffer);
#   else // ! defined(__OBJC__)
            syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_PACKET_2_, OD_FUNC_PREFIX_(rootName), heading,
                   ii, lineBuffer, charBuffer);
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
        }
        memset(lineBuffer, ' ', sizeof(lineBuffer) - 1);
        heading = blankCaption;
    }
    if (lOdLogFile_)
    {
        fflush(lOdLogFile_);
    }
#  if defined(WIN32)
    else
    {
        fflush(OD_LOG_STREAM_);
    }
#  endif // ! defined(WIN32)
    free(blankCaption);
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLogPacket_

#  if defined(__APPLE__)
EXTERN_C void ODLogRect_(const char * fileName,
                         const char * funcName,
                         const int    lineNumber,
                         const char * caption,
                         const CGRect rect)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_RECT_ "\n", OD_FUNC_PREFIX_(rootName), caption, rect.origin.x,
                rect.origin.y, rect.size.height, rect.size.width);
        fflush(lOdLogFile_);
    }
    else
    {
#   if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_RECT_, OD_FUNC_PREFIX_(rootName), caption, rect.origin.x, rect.origin.y,
              rect.size.height, rect.size.width);
#   else // ! defined(__OBJC__)
        syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_RECT_, OD_FUNC_PREFIX_(rootName), caption, rect.origin.x,
               rect.origin.y, rect.size.height, rect.size.width);
#   endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLogRect_
#  endif // defined(__APPLE__)

EXTERN_C void ODLogS1_(const char * fileName,
                       const char * funcName,
                       const int    lineNumber,
                       const char * text1,
                       const char * val1)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_S1_ "\n", OD_FUNC_PREFIX_(rootName), text1,
                odNullOrString(val1));
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(WIN32)
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_FUNC_FORMAT_ OD_FORMAT_S1_ "\n", OD_FUNC_PREFIX_(rootName), text1,
                odNullOrString(val1));
        fflush(OD_LOG_STREAM_);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_S1_, OD_FUNC_PREFIX_(rootName), text1, odNullOrString(val1));
#   else // ! defined(__OBJC__)
        syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_S1_, OD_FUNC_PREFIX_(rootName), text1,
               odNullOrString(val1));
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLogS1_

EXTERN_C void ODLogS2_(const char * fileName,
                       const char * funcName,
                       const int    lineNumber,
                       const char * text1,
                       const char * val1,
                       const char * text2,
                       const char * val2)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_S2_ "\n", OD_FUNC_PREFIX_(rootName), text1,
                odNullOrString(val1), text2, odNullOrString(val2));
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(WIN32)
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_FUNC_FORMAT_ OD_FORMAT_S2_ "\n", OD_FUNC_PREFIX_(rootName), text1,
                odNullOrString(val1), text2, odNullOrString(val2));
        fflush(OD_LOG_STREAM_);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_S2_, OD_FUNC_PREFIX_(rootName), text1, odNullOrString(val1), text2,
              odNullOrString(val2));
#   else // ! defined(__OBJC__)
        syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_S2_, OD_FUNC_PREFIX_(rootName), text1,
               odNullOrString(val1), text2, odNullOrString(val2));
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLogS2_

EXTERN_C void ODLogS3_(const char * fileName,
                       const char * funcName,
                       const int    lineNumber,
                       const char * text1,
                       const char * val1,
                       const char * text2,
                       const char * val2,
                       const char * text3,
                       const char * val3)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_S2_ "\n", OD_FUNC_PREFIX_(rootName), text1,
                odNullOrString(val1), text2, odNullOrString(val2));
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_S1_ "\n", OD_FUNC_PREFIX_(rootName), text3,
                odNullOrString(val3));
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(WIN32)
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_FUNC_FORMAT_ OD_FORMAT_S2_ "\n", OD_FUNC_PREFIX_(rootName), text1,
                odNullOrString(val1), text2, odNullOrString(val2));
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_FUNC_FORMAT_ OD_FORMAT_S1_ "\n", OD_FUNC_PREFIX_(rootName), text3,
                odNullOrString(val3));
        fflush(OD_LOG_STREAM_);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_S2_, OD_FUNC_PREFIX_(rootName), text1, odNullOrString(val1), text2,
              odNullOrString(val2));
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_S1_, OD_FUNC_PREFIX_(rootName), text3, odNullOrString(val3));
#   else // ! defined(__OBJC__)
        syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_S2_, OD_FUNC_PREFIX_(rootName), text1,
               odNullOrString(val1), text2, odNullOrString(val2));
        syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_S1_, OD_FUNC_PREFIX_(rootName), text3,
               odNullOrString(val3));
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLogS3_

EXTERN_C void ODLogS4_(const char * fileName,
                       const char * funcName,
                       const int    lineNumber,
                       const char * text1,
                       const char * val1,
                       const char * text2,
                       const char * val2,
                       const char * text3,
                       const char * val3,
                       const char * text4,
                       const char * val4)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_S2_ "\n", OD_FUNC_PREFIX_(rootName), text1,
                odNullOrString(val1), text2, odNullOrString(val2));
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_S2_ "\n", OD_FUNC_PREFIX_(rootName), text3,
                odNullOrString(val3), text4, odNullOrString(val4));
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(WIN32)
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_FUNC_FORMAT_ OD_FORMAT_S2_ "\n", OD_FUNC_PREFIX_(rootName), text1,
                odNullOrString(val1), text2, odNullOrString(val2));
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_FUNC_FORMAT_ OD_FORMAT_S2_ "\n", OD_FUNC_PREFIX_(rootName), text3,
                odNullOrString(val3), text4, odNullOrString(val4));
        fflush(OD_LOG_STREAM_);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_S2_, OD_FUNC_PREFIX_(rootName), text1, odNullOrString(val1), text2,
              odNullOrString(val2));
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_S2_, OD_FUNC_PREFIX_(rootName), text3, odNullOrString(val3), text4,
              odNullOrString(val4));
#   else // ! defined(__OBJC__)
        syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_S2_, OD_FUNC_PREFIX_(rootName), text1,
               odNullOrString(val1), text2, odNullOrString(val2));
        syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_S2_, OD_FUNC_PREFIX_(rootName), text3,
               odNullOrString(val3), text4, odNullOrString(val4));
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLogS4_

EXTERN_C void ODLogSp_(const char * fileName,
                       const char * funcName,
                       const int    lineNumber,
                       const char * text,
                       const int    len,
                       const char * val)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_SP_ "\n", OD_FUNC_PREFIX_(rootName), text, len, val ? val : "");
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(WIN32)
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_FUNC_FORMAT_ OD_FORMAT_SP_ "\n", OD_FUNC_PREFIX_(rootName), text, len,
                val ? val : "");
        fflush(OD_LOG_STREAM_);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_SP_, OD_FUNC_PREFIX_(rootName), text, len, val ? val : "");
#   else // ! defined(__OBJC__)
        syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_SP_, OD_FUNC_PREFIX_(rootName), text, len,
               val ? val : "");
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLogSp_

EXTERN_C void ODLogTi_(const char *           fileName,
                       const char *           funcName,
                       const int              lineNumber,
                       const char *           text1,
                       const struct timeval * val1)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_TI_ "\n", OD_FUNC_PREFIX_(rootName), text1, val1->tv_sec,
                (long) val1->tv_usec);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(WIN32)
        odWriteTime_(OD_LOG_STREAM_);
        fprintf(OD_LOG_STREAM_, OD_FUNC_FORMAT_ OD_FORMAT_TI_ "\n", OD_FUNC_PREFIX_(rootName), text1, val1->tv_sec,
                (long) val1->tv_usec);
        fflush(OD_LOG_STREAM_);
#  else // ! defined(WIN32)
#   if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_TI_, OD_FUNC_PREFIX_(rootName), text1, val1->tv_sec, (long) val1->tv_usec);
#   else // ! defined(__OBJC__)
        syslog(OD_LOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_TI_, OD_FUNC_PREFIX_(rootName), text1, val1->tv_sec,
               (long) val1->tv_usec);
#   endif // ! defined(__OBJC__)
#  endif // ! defined(WIN32)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODLogTi_

# endif // defined(OD_ENABLE_LOGGING)
#endif // ! defined(ODLOGGING_INC_C_)
