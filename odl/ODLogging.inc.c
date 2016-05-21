//--------------------------------------------------------------------------------------------------
//
//  File:       odl/ODLogging.inc.c
//
//  Project:    m+m
//
//  Contains:   The function definitions for the logging facility.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2013 by OpenDragon.
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
//  Created:    2013-04-07
//
//--------------------------------------------------------------------------------------------------

#if (! defined(ODLOGGING_INC_C_))
# define ODLOGGING_INC_C_  /* Header guard */

# include <odl/ODLogging.h>

# if (defined(__APPLE__) || defined(__linux))
#  include <pthread.h>
#  include <unistd.h>
# endif // defined(__APPLE__) || defined(__linux)
# include <stdlib.h>
# include <string.h>
# include <time.h>
# include <ctype.h>
# include <errno.h>
# include <stdio.h>
# include <inttypes.h>
# if defined(__OBJC__)
#  import "Foundation/NSObjCRuntime.h"
# elif (defined(__APPLE__) || defined(__linux))
#  include <syslog.h>
# endif // defined(__APPLE__) || defined(__linux)

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 @brief The function definitions for the logging facility. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

/*! @brief Make sure that the function will have a 'C'-style external name. */
# if defined(__cplusplus)
#  define EXTERN_C extern "C"
# else // ! defined(__cpluplus)
#  define EXTERN_C /* */
# endif // ! defined(__cplusplus)

# if defined(ODL_ENABLE_LOGGING_)
/*! @brief The number of bytes to be displayed per line when logging a memory region. */
#  define ODL_MAX_BYTES_IN_LINE_ 16

/*! @brief The maximum size of the output displayed per line when logging a memory region. */
#  define ODL_MAX_CHARS_IN_LINE_ (ODL_MAX_BYTES_IN_LINE_ * 4)

/*! @brief The initial indentation level. */
#  define ODL_INIT_VALUE_ 1

/*! @brief The @c syslog facility to be used for logging. */
#  define ODL_TARGET_ LOG_LOCAL7

/*! @brief The @c syslog level to be used for logging. */
#  define ODL_LEVEL_  LOG_NOTICE

/*! @brief The format to be used to log the prefix and indentation strings with functions. */
#  define ODL_FUNC_FORMAT_ \
        "%s%s" ODL_FUNC_WHERE_

/*! @brief The standard parameters to be used with each logged message for functions. */
#  define ODL_FUNC_PREFIX_(file_) \
        prefixString_, indentString_, funcName, file_, lineNumber

/*! @brief The format to be used to log the function name, file name and line number. */
#  define ODL_FUNC_WHERE_ \
        "%s{%s@%04d}"

/*! @brief The format to be used to log the prefix and indentation strings with methods. */
#  define ODL_METHOD_FORMAT_ \
        "%s%s" ODL_METHOD_WHERE_

/*! @brief The standard parameters to be used with each logged message for methods. */
#  define ODL_METHOD_PREFIX_(file_) \
        prefixString_, indentString_, funcName, file_, lineNumber, objPtr

/*! @brief The format to be used to log the method name, file name and line number. */
#  define ODL_METHOD_WHERE_ \
        "%s{%s@%04d}[%p]"

/*! @brief The standard variable used to hold the indentation string. */
#  define ODL_CREATE_INDENT_() \
        char * indentString_ = odBuildIndent_()

/*! @brief The standard variable used to hold the prefix string. */
#  define ODL_CREATE_PREFIX_() \
        char * prefixString_ = odBuildPrefix_()

/*! @brief The standard code to release the memory associated with the indentation string. */
#  define ODL_FREE_INDENT_() \
        free(indentString_)

/*! @brief The standard code to release the memory associated with the prefix string. */
#  define ODL_FREE_PREFIX_() \
        free(prefixString_)

#  if (! MAC_OR_LINUX_)
#   define ODL_STREAM_ stderr  /* The output stream to use for logging with Windows. */
#  endif // ! MAC_OR_LINUX_

/*! @brief The data associated with each thread for logging. */
typedef struct tOdThreadData_
{
    /*! @brief The thread indentation level. */
    int _indentLevel;

} tOdThreadData;

#  if MAC_OR_LINUX_
/*! @brief @c true if thread support is enabled and @c false if all threads share data. */
static bool lOdEnableThreadSupport_ = false;

/*! @brief @c true if the process identifier is to be logged and @c false otherwise. */
static bool lOdIncludeProcessID_ = false;

/*! @brief @c true if the thread identifier is to be logged and @c false otherwise. */
static bool lOdIncludeThreadID_ = false;
#  endif // MAC_OR_LINUX_

/*! @brief The stream to be used when logging to a file. */
static FILE * lOdLogFile_ = NULL;

#  if MAC_OR_LINUX_
/*! @brief The thread key to be used. */
static pthread_key_t lOdThreadSpecificKey_;

/*! @brief The once-only variable to be used with threading. */
static pthread_once_t lOdThreadSpecificKeyOnce_ = PTHREAD_ONCE_INIT;
#  endif // MAC_OR_LINUX_

/*! @brief The thread data to be used when thread support is not enabled. */
static tOdThreadData lOdThreadData_ =
{
    ODL_INIT_VALUE_
};

/*! @brief Return a string corresponding to each @c bool value.
 @param[in] val The input value.
 @returns Either "true" or "false", depending on the input value. */
inline static const char *
odBoolToString_(const bool val)
{
    return (val ? "true" : "false");
} // odBoolToString_

#  if defined(__OBJC__)
/*! @brief Return either the @c description string for an object or a fixed string.
 @param[in] value The input object.
 @returns The description of the object or "<>". */
static const char *
odNullOrDescription(id value)
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
 @param[in] aString The input string.
 @returns The input string or "<>". */
static const char *
odNullOrString(const char * aString)
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

#  if MAC_OR_LINUX_
/*! @brief Release the data associated with a thread.
 @param[in] data A pointer to the data to be released. */
static void
odReleaseThreadSpecificData_(void * data)
{
    tOdThreadData * stuff = (tOdThreadData *) data;

    pthread_setspecific(lOdThreadSpecificKey_, NULL);
    free(stuff);
} // odReleaseThreadSpecificData_
#  endif // MAC_OR_LINUX_

#  if MAC_OR_LINUX_
/*! @brief Create the thread key and record the 'release' function for thread-specific data. */
static void
odSetUpThreadKey_(void)
{
    if (pthread_key_create(&lOdThreadSpecificKey_, odReleaseThreadSpecificData_))
    {
#   if defined(__OBJC__)
        NSLog(@ "problem creating thread-specific key => %d", errno);
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, "problem creating thread-specific key => %d", errno);
#   endif // ! defined(__OBJC__)
    }
} // odSetUpThreadKey_
#  endif // MAC_OR_LINUX_

/*! @brief Get a pointer to the thread-specific data.

 If this is the first call for a thread, create the data. If threading is not supported, return the
 address of the shared thread data.
 @returns A pointer to the thread-specific data. */
inline static tOdThreadData *
odGetThreadData_(void)
{
    tOdThreadData * stuff;

#  if MAC_OR_LINUX_
    if (lOdEnableThreadSupport_)
    {
        pthread_once(&lOdThreadSpecificKeyOnce_, odSetUpThreadKey_);
        stuff = (tOdThreadData *) pthread_getspecific(lOdThreadSpecificKey_);
        if (! stuff)
        {
            stuff = (tOdThreadData *) malloc(sizeof(tOdThreadData));
            stuff->_indentLevel = ODL_INIT_VALUE_;
            pthread_setspecific(lOdThreadSpecificKey_, stuff);
        }
    }
    else
#  endif // MAC_OR_LINUX_
    {
        stuff = &lOdThreadData_;
    }
    return stuff;
} // odGetThreadData_

/*! @brief Return the portion of a file name that does not include the path to the file.
 @param[in] fileName The input file path.
 @returns The file name part of a file path. */
static const char *
odFileNameRoot_(const char * fileName)
{
    const char * result = strrchr(fileName, '/');

    return (result ? (result + 1) : fileName);
} // odFileNameRoot_

/*! @brief Return the current indentation level for the active thread.
 @returns The current indentation level for the active thread. */
inline static int
odGetIndent_(void)
{
    tOdThreadData * stuff = odGetThreadData_();

    return stuff->_indentLevel;
} // odGetIndent_

/*! @brief Set the current indentation level for the active thread.
 @param[in] value The new indentation level. */
inline static void
odSetIndent_(const int value)
{
    tOdThreadData * stuff = odGetThreadData_();

    stuff->_indentLevel = value;
} // odSetIndent_

/*! @brief Generate an indentation string.
 @returns A string of alternating spaces and periods that whose length matches the current
 indentation level for the active thread. */
static char *
odBuildIndent_(void)
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
 @returns A string containing the process identifier and / or the thread identifier, if they are
 enabled for logging. */
static char *
odBuildPrefix_(void)
{
    char *              result;
    size_t              length;
#  if MAC_OR_LINUX_
    static const size_t lengthHexString = 20; // Enough digits for an 8-byte value in hexadecimal,
                                              // plus a little more.
#  endif // MAC_OR_LINUX_

#  if MAC_OR_LINUX_
    if (lOdIncludeProcessID_)
    {
        if (lOdIncludeThreadID_)
        {
            length = (2 * lengthHexString) + 3; // '<', ':', '>'
            result = (char *) malloc(length + 1);
            snprintf(result, length, "<%#lx:%#lx>",
                     (long unsigned) getpid(), (long unsigned) pthread_self());
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
#  endif // MAC_OR_LINUX_
    {
        length = 0;
        result = (char *) malloc(length + 1);
    }
    result[length] = '\0';
    return result;
} // odBuildPrefix_

/*! @brief Reduce the current indentation level for the active thread. */
inline static void
odDecreaseIndent_(void)
{
    odSetIndent_(odGetIndent_() - 1);
} // odDecreaseIndent_

/*! @brief Increase the current indentation level for the active thread. */
inline static void
odIncreaseIndent_(void)
{
    odSetIndent_(odGetIndent_() + 1);
} // odIncreaseIndent_

/*! @brief Write the date and time to a file stream. */
static void
odWriteTime_(FILE * outFile)
{
    char      buffer[80];
    time_t    rawtime;
#  if (! MAC_OR_LINUX_)
    struct tm aTm;
#  endif // ! MAC_OR_LINUX_

    time(&rawtime);
#  if MAC_OR_LINUX_
    strftime(buffer, sizeof(buffer), "%F %T ", localtime(&rawtime));
#  else // ! MAC_OR_LINUX_
    localtime_s(&aTm, &rawtime);
    strftime(buffer, sizeof(buffer), "%x %X ", &aTm);
#  endif // ! MAC_OR_LINUX_
    fputs(buffer, outFile);
} // odWriteTime_

/*! @brief The value prefix string to be used when exiting a function or method. */
#  define ODL_EXIT_VALUE_       "exit -> "

/*! @brief The format string to be used with a single boolean value. */
#  define ODL_FORMAT_B1_        " %s%s"

/*! @brief The format string to be used with a pair of boolean values. */
#  define ODL_FORMAT_B2_        " %s%s, %s%s"

/*! @brief The format string to be used with a single character value. */
#  define ODL_FORMAT_C1_        " %s'%c'(%#02X)"

/*! @brief The format string to be used with a pair of character values. */
#  define ODL_FORMAT_C2_        " %s'%c'(%#02X), %s'%c'(%#02X)"

/*! @brief The format string to be used with a single double value. */
#  define ODL_FORMAT_D1_        " %s%g"

/*! @brief The format string to be used with a pair of double values. */
#  define ODL_FORMAT_D2_        " %s%g, %s%g"

/*! @brief The message string to be used when entering a function or method. */
#  define ODL_FORMAT_ENTER_     " enter"

/*! @brief The message string to be used when exiting a function or method. */
#  define ODL_FORMAT_EXIT_      " exit"

/*! @brief The format string to be used when exiting a function or method via @c exit. */
#  define ODL_FORMAT_EXIT_CALL_ " call exit(%ld(%#lx))"

/*! @brief The format string to be used when exiting a function or method via @c throw of an
 integer. */
#  define ODL_FORMAT_EXIT_T_L_  " throw(%d)"

/*! @brief The format string to be used when exiting a function or method via @c throw of a
 string. */
#  define ODL_FORMAT_EXIT_T_S_  " throw(%s)"

/*! @brief The format string to be used when exiting a function or method via @c throw of an
 integer. */
#  define ODL_FORMAT_EXIT_T_X_  " throw(%d(%#x))"

/*! @brief The format string to be used with an IP address and port value. */
#  define ODL_FORMAT_IP_        " %s%d.%d.%d.%d:%d"

/*! @brief The format string to be used with a single long value. */
#  define ODL_FORMAT_L1_        " %s%d"

/*! @brief The format string to be used with a pair of long values. */
#  define ODL_FORMAT_L2_        " %s%d, %s%d"

/*! @brief The format string to be used with a single int64_t value. */
#  define ODL_FORMAT_LL1_      " %s%" PRId64

/*! @brief The format string to be used with a pair of int64_t values. */
#  define ODL_FORMAT_LL2_      " %s%" PRId64 ", %s%" PRId64

/*! @brief The format string to be used with a message. */
#  define ODL_FORMAT_LOG_       " %s"

/*! @brief The format string to be used with a long string value. */
#  define ODL_FORMAT_LS         " %s | %s |"

/*! @brief The format string to be used with a single object value. */
#  define ODL_FORMAT_O1_        " %s%s"

/*! @brief The format string to be used with a pair of object values. */
#  define ODL_FORMAT_O2_        " %s%s, %s%s"

/*! @brief The format string to be used with a single pointer value. */
#  define ODL_FORMAT_P1_        " %s%p"

/*! @brief The format string to be used with a pair of pointer values. */
#  define ODL_FORMAT_P2_        " %s%p, %s%p"

/*! @brief The format string to be used for the heading when logging a memory region. */
#  define ODL_FORMAT_PACKET_1_  " address = %p, size = %d"

/*! @brief The format string to be used when logging a section of a memory region. */
#  define ODL_FORMAT_PACKET_2_  " %s | %04X : %s| %s"

/*! @brief The format string to be used with a rectangle value. */
#  define ODL_FORMAT_RECT_      " %s[l: %g, t: %g, h: %g, w: %g]"

/*! @brief The format string to be used with a single string value. */
#  define ODL_FORMAT_S1_        " %s'%s'"

/*! @brief The format string to be used with a pair of string values. */
#  define ODL_FORMAT_S2_        " %s'%s', %s'%s'"

/*! @brief The format string to be used with a size value. */
#  define ODL_FORMAT_SIZE_      " %s[h: %g, w: %g]"

/*! @brief The format string to be used with a (possibly unterminated) string value. */
#  define ODL_FORMAT_SP_        " %s'%.*s'"

/*! @brief The format string to be used with a time value. */
#  define ODL_FORMAT_TI_        " %s%ld:%ld"

/*! @brief The format string to be used with a single long hexadecimal value. */
#  define ODL_FORMAT_X1_        " %s%d(%#x)"

/*! @brief The format string to be used with a pair of long hexadecimal values. */
#  define ODL_FORMAT_X2_        " %s%d(%#x), %s%d(%#x)"

/*! @brief The format string to be used with a single int64_t hexadecimal value. */
#  define ODL_FORMAT_XL1_      " %s%" PRId64 "(%" PRIx64 ")"

/*! @brief The format string to be used with a pair of long long hexadecimal values. */
#  define ODL_FORMAT_XL2_      " %s%" PRId64 "(%" PRIx64 "), %s%" PRId64 "(%" PRIx64 ")"

/*! @brief The message string to be used when setting up logging for the first time. */
#  define ODL_INIT_FORMAT_      "* %s%s" ODL_FUNC_WHERE_ " started *"

EXTERN_C void
ODLog_(const char * fileName,
       const char * funcName,
       const int    lineNumber,
       const char * text)
{
    const char * rootName = odFileNameRoot_(fileName);

    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_LOG_ "\n", ODL_FUNC_PREFIX_(rootName),
                text ? text : "");
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_LOG_, ODL_FUNC_PREFIX_(rootName), text ? text : "");
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_LOG_, ODL_FUNC_PREFIX_(rootName),
               text ? text : "");
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_LOG_ "\n", ODL_FUNC_PREFIX_(rootName),
                text ? text : "");
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLog_

EXTERN_C void
ODLogB1_(const char * fileName,
         const char * funcName,
         const int    lineNumber,
         const char * text1,
         const bool   val1)
{
    const char * rootName = odFileNameRoot_(fileName);

    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_B1_ "\n", ODL_FUNC_PREFIX_(rootName), text1,
                odBoolToString_(val1));
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_B1_, ODL_FUNC_PREFIX_(rootName), text1,
              odBoolToString_(val1));
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_B1_, ODL_FUNC_PREFIX_(rootName), text1,
               odBoolToString_(val1));
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_B1_ "\n", ODL_FUNC_PREFIX_(rootName),
                text1, odBoolToString_(val1));
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogB1_

EXTERN_C void
ODLogB2_(const char * fileName,
         const char * funcName,
         const int    lineNumber,
         const char * text1,
         const bool   val1,
         const char * text2,
         const bool   val2)
{
    const char * rootName = odFileNameRoot_(fileName);

    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_B2_ "\n", ODL_FUNC_PREFIX_(rootName), text1,
                odBoolToString_(val1), text2, odBoolToString_(val2));
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_B2_, ODL_FUNC_PREFIX_(rootName), text1,
              odBoolToString_(val1), text2,
              odBoolToString_(val2));
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_B2_, ODL_FUNC_PREFIX_(rootName), text1,
               odBoolToString_(val1), text2, odBoolToString_(val2));
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_B2_ "\n", ODL_FUNC_PREFIX_(rootName),
                text1, odBoolToString_(val1), text2, odBoolToString_(val2));
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogB2_

EXTERN_C void
ODLogB3_(const char * fileName,
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

    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_B2_ "\n", ODL_FUNC_PREFIX_(rootName), text1,
                odBoolToString_(val1), text2, odBoolToString_(val2));
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_B1_ "\n", ODL_FUNC_PREFIX_(rootName), text3,
                odBoolToString_(val3));
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_B2_, ODL_FUNC_PREFIX_(rootName), text1,
              odBoolToString_(val1), text2, odBoolToString_(val2));
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_B1_, ODL_FUNC_PREFIX_(rootName), text3,
              odBoolToString_(val3));
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_B2_, ODL_FUNC_PREFIX_(rootName), text1,
               odBoolToString_(val1), text2, odBoolToString_(val2));
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_B1_, ODL_FUNC_PREFIX_(rootName), text3,
               odBoolToString_(val3));
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_B2_ "\n", ODL_FUNC_PREFIX_(rootName),
                text1, odBoolToString_(val1), text2, odBoolToString_(val2));
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_B1_ "\n", ODL_FUNC_PREFIX_(rootName),
                text3, odBoolToString_(val3));
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogB3_

EXTERN_C void
ODLogB4_(const char * fileName,
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

    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_B2_ "\n", ODL_FUNC_PREFIX_(rootName), text1,
                odBoolToString_(val1), text2, odBoolToString_(val2));
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_B2_ "\n", ODL_FUNC_PREFIX_(rootName), text3,
                odBoolToString_(val3), text4, odBoolToString_(val4));
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_B2_, ODL_FUNC_PREFIX_(rootName), text1,
              odBoolToString_(val1), text2, odBoolToString_(val2));
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_B2_, ODL_FUNC_PREFIX_(rootName), text3,
              odBoolToString_(val3), text4, odBoolToString_(val4));
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_B2_, ODL_FUNC_PREFIX_(rootName), text1,
               odBoolToString_(val1), text2, odBoolToString_(val2));
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_B2_, ODL_FUNC_PREFIX_(rootName), text3,
               odBoolToString_(val3), text4, odBoolToString_(val4));
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_B2_ "\n", ODL_FUNC_PREFIX_(rootName),
                text1, odBoolToString_(val1), text2, odBoolToString_(val2));
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_B2_ "\n", ODL_FUNC_PREFIX_(rootName),
                text3, odBoolToString_(val3), text4, odBoolToString_(val4));
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogB4_

EXTERN_C void
ODLogC1_(const char * fileName,
         const char * funcName,
         const int    lineNumber,
         const char * text1,
         const char   val1)
{
    const char * rootName = odFileNameRoot_(fileName);

    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_C1_ "\n", ODL_FUNC_PREFIX_(rootName), text1,
                val1, val1);
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_C1_, ODL_FUNC_PREFIX_(rootName), text1, val1, val1);
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_C1_, ODL_FUNC_PREFIX_(rootName), text1,
               val1, val1);
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_C1_ "\n", ODL_FUNC_PREFIX_(rootName),
                text1, val1, val1);
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogC1_

EXTERN_C void
ODLogC2_(const char * fileName,
         const char * funcName,
         const int    lineNumber,
         const char * text1,
         const char   val1,
         const char * text2,
         const char   val2)
{
    const char * rootName = odFileNameRoot_(fileName);

    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_C2_ "\n", ODL_FUNC_PREFIX_(rootName), text1,
                val1, val1, text2, val2, val2);
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_C2_, ODL_FUNC_PREFIX_(rootName), text1, val1, val1, text2,
              val2, val2);
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_C2_, ODL_FUNC_PREFIX_(rootName), text1,
               val1, val1, text2, val2, val2);
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_C2_ "\n", ODL_FUNC_PREFIX_(rootName),
                text1, val1, val1, text2, val2, val2);
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogC2_

EXTERN_C void
ODLogC3_(const char * fileName,
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

    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_C2_ "\n", ODL_FUNC_PREFIX_(rootName), text1,
                val1, val1, text2, val2, val2);
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_C1_ "\n", ODL_FUNC_PREFIX_(rootName), text3,
                val3, val3);
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_C2_, ODL_FUNC_PREFIX_(rootName), text1, val1, val1, text2,
              val2, val2);
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_C1_, ODL_FUNC_PREFIX_(rootName), text3, val3, val3);
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_C2_, ODL_FUNC_PREFIX_(rootName), text1,
               val1, val1, text2, val2, val2);
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_C1_, ODL_FUNC_PREFIX_(rootName), text3,
               val3, val3);
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_C2_ "\n", ODL_FUNC_PREFIX_(rootName),
                text1, val1, val1, text2, val2, val2);
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_C1_ "\n", ODL_FUNC_PREFIX_(rootName),
                text3, val3, val3);
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogC3_

EXTERN_C void
ODLogC4_(const char * fileName,
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

    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_C2_ "\n", ODL_FUNC_PREFIX_(rootName), text1,
                val1, val1, text2, val2, val2);
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_C2_ "\n", ODL_FUNC_PREFIX_(rootName), text3,
                val3, val3, text4, val4, val4);
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_C2_, ODL_FUNC_PREFIX_(rootName), text1, val1, val1, text2,
              val2, val2);
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_C2_, ODL_FUNC_PREFIX_(rootName), text3, val3, val3, text4,
              val4, val4);
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_C2_, ODL_FUNC_PREFIX_(rootName), text1,
               val1, val1, text2, val2, val2);
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_C2_, ODL_FUNC_PREFIX_(rootName), text3,
               val3, val3, text4, val4, val4);
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_C2_ "\n", ODL_FUNC_PREFIX_(rootName),
                text1, val1, val1, text2, val2, val2);
        odWriteTime_(lOdLogFile_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_C2_ "\n", ODL_FUNC_PREFIX_(rootName),
                text3, val3, val3, text4, val4, val4);
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogC4_

EXTERN_C void
ODLogD1_(const char * fileName,
         const char * funcName,
         const int    lineNumber,
         const char * text1,
         const double val1)
{
    const char * rootName = odFileNameRoot_(fileName);

    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_D1_ "\n", ODL_FUNC_PREFIX_(rootName),
                text1, val1);
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_D1_, ODL_FUNC_PREFIX_(rootName), text1, val1);
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_D1_, ODL_FUNC_PREFIX_(rootName), text1,
               val1);
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_D1_ "\n", ODL_FUNC_PREFIX_(rootName),
                text1, val1);
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogD1_

EXTERN_C void
ODLogD2_(const char * fileName,
         const char * funcName,
         const int    lineNumber,
         const char * text1,
         const double val1,
         const char * text2,
         const double val2)
{
    const char * rootName = odFileNameRoot_(fileName);

    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_D2_ "\n", ODL_FUNC_PREFIX_(rootName), text1,
                val1, text2, val2);
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_D2_, ODL_FUNC_PREFIX_(rootName), text1, val1, text2, val2);
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_D2_, ODL_FUNC_PREFIX_(rootName), text1,
               val1, text2, val2);
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_D2_ "\n", ODL_FUNC_PREFIX_(rootName),
                text1, val1, text2, val2);
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogD2_

EXTERN_C void
ODLogD3_(const char * fileName,
         const char * funcName,
         const int    lineNumber,
         const char * text1,
         const double val1,
         const char * text2,
         const double val2,
         const char * text3,
         const double val3)
{
    const char * rootName = odFileNameRoot_(fileName);

    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_D2_ "\n", ODL_FUNC_PREFIX_(rootName), text1,
                val1, text2, val2);
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_D1_ "\n", ODL_FUNC_PREFIX_(rootName), text3,
                val3);
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_D2_, ODL_FUNC_PREFIX_(rootName), text1, val1, text2, val2);
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_D1_, ODL_FUNC_PREFIX_(rootName), text3, val3);
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_D2_, ODL_FUNC_PREFIX_(rootName), text1,
               val1, text2, val2);
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_D1_, ODL_FUNC_PREFIX_(rootName), text3,
               val3);
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_D2_ "\n", ODL_FUNC_PREFIX_(rootName),
                text1, val1, text2, val2);
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_D1_ "\n", ODL_FUNC_PREFIX_(rootName),
                text3, val3);
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogD3_

EXTERN_C void
ODLogD4_(const char * fileName,
         const char * funcName,
         const int    lineNumber,
         const char * text1,
         const double val1,
         const char * text2,
         const double val2,
         const char * text3,
         const double val3,
         const char * text4,
         const double val4)
{
    const char * rootName = odFileNameRoot_(fileName);

    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_D2_ "\n", ODL_FUNC_PREFIX_(rootName), text1,
                val1, text2, val2);
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_D2_ "\n", ODL_FUNC_PREFIX_(rootName), text3,
                val3, text4, val4);
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_D2_, ODL_FUNC_PREFIX_(rootName), text1, val1, text2, val2);
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_D2_, ODL_FUNC_PREFIX_(rootName), text3, val3, text4, val4);
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_D2_, ODL_FUNC_PREFIX_(rootName), text1,
               val1, text2, val2);
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_D2_, ODL_FUNC_PREFIX_(rootName), text3,
               val3, text4, val4);
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_D2_ "\n", ODL_FUNC_PREFIX_(rootName),
                text1, val1, text2, val2);
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_D2_ "\n", ODL_FUNC_PREFIX_(rootName),
                text3, val3, text4, val4);
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogD4_

EXTERN_C void
ODLogEnter_(const char * fileName,
            const char * funcName,
            const int    lineNumber)
{
    const char * rootName = odFileNameRoot_(fileName);

    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_ENTER_ "\n", ODL_FUNC_PREFIX_(rootName));
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_ENTER_, ODL_FUNC_PREFIX_(rootName));
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_ENTER_, ODL_FUNC_PREFIX_(rootName));
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_ENTER_ "\n", ODL_FUNC_PREFIX_(rootName));
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
    odIncreaseIndent_();
} // ODLogEnter_

EXTERN_C void
ODLogExit_(const char * fileName,
           const char * funcName,
           const int    lineNumber)
{
    const char * rootName = odFileNameRoot_(fileName);

    odDecreaseIndent_();
    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_EXIT_ "\n", ODL_FUNC_PREFIX_(rootName));
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_EXIT_, ODL_FUNC_PREFIX_(rootName));
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_EXIT_, ODL_FUNC_PREFIX_(rootName));
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_EXIT_ "\n", ODL_FUNC_PREFIX_(rootName));
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogExit_

EXTERN_C void
ODLogExitB_(const char * fileName,
            const char * funcName,
            const int    lineNumber,
            const bool   val)
{
    const char * rootName = odFileNameRoot_(fileName);

    odDecreaseIndent_();
    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_B1_ "\n", ODL_FUNC_PREFIX_(rootName),
                ODL_EXIT_VALUE_, odBoolToString_(val));
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_B1_, ODL_FUNC_PREFIX_(rootName), ODL_EXIT_VALUE_,
              odBoolToString_(val));
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_B1_, ODL_FUNC_PREFIX_(rootName),
               ODL_EXIT_VALUE_, odBoolToString_(val));
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_B1_ "\n", ODL_FUNC_PREFIX_(rootName),
                ODL_EXIT_VALUE_, odBoolToString_(val));
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogExitB_

EXTERN_C void
ODLogExitC_(const char * fileName,
            const char * funcName,
            const int    lineNumber,
            const char   val)
{
    const char * rootName = odFileNameRoot_(fileName);

    odDecreaseIndent_();
    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_C1_ "\n", ODL_FUNC_PREFIX_(rootName),
                ODL_EXIT_VALUE_, val, val);
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_C1_, ODL_FUNC_PREFIX_(rootName), ODL_EXIT_VALUE_, val, val);
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_C1_, ODL_FUNC_PREFIX_(rootName),
               ODL_EXIT_VALUE_, val, val);
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_C1_ "\n", ODL_FUNC_PREFIX_(rootName),
                ODL_EXIT_VALUE_, val, val);
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogExitC_

EXTERN_C void
ODLogExitD_(const char * fileName,
            const char * funcName,
            const int    lineNumber,
            const double val)
{
    const char * rootName = odFileNameRoot_(fileName);

    odDecreaseIndent_();
    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_D1_ "\n", ODL_FUNC_PREFIX_(rootName),
                ODL_EXIT_VALUE_, val);
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_D1_, ODL_FUNC_PREFIX_(rootName), ODL_EXIT_VALUE_, val);
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_D1_, ODL_FUNC_PREFIX_(rootName),
               ODL_EXIT_VALUE_, val);
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_D1_ "\n", ODL_FUNC_PREFIX_(rootName),
                ODL_EXIT_VALUE_, val);
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogExitD_

EXTERN_C void
ODLogExitExit_(const char * fileName,
               const char * funcName,
               const int    lineNumber,
               const long   val)
{
    const char * rootName = odFileNameRoot_(fileName);

    odDecreaseIndent_();
    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_EXIT_CALL_ "\n", ODL_FUNC_PREFIX_(rootName),
                val, val);
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_EXIT_CALL_, ODL_FUNC_PREFIX_(rootName), val, val);
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_EXIT_CALL_, ODL_FUNC_PREFIX_(rootName),
               val, val);
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_EXIT_CALL_ "\n",
                ODL_FUNC_PREFIX_(rootName), val, val);
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogExitExit_

EXTERN_C void
ODLogExitL_(const char *  fileName,
            const char *  funcName,
            const int     lineNumber,
            const int32_t val)
{
    const char * rootName = odFileNameRoot_(fileName);

    odDecreaseIndent_();
    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_L1_ "\n", ODL_FUNC_PREFIX_(rootName),
                ODL_EXIT_VALUE_, val);
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_L1_, ODL_FUNC_PREFIX_(rootName), ODL_EXIT_VALUE_, val);
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_L1_, ODL_FUNC_PREFIX_(rootName),
               ODL_EXIT_VALUE_, val);
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_L1_ "\n", ODL_FUNC_PREFIX_(rootName),
                ODL_EXIT_VALUE_, val);
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogExitL_

EXTERN_C void
ODLogExitLL_(const char *  fileName,
             const char *  funcName,
             const int     lineNumber,
             const int64_t val)
{
    const char * rootName = odFileNameRoot_(fileName);

    odDecreaseIndent_();
    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_LL1_ "\n", ODL_FUNC_PREFIX_(rootName),
                ODL_EXIT_VALUE_, val);
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_LL1_, ODL_FUNC_PREFIX_(rootName), ODL_EXIT_VALUE_, val);
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_LL1_, ODL_FUNC_PREFIX_(rootName),
               ODL_EXIT_VALUE_, val);
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_LL1_ "\n", ODL_FUNC_PREFIX_(rootName),
                ODL_EXIT_VALUE_, val);
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogExitLL_

#  if defined(__OBJC__)
EXTERN_C void
ODLogExitO_(const char * fileName,
            const char * funcName,
            const int    lineNumber,
            const id     val)
{
    const char * rootName = odFileNameRoot_(fileName);
    const char * valString = odNullOrDescription(val);

    odDecreaseIndent_();
    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_O1_ "\n", ODL_FUNC_PREFIX_(rootName),
                ODL_EXIT_VALUE_, valString);
        fflush(lOdLogFile_);
    }
    else
    {
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_O1_, ODL_FUNC_PREFIX_(rootName), ODL_EXIT_VALUE_, valString);
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogExitO_
#  endif // defined(__OBJC__)

EXTERN_C void
ODLogExitP_(const char * fileName,
            const char * funcName,
            const int    lineNumber,
            const void * val)
{
    const char * rootName = odFileNameRoot_(fileName);

    odDecreaseIndent_();
    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_P1_ "\n", ODL_FUNC_PREFIX_(rootName),
                ODL_EXIT_VALUE_, val);
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_P1_, ODL_FUNC_PREFIX_(rootName), ODL_EXIT_VALUE_, val);
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_P1_, ODL_FUNC_PREFIX_(rootName),
               ODL_EXIT_VALUE_, val);
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_P1_ "\n", ODL_FUNC_PREFIX_(rootName),
                ODL_EXIT_VALUE_, val);
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogExitP_

#  if defined(__APPLE__)
EXTERN_C void
ODLogExitRect_(const char * fileName,
               const char * funcName,
               const int    lineNumber,
               const CGRect val)
{
    const char * rootName = odFileNameRoot_(fileName);

    odDecreaseIndent_();
    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_RECT_ "\n", ODL_FUNC_PREFIX_(rootName),
                ODL_EXIT_VALUE_, val.origin.x, val.origin.y, val.size.height, val.size.width);
        fflush(lOdLogFile_);
    }
    else
    {
#   if defined(__OBJC__)
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_RECT_, ODL_FUNC_PREFIX_(rootName), ODL_EXIT_VALUE_,
              val.origin.x, val.origin.y, val.size.height, val.size.width);
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_RECT_, ODL_FUNC_PREFIX_(rootName),
               ODL_EXIT_VALUE_, val.origin.x, val.origin.y, val.size.height, val.size.width);
#   endif // ! defined(__OBJC__)
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogExitRect_
#  endif // defined(__APPLE__)

EXTERN_C void
ODLogExitS_(const char * fileName,
            const char * funcName,
            const int    lineNumber,
            const char * val)
{
    const char * rootName = odFileNameRoot_(fileName);

    odDecreaseIndent_();
    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_S1_ "\n", ODL_FUNC_PREFIX_(rootName),
                ODL_EXIT_VALUE_, odNullOrString(val));
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_S1_, ODL_FUNC_PREFIX_(rootName), ODL_EXIT_VALUE_,
              odNullOrString(val));
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_S1_, ODL_FUNC_PREFIX_(rootName),
               ODL_EXIT_VALUE_, odNullOrString(val));
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_S1_ "\n", ODL_FUNC_PREFIX_(rootName),
                ODL_EXIT_VALUE_, odNullOrString(val));
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogExitS_

#  if defined(__APPLE__)
EXTERN_C void
ODLogExitSize_(const char * fileName,
               const char * funcName,
               const int    lineNumber,
               const CGSize val)
{
    const char * rootName = odFileNameRoot_(fileName);

    odDecreaseIndent_();
    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_SIZE_ "\n", ODL_FUNC_PREFIX_(rootName),
                ODL_EXIT_VALUE_, val.height, val.width);
        fflush(lOdLogFile_);
    }
    else
    {
#   if defined(__OBJC__)
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_SIZE_, ODL_FUNC_PREFIX_(rootName), ODL_EXIT_VALUE_,
              val.height, val.width);
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_SIZE_, ODL_FUNC_PREFIX_(rootName),
               ODL_EXIT_VALUE_, val.height, val.width);
#   endif // ! defined(__OBJC__)
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogExitSize_
#  endif // defined(__APPLE__)

EXTERN_C void
ODLogExitThrowL_(const char *  fileName,
                 const char *  funcName,
                 const int     lineNumber,
                 const int32_t val)
{
    const char * rootName = odFileNameRoot_(fileName);

    odDecreaseIndent_();
    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_EXIT_T_L_ "\n", ODL_FUNC_PREFIX_(rootName),
                val);
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_EXIT_T_L_, ODL_FUNC_PREFIX_(rootName), val);
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_EXIT_T_L_, ODL_FUNC_PREFIX_(rootName),
               val);
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_EXIT_T_L_ "\n", ODL_FUNC_PREFIX_(rootName),
                val);
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogExitThrowL_

EXTERN_C void
ODLogExitThrowS_(const char * fileName,
                 const char * funcName,
                 const int    lineNumber,
                 const char * val)
{
    const char * rootName = odFileNameRoot_(fileName);

    odDecreaseIndent_();
    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_EXIT_T_S_ "\n", ODL_FUNC_PREFIX_(rootName),
                odNullOrString(val));
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_EXIT_T_S_, ODL_FUNC_PREFIX_(rootName), odNullOrString(val));
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_EXIT_T_S_, ODL_FUNC_PREFIX_(rootName),
               odNullOrString(val));
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_EXIT_T_S_ "\n", ODL_FUNC_PREFIX_(rootName),
                odNullOrString(val));
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogExitThrowS_

EXTERN_C void
ODLogExitThrowX_(const char *  fileName,
                 const char *  funcName,
                 const int     lineNumber,
                 const int32_t val)
{
    const char * rootName = odFileNameRoot_(fileName);

    odDecreaseIndent_();
    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_EXIT_T_X_ "\n", ODL_FUNC_PREFIX_(rootName),
                val, val);
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_EXIT_T_X_, ODL_FUNC_PREFIX_(rootName), val, val);
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_EXIT_T_X_, ODL_FUNC_PREFIX_(rootName),
               val, val);
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_EXIT_T_X_ "\n", ODL_FUNC_PREFIX_(rootName),
                val, val);
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogExitThrowX_

EXTERN_C void
ODLogExitX_(const char *  fileName,
            const char *  funcName,
            const int     lineNumber,
            const int32_t val)
{
    const char * rootName = odFileNameRoot_(fileName);

    odDecreaseIndent_();
    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_X1_ "\n", ODL_FUNC_PREFIX_(rootName),
                ODL_EXIT_VALUE_, val, val);
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_X1_, ODL_FUNC_PREFIX_(rootName), ODL_EXIT_VALUE_, val, val);
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_X1_, ODL_FUNC_PREFIX_(rootName),
               ODL_EXIT_VALUE_, val, val);
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_X1_ "\n", ODL_FUNC_PREFIX_(rootName),
                ODL_EXIT_VALUE_, val, val);
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogExitX_

EXTERN_C void
ODLogExitXL_(const char *  fileName,
             const char *  funcName,
             const int     lineNumber,
             const int64_t val)
{
    const char * rootName = odFileNameRoot_(fileName);

    odDecreaseIndent_();
    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_XL1_ "\n", ODL_FUNC_PREFIX_(rootName),
                ODL_EXIT_VALUE_, val, val);
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_XL1_, ODL_FUNC_PREFIX_(rootName), ODL_EXIT_VALUE_, val, val);
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_XL1_, ODL_FUNC_PREFIX_(rootName),
               ODL_EXIT_VALUE_, val, val);
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_XL1_ "\n", ODL_FUNC_PREFIX_(rootName),
                ODL_EXIT_VALUE_, val, val);
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogExitXL_

#  if (! MAC_OR_LINUX_)
#   pragma warning(push)
#   pragma warning(disable: 4100)
#  endif // ! MAC_OR_LINUX_
EXTERN_C void
ODLogInit_(const char * prefix,
           const int    options,
           const char * fileName,
           const char * funcName,
           const int    lineNumber)
{
#  if (defined(__OBJC__) || (! MAC_OR_LINUX_))
#   if MAC_OR_LINUX_
#    pragma unused(prefix)
#   endif // MAC_OR_LINUX_
#  endif // defined(__OBJC__) || (! MAC_OR_LINUX_)
    bool         odWriteToFile = (options & kODLoggingOptionWriteToFile);
    const char * rootName = odFileNameRoot_(fileName);
    const char * suffixString;
    char *       stars;
    size_t       starsLength = strlen(rootName) + strlen(funcName) +
                                (sizeof(ODL_INIT_FORMAT_) - 1) - 8; // 4 %s

#  if MAC_OR_LINUX_
    lOdEnableThreadSupport_ = (options & kODLoggingOptionEnableThreadSupport);
    lOdIncludeProcessID_ = (options & kODLoggingOptionIncludeProcessID);
    lOdIncludeThreadID_ = (options & kODLoggingOptionIncludeThreadID);
#  endif // MAC_OR_LINUX_
    ODL_CREATE_PREFIX_();
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
#  if (MAC_OR_LINUX_ && (! defined(__OBJC__)))
    if (options & kODLoggingOptionWriteToStderr)
    {
        openlog(prefix, LOG_PID | LOG_CONS | LOG_PERROR, ODL_TARGET_);
    }
    else
    {
        openlog(prefix, LOG_PID | LOG_CONS, ODL_TARGET_);
    }
#  endif // MAC_OR_LINUX_ && (! defined(__OBJC__))
    if (odWriteToFile)
    {
        char pidString[100];

#  if MAC_OR_LINUX_
        snprintf(pidString, sizeof(pidString), "/var/log/pid-%lX.log", (long unsigned) getpid());
        lOdLogFile_ = fopen(pidString, "w");
#  else // ! MAC_OR_LINUX_
        sprintf_s(pidString, sizeof(pidString), "od_temp.log");
        if (! fopen_s(&lOdLogFile_, pidString, "w"))
        {
            lOdLogFile_ = NULL;
        }
#  endif // ! MAC_OR_LINUX_
        if (lOdLogFile_)
        {
            odWriteTime_(lOdLogFile_);
            fprintf(lOdLogFile_, "%s\n", stars);
        }
        else
        {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
            NSLog(@ "problem opening %s => %d", pidString, errno);
#   else // ! defined(__OBJC__)
            syslog(ODL_LEVEL_, "problem opening %s => %d", pidString, errno);
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
            fprintf(ODL_STREAM_, "problem opening %s => %d\n", pidString, errno);
#  endif // ! MAC_OR_LINUX_
        }
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ "%s", stars);
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, "%s", stars);
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        fprintf(ODL_STREAM_, "%s\n", stars);
#  endif // ! MAC_OR_LINUX_
    }
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_INIT_FORMAT_ "\n", prefixString_, suffixString, funcName, rootName,
                lineNumber);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_INIT_FORMAT_, prefixString_, suffixString, funcName, rootName, lineNumber);
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_INIT_FORMAT_, prefixString_, suffixString, funcName, rootName,
               lineNumber);
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_INIT_FORMAT_ "\n", prefixString_, suffixString, funcName,
                rootName, lineNumber);
#  endif // ! MAC_OR_LINUX_
    }
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, "%s\n", stars);
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ "%s", stars);
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, "%s", stars);
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, "%s\n", stars);
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    free(stars);
    ODL_FREE_PREFIX_();
    odSetIndent_(ODL_INIT_VALUE_);
} // ODLogInit_
#  if (! MAC_OR_LINUX_)
#   pragma warning(pop)
#  endif // ! MAC_OR_LINUX_

EXTERN_C void
ODLogIP_(const char *  fileName,
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

    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    address.asInt32 = val1;
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_IP_ "\n", ODL_FUNC_PREFIX_(rootName), text1,
                address.asBytes[0], address.asBytes[1], address.asBytes[2], address.asBytes[3],
                val2);
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_IP_, ODL_FUNC_PREFIX_(rootName), text1, address.asBytes[0],
              address.asBytes[1], address.asBytes[2], address.asBytes[3], val2);
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_IP_, ODL_FUNC_PREFIX_(rootName), text1,
               address.asBytes[0], address.asBytes[1], address.asBytes[2], address.asBytes[3],
               val2);
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_IP_ "\n", ODL_FUNC_PREFIX_(rootName),
                text1, address.asBytes[0], address.asBytes[1], address.asBytes[2],
                address.asBytes[3], val2);
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogIP_

EXTERN_C void
ODLogL1_(const char *  fileName,
         const char *  funcName,
         const int     lineNumber,
         const char *  text1,
         const int32_t val1)
{
    const char * rootName = odFileNameRoot_(fileName);

    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_L1_ "\n", ODL_FUNC_PREFIX_(rootName), text1,
                val1);
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_L1_, ODL_FUNC_PREFIX_(rootName), text1, val1);
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_L1_, ODL_FUNC_PREFIX_(rootName), text1,
               val1);
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_L1_ "\n", ODL_FUNC_PREFIX_(rootName),
                text1, val1);
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogL1_

EXTERN_C void
ODLogL2_(const char *  fileName,
         const char *  funcName,
         const int     lineNumber,
         const char *  text1,
         const int32_t val1,
         const char *  text2,
         const int32_t val2)
{
    const char * rootName = odFileNameRoot_(fileName);

    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_L2_ "\n", ODL_FUNC_PREFIX_(rootName), text1,
                val1, text2, val2);
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_L2_, ODL_FUNC_PREFIX_(rootName), text1, val1, text2, val2);
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_L2_, ODL_FUNC_PREFIX_(rootName), text1,
               val1, text2, val2);
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_L2_ "\n", ODL_FUNC_PREFIX_(rootName),
                text1, val1, text2, val2);
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogL2_

EXTERN_C void
ODLogL3_(const char *  fileName,
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

    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_L2_ "\n", ODL_FUNC_PREFIX_(rootName), text1,
                val1, text2, val2);
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_L1_ "\n", ODL_FUNC_PREFIX_(rootName), text3,
                val3);
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_L2_, ODL_FUNC_PREFIX_(rootName), text1, val1, text2, val2);
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_L1_, ODL_FUNC_PREFIX_(rootName), text3, val3);
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_L2_, ODL_FUNC_PREFIX_(rootName), text1,
               val1, text2, val2);
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_L1_, ODL_FUNC_PREFIX_(rootName), text3,
               val3);
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_L2_ "\n", ODL_FUNC_PREFIX_(rootName),
                text1, val1, text2, val2);
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_L1_ "\n", ODL_FUNC_PREFIX_(rootName),
                text3, val3);
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogL3_

EXTERN_C void
ODLogL4_(const char *  fileName,
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

    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_L2_ "\n", ODL_FUNC_PREFIX_(rootName), text1,
                val1, text2, val2);
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_L2_ "\n", ODL_FUNC_PREFIX_(rootName), text3,
                val3, text4, val4);
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_L2_, ODL_FUNC_PREFIX_(rootName), text1, val1, text2, val2);
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_L2_, ODL_FUNC_PREFIX_(rootName), text3, val3, text4, val4);
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_L2_, ODL_FUNC_PREFIX_(rootName), text1,
               val1, text2, val2);
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_L2_, ODL_FUNC_PREFIX_(rootName), text3,
               val3, text4, val4);
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_L2_ "\n", ODL_FUNC_PREFIX_(rootName),
                text1, val1, text2, val2);
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_L2_ "\n", ODL_FUNC_PREFIX_(rootName),
                text3, val3, text4, val4);
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogL4_

EXTERN_C void
ODLogLL1_(const char *  fileName,
          const char *  funcName,
          const int     lineNumber,
          const char *  text1,
          const int64_t val1)
{
    const char * rootName = odFileNameRoot_(fileName);

    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_LL1_ "\n", ODL_FUNC_PREFIX_(rootName), text1,
                val1);
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_LL1_, ODL_FUNC_PREFIX_(rootName), text1, val1);
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_LL1_, ODL_FUNC_PREFIX_(rootName), text1,
               val1);
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_LL1_ "\n", ODL_FUNC_PREFIX_(rootName),
                text1, val1);
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogLL1_

EXTERN_C void
ODLogLL2_(const char *  fileName,
          const char *  funcName,
          const int     lineNumber,
          const char *  text1,
          const int64_t val1,
          const char *  text2,
          const int64_t val2)
{
    const char * rootName = odFileNameRoot_(fileName);

    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_LL2_ "\n", ODL_FUNC_PREFIX_(rootName), text1,
                val1, text2, val2);
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_LL2_, ODL_FUNC_PREFIX_(rootName), text1, val1, text2, val2);
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_LL2_, ODL_FUNC_PREFIX_(rootName), text1,
               val1, text2, val2);
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_LL2_ "\n", ODL_FUNC_PREFIX_(rootName),
                text1, val1, text2, val2);
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogLL2_

EXTERN_C void
ODLogLL3_(const char *  fileName,
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

    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_LL2_ "\n", ODL_FUNC_PREFIX_(rootName), text1,
                val1, text2, val2);
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_LL1_ "\n", ODL_FUNC_PREFIX_(rootName), text3,
                val3);
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_LL2_, ODL_FUNC_PREFIX_(rootName), text1, val1, text2, val2);
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_LL1_, ODL_FUNC_PREFIX_(rootName), text3, val3);
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_LL2_, ODL_FUNC_PREFIX_(rootName), text1,
               val1, text2, val2);
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_LL1_, ODL_FUNC_PREFIX_(rootName), text3,
               val3);
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_LL2_ "\n", ODL_FUNC_PREFIX_(rootName),
                text1, val1, text2, val2);
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_LL1_ "\n", ODL_FUNC_PREFIX_(rootName),
                text3, val3);
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogLL3_

EXTERN_C void
ODLogLL4_(const char *  fileName,
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

    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_LL2_ "\n", ODL_FUNC_PREFIX_(rootName), text1,
                val1, text2, val2);
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_LL2_ "\n", ODL_FUNC_PREFIX_(rootName), text3,
                val3, text4, val4);
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_LL2_, ODL_FUNC_PREFIX_(rootName), text1, val1, text2, val2);
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_LL2_, ODL_FUNC_PREFIX_(rootName), text3, val3, text4, val4);
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_LL2_, ODL_FUNC_PREFIX_(rootName), text1,
               val1, text2, val2);
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_LL2_, ODL_FUNC_PREFIX_(rootName), text3,
               val3, text4, val4);
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_LL2_ "\n", ODL_FUNC_PREFIX_(rootName),
                text1, val1, text2, val2);
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_LL2_ "\n", ODL_FUNC_PREFIX_(rootName),
                text3, val3, text4, val4);
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogLL4_

EXTERN_C void
ODLogLS_(const char * fileName,
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
    char         lineBuffer[ODL_MAX_CHARS_IN_LINE_ + 1];
    char *       blankCaption = (char *) malloc(captionLength + 1);

    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
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
            if (ODL_MAX_CHARS_IN_LINE_ <= jj)
            {
                if (lOdLogFile_)
                {
                    odWriteTime_(lOdLogFile_);
                    fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_LS "\n",
                            ODL_FUNC_PREFIX_(rootName), heading, lineBuffer);
                }
                else
                {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
                    NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_LS, ODL_FUNC_PREFIX_(rootName), heading,
                          lineBuffer);
#   else // ! defined(__OBJC__)
                    syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_LS,
                           ODL_FUNC_PREFIX_(rootName), heading, lineBuffer);
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
                    odWriteTime_(ODL_STREAM_);
                    fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_LS "\n",
                            ODL_FUNC_PREFIX_(rootName), heading, lineBuffer);
#  endif // ! MAC_OR_LINUX_
                }
                heading = blankCaption;
                memset(lineBuffer, ' ', sizeof(lineBuffer) - 1);
                jj = 0;
            }
            bb = (('\n' == bb) ? 'n' : (('\r' == bb) ? 'r' : 't'));
        }
        lineBuffer[jj++] = (isprint(bb) ? bb : '.');
        if (ODL_MAX_CHARS_IN_LINE_ <= jj)
        {
            if (lOdLogFile_)
            {
                odWriteTime_(lOdLogFile_);
                fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_LS "\n", ODL_FUNC_PREFIX_(rootName),
                        heading, lineBuffer);
            }
            else
            {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
                NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_LS, ODL_FUNC_PREFIX_(rootName), heading,
                      lineBuffer);
#   else // ! defined(__OBJC__)
                syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_LS, ODL_FUNC_PREFIX_(rootName),
                       heading, lineBuffer);
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
                odWriteTime_(ODL_STREAM_);
                fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_LS "\n",
                        ODL_FUNC_PREFIX_(rootName), heading, lineBuffer);
#  endif // ! MAC_OR_LINUX_
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
            fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_LS "\n", ODL_FUNC_PREFIX_(rootName),
                    heading, lineBuffer);
        }
        else
        {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
            NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_LS, ODL_FUNC_PREFIX_(rootName), heading, lineBuffer);
#   else // ! defined(__OBJC__)
            syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_LS, ODL_FUNC_PREFIX_(rootName),
                   heading, lineBuffer);
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
            odWriteTime_(ODL_STREAM_);
            fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_LS "\n", ODL_FUNC_PREFIX_(rootName),
                    heading, lineBuffer);
#  endif // ! MAC_OR_LINUX_
        }
    }
    if (lOdLogFile_)
    {
        fflush(lOdLogFile_);
    }
#  if (! MAC_OR_LINUX_)
    else
    {
        fflush(ODL_STREAM_);
    }
#  endif // ! MAC_OR_LINUX_
    free(blankCaption);
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogLS_

#  if defined(__OBJC__)
EXTERN_C void
ODLogO1_(const char * fileName,
         const char * funcName,
         const int    lineNumber,
         const char * text1,
         const id     obj1)
{
    const char * obj1String = odNullOrDescription(obj1);
    const char * rootName = odFileNameRoot_(fileName);

    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_O1_ "\n", ODL_FUNC_PREFIX_(rootName), text1,
                obj1String);
        fflush(lOdLogFile_);
    }
    else
    {
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_O1_, ODL_FUNC_PREFIX_(rootName), text1, obj1String);
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogO1_
#  endif // defined(__OBJC__)

#  if defined(__OBJC__)
EXTERN_C void
ODLogO2_(const char * fileName,
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

    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_O2_ "\n", ODL_FUNC_PREFIX_(rootName), text1,
                obj1String, text2, obj2String);
        fflush(lOdLogFile_);
    }
    else
    {
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_O2_, ODL_FUNC_PREFIX_(rootName), text1, obj1String, text2,
              obj2String);
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogO2_
#  endif // defined(__OBJC__)

#  if defined(__OBJC__)
EXTERN_C void
ODLogO3_(const char * fileName,
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

    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_O2_ "\n", ODL_FUNC_PREFIX_(rootName), text1,
                obj1String, text2, obj2String);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_O1_ "\n", ODL_FUNC_PREFIX_(rootName), text3,
                obj3String);
        fflush(lOdLogFile_);
    }
    else
    {
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_O2_, ODL_FUNC_PREFIX_(rootName), text1, obj1String, text2,
              obj2String);
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_O1_, ODL_FUNC_PREFIX_(rootName), text3, obj3String);
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogO3_
#  endif // defined(__OBJC__)

#  if defined(__OBJC__)
EXTERN_C void
ODLogO4_(const char * fileName,
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

    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_O2_ "\n", ODL_FUNC_PREFIX_(rootName), text1,
                obj1String, text2, obj2String);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_O2_ "\n", ODL_FUNC_PREFIX_(rootName), text3,
                obj3String, text4, obj4String);
        fflush(lOdLogFile_);
    }
    else
    {
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_O2_, ODL_FUNC_PREFIX_(rootName), text1, obj1String, text2,
              obj2String);
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_O2_, ODL_FUNC_PREFIX_(rootName), text3, obj3String, text4,
              obj4String);
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogO4_
#  endif // defined(__OBJC__)

EXTERN_C void
ODLogObjEnter_(const char * fileName,
               const char * funcName,
               const int    lineNumber,
               const void * objPtr)
{
    const char * rootName = odFileNameRoot_(fileName);

    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_METHOD_FORMAT_ ODL_FORMAT_ENTER_ "\n", ODL_METHOD_PREFIX_(rootName));
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_METHOD_FORMAT_ ODL_FORMAT_ENTER_, ODL_METHOD_PREFIX_(rootName));
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_METHOD_FORMAT_ ODL_FORMAT_ENTER_, ODL_METHOD_PREFIX_(rootName));
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_METHOD_FORMAT_ ODL_FORMAT_ENTER_ "\n",
                ODL_METHOD_PREFIX_(rootName));
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
    odIncreaseIndent_();
} // ODLogObjEnter_

EXTERN_C void
ODLogObjExit_(const char * fileName,
              const char * funcName,
              const int    lineNumber,
              const void * objPtr)
{
    const char * rootName = odFileNameRoot_(fileName);

    odDecreaseIndent_();
    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_METHOD_FORMAT_ ODL_FORMAT_EXIT_ "\n", ODL_METHOD_PREFIX_(rootName));
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_METHOD_FORMAT_ ODL_FORMAT_EXIT_, ODL_METHOD_PREFIX_(rootName));
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_METHOD_FORMAT_ ODL_FORMAT_EXIT_, ODL_METHOD_PREFIX_(rootName));
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_METHOD_FORMAT_ ODL_FORMAT_EXIT_ "\n",
                ODL_METHOD_PREFIX_(rootName));
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogObjExit_

EXTERN_C void
ODLogObjExitB_(const char * fileName,
               const char * funcName,
               const int    lineNumber,
               const void * objPtr,
               const bool   val)
{
    const char * rootName = odFileNameRoot_(fileName);

    odDecreaseIndent_();
    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_METHOD_FORMAT_ ODL_FORMAT_B1_ "\n", ODL_METHOD_PREFIX_(rootName),
                ODL_EXIT_VALUE_, odBoolToString_(val));
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_METHOD_FORMAT_ ODL_FORMAT_B1_, ODL_METHOD_PREFIX_(rootName), ODL_EXIT_VALUE_,
              odBoolToString_(val));
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_METHOD_FORMAT_ ODL_FORMAT_B1_, ODL_METHOD_PREFIX_(rootName),
               ODL_EXIT_VALUE_, odBoolToString_(val));
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_METHOD_FORMAT_ ODL_FORMAT_B1_ "\n", ODL_METHOD_PREFIX_(rootName),
                ODL_EXIT_VALUE_, odBoolToString_(val));
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogObjExitB_

EXTERN_C void
ODLogObjExitC_(const char * fileName,
               const char * funcName,
               const int    lineNumber,
               const void * objPtr,
               const char   val)
{
    const char * rootName = odFileNameRoot_(fileName);

    odDecreaseIndent_();
    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_METHOD_FORMAT_ ODL_FORMAT_C1_ "\n", ODL_METHOD_PREFIX_(rootName),
                ODL_EXIT_VALUE_, val, val);
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_METHOD_FORMAT_ ODL_FORMAT_C1_, ODL_METHOD_PREFIX_(rootName), ODL_EXIT_VALUE_, val,
              val);
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_METHOD_FORMAT_ ODL_FORMAT_C1_, ODL_METHOD_PREFIX_(rootName),
               ODL_EXIT_VALUE_, val, val);
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_METHOD_FORMAT_ ODL_FORMAT_C1_ "\n", ODL_METHOD_PREFIX_(rootName),
                ODL_EXIT_VALUE_, val, val);
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogObjExitC_

EXTERN_C void
ODLogObjExitD_(const char * fileName,
               const char * funcName,
               const int    lineNumber,
               const void * objPtr,
               const double val)
{
    const char * rootName = odFileNameRoot_(fileName);

    odDecreaseIndent_();
    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_METHOD_FORMAT_ ODL_FORMAT_D1_ "\n", ODL_METHOD_PREFIX_(rootName),
                ODL_EXIT_VALUE_, val);
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_METHOD_FORMAT_ ODL_FORMAT_D1_, ODL_METHOD_PREFIX_(rootName), ODL_EXIT_VALUE_, val);
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_METHOD_FORMAT_ ODL_FORMAT_D1_, ODL_METHOD_PREFIX_(rootName),
               ODL_EXIT_VALUE_, val);
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_METHOD_FORMAT_ ODL_FORMAT_D1_ "\n", ODL_METHOD_PREFIX_(rootName),
                ODL_EXIT_VALUE_, val);
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogObjExitD_

EXTERN_C void
ODLogObjExitExit_(const char * fileName,
                  const char * funcName,
                  const int    lineNumber,
                  const void * objPtr,
                  const long   val)
{
    const char * rootName = odFileNameRoot_(fileName);

    odDecreaseIndent_();
    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_METHOD_FORMAT_ ODL_FORMAT_EXIT_CALL_ "\n",
                ODL_METHOD_PREFIX_(rootName), val, val);
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_METHOD_FORMAT_ ODL_FORMAT_EXIT_CALL_, ODL_METHOD_PREFIX_(rootName), val, val);
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_METHOD_FORMAT_ ODL_FORMAT_EXIT_CALL_,
               ODL_METHOD_PREFIX_(rootName), val, val);
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_METHOD_FORMAT_ ODL_FORMAT_EXIT_CALL_ "\n",
                ODL_METHOD_PREFIX_(rootName), val, val);
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogObjExitExit_

EXTERN_C void
ODLogObjExitL_(const char *  fileName,
               const char *  funcName,
               const int     lineNumber,
               const void *  objPtr,
               const int32_t val)
{
    const char * rootName = odFileNameRoot_(fileName);

    odDecreaseIndent_();
    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_METHOD_FORMAT_ ODL_FORMAT_L1_ "\n", ODL_METHOD_PREFIX_(rootName),
                ODL_EXIT_VALUE_, val);
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_METHOD_FORMAT_ ODL_FORMAT_L1_, ODL_METHOD_PREFIX_(rootName), ODL_EXIT_VALUE_, val);
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_METHOD_FORMAT_ ODL_FORMAT_L1_, ODL_METHOD_PREFIX_(rootName),
               ODL_EXIT_VALUE_, val);
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_METHOD_FORMAT_ ODL_FORMAT_L1_ "\n", ODL_METHOD_PREFIX_(rootName),
                ODL_EXIT_VALUE_, val);
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogObjExitL_

EXTERN_C void
ODLogObjExitLL_(const char *  fileName,
                const char *  funcName,
                const int     lineNumber,
                const void *  objPtr,
                const int64_t val)
{
    const char * rootName = odFileNameRoot_(fileName);

    odDecreaseIndent_();
    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_METHOD_FORMAT_ ODL_FORMAT_LL1_ "\n", ODL_METHOD_PREFIX_(rootName),
                ODL_EXIT_VALUE_, val);
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_METHOD_FORMAT_ ODL_FORMAT_LL1_, ODL_METHOD_PREFIX_(rootName), ODL_EXIT_VALUE_, val);
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_METHOD_FORMAT_ ODL_FORMAT_LL1_, ODL_METHOD_PREFIX_(rootName),
               ODL_EXIT_VALUE_, val);
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_METHOD_FORMAT_ ODL_FORMAT_LL1_ "\n", ODL_METHOD_PREFIX_(rootName),
                ODL_EXIT_VALUE_, val);
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogObjExitLL_

#  if defined(__OBJC__)
EXTERN_C void
ODLogObjExitO_(const char * fileName,
               const char * funcName,
               const int    lineNumber,
               const void * objPtr,
               const id     val)
{
    const char * rootName = odFileNameRoot_(fileName);
    const char * valString = odNullOrDescription(val);

    odDecreaseIndent_();
    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_METHOD_FORMAT_ ODL_FORMAT_O1_ "\n", ODL_METHOD_PREFIX_(rootName),
                ODL_EXIT_VALUE_, valString);
        fflush(lOdLogFile_);
    }
    else
    {
        NSLog(@ODL_METHOD_FORMAT_ ODL_FORMAT_O1_, ODL_METHOD_PREFIX_(rootName), ODL_EXIT_VALUE_,
              valString);
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogObjExitO_
#  endif // defined(__OBJC__)

EXTERN_C void
ODLogObjExitP_(const char * fileName,
               const char * funcName,
               const int    lineNumber,
               const void * objPtr,
               const void * val)
{
    const char * rootName = odFileNameRoot_(fileName);

    odDecreaseIndent_();
    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_METHOD_FORMAT_ ODL_FORMAT_P1_ "\n", ODL_METHOD_PREFIX_(rootName),
                ODL_EXIT_VALUE_, val);
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_METHOD_FORMAT_ ODL_FORMAT_P1_, ODL_METHOD_PREFIX_(rootName), ODL_EXIT_VALUE_, val);
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_METHOD_FORMAT_ ODL_FORMAT_P1_, ODL_METHOD_PREFIX_(rootName),
               ODL_EXIT_VALUE_, val);
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_METHOD_FORMAT_ ODL_FORMAT_P1_ "\n", ODL_METHOD_PREFIX_(rootName),
                ODL_EXIT_VALUE_, val);
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogObjExitP_

#  if defined(__APPLE__)
EXTERN_C void
ODLogObjExitRect_(const char * fileName,
                  const char * funcName,
                  const int    lineNumber,
                  const void * objPtr,
                  const CGRect val)
{
    const char * rootName = odFileNameRoot_(fileName);

    odDecreaseIndent_();
    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_METHOD_FORMAT_ ODL_FORMAT_RECT_ "\n", ODL_METHOD_PREFIX_(rootName),
                ODL_EXIT_VALUE_, val.origin.x, val.origin.y, val.size.height, val.size.width);
        fflush(lOdLogFile_);
    }
    else
    {
#   if defined(__OBJC__)
        NSLog(@ODL_METHOD_FORMAT_ ODL_FORMAT_RECT_, ODL_METHOD_PREFIX_(rootName), ODL_EXIT_VALUE_,
              val.origin.x, val.origin.y, val.size.height, val.size.width);
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_METHOD_FORMAT_ ODL_FORMAT_RECT_, ODL_METHOD_PREFIX_(rootName),
               ODL_EXIT_VALUE_, val.origin.x, val.origin.y, val.size.height, val.size.width);
#   endif // ! defined(__OBJC__)
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogObjExitRect_
#  endif // defined(__APPLE__)

EXTERN_C void
ODLogObjExitS_(const char * fileName,
               const char * funcName,
               const int    lineNumber,
               const void * objPtr,
               const char * val)
{
    const char * rootName = odFileNameRoot_(fileName);

    odDecreaseIndent_();
    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_METHOD_FORMAT_ ODL_FORMAT_S1_ "\n", ODL_METHOD_PREFIX_(rootName),
                ODL_EXIT_VALUE_, odNullOrString(val));
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_METHOD_FORMAT_ ODL_FORMAT_S1_, ODL_METHOD_PREFIX_(rootName), ODL_EXIT_VALUE_,
              odNullOrString(val));
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_METHOD_FORMAT_ ODL_FORMAT_S1_, ODL_METHOD_PREFIX_(rootName),
               ODL_EXIT_VALUE_, odNullOrString(val));
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_METHOD_FORMAT_ ODL_FORMAT_S1_ "\n", ODL_METHOD_PREFIX_(rootName),
                ODL_EXIT_VALUE_, odNullOrString(val));
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogObjExitS_

#  if defined(__APPLE__)
EXTERN_C void
ODLogObjExitSize_(const char * fileName,
                  const char * funcName,
                  const int    lineNumber,
                  const void * objPtr,
                  const CGSize val)
{
    const char * rootName = odFileNameRoot_(fileName);

    odDecreaseIndent_();
    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_METHOD_FORMAT_ ODL_FORMAT_SIZE_ "\n", ODL_METHOD_PREFIX_(rootName),
                ODL_EXIT_VALUE_, val.height, val.width);
        fflush(lOdLogFile_);
    }
    else
    {
#   if defined(__OBJC__)
        NSLog(@ODL_METHOD_FORMAT_ ODL_FORMAT_SIZE_, ODL_METHOD_PREFIX_(rootName), ODL_EXIT_VALUE_,
              val.height, val.width);
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_METHOD_FORMAT_ ODL_FORMAT_SIZE_, ODL_METHOD_PREFIX_(rootName),
               ODL_EXIT_VALUE_, val.height, val.width);
#   endif // ! defined(__OBJC__)
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogObjExitSize_
#  endif // defined(__APPLE__)

EXTERN_C void
ODLogObjExitThrowL_(const char *  fileName,
                    const char *  funcName,
                    const int     lineNumber,
                    const void *  objPtr,
                    const int32_t val)
{
    const char * rootName = odFileNameRoot_(fileName);

    odDecreaseIndent_();
    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_METHOD_FORMAT_ ODL_FORMAT_EXIT_T_L_ "\n",
                ODL_METHOD_PREFIX_(rootName), val);
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_METHOD_FORMAT_ ODL_FORMAT_EXIT_T_L_, ODL_METHOD_PREFIX_(rootName), val);
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_METHOD_FORMAT_ ODL_FORMAT_EXIT_T_L_,
               ODL_METHOD_PREFIX_(rootName), val);
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_METHOD_FORMAT_ ODL_FORMAT_EXIT_T_L_ "\n",
                ODL_METHOD_PREFIX_(rootName), val);
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogObjExitThrowL_

EXTERN_C void
ODLogObjExitThrowS_(const char * fileName,
                    const char * funcName,
                    const int    lineNumber,
                    const void * objPtr,
                    const char * val)
{
    const char * rootName = odFileNameRoot_(fileName);

    odDecreaseIndent_();
    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_METHOD_FORMAT_ ODL_FORMAT_EXIT_T_S_ "\n",
                ODL_METHOD_PREFIX_(rootName), odNullOrString(val));
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_METHOD_FORMAT_ ODL_FORMAT_EXIT_T_S_, ODL_METHOD_PREFIX_(rootName),
              odNullOrString(val));
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_METHOD_FORMAT_ ODL_FORMAT_EXIT_T_S_,
               ODL_METHOD_PREFIX_(rootName), odNullOrString(val));
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_METHOD_FORMAT_ ODL_FORMAT_EXIT_T_S_ "\n",
                ODL_METHOD_PREFIX_(rootName), odNullOrString(val));
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogObjExitThrowS_

EXTERN_C void
ODLogObjExitThrowX_(const char *  fileName,
                    const char *  funcName,
                    const int     lineNumber,
                    const void *  objPtr,
                    const int32_t val)
{
    const char * rootName = odFileNameRoot_(fileName);

    odDecreaseIndent_();
    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_METHOD_FORMAT_ ODL_FORMAT_EXIT_T_X_ "\n",
                ODL_METHOD_PREFIX_(rootName), val, val);
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_METHOD_FORMAT_ ODL_FORMAT_EXIT_T_X_, ODL_METHOD_PREFIX_(rootName), val, val);
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_METHOD_FORMAT_ ODL_FORMAT_EXIT_T_X_,
               ODL_METHOD_PREFIX_(rootName), val, val);
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_METHOD_FORMAT_ ODL_FORMAT_EXIT_T_X_ "\n",
                ODL_METHOD_PREFIX_(rootName), val, val);
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogObjExitThrowX_

EXTERN_C void
ODLogObjExitX_(const char *  fileName,
               const char *  funcName,
               const int     lineNumber,
               const void *  objPtr,
               const int32_t val)
{
    const char * rootName = odFileNameRoot_(fileName);

    odDecreaseIndent_();
    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_METHOD_FORMAT_ ODL_FORMAT_X1_ "\n", ODL_METHOD_PREFIX_(rootName),
                ODL_EXIT_VALUE_, val, val);
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_METHOD_FORMAT_ ODL_FORMAT_X1_, ODL_METHOD_PREFIX_(rootName), ODL_EXIT_VALUE_, val,
              val);
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_METHOD_FORMAT_ ODL_FORMAT_X1_, ODL_METHOD_PREFIX_(rootName),
               ODL_EXIT_VALUE_, val, val);
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_METHOD_FORMAT_ ODL_FORMAT_X1_ "\n", ODL_METHOD_PREFIX_(rootName),
                ODL_EXIT_VALUE_, val, val);
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogObjExitX_

EXTERN_C void
ODLogObjExitXL_(const char *  fileName,
                const char *  funcName,
                const int     lineNumber,
                const void *  objPtr,
                const int64_t val)
{
    const char * rootName = odFileNameRoot_(fileName);

    odDecreaseIndent_();
    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_METHOD_FORMAT_ ODL_FORMAT_XL1_ "\n", ODL_METHOD_PREFIX_(rootName),
                ODL_EXIT_VALUE_, val, val);
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_METHOD_FORMAT_ ODL_FORMAT_XL1_, ODL_METHOD_PREFIX_(rootName), ODL_EXIT_VALUE_, val,
              val);
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_METHOD_FORMAT_ ODL_FORMAT_XL1_, ODL_METHOD_PREFIX_(rootName),
               ODL_EXIT_VALUE_, val, val);
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_METHOD_FORMAT_ ODL_FORMAT_XL1_ "\n", ODL_METHOD_PREFIX_(rootName),
                ODL_EXIT_VALUE_, val, val);
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogObjExitXL_

EXTERN_C void
ODLogP1_(const char * fileName,
         const char * funcName,
         const int    lineNumber,
         const char * text1,
         const void * ptr1)
{
    const char * rootName = odFileNameRoot_(fileName);

    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_P1_ "\n", ODL_FUNC_PREFIX_(rootName), text1,
                ptr1);
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_P1_, ODL_FUNC_PREFIX_(rootName), text1, ptr1);
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_P1_, ODL_FUNC_PREFIX_(rootName), text1,
               ptr1);
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_P1_ "\n", ODL_FUNC_PREFIX_(rootName),
                text1, ptr1);
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogP1_

EXTERN_C void
ODLogP2_(const char * fileName,
         const char * funcName,
         const int    lineNumber,
         const char * text1,
         const void * ptr1,
         const char * text2,
         const void * ptr2)
{
    const char * rootName = odFileNameRoot_(fileName);

    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_P2_ "\n", ODL_FUNC_PREFIX_(rootName), text1,
                ptr1, text2, ptr2);
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_P2_, ODL_FUNC_PREFIX_(rootName), text1, ptr1, text2, ptr2);
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_P2_, ODL_FUNC_PREFIX_(rootName), text1,
               ptr1, text2, ptr2);
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_P2_ "\n", ODL_FUNC_PREFIX_(rootName),
                text1, ptr1, text2, ptr2);
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogP2_

EXTERN_C void
ODLogP3_(const char * fileName,
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

    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_P2_ "\n", ODL_FUNC_PREFIX_(rootName), text1,
                ptr1, text2, ptr2);
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_P1_ "\n", ODL_FUNC_PREFIX_(rootName), text3,
                ptr3);
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_P2_, ODL_FUNC_PREFIX_(rootName), text1, ptr1, text2, ptr2);
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_P1_, ODL_FUNC_PREFIX_(rootName), text3, ptr3);
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_P2_, ODL_FUNC_PREFIX_(rootName), text1,
               ptr1, text2, ptr2);
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_P1_, ODL_FUNC_PREFIX_(rootName), text3,
               ptr3);
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_P2_ "\n", ODL_FUNC_PREFIX_(rootName),
                text1, ptr1, text2, ptr2);
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_P1_ "\n", ODL_FUNC_PREFIX_(rootName),
                text3, ptr3);
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogP3_

EXTERN_C void
ODLogP4_(const char * fileName,
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

    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_P2_ "\n", ODL_FUNC_PREFIX_(rootName), text1,
                ptr1, text2, ptr2);
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_P2_ "\n", ODL_FUNC_PREFIX_(rootName), text3,
                ptr3, text4, ptr4);
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_P2_, ODL_FUNC_PREFIX_(rootName), text1, ptr1, text2, ptr2);
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_P2_, ODL_FUNC_PREFIX_(rootName), text3, ptr3, text4, ptr4);
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_P2_, ODL_FUNC_PREFIX_(rootName), text1,
               ptr1, text2, ptr2);
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_P2_, ODL_FUNC_PREFIX_(rootName), text3,
               ptr3, text4, ptr4);
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_P2_ "\n", ODL_FUNC_PREFIX_(rootName),
                text1, ptr1, text2, ptr2);
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_P2_ "\n", ODL_FUNC_PREFIX_(rootName),
                text3, ptr3, text4, ptr4);
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogP4_

EXTERN_C void
ODLogPacket_(const char * fileName,
             const char * funcName,
             const int    lineNumber,
             const char * caption,
             const void * buffer,
             const int    size)
{
    const char * heading = caption;
    const char * rootName = odFileNameRoot_(fileName);
    char         lineBuffer[(ODL_MAX_BYTES_IN_LINE_ * 3) + 1];
    char         charBuffer[ODL_MAX_BYTES_IN_LINE_ + 1];
    static char  hexDigits[] = "0123456789ABCDEF";
    size_t       captionLength = strlen(caption);
    char *       blankCaption = (char *) malloc(captionLength + 1);
    const char * walker = reinterpret_cast<const char *>(buffer);

    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_PACKET_1_ "\n", ODL_FUNC_PREFIX_(rootName),
                buffer, size);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_PACKET_1_, ODL_FUNC_PREFIX_(rootName), buffer, size);
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_PACKET_1_, ODL_FUNC_PREFIX_(rootName),
               buffer, size);
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_PACKET_1_ "\n", ODL_FUNC_PREFIX_(rootName),
                buffer, size);
#  endif // ! MAC_OR_LINUX_
    }
    memset(blankCaption, ' ', captionLength);
    blankCaption[captionLength] = '\0';
    memset(lineBuffer, ' ', sizeof(lineBuffer) - 1);
    lineBuffer[sizeof(lineBuffer) - 1] = '\0';
    for (int left = size, ii = 0; left > 0;
         left -= ODL_MAX_BYTES_IN_LINE_, ii += ODL_MAX_BYTES_IN_LINE_)
    {
        int ww = ((left > ODL_MAX_BYTES_IN_LINE_) ? ODL_MAX_BYTES_IN_LINE_ : left);

        for (int jj = 0; jj < ww; ++jj)
        {
            char bb = walker[ii + jj];

            lineBuffer[jj * 3] = hexDigits[(bb >> 4) & 0x0F];
            lineBuffer[(jj * 3) + 1] = hexDigits[bb & 0x0F];
            charBuffer[jj] = (isprint(bb) ? bb : '.');
        }
        charBuffer[ww] = '\0';
        if (lOdLogFile_)
        {
            odWriteTime_(lOdLogFile_);
            fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_PACKET_2_ "\n",
                    ODL_FUNC_PREFIX_(rootName), heading, ii, lineBuffer, charBuffer);
        }
        else
        {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
            NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_PACKET_2_, ODL_FUNC_PREFIX_(rootName), heading, ii,
                  lineBuffer, charBuffer);
#   else // ! defined(__OBJC__)
            syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_PACKET_2_,
                   ODL_FUNC_PREFIX_(rootName), heading, ii, lineBuffer, charBuffer);
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
            odWriteTime_(ODL_STREAM_);
            fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_PACKET_2_ "\n",
                    ODL_FUNC_PREFIX_(rootName), heading, ii, lineBuffer, charBuffer);
#  endif // ! MAC_OR_LINUX_
        }
        memset(lineBuffer, ' ', sizeof(lineBuffer) - 1);
        heading = blankCaption;
    }
    if (lOdLogFile_)
    {
        fflush(lOdLogFile_);
    }
#  if (! MAC_OR_LINUX_)
    else
    {
        fflush(ODL_STREAM_);
    }
#  endif // ! MAC_OR_LINUX_
    free(blankCaption);
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogPacket_

#  if defined(__APPLE__)
EXTERN_C void
ODLogRect_(const char * fileName,
           const char * funcName,
           const int    lineNumber,
           const char * caption,
           const CGRect rect)
{
    const char * rootName = odFileNameRoot_(fileName);

    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_RECT_ "\n", ODL_FUNC_PREFIX_(rootName),
                caption, rect.origin.x, rect.origin.y, rect.size.height, rect.size.width);
        fflush(lOdLogFile_);
    }
    else
    {
#   if defined(__OBJC__)
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_RECT_, ODL_FUNC_PREFIX_(rootName), caption, rect.origin.x,
              rect.origin.y, rect.size.height, rect.size.width);
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_RECT_, ODL_FUNC_PREFIX_(rootName),
               caption, rect.origin.x, rect.origin.y, rect.size.height, rect.size.width);
#   endif // ! defined(__OBJC__)
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogRect_
#  endif // defined(__APPLE__)

EXTERN_C void
ODLogS1_(const char * fileName,
         const char * funcName,
         const int    lineNumber,
         const char * text1,
         const char * val1)
{
    const char * rootName = odFileNameRoot_(fileName);

    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_S1_ "\n", ODL_FUNC_PREFIX_(rootName), text1,
                odNullOrString(val1));
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_S1_, ODL_FUNC_PREFIX_(rootName), text1,
              odNullOrString(val1));
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_S1_, ODL_FUNC_PREFIX_(rootName), text1,
               odNullOrString(val1));
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_S1_ "\n", ODL_FUNC_PREFIX_(rootName),
                text1, odNullOrString(val1));
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogS1_

EXTERN_C void
ODLogS2_(const char * fileName,
         const char * funcName,
         const int    lineNumber,
         const char * text1,
         const char * val1,
         const char * text2,
         const char * val2)
{
    const char * rootName = odFileNameRoot_(fileName);

    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_S2_ "\n", ODL_FUNC_PREFIX_(rootName), text1,
                odNullOrString(val1), text2, odNullOrString(val2));
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_S2_, ODL_FUNC_PREFIX_(rootName), text1,
              odNullOrString(val1), text2, odNullOrString(val2));
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_S2_, ODL_FUNC_PREFIX_(rootName), text1,
               odNullOrString(val1), text2, odNullOrString(val2));
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_S2_ "\n", ODL_FUNC_PREFIX_(rootName),
                text1, odNullOrString(val1), text2, odNullOrString(val2));
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogS2_

EXTERN_C void
ODLogS3_(const char * fileName,
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

    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_S2_ "\n", ODL_FUNC_PREFIX_(rootName), text1,
                odNullOrString(val1), text2, odNullOrString(val2));
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_S1_ "\n", ODL_FUNC_PREFIX_(rootName), text3,
                odNullOrString(val3));
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_S2_, ODL_FUNC_PREFIX_(rootName), text1,
              odNullOrString(val1), text2, odNullOrString(val2));
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_S1_, ODL_FUNC_PREFIX_(rootName), text3,
              odNullOrString(val3));
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_S2_, ODL_FUNC_PREFIX_(rootName), text1,
               odNullOrString(val1), text2, odNullOrString(val2));
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_S1_, ODL_FUNC_PREFIX_(rootName), text3,
               odNullOrString(val3));
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_S2_ "\n", ODL_FUNC_PREFIX_(rootName),
                text1, odNullOrString(val1), text2, odNullOrString(val2));
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_S1_ "\n", ODL_FUNC_PREFIX_(rootName),
                text3, odNullOrString(val3));
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogS3_

EXTERN_C void
ODLogS4_(const char * fileName,
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

    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_S2_ "\n", ODL_FUNC_PREFIX_(rootName), text1,
                odNullOrString(val1), text2, odNullOrString(val2));
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_S2_ "\n", ODL_FUNC_PREFIX_(rootName), text3,
                odNullOrString(val3), text4, odNullOrString(val4));
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_S2_, ODL_FUNC_PREFIX_(rootName), text1,
              odNullOrString(val1), text2, odNullOrString(val2));
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_S2_, ODL_FUNC_PREFIX_(rootName), text3,
              odNullOrString(val3), text4, odNullOrString(val4));
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_S2_, ODL_FUNC_PREFIX_(rootName), text1,
               odNullOrString(val1), text2, odNullOrString(val2));
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_S2_, ODL_FUNC_PREFIX_(rootName), text3,
               odNullOrString(val3), text4, odNullOrString(val4));
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_S2_ "\n", ODL_FUNC_PREFIX_(rootName),
                text1, odNullOrString(val1), text2, odNullOrString(val2));
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_S2_ "\n", ODL_FUNC_PREFIX_(rootName),
                text3, odNullOrString(val3), text4, odNullOrString(val4));
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogS4_

#  if defined(__APPLE__)
EXTERN_C void
ODLogSize_(const char * fileName,
           const char * funcName,
           const int    lineNumber,
           const char * caption,
           const CGSize size)
{
    const char * rootName = odFileNameRoot_(fileName);

    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_SIZE_ "\n", ODL_FUNC_PREFIX_(rootName),
                caption, size.height, size.width);
        fflush(lOdLogFile_);
    }
    else
    {
#   if defined(__OBJC__)
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_SIZE_, ODL_FUNC_PREFIX_(rootName), caption, size.height,
              size.width);
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_SIZE_, ODL_FUNC_PREFIX_(rootName),
               caption, size.height, size.width);
#   endif // ! defined(__OBJC__)
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogRect_
#  endif // defined(__APPLE__)

EXTERN_C void
ODLogSp_(const char * fileName,
         const char * funcName,
         const int    lineNumber,
         const char * text,
         const int    len,
         const char * val)
{
    const char * rootName = odFileNameRoot_(fileName);

    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_SP_ "\n", ODL_FUNC_PREFIX_(rootName), text,
                len, val ? val : "");
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_SP_, ODL_FUNC_PREFIX_(rootName), text, len, val ? val : "");
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_SP_, ODL_FUNC_PREFIX_(rootName), text,
               len, val ? val : "");
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_SP_ "\n", ODL_FUNC_PREFIX_(rootName), text,
                len, val ? val : "");
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogSp_

#  if MAC_OR_LINUX_
EXTERN_C void
ODLogTime_(const char *           fileName,
           const char *           funcName,
           const int              lineNumber,
           const char *           text1,
           const struct timeval * val1)
{
    const char * rootName = odFileNameRoot_(fileName);

    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_TI_ "\n", ODL_FUNC_PREFIX_(rootName), text1,
                val1->tv_sec, (long) val1->tv_usec);
        fflush(lOdLogFile_);
    }
    else
    {
#   if defined(__OBJC__)
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_TI_, ODL_FUNC_PREFIX_(rootName), text1, val1->tv_sec,
              (long) val1->tv_usec);
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_TI_, ODL_FUNC_PREFIX_(rootName), text1,
               val1->tv_sec, (long) val1->tv_usec);
#   endif // ! defined(__OBJC__)
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogTime_
#  endif // MAC_OR_LINUX_

EXTERN_C void
ODLogX1_(const char *  fileName,
         const char *  funcName,
         const int     lineNumber,
         const char *  text1,
         const int32_t val1)
{
    const char * rootName = odFileNameRoot_(fileName);

    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_X1_ "\n", ODL_FUNC_PREFIX_(rootName), text1,
                val1, val1);
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_X1_, ODL_FUNC_PREFIX_(rootName), text1, val1, val1);
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_X1_, ODL_FUNC_PREFIX_(rootName), text1,
               val1, val1);
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_X1_ "\n", ODL_FUNC_PREFIX_(rootName),
                text1, val1, val1);
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogX1_

EXTERN_C void
ODLogX2_(const char *  fileName,
         const char *  funcName,
         const int     lineNumber,
         const char *  text1,
         const int32_t val1,
         const char *  text2,
         const int32_t val2)
{
    const char * rootName = odFileNameRoot_(fileName);

    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_X2_ "\n", ODL_FUNC_PREFIX_(rootName), text1,
                val1, val1, text2, val2, val2);
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_X2_, ODL_FUNC_PREFIX_(rootName), text1, val1, val1, text2,
              val2, val2);
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_X2_, ODL_FUNC_PREFIX_(rootName), text1,
               val1, val1, text2, val2, val2);
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_X2_ "\n", ODL_FUNC_PREFIX_(rootName),
                text1, val1, val1, text2, val2, val2);
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogX2_

EXTERN_C void
ODLogX3_(const char *  fileName,
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

    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_X2_ "\n", ODL_FUNC_PREFIX_(rootName), text1,
                val1, val1, text2, val2, val2);
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_X1_ "\n", ODL_FUNC_PREFIX_(rootName), text3,
                val3, val3);
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_X2_, ODL_FUNC_PREFIX_(rootName), text1, val1, val1, text2,
              val2, val2);
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_X1_, ODL_FUNC_PREFIX_(rootName), text3, val3, val3);
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_X2_, ODL_FUNC_PREFIX_(rootName), text1,
               val1, val1, text2, val2, val2);
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_X1_, ODL_FUNC_PREFIX_(rootName), text3,
               val3, val3);
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_X2_ "\n", ODL_FUNC_PREFIX_(rootName),
                text1, val1, val1, text2, val2, val2);
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_X1_ "\n", ODL_FUNC_PREFIX_(rootName),
                text3, val3, val3);
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogX3_

EXTERN_C void
ODLogX4_(const char *  fileName,
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

    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_X2_ "\n", ODL_FUNC_PREFIX_(rootName), text1,
                val1, val1, text2, val2, val2);
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_X2_ "\n", ODL_FUNC_PREFIX_(rootName), text3,
                val3, val3, text4, val4, val4);
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_X2_, ODL_FUNC_PREFIX_(rootName), text1, val1, val1, text2,
              val2, val2);
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_X2_, ODL_FUNC_PREFIX_(rootName), text3, val3, val3, text4,
              val4, val4);
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_X2_, ODL_FUNC_PREFIX_(rootName), text1,
               val1, val1, text2, val2, val2);
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_X2_, ODL_FUNC_PREFIX_(rootName), text3,
               val3, val3, text4, val4, val4);
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_X2_ "\n", ODL_FUNC_PREFIX_(rootName),
                text1, val1, val1, text2, val2, val2);
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_X2_ "\n", ODL_FUNC_PREFIX_(rootName),
                text3, val3, val3, text4, val4, val4);
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogX4_

EXTERN_C void
ODLogXL1_(const char *  fileName,
          const char *  funcName,
          const int     lineNumber,
          const char *  text1,
          const int64_t val1)
{
    const char * rootName = odFileNameRoot_(fileName);

    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_XL1_ "\n", ODL_FUNC_PREFIX_(rootName), text1,
                val1, val1);
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_XL1_, ODL_FUNC_PREFIX_(rootName), text1, val1, val1);
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_XL1_, ODL_FUNC_PREFIX_(rootName), text1,
               val1, val1);
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_XL1_ "\n", ODL_FUNC_PREFIX_(rootName),
                text1, val1, val1);
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogXL1_

EXTERN_C void
ODLogXL2_(const char *  fileName,
          const char *  funcName,
          const int     lineNumber,
          const char *  text1,
          const int64_t val1,
          const char *  text2,
          const int64_t val2)
{
    const char * rootName = odFileNameRoot_(fileName);

    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_XL2_ "\n", ODL_FUNC_PREFIX_(rootName), text1,
                val1, val1, text2, val2, val2);
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_XL2_, ODL_FUNC_PREFIX_(rootName), text1, val1, val1, text2,
              val2, val2);
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_XL2_, ODL_FUNC_PREFIX_(rootName), text1,
               val1, val1, text2, val2, val2);
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_XL2_ "\n", ODL_FUNC_PREFIX_(rootName),
                text1, val1, val1, text2, val2, val2);
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogXL2_

EXTERN_C void
ODLogXL3_(const char *  fileName,
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

    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_XL2_ "\n", ODL_FUNC_PREFIX_(rootName), text1,
                val1, val1, text2, val2, val2);
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_XL1_ "\n", ODL_FUNC_PREFIX_(rootName), text3,
                val3, val3);
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_XL2_, ODL_FUNC_PREFIX_(rootName), text1, val1, val1, text2,
              val2, val2);
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_XL1_, ODL_FUNC_PREFIX_(rootName), text3, val3, val3);
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_XL2_, ODL_FUNC_PREFIX_(rootName), text1,
               val1, val1, text2, val2, val2);
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_XL1_, ODL_FUNC_PREFIX_(rootName), text3,
               val3, val3);
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_XL2_ "\n", ODL_FUNC_PREFIX_(rootName),
                text1, val1, val1, text2, val2, val2);
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_XL1_ "\n", ODL_FUNC_PREFIX_(rootName),
                text3, val3, val3);
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogXL3_

EXTERN_C void
ODLogXL4_(const char *  fileName,
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

    ODL_CREATE_INDENT_();
    ODL_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_XL2_ "\n", ODL_FUNC_PREFIX_(rootName), text1,
                val1, val1, text2, val2, val2);
        odWriteTime_(lOdLogFile_);
        fprintf(lOdLogFile_, ODL_FUNC_FORMAT_ ODL_FORMAT_XL2_ "\n", ODL_FUNC_PREFIX_(rootName), text3,
                val3, val3, text4, val4, val4);
        fflush(lOdLogFile_);
    }
    else
    {
#  if MAC_OR_LINUX_
#   if defined(__OBJC__)
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_XL2_, ODL_FUNC_PREFIX_(rootName), text1, val1, val1, text2,
              val2, val2);
        NSLog(@ODL_FUNC_FORMAT_ ODL_FORMAT_XL2_, ODL_FUNC_PREFIX_(rootName), text3, val3, val3, text4,
              val4, val4);
#   else // ! defined(__OBJC__)
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_XL2_, ODL_FUNC_PREFIX_(rootName), text1,
               val1, val1, text2, val2, val2);
        syslog(ODL_LEVEL_, ODL_FUNC_FORMAT_ ODL_FORMAT_XL2_, ODL_FUNC_PREFIX_(rootName), text3,
               val3, val3, text4, val4, val4);
#   endif // ! defined(__OBJC__)
#  else // ! MAC_OR_LINUX_
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_XL2_ "\n", ODL_FUNC_PREFIX_(rootName),
                text1, val1, val1, text2, val2, val2);
        odWriteTime_(ODL_STREAM_);
        fprintf(ODL_STREAM_, ODL_FUNC_FORMAT_ ODL_FORMAT_XL2_ "\n", ODL_FUNC_PREFIX_(rootName),
                text3, val3, val3, text4, val4, val4);
        fflush(ODL_STREAM_);
#  endif // ! MAC_OR_LINUX_
    }
    ODL_FREE_PREFIX_();
    ODL_FREE_INDENT_();
} // ODLogXL4_

# endif // defined(ODL_ENABLE_LOGGING_)
#endif // ! defined(ODLOGGING_INC_C_)
