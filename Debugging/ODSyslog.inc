//--------------------------------------------------------------------------------------
//
//  File:       ODSyslog.inc
//
//  Project:    YarpPlusPlus
//
//  Contains:   The function definitions for the debugging facility.
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

#if (! defined(ODSYSLOG_INC_))
# define ODSYSLOG_INC_ /* */

# include "ODSyslog.h"
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

# if defined(__cplusplus)
#  define EXTERN_C extern "C"
# else // ! defined(__cpluplus)
#  define EXTERN_C /* */
# endif // ! defined(__cplusplus)

# if (defined(ENABLE_OD_SYSLOG) && (! defined(DISABLE_OD_SYSLOG)))
#  define OD_MAX_BYTES_IN_LINE_ 16
#  define OD_MAX_CHARS_IN_LINE_ (OD_MAX_BYTES_IN_LINE_ * 4)
#  define OD_SYSLOG_INIT_VALUE_ 1
#  define OD_SYSLOG_LOG_TARGET_ LOG_LOCAL7
#  define OD_SYSLOG_LOG_LEVEL_  LOG_NOTICE

#  define OD_FUNC_FORMAT_ \
        "%s%s" OD_WHERE_FORMAT_
#  define OD_FUNC_PREFIX_(file_) \
        prefixString_, indentString_, funcName, file_, lineNumber
#  define OD_WHERE_FORMAT_ \
        "%s{%s@%04d}"

#  define OD_CREATE_INDENT_() \
        char * indentString_ = odBuildIndent_()
#  define OD_CREATE_PREFIX_() \
        char * prefixString_ = odBuildPrefix_()
#  define OD_FREE_INDENT_() \
        free(indentString_)
#  define OD_FREE_PREFIX_() \
        free(prefixString_)

typedef struct tOdThreadData_
{
    int _indentLevel;
} tOdThreadData;

static bool lOdIncludeProcessID_ = false;
static bool lOdIncludeThreadID_ = false;
static bool lOdEnableThreadSupport_ = false;

static FILE * lOdLogFile_ = NULL;

static pthread_key_t  lOdThreadSpecificKey_;
static pthread_once_t lOdThreadSpecificKeyOnce_ = PTHREAD_ONCE_INIT;

static tOdThreadData lOdThreadData_ = { OD_SYSLOG_INIT_VALUE_ };

inline static const char * odBoolToString_(const bool val)
{
    return (val ? "true" : "false");
} // odBoolToString_

static void odReleaseThreadSpecificData_(void * data)
{
    tOdThreadData * stuff = (tOdThreadData *) data;
    
    pthread_setspecific(lOdThreadSpecificKey_, NULL);
    free(stuff);
} // odReleaseThreadSpecificData_

static void odSetUpThreadKey_(void)
{
    if (pthread_key_create(&lOdThreadSpecificKey_, odReleaseThreadSpecificData_))
    {
#  if defined(__OBJC__)
        NSLog(@"problem creating thread-specific key => %d", errno);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, "problem creating thread-specific key => %d", errno);
#  endif // ! defined(__OBJC__)
    }
} // odSetUpThreadKey_

inline static tOdThreadData * odGetThreadData_(void)
{
    tOdThreadData * stuff;
    
    if (lOdEnableThreadSupport_)
    {
        pthread_once(&lOdThreadSpecificKeyOnce_, odSetUpThreadKey_);
        stuff = (tOdThreadData *) pthread_getspecific(lOdThreadSpecificKey_);
        if (! stuff)
        {
            stuff = (tOdThreadData *) malloc(sizeof(tOdThreadData));
            stuff->_indentLevel = OD_SYSLOG_INIT_VALUE_;
            pthread_setspecific(lOdThreadSpecificKey_, stuff);
        }
    }
    else
    {
        stuff = &lOdThreadData_;
    }
    return stuff;
} // odGetThreadData_

static const char * odFileNameRoot_(const char * fileName)
{
	char * result = strrchr(fileName, '/');

	return (result ? (result + 1) : fileName);
} // odFileNameRoot_

inline static int odGetIndent_(void)
{
    tOdThreadData * stuff = odGetThreadData_();

    return stuff->_indentLevel;
} // odGetIndent_

inline static void odSetIndent_(const int value)
{
    tOdThreadData * stuff = odGetThreadData_();

    stuff->_indentLevel = value;
} // odSetIndent_

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

static char * odBuildPrefix_(void)
{
    char *              result;
    size_t              length;
    static const size_t lengthHexString = 20; // Enough digits for an 8-byte value in hexadecimal, plus a little more.
    
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
    {
        length = 0;
        result = (char *) malloc(length + 1);
    }
    result[length] = '\0';
    return result;
} // odBuildPrefix_

inline static void odDecreaseIndent_(void)
{
    odSetIndent_(odGetIndent_() - 1);
} // odDecreaseIndent_

inline static void odIncreaseIndent_(void)
{
    odSetIndent_(odGetIndent_() + 1);
} // odIncreaseIndent_

static void odWriteTime_(void)
{
    char   buffer[80];
    time_t rawtime;

    time(&rawtime);
    strftime(buffer, sizeof(buffer), "%F %T ", localtime(&rawtime));
    fputs(buffer, lOdLogFile_);
} // odWriteTime_

#  define OD_EXIT_VALUE_       "exit -> "
#  define OD_FORMAT_B1_        " %s%s"
#  define OD_FORMAT_B2_        " %s%s, %s%s"
#  define OD_FORMAT_C1_        " %s'%c'(%#02X)"
#  define OD_FORMAT_C2_        " %s'%c'(%#02X), %s'%c'(%#02X)"
#  define OD_FORMAT_D1_        " %s%g"
#  define OD_FORMAT_D2_        " %s%g, %s%g"
#  define OD_FORMAT_ENTER_     " enter"
#  define OD_FORMAT_EXIT_      " exit"
#  define OD_FORMAT_EXIT_CALL_ " call exit(%ld(%#lx))"
#  define OD_FORMAT_EXIT_T_L_  " throw(%d(%#x))"
#  define OD_FORMAT_EXIT_T_S_  " throw(%s)"
#  define OD_FORMAT_IP_        " %s%d.%d.%d.%d:%d"
#  define OD_FORMAT_L1_        " %s%d(%#x)"
#  define OD_FORMAT_L2_        " %s%d(%#x), %s%d(%#x)"
#  define OD_FORMAT_LL1_       " %s%lld(%#llx)"
#  define OD_FORMAT_LL2_       " %s%lld(%#llx), %s%lld(%#llx)"
#  define OD_FORMAT_LOG_       " %s"
#  define OD_FORMAT_LS         " %s | %s |"
#  define OD_FORMAT_O1_        " %s%s"
#  define OD_FORMAT_O2_        " %s%s, %s%s"
#  define OD_FORMAT_P1_        " %s%p"
#  define OD_FORMAT_P2_        " %s%p, %s%p"
#  define OD_FORMAT_PACKET_1_  " address = %p, size = %d"
#  define OD_FORMAT_PACKET_2_  " %s | %04X : %s| %s"
#  define OD_FORMAT_RECT_      " %s[l: %g, t: %g, h: %g, w: %g]"
#  define OD_FORMAT_S1_        " %s'%s'"
#  define OD_FORMAT_S2_        " %s'%s', %s'%s'"
#  define OD_FORMAT_SP_        " %s'%.*s'"
#  define OD_FORMAT_TI_        " %s%ld:%ld"

EXTERN_C void ODSysLog_(const char * fileName,
                        const char * funcName,
                        const int    lineNumber,
                        const char * text)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_();
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_LOG_ "\n", OD_FUNC_PREFIX_(rootName), text ? text : "");
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_LOG_, OD_FUNC_PREFIX_(rootName), text ? text : "");
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_LOG_, OD_FUNC_PREFIX_(rootName), text ? text : "");
#  endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODSysLog_

EXTERN_C void ODSysLogB1_(const char * fileName,
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
        odWriteTime_();
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_B1_ "\n", OD_FUNC_PREFIX_(rootName), text1,
                odBoolToString_(val1));
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_B1_, OD_FUNC_PREFIX_(rootName), text1, odBoolToString_(val1));
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_B1_, OD_FUNC_PREFIX_(rootName), text1,
               odBoolToString_(val1));
#  endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODSysLogB1_

EXTERN_C void ODSysLogB2_(const char * fileName,
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
        odWriteTime_();
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_B2_ "\n", OD_FUNC_PREFIX_(rootName), text1,
                odBoolToString_(val1), text2, odBoolToString_(val2));
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_B2_, OD_FUNC_PREFIX_(rootName), text1, odBoolToString_(val1), text2,
              odBoolToString_(val2));
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_B2_, OD_FUNC_PREFIX_(rootName), text1,
               odBoolToString_(val1), text2, odBoolToString_(val2));
#  endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODSysLogB2_

EXTERN_C void ODSysLogB3_(const char * fileName,
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
        odWriteTime_();
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_B2_ "\n", OD_FUNC_PREFIX_(rootName), text1,
                odBoolToString_(val1), text2, odBoolToString_(val2));
        odWriteTime_();
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_B1_ "\n", OD_FUNC_PREFIX_(rootName), text3,
                odBoolToString_(val3));
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_B2_, OD_FUNC_PREFIX_(rootName), text1, odBoolToString_(val1), text2,
              odBoolToString_(val2));
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_B1_, OD_FUNC_PREFIX_(rootName), text3, odBoolToString_(val3));
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_B2_, OD_FUNC_PREFIX_(rootName), text1,
               odBoolToString_(val1), text2, odBoolToString_(val2));
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_B1_, OD_FUNC_PREFIX_(rootName), text3,
               odBoolToString_(val3));
#  endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODSysLogB3_

EXTERN_C void ODSysLogB4_(const char * fileName,
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
        odWriteTime_();
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_B2_ "\n", OD_FUNC_PREFIX_(rootName), text1,
                odBoolToString_(val1), text2, odBoolToString_(val2));
        odWriteTime_();
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_B2_ "\n", OD_FUNC_PREFIX_(rootName), text3,
                odBoolToString_(val3), text4, odBoolToString_(val4));
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_B2_, OD_FUNC_PREFIX_(rootName), text1, odBoolToString_(val1), text2,
              odBoolToString_(val2));
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_B2_, OD_FUNC_PREFIX_(rootName), text3, odBoolToString_(val3), text4,
              odBoolToString_(val4));
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_B2_, OD_FUNC_PREFIX_(rootName), text1,
               odBoolToString_(val1), text2, odBoolToString_(val2));
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_B2_, OD_FUNC_PREFIX_(rootName), text3,
               odBoolToString_(val3), text4, odBoolToString_(val4));
#  endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODSysLogB4_

EXTERN_C void ODSysLogC1_(const char * fileName,
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
        odWriteTime_();
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_C1_ "\n", OD_FUNC_PREFIX_(rootName), text1, val1, val1);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_C1_, OD_FUNC_PREFIX_(rootName), text1, val1, val1);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_C1_, OD_FUNC_PREFIX_(rootName), text1, val1, val1);
#  endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODSysLogC1_

EXTERN_C void ODSysLogC2_(const char * fileName,
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
        odWriteTime_();
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_C2_ "\n", OD_FUNC_PREFIX_(rootName), text1, val1, val1, text2,
                val2, val2);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_C2_, OD_FUNC_PREFIX_(rootName), text1, val1, val1, text2, val2, val2);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_C2_, OD_FUNC_PREFIX_(rootName), text1, val1, val1, text2,
               val2, val2);
#  endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODSysLogC2_

EXTERN_C void ODSysLogC3_(const char * fileName,
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
        odWriteTime_();
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_C2_ "\n", OD_FUNC_PREFIX_(rootName), text1, val1, val1, text2,
                val2, val2);
        odWriteTime_();
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_C1_ "\n", OD_FUNC_PREFIX_(rootName), text3, val3, val3);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_C2_, OD_FUNC_PREFIX_(rootName), text1, val1, val1, text2, val2, val2);
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_C1_, OD_FUNC_PREFIX_(rootName), text3, val3, val3);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_C2_, OD_FUNC_PREFIX_(rootName), text1, val1, val1, text2,
               val2, val2);
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_C1_, OD_FUNC_PREFIX_(rootName), text3, val3, val3);
#  endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODSysLogC3_

EXTERN_C void ODSysLogC4_(const char * fileName,
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
        odWriteTime_();
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_C2_ "\n", OD_FUNC_PREFIX_(rootName), text1, val1, val1, text2,
                val2, val2);
        odWriteTime_();
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_C2_ "\n", OD_FUNC_PREFIX_(rootName), text3, val3, val3, text4,
                val4, val4);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_C2_, OD_FUNC_PREFIX_(rootName), text1, val1, val1, text2, val2, val2);
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_C2_, OD_FUNC_PREFIX_(rootName), text3, val3, val3, text4, val4, val4);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_C2_, OD_FUNC_PREFIX_(rootName), text1, val1, val1, text2,
               val2, val2);
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_C2_, OD_FUNC_PREFIX_(rootName), text3, val3, val3, text4,
               val4, val4);
#  endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODSysLogC4_

EXTERN_C void ODSysLogD1_(const char * fileName,
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
        odWriteTime_();
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_D1_ "\n", OD_FUNC_PREFIX_(rootName), text1, val1);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_D1_, OD_FUNC_PREFIX_(rootName), text1, val1);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_D1_, OD_FUNC_PREFIX_(rootName), text1, val1);
#  endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODSysLogD1_

EXTERN_C void ODSysLogD2_(const char * fileName,
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
        odWriteTime_();
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_D2_ "\n", OD_FUNC_PREFIX_(rootName), text1, val1, text2, val2);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_D2_, OD_FUNC_PREFIX_(rootName), text1, val1, text2, val2);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_D2_, OD_FUNC_PREFIX_(rootName), text1, val1, text2,
               val2);
#  endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODSysLogD2_

EXTERN_C void ODSysLogD3_(const char * fileName,
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
        odWriteTime_();
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_D2_ "\n", OD_FUNC_PREFIX_(rootName), text1, val1, text2, val2);
        odWriteTime_();
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_D1_ "\n", OD_FUNC_PREFIX_(rootName), text3, val3);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_D2_, OD_FUNC_PREFIX_(rootName), text1, val1, text2, val2);
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_D1_, OD_FUNC_PREFIX_(rootName), text3, val3);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_D2_, OD_FUNC_PREFIX_(rootName), text1, val1, text2,
               val2);
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_D1_, OD_FUNC_PREFIX_(rootName), text3, val3);
#  endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODSysLogD3_

EXTERN_C void ODSysLogD4_(const char * fileName,
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
        odWriteTime_();
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_D2_ "\n", OD_FUNC_PREFIX_(rootName), text1, val1, text2, val2);
        odWriteTime_();
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_D2_ "\n", OD_FUNC_PREFIX_(rootName), text3, val3, text4, val4);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_D2_, OD_FUNC_PREFIX_(rootName), text1, val1, text2, val2);
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_D2_, OD_FUNC_PREFIX_(rootName), text3, val3, text4, val4);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_D2_, OD_FUNC_PREFIX_(rootName), text1, val1, text2,
               val2);
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_D2_, OD_FUNC_PREFIX_(rootName), text3, val3, text4,
               val4);
#  endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODSysLogD4_

EXTERN_C void ODSysLogEnter_(const char * fileName,
                             const char * funcName,
                             const int    lineNumber)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_();
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_ENTER_ "\n", OD_FUNC_PREFIX_(rootName));
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_ENTER_, OD_FUNC_PREFIX_(rootName));
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_ENTER_, OD_FUNC_PREFIX_(rootName));
#  endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
    odIncreaseIndent_();
} // ODSysLogEnter_

EXTERN_C void ODSysLogExit_(const char * fileName,
                            const char * funcName,
                            const int    lineNumber)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    odDecreaseIndent_();
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_();
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_EXIT_ "\n", OD_FUNC_PREFIX_(rootName));
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_EXIT_, OD_FUNC_PREFIX_(rootName));
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_EXIT_, OD_FUNC_PREFIX_(rootName));
#  endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODSysLogExit_

EXTERN_C void ODSysLogExitB_(const char * fileName,
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
        odWriteTime_();
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_B1_ "\n", OD_FUNC_PREFIX_(rootName), OD_EXIT_VALUE_,
                odBoolToString_(val));
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_B1_, OD_FUNC_PREFIX_(rootName), OD_EXIT_VALUE_, odBoolToString_(val));
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_B1_, OD_FUNC_PREFIX_(rootName), OD_EXIT_VALUE_,
               odBoolToString_(val));
#  endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODSysLogExitB_

EXTERN_C void ODSysLogExitC_(const char * fileName,
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
        odWriteTime_();
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_C1_ "\n", OD_FUNC_PREFIX_(rootName), OD_EXIT_VALUE_, val, val);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_C1_, OD_FUNC_PREFIX_(rootName), OD_EXIT_VALUE_, val, val);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_C1_, OD_FUNC_PREFIX_(rootName), OD_EXIT_VALUE_, val, val);
#  endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODSysLogExitC_

EXTERN_C void ODSysLogExitD_(const char * fileName,
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
        odWriteTime_();
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_D1_ "\n", OD_FUNC_PREFIX_(rootName), OD_EXIT_VALUE_, val);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_D1_, OD_FUNC_PREFIX_(rootName), OD_EXIT_VALUE_, val);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_D1_, OD_FUNC_PREFIX_(rootName), OD_EXIT_VALUE_, val);
#  endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODSysLogExitD_

EXTERN_C void ODSysLogExitExit_(const char * fileName,
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
        odWriteTime_();
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_EXIT_CALL_ "\n", OD_FUNC_PREFIX_(rootName), val, val);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_EXIT_CALL_, OD_FUNC_PREFIX_(rootName), val, val);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_EXIT_CALL_, OD_FUNC_PREFIX_(rootName), val, val);
#  endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODSysLogExitExit_

EXTERN_C void ODSysLogExitL_(const char *  fileName,
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
        odWriteTime_();
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_L1_ "\n", OD_FUNC_PREFIX_(rootName), OD_EXIT_VALUE_, val, val);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_L1_, OD_FUNC_PREFIX_(rootName), OD_EXIT_VALUE_, val, val);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_L1_, OD_FUNC_PREFIX_(rootName), OD_EXIT_VALUE_, val,
               val);
#  endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODSysLogExitL_

EXTERN_C void ODSysLogExitLL_(const char *  fileName,
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
        odWriteTime_();
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_LL1_ "\n", OD_FUNC_PREFIX_(rootName), OD_EXIT_VALUE_, val, val);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_LL1_, OD_FUNC_PREFIX_(rootName), OD_EXIT_VALUE_, val, val);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_LL1_, OD_FUNC_PREFIX_(rootName), OD_EXIT_VALUE_, val,
               val);
#  endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODSysLogExitLL_

#  if defined(__OBJC__)
EXTERN_C void ODSysLogExitO_(const char * fileName,
                             const char * funcName,
                             const int    lineNumber,
                             const id     val)
{
    const char * rootName = odFileNameRoot_(fileName);
    const char * valString = (val ? [[val description] UTF8String] : "<>");
    
    odDecreaseIndent_();
	OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_();
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_O1_ "\n", OD_FUNC_PREFIX_(rootName), OD_EXIT_VALUE_, valString);
        fflush(lOdLogFile_);
    }
    else
    {
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_O1_, OD_FUNC_PREFIX_(rootName), OD_EXIT_VALUE_, valString);
    }
    OD_FREE_PREFIX_();
	OD_FREE_INDENT_();
} // ODSysLogExitO_
#  endif // defined(__OBJC__)

EXTERN_C void ODSysLogExitP_(const char * fileName,
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
        odWriteTime_();
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_P1_ "\n", OD_FUNC_PREFIX_(rootName), OD_EXIT_VALUE_, val);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_P1_, OD_FUNC_PREFIX_(rootName), OD_EXIT_VALUE_, val);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_P1_, OD_FUNC_PREFIX_(rootName), OD_EXIT_VALUE_, val);
#  endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODSysLogExitP_

#  if defined(__APPLE__)
EXTERN_C void ODSysLogExitR_(const char * fileName,
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
        odWriteTime_();
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_RECT_ "\n", OD_FUNC_PREFIX_(rootName), OD_EXIT_VALUE_,
                val.origin.x, val.origin.y, val.size.height, val.size.width);
        fflush(lOdLogFile_);
    }
    else
    {
#    if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_RECT_, OD_FUNC_PREFIX_(rootName), OD_EXIT_VALUE_, val.origin.x, val.origin.y,
              val.size.height, val.size.width);
#    else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_RECT_, OD_FUNC_PREFIX_(rootName), OD_EXIT_VALUE_,
               val.origin.x, val.origin.y, val.size.height, val.size.width);
#    endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
	OD_FREE_INDENT_();
} // ODSysLogExitR_
#   endif // defined(__APPLE__)

EXTERN_C void ODSysLogExitS_(const char * fileName,
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
        odWriteTime_();
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_S1_ "\n", OD_FUNC_PREFIX_(rootName), OD_EXIT_VALUE_,
                val ? val : "<>");
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_S1_, OD_FUNC_PREFIX_(rootName), OD_EXIT_VALUE_, val ? val : "<>");
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_S1_, OD_FUNC_PREFIX_(rootName), OD_EXIT_VALUE_,
               val ? val : "<>");
#  endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODSysLogExitS_

EXTERN_C void ODSysLogExitThrowL_(const char *  fileName,
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
        odWriteTime_();
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_EXIT_T_L_ "\n", OD_FUNC_PREFIX_(rootName), val, val);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_EXIT_T_L_, OD_FUNC_PREFIX_(rootName), val, val);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_EXIT_T_L_, OD_FUNC_PREFIX_(rootName), val, val);
#  endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODSysLogExitThrowL_

EXTERN_C void ODSysLogExitThrowS_(const char * fileName,
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
        odWriteTime_();
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_EXIT_T_S_ "\n", OD_FUNC_PREFIX_(rootName), val ? val : "<>");
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_EXIT_T_S_, OD_FUNC_PREFIX_(rootName), val ? val : "<>");
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_EXIT_T_S_, OD_FUNC_PREFIX_(rootName), val ? val : "<>");
#  endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODSysLogExitThrowS_

# define OD_INIT_FORMAT_ "* %s%s" OD_WHERE_FORMAT_ " started *"

EXTERN_C void ODSysLogInit_(const char * prefix,
                            const int    options,
                            const char * fileName,
                            const char * funcName,
                            const int    lineNumber)
{
#  if defined(__OBJC__)
#   pragma unused(prefix)
#  endif // defined(__OBJC__)
    bool         odWriteToFile = (options & kODSyslogOptionWriteToFile);
	const char * rootName = odFileNameRoot_(fileName);
    const char * suffixString;
	char *       stars;
    size_t       starsLength = strlen(rootName) + strlen(funcName) + (sizeof(OD_INIT_FORMAT_) - 1) - 8; // 4 %s
    
    lOdIncludeProcessID_ = (options & kODSyslogOptionIncludeProcessID);
    lOdIncludeThreadID_ = (options & kODSyslogOptionIncludeThreadID);
    lOdEnableThreadSupport_ = (options & kODSyslogOptionEnableThreadSupport);
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
#  if (! defined(__OBJC__))
    openlog(prefix, LOG_PID | LOG_CONS | LOG_PERROR, OD_SYSLOG_LOG_TARGET_);
#  endif // ! defined(__OBJC__)
    if (odWriteToFile)
    {
        char pidString[100];
        
        snprintf(pidString, sizeof(pidString), "/var/log/pid-%lX.log", (long unsigned) getpid());
        lOdLogFile_ = fopen(pidString, "w");
        if (lOdLogFile_)
        {
            odWriteTime_();
            fprintf(lOdLogFile_, "%s\n", stars);
        }
        else
        {
#  if defined(__OBJC__)
            NSLog(@"problem opening %s => %d", pidString, errno);
#  else // ! defined(__OBJC__)
            syslog(OD_SYSLOG_LOG_LEVEL_, "problem opening %s => %d", pidString, errno);
#  endif // ! defined(__OBJC__)
        }
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@"%s", stars);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, "%s", stars);
#  endif // ! defined(__OBJC__)
    }
    if (lOdLogFile_)
    {
        odWriteTime_();
        fprintf(lOdLogFile_, OD_INIT_FORMAT_ "\n", prefixString_, suffixString, funcName, rootName, lineNumber);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_INIT_FORMAT_, prefixString_, suffixString, funcName, rootName, lineNumber);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_INIT_FORMAT_, prefixString_, suffixString, funcName, rootName, lineNumber);
#  endif // ! defined(__OBJC__)
    }
    if (lOdLogFile_)
    {
        odWriteTime_();
        fprintf(lOdLogFile_, "%s\n", stars);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@"%s", stars);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, "%s", stars);
#  endif // ! defined(__OBJC__)
    }
	free(stars);
    OD_FREE_PREFIX_();
    odSetIndent_(OD_SYSLOG_INIT_VALUE_);
} // ODSysLogInit_

EXTERN_C void ODSysLogIP_(const char *  fileName,
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
        odWriteTime_();
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_IP_ "\n", OD_FUNC_PREFIX_(rootName), text1,
                address.asBytes[0], address.asBytes[1], address.asBytes[2], address.asBytes[3], val2);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_IP_, OD_FUNC_PREFIX_(rootName), text1, address.asBytes[0],
              address.asBytes[1], address.asBytes[2], address.asBytes[3], val2);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_IP_, OD_FUNC_PREFIX_(rootName), text1,
               address.asBytes[0], address.asBytes[1], address.asBytes[2], address.asBytes[3], val2);
#  endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODSysLogIP_

EXTERN_C void ODSysLogL1_(const char *  fileName,
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
        odWriteTime_();
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_L1_ "\n", OD_FUNC_PREFIX_(rootName), text1, val1, val1);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_L1_, OD_FUNC_PREFIX_(rootName), text1, val1, val1);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_L1_, OD_FUNC_PREFIX_(rootName), text1, val1, val1);
#  endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODSysLogL1_

EXTERN_C void ODSysLogL2_(const char *  fileName,
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
        odWriteTime_();
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_L2_ "\n", OD_FUNC_PREFIX_(rootName), text1, val1, val1, text2,
                val2, val2);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_L2_, OD_FUNC_PREFIX_(rootName), text1, val1, val1, text2, val2, val2);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_L2_, OD_FUNC_PREFIX_(rootName), text1, val1, val1, text2,
               val2, val2);
#  endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODSysLogL2_

EXTERN_C void ODSysLogL3_(const char *  fileName,
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
        odWriteTime_();
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_L2_ "\n", OD_FUNC_PREFIX_(rootName), text1, val1, val1, text2,
                val2, val2);
        odWriteTime_();
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_L1_ "\n", OD_FUNC_PREFIX_(rootName), text3, val3, val3);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_L2_, OD_FUNC_PREFIX_(rootName), text1, val1, val1, text2, val2, val2);
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_L1_, OD_FUNC_PREFIX_(rootName), text3, val3, val3);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_L2_, OD_FUNC_PREFIX_(rootName), text1, val1, val1, text2,
               val2, val2);
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_L1_, OD_FUNC_PREFIX_(rootName), text3, val3, val3);
#  endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODSysLogL3_

EXTERN_C void ODSysLogL4_(const char *  fileName,
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
        odWriteTime_();
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_L2_ "\n", OD_FUNC_PREFIX_(rootName), text1, val1, val1, text2,
                val2, val2);
        odWriteTime_();
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_L2_ "\n", OD_FUNC_PREFIX_(rootName), text3, val3, val3, text4,
                val4, val4);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_L2_, OD_FUNC_PREFIX_(rootName), text1, val1, val1, text2, val2, val2);
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_L2_, OD_FUNC_PREFIX_(rootName), text3, val3, val3, text4, val4, val4);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_L2_, OD_FUNC_PREFIX_(rootName), text1, val1, val1, text2,
               val2, val2);
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_L2_, OD_FUNC_PREFIX_(rootName), text3, val3, val3, text4,
               val4, val4);
#  endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODSysLogL4_

EXTERN_C void ODSysLogLL1_(const char *  fileName,
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
        odWriteTime_();
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_LL1_ "\n", OD_FUNC_PREFIX_(rootName), text1, val1, val1);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_LL1_, OD_FUNC_PREFIX_(rootName), text1, val1, val1);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_LL1_, OD_FUNC_PREFIX_(rootName), text1, val1, val1);
#  endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODSysLogLL1_

EXTERN_C void ODSysLogLL2_(const char *  fileName,
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
        odWriteTime_();
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_LL2_ "\n", OD_FUNC_PREFIX_(rootName), text1, val1, val1, text2,
                val2, val2);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_LL2_, OD_FUNC_PREFIX_(rootName), text1, val1, val1, text2, val2, val2);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_LL2_, OD_FUNC_PREFIX_(rootName), text1, val1, val1,
               text2, val2, val2);
#  endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODSysLogLL2_

EXTERN_C void ODSysLogLL3_(const char *  fileName,
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
        odWriteTime_();
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_LL2_ "\n", OD_FUNC_PREFIX_(rootName), text1, val1, val1, text2,
                val2, val2);
        odWriteTime_();
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_LL1_ "\n", OD_FUNC_PREFIX_(rootName), text3, val3, val3);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_LL2_, OD_FUNC_PREFIX_(rootName), text1, val1, val1, text2, val2, val2);
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_LL1_, OD_FUNC_PREFIX_(rootName), text3, val3, val3);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_LL2_, OD_FUNC_PREFIX_(rootName), text1, val1, val1,
               text2, val2, val2);
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_LL1_, OD_FUNC_PREFIX_(rootName), text3, val3, val3);
#  endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODSysLogLL3_

EXTERN_C void ODSysLogLL4_(const char *  fileName,
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
        odWriteTime_();
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_LL2_ "\n", OD_FUNC_PREFIX_(rootName), text1, val1, val1, text2,
                val2, val2);
        odWriteTime_();
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_LL2_ "\n", OD_FUNC_PREFIX_(rootName), text3, val3, val3, text4,
                val4, val4);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_LL2_, OD_FUNC_PREFIX_(rootName), text1, val1, val1, text2, val2, val2);
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_LL2_, OD_FUNC_PREFIX_(rootName), text3, val3, val3, text4, val4, val4);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_LL2_, OD_FUNC_PREFIX_(rootName), text1, val1, val1,
               text2, val2, val2);
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_LL2_, OD_FUNC_PREFIX_(rootName), text3, val3, val3,
               text4, val4, val4);
#  endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODSysLogLL4_

EXTERN_C void ODSysLogLS_(const char * fileName,
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
                    odWriteTime_();
                    fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_LS "\n", OD_FUNC_PREFIX_(rootName), heading,
                            lineBuffer);
                }
                else
                {
#  if defined(__OBJC__)
                    NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_LS, OD_FUNC_PREFIX_(rootName), heading, lineBuffer);
#  else // ! defined(__OBJC__)
                    syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_LS, OD_FUNC_PREFIX_(rootName), heading,
                           lineBuffer);
#  endif // ! defined(__OBJC__)
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
                odWriteTime_();
                fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_LS "\n", OD_FUNC_PREFIX_(rootName), heading,
                        lineBuffer);
            }
            else
            {
#  if defined(__OBJC__)
                NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_LS, OD_FUNC_PREFIX_(rootName), heading, lineBuffer);
#  else // ! defined(__OBJC__)
                syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_LS, OD_FUNC_PREFIX_(rootName), heading,
                       lineBuffer);
#  endif // ! defined(__OBJC__)
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
            odWriteTime_();
            fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_LS "\n", OD_FUNC_PREFIX_(rootName), heading, lineBuffer);
        }
        else
        {
#  if defined(__OBJC__)
            NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_LS, OD_FUNC_PREFIX_(rootName), heading, lineBuffer);
#  else // ! defined(__OBJC__)
            syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_LS, OD_FUNC_PREFIX_(rootName), heading,
                   lineBuffer);
#  endif // ! defined(__OBJC__)
        }
    }
    if (lOdLogFile_)
    {
        fflush(lOdLogFile_);
    }
    free(blankCaption);
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODSysLogLS_

#  if defined(__OBJC__)
EXTERN_C void ODSysLogO1_(const char * fileName,
                          const char * funcName,
                          const int    lineNumber,
                          const char * text1,
                          const id     obj1)
{
    const char * obj1String = (obj1 ? [[obj1 description] UTF8String] : "<>");
    const char * rootName = odFileNameRoot_(fileName);
    
	OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_();
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_O1_ "\n", OD_FUNC_PREFIX_(rootName), text1, obj1String);
        fflush(lOdLogFile_);
    }
    else
    {
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_O1_, OD_FUNC_PREFIX_(rootName), text1, obj1String);
    }
    OD_FREE_PREFIX_();
	OD_FREE_INDENT_();
} // ODSysLogO1_
#  endif // defined(__OBJC__)

#  if defined(__OBJC__)
EXTERN_C void ODSysLogO2_(const char * fileName,
                          const char * funcName,
                          const int    lineNumber,
                          const char * text1,
                          const id     obj1,
                          const char * text2,
                          const id     obj2)
{
    const char * obj1String = (obj1 ? [[obj1 description] UTF8String] : "<>");
    const char * obj2String = (obj2 ? [[obj2 description] UTF8String] : "<>");
    const char * rootName = odFileNameRoot_(fileName);
    
	OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_();
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
} // ODSysLogO2_
#  endif // defined(__OBJC__)

#  if defined(__OBJC__)
EXTERN_C void ODSysLogO3_(const char * fileName,
                          const char * funcName,
                          const int    lineNumber,
                          const char * text1,
                          const id     obj1,
                          const char * text2,
                          const id     obj2,
                          const char * text3,
                          const id     obj3)
{
    const char * obj1String = (obj1 ? [[obj1 description] UTF8String] : "<>");
    const char * obj2String = (obj2 ? [[obj2 description] UTF8String] : "<>");
    const char * obj3String = (obj3 ? [[obj3 description] UTF8String] : "<>");
    const char * rootName = odFileNameRoot_(fileName);
    
	OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_();
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
} // ODSysLogO3_
#  endif // defined(__OBJC__)

#  if defined(__OBJC__)
EXTERN_C void ODSysLogO4_(const char * fileName,
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
    const char * obj1String = (obj1 ? [[obj1 description] UTF8String] : "<>");
    const char * obj2String = (obj2 ? [[obj2 description] UTF8String] : "<>");
    const char * obj3String = (obj3 ? [[obj3 description] UTF8String] : "<>");
    const char * obj4String = (obj4 ? [[obj4 description] UTF8String] : "<>");
    const char * rootName = odFileNameRoot_(fileName);
    
	OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lOdLogFile_)
    {
        odWriteTime_();
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
} // ODSysLogO4_
#  endif // defined(__OBJC__)

EXTERN_C void ODSysLogP1_(const char * fileName,
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
        odWriteTime_();
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_P1_ "\n", OD_FUNC_PREFIX_(rootName), text1, ptr1);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_P1_, OD_FUNC_PREFIX_(rootName), text1, ptr1);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_P1_, OD_FUNC_PREFIX_(rootName), text1, ptr1);
#  endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODSysLogP1_

EXTERN_C void ODSysLogP2_(const char * fileName,
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
        odWriteTime_();
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_P2_ "\n", OD_FUNC_PREFIX_(rootName), text1, ptr1, text2, ptr2);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_P2_, OD_FUNC_PREFIX_(rootName), text1, ptr1, text2, ptr2);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_P2_, OD_FUNC_PREFIX_(rootName), text1, ptr1, text2,
               ptr2);
#  endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODSysLogP2_

EXTERN_C void ODSysLogP3_(const char * fileName,
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
        odWriteTime_();
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_P2_ "\n", OD_FUNC_PREFIX_(rootName), text1, ptr1, text2, ptr2);
        odWriteTime_();
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_P1_ "\n", OD_FUNC_PREFIX_(rootName), text3, ptr3);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_P2_, OD_FUNC_PREFIX_(rootName), text1, ptr1, text2, ptr2);
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_P1_, OD_FUNC_PREFIX_(rootName), text3, ptr3);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_P2_, OD_FUNC_PREFIX_(rootName), text1, ptr1, text2,
               ptr2);
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_P1_, OD_FUNC_PREFIX_(rootName), text3, ptr3);
#  endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODSysLogP3_

EXTERN_C void ODSysLogP4_(const char * fileName,
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
        odWriteTime_();
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_P2_ "\n", OD_FUNC_PREFIX_(rootName), text1, ptr1, text2, ptr2);
        odWriteTime_();
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_P2_ "\n", OD_FUNC_PREFIX_(rootName), text3, ptr3, text4, ptr4);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_P2_, OD_FUNC_PREFIX_(rootName), text1, ptr1, text2, ptr2);
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_P2_, OD_FUNC_PREFIX_(rootName), text3, ptr3, text4, ptr4);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_P2_, OD_FUNC_PREFIX_(rootName), text1, ptr1, text2,
               ptr2);
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_P2_, OD_FUNC_PREFIX_(rootName), text3, ptr3, text4,
               ptr4);
#  endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODSysLogP4_

EXTERN_C void ODSysLogPacket_(const char * fileName,
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
        odWriteTime_();
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_PACKET_1_ "\n", OD_FUNC_PREFIX_(rootName), buffer, size);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_PACKET_1_, OD_FUNC_PREFIX_(rootName), buffer, size);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_PACKET_1_, OD_FUNC_PREFIX_(rootName), buffer, size);
#  endif // ! defined(__OBJC__)
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
            odWriteTime_();
            fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_PACKET_2_ "\n", OD_FUNC_PREFIX_(rootName), heading, ii,
                    lineBuffer, charBuffer);
        }
        else
        {
#  if defined(__OBJC__)
            NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_PACKET_2_, OD_FUNC_PREFIX_(rootName), heading, ii, lineBuffer,
                  charBuffer);
#  else // ! defined(__OBJC__)
            syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_PACKET_2_, OD_FUNC_PREFIX_(rootName), heading,
                   ii, lineBuffer, charBuffer);
#  endif // ! defined(__OBJC__)
        }
        memset(lineBuffer, ' ', sizeof(lineBuffer) - 1);
        heading = blankCaption;
    }
    if (lOdLogFile_)
    {
        fflush(lOdLogFile_);
    }
    free(blankCaption);
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODSysLogPacket_

#  if defined(__APPLE__)
EXTERN_C void ODSysLogRect_(const char * fileName,
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
        odWriteTime_();
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
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_RECT_, OD_FUNC_PREFIX_(rootName), caption, rect.origin.x,
               rect.origin.y, rect.size.height, rect.size.width);
#   endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
	OD_FREE_INDENT_();
} // ODSysLogRect_
#  endif // defined(__APPLE__)

EXTERN_C void ODSysLogS1_(const char * fileName,
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
        odWriteTime_();
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_S1_ "\n", OD_FUNC_PREFIX_(rootName), text1, val1 ? val1 : "<>");
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_S1_, OD_FUNC_PREFIX_(rootName), text1, val1 ? val1 : "<>");
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_S1_, OD_FUNC_PREFIX_(rootName), text1,
               val1 ? val1 : "<>");
#  endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODSysLogS1_

EXTERN_C void ODSysLogS2_(const char * fileName,
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
        odWriteTime_();
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_S2_ "\n", OD_FUNC_PREFIX_(rootName), text1,
                val1 ? val1 : "<>", text2, val2 ? val2 : "<>");
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_S2_, OD_FUNC_PREFIX_(rootName), text1, val1 ? val1 : "<>", text2,
              val2 ? val2 : "<>");
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_S2_, OD_FUNC_PREFIX_(rootName), text1,
               val1 ? val1 : "<>", text2, val2 ? val2 : "<>");
#  endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODSysLogS2_

EXTERN_C void ODSysLogS3_(const char * fileName,
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
        odWriteTime_();
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_S2_ "\n", OD_FUNC_PREFIX_(rootName), text1,
                val1 ? val1 : "<>", text2, val2 ? val2 : "<>");
        odWriteTime_();
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_S1_ "\n", OD_FUNC_PREFIX_(rootName), text3, val3 ? val3 : "<>");
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_S2_, OD_FUNC_PREFIX_(rootName), text1, val1 ? val1 : "<>", text2,
              val2 ? val2 : "<>");
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_S1_, OD_FUNC_PREFIX_(rootName), text3, val3 ? val3 : "<>");
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_S2_, OD_FUNC_PREFIX_(rootName), text1,
               val1 ? val1 : "<>", text2, val2 ? val2 : "<>");
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_S1_, OD_FUNC_PREFIX_(rootName), text3,
               val3 ? val3 : "<>");
#  endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODSysLogS3_

EXTERN_C void ODSysLogS4_(const char * fileName,
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
        odWriteTime_();
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_S2_ "\n", OD_FUNC_PREFIX_(rootName), text1,
                val1 ? val1 : "<>", text2, val2 ? val2 : "<>");
        odWriteTime_();
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_S2_ "\n", OD_FUNC_PREFIX_(rootName), text3,
                val3 ? val3 : "<>", text4, val4 ? val4 : "<>");
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_S2_, OD_FUNC_PREFIX_(rootName), text1, val1 ? val1 : "<>", text2,
              val2 ? val2 : "<>");
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_S2_, OD_FUNC_PREFIX_(rootName), text3, val3 ? val3 : "<>", text4,
              val4 ? val4 : "<>");
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_S2_, OD_FUNC_PREFIX_(rootName), text1,
               val1 ? val1 : "<>", text2, val2 ? val2 : "<>");
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_S2_, OD_FUNC_PREFIX_(rootName), text3,
               val3 ? val3 : "<>", text4, val4 ? val4 : "<>");
#  endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODSysLogS4_

EXTERN_C void ODSysLogSp_(const char * fileName,
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
        odWriteTime_();
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_SP_ "\n", OD_FUNC_PREFIX_(rootName), text, len, val ? val : "");
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_SP_, OD_FUNC_PREFIX_(rootName), text, len, val ? val : "");
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_SP_, OD_FUNC_PREFIX_(rootName), text, len,
               val ? val : "");
#  endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODSysLogSp_

EXTERN_C void ODSysLogTi_(const char *           fileName,
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
        odWriteTime_();
        fprintf(lOdLogFile_, OD_FUNC_FORMAT_ OD_FORMAT_TI_ "\n", OD_FUNC_PREFIX_(rootName), text1, val1->tv_sec,
                (long) val1->tv_usec);
        fflush(lOdLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ OD_FORMAT_TI_, OD_FUNC_PREFIX_(rootName), text1, val1->tv_sec, (long) val1->tv_usec);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ OD_FORMAT_TI_, OD_FUNC_PREFIX_(rootName), text1, val1->tv_sec,
               (long) val1->tv_usec);
#  endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODSysLogTi_

# endif // defined(ENABLE_OD_SYSLOG) && (! defined(DISABLE_OD_SYSLOG))
#endif // ! defined(ODSYSLOG_INC_)
