//
//  ODSyslog.inc
//  iExamine
//
//  Created by Norman Jaffe on 2013-04-07.
//  Copyright (c) 2013 OpenDragon. All rights reserved.
//

#if (! defined(ODSYSLOG_INC_))
# define ODSYSLOG_INC_ /* */

# include "ODSyslog.h"
# include <pthread.h>
# include <stdlib.h>
# include <string.h>
# include <ctype.h>
# include <errno.h>
# include <unistd.h>
# if defined(__OBJC__)
#  import "Foundation/NSObjCRuntime.h"
# else // ! defined(__OBJC__)
#  include <syslog.h>
# endif // ! defined(__OBJC__)

# if defined(__cplusplus)
#  define EXTERN_C extern "C"
# else // ! defined(__cpluplus)
#  define EXTERN_C /* */
# endif // ! defined(__cplusplus)

# if defined(ENABLE_OD_SYSLOG)
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

static bool lAbsIncludeProcessID = false;
static bool lAbsIncludeThreadID = false;

static FILE * lAbsLogFile_ = NULL;

static pthread_key_t lAbsThreadSpecificKey;
static pthread_once_t lAbsThreadSpecificKeyOnce = PTHREAD_ONCE_INIT;

typedef struct tAbsThreadData_
{
    int _indentLevel;
} tAbsThreadData;

inline static const char * odBoolToString_(const bool val)
{
    return (val ? "true" : "false");
} // odBoolToString_

static void odReleaseThreadSpecificData_(void * data)
{
    tAbsThreadData * stuff = (tAbsThreadData *) data;
    
    pthread_setspecific(lAbsThreadSpecificKey, NULL);
    free(stuff);
} // odReleaseThreadSpecificData_

static void odSetUpThreadKey_(void)
{
    if (pthread_key_create(&lAbsThreadSpecificKey, odReleaseThreadSpecificData_))
    {
#  if defined(__OBJC__)
        NSLog(@"problem creating thread-specific key => %d", errno);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, "problem creating thread-specific key => %d", errno);
#  endif // ! defined(__OBJC__)
    }
} // odSetUpThreadKey_

inline static tAbsThreadData * odGetThreadData_(void)
{
    tAbsThreadData * stuff;
    
    pthread_once(&lAbsThreadSpecificKeyOnce, odSetUpThreadKey_);
    stuff = (tAbsThreadData *) pthread_getspecific(lAbsThreadSpecificKey);
    if (! stuff)
    {
        stuff = (tAbsThreadData *) malloc(sizeof(tAbsThreadData));
        stuff->_indentLevel = OD_SYSLOG_INIT_VALUE_;
        pthread_setspecific(lAbsThreadSpecificKey, stuff);
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
    tAbsThreadData * stuff = odGetThreadData_();

    return stuff->_indentLevel;
} // odGetIndent_

inline static void odSetIndent_(const int value)
{
    tAbsThreadData * stuff = odGetThreadData_();

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
    
    if (lAbsIncludeProcessID)
    {
        if (lAbsIncludeThreadID)
        {
            length = (2 * lengthHexString) + 3; // '<', ':', '>'
            result = (char *) malloc(length + 1);
            snprintf(result, length, "<%#lX:%#lX>", (long unsigned) getpid(), (long unsigned) pthread_self());
        }
        else
        {
            length = lengthHexString + 2; // '<', '>'
            result = (char *) malloc(length + 1);
            snprintf(result, length, "<%#lX>", (long unsigned) getpid());
        }
    }
    else if (lAbsIncludeThreadID)
    {
        length = lengthHexString + 2; // '<', '>'
        result = (char *) malloc(length + 1);
        snprintf(result, length, "<%#lX>", (long unsigned) pthread_self());
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
    fputs(buffer, lAbsLogFile_);
} // odWriteTime_

EXTERN_C void ODSysLog_(const char * fileName,
                        const char * funcName,
                        const int    lineNumber,
                        const char * text)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lAbsLogFile_)
    {
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " %s\n", OD_FUNC_PREFIX_(rootName), text ? text : "");
        fflush(lAbsLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ " %s", OD_FUNC_PREFIX_(rootName), text ? text : "");
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " %s", OD_FUNC_PREFIX_(rootName), text ? text : "");
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
    if (lAbsLogFile_)
    {
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " %s%s\n", OD_FUNC_PREFIX_(rootName), text1, odBoolToString_(val1));
        fflush(lAbsLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ " %s%s", OD_FUNC_PREFIX_(rootName), text1, odBoolToString_(val1));
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " %s%s", OD_FUNC_PREFIX_(rootName), text1,
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
    if (lAbsLogFile_)
    {
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " %s%s, %s%s\n", OD_FUNC_PREFIX_(rootName), text1,
                odBoolToString_(val1), text2, odBoolToString_(val2));
        fflush(lAbsLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ " %s%s, %s%s", OD_FUNC_PREFIX_(rootName), text1, odBoolToString_(val1), text2,
              odBoolToString_(val2));
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " %s%s, %s%s", OD_FUNC_PREFIX_(rootName), text1,
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
    if (lAbsLogFile_)
    {
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " %s%s, %s%s\n", OD_FUNC_PREFIX_(rootName), text1,
                odBoolToString_(val1), text2, odBoolToString_(val2));
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " %s%s\n", OD_FUNC_PREFIX_(rootName), text3, odBoolToString_(val3));
        fflush(lAbsLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ " %s%s, %s%s", OD_FUNC_PREFIX_(rootName), text1, odBoolToString_(val1), text2,
              odBoolToString_(val2));
        NSLog(@OD_FUNC_FORMAT_ " %s%s", OD_FUNC_PREFIX_(rootName), text3, odBoolToString_(val3));
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " %s%s, %s%s", OD_FUNC_PREFIX_(rootName), text1,
               odBoolToString_(val1), text2, odBoolToString_(val2));
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " %s%s", OD_FUNC_PREFIX_(rootName), text3,
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
    if (lAbsLogFile_)
    {
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " %s%s, %s%s\n", OD_FUNC_PREFIX_(rootName), text1,
                odBoolToString_(val1), text2, odBoolToString_(val2));
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " %s%s, %s%s\n", OD_FUNC_PREFIX_(rootName), text3,
                odBoolToString_(val3), text4, odBoolToString_(val4));
        fflush(lAbsLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ " %s%s, %s%s", OD_FUNC_PREFIX_(rootName), text1, odBoolToString_(val1), text2,
              odBoolToString_(val2));
        NSLog(@OD_FUNC_FORMAT_ " %s%s, %s%s", OD_FUNC_PREFIX_(rootName), text3, odBoolToString_(val3), text4,
              odBoolToString_(val4));
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " %s%s, %s%s", OD_FUNC_PREFIX_(rootName), text1,
               odBoolToString_(val1), text2, odBoolToString_(val2));
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " %s%s, %s%s", OD_FUNC_PREFIX_(rootName), text3,
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
    if (lAbsLogFile_)
    {
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " %s'%c'(%#02X)\n", OD_FUNC_PREFIX_(rootName), text1, val1, val1);
        fflush(lAbsLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ " %s'%c'(%#02X)", OD_FUNC_PREFIX_(rootName), text1, val1, val1);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " %s'%c'(%#02X)", OD_FUNC_PREFIX_(rootName), text1, val1, val1);
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
    if (lAbsLogFile_)
    {
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " %s'%c'(%#02X), %s'%c'(%#02X)\n", OD_FUNC_PREFIX_(rootName), text1,
                val1, val1, text2, val2, val2);
        fflush(lAbsLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ " %s'%c'(%#02X), %s'%c'(%#02X)", OD_FUNC_PREFIX_(rootName), text1, val1, val1, text2,
              val2, val2);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " %s'%c'(%#02X), %s'%c'(%#02X)", OD_FUNC_PREFIX_(rootName),
               text1, val1, val1, text2, val2, val2);
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
    if (lAbsLogFile_)
    {
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " %s'%c'(%#02X), %s'%c'(%#02X)\n", OD_FUNC_PREFIX_(rootName), text1,
                val1, val1, text2, val2, val2);
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " %s'%c'(%#02X)\n", OD_FUNC_PREFIX_(rootName), text3, val3, val3);
        fflush(lAbsLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ " %s'%c'(%#02X), %s'%c'(%#02X)", OD_FUNC_PREFIX_(rootName), text1, val1, val1, text2,
              val2, val2);
        NSLog(@OD_FUNC_FORMAT_ " %s'%c'(%#02X)", OD_FUNC_PREFIX_(rootName), text3, val3, val3);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " %s'%c'(%#02X), %s'%c'(%#02X)", OD_FUNC_PREFIX_(rootName),
               text1, val1, val1, text2, val2, val2);
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " %s'%c'(%#02X)", OD_FUNC_PREFIX_(rootName), text3, val3, val3);
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
    if (lAbsLogFile_)
    {
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " %s'%c'(%#02X), %s'%c'(%#02X)\n", OD_FUNC_PREFIX_(rootName), text1,
                val1, val1, text2, val2, val2);
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " %s'%c'(%#02X), %s'%c'(%#02X)\n", OD_FUNC_PREFIX_(rootName), text3,
                val3, val3, text4, val4, val4);
        fflush(lAbsLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ " %s'%c'(%#02X), %s'%c'(%#02X)", OD_FUNC_PREFIX_(rootName), text1, val1, val1, text2,
              val2, val2);
        NSLog(@OD_FUNC_FORMAT_ " %s'%c'(%#02X), %s'%c'(%#02X)", OD_FUNC_PREFIX_(rootName), text3, val3, val3, text4,
              val4, val4);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " %s'%c'(%#02X), %s'%c'(%#02X)", OD_FUNC_PREFIX_(rootName),
               text1, val1, val1, text2, val2, val2);
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " %s'%c'(%#02X), %s'%c'(%#02X)", OD_FUNC_PREFIX_(rootName),
               text3, val3, val3, text4, val4, val4);
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
    if (lAbsLogFile_)
    {
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " %s%g\n", OD_FUNC_PREFIX_(rootName), text1, val1);
        fflush(lAbsLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ " %s%g", OD_FUNC_PREFIX_(rootName), text1, val1);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " %s%g", OD_FUNC_PREFIX_(rootName), text1, val1);
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
    if (lAbsLogFile_)
    {
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " %s%g, %s%g\n", OD_FUNC_PREFIX_(rootName), text1, val1, text2, val2);
        fflush(lAbsLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ " %s%g, %s%g", OD_FUNC_PREFIX_(rootName), text1, val1, text2, val2);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " %s%g, %s%g", OD_FUNC_PREFIX_(rootName), text1, val1, text2,
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
    if (lAbsLogFile_)
    {
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " %s%g, %s%g\n", OD_FUNC_PREFIX_(rootName), text1, val1, text2, val2);
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " %s%g\n", OD_FUNC_PREFIX_(rootName), text3, val3);
        fflush(lAbsLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ " %s%g, %s%g", OD_FUNC_PREFIX_(rootName), text1, val1, text2, val2);
        NSLog(@OD_FUNC_FORMAT_ " %s%g", OD_FUNC_PREFIX_(rootName), text3, val3);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " %s%g, %s%g", OD_FUNC_PREFIX_(rootName), text1, val1, text2,
               val2);
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " %s%g", OD_FUNC_PREFIX_(rootName), text3, val3);
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
    if (lAbsLogFile_)
    {
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " %s%g, %s%g\n", OD_FUNC_PREFIX_(rootName), text1, val1, text2, val2);
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " %s%g, %s%g\n", OD_FUNC_PREFIX_(rootName), text3, val3, text4, val4);
        fflush(lAbsLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ " %s%g, %s%g", OD_FUNC_PREFIX_(rootName), text1, val1, text2, val2);
        NSLog(@OD_FUNC_FORMAT_ " %s%g, %s%g", OD_FUNC_PREFIX_(rootName), text3, val3, text4, val4);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " %s%g, %s%g", OD_FUNC_PREFIX_(rootName), text1, val1, text2,
               val2);
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " %s%g, %s%g", OD_FUNC_PREFIX_(rootName), text3, val3, text4,
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
    if (lAbsLogFile_)
    {
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " enter\n", OD_FUNC_PREFIX_(rootName));
        fflush(lAbsLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ " enter", OD_FUNC_PREFIX_(rootName));
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " enter", OD_FUNC_PREFIX_(rootName));
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
    if (lAbsLogFile_)
    {
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " exit\n", OD_FUNC_PREFIX_(rootName));
        fflush(lAbsLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ " exit", OD_FUNC_PREFIX_(rootName));
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " exit", OD_FUNC_PREFIX_(rootName));
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
    if (lAbsLogFile_)
    {
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " exit -> %s\n", OD_FUNC_PREFIX_(rootName), odBoolToString_(val));
        fflush(lAbsLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ " exit -> %s", OD_FUNC_PREFIX_(rootName), odBoolToString_(val));
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " exit -> %s", OD_FUNC_PREFIX_(rootName),
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
    if (lAbsLogFile_)
    {
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " exit -> '%c'(%#02X)\n", OD_FUNC_PREFIX_(rootName), val, val);
        fflush(lAbsLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ " exit -> '%c'(%#02X)", OD_FUNC_PREFIX_(rootName), val, val);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " exit -> '%c'(%#02X)", OD_FUNC_PREFIX_(rootName), val, val);
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
    if (lAbsLogFile_)
    {
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " exit -> %g\n", OD_FUNC_PREFIX_(rootName), val);
        fflush(lAbsLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ " exit -> %g", OD_FUNC_PREFIX_(rootName), val);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " exit -> %g", OD_FUNC_PREFIX_(rootName), val);
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
    if (lAbsLogFile_)
    {
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " call exit(%ld(%#lx))\n", OD_FUNC_PREFIX_(rootName), val, val);
        fflush(lAbsLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ " call exit(%ld(%#lx))", OD_FUNC_PREFIX_(rootName), val, val);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " call exit(%ld(%#lx))", OD_FUNC_PREFIX_(rootName), val, val);
#  endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODSysLogExitExit_

EXTERN_C void ODSysLogExitL_(const char * fileName,
                             const char * funcName,
                             const int    lineNumber,
                             const long   val)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    odDecreaseIndent_();
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lAbsLogFile_)
    {
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " exit -> %ld(%#lX)\n", OD_FUNC_PREFIX_(rootName), val, val);
        fflush(lAbsLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ " exit -> %ld(%#lX)", OD_FUNC_PREFIX_(rootName), val, val);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " exit -> %ld(%#lX)", OD_FUNC_PREFIX_(rootName), val, val);
#  endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODSysLogExitL_

EXTERN_C void ODSysLogExitLL_(const char *    fileName,
                              const char *    funcName,
                              const int       lineNumber,
                              const long long val)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    odDecreaseIndent_();
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lAbsLogFile_)
    {
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " exit -> %lld(%#llX)\n", OD_FUNC_PREFIX_(rootName), val, val);
        fflush(lAbsLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ " exit -> %lld(%#llX)", OD_FUNC_PREFIX_(rootName), val, val);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " exit -> %lld(%#llX)", OD_FUNC_PREFIX_(rootName), val, val);
#  endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODSysLogExitLL_

# if defined(__OBJC__)
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
    if (lAbsLogFile_)
    {
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " exit -> %s\n", OD_FUNC_PREFIX_(rootName), valString);
        fflush(lAbsLogFile_);
    }
    else
    {
        NSLog(@OD_FUNC_FORMAT_ " exit -> %s", OD_FUNC_PREFIX_(rootName), valString);
    }
    OD_FREE_PREFIX_();
	OD_FREE_INDENT_();
} // ODSysLogExitO_
# endif // defined(__OBJC__)

EXTERN_C void ODSysLogExitP_(const char * fileName,
                             const char * funcName,
                             const int    lineNumber,
                             const void * val)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    odDecreaseIndent_();
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lAbsLogFile_)
    {
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " exit -> %p\n", OD_FUNC_PREFIX_(rootName), val);
        fflush(lAbsLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ " exit -> %p", OD_FUNC_PREFIX_(rootName), val);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " exit -> %p", OD_FUNC_PREFIX_(rootName), val);
#  endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODSysLogExitP_

#  if defined(__MAC__)
EXTERN_C void ODSysLogExitR_(const char * fileName,
                             const char * funcName,
                             const int    lineNumber,
                             const CGRect val)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    odDecreaseIndent_();
	OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lAbsLogFile_)
    {
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " exit -> [l: %g, t: %g, h: %g, w: %g]\n", OD_FUNC_PREFIX_(rootName),
                val.origin.x, val.origin.y, val.size.height, val.size.width);
        fflush(lAbsLogFile_);
    }
    else
    {
#   if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ " exit -> [l: %g, t: %g, h: %g, w: %g]", OD_FUNC_PREFIX_(rootName), val.origin.x,
              val.origin.y, val.size.height, val.size.width);
#   else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " exit -> [l: %g, t: %g, h: %g, w: %g]", OD_FUNC_PREFIX_(rootName),
               val.origin.x, val.origin.y, val.size.height, val.size.width);
#   endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
	OD_FREE_INDENT_();
} // ODSysLogExitR_
#  endif // defined(__MAC__)

EXTERN_C void ODSysLogExitS_(const char * fileName,
                             const char * funcName,
                             const int    lineNumber,
                             const char * val)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    odDecreaseIndent_();
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lAbsLogFile_)
    {
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " exit -> '%s'\n", OD_FUNC_PREFIX_(rootName), val ? val : "<>");
        fflush(lAbsLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ " exit -> '%s'", OD_FUNC_PREFIX_(rootName), val ? val : "<>");
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " exit -> '%s'", OD_FUNC_PREFIX_(rootName), val ? val : "<>");
#  endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODSysLogExitS_

EXTERN_C void ODSysLogExitThrowL_(const char * fileName,
                                  const char * funcName,
                                  const int    lineNumber,
                                  const long   val)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    odDecreaseIndent_();
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lAbsLogFile_)
    {
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " throw(%ld(%#lx))\n", OD_FUNC_PREFIX_(rootName), val, val);
        fflush(lAbsLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ " throw(%ld(%#lx))", OD_FUNC_PREFIX_(rootName), val, val);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " throw(%ld(%#lx))", OD_FUNC_PREFIX_(rootName), val, val);
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
    if (lAbsLogFile_)
    {
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " throw(%s)\n", OD_FUNC_PREFIX_(rootName), val ? val : "<>");
        fflush(lAbsLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ " throw(%s)", OD_FUNC_PREFIX_(rootName), val ? val : "<>");
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " throw(%s)", OD_FUNC_PREFIX_(rootName), val ? val : "<>");
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
    bool         odWriteToFile = (options & kODSyslogOptionWriteToFile);
	const char * rootName = odFileNameRoot_(fileName);
    const char * suffixString;
	char *       stars;
    size_t       starsLength = strlen(rootName) + strlen(funcName) + (sizeof(OD_INIT_FORMAT_) - 1) - 8; // 4 %s
    
    lAbsIncludeProcessID = (options & kODSyslogOptionIncludeProcessID);
    lAbsIncludeThreadID = (options & kODSyslogOptionIncludeThreadID);
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
        lAbsLogFile_ = fopen(pidString, "w");
        if (lAbsLogFile_)
        {
            odWriteTime_();
            fprintf(lAbsLogFile_, "%s\n", stars);
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
    if (lAbsLogFile_)
    {
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_INIT_FORMAT_ "\n", prefixString_, suffixString, funcName, rootName, lineNumber);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_INIT_FORMAT_, prefixString_, suffixString, funcName, rootName, lineNumber);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_INIT_FORMAT_, prefixString_, suffixString, funcName, rootName, lineNumber);
#  endif // ! defined(__OBJC__)
    }
    if (lAbsLogFile_)
    {
        odWriteTime_();
        fprintf(lAbsLogFile_, "%s\n", stars);
        fflush(lAbsLogFile_);
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

EXTERN_C void ODSysLogIP_(const char * fileName,
                          const char * funcName,
                          const int    lineNumber,
                          const char * text1,
                          const long   val1,
                          const long   val2)
{
    const char * rootName = odFileNameRoot_(fileName);
    union IP_TYPE_
    {
        uint8_t asBytes[4];
        int32_t asInt32;
    } address;
    
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    address.asInt32 = (int32_t) val1;
    if (lAbsLogFile_)
    {
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " %s%d.%d.%d.%d:%ld\n", OD_FUNC_PREFIX_(rootName), text1,
                address.asBytes[0], address.asBytes[1], address.asBytes[2], address.asBytes[3], val2);
        fflush(lAbsLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ " %s%d.%d.%d.%d:%ld", OD_FUNC_PREFIX_(rootName), text1, address.asBytes[0],
              address.asBytes[1], address.asBytes[2], address.asBytes[3], val2);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " %s%d.%d.%d.%d:%ld", OD_FUNC_PREFIX_(rootName), text1,
               address.asBytes[0], address.asBytes[1], address.asBytes[2], address.asBytes[3], val2);
#  endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODSysLogIP_

EXTERN_C void ODSysLogL1_(const char * fileName,
                          const char * funcName,
                          const int    lineNumber,
                          const char * text1,
                          const long   val1)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lAbsLogFile_)
    {
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " %s%ld(%#lX)\n", OD_FUNC_PREFIX_(rootName), text1, val1, val1);
        fflush(lAbsLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ " %s%ld(%#lX)", OD_FUNC_PREFIX_(rootName), text1, val1, val1);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " %s%ld(%#lX)", OD_FUNC_PREFIX_(rootName), text1, val1, val1);
#  endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODSysLogL1_

EXTERN_C void ODSysLogL2_(const char * fileName,
                          const char * funcName,
                          const int    lineNumber,
                          const char * text1,
                          const long   val1,
                          const char * text2,
                          const long   val2)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lAbsLogFile_)
    {
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " %s%ld(%#lX), %s%ld(%#lX)\n", OD_FUNC_PREFIX_(rootName), text1, val1,
                val1, text2, val2, val2);
        fflush(lAbsLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ " %s%ld(%#lX), %s%ld(%#lX)", OD_FUNC_PREFIX_(rootName), text1, val1, val1, text2, val2,
              val2);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " %s%ld(%#lX), %s%ld(%#lX)", OD_FUNC_PREFIX_(rootName), text1,
               val1, val1, text2, val2, val2);
#  endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODSysLogL2_

EXTERN_C void ODSysLogL3_(const char * fileName,
                          const char * funcName,
                          const int    lineNumber,
                          const char * text1,
                          const long   val1,
                          const char * text2,
                          const long   val2,
                          const char * text3,
                          const long   val3)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lAbsLogFile_)
    {
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " %s%ld(%#lX), %s%ld(%#lX)\n", OD_FUNC_PREFIX_(rootName), text1, val1,
                val1, text2, val2, val2);
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " %s%ld(%#lX)\n", OD_FUNC_PREFIX_(rootName), text3, val3, val3);
        fflush(lAbsLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ " %s%ld(%#lX), %s%ld(%#lX)", OD_FUNC_PREFIX_(rootName), text1, val1, val1, text2, val2,
              val2);
        NSLog(@OD_FUNC_FORMAT_ " %s%ld(%#lX)", OD_FUNC_PREFIX_(rootName), text3, val3, val3);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " %s%ld(%#lX), %s%ld(%#lX)", OD_FUNC_PREFIX_(rootName), text1,
               val1, val1, text2, val2, val2);
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " %s%ld(%#lX)", OD_FUNC_PREFIX_(rootName), text3, val3, val3);
#  endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODSysLogL3_

EXTERN_C void ODSysLogL4_(const char * fileName,
                          const char * funcName,
                          const int    lineNumber,
                          const char * text1,
                          const long   val1,
                          const char * text2,
                          const long   val2,
                          const char * text3,
                          const long   val3,
                          const char * text4,
                          const long   val4)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lAbsLogFile_)
    {
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " %s%ld(%#lX), %s%ld(%#lX)\n", OD_FUNC_PREFIX_(rootName), text1, val1,
                val1, text2, val2, val2);
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " %s%ld(%#lX), %s%ld(%#lX)\n", OD_FUNC_PREFIX_(rootName), text3, val3,
                val3, text4, val4, val4);
        fflush(lAbsLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ " %s%ld(%#lX), %s%ld(%#lX)", OD_FUNC_PREFIX_(rootName), text1, val1, val1, text2, val2,
              val2);
        NSLog(@OD_FUNC_FORMAT_ " %s%ld(%#lX), %s%ld(%#lX)", OD_FUNC_PREFIX_(rootName), text3, val3, val3, text4, val4,
              val4);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " %s%ld(%#lX), %s%ld(%#lX)", OD_FUNC_PREFIX_(rootName), text1,
               val1, val1, text2, val2, val2);
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " %s%ld(%#lX), %s%ld(%#lX)", OD_FUNC_PREFIX_(rootName), text3,
               val3, val3, text4, val4, val4);
#  endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODSysLogL4_

EXTERN_C void ODSysLogLL1_(const char *    fileName,
                           const char *    funcName,
                           const int       lineNumber,
                           const char *    text1,
                           const long long val1)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lAbsLogFile_)
    {
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " %s%lld(%#llX)\n", OD_FUNC_PREFIX_(rootName), text1, val1, val1);
        fflush(lAbsLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ " %s%lld(%#llX)", OD_FUNC_PREFIX_(rootName), text1, val1, val1);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " %s%lld(%#llX)", OD_FUNC_PREFIX_(rootName), text1, val1, val1);
#  endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODSysLogLL1_

EXTERN_C void ODSysLogLL2_(const char *    fileName,
                           const char *    funcName,
                           const int       lineNumber,
                           const char *    text1,
                           const long long val1,
                           const char *    text2,
                           const long long val2)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lAbsLogFile_)
    {
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " %s%lld(%#llX), %s%lld(%#llX)\n", OD_FUNC_PREFIX_(rootName), text1,
                val1, val1, text2, val2, val2);
        fflush(lAbsLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ " %s%lld(%#llX), %s%lld(%#llX)", OD_FUNC_PREFIX_(rootName), text1, val1, val1, text2,
              val2, val2);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " %s%lld(%#llX), %s%lld(%#llX)", OD_FUNC_PREFIX_(rootName),
               text1, val1, val1, text2, val2, val2);
#  endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODSysLogLL2_

EXTERN_C void ODSysLogLL3_(const char *    fileName,
                           const char *    funcName,
                           const int       lineNumber,
                           const char *    text1,
                           const long long val1,
                           const char *    text2,
                           const long long val2,
                           const char *    text3,
                           const long long val3)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lAbsLogFile_)
    {
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " %s%lld(%#llX), %s%lld(%#llX)\n", OD_FUNC_PREFIX_(rootName), text1,
                val1, val1, text2, val2, val2);
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " %s%lld(%#llX)\n", OD_FUNC_PREFIX_(rootName), text3, val3, val3);
        fflush(lAbsLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ " %s%lld(%#llX), %s%lld(%#llX)", OD_FUNC_PREFIX_(rootName), text1, val1, val1, text2,
              val2, val2);
        NSLog(@OD_FUNC_FORMAT_ " %s%lld(%#llX)", OD_FUNC_PREFIX_(rootName), text3, val3, val3);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " %s%lld(%#llX), %s%lld(%#llX)", OD_FUNC_PREFIX_(rootName),
               text1, val1, val1, text2, val2, val2);
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " %s%lld(%#llX)", OD_FUNC_PREFIX_(rootName), text3, val3, val3);
#  endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODSysLogLL3_

EXTERN_C void ODSysLogLL4_(const char *    fileName,
                           const char *    funcName,
                           const int       lineNumber,
                           const char *    text1,
                           const long long val1,
                           const char *    text2,
                           const long long val2,
                           const char *    text3,
                           const long long val3,
                           const char *    text4,
                           const long long val4)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lAbsLogFile_)
    {
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " %s%lld(%#llX), %s%lld(%#llX)\n", OD_FUNC_PREFIX_(rootName), text1,
                val1, val1, text2, val2, val2);
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " %s%lld(%#llX), %s%lld(%#llX)\n", OD_FUNC_PREFIX_(rootName), text3,
                val3, val3, text4, val4, val4);
        fflush(lAbsLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ " %s%lld(%#llX), %s%lld(%#llX)", OD_FUNC_PREFIX_(rootName), text1, val1, val1, text2,
              val2, val2);
        NSLog(@OD_FUNC_FORMAT_ " %s%lld(%#llX), %s%lld(%#llX)", OD_FUNC_PREFIX_(rootName), text3, val3, val3, text4,
              val4, val4);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " %s%lld(%#llX), %s%lld(%#llX)", OD_FUNC_PREFIX_(rootName),
               text1, val1, val1, text2, val2, val2);
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " %s%lld(%#llX), %s%lld(%#llX)", OD_FUNC_PREFIX_(rootName),
               text3, val3, val3, text4, val4, val4);
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
                if (lAbsLogFile_)
                {
                    odWriteTime_();
                    fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " %s | %s |\n", OD_FUNC_PREFIX_(rootName), heading,
                            lineBuffer);
                }
                else
                {
#  if defined(__OBJC__)
                    NSLog(@OD_FUNC_FORMAT_ " %s | %s |", OD_FUNC_PREFIX_(rootName), heading, lineBuffer);
#  else // ! defined(__OBJC__)
                    syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " %s | %s |", OD_FUNC_PREFIX_(rootName), heading,
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
            if (lAbsLogFile_)
            {
                odWriteTime_();
                fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " %s | %s |\n", OD_FUNC_PREFIX_(rootName), heading, lineBuffer);
            }
            else
            {
#  if defined(__OBJC__)
                NSLog(@OD_FUNC_FORMAT_ " %s | %s |", OD_FUNC_PREFIX_(rootName), heading, lineBuffer);
#  else // ! defined(__OBJC__)
                syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " %s | %s |", OD_FUNC_PREFIX_(rootName), heading,
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
        if (lAbsLogFile_)
        {
            odWriteTime_();
            fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " %s | %s |\n", OD_FUNC_PREFIX_(rootName), heading, lineBuffer);
        }
        else
        {
#  if defined(__OBJC__)
            NSLog(@OD_FUNC_FORMAT_ " %s | %s |", OD_FUNC_PREFIX_(rootName), heading, lineBuffer);
#  else // ! defined(__OBJC__)
            syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " %s | %s |", OD_FUNC_PREFIX_(rootName), heading,
                   lineBuffer);
#  endif // ! defined(__OBJC__)
        }
    }
    if (lAbsLogFile_)
    {
        fflush(lAbsLogFile_);
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
    if (lAbsLogFile_)
    {
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " %s%s\n", OD_FUNC_PREFIX_(rootName), text1, obj1String);
        fflush(lAbsLogFile_);
    }
    else
    {
        NSLog(@OD_FUNC_FORMAT_ " %s%s", OD_FUNC_PREFIX_(rootName), text1, obj1String);
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
    if (lAbsLogFile_)
    {
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " %s%sX, %s%s\n", OD_FUNC_PREFIX_(rootName), text1, obj1String, text2,
                obj2String);
        fflush(lAbsLogFile_);
    }
    else
    {
        NSLog(@OD_FUNC_FORMAT_ " %s%s, %s%s", OD_FUNC_PREFIX_(rootName), text1, obj1String, text2, obj2String);
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
    if (lAbsLogFile_)
    {
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " %s%s, %s%s\n", OD_FUNC_PREFIX_(rootName), text1, obj1String, text2,
                obj2String);
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " %s%s\n", OD_FUNC_PREFIX_(rootName), text3, obj3String);
        fflush(lAbsLogFile_);
    }
    else
    {
        NSLog(@OD_FUNC_FORMAT_ " %s%s, %s%s", OD_FUNC_PREFIX_(rootName), text1, obj1String, text2, obj2String);
        NSLog(@OD_FUNC_FORMAT_ " %s%s", OD_FUNC_PREFIX_(rootName), text3, obj3String);
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
    if (lAbsLogFile_)
    {
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " %s%s, %s%s\n", OD_FUNC_PREFIX_(rootName), text1, obj1String, text2,
                obj2String);
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " %s%s, %s%s\n", OD_FUNC_PREFIX_(rootName), text3, obj3String, text4,
                obj4String);
        fflush(lAbsLogFile_);
    }
    else
    {
        NSLog(@OD_FUNC_FORMAT_ " %s%s, %s%s", OD_FUNC_PREFIX_(rootName), text1, obj1String, text2, obj2String);
        NSLog(@OD_FUNC_FORMAT_ " %s%s, %s%s", OD_FUNC_PREFIX_(rootName), text3, obj3String, text4, obj4String);
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
    if (lAbsLogFile_)
    {
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " %s%p\n", OD_FUNC_PREFIX_(rootName), text1, ptr1);
        fflush(lAbsLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ " %s%p", OD_FUNC_PREFIX_(rootName), text1, ptr1);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " %s%p", OD_FUNC_PREFIX_(rootName), text1, ptr1);
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
    if (lAbsLogFile_)
    {
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " %s%p, %s%p\n", OD_FUNC_PREFIX_(rootName), text1, ptr1, text2, ptr2);
        fflush(lAbsLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ " %s%p, %s%p", OD_FUNC_PREFIX_(rootName), text1, ptr1, text2, ptr2);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " %s%p, %s%p", OD_FUNC_PREFIX_(rootName), text1, ptr1, text2,
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
    if (lAbsLogFile_)
    {
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " %s%p, %s%p\n", OD_FUNC_PREFIX_(rootName), text1, ptr1, text2, ptr2);
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " %s%p\n", OD_FUNC_PREFIX_(rootName), text3, ptr3);
        fflush(lAbsLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ " %s%p, %s%p", OD_FUNC_PREFIX_(rootName), text1, ptr1, text2, ptr2);
        NSLog(@OD_FUNC_FORMAT_ " %s%p", OD_FUNC_PREFIX_(rootName), text3, ptr3);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " %s%p, %s%p", OD_FUNC_PREFIX_(rootName), text1, ptr1, text2,
               ptr2);
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " %s%p", OD_FUNC_PREFIX_(rootName), text3, ptr3);
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
    if (lAbsLogFile_)
    {
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " %s%p, %s%p\n", OD_FUNC_PREFIX_(rootName), text1, ptr1, text2, ptr2);
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " %s%p, %s%p\n", OD_FUNC_PREFIX_(rootName), text3, ptr3, text4, ptr4);
        fflush(lAbsLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ " %s%p, %s%p", OD_FUNC_PREFIX_(rootName), text1, ptr1, text2, ptr2);
        NSLog(@OD_FUNC_FORMAT_ " %s%p, %s%p", OD_FUNC_PREFIX_(rootName), text3, ptr3, text4, ptr4);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " %s%p, %s%p", OD_FUNC_PREFIX_(rootName), text1, ptr1, text2,
               ptr2);
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " %s%p, %s%p", OD_FUNC_PREFIX_(rootName), text3, ptr3, text4,
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
    if (lAbsLogFile_)
    {
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " address = %p, size = %d\n", OD_FUNC_PREFIX_(rootName), buffer, size);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ " address = %p, size = %d", OD_FUNC_PREFIX_(rootName), buffer, size);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " address = %p, size = %d", OD_FUNC_PREFIX_(rootName), buffer,
               size);
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
        if (lAbsLogFile_)
        {
            odWriteTime_();
            fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " %s | %04X : %s| %s\n", OD_FUNC_PREFIX_(rootName), heading, ii,
                    lineBuffer, charBuffer);
        }
        else
        {
#  if defined(__OBJC__)
            NSLog(@OD_FUNC_FORMAT_ " %s | %04X : %s| %s", OD_FUNC_PREFIX_(rootName), heading, ii, lineBuffer,
                  charBuffer);
#  else // ! defined(__OBJC__)
            syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " %s | %04X : %s| %s", OD_FUNC_PREFIX_(rootName), heading,
                   ii, lineBuffer, charBuffer);
#  endif // ! defined(__OBJC__)
        }
        memset(lineBuffer, ' ', sizeof(lineBuffer) - 1);
        heading = blankCaption;
    }
    if (lAbsLogFile_)
    {
        fflush(lAbsLogFile_);
    }
    free(blankCaption);
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODSysLogPacket_

#  if defined(__MAC__)
EXTERN_C void ODSysLogRect_(const char * fileName,
                            const char * funcName,
                            const int    lineNumber,
                            const char * caption,
                            const CGRect rect)
{
    const char * rootName = odFileNameRoot_(fileName);
    
	OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lAbsLogFile_)
    {
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " %s[l: %g, t: %g, h: %g, w: %g]\n", OD_FUNC_PREFIX_(rootName), caption,
                rect.origin.x, rect.origin.y, rect.size.height, rect.size.width);
        fflush(lAbsLogFile_);
    }
    else
    {
#   if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ " %s[l: %g, t: %g, h: %g, w: %g]", OD_FUNC_PREFIX_(rootName), caption, rect.origin.x,
              rect.origin.y, rect.size.height, rect.size.width);
#   else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " %s[l: %g, t: %g, h: %g, w: %g]", OD_FUNC_PREFIX_(rootName),
               caption, rect.origin.x, rect.origin.y, rect.size.height, rect.size.width);
#   endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
	OD_FREE_INDENT_();
} // ODSysLogRect_
#  endif // defined(__MAC__)

EXTERN_C void ODSysLogS1_(const char * fileName,
                          const char * funcName,
                          const int    lineNumber,
                          const char * text1,
                          const char * val1)
{
    const char * rootName = odFileNameRoot_(fileName);
    
    OD_CREATE_INDENT_();
    OD_CREATE_PREFIX_();
    if (lAbsLogFile_)
    {
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " %s'%s'\n", OD_FUNC_PREFIX_(rootName), text1, val1 ? val1 : "<>");
        fflush(lAbsLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ " %s'%s'", OD_FUNC_PREFIX_(rootName), text1, val1 ? val1 : "<>");
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " %s'%s'", OD_FUNC_PREFIX_(rootName), text1,
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
    if (lAbsLogFile_)
    {
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " %s'%s', %s'%s'\n", OD_FUNC_PREFIX_(rootName), text1,
                val1 ? val1 : "<>", text2, val2 ? val2 : "<>");
        fflush(lAbsLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ " %s'%s', %s'%s'", OD_FUNC_PREFIX_(rootName), text1, val1 ? val1 : "<>", text2,
              val2 ? val2 : "<>");
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " %s'%s', %s'%s'", OD_FUNC_PREFIX_(rootName), text1,
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
    if (lAbsLogFile_)
    {
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " %s'%s', %s'%s'\n", OD_FUNC_PREFIX_(rootName), text1,
                val1 ? val1 : "<>", text2, val2 ? val2 : "<>");
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " %s'%s'\n", OD_FUNC_PREFIX_(rootName), text3, val3 ? val3 : "<>");
        fflush(lAbsLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ " %s'%s', %s'%s'", OD_FUNC_PREFIX_(rootName), text1, val1 ? val1 : "<>", text2,
              val2 ? val2 : "<>");
        NSLog(@OD_FUNC_FORMAT_ " %s'%s'", OD_FUNC_PREFIX_(rootName), text3, val3 ? val3 : "<>");
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " %s'%s', %s'%s'", OD_FUNC_PREFIX_(rootName), text1,
               val1 ? val1 : "<>", text2, val2 ? val2 : "<>");
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " %s'%s'", OD_FUNC_PREFIX_(rootName), text3,
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
    if (lAbsLogFile_)
    {
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " %s'%s', %s'%s'\n", OD_FUNC_PREFIX_(rootName), text1,
                val1 ? val1 : "<>", text2, val2 ? val2 : "<>");
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " %s'%s', %s'%s'\n", OD_FUNC_PREFIX_(rootName), text3,
                val3 ? val3 : "<>", text4, val4 ? val4 : "<>");
        fflush(lAbsLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ " %s'%s', %s'%s'", OD_FUNC_PREFIX_(rootName), text1, val1 ? val1 : "<>", text2,
              val2 ? val2 : "<>");
        NSLog(@OD_FUNC_FORMAT_ " %s'%s', %s'%s'", OD_FUNC_PREFIX_(rootName), text3, val3 ? val3 : "<>", text4,
              val4 ? val4 : "<>");
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " %s'%s', %s'%s'", OD_FUNC_PREFIX_(rootName), text1,
               val1 ? val1 : "<>", text2, val2 ? val2 : "<>");
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " %s'%s', %s'%s'", OD_FUNC_PREFIX_(rootName), text3,
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
    if (lAbsLogFile_)
    {
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " %s'%.*s'\n", OD_FUNC_PREFIX_(rootName), text, len, val ? val : "");
        fflush(lAbsLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ " %s'%.*s'", OD_FUNC_PREFIX_(rootName), text, len, val ? val : "");
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " %s'%.*s'", OD_FUNC_PREFIX_(rootName), text, len,
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
    if (lAbsLogFile_)
    {
        odWriteTime_();
        fprintf(lAbsLogFile_, OD_FUNC_FORMAT_ " %s%ld:%ld\n", OD_FUNC_PREFIX_(rootName), text1, val1->tv_sec,
                (long) val1->tv_usec);
        fflush(lAbsLogFile_);
    }
    else
    {
#  if defined(__OBJC__)
        NSLog(@OD_FUNC_FORMAT_ " %s%ld:%ld", OD_FUNC_PREFIX_(rootName), text1, val1->tv_sec, (long) val1->tv_usec);
#  else // ! defined(__OBJC__)
        syslog(OD_SYSLOG_LOG_LEVEL_, OD_FUNC_FORMAT_ " %s%ld:%ld", OD_FUNC_PREFIX_(rootName), text1, val1->tv_sec,
               (long) val1->tv_usec);
#  endif // ! defined(__OBJC__)
    }
    OD_FREE_PREFIX_();
    OD_FREE_INDENT_();
} // ODSysLogTi_

# endif // defined(ENABLE_OD_SYSLOG)
#endif // ! defined(ODSYSLOG_INC_)
