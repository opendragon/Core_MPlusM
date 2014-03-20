//--------------------------------------------------------------------------------------
//
//  File:       ODSyslog.h
//
//  Project:    YarpPlusPlus
//
//  Contains:   The function declarations and macro definitions for the debugging facility.
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

#if (! defined(ODSYSLOG_H_))
# define ODSYSLOG_H_ /* */

# if defined(__OBJC__)
#  import <Foundation/Foundation.h>
# endif // defined(__OBJC__)
# if defined(__APPLE__)
#  include <CoreGraphics/CGGeometry.h>
# endif // defined(__APPLE__)

enum
{
    kODSyslogOptionNone = 0,
    kODSyslogOptionWriteToFile = 1,
    kODSyslogOptionIncludeProcessID = 2,
    kODSyslogOptionIncludeThreadID = 4,
    kODSyslogOptionEnableThreadSupport = 8,
    kODSyslogOptionWriteToStderr = 16
};

# if defined(DISABLE_OD_SYSLOG)
#  undef ENABLE_OD_SYSLOG
# endif // defined(DISABLE_OD_SYSLOG)

# if defined(ENABLE_OD_SYSLOG)
#  if defined(__OBJC__)
#   define OD_PRINTABLE_STRING(xx)   (xx ? [[xx description] UTF8String] : "<>")
#  endif // defined(__OBJC__)
#  define OD_SYSLOG(text) \
        ODSysLog_(__FILE__, __func__,  __LINE__, text)
#  define OD_SYSLOG_B1(text1, val1)  \
        ODSysLogB1_(__FILE__, __func__, __LINE__, text1, (long) (val1))
#  define OD_SYSLOG_B2(text1, val1, text2, val2)  \
        ODSysLogB2_(__FILE__, __func__, __LINE__, text1, (long) (val1), text2, (long) (val2))
#  define OD_SYSLOG_B3(text1, val1, text2, val2, text3, val3)  \
        ODSysLogB3_(__FILE__, __func__, __LINE__, text1, (long) (val1), text2, (long) (val2), text3, (long) (val3))
#  define OD_SYSLOG_B4(text1, val1, text2, val2, text3, val3, text4, val4)  \
        ODSysLogB4_(__FILE__, __func__, __LINE__, text1, (long) (val1), text2, (long) (val2), text3, (long) (val3),\
                    text4, (long) (val4))
#  define OD_SYSLOG_C1(text1, val1)  \
        ODSysLogC1_(__FILE__, __func__, __LINE__, text1, (char) (val1))
#  define OD_SYSLOG_C2(text1, val1, text2, val2)  \
        ODSysLogC2_(__FILE__, __func__, __LINE__, text1, (char) (val1), text2, (char) (val2))
#  define OD_SYSLOG_C3(text1, val1, text2, val2, text3, val3)  \
        ODSysLogC3_(__FILE__, __func__, __LINE__, text1, (char) (val1), text2, (char) (val2), text3, (char) (val3))
#  define OD_SYSLOG_C4(text1, val1, text2, val2, text3, val3, text4, val4)  \
        ODSysLogC4_(__FILE__, __func__, __LINE__, text1, (char) (val1), text2, (char) (val2), text3, (char) (val3),\
                    text4, (char) (val4))
#  define OD_SYSLOG_D1(text1, val1)  \
        ODSysLogD1_(__FILE__, __func__, __LINE__, text1, val1)
#  define OD_SYSLOG_D2(text1, val1, text2, val2)  \
        ODSysLogD2_(__FILE__, __func__, __LINE__, text1, val1, text2, val2)
#  define OD_SYSLOG_D3(text1, val1, text2, val2, text3, val3)  \
        ODSysLogD3_(__FILE__, __func__, __LINE__, text1, val1, text2, val2, text3, val3)
#  define OD_SYSLOG_D4(text1, val1, text2, val2, text3, val3, text4, val4)  \
        ODSysLogD4_(__FILE__, __func__, __LINE__, text1, val1, text2, val2, text3, val3, text4, val4)
#  define OD_SYSLOG_ENTER()  \
        ODSysLogEnter_(__FILE__, __func__, __LINE__)
#  define OD_SYSLOG_EXIT()  \
        ODSysLogExit_(__FILE__, __func__, __LINE__)
#  define OD_SYSLOG_EXIT_B(val) \
        ODSysLogExitB_(__FILE__, __func__, __LINE__, val)
#  define OD_SYSLOG_EXIT_C(val) \
        ODSysLogExitC_(__FILE__, __func__, __LINE__, val)
#  define OD_SYSLOG_EXIT_D(val) \
        ODSysLogExitD_(__FILE__, __func__, __LINE__, val)
#  define OD_SYSLOG_EXIT_EXIT(val) \
        ODSysLogExitExit_(__FILE__, __func__, __LINE__, val)
#  define OD_SYSLOG_EXIT_L(val) \
        ODSysLogExitL_(__FILE__, __func__, __LINE__, val)
#  define OD_SYSLOG_EXIT_LL(val) \
        ODSysLogExitLL_(__FILE__, __func__, __LINE__, val)
#  if defined(__OBJC__)
#   define OD_SYSLOG_EXIT_O(val) \
        ODSysLogExitO_(__FILE__, __func__, __LINE__, val)
#  endif // defined(__OBJC__)
#  define OD_SYSLOG_EXIT_P(val) \
        ODSysLogExitP_(__FILE__, __func__, __LINE__, val)
#  if defined(__APPLE__)
#   define OD_SYSLOG_EXIT_R(val) \
        ODSysLogExitR_(__FILE__, __func__, __LINE__, val)
#  endif // defined(__APPLE__)
#  define OD_SYSLOG_EXIT_S(val) \
        ODSysLogExitS_(__FILE__, __func__, __LINE__, val)
#  define OD_SYSLOG_EXIT_THROW_L(val) \
        ODSysLogExitThrowL_(__FILE__, __func__, __LINE__, val)
#  define OD_SYSLOG_EXIT_THROW_S(val) \
        ODSysLogExitThrowS_(__FILE__, __func__, __LINE__, val)
#  define OD_SYSLOG_INIT(prefix, options) \
        ODSysLogInit_(prefix, options, __FILE__, __func__, __LINE__)
#  define OD_SYSLOG_IP(text1, val1, val2) \
        ODSysLogIP_(__FILE__, __func__, __LINE__, text1, (long) (val1), (long) (val2))
#  define OD_SYSLOG_L1(text1, val1)  \
        ODSysLogL1_(__FILE__, __func__, __LINE__, text1, (long) (val1))
#  define OD_SYSLOG_L2(text1, val1, text2, val2)  \
        ODSysLogL2_(__FILE__, __func__, __LINE__, text1, (long) (val1), text2, (long) (val2))
#  define OD_SYSLOG_L3(text1, val1, text2, val2, text3, val3)  \
        ODSysLogL3_(__FILE__, __func__, __LINE__, text1, (long) (val1), text2, (long) (val2), text3, (long) (val3))
#  define OD_SYSLOG_L4(text1, val1, text2, val2, text3, val3, text4, val4)  \
        ODSysLogL4_(__FILE__, __func__, __LINE__, text1, (long) (val1), text2, (long) (val2), text3, (long) (val3),\
                    text4, (long) (val4))
#  define OD_SYSLOG_LL1(text1, val1)  \
        ODSysLogLL1_(__FILE__, __func__, __LINE__, text1, (long int) (val1))
#  define OD_SYSLOG_LL2(text1, val1, text2, val2)  \
        ODSysLogLL2_(__FILE__, __func__, __LINE__, text1, (long int) (val1), text2, (long int) (val2))
#  define OD_SYSLOG_LL3(text1, val1, text2, val2, text3, val3)  \
        ODSysLogLL3_(__FILE__, __func__, __LINE__, text1, (long int) (val1), text2, (long int) (val2), text3,\
                        (long int) (val3))
#  define OD_SYSLOG_LL4(text1, val1, text2, val2, text3, val3, text4, val4)  \
        ODSysLogLL4_(__FILE__, __func__, __LINE__, text1, (long int) (val1), text2, (long int) (val2), text3,\
                        (long int) (val3), text4, (long int) (val4))
#  define OD_SYSLOG_LS(text1, val1)  \
        ODSysLogLS_(__FILE__, __func__, __LINE__, text1, val1)
#  if defined(__OBJC__)
#   define OD_SYSLOG_O1(text1, obj1)  \
        ODSysLogO1_(__FILE__, __func__, __LINE__, text1, obj1)
#   define OD_SYSLOG_O2(text1, obj1, text2, obj2)  \
        ODSysLogO2_(__FILE__, __func__, __LINE__, text1, obj1, text2, obj2)
#   define OD_SYSLOG_O3(text1, obj1, text2, obj2, text3, obj3)  \
        ODSysLogO3_(__FILE__, __func__, __LINE__, text1, obj1, text2, obj2, text3, obj3)
#   define OD_SYSLOG_O4(text1, obj1, text2, obj2, text3, obj3, text4, obj4)  \
        ODSysLogO4_(__FILE__, __func__, __LINE__, text1, obj1, text2, obj2, text3, obj3, text4, obj4)
#  endif // defined(__OBJC__)
#  define OD_SYSLOG_P1(text1, ptr1)  \
        ODSysLogP1_(__FILE__, __func__, __LINE__, text1, (const void *) (ptr1))
#  define OD_SYSLOG_P2(text1, ptr1, text2, ptr2)  \
        ODSysLogP2_(__FILE__, __func__, __LINE__, text1, (const void *) (ptr1), text2, (const void *) (ptr2))
#  define OD_SYSLOG_P3(text1, ptr1, text2, ptr2, text3, ptr3)  \
        ODSysLogP3_(__FILE__, __func__, __LINE__, text1, (const void *) (ptr1), text2, (const void *) (ptr2), text3,\
                    (const void *) (ptr3))
#  define OD_SYSLOG_P4(text1, ptr1, text2, ptr2, text3, ptr3, text4, ptr4)  \
        ODSysLogP4_(__FILE__, __func__, __LINE__, text1, (const void *) (ptr1), text2, (const void *) (ptr2), text3,\
                    (const void *) (ptr3), text4, (const void *) (ptr4))
#  define OD_SYSLOG_PACKET(caption, buffer, size)  \
        ODSysLogPacket_(__FILE__, __func__,  __LINE__, caption, buffer, size)
#  if defined(__APPLE__)
#   define OD_SYSLOG_RECT(caption, rect)  \
        ODSysLogRect_(__FILE__, __func__,  __LINE__, caption, rect)
#  endif // defined(__APPLE__)
#  define OD_SYSLOG_S1(text1, val1)  \
        ODSysLogS1_(__FILE__, __func__, __LINE__, text1, val1)
#  define OD_SYSLOG_S2(text1, val1, text2, val2)  \
        ODSysLogS2_(__FILE__, __func__, __LINE__, text1, val1, text2, val2)
#  define OD_SYSLOG_S3(text1, val1, text2, val2, text3, val3)  \
        ODSysLogS3_(__FILE__, __func__, __LINE__, text1, val1, text2, val2, text3, val3)
#  define OD_SYSLOG_S4(text1, val1, text2, val2, text3, val3, text4, val4)  \
        ODSysLogS4_(__FILE__, __func__, __LINE__, text1, val1, text2, val2, text3, val3, text4, val4)
#  define OD_SYSLOG_Sp(text, len, val)  \
        ODSysLogSp_(__FILE__, __func__, __LINE__, text, (long) len, val)
#  define OD_SYSLOG_Ti(text1, val1)  \
        ODSysLogTi_(__FILE__, __func__, __LINE__, text1, val1)

#  if defined(__cplusplus)
extern "C"
{
#  endif // defined(__cplusplus)

    void ODSysLog_(const char * fileName,
                   const char * funcName,
                   const int    lineNumber,
                   const char * text);

    void ODSysLogB1_(const char * fileName,
                     const char * funcName,
                     const int    lineNumber,
                     const char * text1,
                     const bool   val1);

    void ODSysLogB2_(const char * fileName,
                     const char * funcName,
                     const int    lineNumber,
                     const char * text1,
                     const bool   val1,
                     const char * text2,
                     const bool   val2);

    void ODSysLogB3_(const char * fileName,
                     const char * funcName,
                     const int    lineNumber,
                     const char * text1,
                     const bool   val1,
                     const char * text2,
                     const bool   val2,
                     const char * text3,
                     const bool   val3);

    void ODSysLogB4_(const char * fileName,
                     const char * funcName,
                     const int    lineNumber,
                     const char * text1,
                     const bool   val1,
                     const char * text2,
                     const bool   val2,
                     const char * text3,
                     const bool   val3,
                     const char * text4,
                     const bool   val4);

    void ODSysLogC1_(const char * fileName,
                     const char * funcName,
                     const int    lineNumber,
                     const char * text1,
                     const char   val1);

    void ODSysLogC2_(const char * fileName,
                     const char * funcName,
                     const int    lineNumber,
                     const char * text1,
                     const char   val1,
                     const char * text2,
                     const char   val2);

    void ODSysLogC3_(const char * fileName,
                     const char * funcName,
                     const int    lineNumber,
                     const char * text1,
                     const char   val1,
                     const char * text2,
                     const char   val2,
                     const char * text3,
                     const char   val3);

    void ODSysLogC4_(const char * fileName,
                     const char * funcName,
                     const int    lineNumber,
                     const char * text1,
                     const char   val1,
                     const char * text2,
                     const char   val2,
                     const char * text3,
                     const char   val3,
                     const char * text4,
                     const char   val4);

    void ODSysLogD1_(const char * fileName,
                     const char * funcName,
                     const int    lineNumber,
                     const char * text1,
                     const double val1);

    void ODSysLogD2_(const char * fileName,
                     const char * funcName,
                     const int    lineNumber,
                     const char * text1,
                     const double val1,
                     const char * text2,
                     const double val2);

    void ODSysLogD3_(const char * fileName,
                     const char * funcName,
                     const int    lineNumber,
                     const char * text1,
                     const double val1,
                     const char * text2,
                     const double val2,
                     const char * text3,
                     const double val3);

    void ODSysLogD4_(const char * fileName,
                     const char * funcName,
                     const int    lineNumber,
                     const char * text1,
                     const double val1,
                     const char * text2,
                     const double val2,
                     const char * text3,
                     const double val3,
                     const char * text4,
                     const double val4);

    void ODSysLogEnter_(const char * fileName,
                        const char * funcName,
                        const int    lineNumber);

    void ODSysLogExit_(const char * fileName,
                       const char * funcName,
                       const int    lineNumber);

    void ODSysLogExitB_(const char * fileName,
                        const char * funcName,
                        const int    lineNumber,
                        const bool   val);

    void ODSysLogExitC_(const char * fileName,
                        const char * funcName,
                        const int    lineNumber,
                        const char   val);

    void ODSysLogExitD_(const char * fileName,
                        const char * funcName,
                        const int    lineNumber,
                        const double val);

    void ODSysLogExitExit_(const char * fileName,
                           const char * funcName,
                           const int    lineNumber,
                           const long   val);

    void ODSysLogExitL_(const char *  fileName,
                        const char *  funcName,
                        const int     lineNumber,
                        const int32_t val);

    void ODSysLogExitLL_(const char *  fileName,
                         const char *  funcName,
                         const int     lineNumber,
                         const int64_t val);

#  if defined(__OBJC__)
    void ODSysLogExitO_(const char * fileName,
                        const char * funcName,
                        const int    lineNumber,
                        const id     val);
#  endif // defined(__OBJC__)

    void ODSysLogExitP_(const char * fileName,
                        const char * funcName,
                        const int    lineNumber,
                        const void * val);

#  if defined(__APPLE__)
    void ODSysLogExitR_(const char * fileName,
                        const char * funcName,
                        const int    lineNumber,
                        const CGRect val);
#  endif // defined(__APPLE__)

    void ODSysLogExitS_(const char * fileName,
                        const char * funcName,
                        const int    lineNumber,
                        const char * val);

    void ODSysLogExitThrowL_(const char *  fileName,
                             const char *  funcName,
                             const int     lineNumber,
                             const int32_t val);
    
    void ODSysLogExitThrowS_(const char * fileName,
                             const char * funcName,
                             const int    lineNumber,
                             const char * val);

    void ODSysLogInit_(const char * prefix,
                       const int    options,
                       const char * fileName,
                       const char * funcName,
                       const int    lineNumber);

    void ODSysLogIP_(const char *  fileName,
                     const char *  funcName,
                     const int     lineNumber,
                     const char *  text1,
                     const int32_t val1,
                     const int     val2);

    void ODSysLogL1_(const char *  fileName,
                     const char *  funcName,
                     const int     lineNumber,
                     const char *  text1,
                     const int32_t val1);

    void ODSysLogL2_(const char *  fileName,
                     const char *  funcName,
                     const int     lineNumber,
                     const char *  text1,
                     const int32_t val1,
                     const char *  text2,
                     const int32_t val2);

    void ODSysLogL3_(const char *  fileName,
                     const char *  funcName,
                     const int     lineNumber,
                     const char *  text1,
                     const int32_t val1,
                     const char *  text2,
                     const int32_t val2,
                     const char *  text3,
                     const int32_t val3);

    void ODSysLogL4_(const char *  fileName,
                     const char *  funcName,
                     const int     lineNumber,
                     const char *  text1,
                     const int32_t val1,
                     const char *  text2,
                     const int32_t val2,
                     const char *  text3,
                     const int32_t val3,
                     const char *  text4,
                     const int32_t val4);

    void ODSysLogLL1_(const char *  fileName,
                      const char *  funcName,
                      const int     lineNumber,
                      const char *  text1,
                      const int64_t val1);

    void ODSysLogLL2_(const char *  fileName,
                      const char *  funcName,
                      const int     lineNumber,
                      const char *  text1,
                      const int64_t val1,
                      const char *  text2,
                      const int64_t val2);

    void ODSysLogLL3_(const char *  fileName,
                      const char *  funcName,
                      const int     lineNumber,
                      const char *  text1,
                      const int64_t val1,
                      const char *  text2,
                      const int64_t val2,
                      const char *  text3,
                      const int64_t val3);

    void ODSysLogLL4_(const char *  fileName,
                      const char *  funcName,
                      const int     lineNumber,
                      const char *  text1,
                      const int64_t val1,
                      const char *  text2,
                      const int64_t val2,
                      const char *  text3,
                      const int64_t val3,
                      const char *  text4,
                      const int64_t val4);

    void ODSysLogLS_(const char * fileName,
                     const char * funcName,
                     const int    lineNumber,
                     const char * text1,
                     const char * val1);

#  if defined(__OBJC__)
    void ODSysLogO1_(const char * fileName,
                     const char * funcName,
                     const int    lineNumber,
                     const char * text1,
                     const id     obj1);

    void ODSysLogO2_(const char * fileName,
                     const char * funcName,
                     const int    lineNumber,
                     const char * text1,
                     const id     obj1,
                     const char * text2,
                     const id     obj2);

    void ODSysLogO3_(const char * fileName,
                     const char * funcName,
                     const int    lineNumber,
                     const char * text1,
                     const id     obj1,
                     const char * text2,
                     const id     obj2,
                     const char * text3,
                     const id     obj3);

    void ODSysLogO4_(const char * fileName,
                     const char * funcName,
                     const int    lineNumber,
                     const char * text1,
                     const id     obj1,
                     const char * text2,
                     const id     obj2,
                     const char * text3,
                     const id     obj3,
                     const char * text4,
                     const id     obj4);
#  endif // defined(__OBJC__)

    void ODSysLogP1_(const char * fileName,
                     const char * funcName,
                     const int    lineNumber,
                     const char * text1,
                     const void * ptr1);

    void ODSysLogP2_(const char * fileName,
                     const char * funcName,
                     const int    lineNumber,
                     const char * text1,
                     const void * ptr1,
                     const char * text2,
                     const void * ptr2);

    void ODSysLogP3_(const char * fileName,
                     const char * funcName,
                     const int    lineNumber,
                     const char * text1,
                     const void * ptr1,
                     const char * text2,
                     const void * ptr2,
                     const char * text3,
                     const void * ptr3);

    void ODSysLogP4_(const char * fileName,
                     const char * funcName,
                     const int    lineNumber,
                     const char * text1,
                     const void * ptr1,
                     const char * text2,
                     const void * ptr2,
                     const char * text3,
                     const void * ptr3,
                     const char * text4,
                     const void * ptr4);

    void ODSysLogPacket_(const char * fileName,
                         const char * funcName,
                         const int    lineNumber,
                         const char * caption,
                         const char * buffer,
                         const int    size);

#  if defined(__APPLE__)
    void ODSysLogRect_(const char * fileName,
                       const char * funcName,
                       const int    lineNumber,
                       const char * caption,
                       const CGRect rect);
#  endif // defined(__APPLE__)

    void ODSysLogS1_(const char * fileName,
                     const char * funcName,
                     const int    lineNumber,
                     const char * text1,
                     const char * val1);

    void ODSysLogS2_(const char * fileName,
                     const char * funcName,
                     const int    lineNumber,
                     const char * text1,
                     const char * val1,
                     const char * text2,
                     const char * val2);

    void ODSysLogS3_(const char * fileName,
                     const char * funcName,
                     const int    lineNumber,
                     const char * text1,
                     const char * val1,
                     const char * text2,
                     const char * val2,
                     const char * text3,
                     const char * val3);

    void ODSysLogS4_(const char * fileName,
                     const char * funcName,
                     const int    lineNumber,
                     const char * text1,
                     const char * val1,
                     const char * text2,
                     const char * val2,
                     const char * text3,
                     const char * val3,
                     const char * text4,
                     const char * val4);

    void ODSysLogSp_(const char * fileName,
                     const char * funcName,
                     const int    lineNumber,
                     const char * text,
                     const int    len,
                     const char * val);

    void ODSysLogTi_(const char *           fileName,
                     const char *           funcName,
                     const int              lineNumber,
                     const char *           text1,
                     const struct timeval * val1);

#  if defined(__cplusplus)
}
#  endif // defined(__cplusplus)
# else // ! defined(ENABLE_OD_SYSLOG)
#  if defined(__OBJC__)
#   define OD_PRINTABLE_STRING(xx) ""
#  endif // defined(__OBJC__)
#  define OD_SYSLOG(text) /* */
#  define OD_SYSLOG_B1(text1, val1)  /* */
#  define OD_SYSLOG_B2(text1, val1, text2, val2)  /* */
#  define OD_SYSLOG_B3(text1, val1, text2, val2, text3, val3)  /* */
#  define OD_SYSLOG_B4(text1, val1, text2, val2, text3, val3, text4, val4)  /* */
#  define OD_SYSLOG_C1(text1, val1)  /* */
#  define OD_SYSLOG_C2(text1, val1, text2, val2)  /* */
#  define OD_SYSLOG_C3(text1, val1, text2, val2, text3, val3)  /* */
#  define OD_SYSLOG_C4(text1, val1, text2, val2, text3, val3, text4, val4)  /* */
#  define OD_SYSLOG_D1(text1, val1)  /* */
#  define OD_SYSLOG_D2(text1, val1, text2, val2)  /* */
#  define OD_SYSLOG_D3(text1, val1, text2, val2, text3, val3)  /* */
#  define OD_SYSLOG_D4(text1, val1, text2, val2, text3, val3, text4, val4)  /* */
#  define OD_SYSLOG_ENTER()  /* */
#  define OD_SYSLOG_EXIT()  /* */
#  define OD_SYSLOG_EXIT_B(val) /* */
#  define OD_SYSLOG_EXIT_C(val) /* */
#  define OD_SYSLOG_EXIT_D(val) /* */
#  define OD_SYSLOG_EXIT_EXIT(val) /* */
#  define OD_SYSLOG_EXIT_L(val) /* */
#  define OD_SYSLOG_EXIT_LL(val) /* */
#  if defined(__OBJC__)
#   define OD_SYSLOG_EXIT_O(val) /* */
#  endif // defined(__OBJC__)
#  define OD_SYSLOG_EXIT_P(val) /* */
#  if defined(__APPLE__)
#   define OD_SYSLOG_EXIT_R(val) /* */
#  endif // defined(__APPLE__)
#  define OD_SYSLOG_EXIT_S(val) /* */
#  define OD_SYSLOG_EXIT_THROW_L(val) /* */
#  define OD_SYSLOG_EXIT_THROW_S(val) /* */
#  define OD_SYSLOG_INIT(prefix, options) /* */
#  define OD_SYSLOG_IP(text1, val1, val2) /* */
#  define OD_SYSLOG_L1(text1, val1)  /* */
#  define OD_SYSLOG_L2(text1, val1, text2, val2)  /* */
#  define OD_SYSLOG_L3(text1, val1, text2, val2, text3, val3)  /* */
#  define OD_SYSLOG_L4(text1, val1, text2, val2, text3, val3, text4, val4)  /* */
#  define OD_SYSLOG_LL1(text1, val1)  /* */
#  define OD_SYSLOG_LL2(text1, val1, text2, val2)  /* */
#  define OD_SYSLOG_LL3(text1, val1, text2, val2, text3, val3)  /* */
#  define OD_SYSLOG_LL4(text1, val1, text2, val2, text3, val3, text4, val4)  /* */
#  define OD_SYSLOG_LS(text1, val1)  /* */
#  if defined(__OBJC__)
#   define OD_SYSLOG_O1(text1, obj1)  /* */
#   define OD_SYSLOG_O2(text1, obj1, text2, obj2)  /* */
#   define OD_SYSLOG_O3(text1, obj1, text2, obj2, text3, obj3)  /* */
#   define OD_SYSLOG_O4(text1, obj1, text2, obj2, text3, obj3, text4, obj4)  /* */
#  endif // defined(__OBJC__)
#  define OD_SYSLOG_P1(text1, ptr1)  /* */
#  define OD_SYSLOG_P2(text1, ptr1, text2, ptr2)  /* */
#  define OD_SYSLOG_P3(text1, ptr1, text2, ptr2, text3, ptr3)  /* */
#  define OD_SYSLOG_P4(text1, ptr1, text2, ptr2, text3, ptr3, text4, ptr4)  /* */
#  define OD_SYSLOG_PACKET(caption, buffer, size)  /* */
#  if defined(__APPLE__)
#   define OD_SYSLOG_RECT(caption, rect)  /* */
#  endif // defined(__APPLE__)
#  define OD_SYSLOG_S1(text1, val1)  /* */
#  define OD_SYSLOG_S2(text1, val1, text2, val2)  /* */
#  define OD_SYSLOG_S3(text1, val1, text2, val2, text3, val3)  /* */
#  define OD_SYSLOG_S4(text1, val1, text2, val2, text3, val3, text4, val4) /* */
#  define OD_SYSLOG_Sp(text, len, val)  /* */
#  define OD_SYSLOG_Ti(text1, val1)  /* */
# endif // ! defined(ENABLE_OD_SYSLOG)

#endif // ! defined(ODSYSLOG_H_)
