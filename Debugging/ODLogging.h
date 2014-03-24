//--------------------------------------------------------------------------------------
//
//  File:       ODLogging.h
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
    kODLoggingOptionNone = 0,
    kODLoggingOptionWriteToFile = 1,
    kODLoggingOptionIncludeProcessID = 2,
    kODLoggingOptionIncludeThreadID = 4,
    kODLoggingOptionEnableThreadSupport = 8,
    kODLoggingOptionWriteToStderr = 16
};

# if defined(OD_DISABLE_LOGGING)
#  undef OD_ENABLE_LOGGING
# endif // defined(OD_DISABLE_LOGGING)

# if defined(OD_ENABLE_LOGGING)
#  if defined(__OBJC__)
#   define OD_OBJPOINTER               self
#   define OD_OBJPRINTABLE_STRING(xx)  (xx ? [[xx description] UTF8String] : "<>")
#  elif defined(__cplusplus)
#   define OD_OBJPOINTER               this
#  else // ! defined(__cplusplus)
#   define OD_OBJPOINTER               NULL
#  endif // defined(__cplusplus)
#  define OD_LOG(text) \
        ODLog_(__FILE__, __func__,  __LINE__, text)
#  define OD_LOG_B1(text1, val1)  \
        ODLogB1_(__FILE__, __func__, __LINE__, text1, (long) (val1))
#  define OD_LOG_B2(text1, val1, text2, val2)  \
        ODLogB2_(__FILE__, __func__, __LINE__, text1, (long) (val1), text2, (long) (val2))
#  define OD_LOG_B3(text1, val1, text2, val2, text3, val3)  \
        ODLogB3_(__FILE__, __func__, __LINE__, text1, (long) (val1), text2, (long) (val2), text3, (long) (val3))
#  define OD_LOG_B4(text1, val1, text2, val2, text3, val3, text4, val4)  \
        ODLogB4_(__FILE__, __func__, __LINE__, text1, (long) (val1), text2, (long) (val2), text3, (long) (val3), text4,\
                    (long) (val4))
#  define OD_LOG_C1(text1, val1)  \
        ODLogC1_(__FILE__, __func__, __LINE__, text1, (char) (val1))
#  define OD_LOG_C2(text1, val1, text2, val2)  \
        ODLogC2_(__FILE__, __func__, __LINE__, text1, (char) (val1), text2, (char) (val2))
#  define OD_LOG_C3(text1, val1, text2, val2, text3, val3)  \
        ODLogC3_(__FILE__, __func__, __LINE__, text1, (char) (val1), text2, (char) (val2), text3, (char) (val3))
#  define OD_LOG_C4(text1, val1, text2, val2, text3, val3, text4, val4)  \
        ODLogC4_(__FILE__, __func__, __LINE__, text1, (char) (val1), text2, (char) (val2), text3, (char) (val3), text4,\
                    (char) (val4))
#  define OD_LOG_D1(text1, val1)  \
        ODLogD1_(__FILE__, __func__, __LINE__, text1, val1)
#  define OD_LOG_D2(text1, val1, text2, val2)  \
        ODLogD2_(__FILE__, __func__, __LINE__, text1, val1, text2, val2)
#  define OD_LOG_D3(text1, val1, text2, val2, text3, val3)  \
        ODLogD3_(__FILE__, __func__, __LINE__, text1, val1, text2, val2, text3, val3)
#  define OD_LOG_D4(text1, val1, text2, val2, text3, val3, text4, val4)  \
        ODLogD4_(__FILE__, __func__, __LINE__, text1, val1, text2, val2, text3, val3, text4, val4)
#  define OD_LOG_ENTER()  \
        ODLogEnter_(__FILE__, __func__, __LINE__)
#  define OD_LOG_EXIT()  \
        ODLogExit_(__FILE__, __func__, __LINE__)
#  define OD_LOG_EXIT_B(val) \
        ODLogExitB_(__FILE__, __func__, __LINE__, val)
#  define OD_LOG_EXIT_C(val) \
        ODLogExitC_(__FILE__, __func__, __LINE__, val)
#  define OD_LOG_EXIT_D(val) \
        ODLogExitD_(__FILE__, __func__, __LINE__, val)
#  define OD_LOG_EXIT_EXIT(val) \
        ODLogExitExit_(__FILE__, __func__, __LINE__, val)
#  define OD_LOG_EXIT_L(val) \
        ODLogExitL_(__FILE__, __func__, __LINE__, val)
#  define OD_LOG_EXIT_LL(val) \
        ODLogExitLL_(__FILE__, __func__, __LINE__, val)
#  if defined(__OBJC__)
#   define OD_LOG_EXIT_O(val) \
        ODLogExitO_(__FILE__, __func__, __LINE__, val)
#  endif // defined(__OBJC__)
#  define OD_LOG_EXIT_P(val) \
        ODLogExitP_(__FILE__, __func__, __LINE__, val)
#  if defined(__APPLE__)
#   define OD_LOG_EXIT_R(val) \
        ODLogExitR_(__FILE__, __func__, __LINE__, val)
#  endif // defined(__APPLE__)
#  define OD_LOG_EXIT_S(val) \
        ODLogExitS_(__FILE__, __func__, __LINE__, val)
#  define OD_LOG_EXIT_THROW_L(val) \
        ODLogExitThrowL_(__FILE__, __func__, __LINE__, val)
#  define OD_LOG_EXIT_THROW_S(val) \
        ODLogExitThrowS_(__FILE__, __func__, __LINE__, val)
#  define OD_LOG_INIT(prefix, options) \
        ODLogInit_(prefix, options, __FILE__, __func__, __LINE__)
#  define OD_LOG_IP(text1, val1, val2) \
        ODLogIP_(__FILE__, __func__, __LINE__, text1, (long) (val1), (long) (val2))
#  define OD_LOG_L1(text1, val1)  \
        ODLogL1_(__FILE__, __func__, __LINE__, text1, (long) (val1))
#  define OD_LOG_L2(text1, val1, text2, val2)  \
        ODLogL2_(__FILE__, __func__, __LINE__, text1, (long) (val1), text2, (long) (val2))
#  define OD_LOG_L3(text1, val1, text2, val2, text3, val3)  \
        ODLogL3_(__FILE__, __func__, __LINE__, text1, (long) (val1), text2, (long) (val2), text3, (long) (val3))
#  define OD_LOG_L4(text1, val1, text2, val2, text3, val3, text4, val4)  \
        ODLogL4_(__FILE__, __func__, __LINE__, text1, (long) (val1), text2, (long) (val2), text3, (long) (val3), text4,\
                    (long) (val4))
#  define OD_LOG_LL1(text1, val1)  \
        ODLogLL1_(__FILE__, __func__, __LINE__, text1, (long int) (val1))
#  define OD_LOG_LL2(text1, val1, text2, val2)  \
        ODLogLL2_(__FILE__, __func__, __LINE__, text1, (long int) (val1), text2, (long int) (val2))
#  define OD_LOG_LL3(text1, val1, text2, val2, text3, val3)  \
        ODLogLL3_(__FILE__, __func__, __LINE__, text1, (long int) (val1), text2, (long int) (val2), text3,\
                    (long int) (val3))
#  define OD_LOG_LL4(text1, val1, text2, val2, text3, val3, text4, val4)  \
        ODLogLL4_(__FILE__, __func__, __LINE__, text1, (long int) (val1), text2, (long int) (val2), text3,\
                    (long int) (val3), text4, (long int) (val4))
#  define OD_LOG_LS(text1, val1)  \
        ODLogLS_(__FILE__, __func__, __LINE__, text1, val1)
#  if defined(__OBJC__)
#   define OD_LOG_O1(text1, obj1)  \
        ODLogO1_(__FILE__, __func__, __LINE__, text1, obj1)
#   define OD_LOG_O2(text1, obj1, text2, obj2)  \
        ODLogO2_(__FILE__, __func__, __LINE__, text1, obj1, text2, obj2)
#   define OD_LOG_O3(text1, obj1, text2, obj2, text3, obj3)  \
        ODLogO3_(__FILE__, __func__, __LINE__, text1, obj1, text2, obj2, text3, obj3)
#   define OD_LOG_O4(text1, obj1, text2, obj2, text3, obj3, text4, obj4)  \
        ODLogO4_(__FILE__, __func__, __LINE__, text1, obj1, text2, obj2, text3, obj3, text4, obj4)
#  endif // defined(__OBJC__)
#  define OD_LOG_OBJENTER()  \
        ODLogObjEnter_(__FILE__, __func__, __LINE__, OD_OBJPOINTER)
#  define OD_LOG_OBJEXIT()  \
        ODLogObjExit_(__FILE__, __func__, __LINE__, OD_OBJPOINTER)
#  define OD_LOG_OBJEXIT_B(val) \
        ODLogObjExitB_(__FILE__, __func__, __LINE__, OD_OBJPOINTER, val)
#  define OD_LOG_OBJEXIT_C(val) \
        ODLogObjExitC_(__FILE__, __func__, __LINE__, OD_OBJPOINTER, val)
#  define OD_LOG_OBJEXIT_D(val) \
        ODLogObjExitD_(__FILE__, __func__, __LINE__, OD_OBJPOINTER, val)
#  define OD_LOG_OBJEXIT_EXIT(val) \
        ODLogObjExitExit_(__FILE__, __func__, __LINE__, OD_OBJPOINTER, val)
#  define OD_LOG_OBJEXIT_L(val) \
        ODLogObjExitL_(__FILE__, __func__, __LINE__, OD_OBJPOINTER, val)
#  define OD_LOG_OBJEXIT_LL(val) \
        ODLogObjExitLL_(__FILE__, __func__, __LINE__, OD_OBJPOINTER, val)
#  if defined(__OBJC__)
#   define OD_LOG_OBJEXIT_O(val) \
        ODLogObjExitO_(__FILE__, __func__, __LINE__, OD_OBJPOINTER, val)
#  endif // defined(__OBJC__)
#  define OD_LOG_OBJEXIT_P(val) \
        ODLogObjExitP_(__FILE__, __func__, __LINE__, OD_OBJPOINTER, val)
#  if defined(__APPLE__)
#   define OD_LOG_OBJEXIT_R(val) \
        ODLogObjExitR_(__FILE__, __func__, __LINE__, OD_OBJPOINTER, val)
#  endif // defined(__APPLE__)
#  define OD_LOG_OBJEXIT_S(val) \
        ODLogObjExitS_(__FILE__, __func__, __LINE__, OD_OBJPOINTER, val)
#  define OD_LOG_OBJEXIT_THROW_L(val) \
        ODLogObjExitThrowL_(__FILE__, __func__, __LINE__, OD_OBJPOINTER, val)
#  define OD_LOG_OBJEXIT_THROW_S(val) \
        ODLogObjExitThrowS_(__FILE__, __func__, __LINE__, OD_OBJPOINTER, val)
#  define OD_LOG_P1(text1, ptr1)  \
        ODLogP1_(__FILE__, __func__, __LINE__, text1, (const void *) (ptr1))
#  define OD_LOG_P2(text1, ptr1, text2, ptr2)  \
        ODLogP2_(__FILE__, __func__, __LINE__, text1, (const void *) (ptr1), text2, (const void *) (ptr2))
#  define OD_LOG_P3(text1, ptr1, text2, ptr2, text3, ptr3)  \
        ODLogP3_(__FILE__, __func__, __LINE__, text1, (const void *) (ptr1), text2, (const void *) (ptr2), text3,\
                    (const void *) (ptr3))
#  define OD_LOG_P4(text1, ptr1, text2, ptr2, text3, ptr3, text4, ptr4)  \
        ODLogP4_(__FILE__, __func__, __LINE__, text1, (const void *) (ptr1), text2, (const void *) (ptr2), text3,\
                    (const void *) (ptr3), text4, (const void *) (ptr4))
#  define OD_LOG_PACKET(caption, buffer, size)  \
        ODLogPacket_(__FILE__, __func__,  __LINE__, caption, buffer, size)
#  if defined(__APPLE__)
#   define OD_LOG_RECT(caption, rect)  \
        ODLogRect_(__FILE__, __func__,  __LINE__, caption, rect)
#  endif // defined(__APPLE__)
#  define OD_LOG_S1(text1, val1)  \
        ODLogS1_(__FILE__, __func__, __LINE__, text1, val1)
#  define OD_LOG_S2(text1, val1, text2, val2)  \
        ODLogS2_(__FILE__, __func__, __LINE__, text1, val1, text2, val2)
#  define OD_LOG_S3(text1, val1, text2, val2, text3, val3)  \
        ODLogS3_(__FILE__, __func__, __LINE__, text1, val1, text2, val2, text3, val3)
#  define OD_LOG_S4(text1, val1, text2, val2, text3, val3, text4, val4)  \
        ODLogS4_(__FILE__, __func__, __LINE__, text1, val1, text2, val2, text3, val3, text4, val4)
#  define OD_LOG_Sp(text, len, val)  \
        ODLogSp_(__FILE__, __func__, __LINE__, text, (long) len, val)
#  define OD_LOG_Ti(text1, val1)  \
        ODLogTi_(__FILE__, __func__, __LINE__, text1, val1)

#  if defined(__cplusplus)
extern "C"
{
#  endif // defined(__cplusplus)

    void ODLog_(const char * fileName,
                const char * funcName,
                const int    lineNumber,
                const char * text);

    void ODLogB1_(const char * fileName,
                  const char * funcName,
                  const int    lineNumber,
                  const char * text1,
                  const bool   val1);

    void ODLogB2_(const char * fileName,
                  const char * funcName,
                  const int    lineNumber,
                  const char * text1,
                  const bool   val1,
                  const char * text2,
                  const bool   val2);

    void ODLogB3_(const char * fileName,
                  const char * funcName,
                  const int    lineNumber,
                  const char * text1,
                  const bool   val1,
                  const char * text2,
                  const bool   val2,
                  const char * text3,
                  const bool   val3);

    void ODLogB4_(const char * fileName,
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

    void ODLogC1_(const char * fileName,
                  const char * funcName,
                  const int    lineNumber,
                  const char * text1,
                  const char   val1);

    void ODLogC2_(const char * fileName,
                  const char * funcName,
                  const int    lineNumber,
                  const char * text1,
                  const char   val1,
                  const char * text2,
                  const char   val2);

    void ODLogC3_(const char * fileName,
                  const char * funcName,
                  const int    lineNumber,
                  const char * text1,
                  const char   val1,
                  const char * text2,
                  const char   val2,
                  const char * text3,
                  const char   val3);

    void ODLogC4_(const char * fileName,
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

    void ODLogD1_(const char * fileName,
                  const char * funcName,
                  const int    lineNumber,
                  const char * text1,
                  const double val1);

    void ODLogD2_(const char * fileName,
                  const char * funcName,
                  const int    lineNumber,
                  const char * text1,
                  const double val1,
                  const char * text2,
                  const double val2);

    void ODLogD3_(const char * fileName,
                  const char * funcName,
                  const int    lineNumber,
                  const char * text1,
                  const double val1,
                  const char * text2,
                  const double val2,
                  const char * text3,
                  const double val3);

    void ODLogD4_(const char * fileName,
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

    void ODLogEnter_(const char * fileName,
                     const char * funcName,
                     const int    lineNumber);

    void ODLogExit_(const char * fileName,
                    const char * funcName,
                    const int    lineNumber);

    void ODLogExitB_(const char * fileName,
                     const char * funcName,
                     const int    lineNumber,
                     const bool   val);

    void ODLogExitC_(const char * fileName,
                     const char * funcName,
                     const int    lineNumber,
                     const char   val);

    void ODLogExitD_(const char * fileName,
                     const char * funcName,
                     const int    lineNumber,
                     const double val);

    void ODLogExitExit_(const char * fileName,
                        const char * funcName,
                        const int    lineNumber,
                        const long   val);

    void ODLogExitL_(const char *  fileName,
                     const char *  funcName,
                     const int     lineNumber,
                     const int32_t val);

    void ODLogExitLL_(const char *  fileName,
                      const char *  funcName,
                      const int     lineNumber,
                      const int64_t val);

#  if defined(__OBJC__)
    void ODLogExitO_(const char * fileName,
                     const char * funcName,
                     const int    lineNumber,
                     const id     val);
#  endif // defined(__OBJC__)

    void ODLogExitP_(const char * fileName,
                     const char * funcName,
                     const int    lineNumber,
                     const void * val);

#  if defined(__APPLE__)
    void ODLogExitR_(const char * fileName,
                     const char * funcName,
                     const int    lineNumber,
                     const CGRect val);
#  endif // defined(__APPLE__)

    void ODLogExitS_(const char * fileName,
                     const char * funcName,
                     const int    lineNumber,
                     const char * val);

    void ODLogExitThrowL_(const char *  fileName,
                          const char *  funcName,
                          const int     lineNumber,
                          const int32_t val);
    
    void ODLogExitThrowS_(const char * fileName,
                          const char * funcName,
                          const int    lineNumber,
                          const char * val);

    void ODLogInit_(const char * prefix,
                    const int    options,
                    const char * fileName,
                    const char * funcName,
                    const int    lineNumber);

    void ODLogIP_(const char *  fileName,
                  const char *  funcName,
                  const int     lineNumber,
                  const char *  text1,
                  const int32_t val1,
                  const int     val2);

    void ODLogL1_(const char *  fileName,
                  const char *  funcName,
                  const int     lineNumber,
                  const char *  text1,
                  const int32_t val1);

    void ODLogL2_(const char *  fileName,
                  const char *  funcName,
                  const int     lineNumber,
                  const char *  text1,
                  const int32_t val1,
                  const char *  text2,
                  const int32_t val2);

    void ODLogL3_(const char *  fileName,
                  const char *  funcName,
                  const int     lineNumber,
                  const char *  text1,
                  const int32_t val1,
                  const char *  text2,
                  const int32_t val2,
                  const char *  text3,
                  const int32_t val3);

    void ODLogL4_(const char *  fileName,
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

    void ODLogLL1_(const char *  fileName,
                   const char *  funcName,
                   const int     lineNumber,
                   const char *  text1,
                   const int64_t val1);

    void ODLogLL2_(const char *  fileName,
                   const char *  funcName,
                   const int     lineNumber,
                   const char *  text1,
                   const int64_t val1,
                   const char *  text2,
                   const int64_t val2);

    void ODLogLL3_(const char *  fileName,
                   const char *  funcName,
                   const int     lineNumber,
                   const char *  text1,
                   const int64_t val1,
                   const char *  text2,
                   const int64_t val2,
                   const char *  text3,
                   const int64_t val3);

    void ODLogLL4_(const char *  fileName,
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

    void ODLogLS_(const char * fileName,
                  const char * funcName,
                  const int    lineNumber,
                  const char * text1,
                  const char * val1);

#  if defined(__OBJC__)
    void ODLogO1_(const char * fileName,
                  const char * funcName,
                  const int    lineNumber,
                  const char * text1,
                  const id     obj1);

    void ODLogO2_(const char * fileName,
                  const char * funcName,
                  const int    lineNumber,
                  const char * text1,
                  const id     obj1,
                  const char * text2,
                  const id     obj2);

    void ODLogO3_(const char * fileName,
                  const char * funcName,
                  const int    lineNumber,
                  const char * text1,
                  const id     obj1,
                  const char * text2,
                  const id     obj2,
                  const char * text3,
                  const id     obj3);

    void ODLogO4_(const char * fileName,
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

    void ODLogObjEnter_(const char * fileName,
                        const char * funcName,
                        const int    lineNumber,
                        const void * objPtr);

    void ODLogObjExit_(const char * fileName,
                       const char * funcName,
                       const int    lineNumber,
                       const void * objPtr);

    void ODLogObjExitB_(const char * fileName,
                        const char * funcName,
                        const int    lineNumber,
                        const void * objPtr,
                        const bool   val);

    void ODLogObjExitC_(const char * fileName,
                        const char * funcName,
                        const int    lineNumber,
                        const void * objPtr,
                        const char   val);

    void ODLogObjExitD_(const char * fileName,
                        const char * funcName,
                        const int    lineNumber,
                        const void * objPtr,
                        const double val);

    void ODLogObjExitExit_(const char * fileName,
                           const char * funcName,
                           const int    lineNumber,
                           const void * objPtr,
                           const long   val);

    void ODLogObjExitL_(const char *  fileName,
                        const char *  funcName,
                        const int     lineNumber,
                        const void *  objPtr,
                        const int32_t val);

    void ODLogObjExitLL_(const char *  fileName,
                         const char *  funcName,
                         const int     lineNumber,
                         const void *  objPtr,
                         const int64_t val);

#  if defined(__OBJC__)
    void ODLogObjExitO_(const char * fileName,
                        const char * funcName,
                        const int    lineNumber,
                        const void * objPtr,
                        const id     val);
#  endif // defined(__OBJC__)

    void ODLogObjExitP_(const char * fileName,
                        const char * funcName,
                        const int    lineNumber,
                        const void * objPtr,
                        const void * val);

#  if defined(__APPLE__)
    void ODLogObjExitR_(const char * fileName,
                        const char * funcName,
                        const int    lineNumber,
                        const void * objPtr,
                        const CGRect val);
#  endif // defined(__APPLE__)

    void ODLogObjExitS_(const char * fileName,
                        const char * funcName,
                        const int    lineNumber,
                        const void * objPtr,
                        const char * val);

    void ODLogObjExitThrowL_(const char *  fileName,
                             const char *  funcName,
                             const int     lineNumber,
                             const void *  objPtr,
                             const int32_t val);

    void ODLogObjExitThrowS_(const char * fileName,
                             const char * funcName,
                             const int    lineNumber,
                             const void * objPtr,
                             const char * val);
    
    void ODLogP1_(const char * fileName,
                  const char * funcName,
                  const int    lineNumber,
                  const char * text1,
                  const void * ptr1);

    void ODLogP2_(const char * fileName,
                  const char * funcName,
                  const int    lineNumber,
                  const char * text1,
                  const void * ptr1,
                  const char * text2,
                  const void * ptr2);

    void ODLogP3_(const char * fileName,
                  const char * funcName,
                  const int    lineNumber,
                  const char * text1,
                  const void * ptr1,
                  const char * text2,
                  const void * ptr2,
                  const char * text3,
                  const void * ptr3);

    void ODLogP4_(const char * fileName,
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

    void ODLogPacket_(const char * fileName,
                      const char * funcName,
                      const int    lineNumber,
                      const char * caption,
                      const char * buffer,
                      const int    size);

#  if defined(__APPLE__)
    void ODLogRect_(const char * fileName,
                    const char * funcName,
                    const int    lineNumber,
                    const char * caption,
                    const CGRect rect);
#  endif // defined(__APPLE__)

    void ODLogS1_(const char * fileName,
                  const char * funcName,
                  const int    lineNumber,
                  const char * text1,
                  const char * val1);

    void ODLogS2_(const char * fileName,
                  const char * funcName,
                  const int    lineNumber,
                  const char * text1,
                  const char * val1,
                  const char * text2,
                  const char * val2);

    void ODLogS3_(const char * fileName,
                  const char * funcName,
                  const int    lineNumber,
                  const char * text1,
                  const char * val1,
                  const char * text2,
                  const char * val2,
                  const char * text3,
                  const char * val3);

    void ODLogS4_(const char * fileName,
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

    void ODLogSp_(const char * fileName,
                  const char * funcName,
                  const int    lineNumber,
                  const char * text,
                  const int    len,
                  const char * val);

    void ODLogTi_(const char *           fileName,
                  const char *           funcName,
                  const int              lineNumber,
                  const char *           text1,
                  const struct timeval * val1);

#  if defined(__cplusplus)
}
#  endif // defined(__cplusplus)
# else // ! defined(OD_ENABLE_LOGGING)
#  if defined(__OBJC__)
#   define OD_OBJPRINTABLE_STRING(xx) ""
#  endif // defined(__OBJC__)
#  define OD_LOG(text) /* */
#  define OD_LOG_B1(text1, val1)  /* */
#  define OD_LOG_B2(text1, val1, text2, val2)  /* */
#  define OD_LOG_B3(text1, val1, text2, val2, text3, val3)  /* */
#  define OD_LOG_B4(text1, val1, text2, val2, text3, val3, text4, val4)  /* */
#  define OD_LOG_C1(text1, val1)  /* */
#  define OD_LOG_C2(text1, val1, text2, val2)  /* */
#  define OD_LOG_C3(text1, val1, text2, val2, text3, val3)  /* */
#  define OD_LOG_C4(text1, val1, text2, val2, text3, val3, text4, val4)  /* */
#  define OD_LOG_D1(text1, val1)  /* */
#  define OD_LOG_D2(text1, val1, text2, val2)  /* */
#  define OD_LOG_D3(text1, val1, text2, val2, text3, val3)  /* */
#  define OD_LOG_D4(text1, val1, text2, val2, text3, val3, text4, val4)  /* */
#  define OD_LOG_ENTER()  /* */
#  define OD_LOG_EXIT()  /* */
#  define OD_LOG_EXIT_B(val) /* */
#  define OD_LOG_EXIT_C(val) /* */
#  define OD_LOG_EXIT_D(val) /* */
#  define OD_LOG_EXIT_EXIT(val) /* */
#  define OD_LOG_EXIT_L(val) /* */
#  define OD_LOG_EXIT_LL(val) /* */
#  if defined(__OBJC__)
#   define OD_LOG_EXIT_O(val) /* */
#  endif // defined(__OBJC__)
#  define OD_LOG_EXIT_P(val) /* */
#  if defined(__APPLE__)
#   define OD_LOG_EXIT_R(val) /* */
#  endif // defined(__APPLE__)
#  define OD_LOG_EXIT_S(val) /* */
#  define OD_LOG_EXIT_THROW_L(val) /* */
#  define OD_LOG_EXIT_THROW_S(val) /* */
#  define OD_LOG_INIT(prefix, options) /* */
#  define OD_LOG_IP(text1, val1, val2) /* */
#  define OD_LOG_L1(text1, val1)  /* */
#  define OD_LOG_L2(text1, val1, text2, val2)  /* */
#  define OD_LOG_L3(text1, val1, text2, val2, text3, val3)  /* */
#  define OD_LOG_L4(text1, val1, text2, val2, text3, val3, text4, val4)  /* */
#  define OD_LOG_LL1(text1, val1)  /* */
#  define OD_LOG_LL2(text1, val1, text2, val2)  /* */
#  define OD_LOG_LL3(text1, val1, text2, val2, text3, val3)  /* */
#  define OD_LOG_LL4(text1, val1, text2, val2, text3, val3, text4, val4)  /* */
#  define OD_LOG_LS(text1, val1)  /* */
#  if defined(__OBJC__)
#   define OD_LOG_O1(text1, obj1)  /* */
#   define OD_LOG_O2(text1, obj1, text2, obj2)  /* */
#   define OD_LOG_O3(text1, obj1, text2, obj2, text3, obj3)  /* */
#   define OD_LOG_O4(text1, obj1, text2, obj2, text3, obj3, text4, obj4)  /* */
#  endif // defined(__OBJC__)
#  define OD_LOG_OBJENTER()  /* */
#  define OD_LOG_OBJEXIT()  /* */
#  define OD_LOG_OBJEXIT_B(val) /* */
#  define OD_LOG_OBJEXIT_C(val) /* */
#  define OD_LOG_OBJEXIT_D(val) /* */
#  define OD_LOG_OBJEXIT_EXIT(val) /* */
#  define OD_LOG_OBJEXIT_L(val) /* */
#  define OD_LOG_OBJEXIT_LL(val) /* */
#  if defined(__OBJC__)
#   define OD_LOG_OBJEXIT_O(val) /* */
#  endif // defined(__OBJC__)
#  define OD_LOG_OBJEXIT_P(val) /* */
#  if defined(__APPLE__)
#   define OD_LOG_OBJEXIT_R(val) /* */
#  endif // defined(__APPLE__)
#  define OD_LOG_OBJEXIT_S(val) /* */
#  define OD_LOG_OBJEXIT_THROW_L(val) /* */
#  define OD_LOG_OBJEXIT_THROW_S(val) /* */
#  define OD_LOG_P1(text1, ptr1)  /* */
#  define OD_LOG_P2(text1, ptr1, text2, ptr2)  /* */
#  define OD_LOG_P3(text1, ptr1, text2, ptr2, text3, ptr3)  /* */
#  define OD_LOG_P4(text1, ptr1, text2, ptr2, text3, ptr3, text4, ptr4)  /* */
#  define OD_LOG_PACKET(caption, buffer, size)  /* */
#  if defined(__APPLE__)
#   define OD_LOG_RECT(caption, rect)  /* */
#  endif // defined(__APPLE__)
#  define OD_LOG_S1(text1, val1)  /* */
#  define OD_LOG_S2(text1, val1, text2, val2)  /* */
#  define OD_LOG_S3(text1, val1, text2, val2, text3, val3)  /* */
#  define OD_LOG_S4(text1, val1, text2, val2, text3, val3, text4, val4) /* */
#  define OD_LOG_Sp(text, len, val)  /* */
#  define OD_LOG_Ti(text1, val1)  /* */
# endif // ! defined(OD_ENABLE_LOGGING)

#endif // ! defined(ODSYSLOG_H_)
