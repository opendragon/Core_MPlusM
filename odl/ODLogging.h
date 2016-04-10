//--------------------------------------------------------------------------------------------------
//
//  File:       odl/ODLogging.h
//
//  Project:    m+m
//
//  Contains:   The function declarations and macro definitions for the logging facility.
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

#if (! defined(ODLOGGING_H_))
# define ODLOGGING_H_ /* Header guard */

# if (! defined(ODL_DEFINITIONS_LOADED))
#  include <stdint.h>
#  if defined(__OBJC__)
#   import <Foundation/Foundation.h>
#  endif // defined(__OBJC__)
#  if defined(__APPLE__)
#   include <CoreGraphics/CGGeometry.h>
#  endif // defined(__APPLE__)
# endif // ! defined(ODL_DEFINITIONS_LOADED)

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 @brief The function declarations and macro definitions for the logging facility. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

# if (! defined(MAC_OR_LINUX_))
  /* @c TRUE if non-Windows, @c FALSE if Windows. */
#  define MAC_OR_LINUX_ (defined(__APPLE__) || defined(__linux__))
# endif // ! defined(MAC_OR_LINUX_)

# if MAC_OR_LINUX_
#  define ODL_FUNC_NAME_ __func__
# else // ! MAC_OR_LINUX_
#  define ODL_FUNC_NAME_ __FUNCTION__
# endif // ! MAC_OR_LINUX_

// Note that the following values could be in an enumeration, but C/C++ is not happy
// seeing the same type declared multiple times, which will occur if this header file
// is included more than once...

# undef kODLoggingOptionNone
# undef kODLoggingOptionWriteToFile
# undef kODLoggingOptionIncludeProcessID
# undef kODLoggingOptionIncludeThreadID
# undef kODLoggingOptionEnableThreadSupport
# undef kODLoggingOptionWriteToStderr

/*! @brief No special logging options. */
# define kODLoggingOptionNone                0

/*! @brief Write the logging output to a file. */
# define kODLoggingOptionWriteToFile         1 /* bit mask, must be power of two */

/*! @brief Include the process identifier in the logging output. */
# define kODLoggingOptionIncludeProcessID    2 /* bit mask, must be power of two */

/*! @brief Include the thread identifier in the logging output. */
# define kODLoggingOptionIncludeThreadID     4 /* bit mask, must be power of two */

/*! @brief Enable threading support for logging. */
# define kODLoggingOptionEnableThreadSupport 8 /* bit mask, must be power of two */

/*! @brief Write the logging output to 'stderr' as well, if not logging to a file. */
# define kODLoggingOptionWriteToStderr       16 /* bit mask, must be power of two */

# if defined(ODL_DISABLE_LOGGING_)
#  undef ODL_ENABLE_LOGGING_
# endif // defined(ODL_DISABLE_LOGGING_)

# undef ODL_OBJPRINTABLE_STRING
# undef ODL_SELF_OR_THIS_OR_NULL_

# undef ODL_LOG
# undef ODL_B1
# undef ODL_B2
# undef ODL_B3
# undef ODL_B4
# undef ODL_C1
# undef ODL_C2
# undef ODL_C3
# undef ODL_C4
# undef ODL_D1
# undef ODL_D2
# undef ODL_D3
# undef ODL_D4
# undef ODL_ENTER
# undef ODL_EXIT
# undef ODL_EXIT_B
# undef ODL_EXIT_C
# undef ODL_EXIT_D
# undef ODL_EXIT_EXIT
# undef ODL_EXIT_L
# undef ODL_EXIT_LL
# undef ODL_EXIT_O
# undef ODL_EXIT_P
# undef ODL_EXIT_RECT
# undef ODL_EXIT_S
# undef ODL_EXIT_s
# undef ODL_EXIT_SIZE
# undef ODL_EXIT_THROW_L
# undef ODL_EXIT_THROW_S
# undef ODL_EXIT_THROW_X
# undef ODL_EXIT_X
# undef ODL_EXIT_XL
# undef ODL_INIT
# undef ODL_IP
# undef ODL_L1
# undef ODL_L2
# undef ODL_L3
# undef ODL_L4
# undef ODL_LL1
# undef ODL_LL2
# undef ODL_LL3
# undef ODL_LL4
# undef ODL_LS
# undef ODL_O1
# undef ODL_O2
# undef ODL_O3
# undef ODL_O4
# undef ODL_OBJENTER
# undef ODL_OBJEXIT
# undef ODL_OBJEXIT_B
# undef ODL_OBJEXIT_C
# undef ODL_OBJEXIT_D
# undef ODL_OBJEXIT_EXIT
# undef ODL_OBJEXIT_L
# undef ODL_OBJEXIT_LL
# undef ODL_OBJEXIT_O
# undef ODL_OBJEXIT_P
# undef ODL_OBJEXIT_RECT
# undef ODL_OBJEXIT_S
# undef ODL_OBJEXIT_s
# undef ODL_OBJEXIT_SIZE
# undef ODL_OBJEXIT_THROW_L
# undef ODL_OBJEXIT_THROW_S
# undef ODL_OBJEXIT_THROW_X
# undef ODL_OBJEXIT_X
# undef ODL_OBJEXIT_XL
# undef ODL_P1
# undef ODL_P2
# undef ODL_P3
# undef ODL_P4
# undef ODL_PACKET
# undef ODL_RECT
# undef ODL_S1
# undef ODL_S1s
# undef ODL_S2
# undef ODL_S2s
# undef ODL_S3
# undef ODL_S3s
# undef ODL_S4
# undef ODL_S4s
# undef ODL_SIZE
# undef ODL_Sp
# undef ODL_TIME
# undef ODL_X1
# undef ODL_X2
# undef ODL_X3
# undef ODL_X4
# undef ODL_XL1
# undef ODL_XL2
# undef ODL_XL3
# undef ODL_XL4

# if defined(ODL_ENABLE_LOGGING_)
#  if defined(__OBJC__)
/*! @brief The pointer to the calling object for a method. */
#   define ODL_SELF_OR_THIS_OR_NULL_   (__bridge const void *) self
/*! @brief Return the string description for an Objective-C object. */
#   define ODL_OBJPRINTABLE_STRING(xx) (xx ? [[xx description] UTF8String] : "<>")
#  elif defined(__cplusplus)
/*! @brief The pointer to the calling object for a method. */
#   define ODL_SELF_OR_THIS_OR_NULL_   (const void *) this
#  else // ! defined(__cplusplus)
/*! @brief The pointer to the calling object for a method. */
#   define ODL_SELF_OR_THIS_OR_NULL_   NULL
#  endif // defined(__cplusplus)

/*! @brief Write a string to the log.
 @param text The string to be written. */
#  define ODL_LOG(text) \
        ODLog_(__FILE__, ODL_FUNC_NAME_,  __LINE__, text)

/*! @brief Write a boolean value to the log.
 @param text1 The caption for the value to be written.
 @param val1 The value to be written. */
#  define ODL_B1(text1, val1) \
        ODLogB1_(__FILE__, ODL_FUNC_NAME_, __LINE__, text1, (long) (val1))

/*! @brief Write two boolean values to the log.
 @param text1 The caption for the first value to be written.
 @param val1 The first value to be written.
 @param text2 The caption for the second value to be written.
 @param val2 The second value to be written. */
#  define ODL_B2(text1, val1, text2, val2) \
        ODLogB2_(__FILE__, ODL_FUNC_NAME_, __LINE__, text1, (long) (val1), text2, (long) (val2))

/*! @brief Write three boolean values to the log.
 @param text1 The caption for the first value to be written.
 @param val1 The first value to be written.
 @param text2 The caption for the second value to be written.
 @param val2 The second value to be written.
 @param text3 The caption for the third value to be written.
 @param val3 The third value to be written. */
#  define ODL_B3(text1, val1, text2, val2, text3, val3) \
        ODLogB3_(__FILE__, ODL_FUNC_NAME_, __LINE__, text1, (long) (val1), text2, (long) (val2),\
                    text3, (long) (val3))

/*! @brief Write four boolean values to the log.
 @param text1 The caption for the first value to be written.
 @param val1 The first value to be written.
 @param text2 The caption for the second value to be written.
 @param val2 The second value to be written.
 @param text3 The caption for the third value to be written.
 @param val3 The third value to be written.
 @param text4 The caption for the fourth value to be written.
 @param val4 The fourth value to be written. */
#  define ODL_B4(text1, val1, text2, val2, text3, val3, text4, val4) \
        ODLogB4_(__FILE__, ODL_FUNC_NAME_, __LINE__, text1, (long) (val1), text2, (long) (val2),\
                    text3, (long) (val3), text4, (long) (val4))

/*! @brief Write a character value to the log.
 @param text1 The caption for the value to be written.
 @param val1 The value to be written. */
#  define ODL_C1(text1, val1) \
        ODLogC1_(__FILE__, ODL_FUNC_NAME_, __LINE__, text1, (char) (val1))

/*! @brief Write two character values to the log.
 @param text1 The caption for the first value to be written.
 @param val1 The first value to be written.
 @param text2 The caption for the second value to be written.
 @param val2 The second value to be written. */
#  define ODL_C2(text1, val1, text2, val2) \
        ODLogC2_(__FILE__, ODL_FUNC_NAME_, __LINE__, text1, (char) (val1), text2, (char) (val2))

/*! @brief Write three character values to the log.
 @param text1 The caption for the first value to be written.
 @param val1 The first value to be written.
 @param text2 The caption for the second value to be written.
 @param val2 The second value to be written.
 @param text3 The caption for the third value to be written.
 @param val3 The third value to be written. */
#  define ODL_C3(text1, val1, text2, val2, text3, val3) \
        ODLogC3_(__FILE__, ODL_FUNC_NAME_, __LINE__, text1, (char) (val1), text2, (char) (val2),\
                    text3, (char) (val3))

/*! @brief Write four character values to the log.
 @param text1 The caption for the first value to be written.
 @param val1 The first value to be written.
 @param text2 The caption for the second value to be written.
 @param val2 The second value to be written.
 @param text3 The caption for the third value to be written.
 @param val3 The third value to be written.
 @param text4 The caption for the fourth value to be written.
 @param val4 The fourth value to be written. */
#  define ODL_C4(text1, val1, text2, val2, text3, val3, text4, val4) \
        ODLogC4_(__FILE__, ODL_FUNC_NAME_, __LINE__, text1, (char) (val1), text2, (char) (val2),\
                    text3, (char) (val3), text4, (char) (val4))

/*! @brief Write a double value to the log.
 @param text1 The caption for the value to be written.
 @param val1 The value to be written. */
#  define ODL_D1(text1, val1) \
        ODLogD1_(__FILE__, ODL_FUNC_NAME_, __LINE__, text1, val1)

/*! @brief Write two double values to the log.
 @param text1 The caption for the first value to be written.
 @param val1 The first value to be written.
 @param text2 The caption for the second value to be written.
 @param val2 The second value to be written. */
#  define ODL_D2(text1, val1, text2, val2) \
        ODLogD2_(__FILE__, ODL_FUNC_NAME_, __LINE__, text1, val1, text2, val2)

/*! @brief Write three double values to the log.
 @param text1 The caption for the first value to be written.
 @param val1 The first value to be written.
 @param text2 The caption for the second value to be written.
 @param val2 The second value to be written.
 @param text3 The caption for the third value to be written.
 @param val3 The third value to be written. */
#  define ODL_D3(text1, val1, text2, val2, text3, val3) \
        ODLogD3_(__FILE__, ODL_FUNC_NAME_, __LINE__, text1, val1, text2, val2, text3, val3)

/*! @brief Write four double values to the log.
 @param text1 The caption for the first value to be written.
 @param val1 The first value to be written.
 @param text2 The caption for the second value to be written.
 @param val2 The second value to be written.
 @param text3 The caption for the third value to be written.
 @param val3 The third value to be written.
 @param text4 The caption for the fourth value to be written.
 @param val4 The fourth value to be written. */
#  define ODL_D4(text1, val1, text2, val2, text3, val3, text4, val4) \
        ODLogD4_(__FILE__, ODL_FUNC_NAME_, __LINE__, text1, val1, text2, val2, text3, val3, text4,\
                    val4)

/*! @brief Write a function entry string to the log. */
#  define ODL_ENTER() \
        ODLogEnter_(__FILE__, ODL_FUNC_NAME_, __LINE__)

/*! @brief Write a void function exit string to the log. */
#  define ODL_EXIT() \
        ODLogExit_(__FILE__, ODL_FUNC_NAME_, __LINE__)

/*! @brief Write a boolean function exit string to the log.
 @param val The value being returned by the function. */
#  define ODL_EXIT_B(val) \
        ODLogExitB_(__FILE__, ODL_FUNC_NAME_, __LINE__, val)

/*! @brief Write a character function exit string to the log.
 @param val The value being returned by the function. */
#  define ODL_EXIT_C(val) \
        ODLogExitC_(__FILE__, ODL_FUNC_NAME_, __LINE__, val)

/*! @brief Write a double function exit string to the log.
 @param val The value being returned by the function. */
#  define ODL_EXIT_D(val) \
        ODLogExitD_(__FILE__, ODL_FUNC_NAME_, __LINE__, val)

/*! @brief Write an exit function string to the log.
 @param val The value being returned by the function. */
#  define ODL_EXIT_EXIT(val) \
        ODLogExitExit_(__FILE__, ODL_FUNC_NAME_, __LINE__, val)

/*! @brief Write a long function exit string to the log.
 @param val The value being returned by the function. */
#  define ODL_EXIT_L(val) \
        ODLogExitL_(__FILE__, ODL_FUNC_NAME_, __LINE__, val)

/*! @brief Write a long long function exit string to the log.
 @param val The value being returned by the function. */
#  define ODL_EXIT_LL(val) \
        ODLogExitLL_(__FILE__, ODL_FUNC_NAME_, __LINE__, val)

#  if defined(__OBJC__)
/*! @brief Write an object function exit string to the log.
 @param val The value being returned by the function. */
#   define ODL_EXIT_O(val) \
        ODLogExitO_(__FILE__, ODL_FUNC_NAME_, __LINE__, val)
#  endif // defined(__OBJC__)

/*! @brief Write a pointer function exit string to the log.
 @param val The value being returned by the function. */
#  define ODL_EXIT_P(val) \
        ODLogExitP_(__FILE__, ODL_FUNC_NAME_, __LINE__, val)

#  if defined(__APPLE__)
/*! @brief Write a rectangle function exit string to the log.
 @param val The value being returned by the function. */
#   define ODL_EXIT_RECT(val) \
        ODLogExitRect_(__FILE__, ODL_FUNC_NAME_, __LINE__, val)
#  endif // defined(__APPLE__)

/*! @brief Write a string function exit string to the log.
 @param val The value being returned by the function. */
#  define ODL_EXIT_S(val) \
        ODLogExitS_(__FILE__, ODL_FUNC_NAME_, __LINE__, val)

#  if defined(__APPLE__)
/*! @brief Write a size function exit string to the log.
 @param val The value being returned by the function. */
#   define ODL_EXIT_SIZE(val) \
        ODLogExitSize_(__FILE__, ODL_FUNC_NAME_, __LINE__, val)
#  endif // defined(__APPLE__)

/*! @brief Write a throw/long function exit string to the log.
 @param val The value being thrown by the function. */
#  define ODL_EXIT_THROW_L(val) \
        ODLogExitThrowL_(__FILE__, ODL_FUNC_NAME_, __LINE__, val)

/*! @brief Write a throw/string function exit string to the log.
 @param val The value being thrown by the function. */
#  define ODL_EXIT_THROW_S(val) \
        ODLogExitThrowS_(__FILE__, ODL_FUNC_NAME_, __LINE__, val)

/*! @brief Write a throw/long hexadecimal function exit string to the log.
 @param val The value being thrown by the function. */
#  define ODL_EXIT_THROW_X(val) \
        ODLogExitThrowX_(__FILE__, ODL_FUNC_NAME_, __LINE__, val)

/*! @brief Write a long hexadecimal function exit string to the log.
 @param val The value being returned by the function. */
#  define ODL_EXIT_X(val) \
        ODLogExitX_(__FILE__, ODL_FUNC_NAME_, __LINE__, val)

/*! @brief Write a long long hexadecimal function exit string to the log.
 @param val The value being returned by the function. */
#  define ODL_EXIT_XL(val) \
        ODLogExitXL_(__FILE__, ODL_FUNC_NAME_, __LINE__, val)

/*! @brief Set up the logging state.
 @param prefix The output prefix string to be applied.
 @param options The logging options to be applied. */
#  define ODL_INIT(prefix, options) \
        ODLogInit_(prefix, options, __FILE__, ODL_FUNC_NAME_, __LINE__)

/*! @brief Write an IP address to the log.
 @param text1 The caption for the value to be written.
 @param val1 The IP address value to be written.
 @param val2 The port value to be written. */
#  define ODL_IP(text1, val1, val2) \
        ODLogIP_(__FILE__, ODL_FUNC_NAME_, __LINE__, text1, (long) (val1), (long) (val2))

/*! @brief Write a long value to the log.
 @param text1 The caption for the value to be written.
 @param val1 The value to be written. */
#  define ODL_L1(text1, val1) \
        ODLogL1_(__FILE__, ODL_FUNC_NAME_, __LINE__, text1, (long) (val1))

/*! @brief Write two long values to the log.
 @param text1 The caption for the first value to be written.
 @param val1 The first value to be written.
 @param text2 The caption for the second value to be written.
 @param val2 The second value to be written. */
#  define ODL_L2(text1, val1, text2, val2) \
        ODLogL2_(__FILE__, ODL_FUNC_NAME_, __LINE__, text1, (long) (val1), text2, (long) (val2))

/*! @brief Write three long values to the log.
 @param text1 The caption for the first value to be written.
 @param val1 The first value to be written.
 @param text2 The caption for the second value to be written.
 @param val2 The second value to be written.
 @param text3 The caption for the third value to be written.
 @param val3 The third value to be written. */
#  define ODL_L3(text1, val1, text2, val2, text3, val3) \
        ODLogL3_(__FILE__, ODL_FUNC_NAME_, __LINE__, text1, (long) (val1), text2, (long) (val2),\
                    text3, (long) (val3))

/*! @brief Write four long values to the log.
 @param text1 The caption for the first value to be written.
 @param val1 The first value to be written.
 @param text2 The caption for the second value to be written.
 @param val2 The second value to be written.
 @param text3 The caption for the third value to be written.
 @param val3 The third value to be written.
 @param text4 The caption for the fourth value to be written.
 @param val4 The fourth value to be written. */
#  define ODL_L4(text1, val1, text2, val2, text3, val3, text4, val4) \
        ODLogL4_(__FILE__, ODL_FUNC_NAME_, __LINE__, text1, (long) (val1), text2, (long) (val2),\
                    text3, (long) (val3), text4, (long) (val4))

/*! @brief Write a long long value to the log.
 @param text1 The caption for the value to be written.
 @param val1 The value to be written. */
#  define ODL_LL1(text1, val1) \
        ODLogLL1_(__FILE__, ODL_FUNC_NAME_, __LINE__, text1, (long int) (val1))

/*! @brief Write two long long values to the log.
 @param text1 The caption for the first value to be written.
 @param val1 The first value to be written.
 @param text2 The caption for the second value to be written.
 @param val2 The second value to be written. */
#  define ODL_LL2(text1, val1, text2, val2) \
        ODLogLL2_(__FILE__, ODL_FUNC_NAME_, __LINE__, text1, (long int) (val1), text2,\
                    (long int) (val2))

/*! @brief Write three long long values to the log.
 @param text1 The caption for the first value to be written.
 @param val1 The first value to be written.
 @param text2 The caption for the second value to be written.
 @param val2 The second value to be written.
 @param text3 The caption for the third value to be written.
 @param val3 The third value to be written. */
#  define ODL_LL3(text1, val1, text2, val2, text3, val3) \
        ODLogLL3_(__FILE__, ODL_FUNC_NAME_, __LINE__, text1, (long int) (val1), text2, \
                    (long int) (val2), text3, (long int) (val3))

/*! @brief Write four long long values to the log.
 @param text1 The caption for the first value to be written.
 @param val1 The first value to be written.
 @param text2 The caption for the second value to be written.
 @param val2 The second value to be written.
 @param text3 The caption for the third value to be written.
 @param val3 The third value to be written.
 @param text4 The caption for the fourth value to be written.
 @param val4 The fourth value to be written. */
#  define ODL_LL4(text1, val1, text2, val2, text3, val3, text4, val4) \
        ODLogLL4_(__FILE__, ODL_FUNC_NAME_, __LINE__, text1, (long int) (val1), text2, \
                    (long int) (val2), text3, (long int) (val3), text4, (long int) (val4))

/*! @brief Write a long string value to the log.
 @param text1 The caption for the value to be written.
 @param val1 The value to be written. */
#  define ODL_LS(text1, val1) \
        ODLogLS_(__FILE__, ODL_FUNC_NAME_, __LINE__, text1, val1)

#  if defined(__OBJC__)
/*! @brief Write an object value to the log.
 @param text1 The caption for the value to be written.
 @param obj1 The value to be written. */
#   define ODL_O1(text1, obj1) \
        ODLogO1_(__FILE__, ODL_FUNC_NAME_, __LINE__, text1, obj1)

/*! @brief Write two object values to the log.
 @param text1 The caption for the first value to be written.
 @param obj1 The value to be written.
 @param text2 The caption for the second value to be written.
 @param obj2 The second value to be written. */
#   define ODL_O2(text1, obj1, text2, obj2) \
        ODLogO2_(__FILE__, ODL_FUNC_NAME_, __LINE__, text1, obj1, text2, obj2)

/*! @brief Write three object values to the log.
 @param text1 The caption for the first value to be written.
 @param obj1 The value to be written.
 @param text2 The caption for the second value to be written.
 @param obj2 The second value to be written.
 @param text3 The caption for the third value to be written.
 @param obj3 The third value to be written. */
#   define ODL_O3(text1, obj1, text2, obj2, text3, obj3) \
        ODLogO3_(__FILE__, ODL_FUNC_NAME_, __LINE__, text1, obj1, text2, obj2, text3, obj3)

/*! @brief Write four object values to the log.
 @param text1 The caption for the first value to be written.
 @param obj1 The value to be written.
 @param text2 The caption for the second value to be written.
 @param obj2 The second value to be written.
 @param text3 The caption for the third value to be written.
 @param obj3 The third value to be written.
 @param text4 The caption for the fourth value to be written.
 @param obj4 The fourth value to be written. */
#   define ODL_O4(text1, obj1, text2, obj2, text3, obj3, text4, obj4) \
        ODLogO4_(__FILE__, ODL_FUNC_NAME_, __LINE__, text1, obj1, text2, obj2, text3, obj3, text4,\
                    obj4)
#  endif // defined(__OBJC__)

/*! @brief Write a method entry string to the log. */
#  define ODL_OBJENTER() \
        ODLogObjEnter_(__FILE__, ODL_FUNC_NAME_, __LINE__, ODL_SELF_OR_THIS_OR_NULL_)

/*! @brief Write a void method exit string to the log. */
#  define ODL_OBJEXIT() \
        ODLogObjExit_(__FILE__, ODL_FUNC_NAME_, __LINE__, ODL_SELF_OR_THIS_OR_NULL_)

/*! @brief Write a boolean method exit string to the log.
 @param val The value being returned by the method. */
#  define ODL_OBJEXIT_B(val) \
        ODLogObjExitB_(__FILE__, ODL_FUNC_NAME_, __LINE__, ODL_SELF_OR_THIS_OR_NULL_, val)

/*! @brief Write a character method exit string to the log.
 @param val The value being returned by the method. */
#  define ODL_OBJEXIT_C(val) \
        ODLogObjExitC_(__FILE__, ODL_FUNC_NAME_, __LINE__, ODL_SELF_OR_THIS_OR_NULL_, val)

/*! @brief Write a double method exit string to the log.
 @param val The value being returned by the method. */
#  define ODL_OBJEXIT_D(val) \
        ODLogObjExitD_(__FILE__, ODL_FUNC_NAME_, __LINE__, ODL_SELF_OR_THIS_OR_NULL_, val)

/*! @brief Write an exit method string to the log.
 @param val The value being returned by the method. */
#  define ODL_OBJEXIT_EXIT(val) \
        ODLogObjExitExit_(__FILE__, ODL_FUNC_NAME_, __LINE__, ODL_SELF_OR_THIS_OR_NULL_, val)

/*! @brief Write a long method exit string to the log.
 @param val The value being returned by the method. */
#  define ODL_OBJEXIT_L(val) \
        ODLogObjExitL_(__FILE__, ODL_FUNC_NAME_, __LINE__, ODL_SELF_OR_THIS_OR_NULL_, val)

/*! @brief Write a long long method exit string to the log.
 @param val The value being returned by the method. */
#  define ODL_OBJEXIT_LL(val) \
        ODLogObjExitLL_(__FILE__, ODL_FUNC_NAME_, __LINE__, ODL_SELF_OR_THIS_OR_NULL_, val)

#  if defined(__OBJC__)
/*! @brief Write an object method exit string to the log.
 @param val The value being returned by the method. */
#   define ODL_OBJEXIT_O(val) \
        ODLogObjExitO_(__FILE__, ODL_FUNC_NAME_, __LINE__, ODL_SELF_OR_THIS_OR_NULL_, val)
#  endif // defined(__OBJC__)

/*! @brief Write a pointer method exit string to the log.
 @param val The value being returned by the method. */
#  define ODL_OBJEXIT_P(val) \
        ODLogObjExitP_(__FILE__, ODL_FUNC_NAME_, __LINE__, ODL_SELF_OR_THIS_OR_NULL_, val)

#  if defined(__APPLE__)
/*! @brief Write a rectangle method exit string to the log.
 @param val The value being returned by the method. */
#   define ODL_OBJEXIT_RECT(val) \
        ODLogObjExitRect_(__FILE__, ODL_FUNC_NAME_, __LINE__, ODL_SELF_OR_THIS_OR_NULL_, val)
#  endif // defined(__APPLE__)

/*! @brief Write a string method exit string to the log.
 @param val The value being returned by the method. */
#  define ODL_OBJEXIT_S(val) \
        ODLogObjExitS_(__FILE__, ODL_FUNC_NAME_, __LINE__, ODL_SELF_OR_THIS_OR_NULL_, val)

#  if defined(__APPLE__)
/*! @brief Write a size method exit string to the log.
 @param val The value being returned by the method. */
#   define ODL_OBJEXIT_SIZE(val) \
        ODLogObjExitSize_(__FILE__, ODL_FUNC_NAME_, __LINE__, ODL_SELF_OR_THIS_OR_NULL_, val)
#  endif // defined(__APPLE__)

/*! @brief Write a throw/long method exit string to the log.
 @param val The value being thrown by the method. */
#  define ODL_OBJEXIT_THROW_L(val) \
        ODLogObjExitThrowL_(__FILE__, ODL_FUNC_NAME_, __LINE__, ODL_SELF_OR_THIS_OR_NULL_, val)

/*! @brief Write a throw/string method exit string to the log.
 @param val The value being thrown by the method. */
#  define ODL_OBJEXIT_THROW_S(val) \
        ODLogObjExitThrowS_(__FILE__, ODL_FUNC_NAME_, __LINE__, ODL_SELF_OR_THIS_OR_NULL_, val)

/*! @brief Write a throw/long hexadecimal method exit string to the log.
 @param val The value being thrown by the method. */
#  define ODL_OBJEXIT_THROW_X(val) \
        ODLogObjExitThrowX_(__FILE__, ODL_FUNC_NAME_, __LINE__, ODL_SELF_OR_THIS_OR_NULL_, val)

/*! @brief Write a long hexadecimal method exit string to the log.
 @param val The value being returned by the method. */
#  define ODL_OBJEXIT_X(val) \
        ODLogObjExitX_(__FILE__, ODL_FUNC_NAME_, __LINE__, ODL_SELF_OR_THIS_OR_NULL_, val)

/*! @brief Write a long long hexadecimal method exit string to the log.
 @param val The value being returned by the method. */
#  define ODL_OBJEXIT_XL(val) \
        ODLogObjExitXL_(__FILE__, ODL_FUNC_NAME_, __LINE__, ODL_SELF_OR_THIS_OR_NULL_, val)

/*! @brief Write a pointer value to the log.
 @param text1 The caption for the value to be written.
 @param ptr1 The value to be written. */
#  define ODL_P1(text1, ptr1) \
        ODLogP1_(__FILE__, ODL_FUNC_NAME_, __LINE__, text1, ptr1)

/*! @brief Write two pointer values to the log.
 @param text1 The caption for the first value to be written.
 @param ptr1 The value to be written.
 @param text2 The caption for the second value to be written.
 @param ptr2 The second value to be written. */
#  define ODL_P2(text1, ptr1, text2, ptr2) \
        ODLogP2_(__FILE__, ODL_FUNC_NAME_, __LINE__, text1, ptr1, text2, ptr2)

/*! @brief Write three pointer values to the log.
 @param text1 The caption for the first value to be written.
 @param ptr1 The value to be written.
 @param text2 The caption for the second value to be written.
 @param ptr2 The second value to be written.
 @param text3 The caption for the third value to be written.
 @param ptr3 The third value to be written. */
#  define ODL_P3(text1, ptr1, text2, ptr2, text3, ptr3) \
        ODLogP3_(__FILE__, ODL_FUNC_NAME_, __LINE__, text1, ptr1, text2, ptr2, text3, ptr3)

/*! @brief Write four pointer values to the log.
 @param text1 The caption for the first value to be written.
 @param ptr1 The value to be written.
 @param text2 The caption for the second value to be written.
 @param ptr2 The second value to be written.
 @param text3 The caption for the third value to be written.
 @param ptr3 The third value to be written.
 @param text4 The caption for the fourth value to be written.
 @param ptr4 The fourth value to be written. */
#  define ODL_P4(text1, ptr1, text2, ptr2, text3, ptr3, text4, ptr4) \
        ODLogP4_(__FILE__, ODL_FUNC_NAME_, __LINE__, text1, ptr1, text2, ptr2, text3, ptr3, text4,\
                    ptr4)

/*! @brief Write a region of memory to the log.
 @param caption The caption for the region to be written.
 @param buffer The starting address of the region.
 @param size The number of bytes to be written. */
#  define ODL_PACKET(caption, buffer, size) \
        ODLogPacket_(__FILE__, ODL_FUNC_NAME_,  __LINE__, caption, buffer, size)

#  if defined(__APPLE__)
/*! @brief Write a rectangle to the log.
 @param caption The caption for the value to be written.
 @param rect The value to be written. */
#   define ODL_RECT(caption, rect) \
        ODLogRect_(__FILE__, ODL_FUNC_NAME_,  __LINE__, caption, rect)
#  endif // defined(__APPLE__)

/*! @brief Write a string value to the log.
 @param text1 The caption for the value to be written.
 @param val1 The value to be written. */
#  define ODL_S1(text1, val1) \
        ODLogS1_(__FILE__, ODL_FUNC_NAME_, __LINE__, text1, val1)

/*! @brief Write two string values to the log.
 @param text1 The caption for the first value to be written.
 @param val1 The first value to be written.
 @param text2 The caption for the second value to be written.
 @param val2 The second value to be written. */
#  define ODL_S2(text1, val1, text2, val2) \
        ODLogS2_(__FILE__, ODL_FUNC_NAME_, __LINE__, text1, val1, text2, val2)

/*! @brief Write three string values to the log.
 @param text1 The caption for the first value to be written.
 @param val1 The first value to be written.
 @param text2 The caption for the second value to be written.
 @param val2 The second value to be written.
 @param text3 The caption for the third value to be written.
 @param val3 The third value to be written. */
#  define ODL_S3(text1, val1, text2, val2, text3, val3) \
        ODLogS3_(__FILE__, ODL_FUNC_NAME_, __LINE__, text1, val1, text2, val2, text3, val3)

/*! @brief Write four string values to the log.
 @param text1 The caption for the first value to be written.
 @param val1 The first value to be written.
 @param text2 The caption for the second value to be written.
 @param val2 The second value to be written.
 @param text3 The caption for the third value to be written.
 @param val3 The third value to be written.
 @param text4 The caption for the fourth value to be written.
 @param val4 The fourth value to be written. */
#  define ODL_S4(text1, val1, text2, val2, text3, val3, text4, val4) \
        ODLogS4_(__FILE__, ODL_FUNC_NAME_, __LINE__, text1, val1, text2, val2, text3, val3, text4,\
                    val4)

#  if defined(__APPLE__)
/*! @brief Write a size to the log.
 @param caption The caption for the value to be written.
 @param size The value to be written. */
#   define ODL_SIZE(caption, size) \
        ODLogSize_(__FILE__, ODL_FUNC_NAME_,  __LINE__, caption, size)
#  endif // defined(__APPLE__)

/*! @brief Write a (possibly unterminated) string to the log.
 @param text The caption for the value to be written.
 @param len The number of bytes to be written.
 @param val The value to be written. */
#  define ODL_Sp(text, len, val) \
        ODLogSp_(__FILE__, ODL_FUNC_NAME_, __LINE__, text, (long) len, val)

#  if MAC_OR_LINUX_
/*! @brief Write a time value to the log.
 @param text1 The caption for the value to be written.
 @param val1 The value to be written. */
#   define ODL_TIME(text1, val1) \
        ODLogTime_(__FILE__, ODL_FUNC_NAME_, __LINE__, text1, val1)
#  endif // MAC_OR_LINUX_

/*! @brief Write a long hexadecimal value to the log.
 @param text1 The caption for the value to be written.
 @param val1 The value to be written. */
#  define ODL_X1(text1, val1) \
        ODLogX1_(__FILE__, ODL_FUNC_NAME_, __LINE__, text1, (long) (val1))

/*! @brief Write two long hexadecimal values to the log.
 @param text1 The caption for the first value to be written.
 @param val1 The first value to be written.
 @param text2 The caption for the second value to be written.
 @param val2 The second value to be written. */
#  define ODL_X2(text1, val1, text2, val2) \
        ODLogX2_(__FILE__, ODL_FUNC_NAME_, __LINE__, text1, (long) (val1), text2, (long) (val2))

/*! @brief Write three long hexadecimal values to the log.
 @param text1 The caption for the first value to be written.
 @param val1 The first value to be written.
 @param text2 The caption for the second value to be written.
 @param val2 The second value to be written.
 @param text3 The caption for the third value to be written.
 @param val3 The third value to be written. */
#  define ODL_X3(text1, val1, text2, val2, text3, val3) \
        ODLogX3_(__FILE__, ODL_FUNC_NAME_, __LINE__, text1, (long) (val1), text2, (long) (val2),\
                    text3, (long) (val3))

/*! @brief Write four long hexadecimal values to the log.
 @param text1 The caption for the first value to be written.
 @param val1 The first value to be written.
 @param text2 The caption for the second value to be written.
 @param val2 The second value to be written.
 @param text3 The caption for the third value to be written.
 @param val3 The third value to be written.
 @param text4 The caption for the fourth value to be written.
 @param val4 The fourth value to be written. */
#  define ODL_X4(text1, val1, text2, val2, text3, val3, text4, val4) \
        ODLogX4_(__FILE__, ODL_FUNC_NAME_, __LINE__, text1, (long) (val1), text2, (long) (val2),\
                    text3, (long) (val3), text4, (long) (val4))

/*! @brief Write a long long hexadecimal value to the log.
 @param text1 The caption for the value to be written.
 @param val1 The value to be written. */
#  define ODL_XL1(text1, val1) \
        ODLogXL1_(__FILE__, ODL_FUNC_NAME_, __LINE__, text1, (long int) (val1))

/*! @brief Write two long long hexadecimal values to the log.
 @param text1 The caption for the first value to be written.
 @param val1 The first value to be written.
 @param text2 The caption for the second value to be written.
 @param val2 The second value to be written. */
#  define ODL_XL2(text1, val1, text2, val2) \
        ODLogXL2_(__FILE__, ODL_FUNC_NAME_, __LINE__, text1, (long int) (val1), text2,\
                    (long int) (val2))

/*! @brief Write three long long hexadecimal values to the log.
 @param text1 The caption for the first value to be written.
 @param val1 The first value to be written.
 @param text2 The caption for the second value to be written.
 @param val2 The second value to be written.
 @param text3 The caption for the third value to be written.
 @param val3 The third value to be written. */
#  define ODL_XL3(text1, val1, text2, val2, text3, val3) \
        ODLogXL3_(__FILE__, ODL_FUNC_NAME_, __LINE__, text1, (long int) (val1), text2, \
                    (long int) (val2), text3, (long int) (val3))

/*! @brief Write four long long hexadecimal values to the log.
 @param text1 The caption for the first value to be written.
 @param val1 The first value to be written.
 @param text2 The caption for the second value to be written.
 @param val2 The second value to be written.
 @param text3 The caption for the third value to be written.
 @param val3 The third value to be written.
 @param text4 The caption for the fourth value to be written.
 @param val4 The fourth value to be written. */
#  define ODL_XL4(text1, val1, text2, val2, text3, val3, text4, val4) \
        ODLogXL4_(__FILE__, ODL_FUNC_NAME_, __LINE__, text1, (long int) (val1), text2, \
                    (long int) (val2), text3, (long int) (val3), text4, (long int) (val4))

#  if (! defined(ODL_DEFINITIONS_LOADED))
#   if defined(__cplusplus)
extern "C"
{
#   endif // defined(__cplusplus)

    /*! @brief Write a string to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param text The string to be written. */
    void
    ODLog_(const char * fileName,
           const char * funcName,
           const int    lineNumber,
           const char * text);

    /*! @brief Write a boolean value to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param text1 The caption for the value to be written.
     @param val1 The value to be written. */
    void
    ODLogB1_(const char * fileName,
             const char * funcName,
             const int    lineNumber,
             const char * text1,
             const bool   val1);

    /*! @brief Write two boolean values to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param text1 The caption for the first value to be written.
     @param val1 The first value to be written.
     @param text2 The caption for the second value to be written.
     @param val2 The second value to be written. */
    void
    ODLogB2_(const char * fileName,
             const char * funcName,
             const int    lineNumber,
             const char * text1,
             const bool   val1,
             const char * text2,
             const bool   val2);

    /*! @brief Write three boolean values to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param text1 The caption for the first value to be written.
     @param val1 The first value to be written.
     @param text2 The caption for the second value to be written.
     @param val2 The second value to be written.
     @param text3 The caption for the third value to be written.
     @param val3 The third value to be written. */
    void
    ODLogB3_(const char * fileName,
             const char * funcName,
             const int    lineNumber,
             const char * text1,
             const bool   val1,
             const char * text2,
             const bool   val2,
             const char * text3,
             const bool   val3);

    /*! @brief Write four boolean values to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param text1 The caption for the first value to be written.
     @param val1 The first value to be written.
     @param text2 The caption for the second value to be written.
     @param val2 The second value to be written.
     @param text3 The caption for the third value to be written.
     @param val3 The third value to be written.
     @param text4 The caption for the fourth value to be written.
     @param val4 The fourth value to be written. */
    void
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
             const bool   val4);

    /*! @brief Write a character value to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param text1 The caption for the value to be written.
     @param val1 The value to be written. */
    void
    ODLogC1_(const char * fileName,
             const char * funcName,
             const int    lineNumber,
             const char * text1,
             const char   val1);

    /*! @brief Write two character values to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param text1 The caption for the first value to be written.
     @param val1 The first value to be written.
     @param text2 The caption for the second value to be written.
     @param val2 The second value to be written. */
    void
    ODLogC2_(const char * fileName,
             const char * funcName,
             const int    lineNumber,
             const char * text1,
             const char   val1,
             const char * text2,
             const char   val2);

    /*! @brief Write three character values to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param text1 The caption for the first value to be written.
     @param val1 The first value to be written.
     @param text2 The caption for the second value to be written.
     @param val2 The second value to be written.
     @param text3 The caption for the third value to be written.
     @param val3 The third value to be written. */
    void
    ODLogC3_(const char * fileName,
             const char * funcName,
             const int    lineNumber,
             const char * text1,
             const char   val1,
             const char * text2,
             const char   val2,
             const char * text3,
             const char   val3);

    /*! @brief Write four character values to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param text1 The caption for the first value to be written.
     @param val1 The first value to be written.
     @param text2 The caption for the second value to be written.
     @param val2 The second value to be written.
     @param text3 The caption for the third value to be written.
     @param val3 The third value to be written.
     @param text4 The caption for the fourth value to be written.
     @param val4 The fourth value to be written. */
    void
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
             const char   val4);

    /*! @brief Write a double value to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param text1 The caption for the value to be written.
     @param val1 The value to be written. */
    void
    ODLogD1_(const char * fileName,
             const char * funcName,
             const int    lineNumber,
             const char * text1,
             const double val1);

    /*! @brief Write two double values to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param text1 The caption for the first value to be written.
     @param val1 The first value to be written.
     @param text2 The caption for the second value to be written.
     @param val2 The second value to be written. */
    void
    ODLogD2_(const char * fileName,
             const char * funcName,
             const int    lineNumber,
             const char * text1,
             const double val1,
             const char * text2,
             const double val2);

    /*! @brief Write three double values to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param text1 The caption for the first value to be written.
     @param val1 The first value to be written.
     @param text2 The caption for the second value to be written.
     @param val2 The second value to be written.
     @param text3 The caption for the third value to be written.
     @param val3 The third value to be written. */
    void
    ODLogD3_(const char * fileName,
             const char * funcName,
             const int    lineNumber,
             const char * text1,
             const double val1,
             const char * text2,
             const double val2,
             const char * text3,
             const double val3);

    /*! @brief Write four double values to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param text1 The caption for the first value to be written.
     @param val1 The first value to be written.
     @param text2 The caption for the second value to be written.
     @param val2 The second value to be written.
     @param text3 The caption for the third value to be written.
     @param val3 The third value to be written.
     @param text4 The caption for the fourth value to be written.
     @param val4 The fourth value to be written. */
    void
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
             const double val4);

    /*! @brief Write a function entry string to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs. */
    void
    ODLogEnter_(const char * fileName,
                const char * funcName,
                const int    lineNumber);

    /*! @brief Write a void function exit string to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs. */
    void
    ODLogExit_(const char * fileName,
               const char * funcName,
               const int    lineNumber);

    /*! @brief Write a boolean function exit string to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param val The value being returned by the function. */
    void
    ODLogExitB_(const char * fileName,
                const char * funcName,
                const int    lineNumber,
                const bool   val);

    /*! @brief Write a character function exit string to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param val The value being returned by the function. */
    void
    ODLogExitC_(const char * fileName,
                const char * funcName,
                const int    lineNumber,
                const char   val);

    /*! @brief Write a double function exit string to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param val The value being returned by the function. */
    void
    ODLogExitD_(const char * fileName,
                const char * funcName,
                const int    lineNumber,
                const double val);

    /*! @brief Write an exit function string to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param val The value being returned by the function. */
    void
    ODLogExitExit_(const char * fileName,
                   const char * funcName,
                   const int    lineNumber,
                   const long   val);

    /*! @brief Write a long function exit string to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param val The value being returned by the function. */
    void
    ODLogExitL_(const char *  fileName,
                const char *  funcName,
                const int     lineNumber,
                const int32_t val);

    /*! @brief Write a long long function exit string to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param val The value being returned by the function. */
    void
    ODLogExitLL_(const char *  fileName,
                 const char *  funcName,
                 const int     lineNumber,
                 const int64_t val);

#   if defined(__OBJC__)
    /*! @brief Write an object function exit string to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param val The value being returned by the function. */
    void
    ODLogExitO_(const char * fileName,
                const char * funcName,
                const int    lineNumber,
                const id     val);
#   endif // defined(__OBJC__)

    /*! @brief Write a pointer function exit string to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param val The value being returned by the function. */
    void
    ODLogExitP_(const char * fileName,
                const char * funcName,
                const int    lineNumber,
                const void * val);

#   if defined(__APPLE__)
    /*! @brief Write a rectangle function exit string to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param val The value being returned by the function. */
    void
    ODLogExitRect_(const char * fileName,
                   const char * funcName,
                   const int    lineNumber,
                   const CGRect val);
#   endif // defined(__APPLE__)

    /*! @brief Write a string function exit string to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param val The value being returned by the function. */
    void
    ODLogExitS_(const char * fileName,
                const char * funcName,
                const int    lineNumber,
                const char * val);

#   if defined(__APPLE__)
    /*! @brief Write a size function exit string to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param val The value being returned by the function. */
    void
    ODLogExitSize_(const char * fileName,
                   const char * funcName,
                   const int    lineNumber,
                   const CGSize val);
#   endif // defined(__APPLE__)

    /*! @brief Write a throw/long function exit string to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param val The value being thrown by the function. */
    void
    ODLogExitThrowL_(const char *  fileName,
                     const char *  funcName,
                     const int     lineNumber,
                     const int32_t val);

    /*! @brief Write a throw/string function exit string to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param val The value being thrown by the function. */
    void
    ODLogExitThrowS_(const char * fileName,
                     const char * funcName,
                     const int    lineNumber,
                     const char * val);

    /*! @brief Write a throw/long hexadecimal function exit string to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param val The value being thrown by the function. */
    void
    ODLogExitThrowX_(const char *  fileName,
                     const char *  funcName,
                     const int     lineNumber,
                     const int32_t val);

    /*! @brief Write a long hexadecimal function exit string to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param val The value being returned by the function. */
    void
    ODLogExitX_(const char *  fileName,
                const char *  funcName,
                const int     lineNumber,
                const int32_t val);

    /*! @brief Write a long long hexadecimal function exit string to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param val The value being returned by the function. */
    void
    ODLogExitXL_(const char *  fileName,
                 const char *  funcName,
                 const int     lineNumber,
                 const int64_t val);

    /*! @brief Set up the logging state.
     @param prefix The output prefix string to be applied.
     @param options The logging options to be applied.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs. */
    void
    ODLogInit_(const char * prefix,
               const int    options,
               const char * fileName,
               const char * funcName,
               const int    lineNumber);

    /*! @brief Write an IP address to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param text1 The caption for the value to be written.
     @param val1 The IP address value to be written.
     @param val2 The port value to be written. */
    void
    ODLogIP_(const char *  fileName,
             const char *  funcName,
             const int     lineNumber,
             const char *  text1,
             const int32_t val1,
             const int     val2);

    /*! @brief Write a long value to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param text1 The caption for the value to be written.
     @param val1 The value to be written. */
    void
    ODLogL1_(const char *  fileName,
             const char *  funcName,
             const int     lineNumber,
             const char *  text1,
             const int32_t val1);

    /*! @brief Write two long values to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param text1 The caption for the first value to be written.
     @param val1 The first value to be written.
     @param text2 The caption for the second value to be written.
     @param val2 The second value to be written. */
    void
    ODLogL2_(const char *  fileName,
             const char *  funcName,
             const int     lineNumber,
             const char *  text1,
             const int32_t val1,
             const char *  text2,
             const int32_t val2);

    /*! @brief Write three long values to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param text1 The caption for the first value to be written.
     @param val1 The first value to be written.
     @param text2 The caption for the second value to be written.
     @param val2 The second value to be written.
     @param text3 The caption for the third value to be written.
     @param val3 The third value to be written. */
    void
    ODLogL3_(const char *  fileName,
             const char *  funcName,
             const int     lineNumber,
             const char *  text1,
             const int32_t val1,
             const char *  text2,
             const int32_t val2,
             const char *  text3,
             const int32_t val3);

    /*! @brief Write four long values to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param text1 The caption for the first value to be written.
     @param val1 The first value to be written.
     @param text2 The caption for the second value to be written.
     @param val2 The second value to be written.
     @param text3 The caption for the third value to be written.
     @param val3 The third value to be written.
     @param text4 The caption for the fourth value to be written.
     @param val4 The fourth value to be written. */
    void
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
             const int32_t val4);

    /*! @brief Write a long long value to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param text1 The caption for the value to be written.
     @param val1 The value to be written. */
    void
    ODLogLL1_(const char *  fileName,
              const char *  funcName,
              const int     lineNumber,
              const char *  text1,
              const int64_t val1);

    /*! @brief Write two long long values to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param text1 The caption for the first value to be written.
     @param val1 The first value to be written.
     @param text2 The caption for the second value to be written.
     @param val2 The second value to be written. */
    void
    ODLogLL2_(const char *  fileName,
              const char *  funcName,
              const int     lineNumber,
              const char *  text1,
              const int64_t val1,
              const char *  text2,
              const int64_t val2);

    /*! @brief Write three long long values to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param text1 The caption for the first value to be written.
     @param val1 The first value to be written.
     @param text2 The caption for the second value to be written.
     @param val2 The second value to be written.
     @param text3 The caption for the third value to be written.
     @param val3 The third value to be written. */
    void
    ODLogLL3_(const char *  fileName,
              const char *  funcName,
              const int     lineNumber,
              const char *  text1,
              const int64_t val1,
              const char *  text2,
              const int64_t val2,
              const char *  text3,
              const int64_t val3);

    /*! @brief Write four long long values to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param text1 The caption for the first value to be written.
     @param val1 The first value to be written.
     @param text2 The caption for the second value to be written.
     @param val2 The second value to be written.
     @param text3 The caption for the third value to be written.
     @param val3 The third value to be written.
     @param text4 The caption for the fourth value to be written.
     @param val4 The fourth value to be written. */
    void
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
              const int64_t val4);

    /*! @brief Write a long string value to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param text1 The caption for the value to be written.
     @param val1 The value to be written. */
    void
    ODLogLS_(const char * fileName,
             const char * funcName,
             const int    lineNumber,
             const char * text1,
             const char * val1);

#   if defined(__OBJC__)
    /*! @brief Write an object value to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param text1 The caption for the value to be written.
     @param obj1 The value to be written. */
    void
    ODLogO1_(const char * fileName,
             const char * funcName,
             const int    lineNumber,
             const char * text1,
             const id     obj1);

    /*! @brief Write two object values to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param text1 The caption for the first value to be written.
     @param obj1 The value to be written.
     @param text2 The caption for the second value to be written.
     @param obj2 The second value to be written. */
    void
    ODLogO2_(const char * fileName,
             const char * funcName,
             const int    lineNumber,
             const char * text1,
             const id     obj1,
             const char * text2,
             const id     obj2);

    /*! @brief Write three object values to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param text1 The caption for the first value to be written.
     @param obj1 The value to be written.
     @param text2 The caption for the second value to be written.
     @param obj2 The second value to be written.
     @param text3 The caption for the third value to be written.
     @param obj3 The third value to be written. */
    void
    ODLogO3_(const char * fileName,
             const char * funcName,
             const int    lineNumber,
             const char * text1,
             const id     obj1,
             const char * text2,
             const id     obj2,
             const char * text3,
             const id     obj3);

    /*! @brief Write four object values to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param text1 The caption for the first value to be written.
     @param obj1 The value to be written.
     @param text2 The caption for the second value to be written.
     @param obj2 The second value to be written.
     @param text3 The caption for the third value to be written.
     @param obj3 The third value to be written.
     @param text4 The caption for the fourth value to be written.
     @param obj4 The fourth value to be written. */
    void
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
             const id     obj4);
#   endif // defined(__OBJC__)

    /*! @brief Write a method entry string to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param objPtr The this/self pointer for the caller.*/
    void
    ODLogObjEnter_(const char * fileName,
                   const char * funcName,
                   const int    lineNumber,
                   const void * objPtr);

    /*! @brief Write a void method exit string to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param objPtr The this/self pointer for the caller.*/
    void
    ODLogObjExit_(const char * fileName,
                  const char * funcName,
                  const int    lineNumber,
                  const void * objPtr);

    /*! @brief Write a boolean method exit string to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param objPtr The this/self pointer for the caller.
     @param val The value being returned by the method. */
    void
    ODLogObjExitB_(const char * fileName,
                   const char * funcName,
                   const int    lineNumber,
                   const void * objPtr,
                   const bool   val);

    /*! @brief Write a character method exit string to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param objPtr The this/self pointer for the caller.
     @param val The value being returned by the method. */
    void
    ODLogObjExitC_(const char * fileName,
                   const char * funcName,
                   const int    lineNumber,
                   const void * objPtr,
                   const char   val);

    /*! @brief Write a double method exit string to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param objPtr The this/self pointer for the caller.
     @param val The value being returned by the method. */
    void
    ODLogObjExitD_(const char * fileName,
                   const char * funcName,
                   const int    lineNumber,
                   const void * objPtr,
                   const double val);

    /*! @brief Write an exit method string to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param objPtr The this/self pointer for the caller.
     @param val The value being returned by the method. */
    void
    ODLogObjExitExit_(const char * fileName,
                      const char * funcName,
                      const int    lineNumber,
                      const void * objPtr,
                      const long   val);

    /*! @brief Write a long method exit string to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param objPtr The this/self pointer for the caller.
     @param val The value being returned by the method. */
    void
    ODLogObjExitL_(const char *  fileName,
                   const char *  funcName,
                   const int     lineNumber,
                   const void *  objPtr,
                   const int32_t val);

    /*! @brief Write a long long method exit string to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param objPtr The this/self pointer for the caller.
     @param val The value being returned by the method. */
    void
    ODLogObjExitLL_(const char *  fileName,
                    const char *  funcName,
                    const int     lineNumber,
                    const void *  objPtr,
                    const int64_t val);

#   if defined(__OBJC__)
    /*! @brief Write an object method exit string to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param objPtr The this/self pointer for the caller.
     @param val The value being returned by the method. */
    void
    ODLogObjExitO_(const char * fileName,
                   const char * funcName,
                   const int    lineNumber,
                   const void * objPtr,
                   const id     val);
#   endif // defined(__OBJC__)

    /*! @brief Write a pointer method exit string to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param objPtr The this/self pointer for the caller.
     @param val The value being returned by the method. */
    void
    ODLogObjExitP_(const char * fileName,
                   const char * funcName,
                   const int    lineNumber,
                   const void * objPtr,
                   const void * val);

#   if defined(__APPLE__)
    /*! @brief Write a rectangle method exit string to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param objPtr The this/self pointer for the caller.
     @param val The value being returned by the method. */
    void
    ODLogObjExitRect_(const char * fileName,
                      const char * funcName,
                      const int    lineNumber,
                      const void * objPtr,
                      const CGRect val);
#   endif // defined(__APPLE__)

    /*! @brief Write a string method exit string to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param objPtr The this/self pointer for the caller.
     @param val The value being returned by the method. */
    void
    ODLogObjExitS_(const char * fileName,
                   const char * funcName,
                   const int    lineNumber,
                   const void * objPtr,
                   const char * val);

#   if defined(__APPLE__)
    /*! @brief Write a size method exit string to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param objPtr The this/self pointer for the caller.
     @param val The value being returned by the method. */
    void
    ODLogObjExitSize_(const char * fileName,
                      const char * funcName,
                      const int    lineNumber,
                      const void * objPtr,
                      const CGSize val);
#   endif // defined(__APPLE__)

    /*! @brief Write a throw/long method exit string to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param objPtr The this/self pointer for the caller.
     @param val The value being thrown by the method. */
    void
    ODLogObjExitThrowL_(const char *  fileName,
                        const char *  funcName,
                        const int     lineNumber,
                        const void *  objPtr,
                        const int32_t val);

    /*! @brief Write a throw/string method exit string to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param objPtr The this/self pointer for the caller.
     @param val The value being thrown by the method. */
    void
    ODLogObjExitThrowS_(const char * fileName,
                        const char * funcName,
                        const int    lineNumber,
                        const void * objPtr,
                        const char * val);

    /*! @brief Write a throw/long hexadecimal method exit string to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param objPtr The this/self pointer for the caller.
     @param val The value being thrown by the method. */
    void
    ODLogObjExitThrowX_(const char *  fileName,
                        const char *  funcName,
                        const int     lineNumber,
                        const void *  objPtr,
                        const int32_t val);

    /*! @brief Write a long hexadecimal method exit string to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param objPtr The this/self pointer for the caller.
     @param val The value being returned by the method. */
    void
    ODLogObjExitX_(const char *  fileName,
                   const char *  funcName,
                   const int     lineNumber,
                   const void *  objPtr,
                   const int32_t val);

    /*! @brief Write a long long hexadecimal method exit string to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param objPtr The this/self pointer for the caller.
     @param val The value being returned by the method. */
    void
    ODLogObjExitXL_(const char *  fileName,
                    const char *  funcName,
                    const int     lineNumber,
                    const void *  objPtr,
                    const int64_t val);

    /*! @brief Write a pointer value to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param text1 The caption for the value to be written.
     @param ptr1 The value to be written. */
    void
    ODLogP1_(const char * fileName,
             const char * funcName,
             const int    lineNumber,
             const char * text1,
             const void * ptr1);

    /*! @brief Write two pointer values to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param text1 The caption for the first value to be written.
     @param ptr1 The value to be written.
     @param text2 The caption for the second value to be written.
     @param ptr2 The second value to be written. */
    void
    ODLogP2_(const char * fileName,
             const char * funcName,
             const int    lineNumber,
             const char * text1,
             const void * ptr1,
             const char * text2,
             const void * ptr2);

    /*! @brief Write three pointer values to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param text1 The caption for the first value to be written.
     @param ptr1 The value to be written.
     @param text2 The caption for the second value to be written.
     @param ptr2 The second value to be written.
     @param text3 The caption for the third value to be written.
     @param ptr3 The third value to be written. */
    void
    ODLogP3_(const char * fileName,
             const char * funcName,
             const int    lineNumber,
             const char * text1,
             const void * ptr1,
             const char * text2,
             const void * ptr2,
             const char * text3,
             const void * ptr3);

    /*! @brief Write four pointer values to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param text1 The caption for the first value to be written.
     @param ptr1 The value to be written.
     @param text2 The caption for the second value to be written.
     @param ptr2 The second value to be written.
     @param text3 The caption for the third value to be written.
     @param ptr3 The third value to be written.
     @param text4 The caption for the fourth value to be written.
     @param ptr4 The fourth value to be written. */
    void
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
             const void * ptr4);

    /*! @brief Write a region of memory to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param caption The caption for the region to be written.
     @param buffer The starting address of the region.
     @param size The number of bytes to be written. */
    void
    ODLogPacket_(const char * fileName,
                 const char * funcName,
                 const int    lineNumber,
                 const char * caption,
                 const char * buffer,
                 const int    size);

#   if defined(__APPLE__)
    /*! @brief Write a rectangle to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param caption The caption for the value to be written.
     @param rect The value to be written. */
    void
    ODLogRect_(const char * fileName,
               const char * funcName,
               const int    lineNumber,
               const char * caption,
               const CGRect rect);
#   endif // defined(__APPLE__)

    /*! @brief Write a string value to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param text1 The caption for the value to be written.
     @param val1 The value to be written. */
    void
    ODLogS1_(const char * fileName,
             const char * funcName,
             const int    lineNumber,
             const char * text1,
             const char * val1);

    /*! @brief Write two string values to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param text1 The caption for the first value to be written.
     @param val1 The first value to be written.
     @param text2 The caption for the second value to be written.
     @param val2 The second value to be written. */
    void
    ODLogS2_(const char * fileName,
             const char * funcName,
             const int    lineNumber,
             const char * text1,
             const char * val1,
             const char * text2,
             const char * val2);

    /*! @brief Write three string values to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param text1 The caption for the first value to be written.
     @param val1 The first value to be written.
     @param text2 The caption for the second value to be written.
     @param val2 The second value to be written.
     @param text3 The caption for the third value to be written.
     @param val3 The third value to be written. */
    void
    ODLogS3_(const char * fileName,
             const char * funcName,
             const int    lineNumber,
             const char * text1,
             const char * val1,
             const char * text2,
             const char * val2,
             const char * text3,
             const char * val3);

    /*! @brief Write four string values to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param text1 The caption for the first value to be written.
     @param val1 The first value to be written.
     @param text2 The caption for the second value to be written.
     @param val2 The second value to be written.
     @param text3 The caption for the third value to be written.
     @param val3 The third value to be written.
     @param text4 The caption for the fourth value to be written.
     @param val4 The fourth value to be written. */
    void
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
             const char * val4);

#   if defined(__APPLE__)
    /*! @brief Write a size to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param caption The caption for the value to be written.
     @param size The value to be written. */
    void
    ODLogSize_(const char * fileName,
               const char * funcName,
               const int    lineNumber,
               const char * caption,
               const CGSize size);
#   endif // defined(__APPLE__)

    /*! @brief Write a (possibly unterminated) string to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param text The caption for the value to be written.
     @param len The number of bytes to be written.
     @param val The value to be written. */
    void
    ODLogSp_(const char * fileName,
             const char * funcName,
             const int    lineNumber,
             const char * text,
             const int    len,
             const char * val);

#   if MAC_OR_LINUX_
    /*! @brief Write a time value to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param text1 The caption for the value to be written.
     @param val1 The value to be written. */
    void
    ODLogTime_(const char *           fileName,
               const char *           funcName,
               const int              lineNumber,
               const char *           text1,
               const struct timeval * val1);
#   endif // MAC_OR_LINUX_

    /*! @brief Write a long hexadecimal value to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param text1 The caption for the value to be written.
     @param val1 The value to be written. */
    void
    ODLogX1_(const char *  fileName,
             const char *  funcName,
             const int     lineNumber,
             const char *  text1,
             const int32_t val1);

    /*! @brief Write two long hexadecimal values to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param text1 The caption for the first value to be written.
     @param val1 The first value to be written.
     @param text2 The caption for the second value to be written.
     @param val2 The second value to be written. */
    void
    ODLogX2_(const char *  fileName,
             const char *  funcName,
             const int     lineNumber,
             const char *  text1,
             const int32_t val1,
             const char *  text2,
             const int32_t val2);

    /*! @brief Write three long hexadecimal values to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param text1 The caption for the first value to be written.
     @param val1 The first value to be written.
     @param text2 The caption for the second value to be written.
     @param val2 The second value to be written.
     @param text3 The caption for the third value to be written.
     @param val3 The third value to be written. */
    void
    ODLogX3_(const char *  fileName,
             const char *  funcName,
             const int     lineNumber,
             const char *  text1,
             const int32_t val1,
             const char *  text2,
             const int32_t val2,
             const char *  text3,
             const int32_t val3);

    /*! @brief Write four long hexadecimal values to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param text1 The caption for the first value to be written.
     @param val1 The first value to be written.
     @param text2 The caption for the second value to be written.
     @param val2 The second value to be written.
     @param text3 The caption for the third value to be written.
     @param val3 The third value to be written.
     @param text4 The caption for the fourth value to be written.
     @param val4 The fourth value to be written. */
    void
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
             const int32_t val4);

    /*! @brief Write a long long hexadecimal value to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param text1 The caption for the value to be written.
     @param val1 The value to be written. */
    void
    ODLogXL1_(const char *  fileName,
              const char *  funcName,
              const int     lineNumber,
              const char *  text1,
              const int64_t val1);

    /*! @brief Write two long long hexadecimal values to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param text1 The caption for the first value to be written.
     @param val1 The first value to be written.
     @param text2 The caption for the second value to be written.
     @param val2 The second value to be written. */
    void
    ODLogXL2_(const char *  fileName,
              const char *  funcName,
              const int     lineNumber,
              const char *  text1,
              const int64_t val1,
              const char *  text2,
              const int64_t val2);

    /*! @brief Write three long long hexadecimal values to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param text1 The caption for the first value to be written.
     @param val1 The first value to be written.
     @param text2 The caption for the second value to be written.
     @param val2 The second value to be written.
     @param text3 The caption for the third value to be written.
     @param val3 The third value to be written. */
    void
    ODLogXL3_(const char *  fileName,
              const char *  funcName,
              const int     lineNumber,
              const char *  text1,
              const int64_t val1,
              const char *  text2,
              const int64_t val2,
              const char *  text3,
              const int64_t val3);

    /*! @brief Write four long long hexadecimal values to the log.
     @param fileName The name of the source file containing the call to this function.
     @param funcName The name of the calling function.
     @param lineNumber The line number in the source file where the call occurs.
     @param text1 The caption for the first value to be written.
     @param val1 The first value to be written.
     @param text2 The caption for the second value to be written.
     @param val2 The second value to be written.
     @param text3 The caption for the third value to be written.
     @param val3 The third value to be written.
     @param text4 The caption for the fourth value to be written.
     @param val4 The fourth value to be written. */
    void
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
              const int64_t val4);

#   if defined(__cplusplus)
}
#   endif // defined(__cplusplus)
#  endif // ! defined(ODL_DEFINITIONS_LOADED)
# else // ! defined(ODL_ENABLE_LOGGING_)
#  if defined(__OBJC__)
 /* Return the string description of an Objective-C object. */
#   define ODL_OBJPRINTABLE_STRING(xx) ""
#  endif // defined(__OBJC__)

/*! @brief Write a string to the log.
 @param text The string to be written. */
#  define ODL_LOG(text) /* */

/*! @brief Write a boolean value to the log.
 @param text1 The caption for the value to be written.
 @param val1 The value to be written. */
#  define ODL_B1(text1, val1) /* */

/*! @brief Write two boolean values to the log.
 @param text1 The caption for the first value to be written.
 @param val1 The first value to be written.
 @param text2 The caption for the second value to be written.
 @param val2 The second value to be written. */
#  define ODL_B2(text1, val1, text2, val2) /* */

/*! @brief Write three boolean values to the log.
 @param text1 The caption for the first value to be written.
 @param val1 The first value to be written.
 @param text2 The caption for the second value to be written.
 @param val2 The second value to be written.
 @param text3 The caption for the third value to be written.
 @param val3 The third value to be written. */
#  define ODL_B3(text1, val1, text2, val2, text3, val3) /* */

/*! @brief Write four boolean values to the log.
 @param text1 The caption for the first value to be written.
 @param val1 The first value to be written.
 @param text2 The caption for the second value to be written.
 @param val2 The second value to be written.
 @param text3 The caption for the third value to be written.
 @param val3 The third value to be written.
 @param text4 The caption for the fourth value to be written.
 @param val4 The fourth value to be written. */
#  define ODL_B4(text1, val1, text2, val2, text3, val3, text4, val4) /* */

/*! @brief Write a character value to the log.
 @param text1 The caption for the value to be written.
 @param val1 The value to be written. */
#  define ODL_C1(text1, val1) /* */

/*! @brief Write two character values to the log.
 @param text1 The caption for the first value to be written.
 @param val1 The first value to be written.
 @param text2 The caption for the second value to be written.
 @param val2 The second value to be written. */
#  define ODL_C2(text1, val1, text2, val2) /* */

/*! @brief Write three character values to the log.
 @param text1 The caption for the first value to be written.
 @param val1 The first value to be written.
 @param text2 The caption for the second value to be written.
 @param val2 The second value to be written.
 @param text3 The caption for the third value to be written.
 @param val3 The third value to be written. */
#  define ODL_C3(text1, val1, text2, val2, text3, val3) /* */

/*! @brief Write four character values to the log.
 @param text1 The caption for the first value to be written.
 @param val1 The first value to be written.
 @param text2 The caption for the second value to be written.
 @param val2 The second value to be written.
 @param text3 The caption for the third value to be written.
 @param val3 The third value to be written.
 @param text4 The caption for the fourth value to be written.
 @param val4 The fourth value to be written. */
#  define ODL_C4(text1, val1, text2, val2, text3, val3, text4, val4) /* */

/*! @brief Write a double value to the log.
 @param text1 The caption for the value to be written.
 @param val1 The value to be written. */
#  define ODL_D1(text1, val1) /* */

/*! @brief Write two double values to the log.
 @param text1 The caption for the first value to be written.
 @param val1 The first value to be written.
 @param text2 The caption for the second value to be written.
 @param val2 The second value to be written. */
#  define ODL_D2(text1, val1, text2, val2) /* */

/*! @brief Write three double values to the log.
 @param text1 The caption for the first value to be written.
 @param val1 The first value to be written.
 @param text2 The caption for the second value to be written.
 @param val2 The second value to be written.
 @param text3 The caption for the third value to be written.
 @param val3 The third value to be written. */
#  define ODL_D3(text1, val1, text2, val2, text3, val3) /* */

/*! @brief Write four double values to the log.
 @param text1 The caption for the first value to be written.
 @param val1 The first value to be written.
 @param text2 The caption for the second value to be written.
 @param val2 The second value to be written.
 @param text3 The caption for the third value to be written.
 @param val3 The third value to be written.
 @param text4 The caption for the fourth value to be written.
 @param val4 The fourth value to be written. */
#  define ODL_D4(text1, val1, text2, val2, text3, val3, text4, val4) /* */

/*! @brief Write a function entry string to the log. */
#  define ODL_ENTER() /* */

/*! @brief Write a void function exit string to the log. */
#  define ODL_EXIT() /* */

/*! @brief Write a boolean function exit string to the log.
 @param val The value being returned by the function. */
#  define ODL_EXIT_B(val) /* */

/*! @brief Write a character function exit string to the log.
 @param val The value being returned by the function. */
#  define ODL_EXIT_C(val) /* */

/*! @brief Write a double function exit string to the log.
 @param val The value being returned by the function. */
#  define ODL_EXIT_D(val) /* */

/*! @brief Write an exit function string to the log.
 @param val The value being returned by the function. */
#  define ODL_EXIT_EXIT(val) /* */

/*! @brief Write a long function exit string to the log.
 @param val The value being returned by the function. */
#  define ODL_EXIT_L(val) /* */

/*! @brief Write a long long function exit string to the log.
 @param val The value being returned by the function. */
#  define ODL_EXIT_LL(val) /* */

#  if defined(__OBJC__)
/*! @brief Write an object function exit string to the log.
 @param val The value being returned by the function. */
#   define ODL_EXIT_O(val) /* */
#  endif // defined(__OBJC__)

/*! @brief Write a pointer function exit string to the log.
 @param val The value being returned by the function. */
#  define ODL_EXIT_P(val) /* */

#  if defined(__APPLE__)
/*! @brief Write a rectangle function exit string to the log.
 @param val The value being returned by the function. */
#   define ODL_EXIT_RECT(val) /* */
#  endif // defined(__APPLE__)

/*! @brief Write a string function exit string to the log.
 @param val The value being returned by the function. */
#  define ODL_EXIT_S(val) /* */

#  if defined(__APPLE__)
/*! @brief Write a size function exit string to the log.
 @param val The value being returned by the function. */
#   define ODL_EXIT_SIZE(val) /* */
#  endif // defined(__APPLE__)

/*! @brief Write a throw/long function exit string to the log.
 @param val The value being thrown by the function. */
#  define ODL_EXIT_THROW_L(val) /* */

/*! @brief Write a throw/string function exit string to the log.
 @param val The value being thrown by the function. */
#  define ODL_EXIT_THROW_S(val) /* */

/*! @brief Write a throw/long hexadecimal function exit string to the log.
 @param val The value being thrown by the function. */
#  define ODL_EXIT_THROW_X(val) /* */

/*! @brief Write a long hexadecimal function exit string to the log.
 @param val The value being returned by the function. */
#  define ODL_EXIT_X(val) /* */

/*! @brief Write a long long hexadecimal function exit string to the log.
 @param val The value being returned by the function. */
#  define ODL_EXIT_XL(val) /* */

/*! @brief Set up the logging state.
 @param prefix The output prefix string to be applied.
 @param options The logging options to be applied. */
#  define ODL_INIT(prefix, options) /* */

/*! @brief Write an IP address to the log.
 @param text1 The caption for the value to be written.
 @param val1 The IP address value to be written.
 @param val2 The port value to be written. */
#  define ODL_IP(text1, val1, val2) /* */

/*! @brief Write a long value to the log.
 @param text1 The caption for the value to be written.
 @param val1 The value to be written. */
#  define ODL_L1(text1, val1) /* */

/*! @brief Write two long values to the log.
 @param text1 The caption for the first value to be written.
 @param val1 The first value to be written.
 @param text2 The caption for the second value to be written.
 @param val2 The second value to be written. */
#  define ODL_L2(text1, val1, text2, val2) /* */

/*! @brief Write three long values to the log.
 @param text1 The caption for the first value to be written.
 @param val1 The first value to be written.
 @param text2 The caption for the second value to be written.
 @param val2 The second value to be written.
 @param text3 The caption for the third value to be written.
 @param val3 The third value to be written. */
#  define ODL_L3(text1, val1, text2, val2, text3, val3) /* */

/*! @brief Write four long values to the log.
 @param text1 The caption for the first value to be written.
 @param val1 The first value to be written.
 @param text2 The caption for the second value to be written.
 @param val2 The second value to be written.
 @param text3 The caption for the third value to be written.
 @param val3 The third value to be written.
 @param text4 The caption for the fourth value to be written.
 @param val4 The fourth value to be written. */
#  define ODL_L4(text1, val1, text2, val2, text3, val3, text4, val4) /* */

/*! @brief Write a long long value to the log.
 @param text1 The caption for the value to be written.
 @param val1 The value to be written. */
#  define ODL_LL1(text1, val1) /* */

/*! @brief Write two long long values to the log.
 @param text1 The caption for the first value to be written.
 @param val1 The first value to be written.
 @param text2 The caption for the second value to be written.
 @param val2 The second value to be written. */
#  define ODL_LL2(text1, val1, text2, val2) /* */

/*! @brief Write three long long values to the log.
 @param text1 The caption for the first value to be written.
 @param val1 The first value to be written.
 @param text2 The caption for the second value to be written.
 @param val2 The second value to be written.
 @param text3 The caption for the third value to be written.
 @param val3 The third value to be written. */
#  define ODL_LL3(text1, val1, text2, val2, text3, val3) /* */

/*! @brief Write four long long values to the log.
 @param text1 The caption for the first value to be written.
 @param val1 The first value to be written.
 @param text2 The caption for the second value to be written.
 @param val2 The second value to be written.
 @param text3 The caption for the third value to be written.
 @param val3 The third value to be written.
 @param text4 The caption for the fourth value to be written.
 @param val4 The fourth value to be written. */
#  define ODL_LL4(text1, val1, text2, val2, text3, val3, text4, val4) /* */

/*! @brief Write a long string value to the log.
 @param text1 The caption for the value to be written.
 @param val1 The value to be written. */
#  define ODL_LS(text1, val1) /* */

#  if defined(__OBJC__)
/*! @brief Write an object value to the log.
 @param text1 The caption for the value to be written.
 @param obj1 The value to be written. */
#   define ODL_O1(text1, obj1) /* */

/*! @brief Write two object values to the log.
 @param text1 The caption for the first value to be written.
 @param obj1 The value to be written.
 @param text2 The caption for the second value to be written.
 @param obj2 The second value to be written. */
#   define ODL_O2(text1, obj1, text2, obj2) /* */

/*! @brief Write three object values to the log.
 @param text1 The caption for the first value to be written.
 @param obj1 The value to be written.
 @param text2 The caption for the second value to be written.
 @param obj2 The second value to be written.
 @param text3 The caption for the third value to be written.
 @param obj3 The third value to be written. */
#   define ODL_O3(text1, obj1, text2, obj2, text3, obj3) /* */

/*! @brief Write four object values to the log.
 @param text1 The caption for the first value to be written.
 @param obj1 The value to be written.
 @param text2 The caption for the second value to be written.
 @param obj2 The second value to be written.
 @param text3 The caption for the third value to be written.
 @param obj3 The third value to be written.
 @param text4 The caption for the fourth value to be written.
 @param obj4 The fourth value to be written. */
#   define ODL_O4(text1, obj1, text2, obj2, text3, obj3, text4, obj4) /* */
#  endif // defined(__OBJC__)

/*! @brief Write a method entry string to the log. */
#  define ODL_OBJENTER() /* */

/*! @brief Write a void method exit string to the log. */
#  define ODL_OBJEXIT() /* */

/*! @brief Write a boolean method exit string to the log.
 @param val The value being returned by the method. */
#  define ODL_OBJEXIT_B(val) /* */

/*! @brief Write a character method exit string to the log.
 @param val The value being returned by the method. */
#  define ODL_OBJEXIT_C(val) /* */

/*! @brief Write a double method exit string to the log.
 @param val The value being returned by the method. */
#  define ODL_OBJEXIT_D(val) /* */

/*! @brief Write an exit method string to the log.
 @param val The value being returned by the method. */
#  define ODL_OBJEXIT_EXIT(val) /* */

/*! @brief Write a long method exit string to the log.
 @param val The value being returned by the method. */
#  define ODL_OBJEXIT_L(val) /* */

/*! @brief Write a long long method exit string to the log.
 @param val The value being returned by the method. */
#  define ODL_OBJEXIT_LL(val) /* */

#  if defined(__OBJC__)
/*! @brief Write an object method exit string to the log.
 @param val The value being returned by the method. */
#   define ODL_OBJEXIT_O(val) /* */
#  endif // defined(__OBJC__)

/*! @brief Write a pointer method exit string to the log.
 @param val The value being returned by the method. */
#  define ODL_OBJEXIT_P(val) /* */

#  if defined(__APPLE__)
/*! @brief Write a rectangle method exit string to the log.
 @param val The value being returned by the method. */
#   define ODL_OBJEXIT_RECT(val) /* */
#  endif // defined(__APPLE__)

/*! @brief Write a string method exit string to the log.
 @param val The value being returned by the method. */
#  define ODL_OBJEXIT_S(val) /* */

#  if defined(__APPLE__)
/*! @brief Write a size method exit string to the log.
 @param val The value being returned by the method. */
#   define ODL_OBJEXIT_SIZE(val) /* */
#  endif // defined(__APPLE__)

/*! @brief Write a throw/long method exit string to the log.
 @param val The value being thrown by the method. */
#  define ODL_OBJEXIT_THROW_L(val) /* */

/*! @brief Write a throw/string method exit string to the log.
 @param val The value being thrown by the method. */
#  define ODL_OBJEXIT_THROW_S(val) /* */

/*! @brief Write a throw/long hexadecimal method exit string to the log.
 @param val The value being thrown by the method. */
#  define ODL_OBJEXIT_THROW_X(val) /* */

/*! @brief Write a long hexadecimal method exit string to the log.
 @param val The value being returned by the method. */
#  define ODL_OBJEXIT_X(val) /* */

/*! @brief Write a long long hexadecimal method exit string to the log.
 @param val The value being returned by the method. */
#  define ODL_OBJEXIT_XL(val) /* */

/*! @brief Write a pointer value to the log.
 @param text1 The caption for the value to be written.
 @param ptr1 The value to be written. */
#  define ODL_P1(text1, ptr1) /* */

/*! @brief Write two pointer values to the log.
 @param text1 The caption for the first value to be written.
 @param ptr1 The value to be written.
 @param text2 The caption for the second value to be written.
 @param ptr2 The second value to be written. */
#  define ODL_P2(text1, ptr1, text2, ptr2) /* */

/*! @brief Write three pointer values to the log.
 @param text1 The caption for the first value to be written.
 @param ptr1 The value to be written.
 @param text2 The caption for the second value to be written.
 @param ptr2 The second value to be written.
 @param text3 The caption for the third value to be written.
 @param ptr3 The third value to be written. */
#  define ODL_P3(text1, ptr1, text2, ptr2, text3, ptr3) /* */

/*! @brief Write four pointer values to the log.
 @param text1 The caption for the first value to be written.
 @param ptr1 The value to be written.
 @param text2 The caption for the second value to be written.
 @param ptr2 The second value to be written.
 @param text3 The caption for the third value to be written.
 @param ptr3 The third value to be written.
 @param text4 The caption for the fourth value to be written.
 @param ptr4 The fourth value to be written. */
#  define ODL_P4(text1, ptr1, text2, ptr2, text3, ptr3, text4, ptr4) /* */

/*! @brief Write a region of memory to the log.
 @param caption The caption for the region to be written.
 @param buffer The starting address of the region.
 @param size The number of bytes to be written. */
#  define ODL_PACKET(caption, buffer, size) /* */

#  if defined(__APPLE__)
/*! @brief Write a rectangle to the log.
 @param caption The caption for the value to be written.
 @param rect The value to be written. */
#   define ODL_RECT(caption, rect) /* */
#  endif // defined(__APPLE__)

/*! @brief Write a string value to the log.
 @param text1 The caption for the value to be written.
 @param val1 The value to be written. */
#  define ODL_S1(text1, val1) /* */

/*! @brief Write two string values to the log.
 @param text1 The caption for the first value to be written.
 @param val1 The first value to be written.
 @param text2 The caption for the second value to be written.
 @param val2 The second value to be written. */
#  define ODL_S2(text1, val1, text2, val2) /* */

/*! @brief Write three string values to the log.
 @param text1 The caption for the first value to be written.
 @param val1 The first value to be written.
 @param text2 The caption for the second value to be written.
 @param val2 The second value to be written.
 @param text3 The caption for the third value to be written.
 @param val3 The third value to be written. */
#  define ODL_S3(text1, val1, text2, val2, text3, val3) /* */

/*! @brief Write four string values to the log.
 @param text1 The caption for the first value to be written.
 @param val1 The first value to be written.
 @param text2 The caption for the second value to be written.
 @param val2 The second value to be written.
 @param text3 The caption for the third value to be written.
 @param val3 The third value to be written.
 @param text4 The caption for the fourth value to be written.
 @param val4 The fourth value to be written. */
#  define ODL_S4(text1, val1, text2, val2, text3, val3, text4, val4) /* */

#  if defined(__APPLE__)
/*! @brief Write a size to the log.
 @param caption The caption for the value to be written.
 @param size The value to be written. */
#   define ODL_SIZE(caption, size) /* */
#  endif // defined(__APPLE__)

/*! @brief Write a (possibly unterminated) string to the log.
 @param text The caption for the value to be written.
 @param len The number of bytes to be written.
 @param val The value to be written. */
#  define ODL_Sp(text, len, val) /* */

#  if MAC_OR_LINUX_
/*! @brief Write a time value to the log.
 @param text1 The caption for the value to be written.
 @param val1 The value to be written. */
#   define ODL_TIME(text1, val1) /* */
#  endif // MAC_OR_LINUX_

/*! @brief Write a long hexadecimal value to the log.
 @param text1 The caption for the value to be written.
 @param val1 The value to be written. */
#  define ODL_X1(text1, val1) /* */

/*! @brief Write two long hexadecimal values to the log.
 @param text1 The caption for the first value to be written.
 @param val1 The first value to be written.
 @param text2 The caption for the second value to be written.
 @param val2 The second value to be written. */
#  define ODL_X2(text1, val1, text2, val2) /* */

/*! @brief Write three long hexadecimal values to the log.
 @param text1 The caption for the first value to be written.
 @param val1 The first value to be written.
 @param text2 The caption for the second value to be written.
 @param val2 The second value to be written.
 @param text3 The caption for the third value to be written.
 @param val3 The third value to be written. */
#  define ODL_X3(text1, val1, text2, val2, text3, val3) /* */

/*! @brief Write four long hexadecimal values to the log.
 @param text1 The caption for the first value to be written.
 @param val1 The first value to be written.
 @param text2 The caption for the second value to be written.
 @param val2 The second value to be written.
 @param text3 The caption for the third value to be written.
 @param val3 The third value to be written.
 @param text4 The caption for the fourth value to be written.
 @param val4 The fourth value to be written. */
#  define ODL_X4(text1, val1, text2, val2, text3, val3, text4, val4) /* */

/*! @brief Write a long long hexadecimal value to the log.
 @param text1 The caption for the value to be written.
 @param val1 The value to be written. */
#  define ODL_XL1(text1, val1) /* */

/*! @brief Write two long long hexadecimal values to the log.
 @param text1 The caption for the first value to be written.
 @param val1 The first value to be written.
 @param text2 The caption for the second value to be written.
 @param val2 The second value to be written. */
#  define ODL_XL2(text1, val1, text2, val2) /* */

/*! @brief Write three long long hexadecimal values to the log.
 @param text1 The caption for the first value to be written.
 @param val1 The first value to be written.
 @param text2 The caption for the second value to be written.
 @param val2 The second value to be written.
 @param text3 The caption for the third value to be written.
 @param val3 The third value to be written. */
#  define ODL_XL3(text1, val1, text2, val2, text3, val3) /* */

/*! @brief Write four long long hexadecimal values to the log.
 @param text1 The caption for the first value to be written.
 @param val1 The first value to be written.
 @param text2 The caption for the second value to be written.
 @param val2 The second value to be written.
 @param text3 The caption for the third value to be written.
 @param val3 The third value to be written.
 @param text4 The caption for the fourth value to be written.
 @param val4 The fourth value to be written. */
#  define ODL_XL4(text1, val1, text2, val2, text3, val3, text4, val4) /* */

# endif // ! defined(ODL_ENABLE_LOGGING_)

/*! @brief Write a string function exit string to the log.
 @param val The value being returned by the function. */
#  define ODL_EXIT_s(val) \
        ODL_EXIT_S(val.c_str())

/*! @brief Write a string method exit string to the log.
 @param val The value being returned by the method. */
#  define ODL_OBJEXIT_s(val) \
        ODL_OBJEXIT_S(val.c_str())

/*! @brief Write a string value to the log.
 @param text1 The caption for the value to be written.
 @param val1 The value to be written. */
# define ODL_S1s(text1, val1) \
        ODL_S1(text1, val1.c_str())

/*! @brief Write two string values to the log.
 @param text1 The caption for the first value to be written.
 @param val1 The first value to be written.
 @param text2 The caption for the second value to be written.
 @param val2 The second value to be written. */
# define ODL_S2s(text1, val1, text2, val2) \
        ODL_S2(text1, val1.c_str(), text2, val2.c_str())

/*! @brief Write three string values to the log.
 @param text1 The caption for the first value to be written.
 @param val1 The first value to be written.
 @param text2 The caption for the second value to be written.
 @param val2 The second value to be written.
 @param text3 The caption for the third value to be written.
 @param val3 The third value to be written. */
# define ODL_S3s(text1, val1, text2, val2, text3, val3) \
        ODL_S3(text1, val1.c_str(), text2, val2.c_str(), text3, val3.c_str())

/*! @brief Write four string values to the log.
 @param text1 The caption for the first value to be written.
 @param val1 The first value to be written.
 @param text2 The caption for the second value to be written.
 @param val2 The second value to be written.
 @param text3 The caption for the third value to be written.
 @param val3 The third value to be written.
 @param text4 The caption for the fourth value to be written.
 @param val4 The fourth value to be written. */
# define ODL_S4s(text1, val1, text2, val2, text3, val3, text4, val4) \
        ODL_S4(text1, val1.c_str(), text2, val2.c_str(), text3, val3.c_str(), text4,\
                    val4.c_str())

/*! @brief A flag to suppress multiple includes / declarations, which are a problem with
 Objective-C. */
# define ODL_DEFINITIONS_LOADED /* */

#endif // ! defined(ODLOGGING_H_)
