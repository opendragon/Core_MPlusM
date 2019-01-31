//--------------------------------------------------------------------------------------------------
//
//  File:       m+m/m+mStringBuffer.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for a string buffer.
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
//  Created:    2015-07-23
//
//--------------------------------------------------------------------------------------------------

#include "m+mStringBuffer.hpp"

#include <m+m/m+mClientChannel.hpp>
#include <m+m/m+mServiceResponse.hpp>

//#include <odlEnable.h>
#include <odlInclude.h>

#include <inttypes.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for a string buffer. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

/*! @brief The value to be used in the calculation of the threshold length. */
static const double kThresholdFactor = 0.95;

/*! @brief The factor to use in calculating the new size of the buffer when an overflow occurs. */
static const double kBufferIncreaseFactor = 1.75;

/*! @brief The excess factor to use when adding a string, to allow some extra room. */
static const double kFactorSlop = 1.23;

/*! @brief The initial size to use for the buffer. */
static const size_t kInitialBufferSize = 10000;

/*! @brief The size of a scratch buffer to use when formatting numeric values. */
static const size_t kNumBuffSize = 100;

#if defined(__APPLE__)
# pragma mark Global constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and Destructors
#endif // defined(__APPLE__)

StringBuffer::StringBuffer(void) :
    _buffer(NULL), _currentLength(0), _currentSize(0), _thresholdLength(0)
{
    ODL_ENTER(); //####
    ODL_P1("_buffer = ", _buffer); //####
    ODL_I3("_currentLength = ", _currentLength, "_currentSize = ", _currentSize, //####
            "_thresholdLength = ", _thresholdLength); //####
    setSize(kInitialBufferSize);
    ODL_EXIT_P(this); //####
} // StringBuffer::StringBuffer

StringBuffer::~StringBuffer(void)
{
    ODL_OBJENTER(); //####
    delete _buffer;
    ODL_OBJEXIT(); //####
} // StringBuffer::~StringBuffer

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

StringBuffer &
StringBuffer::addChar(const char aChar)
{
    ODL_OBJENTER(); //####
    ODL_C1("aChar = ", aChar); //####
    if ((_currentLength + 1) >= _thresholdLength)
    {
        ODL_LOG("((_currentLength + 1) >= _thresholdLength)"); //####
        setSize(static_cast<size_t>(_currentSize * kBufferIncreaseFactor));
    }
    *(_buffer + _currentLength) = aChar;
    ++_currentLength;
    ODL_I1("_currentLength <- ", _currentLength); //####
    ODL_OBJEXIT_P(this); //####
    return *this;
} // StringBuffer::addChar

StringBuffer &
StringBuffer::addDouble(const double aDouble)
{
    ODL_OBJENTER(); //####
    ODL_D1("aDouble = ", aDouble); //####
    char numBuff[kNumBuffSize];

#if MAC_OR_LINUX_
    snprintf(numBuff, sizeof(numBuff), "%g", aDouble);
#else // ! MAC_OR_LINUX_
    sprintf_s(numBuff, sizeof(numBuff), "%g", aDouble);
#endif // ! MAC_OR_LINUX_
    ODL_S1("numBuff <- ", numBuff); //####
    size_t lengthToAdd = strlen(numBuff);

    // Note that the array 'numBuff' is significantly smaller than the initial size of the internal
    // buffer, so there's no need to calculate the increase factor to apply.
    if ((_currentLength + lengthToAdd) >= _thresholdLength)
    {
        ODL_LOG("((_currentLength + lengthToAdd) >= _thresholdLength)"); //####
        setSize(static_cast<size_t>(_currentSize * kBufferIncreaseFactor));
    }
    memcpy(_buffer + _currentLength, numBuff, lengthToAdd + 1);
    _currentLength += lengthToAdd;
    ODL_I1("_currentLength <- ", _currentLength); //####
    ODL_OBJEXIT_P(this); //####
    return *this;
} // StringBuffer::addDouble

StringBuffer &
StringBuffer::addLong(const int64_t aLong)
{
    ODL_OBJENTER(); //####
    ODL_I1("aLong = ", aLong); //####
    char numBuff[kNumBuffSize];

    snprintf(numBuff, sizeof(numBuff), "%" PRId64, aLong);
    ODL_S1("numBuff <- ", numBuff); //####
    size_t lengthToAdd = strlen(numBuff);

    // Note that the array 'numBuff' is significantly smaller than the initial size of the internal
    // buffer, so there's no need to calculate the increase factor to apply.
    if ((_currentLength + lengthToAdd) >= _thresholdLength)
    {
        ODL_LOG("((_currentLength + lengthToAdd) >= _thresholdLength)"); //####
        setSize(static_cast<size_t>(_currentSize * kBufferIncreaseFactor));
    }
    memcpy(_buffer + _currentLength, numBuff, lengthToAdd + 1);
    _currentLength += lengthToAdd;
    ODL_I1("_currentLength <- ", _currentLength); //####
    ODL_OBJEXIT_P(this); //####
    return *this;
} // StringBuffer::addLong

StringBuffer &
StringBuffer::addString(const char * aString)
{
    ODL_OBJENTER(); //####
    ODL_S1("aString = ", aString); //####
    if (aString)
    {
        size_t lengthToAdd = strlen(aString);

        if ((_currentLength + lengthToAdd) >= _thresholdLength)
        {
            ODL_LOG("((_currentLength + lengthToAdd) >= _thresholdLength)"); //####
            double scaleFactor = ((kFactorSlop * (_currentLength + lengthToAdd)) / _currentLength);
#if MAC_OR_LINUX_
            double factorToUse = std::max(scaleFactor, kBufferIncreaseFactor);
#else // ! MAC_OR_LINUX_
            double factorToUse = max(scaleFactor, kBufferIncreaseFactor);
#endif // ! MAC_OR_LINUX_

            setSize(static_cast<size_t>(_currentSize * factorToUse));
        }
        memcpy(_buffer + _currentLength, aString, lengthToAdd + 1);
        _currentLength += lengthToAdd;
        ODL_I1("_currentLength <- ", _currentLength); //####
    }
    ODL_OBJEXIT_P(this); //####
    return *this;
} // StringBuffer::addString

StringBuffer &
StringBuffer::addString(const YarpString & aString)
{
    ODL_OBJENTER(); //####
    ODL_S1s("aString = ", aString); //####
    const char * actualString = aString.c_str();

    if (actualString)
    {
        size_t lengthToAdd = strlen(actualString);

        if ((_currentLength + lengthToAdd) >= _thresholdLength)
        {
            ODL_LOG("((_currentLength + lengthToAdd) >= _thresholdLength)"); //####
            double scaleFactor = ((kFactorSlop * (_currentLength + lengthToAdd)) / _currentLength);
#if MAC_OR_LINUX_
            double factorToUse = std::max(scaleFactor, kBufferIncreaseFactor);
#else // ! MAC_OR_LINUX_
            double factorToUse = max(scaleFactor, kBufferIncreaseFactor);
#endif // ! MAC_OR_LINUX_

            setSize(static_cast<size_t>(_currentSize * factorToUse));
        }
        memcpy(_buffer + _currentLength, actualString, lengthToAdd + 1);
        _currentLength += lengthToAdd;
        ODL_I1("_currentLength <- ", _currentLength); //####
    }
    ODL_OBJEXIT_P(this); //####
    return *this;
} // StringBuffer::addString

StringBuffer &
StringBuffer::addTab(void)
{
    ODL_OBJENTER(); //####
    if ((_currentLength + 1) >= _thresholdLength)
    {
        ODL_LOG("((_currentLength + 1) >= _thresholdLength)"); //####
        setSize(static_cast<size_t>(_currentSize * kBufferIncreaseFactor));
    }
    *(_buffer + _currentLength) = '\t';
    ++_currentLength;
    ODL_I1("_currentLength <- ", _currentLength); //####
    ODL_OBJEXIT_P(this); //####
    return *this;
} // StringBuffer::addTab

StringBuffer &
StringBuffer::reset(void)
{
    ODL_OBJENTER(); //####
    _currentLength = 0;
    ODL_OBJEXIT_P(this); //####
    return *this;
} // StringBuffer::reset

void
StringBuffer::setSize(const size_t newSize)
{
    ODL_OBJENTER(); //####
    ODL_I1("newSize = ", newSize); //####
    char * newBuffer = new char[newSize];

    if (_buffer && (0 < _currentLength))
    {
        memcpy(newBuffer, _buffer, _currentLength);
    }
    _buffer = newBuffer;
    ODL_P1("_buffer <- ", _buffer); //####
    _currentSize = newSize;
    _thresholdLength = static_cast<size_t>(newSize * kThresholdFactor);
    ODL_I2("_currentSize <- ", _currentSize, "_thresholdLength <- ", _thresholdLength); //####
    ODL_OBJEXIT(); //####
} // StringBuffer::setSize

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
