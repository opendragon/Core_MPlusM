//--------------------------------------------------------------------------------------------------
//
//  File:       m+mPlatonicDisplayInputHandler.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for the input channel input handler used by the platonic
//              display output service.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2016 by OpenDragon.
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
//  Created:    2016-06-04
//
//--------------------------------------------------------------------------------------------------

#include "m+mPlatonicDisplayInputHandler.hpp"
#include "m+mPlatonicDisplayApplication.hpp"
#include "m+mPlatonicDisplayDataTypes.hpp"
#include "m+mPlatonicDisplayGraphicsPanel.hpp"

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for the input channel input handler used by the platonic display output
 service. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace PlatonicDisplay;
using namespace MplusM;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

/*! @brief The scale factor to apply to coordinates after clamping. */
static const double kScaler = 3.1;

/*! @brief The maximum observed value for X. */
static const double kMaxXValue = 300;

/*! @brief The maximum observed value for Y. */
static const double kMaxYValue = 800;

/*! @brief The maximum observed value for Z. */
static const double kMaxZValue = 300;

/*! @brief The minimum observed value for X. */
static const double kMinXValue = -300;

/*! @brief The minimum observed value for Y. */
static const double kMinYValue = 0;

/*! @brief The minimum observed value for Z. */
static const double kMinZValue = -300;

/*! @brief The number of values for each finger. */
static const int kValuesPerFinger = 3; // 3D positions

/*! @brief The number of values per hand. */
static const int kValuesPerHand = (kValuesPerFinger * 6);

/*! @brief The total number of values. */
static const int kTotalValues = (kValuesPerHand * 2);

/*! @brief The mask bits for the fingers. */
enum TipValueBits
{
    /*! @brief The flag bit for the left palm. */
    kLeftPalmBit =    0x0001,

    /*! @brief The flag bit for the left thumb. */
    kLeftThumbBit =   0x0002,
    
    /*! @brief The flag bit for the left index finger. */
    kLeftIndexBit =   0x0004,
    
    /*! @brief The flag bit for the left middle finger. */
    kLeftMiddleBit =  0x0008,
    
    /*! @brief The flag bit for the left ring finger. */
    kLeftRingBit =    0x0010,
    
    /*! @brief The flag bit for the left pinky. */
    kLeftPinkyBit =   0x0020,

    /*! @brief The flag bit for the right palm. */
    kRightPalmBit =   0x0040,

    /*! @brief The flag bit for the right thumb. */
    kRightThumbBit =  0x0080,
    
    /*! @brief The flag bit for the right index finger. */
    kRightIndexBit =  0x0100,
    
    /*! @brief The flag bit for the right middle finger. */
    kRightMiddleBit = 0x0200,
    
    /*! @brief The flag bit for the right ring finger. */
    kRightRingBit =   0x0400,
    
    /*! @brief The flag bit for the right pinky. */
    kRightPinkyBit =  0x0800
    
}; // TipValueBits

/*! @brief The positions of the data for the fingers. */
enum TipValuePosition
{
    /*! @brief The position of the values for the left palm. */
    kLeftPalmPosition = 0,

    /*! @brief The position of the values for the left thumb. */
    kLeftThumbPosition,
    
    /*! @brief The position of the values for the left index finger. */
    kLeftIndexPosition,
    
    /*! @brief The position of the values for the left middle finger. */
    kLeftMiddlePosition,
    
    /*! @brief The position of the values for the left ring finger. */
    kLeftRingPosition,
    
    /*! @brief The position of the values for the left pinky. */
    kLeftPinkyPosition,
    
    /*! @brief The position of the values for the right palm. */
    kRightPalmPosition,

    /*! @brief The position of the values for the right thumb. */
    kRightThumbPosition,
    
    /*! @brief The position of the values for the right index finger. */
    kRightIndexPosition,
    
    /*! @brief The position of the values for the right middle finger. */
    kRightMiddlePosition,
    
    /*! @brief The position of the values for the right ring finger. */
    kRightRingPosition,
    
    /*! @brief The position of the values for the right pinky. */
    kRightPinkyPosition
    
}; // TipValuePosition

#if defined(__APPLE__)
# pragma mark Global constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

/*! @brief Scale and constrain an input value.
 @param[in] inValue The value to be scaled.
 @param[in] minValue The lowest expected value.
 @param[in] maxValue The highest expected value.
 @returns A value in the range zero to one, inclusive. */
static double
clampedValue(const double inValue,
             const double minValue,
             const double maxValue)
{
    ODL_ENTER(); //####
    ODL_D3("inValue = ", inValue, "minValue = ", minValue, "maxValue = ", maxValue); //####
    double range = maxValue - minValue;
    double scaled = (2.0 * ((inValue - minValue) / range)) - 1.0;
    double result;

    if (-1.0 <= scaled)
    {
        if (1.0 >= scaled)
        {
            result = scaled;
        }
        else
        {
            result = 1.0;
        }
    }
    else
    {
        result = -1.0;
    }
    ODL_EXIT_D(result); //####
    return result;
} // clampedValue

/*! @brief Extract the information for a finger from the input.

 Note that missing data for a finger is not considered an error.
 @param[in] flag The flag identifying if the finger data is present.
 @param[in] position The index of the finger data in the input.
 @param[in] input The input data to be processed.
 @param[out] finger The output finger information.
 @returns @c true if the finger data was correct and @c false if the data in the message was not
 correct for a finger. */
static bool
checkFinger(const int                flag,
            const TipValuePosition   position,
            const yarp::os::Bottle & input,
            FingerTip &              finger)
{
    ODL_ENTER(); //####
    ODL_LL2("flag = ", flag, "position = ", position); //####
    ODL_P2("input = ", &input, "finger = ", &finger); //####
    bool okSoFar = true;

    if (0 == flag)
    {
        finger._valid = false;
        ODL_B1("finger._valid <- ", finger._valid); //####
    }
    else
    {
        int             offset = (position * kValuesPerFinger) + 1;
        yarp::os::Value aValue(input.get(offset));

        if (aValue.isDouble())
        {
            finger._where.x = (clampedValue(aValue.asDouble(), kMinXValue, kMaxXValue) * kScaler);
            ODL_D1("finger._where.x <- ", finger._where.x); //####
        }
        else
        {
            okSoFar = false;
        }
        if (okSoFar)
        {
            aValue = input.get(offset + 1);
            if (aValue.isDouble())
            {
                finger._where.y = (clampedValue(aValue.asDouble(), kMinYValue, kMaxYValue) *
                                   kScaler);
                ODL_D1("finger._where.y <- ", finger._where.y); //####
            }
            else
            {
                okSoFar = false;
            }
        }
        if (okSoFar)
        {
            aValue = input.get(offset + 2);
            if (aValue.isDouble())
            {
                finger._where.z = (clampedValue(aValue.asDouble(), kMinZValue, kMaxZValue) *
                                   kScaler);
                ODL_D1("finger._where.z <- ", finger._where.z); //####
            }
            else
            {
                okSoFar = false;
            }
        }
        finger._valid = okSoFar;
        ODL_B1("finger._valid <- ", finger._valid); //####
    }
    ODL_EXIT_B(okSoFar); //####
    return okSoFar;
} // checkFinger

/*! @brief Mark all the fingers as not present.
 @param[in,out] aHand The hand to be invalidated. */
static void
invalidateFingerData(HandData & aHand)
{
    ODL_ENTER(); //####
    ODL_P1("aHand = ", &aHand); //####
    aHand._palm._valid = aHand._thumb._valid = aHand._index._valid = aHand._middle._valid =
        aHand._ring._valid = aHand._pinky._valid = false;
    ODL_EXIT(); //####
} // invalidateFingerData

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and Destructors
#endif // defined(__APPLE__)

PlatonicDisplayInputHandler::PlatonicDisplayInputHandler(void) :
    inherited()
{
    ODL_ENTER(); //####
    ODL_EXIT_P(this); //####
} // PlatonicDisplayInputHandler::PlatonicDisplayInputHandler

PlatonicDisplayInputHandler::~PlatonicDisplayInputHandler(void)
{
    ODL_OBJENTER(); //####
    ODL_OBJEXIT(); //####
} // PlatonicDisplayInputHandler::~PlatonicDisplayInputHandler

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

#if (! MAC_OR_LINUX_)
# pragma warning(push)
# pragma warning(disable: 4100)
#endif // ! MAC_OR_LINUX_
bool
PlatonicDisplayInputHandler::handleInput(const yarp::os::Bottle &     input,
                                     const YarpString &           senderChannel,
                                     yarp::os::ConnectionWriter * replyMechanism,
                                     const size_t                 numBytes)
{
#if (! defined(OD_ENABLE_LOGGING_))
# if MAC_OR_LINUX_
#  pragma unused(senderChannel,replyMechanism,numBytes)
# endif // MAC_OR_LINUX_
#endif // ! defined(OD_ENABLE_LOGGING_)
    ODL_OBJENTER(); //####
    ODL_S2s("senderChannel = ", senderChannel, "got ", input.toString()); //####
    ODL_P1("replyMechanism = ", replyMechanism); //####
    ODL_LL1("numBytes = ", numBytes); //####
    bool result = true;

    try
    {
        if ((kTotalValues + 1) == input.size())
        {
            // We have the flag field plus the finger values.
            yarp::os::Value aValue(input.get(0));
            
            if (aValue.isInt())
            {
                HandData leftHand;
                HandData rightHand;
                int      flags = aValue.asInt();

                if (0 == flags)
                {
                    ODL_LOG("(0 == flags)"); //####
                    PlatonicDisplayApplication * ourApp = PlatonicDisplayApplication::getApp();

                    if (ourApp)
                    {
                        GraphicsPanel * aPanel = ourApp->getGraphicsPanel();

                        if (aPanel)
                        {
                            invalidateFingerData(leftHand);
                            invalidateFingerData(rightHand);
                            aPanel->updateFingerData(leftHand, rightHand);
                        }
                    }
                }
                else
                {
                    bool okSoFar = true;

                    okSoFar &= checkFinger(flags & kLeftPalmBit, kLeftPalmPosition, input,
                                           leftHand._palm);
                    okSoFar &= checkFinger(flags & kLeftThumbBit, kLeftThumbPosition, input,
                                           leftHand._thumb);
                    okSoFar &= checkFinger(flags & kLeftIndexBit, kLeftIndexPosition, input,
                                           leftHand._index);
                    okSoFar &= checkFinger(flags & kLeftMiddleBit, kLeftMiddlePosition, input,
                                           leftHand._middle);
                    okSoFar &= checkFinger(flags & kLeftRingBit, kLeftRingPosition, input,
                                           leftHand._ring);
                    okSoFar &= checkFinger(flags & kLeftPinkyBit, kLeftPinkyPosition, input,
                                           leftHand._pinky);
                    okSoFar &= checkFinger(flags & kRightPalmBit, kRightPalmPosition, input,
                                           rightHand._palm);
                    okSoFar &= checkFinger(flags & kRightThumbBit, kRightThumbPosition, input,
                                           rightHand._thumb);
                    okSoFar &= checkFinger(flags & kRightIndexBit, kRightIndexPosition, input,
                                           rightHand._index);
                    okSoFar &= checkFinger(flags & kRightMiddleBit, kRightMiddlePosition, input,
                                           rightHand._middle);
                    okSoFar &= checkFinger(flags & kRightRingBit, kRightRingPosition, input,
                                           rightHand._ring);
                    okSoFar &= checkFinger(flags & kRightPinkyBit, kRightPinkyPosition, input,
                                           rightHand._pinky);
                    if (okSoFar)
                    {
                        PlatonicDisplayApplication * ourApp = PlatonicDisplayApplication::getApp();
                        
                        if (ourApp)
                        {
                            GraphicsPanel * aPanel = ourApp->getGraphicsPanel();
                            
                            if (aPanel)
                            {
                                aPanel->updateFingerData(leftHand, rightHand);
                            }
                        }
                    }
                    else
                    {
                        ODL_LOG("! (okSoFar)"); //####
                    }
                }
            }
            else
            {
                ODL_LOG("! (aValue.isInt())"); //####
            }
        }
        else
        {
            ODL_LOG("! ((kTotalValues + 1) == input.size())"); //####
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT_B(result); //####
    return result;
} // PlatonicDisplayInputHandler::handleInput
#if (! MAC_OR_LINUX_)
# pragma warning(pop)
#endif // ! MAC_OR_LINUX_

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
