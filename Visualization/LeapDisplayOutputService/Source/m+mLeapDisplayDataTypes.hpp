//--------------------------------------------------------------------------------------------------
//
//  File:       m+mLeapDisplayDataTypes.hpp
//
//  Project:    m+m
//
//  Contains:   The common data types for the Leap Motion display output service application.
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
//                  list of conditions and the following disclaimer in the documentation and/or
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
//  Created:    2016-05-12
//
//--------------------------------------------------------------------------------------------------

#if (! defined(mpmLeapDisplayDataTypes_HPP_))
# define mpmLeapDisplayDataTypes_HPP_ /* Header guard */

# include "m+mCommonVisuals.hpp"

# include <m+m/m+mUtilities.hpp>

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file

 @brief The common data types for the %Leap Motion display output service application. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace LeapDisplay
{
    /*! @brief The menu selection from the popup menu. */
    enum EntityPopupMenuSelection
    {
        /*! @brief Placeholder to ensure that the menu items don't start at zero. */
        kPopupEntityDummy = 0x2200,

        /*! @brief Configure settings for an input / output service or adapter. */
        kPopupConfigureService,

        /*! @brief Restart the channels for an input / output service or adapter. */
        kPopupRestartService,

        /*! @brief Stop the service or adapter. */
        kPopupStopService

    }; // EntityPopupMenuSelection

    /*! @brief The information for a finger. */
    struct FingerTip
    {
        /*! @brief The location in space for the finger tip. */
        CommonVisuals::Location _where;
        
        /*! @brief @c true if the location is known. */
        bool _valid;
        
        /*! @brief The constructor. */
        inline FingerTip(void) :
            _valid(false)
        {
        } // FingerTip
        
    }; // FingerTip
    
    /*! @brief The information for a hand. */
    struct HandData
    {
        /*! @brief The information for the palm. */
        FingerTip _palm;

        /*! @brief The information for the thumb. */
        FingerTip _thumb;
        
        /*! @brief The information for the index finger. */
        FingerTip _index;
        
        /*! @brief The information for the middle finger. */
        FingerTip _middle;
        
        /*! @brief The information for the ring finger. */
        FingerTip _ring;
        
        /*! @brief The information for the pinky. */
        FingerTip _pinky;
        
    }; // HandData

} // LeapDisplay

#endif // ! defined(mpmLeapDisplayDataTypes_HPP_)
