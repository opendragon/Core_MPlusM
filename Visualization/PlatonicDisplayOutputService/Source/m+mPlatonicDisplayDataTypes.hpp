//--------------------------------------------------------------------------------------------------
//
//  File:       m+mPlatonicDisplayDataTypes.hpp
//
//  Project:    m+m
//
//  Contains:   The common data types for the platonic display output service application.
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
//  Created:    2016-06-04
//
//--------------------------------------------------------------------------------------------------

#if (! defined(mpmPlatonicDisplayDataTypes_HPP_))
# define mpmPlatonicDisplayDataTypes_HPP_ /* Header guard */

# include "m+mCommonVisuals.hpp"

# include <m+m/m+mUtilities.hpp>

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file

 @brief The common data types for the platonic display output service application. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace PlatonicDisplay
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

    /*! @brief The information for a pixie. */
    struct Pixie
    {
        /*! @brief The location in space for the pixie. */
        CommonVisuals::Location _where;

        /*! @brief @c true if the location is known. */
        bool _valid;

        /*! @brief The constructor. */
        inline Pixie(void) :
            _valid(false)
        {
        } // Pixie

        /*! @brief The constructor.
         @param[in] newX The X coordinate of the new pixie.
         @param[in] newY The Y coordinate of the new pixie.
         @param[in] newZ The Z coordinate of the new pixie. */
        inline Pixie(const double newX,
                     const double newY,
                     const double newZ) :
            _valid(true)
        {
            _where.x = newX;
            _where.y = newY;
            _where.z = newZ;
        } // Pixie

    }; // Pixie

} // PlatonicDisplay

#endif // ! defined(mpmPlatonicDisplayDataTypes_HPP_)
