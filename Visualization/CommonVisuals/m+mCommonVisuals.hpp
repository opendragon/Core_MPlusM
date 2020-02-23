//--------------------------------------------------------------------------------------------------
//
//  File:       m+mCommonVisuals.hpp
//
//  Project:    m+m
//
//  Contains:   The declarations of common data types for the common visualization code.
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
//  Created:    2016-06-05
//
//--------------------------------------------------------------------------------------------------

#if (! defined(mpmCommonVisuals_HPP_))
# define mpmCommonVisuals_HPP_ /* Header guard */

# include <m+m/m+mBaseArgumentDescriptor.hpp>

# if (! defined(DOXYGEN))
#  if (! MAC_OR_LINUX_)
#   pragma warning(push)
#   pragma warning(disable: 4458)
#   pragma warning(disable: 4459)
#  endif // ! MAC_OR_LINUX_
#  include "../JuceLibraryCode/JuceHeader.h"
#  if (! MAC_OR_LINUX_)
#   pragma warning(pop)
#  endif // ! MAC_OR_LINUX_
# endif // ! defined(DOXYGEN)

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file

 @brief The declarations of common data types for the common visualization code. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace CommonVisuals
{
    /*! @brief The kind of application. */
    enum ApplicationKind
    {
        /*! @brief The application is an Adapter. */
        kApplicationAdapter,

        /*! @brief The application is a Service. */
        kApplicationService,

        /*! @brief The application is not recognized. */
        kApplicationUnknown

    }; // ApplicationKind

    /*! @brief The values to be returned by a configuration or settings window. */
    enum ConfigurationRequest
    {
        /*! @brief 'Cancel' was requested. */
        kConfigurationCancel,

        /*! @brief 'OK' was requested. */
        kConfigurationOK,

        /*! @brief '+ argument' was requested. */
        kConfigurationAddField,

        /*! @brief '...' was requested. */
        kConfigurationFileRequest,

        /*! @brief '- argument' was requested. */
        kConfigurationRemoveField

    }; // ConfigurationRequest

    /*! @brief The information used to launch an application. */
    struct ApplicationInfo
    {
        /*! @brief The argument descriptions for the application. */
        MplusM::Utilities::DescriptorVector _argDescriptions;

        /*! @brief The file system path to the application. */
        String _applicationPath;

        /*! @brief The matching criteria (if an Adapter). */
        String _criteria;

        /*! @brief The supported options (if a Service). */
        String _options;

        /*! @brief The description provided by the application. */
        String _description;

        /*! @brief The 'short name' of the application. */
        String _shortName;

        /*! @brief What kind of application this is. */
        ApplicationKind _kind;

    }; // ApplicationInfo

    /*! @brief Coordinates in space. */
    typedef Vector3D<double> Location;

    /*! @brief Coordinates on the display. */
    typedef Point<float> Position;
    
    /*! @brief The data associated with a vertex. */
    struct Vertex
    {
        /*! @brief The location of the vertex. */
        float _position[3];
        
        /*! @brief The surface normal at the vertex. */
        float _normal[3];
        
        /*! @brief The colour at the vertex. */
        float _colour[4];
        
        /*! @brief The texture coordinates for the vertex. */
        float _texCoord[2];
        
    }; // Vertex

    /*! @brief Determine the maximum dimensions of a text string.
     @param[out] dimensions The calculated maximum width and height.
     @param[in] aFont The font to use for the calculations.
     @param[in] aString The string to be analyzed. */
    void
    CalculateTextArea(Point<int> &   dimensions,
                      const Font &   aFont,
                      const String & aString);

    /*! @brief Returns the command manager object used to dispatch command events.
     @return The command manager object used to dispatch command events. */
    ApplicationCommandManager &
    GetApplicationCommandManager(void);

    /*! @brief Return the button height to use.
     @return The button height to use. */
    int
    GetButtonHeight(void);

    /*! @brief Releases the command manager object used to dispatch command events. */
    void
    ReleaseApplicationCommandManager(void);

} // CommonVisuals

/*! @brief Return @c true if exit is requested.
 @param[in] stuff Dummy argument to satisfy caller.
 @return @c true if exit has been requested. */
bool
CheckForExit(void * stuff);

/*! @brief Launch a process, checking periodically for completion.
 @param[in] aProcess The process to execute.
 @param[in] timeout The number of milliseconds allowed for the process to complete
 (<= 0 == forever).
 @return @c true if the process completed in the time provided. */
bool
LazyLaunchProcess(ChildProcess & aProcess,
                  const int      timeout);

/*! @brief Indicate that an exit has been requested. */
void
SetExitRequest(void);

#endif // ! defined(mpmCommonVisuals_HPP_)
