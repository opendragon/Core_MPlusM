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

# include <m+m/m+mBaseArgumentDescriptor.hpp>
# include <m+m/m+mGeneralChannel.hpp>
# include <m+m/m+mUtilities.hpp>

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

# include <list>
# include <map>
# include <set>
# include <vector>

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file

 @brief The common data types for the Leap Motion display output service application. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

/*! @brief A longer sleep, in milliseconds. */
# define LONG_SLEEP_ (VERY_SHORT_SLEEP_ * 100)

/*! @brief A slightly longer sleep, in milliseconds. */
# define MIDDLE_SLEEP_ (VERY_SHORT_SLEEP_ * 7)

/*! @brief The minimum time for a thread to sleep, in milliseconds. */
# define SHORT_SLEEP_ (VERY_SHORT_SLEEP_ * 4)

/*! @brief A very short sleep, in milliseconds. */
# define VERY_SHORT_SLEEP_ 5

namespace LeapDisplay
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

    /*! @brief What kind of container. */
    enum ContainerKind
    {
        /*! @brief The container is an adapter - a special form of service. */
        kContainerKindAdapter,

        /*! @brief The container is a service. */
        kContainerKindService,

        /*! @brief The container is not a service. */
        kContainerKindOther,

        /*! @brief Force the size to be 4 bytes. */
        kContainerKindUnknown = 0x7FFFFFF

    }; // ContainerKind

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

    /*! @brief Coordinates on the display. */
    typedef Point<float> Position;
    
    /*! @brief Coordinates in space. */
    typedef Vector3D<double> Location;
    
    /*! @brief The information for a finger. */
    struct FingerTip
    {
        /*! @brief The location in space for the finger tip. */
        Location _where;
        
        /*! @brief @c true if the location is known. */
        bool     _valid;
        
    }; // FingerTip
    
    /*! @brief The information for a hand. */
    struct HandData
    {
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

/*! @brief Return @c true if exit is requested.
 @param stuff Dummy argument to satisfy caller.
 @returns @c true if exit has been requested. */
bool
CheckForExit(void * stuff);

/*! @brief Launch a process, checking periodically for completion.
 @param aProcess The process to execute.
 @param timeout The number of milliseconds allowed for the process to complete (<= 0 == forever).
 @returns @c true if the process completed in the time provided. */
bool
LazyLaunchProcess(ChildProcess & aProcess,
                  const int      timeout);

/*! @brief Indicate that an exit has been requested. */
void
SetExitRequest(void);

#endif // ! defined(mpmLeapDisplayDataTypes_HPP_)
