//--------------------------------------------------------------------------------------------------
//
//  File:       m+mLeapDisplayApplication.hpp
//
//  Project:    m+m
//
//  Contains:   The class declaration for the application object of the Leap Motion display output
//              service application.
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

#if (! defined(mpmLeapDisplayApplication_HPP_))
# define mpmLeapDisplayApplication_HPP_ /* Header guard */

# include "m+mLeapDisplayWindow.hpp"

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wc++11-extensions"
#  pragma clang diagnostic ignored "-Wdocumentation"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#  pragma clang diagnostic ignored "-Wpadded"
#  pragma clang diagnostic ignored "-Wshadow"
#  pragma clang diagnostic ignored "-Wunused-parameter"
#  pragma clang diagnostic ignored "-Wweak-vtables"
# endif // defined(__APPLE__)
# if (! MAC_OR_LINUX_)
#  pragma warning(push)
#  pragma warning(disable: 4996)
#  pragma warning(disable: 4458)
# endif // ! MAC_OR_LINUX_
# include <yarp/os/Network.h>
# if (! MAC_OR_LINUX_)
#  pragma warning(pop)
# endif // ! MAC_OR_LINUX_
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file

 @brief The class declaration for the application object of the %Leap Motion display output service
 application. */

/*! @dir Source
 @brief The set of files that support displaying %Leap Motion data. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace LeapDisplay
{
    class GraphicsPanel;
    class LeapDisplayOutputService;
    class LeapServiceThread;
    
    /*! @brief The application object of the application. */
    class LeapDisplayApplication : public JUCEApplication
    {
    public :

    protected :

    private :

        /*! @brief The class that this class is derived from. */
        typedef JUCEApplication inherited;

    public :

        /*! @brief The constructor. */
        LeapDisplayApplication(void);

        /*! @brief The destructor. */
        virtual
        ~LeapDisplayApplication(void);

#if 0
        /*! @brief Configure the associated service. */
        void
        configureAssociatedService(void);
#endif//0

        /*! @brief Return the application object.
         @returns The application object. */
        static LeapDisplayApplication *
        getApp(void);

        /*! @brief Return the value of a system environment variable.
         @param[in] varName The name of the system environment variable.
         @returns The value of the system environment variable, or an empty value. */
        static String
        getEnvironmentVar(const char * varName);

        /*! @brief Collect the set of system environment variables.

         Note that the generated code for a std::map of String, String includes illegal instructions
         when end() is referenced, so we are using a pair of simple vectors - performance is not
         critical, since the environment variables are only used once.
         @param[out] keys The list of environment variable names.
         @param[out] values The list of environment variable values. */
        static void
        getEnvironmentVars(YarpStringVector & keys,
                           YarpStringVector & values);

        
        /*! @brief Return a pointer to the graphics panel.
         @returns A pointer to the graphics panel. */
        GraphicsPanel *
        getGraphicsPanel(void)
        const
        {
            return _graphicsPanel;
        } // getGraphicsPanel
        
        /*! @brief Return the main window of the application.
         @returns The main window of the application. */
        static LeapDisplayWindow *
        getMainWindow(void);

        /*! @brief Restart the associated service. */
        inline void
        restartAssociatedService(void)
        {
            stopAssociatedService();
            startAssociatedService();
        } // restartAssociatedService
        
        /*! @brief Return @c true if the associated service is running and @c false if the service
         is not running.
         @returns @c true if the associated service is running and @c false otherwise. */
        bool
        serviceIsRunning(void)
        const;
        
        /*! @brief Records the address of the graphics panel.
         @param[in] thePanel A pointer to the graphics panel. */
        void
        setGraphicsPanel(GraphicsPanel * thePanel)
        {
            _graphicsPanel = thePanel;
        } // setGraphicsPanel
        
        /*! @brief Start the associated service. */
        void
        startAssociatedService(void);
        
        /*! @brief Stop the associated service. */
        void
        stopAssociatedService(void);
        
        /*! @brief Move closer to the images. */
        void
        zoomIn(void);
        
        /*! @brief Move away from the images. */
        void
        zoomOut(void);

    protected :

    private :

        /*! @brief The copy constructor.
         @param[in] other The object to be copied. */
        LeapDisplayApplication(const LeapDisplayApplication & other);

        /*! @brief Called when an attempt was made to launch another instance of the application.
         @param[in] commandLine The arguments passed to the new instance. */
        virtual void
        anotherInstanceStarted(const String & commandLine);

        /*! @brief Collect the launch details for the service and create the service.
         @returns The newly created service. */
        LeapDisplayOutputService *
        configureAndCreateService(void);
        
        /*! @brief Determine the path to an executable, using the system PATH environment variable.
         @param[in] execName The short name of the executable.
         @returns The full path to the first executable found in the system PATH environment
         variable. */
        static String
        findPathToExecutable(const String & execName);
        
        /*! @brief Return the application name.
         @returns The application's name. */
        virtual const String
        getApplicationName(void);

        /*! @brief Return the application version number.
         @returns The application's version number. */
        virtual const String
        getApplicationVersion(void);

        /*! @brief Return the thread containing the service.
         @returns The thread containing the service. */
        LeapServiceThread *
        getServiceThread(void)
        const
        {
            return _serviceThread;
        } // getServiceThread
        
        /*! @brief Called when the application starts.
         @param[in] commandLine The parameters passed to the application. */
        virtual void
        initialise(const String & commandLine);

        /*! @brief Return @c true if multiple instances of the application are allowed and @c false
         otherwise.
         @returns @c true if multiple instanaces of the application are allowed and @c false
         otherwise. */
        virtual bool
        moreThanOneInstanceAllowed(void);

        /*! @brief The assignment operator.
         @param[in] other The object to be copied.
         @returns The updated object. */
        LeapDisplayApplication &
        operator =(const LeapDisplayApplication & other);

        /*! @brief Set the thread containing the service.
         @param[in] newThread The thread containing the service. */
        void
        setServiceThread(LeapServiceThread * newThread);
        
        /*! @brief Called to allow the application to clear up before exiting. */
        virtual void
        shutdown(void);

        /*! @brief Called when the operating system is trying to close the application. */
        virtual void
        systemRequestedQuit(void);

        /*! @brief Check if the Registry Service can be launched.
         @param[in] registryServicePath The file system path to the Registry Service executable. */
        void
        validateRegistryService(const String & registryServicePath);

        /*! @brief Check if YARP can be launched.
         @param[in] yarpPath The file system path to the YARP executable. */
        void
        validateYarp(const String & yarpPath);
        
    public :

    protected :

    private :

        /*! @brief The primary window of the application. */
        ScopedPointer<LeapDisplayWindow> _mainWindow;

        /*! @brief Used to establish connections to the YARP infrastructure. */
        ScopedPointer<yarp::os::Network> _yarp;

        /*! @brief The thread containing the output service. */
        ScopedPointer<LeapServiceThread> _serviceThread;
        
        /*! @brief The graphics panel that will perform rendering. */
        GraphicsPanel * _graphicsPanel;
        
    }; // LeapDisplayApplication

} // LeapDisplay

#endif // ! defined(mpmLeapDisplayApplication_HPP_)
