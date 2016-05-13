//--------------------------------------------------------------------------------------------------
//
//  File:       m+mYarpLaunchThread.hpp
//
//  Project:    m+m
//
//  Contains:   The class declaration for the background YARP launcher.
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
//  Created:    2015-05-05
//
//--------------------------------------------------------------------------------------------------

#if (! defined(mpmYarpLaunchThread_HPP_))
# define mpmYarpLaunchThread_HPP_ /* Header guard */

# include "m+mManagerDataTypes.hpp"

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file

 @brief The class declaration for the background YARP launcher. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace MPlusM_Manager
{
    /*! @brief A background YARP launcher. */
    class YarpLaunchThread : public Thread
    {
    public :

    protected :

    private :

        /*! @brief The class that this class is derived from. */
        typedef Thread inherited;

    public :

        /*! @brief The constructor.
         @param pathToExecutable The file system path for the executable.
         @param ipAddress The network address to use.
         @param portNumber The network port number to use. */
        YarpLaunchThread(const String & pathToExecutable,
                         const String & ipAddress,
                         const int      portNumber);

        /*! @brief The destructor. */
        virtual
        ~YarpLaunchThread(void);

        /*! @brief Force the child process to terminate. */
        void
        killChildProcess(void);

    protected :

    private :

        /*! @brief Perform the background scan. */
        virtual void
        run(void);

    public :

    protected :

    private :

        /*! @brief The running YARP process. */
        ScopedPointer<ChildProcess> _yarpProcess;

        /*! @brief The network address to use. */
        String _yarpAddress;

        /*! @brief The file system path to the executable. */
        String _yarpPath;

        /*! @brief The network port number to use. */
        int _yarpPort;

        JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR(YarpLaunchThread)

    }; // YarpLaunchThread

} // MPlusM_Manager

#endif // ! defined(mpmYarpLaunchThread_HPP_)
