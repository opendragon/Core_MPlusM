//--------------------------------------------------------------------------------------------------
//
//  File:       m+mRegistryLaunchThread.hpp
//
//  Project:    m+m
//
//  Contains:   The class declaration for the background Registry Service launcher.
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
//  Created:    2015-05-07
//
//--------------------------------------------------------------------------------------------------

#if (! defined(mpmRegistryLaunchThread_HPP_))
# define mpmRegistryLaunchThread_HPP_ /* Header guard */

# include "m+mManagerDataTypes.hpp"

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file

 @brief The class declaration for the background Registry Service launcher. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace MPlusM_Manager
{
    /*! @brief A background Registry Service launcher. */
    class RegistryLaunchThread : public Thread
    {
    public :

    protected :

    private :

        /*! @brief The class that this class is derived from. */
        typedef Thread inherited;

    public :

        /*! @brief The constructor.
         @param pathToExecutable The file system path for the executable.
         @param portNumber The network port number to use. */
        explicit
        RegistryLaunchThread(const String & pathToExecutable,
                             const int      portNumber = 0);

        /*! @brief The destructor. */
        virtual
        ~RegistryLaunchThread(void);

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

        /*! @brief The running Registry Service process. */
        ScopedPointer<ChildProcess> _registryServiceProcess;

        /*! @brief The file system path to the executable. */
        String _registryServicePath;

        /*! @brief The network port number to use. */
        int _registryServicePort;

        JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR(RegistryLaunchThread)

    }; // RegistryLaunchThread

} // MPlusM_Manager

#endif // ! defined(mpmRegistryLaunchThread_HPP_)
