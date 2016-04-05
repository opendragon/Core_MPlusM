//--------------------------------------------------------------------------------------------------
//
//  File:       m+m/m+mBaseThread.h
//
//  Project:    m+m
//
//  Contains:   The class declaration for the common thread class for m+m.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2016 by H Plus Technologies Ltd. and Simon Fraser University.
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
//  Created:    2016-01-17
//
//--------------------------------------------------------------------------------------------------

#if (! defined(MpMBaseThread_H_))
# define MpMBaseThread_H_ /* Header guard */

# include <m+m/m+mCommon.h>

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 @brief The class declaration for the common thread class for m+m. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

/*! @brief Declare the run method. */
# define DECLARE_RUN_ \
    virtual void\
    run(void)

/*! @brief Declare the threadInit method, which returns @c true if the thread is ready to run. */
# define DECLARE_THREADINIT_ \
    virtual bool\
    threadInit(void)

/*! @brief Declare the threadRelease method. */
# define DECLARE_THREADRELEASE_ \
    virtual void\
    threadRelease(void)

/*! @brief Define the run method. */
# define DEFINE_RUN_(class_) \
    void\
    class_::run(void)

/*! @brief Define the threadInit method. */
# define DEFINE_THREADINIT_(class_) \
    bool\
    class_::threadInit(void)

/*! @brief Define the threadInit method. */
# define DEFINE_THREADRELEASE_(class_) \
    void\
    class_::threadRelease(void)

namespace MplusM
{
    namespace Common
    {
        /*! @brief A convenience class for threads. */
        class BaseThread : public yarp::os::Thread
        {
        public :

        protected :

        private :

            /*! @brief The class that this class is derived from. */
            typedef yarp::os::Thread inherited;

        public :

            /*! @brief The constructor. */
            BaseThread(void);

            /*! @brief The destructor. */
            virtual
            ~BaseThread(void);

        protected :

        private :

            /*! @fn virtual void
                    run(void)
             @brief The thread main body. */
            DECLARE_RUN_;

            /*! @fn virtual bool
                    threadInit(void)
             @brief The thread initialization method.
             @returns @c true if the thread is ready to run. */
            DECLARE_THREADINIT_;

            /*! @fn virtual void
                    threadRelease(void)
             @brief The thread termination method. */
            DECLARE_THREADRELEASE_;

            COPY_AND_ASSIGNMENT_(BaseThread);

        public :

        protected :

        private :

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunused-private-field"
# endif // defined(__APPLE__)
            /*! @brief Filler to pad to alignment boundary */
            char _filler1[7];
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

        }; // BaseThread

    } // Common

} // MplusM

#endif // ! defined(MpMBaseThread_H_)
