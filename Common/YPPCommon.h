//--------------------------------------------------------------------------------------
//
//  File:       YPPCommon.h
//
//  Project:    YarpPlusPlus
//
//  Contains:   The function and variable declarations for common entities for Yarp++
//              clients and services.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2014 by HPlus Technologies Ltd. and Simon Fraser University.
//
//              All rights reserved. Redistribution and use in source and binary forms,
//              with or without modification, are permitted provided that the following
//              conditions are met:
//                * Redistributions of source code must retain the above copyright
//                  notice, this list of conditions and the following disclaimer.
//                * Redistributions in binary form must reproduce the above copyright
//                  notice, this list of conditions and the following disclaimer in the
//                  documentation and/or other materials provided with the
//                  distribution.
//                * Neither the name of the copyright holders nor the names of its
//                  contributors may be used to endorse or promote products derived
//                  from this software without specific prior written permission.
//
//              THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
//              "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
//              LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
//              PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
//              OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
//              SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
//              LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
//              DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
//              THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
//              (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
//              OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//
//  Created:    2014-02-18
//
//--------------------------------------------------------------------------------------

#if (! defined(YPPCOMMON_H_))
/*! @brief Header guard. */
# define YPPCOMMON_H_ /* */

# include "YPPConfig.h"
# include <string>
# include <vector>
# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wc++11-extensions"
#  pragma clang diagnostic ignored "-Wdocumentation"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#  pragma clang diagnostic ignored "-Wpadded"
#  pragma clang diagnostic ignored "-Wshadow"
#  pragma clang diagnostic ignored "-Wunused-parameter"
#  pragma clang diagnostic ignored "-Wweak-vtables"
# endif // defined(__APPLE__)
# include <yarp/os/Bottle.h>
# include <yarp/os/ConstString.h>
# include <yarp/os/Contact.h>
# include <yarp/os/Port.h>
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 
 @brief The function and variable declarations for common entities for Yarp++ clients and services. */

/*! @dir Common
 @brief The set of files that implement the Yarp++ framework. */

/*! @namespace YarpPlusPlus
 @brief The classes that implement the Yarp++ framework. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

/*! @brief The default name for the root part of a channel name. */
# define DEFAULT_CHANNEL_ROOT "channel_"

namespace YarpPlusPlus
{
    /*! @brief The logical connection between a client and a service. */
    typedef yarp::os::Bottle         Package;

    /*! @brief The logical connection between a client and a service. */
    typedef yarp::os::Port           Channel;
    
    /*! @brief A sequence of random numbers. */
    typedef std::vector<double>      DoubleVector;
    
    /*! @brief A sequence of strings. */
    typedef std::vector<std::string> StringVector;

    /*! @brief Add an output to a channel, using a backoff strategy with retries.
     @param theChannel The channel to be modified.
     @param theChannelName The name to be associated with the channel.
     @returns @c true if the channel was opened and @c false if it could not be opened. */
    bool AddOutputToChannelWithRetries(Channel &                     theChannel,
                                       const yarp::os::ConstString & theChannelName);
    
    /*! @brief Close a channel.
     @param theChannel The channel to be closed. */
    void CloseChannel(Channel & theChannel);
    
    /*! @brief Dump out a description of the provided connection information to the log.
     @param tag A unique string used to identify the call point for the output.
     @param aContact The connection information to be reported. */
    void DumpContact(const char *              tag,
                     const yarp::os::Contact & aContact);
    
    /*! @brief Generate a random channel name.
     @returns A randomly-generated channel name. */
    yarp::os::ConstString GetRandomChannelName(const char * channelRoot = DEFAULT_CHANNEL_ROOT);
    
    /*! @brief Perform initialization of internal resources.
     
     Should be called in the main() function of each application or service. */
    void Initialize(void);
    
    /*! @brief Connect two channels, using a backoff strategy with retries.
     @param sourceName The name of the source channel.
     @param destinationName The name of the destination channel.
     @returns @c true if the connection was established and @ false otherwise. */
    bool NetworkConnectWithRetries(const yarp::os::ConstString & sourceName,
                                   const yarp::os::ConstString & destinationName);
    
    /*! @brief Disconnect two channels, using a backoff strategy with retries.
     @param sourceName The name of the source channel.
     @param destinationName The name of the destination channel.
     @returns @c true if the connection was removed and @ false otherwise. */
    bool NetworkDisconnectWithRetries(const yarp::os::ConstString & sourceName,
                                      const yarp::os::ConstString & destinationName);
    
    /*! @brief Open a channel, using a backoff strategy with retries.
     @param theChannel The channel to be opened.
     @param theChannelName The name to be associated with the channel.
     @returns @c true if the channel was opened and @c false if it could not be opened. */
    bool OpenChannelWithRetries(Channel &                     theChannel,
                                const yarp::os::ConstString & theChannelName);
    
    /*! @brief Open a channel, using a backoff strategy with retries.
     @param theChannel The channel to be opened.
     @param theContactInfo The connection information to be associated with the channel.
     @returns @c true if the channel was opened and @c false if it could not be opened. */
    bool OpenChannelWithRetries(Channel &           theChannel,
                                yarp::os::Contact & theContactInfo);

} // YarpPlusPlus

#endif // ! defined(YPPCOMMON_H_)
