//--------------------------------------------------------------------------------------------------
//
//  File:       m+mSendToMQOutputService.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for the SendToMQ output service.
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
//  Created:    2015-07-26
//
//--------------------------------------------------------------------------------------------------

#include "m+mSendToMQOutputService.hpp"
#include "m+mSendToMQOutputInputHandler.hpp"
#include "m+mSendToMQOutputRequests.hpp"

#include <m+m/m+mEndpoint.hpp>
#include <m+m/m+mGeneralChannel.hpp>

//#include <odlEnable.h>
#include <odlInclude.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for the %SendToMQ output service. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MplusM::Common;
using namespace MplusM::SendToMQ;
using std::cerr;
using std::endl;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

/*! @brief Set to @c true if a transacted session is created and @c false otherwise. */
static const bool kSessionIsTransacted = false;

#if defined(__APPLE__)
# pragma mark Global constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

static std::string
constructURI(const YarpString & hostName,
             const int          hostPort)
{
    ODL_ENTER(); //####
    ODL_S1s("hostName = ", hostName); //####
    ODL_I1("hostPort = ", hostPort); //####
    std::stringstream buff;

    buff << "failover://(tcp://" << hostName.c_str() << ":" << hostPort << ")";
    ODL_EXIT_s(buff.str()); //####
    return buff.str();
} // constructURI

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and Destructors
#endif // defined(__APPLE__)

SendToMQOutputService::SendToMQOutputService(const YarpString &                  hostName,
                                             const int                           hostPort,
                                             const YarpString &                  userName,
                                             const YarpString &                  userPassword,
                                             const Utilities::DescriptorVector & argumentList,
                                             const YarpString &                  launchPath,
                                             const int                           argc,
                                             char * *                            argv,
                                             const YarpString &                  tag,
                                             const YarpString &
                                                                                serviceEndpointName,
                                             const YarpString &
                                                                                servicePortNumber) :
inherited(argumentList, launchPath, argc, argv, tag, true, MpM_SENDTOMQOUTPUT_CANONICAL_NAME_,
          SENDTOMQOUTPUT_SERVICE_DESCRIPTION_, "", serviceEndpointName, servicePortNumber),
    _hostName(hostName), _password(userPassword), _userName(userName), _hostPort(hostPort),
    _inHandler(new SendToMQOutputInputHandler(*this)), _connection(NULL), _session(NULL),
    _destination(NULL), _producer(NULL), _useQueue(false)
{
    ODL_ENTER(); //####
    ODL_S4s("hostName = ", hostName, "userName = ", userName, "userPassword = ", //####
            userPassword, "launchPath = ", launchPath); //####
    ODL_S3s("tag = ", tag, "serviceEndpointName = ", serviceEndpointName, //####
            "servicePortNumber = ", servicePortNumber); //####
    ODL_P2("argumentList = ", &argumentList, "argv = ", argv); //####
    ODL_I2("hostPort = ", hostPort, "argc = ", argc); //####
    ODL_EXIT_P(this); //####
} // SendToMQOutputService::SendToMQOutputService

SendToMQOutputService::~SendToMQOutputService(void)
{
    ODL_OBJENTER(); //####
    stopStreams();
    delete _inHandler;
    ODL_OBJEXIT(); //####
} // SendToMQOutputService::~SendToMQOutputService

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

bool
SendToMQOutputService::configure(const yarp::os::Bottle & details)
{
    ODL_OBJENTER(); //####
    ODL_P1("details = ", &details); //####
    bool result = false;

    try
    {
        if (2 <= details.size())
        {
            yarp::os::Value firstValue(details.get(0));
            yarp::os::Value secondValue(details.get(1));

            if (firstValue.isString() && secondValue.isInt())
            {
                int               secondNumber = secondValue.asInt();
                std::stringstream buff;

                _topicOrQueueName = firstValue.asString();
                ODL_S1s("_topicOrQueueName <- ", _topicOrQueueName); //####
                _useQueue = (0 != secondNumber);
                ODL_B1("_useQueue <- ", _useQueue); //####
                // Don't trace the password OR report it via the GUI!!
                buff << "Host name is '" << _hostName.c_str() << "', host port is " << _hostPort <<
                        ", user name is '" << _userName << "', topic/queue name is '" <<
                        _topicOrQueueName << "', send via " << (_useQueue ? "queue" : "topic") <<
                        ".";
                setExtraInformation(buff.str());
                result = true;
            }
            else
            {
                cerr << "One or more inputs have the wrong type." << endl;
            }
        }
        else
        {
            cerr << "Missing input(s)." << endl;
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT_B(result); //####
    return result;
} // SendToMQOutputService::configure

void
SendToMQOutputService::disableMetrics(void)
{
    ODL_OBJENTER(); //####
    inherited::disableMetrics();
    if (_inHandler)
    {
        _inHandler->disableMetrics();
    }
    ODL_OBJEXIT(); //####
} // SendToMQOutputService::disableMetrics

void
SendToMQOutputService::enableMetrics(void)
{
    ODL_OBJENTER(); //####
    inherited::enableMetrics();
    if (_inHandler)
    {
        _inHandler->enableMetrics();
    }
    ODL_OBJEXIT(); //####
} // SendToMQOutputService::enableMetrics

bool
SendToMQOutputService::getConfiguration(yarp::os::Bottle & details)
{
    ODL_OBJENTER(); //####
    ODL_P1("details = ", &details); //####
    bool result = true;

    details.clear();
    details.addString(_topicOrQueueName);
    details.addInt(_useQueue ? 1 : 0);
    ODL_OBJEXIT_B(result); //####
    return result;
} // SendToMQOutputService::getConfiguration

void
SendToMQOutputService::deactivateConnection(void)
{
    ODL_ENTER(); //####
    clearActive();
    cerr << "connection is dead" << endl; //!!!!
    if (_connection)
    {
        try
        {
            _connection->close();
        }
        catch (cms::CMSException & ex)
        {
            ex.printStackTrace();
        }
    }
    // Destroy resources.
    try
    {
        delete _producer;
        _producer = NULL;
        delete _destination;
        _destination = NULL;
        delete _session;
        _session = NULL;
        delete _connection;
        _connection = NULL;
    }
    catch (cms::CMSException & ex)
    {
        ex.printStackTrace();
    }
    ODL_EXIT(); //####
} // SendToMQOutputService::deactivateConnection

void
SendToMQOutputService::sendMessage(const std::string & aMessage,
                                   const size_t        messageLength)
{
    ODL_OBJENTER(); //####
    ODL_S1s("aMessage = ", aMessage); //####
    ODL_I1("messageLength = ", messageLength); //####
    try
    {
        if (isActive())
        {
            ODL_LOG("(isActive())"); //####
            Common::SendReceiveCounters     newCount(0, 0, messageLength, 1);
            std::auto_ptr<cms::TextMessage> stuff(_session->createTextMessage(aMessage));

            incrementAuxiliaryCounters(newCount);
            _producer->send(stuff.get());
        }
    }
    catch (cms::CMSException & ex)
    {
        ex.printStackTrace();
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT(); //####
} // SendToMQOutputService::sendMessage

bool
SendToMQOutputService::setUpStreamDescriptions(void)
{
    ODL_OBJENTER(); //####
    bool               result = true;
    ChannelDescription description;
    YarpString         rootName(getEndpoint().getName() + "/");

    _inDescriptions.clear();
    description._portName = rootName + "input";
    description._portProtocol = "*";
    description._protocolDescription = "Arbitrary YARP messages";
    _inDescriptions.push_back(description);
    ODL_OBJEXIT_B(result); //####
    return result;
} // SendToMQOutputService::setUpStreamDescriptions

void
SendToMQOutputService::startStreams(void)
{
    ODL_OBJENTER(); //####
    try
    {
        if (! isActive())
        {
            std::string brokerURI(constructURI(_hostName, _hostPort));

            _connectionFactory.reset(cms::ConnectionFactory::createCMSConnectionFactory(brokerURI));
            if (_connectionFactory.get())
            {
                ODL_LOG("(_connectionFactory.get())"); //####
            }
            _connection = _connectionFactory->createConnection(_userName, _password);
            if (_connection)
            {
                ODL_LOG("(_connection)"); //####
                try
                {
                    _connection->start();
                    if (kSessionIsTransacted)
                    {
                        _session = _connection->createSession(cms::Session::SESSION_TRANSACTED);
                    }
                    else
                    {
                        _session = _connection->createSession(cms::Session::AUTO_ACKNOWLEDGE);
                    }
                    if (! _session)
                    {
                        MpM_FAIL_("Could not create session.");
                    }
                }
                catch (cms::CMSException & )
                {
                    // This likely to be a bad password or user name.
                    ODL_LOG("CMSException caught."); //####
                    throw;
                }
            }
            else
            {
                cerr << "Could not create connection." << endl;
            }
            if (_session)
            {
                ODL_LOG("(_session)"); //####
                if (_useQueue)
                {
                    _destination = _session->createQueue(_topicOrQueueName);
                }
                else
                {
                    _destination = _session->createTopic(_topicOrQueueName);
                }
                if (! _destination)
                {
                    if (_useQueue)
                    {
                        MpM_FAIL_("Could not create queue.");
                    }
                    else
                    {
                        MpM_FAIL_("Could not create topic.");
                    }
                }
            }
            if (_destination)
            {
                ODL_LOG("(_destination)"); //####
                _producer = _session->createProducer(_destination);
                if (! _producer)
                {
                    MpM_FAIL_("Could not create producer.");
                }
            }
            if (_producer)
            {
                ODL_LOG("(_producer)"); //####
                _producer->setDeliveryMode(cms::DeliveryMode::NON_PERSISTENT);
                if (_inHandler)
                {
                    _inHandler->setChannel(getInletStream(0));
                    getInletStream(0)->setReader(*_inHandler);
                    setActive();
                }
            }
        }
    }
    catch (cms::CMSException & ex)
    {
        ex.printStackTrace();
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT(); //####
} // SendToMQOutputService::startStreams

void
SendToMQOutputService::stopStreams(void)
{
    ODL_OBJENTER(); //####
    try
    {
        if (isActive())
        {
            deactivateConnection();
        }
    }
    catch (...)
    {
        ODL_LOG("Exception caught"); //####
        throw;
    }
    ODL_OBJEXIT(); //####
} // SendToMQOutputService::stopStreams

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
