//--------------------------------------------------------------------------------------------------
//
//  File:       m+mScannerThread.cpp
//
//  Project:    m+m
//
//  Contains:   The class declaration for the background port and service scanner.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2014 by H Plus Technologies Ltd. and Simon Fraser University.
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
//  Created:    2014-07-17
//
//--------------------------------------------------------------------------------------------------

#include "m+mScannerThread.hpp"

#include "m+mChannelEntry.hpp"
#include "m+mEntitiesPanel.hpp"
#include "m+mEntityData.hpp"
#include "m+mManagerApplication.hpp"
#include "m+mPortData.hpp"

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)

#if (! MAC_OR_LINUX_)
# include <Windows.h>
#endif //! MAC_OR_LINUX_

/*! @file

 @brief The class declaration for the background port and service scanner. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace MplusM;
using namespace MPlusM_Manager;
using namespace std;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

/*! @brief The minimum time between background scans in milliseconds. */
static const int64 kMinScanInterval = 5000;

#if (defined(CHECK_FOR_STALE_PORTS_) && (! defined(DO_SINGLE_CHECK_FOR_STALE_PORTS_)))
/*! @brief The minimum time between removing stale entries, in milliseconds. */
static const int64 kMinStaleInterval = 60000;
#endif // defined(CHECK_FOR_STALE_PORTS_) && (! defined(DO_SINGLE_CHECK_FOR_STALE_PORTS_))

#if defined(__APPLE__)
# pragma mark Global constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

/*! @brief Locate the IP address and port corresponding to a port name.
 @param detectedPorts The set of detected YARP ports.
 @param portName The port name to search for.
 @param ipAddress The IP address of the port.
 @param ipPort The IP port of the port. */
static void
findMatchingIpAddressAndPort(const Utilities::PortVector & detectedPorts,
                             const YarpString &            portName,
                             YarpString &                  ipAddress,
                             YarpString &                  ipPort)
{
    ODL_ENTER(); //####
    ODL_P3("detectedPorts = ", &detectedPorts, "ipAddress = ", &ipAddress, "ipPort = ", //####
           &ipPort); //####
    ODL_S1s("portName = ", portName); //####
    ipAddress = ipPort = "";
    for (Utilities::PortVector::const_iterator walker(detectedPorts.begin());
         detectedPorts.end() != walker; ++walker)
    {
        YarpString walkerName(walker->_portName);

        if (portName == walkerName)
        {
            ipAddress = walker->_portIpAddress;
            ipPort = walker->_portPortNumber;
            break;
        }

    }
    ODL_EXIT(); //####
} // findMatchingIpAddressAndPort

/*! @brief Extract the IP address and port number from a combined string.
 @param combined The combined IP address and port number.
 @param ipAddress The IP address of the port.
 @param ipPort The IP port of the port. */
static void
splitCombinedAddressAndPort(const YarpString & combined,
                            YarpString &       ipAddress,
                            YarpString &       ipPort)
{
    ODL_ENTER(); //####
    ODL_S1s("combined = ", combined); //####
    ODL_P2("ipAddress = ", &ipAddress, "ipPort = ", &ipPort); //####
    size_t splitPos = combined.find(":");

    if (YarpString::npos == splitPos)
    {
        ipAddress = ipPort = "";
    }
    else
    {
        ipAddress = combined.substr(0, splitPos);
        ipPort = combined.substr(splitPos + 1);
    }
    ODL_EXIT(); //####
} // splitCombinedAddressAndPort

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and Destructors
#endif // defined(__APPLE__)

ScannerThread::ScannerThread(ManagerWindow & window,
                             const bool      delayFirstScan) :
    inherited("port scanner"), _window(window), _rememberedPorts(), _detectedServices(),
    _standalonePorts(),
#if (defined(CHECK_FOR_STALE_PORTS_) && (! defined(DO_SINGLE_CHECK_FOR_STALE_PORTS_)))
    _lastStaleTime(- (2 * kMinStaleInterval)),
#endif // efined(CHECK_FOR_STALE_PORTS_) && (! defined(DO_SINGLE_CHECK_FOR_STALE_PORTS_))
    _inputOnlyPort(NULL), _outputOnlyPort(NULL), _cleanupSoon(false),
    _delayScan(delayFirstScan),
#if (defined(CHECK_FOR_STALE_PORTS_) && defined(DO_SINGLE_CHECK_FOR_STALE_PORTS_))
    _initialStaleCheckDone(false),
#endif // defined(CHECK_FOR_STALE_PORTS_) && defined(DO_SINGLE_CHECK_FOR_STALE_PORTS_)
    _portsValid(false), _scanCanProceed(true), _scanIsComplete(false), _scanSoon(false)
{
    ODL_ENTER(); //####
    ODL_S1s("name = ", name); //####
    ODL_P1("window = ", &window); //####
    _inputOnlyPortName = Common::GetRandomChannelName(HIDDEN_CHANNEL_PREFIX_
                                                      "checkdirection/channel_");
    _outputOnlyPortName = Common::GetRandomChannelName(HIDDEN_CHANNEL_PREFIX_
                                                       "checkdirection/channel_");
    _inputOnlyPort = new Common::GeneralChannel(false);
    if (_inputOnlyPort)
    {
        _inputOnlyPort->setInputMode(true);
        _inputOnlyPort->setOutputMode(false);
        _outputOnlyPort = new Common::GeneralChannel(true);
        if (_outputOnlyPort)
        {
            _outputOnlyPort->setInputMode(false);
            _outputOnlyPort->setOutputMode(true);
            if (_inputOnlyPort->openWithRetries(_inputOnlyPortName, STANDARD_WAIT_TIME_) &&
                _outputOnlyPort->openWithRetries(_outputOnlyPortName, STANDARD_WAIT_TIME_))
            {
                _portsValid = true;
                _window.setScannerThread(this);
            }
        }
    }
    ODL_EXIT_P(this); //####
} // ScannerThread::ScannerThread

ScannerThread::~ScannerThread(void)
{
    ODL_OBJENTER(); //####
    stopThread(3000); // Give thread 3 seconds to shut down.
    if (_inputOnlyPort)
    {
#if defined(MpM_DoExplicitClose)
        _inputOnlyPort->close();
#endif // defined(MpM_DoExplicitClose)
        Common::GeneralChannel::RelinquishChannel(_inputOnlyPort);
    }
    if (_outputOnlyPort)
    {
#if defined(MpM_DoExplicitClose)
        _outputOnlyPort->close();
#endif // defined(MpM_DoExplicitClose)
        Common::GeneralChannel::RelinquishChannel(_outputOnlyPort);
    }
    _portsValid = false;
    _detectedServices.clear();
    _rememberedPorts.clear();
    _standalonePorts.clear();
    ODL_OBJEXIT(); //####
} // ScannerThread::~ScannerThread

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

void
ScannerThread::addEntities(const Utilities::PortVector & detectedPorts)
{
    ODL_OBJENTER(); //####
    ODL_P1("detectedPorts = ", &detectedPorts); //####
    PortDataMap portsSeen;

    for (ServiceMap::const_iterator outer(_detectedServices.begin());
         (_detectedServices.end() != outer) && (! threadShouldExit()); ++outer)
    {
        Utilities::ServiceDescriptor descriptor(outer->second);
        bool                         isAdapter = (0 < descriptor._clientChannels.size());
        Common::ChannelVector &      clientChannels = descriptor._clientChannels;
        Common::ChannelVector &      inChannels = descriptor._inputChannels;
        Common::ChannelVector &      outChannels = descriptor._outputChannels;
        YarpString                   ipAddress;
        YarpString                   ipPort;
        EntityData *                 anEntity = new EntityData(isAdapter ? kContainerKindAdapter :
                                                               kContainerKindService,
                                                               descriptor._serviceName,
                                                               descriptor._kind,
                                                               descriptor._description,
                                                               descriptor._extraInfo,
                                                               descriptor._requestsDescription);
        PortData *                   aPort = anEntity->addPort(descriptor._channelName, "", "",
                                                               kPortUsageService,
                                                               kPortDirectionInput);

        findMatchingIpAddressAndPort(detectedPorts, descriptor._channelName, ipAddress, ipPort);
        anEntity->setIPAddress(ipAddress);
        aPort->setPortNumber(ipPort);
        for (Common::ChannelVector::const_iterator inner = inChannels.begin();
             (inChannels.end() != inner) && (! threadShouldExit()); ++inner)
        {
            Common::ChannelDescription aChannel(*inner);

            aPort = anEntity->addPort(aChannel._portName, aChannel._portProtocol,
                                      aChannel._protocolDescription, kPortUsageInputOutput,
                                      kPortDirectionInput);
            findMatchingIpAddressAndPort(detectedPorts, aChannel._portName, ipAddress, ipPort);
            aPort->setPortNumber(ipPort);
        }
        for (Common::ChannelVector::const_iterator inner = outChannels.begin();
             (outChannels.end() != inner) && (! threadShouldExit()); ++inner)
        {
            Common::ChannelDescription aChannel(*inner);

            aPort = anEntity->addPort(aChannel._portName, aChannel._portProtocol,
                                      aChannel._protocolDescription, kPortUsageInputOutput,
                                      kPortDirectionOutput);
            findMatchingIpAddressAndPort(detectedPorts, aChannel._portName, ipAddress, ipPort);
            aPort->setPortNumber(ipPort);
        }
        for (Common::ChannelVector::const_iterator inner = clientChannels.begin();
             (clientChannels.end() != inner) && (! threadShouldExit()); ++inner)
        {
            Common::ChannelDescription aChannel(*inner);

            aPort = anEntity->addPort(aChannel._portName, aChannel._portProtocol,
                                      aChannel._protocolDescription, kPortUsageClient,
                                      kPortDirectionInputOutput);
            findMatchingIpAddressAndPort(detectedPorts, aChannel._portName, ipAddress, ipPort);
            aPort->setPortNumber(ipPort);
        }
        for (size_t ii = 0, mm = descriptor._argumentList.size(); mm > ii; ++ii)
        {
            Utilities::BaseArgumentDescriptor * argDesc = descriptor._argumentList[ii];

            if (argDesc)
            {
                anEntity->addArgumentDescription(argDesc);
            }
        }
        _workingData.addEntity(anEntity);
    }
    // Convert the detected standalone ports into entities in the background list.
    for (SingularPortMap::const_iterator walker(_standalonePorts.begin());
         (_standalonePorts.end() != walker) && (! threadShouldExit()); ++walker)
    {
        // The key is 'ipaddress:port'
        YarpString   ipAddress;
        YarpString   ipPort;
        EntityData * anEntity = new EntityData(kContainerKindOther, walker->first, "", "", "", "");
        PortUsage    usage;

        splitCombinedAddressAndPort(walker->first, ipAddress, ipPort);
        anEntity->setIPAddress(ipAddress);
        switch (Utilities::GetPortKind(walker->second._name))
        {
            case Utilities::kPortKindClient :
                usage = kPortUsageClient;
                break;

            case Utilities::kPortKindRegistryService :
            case Utilities::kPortKindService :
                usage = kPortUsageService;
                break;

            default :
                usage = kPortUsageOther;
                break;

        }
        PortData * aPort = anEntity->addPort(walker->second._name, "", "", usage,
                                             walker->second._direction);

        aPort->setPortNumber(ipPort);
        _workingData.addEntity(anEntity);
    }
    ODL_OBJEXIT(); //####
} // ScannerThread::addEntities

void
ScannerThread::addPortConnections(const Utilities::PortVector & detectedPorts,
                                  Common::CheckFunction         checker,
                                  void *                        checkStuff)
{
    ODL_OBJENTER(); //####
    ODL_P2("detectedPorts = ", &detectedPorts, "checkStuff = ", checkStuff); //####
    _workingData.clearConnections();
    for (Utilities::PortVector::const_iterator outer(detectedPorts.begin());
         (detectedPorts.end() != outer) && (! threadShouldExit()); ++outer)
    {
        YarpString outerName(outer->_portName);

        if (_rememberedPorts.end() != _rememberedPorts.find(outerName))
        {
            Common::ChannelVector inputs;
            Common::ChannelVector outputs;

            Utilities::GatherPortConnections(outer->_portName, inputs, outputs,
                                             Utilities::kInputAndOutputOutput, true, checker,
                                             checkStuff);
            for (Common::ChannelVector::const_iterator inner(outputs.begin());
                 (outputs.end() != inner) && (! threadShouldExit()); ++inner)
            {
                YarpString innerName(inner->_portName);

                if (_rememberedPorts.end() != _rememberedPorts.find(innerName))
                {
                    _workingData.addConnection(innerName, outerName, inner->_portMode);
                }
                yield();
            }
        }
        yield();
    }
    ODL_OBJEXIT(); //####
} // ScannerThread::addPortConnections

void
ScannerThread::addRegularPortEntities(const Utilities::PortVector & detectedPorts,
                                      Common::CheckFunction         checker,
                                      void *                        checkStuff)
{
    ODL_OBJENTER(); //####
    ODL_P2("detectedPorts = ", &detectedPorts, "checkStuff = ", checkStuff); //####
    _standalonePorts.clear();
    for (Utilities::PortVector::const_iterator walker(detectedPorts.begin());
         (detectedPorts.end() != walker) && (! threadShouldExit()); ++walker)
    {
        YarpString walkerName(walker->_portName);

        if (_rememberedPorts.end() == _rememberedPorts.find(walkerName))
        {
            EntitiesPanel &  entitiesPanel(_window.getEntitiesPanel());
            YarpString       caption(walker->_portIpAddress + ":" + walker->_portPortNumber);
            NameAndDirection info;
            ChannelEntry *   oldEntry = entitiesPanel.findKnownPort(walkerName);

            _rememberedPorts.insert(walkerName);
            info._name = walkerName;
            info._direction = determineDirection(oldEntry, walker->_portName, checker, checkStuff);
            _standalonePorts[caption] = info;
        }
        yield();
    }
    ODL_OBJEXIT(); //####
} // ScannerThread::addRegularPortEntities

void
ScannerThread::addServices(const YarpStringVector & services,
                           Common::CheckFunction    checker,
                           void *                   checkStuff)
{
    ODL_OBJENTER(); //####
    ODL_P2("services = ", &services, "checkStuff = ", checkStuff); //####
    _detectedServices.clear();
    for (YarpStringVector::const_iterator outer(services.begin());
         (services.end() != outer) && (! threadShouldExit()); ++outer)
    {
        YarpString outerName(*outer);

        if (_detectedServices.end() == _detectedServices.find(outerName))
        {
            Utilities::ServiceDescriptor descriptor;

            if (Utilities::GetNameAndDescriptionForService(*outer, descriptor, STANDARD_WAIT_TIME_,
                                                           checker, checkStuff))
            {
                _detectedServices[outerName] = descriptor;
                _rememberedPorts.insert(descriptor._channelName);
                Common::ChannelVector & clientChannels = descriptor._clientChannels;
                Common::ChannelVector & inChannels = descriptor._inputChannels;
                Common::ChannelVector & outChannels = descriptor._outputChannels;

                for (Common::ChannelVector::const_iterator inner = inChannels.begin();
                     (inChannels.end() != inner) && (! threadShouldExit()); ++inner)
                {
                    Common::ChannelDescription aChannel(*inner);

                    _rememberedPorts.insert(aChannel._portName);
                    yield();
                }
                for (Common::ChannelVector::const_iterator inner = outChannels.begin();
                     (outChannels.end() != inner) && (! threadShouldExit()); ++inner)
                {
                    Common::ChannelDescription aChannel(*inner);

                    _rememberedPorts.insert(aChannel._portName);
                    yield();
                }
                for (Common::ChannelVector::const_iterator inner = clientChannels.begin();
                     (clientChannels.end() != inner) && (! threadShouldExit()); ++inner)
                {
                    Common::ChannelDescription aChannel(*inner);

                    _rememberedPorts.insert(aChannel._portName);
                    yield();
                }
            }
        }
        yield();
    }
    ODL_OBJEXIT(); //####
} // ScannerThread::addServices

bool
ScannerThread::checkAndClearIfScanIsComplete(void)
{
    ODL_OBJENTER(); //####
    for (bool locked = conditionallyAcquireForRead(); ! locked;
         locked = conditionallyAcquireForRead())
    {
        Utilities::GoToSleep(SHORT_SLEEP_);
    }
    bool result = _scanIsComplete;

    _scanIsComplete = false;
    ODL_B1("_scanIsComplete <- ", _scanIsComplete); //####
    relinquishFromRead();
    ODL_OBJEXIT_B(result); //####
    return result;
} // ScannerThread::checkAndClearIfScanIsComplete

bool
ScannerThread::conditionallyAcquireForRead(void)
{
    ODL_OBJENTER(); //####
    bool result = _lock.tryEnterRead();

    ODL_OBJEXIT_B(result); //####
    return result;
} // ScannerThread::conditionallyAcquireForRead

bool
ScannerThread::conditionallyAcquireForWrite(void)
{
    ODL_OBJENTER(); //####
    bool result = _lock.tryEnterWrite();

    ODL_OBJEXIT_B(result); //####
    return result;
} // ScannerThread::conditionallyAcquireForWrite

PortDirection
ScannerThread::determineDirection(ChannelEntry *        oldEntry,
                                  const YarpString &    portName,
                                  Common::CheckFunction checker,
                                  void *                checkStuff)
{
    ODL_OBJENTER(); //####
    ODL_S1s("portName = ", portName); //####
    ODL_P1("checkStuff = ", checkStuff); //####
    PortDirection result = kPortDirectionUnknown;

    if (oldEntry)
    {
        result = oldEntry->getDirection();
    }
    else if (_portsValid)
    {
        bool canDoInput = false;
        bool canDoOutput = false;

        // First, check if we are looking at a client port - because of how they are
        // constructed, attempting to connect to them will result in a hang, so we just
        // treat them as I/O.
        switch (Utilities::GetPortKind(portName))
        {
            case Utilities::kPortKindClient :
                canDoInput = canDoOutput = true;
                break;

            case Utilities::kPortKindRegistryService :
            case Utilities::kPortKindService :
                canDoInput = true;
                break;

            default :
                // Determine by doing a test connection.
                if (Utilities::NetworkConnectWithRetries(_outputOnlyPortName, portName,
                                                         STANDARD_WAIT_TIME_, false, checker,
                                                         checkStuff))
                {
                    canDoInput = true;
                    if (! Utilities::NetworkDisconnectWithRetries(_outputOnlyPortName, portName,
                                                                  STANDARD_WAIT_TIME_, checker,
                                                                  checkStuff))
                    {
                        ODL_LOG("(! Utilities::NetworkDisconnectWithRetries(" //####
                                "lOutputOnlyPortName, portName, STANDARD_WAIT_TIME_, " //####
                                "checker, checkStuff))"); //####
                    }
                }
                if (Utilities::NetworkConnectWithRetries(portName, _inputOnlyPortName,
                                                         STANDARD_WAIT_TIME_, false, checker,
                                                         checkStuff))
                {
                    canDoOutput = true;
                    if (! Utilities::NetworkDisconnectWithRetries(portName,  _inputOnlyPortName,
                                                                  STANDARD_WAIT_TIME_, checker,
                                                                  checkStuff))
                    {
                        ODL_LOG("(! Utilities::NetworkDisconnectWithRetries(portName, " //####
                                "lInputOnlyPortName, STANDARD_WAIT_TIME_, checker, " //####
                                "checkStuff))"); //####
                    }
                }
                break;

        }
        if (canDoInput)
        {
            result = (canDoOutput ? kPortDirectionInputOutput : kPortDirectionInput);
        }
        else if (canDoOutput)
        {
            result = kPortDirectionOutput;
        }
        else
        {
            result = kPortDirectionUnknown;
        }
    }
    else
    {
        result = kPortDirectionUnknown;
    }
    ODL_OBJEXIT_LL(static_cast<long>(result)); //####
    return result;
} // ScannerThread::determineDirection

void
ScannerThread::doCleanupSoon(void)
{
    ODL_OBJENTER(); //####
    bool locked = conditionallyAcquireForWrite();
    bool needToLeave = false;

    for ( ; (! locked) && (! needToLeave); locked = conditionallyAcquireForWrite())
    {
        if (threadShouldExit())
        {
            ODL_LOG("threadShouldExit()"); //####
            needToLeave = true;
        }
        else
        {
            Utilities::GoToSleep(SHORT_SLEEP_);
        }
    }
    if (locked)
    {
        _cleanupSoon = true;
        relinquishFromWrite();
    }
    ODL_OBJEXIT(); //####
} // ScannerThread::doCleanupSoon

void
ScannerThread::doScanSoon(void)
{
    ODL_OBJENTER(); //####
    bool locked = conditionallyAcquireForWrite();
    bool needToLeave = false;

    for ( ; (! locked) && (! needToLeave); locked = conditionallyAcquireForWrite())
    {
        if (threadShouldExit())
        {
            ODL_LOG("threadShouldExit()"); //####
            needToLeave = true;
        }
        else
        {
            Utilities::GoToSleep(SHORT_SLEEP_);
        }
    }
    if (locked)
    {
        _scanSoon = true;
        relinquishFromWrite();
    }
    ODL_OBJEXIT(); //####
} // ScannerThread::doScanSoon

bool
ScannerThread::gatherEntities(Utilities::PortVector & detectedPorts,
                              Common::CheckFunction   checker,
                              void *                  checkStuff)
{
    ODL_OBJENTER(); //####
    ODL_P2("detectedPorts = ", &detectedPorts, "checkStuff = ", checkStuff); //####
    bool  okSoFar;
#if (defined(CHECK_FOR_STALE_PORTS_) && (! defined(DO_SINGLE_CHECK_FOR_STALE_PORTS_)))
    int64 now = Time::currentTimeMillis();
#endif //defined(CHECK_FOR_STALE_PORTS_) && (! defined(DO_SINGLE_CHECK_FOR_STALE_PORTS_))

    // Mark our utility ports as known.
#if defined(CHECK_FOR_STALE_PORTS_)
# if defined(DO_SINGLE_CHECK_FOR_STALE_PORTS_)
    if (! _initialStaleCheckDone)
    {
        Utilities::RemoveStalePorts();
        _initialStaleCheckDone = true;
    }
# else // ! defined(DO_SINGLE_CHECK_FOR_STALE_PORTS_)
    if ((_lastStaleTime + kMinStaleInterval) <= now)
    {
        Utilities::RemoveStalePorts();
        _lastStaleTime = now;
    }
# endif // ! defined(DO_SINGLE_CHECK_FOR_STALE_PORTS_)
#endif // defined(CHECK_FOR_STALE_PORTS_)
    if (Utilities::GetDetectedPortList(detectedPorts))
    {
        okSoFar = true;
    }
    else
    {
        // Try again.
        okSoFar = Utilities::GetDetectedPortList(detectedPorts);
    }
    if (okSoFar)
    {
        bool             servicesSeen;
        YarpStringVector services;

        _detectedServices.clear();
        _rememberedPorts.clear();
        _rememberedPorts.insert(_inputOnlyPortName);
        _rememberedPorts.insert(_outputOnlyPortName);
        if (Utilities::GetServiceNames(services, true, checker, checkStuff))
        {
            servicesSeen = true;
        }
        else
        {
            // Try again.
            servicesSeen = Utilities::GetServiceNames(services, true, checker, checkStuff);
        }
        if (servicesSeen)
        {
            // Record the services to be displayed.
            addServices(services, checker, checkStuff);
        }
        // Record the ports that are standalone.
        addRegularPortEntities(detectedPorts, checker, checkStuff);
        // Record the port connections.
        addPortConnections(detectedPorts, checker, checkStuff);
        ManagerApplication * ourApp = ManagerApplication::getApp();

        if (ourApp && servicesSeen)
        {
            ourApp->connectPeekChannel();
        }
    }
    ODL_OBJEXIT_B(okSoFar); //####
    return okSoFar;
} // ScannerThread::gatherEntities

void
ScannerThread::relinquishFromRead(void)
{
    ODL_OBJENTER(); //####
    _lock.exitRead();
    ODL_OBJEXIT(); //####
} // ScannerThread::relinquishFromRead

void
ScannerThread::relinquishFromWrite(void)
{
    ODL_OBJENTER(); //####
    _lock.exitWrite();
    ODL_OBJEXIT(); //####
} // ScannerThread::relinquishFromWrite

void
ScannerThread::run(void)
{
    ODL_OBJENTER(); //####
    for ( ; ! threadShouldExit(); )
    {
        bool                  needToLeave = false;
        Utilities::PortVector detectedPorts;

        if (_cleanupSoon)
        {
            unconditionallyAcquireForWrite();
            _cleanupSoon = false;
            ODL_B1("_cleanupSoon <- ", _cleanupSoon); //####
            relinquishFromWrite();
            Utilities::RemoveStalePorts();
        }
        else if (_delayScan)
        {
            bool shouldCleanupSoon = false;
            bool shouldScanSoon = false;
            int  kk = (LONG_SLEEP_ / VERY_SHORT_SLEEP_);

            _delayScan = false;
            do
            {
                bool locked = conditionallyAcquireForRead();

                for ( ; (! locked) && (! needToLeave); locked = conditionallyAcquireForRead())
                {
                    for (int ii = 0, mm = (MIDDLE_SLEEP_ / VERY_SHORT_SLEEP_);
                         (mm > ii) && (0 <= kk) && (! needToLeave); ++ii, --kk)
                    {
                        if (threadShouldExit())
                        {
                            ODL_LOG("threadShouldExit()"); //####
                            needToLeave = true;
                        }
                        else
                        {
                            Utilities::GoToSleep(VERY_SHORT_SLEEP_);
                        }
                    }
                }
                if (locked)
                {
                    ODL_LOG("(locked)"); //####
                    shouldCleanupSoon = _cleanupSoon;
                    shouldScanSoon = _scanSoon;
                    ODL_B2("shouldCleanupSoon <- ", shouldCleanupSoon, //####
                           "shouldScanSoon <- ", shouldScanSoon); //####
                    relinquishFromRead();
                    // Sleep at least once!
                    if (0 <= kk)
                    {
                        --kk;
                        Utilities::GoToSleep(VERY_SHORT_SLEEP_);
                    }
                }
                if (needToLeave || shouldCleanupSoon || shouldScanSoon)
                {
                    ODL_LOG("(needToLeave || shouldCleanupSoon || shouldScanSoon)"); //####
                    break;
                }

            }
            while (0 <= kk);
        }
        else if (gatherEntities(detectedPorts, CheckForExit))
        {
            int64 loopStartTime = Time::currentTimeMillis();

            addEntities(detectedPorts);
            // Indicate that the scan data is available.
            unconditionallyAcquireForWrite();
            _scanIsComplete = true;
            _scanSoon = _scanCanProceed = false;
            ODL_B3("_scanIsComplete <- ", _scanIsComplete, "_scanCanProceed <- ", //####
                   _scanCanProceed, "_scanSoon <- ", _scanSoon); //####
            relinquishFromWrite();
            // The data has been gathered, so it's safe for the foreground thread to process it -
            // force a repaint of the displayed panel, which will retrieve our data.
            triggerRepaint();
            bool canProceed = false;

            do
            {
                for (int ii = 0, mm = (MIDDLE_SLEEP_ / VERY_SHORT_SLEEP_);
                     (mm > ii) && (! needToLeave) && (! _cleanupSoon); ++ii)
                {
                    if (threadShouldExit())
                    {
                        ODL_LOG("threadShouldExit()"); //####
                        needToLeave = true;
                    }
                    else
                    {
                        Utilities::GoToSleep(VERY_SHORT_SLEEP_);
                    }
                }
                if (needToLeave)
                {
                    ODL_LOG("(needToLeave)"); //####
                    break;
                }

                // Wait for the scan data to be processed, and then continue with the next scan.
                bool locked = conditionallyAcquireForRead();

                for ( ; (! locked) && (! needToLeave); locked = conditionallyAcquireForRead())
                {
                    for (int ii = 0, mm = (MIDDLE_SLEEP_ / VERY_SHORT_SLEEP_);
                         (mm > ii) && (! needToLeave) && (! _cleanupSoon); ++ii)
                    {
                        if (threadShouldExit())
                        {
                            ODL_LOG("threadShouldExit()"); //####
                            needToLeave = true;
                        }
                        else
                        {
                            Utilities::GoToSleep(VERY_SHORT_SLEEP_);
                        }
                    }
                }
                if (locked)
                {
                    ODL_LOG("(locked)"); //####
                    canProceed = _scanCanProceed;
                    ODL_B1("canProceed <- ", canProceed); //####
                    relinquishFromRead();
                }
            }
            while ((! canProceed) && (! _cleanupSoon) && (! needToLeave));
            if (needToLeave)
            {
                ODL_LOG("(needToLeave)"); //####
                break;
            }

            if (canProceed)
            {
                ODL_LOG("(canProceed)"); //####
                _workingData.clearOutData();
                if (! threadShouldExit())
                {
                    ODL_LOG("! threadShouldExit()"); //####
                    int64 loopEndTime = Time::currentTimeMillis();
                    int64 delayAmount = (loopStartTime + kMinScanInterval) - loopEndTime;

                    if (kMinScanInterval < delayAmount)
                    {
                        delayAmount = kMinScanInterval;
                    }
                    if (0 < delayAmount)
                    {
                        // Add a bit of delay.
                        bool shouldCleanupSoon = false;
                        bool shouldScanSoon = false;
                        int  kk = static_cast<int>(delayAmount / VERY_SHORT_SLEEP_);

                        do
                        {
                            bool locked = conditionallyAcquireForRead();

                            for ( ; (! locked) && (! needToLeave);
                                 locked = conditionallyAcquireForRead())
                            {
                                for (int ii = 0, mm = (MIDDLE_SLEEP_ / VERY_SHORT_SLEEP_);
                                     (mm > ii) && (0 <= kk) && (! needToLeave); ++ii, --kk)
                                {
                                    if (threadShouldExit())
                                    {
                                        ODL_LOG("threadShouldExit()"); //####
                                        needToLeave = true;
                                    }
                                    else
                                    {
                                        Utilities::GoToSleep(VERY_SHORT_SLEEP_);
                                    }
                                }
                            }
                            if (locked)
                            {
                                ODL_LOG("(locked)"); //####
                                shouldCleanupSoon = _cleanupSoon;
                                shouldScanSoon = _scanSoon;
                                ODL_B2("shouldCleanupSoon <- ", shouldCleanupSoon, //####
                                       "shouldScanSoon <- ", shouldScanSoon); //####
                                relinquishFromRead();
                                // Sleep at least once!
                                if (0 <= kk)
                                {
                                    --kk;
                                    Utilities::GoToSleep(VERY_SHORT_SLEEP_);
                                }
                                if (threadShouldExit())
                                {
                                    ODL_LOG("threadShouldExit()"); //####
                                    needToLeave = true;
                                }
                            }
                            if (needToLeave || shouldCleanupSoon || shouldScanSoon)
                            {
                                ODL_LOG("(needToLeave || shouldCleanupSoon || " //####
                                        "shouldScanSoon)"); //####
                                break;
                            }

                        }
                        while (0 <= kk);
                    }
                    else
                    {
                        std::stringstream buff;

                        buff << ((loopEndTime - loopStartTime) / 1000.0);
                        MpM_INFO_((YarpString("actual interval = ") + buff.str() +
                                   YarpString(" seconds")).c_str());
                        yield();
                    }
                }
            }
        }
        else
        {
            bool shouldCleanupSoon = false;
            bool shouldScanSoon = false;
            int  kk = (LONG_SLEEP_ / VERY_SHORT_SLEEP_);

            do
            {
                bool locked = conditionallyAcquireForRead();

                for ( ; (! locked) && (! needToLeave); locked = conditionallyAcquireForRead())
                {
                    for (int ii = 0, mm = (MIDDLE_SLEEP_ / VERY_SHORT_SLEEP_);
                         (mm > ii) && (0 <= kk) && (! needToLeave); ++ii, --kk)
                    {
                        if (threadShouldExit())
                        {
                            ODL_LOG("threadShouldExit()"); //####
                            needToLeave = true;
                        }
                        else
                        {
                            Utilities::GoToSleep(VERY_SHORT_SLEEP_);
                        }
                    }
                }
                if (locked)
                {
                    ODL_LOG("(locked)"); //####
                    shouldCleanupSoon = _cleanupSoon;
                    shouldScanSoon = _scanSoon;
                    ODL_B2("shouldCleanupSoon <- ", shouldCleanupSoon, //####
                           "shouldScanSoon <- ", shouldScanSoon); //####
                    relinquishFromRead();
                    // Sleep at least once!
                    if (0 <= kk)
                    {
                        --kk;
                        Utilities::GoToSleep(VERY_SHORT_SLEEP_);
                    }
                }
                if (needToLeave || shouldCleanupSoon || shouldScanSoon)
                {
                    ODL_LOG("(needToLeave || shouldCleanupSoon || shouldScanSoon)"); //####
                    break;
                }

            }
            while (0 <= kk);
        }
    }
    ODL_OBJEXIT(); //####
} // ScannerThread::run

void
ScannerThread::scanCanProceed(void)
{
    ODL_OBJENTER(); //####
    unconditionallyAcquireForWrite();
    _scanCanProceed = true;
    relinquishFromWrite();
    ODL_B1("_scanCanProceed <- ", _scanCanProceed); //####
    ODL_OBJEXIT(); //####
} // ScannerThread::scanCanProceed

void
ScannerThread::triggerRepaint(void)
{
    ODL_OBJENTER(); //####
    // Because this is a background thread, we mustn't do any UI work without first grabbing a
    // MessageManagerLock.
    const MessageManagerLock mml(Thread::getCurrentThread());

    // If something is trying to kill this job, the lock will fail, in which case we'd better
    // return.
    if (mml.lockWasGained())
    {
        _window.getEntitiesPanel().repaint();
    }
    ODL_OBJEXIT(); //####
} // ScannerThread::triggerRepaint

void
ScannerThread::unconditionallyAcquireForRead(void)
{
    ODL_OBJENTER(); //####
    _lock.enterRead();
    ODL_OBJEXIT(); //####
} // ScannerThread::unconditionallyAcquireForRead

void
ScannerThread::unconditionallyAcquireForWrite(void)
{
    ODL_OBJENTER(); //####
    _lock.enterWrite();
    ODL_OBJEXIT(); //####
} // ScannerThread::unconditionallyAcquireForWrite

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)
