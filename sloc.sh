#!/bin/sh

sloccount ClientList CommonTests Exemplars KinectV2 LeapMotion MovementDb ParserTest PortLister RegistryService RequestCounter RequestInfo ServiceLister Version examples mpm odl
echo ---------------
echo subtract the following -
echo ---------------
sloccount KinectV2/KinectV2InputService/KinectLib LeapMotion/LeapMotionInputService/Leap*.h
