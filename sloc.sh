#!/bin/sh

sloccount Address Blob ClientList CommonTests Emotiv FindServices JavaScript KinectV2 LeapMotion MovementDb OpenStage ParserTest PortLister ProComp2 RegistryService RequestCounter RequestInfo ServiceLister ServiceMetrics StopService Tunnel Unreal Version ViconDataStream examples mpm odl
echo ---------------
echo subtract the following -
echo ---------------
sloccount KinectV2/KinectV2InputService/KinectLib LeapMotion/LeapMotionInputService/Leap*.h OpenStage/OpenStageInputService/glm ViconDataStream/ViconDataStreamInputService/ViconLib mpm/swig_mpm_in mpm/swig_mpm_out_csharp mpm/swig_mpm_out_python odl/swig_odl_in odl/swig_odl_out_csharp odl/swig_odl_out_python ProComp2/ProComp2InputService/ProCompLib/inc
