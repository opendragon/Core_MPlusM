#!/bin/sh

CheckLength 100|grep -v -e glm -e rapidjson -e '^ *$' -e JuceLibraryCode -e ViconLib -e NatNetSDK -e optionparser -e KinectLib -e CMakeFiles -e stdafx.h -e Leap.h -e LeapMath.h
