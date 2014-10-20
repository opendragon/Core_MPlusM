#!/bin/bash

examples/RecordIntegersService/mpmRecordIntegersService &
examples/TruncateFloatService/mpmTruncateFloatService /shorten &
examples/RandomBurstService/mpmRandomBurstService -s 7 -p 4.1 &
examples/RandomNumberService/mpmRandomNumberService &
examples/RandomNumberAdapter/mpmRandomNumberAdapter &
examples/RecordIntegersService/mpmRecordIntegersService -t two &
examples/AbsorberService/mpmAbsorberService /blotter &
examples/TruncateFloatService/mpmTruncateFloatService -t one &
mpmLeapMotionInputService &
