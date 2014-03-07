#
#  YarpPlusPlus/Tests/CMakeLists.txt
#  YarpPlusPlus
#
#  Created by Norman Jaffe on 2014-02-28.
#  Copyright (c) 2014 OpenDragon. All rights reserved.
#

include_directories("${YPP_DEBUGGING_DIR}"
                    "${YPP_COMMON_DIR}"
                    “${YPP_PARSER_DIR}”)

add_library(YPPTestSupport
            YPPTEndpointStatusReporter.cpp
            YPPTTest03Handler.cpp
            YPPTTest04Handler.cpp
            YPPTTest05Handler.cpp
            YPPTTest05HandlerCreator.cpp
            YPPTTest08Handler.cpp
            YPPTTest09DefaultRequestHandler.cpp
            YPPTTest09Service.cpp
            YPPTTest10DefaultRequestHandler.cpp
            YPPTTest10Service.cpp
            YPPTTest11EchoRequestHandler.cpp
            YPPTTest11Service.cpp
            YPPTTest12EchoRequestHandler.cpp
            YPPTTest12Service.cpp
            YPPTTest14EchoRequestHandler.cpp
            YPPTTest14Service.cpp
            YPPTTest15EchoRequestHandler.cpp
            YPPTTest15Service.cpp
            YPPTTest16EchoRequestHandler.cpp
            YPPTTest16Service.cpp)

install(TARGETS YPPTestSupport DESTINATION bin)
install(FILES YPPTestSupport DESTINATION include)
