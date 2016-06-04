//--------------------------------------------------------------------------------------------------
//
//  File:       m+mVertexBuffer.cpp
//
//  Project:    m+m
//
//  Contains:   The class definition for vertex buffers for the Leap Motion display output service.
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
//  Created:    2016-05-20
//
//--------------------------------------------------------------------------------------------------

#include "m+mVertexBuffer.hpp"

//#include <odl/ODEnableLogging.h>
#include <odl/ODLogging.h>

#if defined(__APPLE__)
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wunknown-pragmas"
# pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
#endif // defined(__APPLE__)
/*! @file
 @brief The class definition for vertex buffers for the %Leap Motion display output service. */
#if defined(__APPLE__)
# pragma clang diagnostic pop
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Namespace references
#endif // defined(__APPLE__)

using namespace LeapDisplay;

#if defined(__APPLE__)
# pragma mark Private structures, constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Global constants and variables
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Local functions
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Class methods
#endif // defined(__APPLE__)

#if defined(__APPLE__)
# pragma mark Constructors and Destructors
#endif // defined(__APPLE__)

VertexBuffer::VertexBuffer(OpenGLContext &      context,
                           Array<Vertex> &      vertexList,
                           const juce::uint32 * indices,
                           const GLsizei        numIndices) :
    _numIndices(numIndices), _context(context)
{
    ODL_ENTER(); //####
    ODL_P3("context = ", &_context, "vertexList = ", &vertexList, "indices = ", indices); //####
    ODL_LL1("numIndices = ", numIndices); //####
    _context.extensions.glGenBuffers(1, &_vertexBuffer);
    _context.extensions.glBindBuffer(GL_ARRAY_BUFFER, _vertexBuffer);
    _context.extensions.glBufferData(GL_ARRAY_BUFFER, vertexList.size() * sizeof(Vertex),
                                     vertexList.getRawDataPointer(), GL_STATIC_DRAW);
    _context.extensions.glGenBuffers(1, &_indexBuffer);
    _context.extensions.glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, _indexBuffer);
    _context.extensions.glBufferData(GL_ELEMENT_ARRAY_BUFFER, numIndices * sizeof(*indices),
                                     indices, GL_STATIC_DRAW);
    ODL_EXIT_P(this); //####
} // VertexBuffer::VertexBuffer

VertexBuffer::~VertexBuffer(void)
{
    ODL_OBJENTER(); //####
    _context.extensions.glDeleteBuffers (1, &_vertexBuffer);
    _context.extensions.glDeleteBuffers (1, &_indexBuffer);
    ODL_OBJEXIT(); //####
} // VertexBuffer::~VertexBuffer

#if defined(__APPLE__)
# pragma mark Actions and Accessors
#endif // defined(__APPLE__)

void
VertexBuffer::bind(void)
{
    ODL_OBJENTER(); //####
    _context.extensions.glBindBuffer(GL_ARRAY_BUFFER, _vertexBuffer);
    _context.extensions.glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, _indexBuffer);
    ODL_OBJEXIT(); //####
} // VertexBuffer::bind

#if defined(__APPLE__)
# pragma mark Global functions
#endif // defined(__APPLE__)

