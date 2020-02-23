//--------------------------------------------------------------------------------------------------
//
//  File:       m+mVertexBuffer.hpp
//
//  Project:    m+m
//
//  Contains:   The class declaration for vertex buffers for the platonic display output service.
//
//  Written by: Norman Jaffe
//
//  Copyright:  (c) 2016 by OpenDragon.
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

#if (! defined(MpMVertexBuffer_HPP_))
# define MpMVertexBuffer_HPP_ /* Header guard */

# include "m+mCommonVisuals.hpp"

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 @brief The class declaration for vertex buffers for the platonic display output service */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace CommonVisuals
{
    
    /*! @brief A convenience class to hold vertices and their indices. */
    class VertexBuffer
    {
    public :
    
    protected :
    
    private :
        
        /*! @brief The class that this class is derived from. */
        typedef Thread inherited;
        
    public :
        
        /*! @brief The constructor.
         @param[in] context The OpenGL context to be used.
         @param[in] vertexList The vertex data to be loaded into the buffer.
         @param[in] indices The vertex indices.
         @param[in] numIndices The number of indices. */
        VertexBuffer(OpenGLContext &      context,
                     Array<Vertex> &      vertexList,
                     const juce::uint32 * indices,
                     const GLsizei        numIndices);
        
        /*! @brief The destructor. */
        virtual
        ~VertexBuffer(void);

        /*! @brief Bind the vertex buffers to the context. */        
        void
        bind(void);

        /*! @brief Return the number of indices.
         @return The number of indices. */
        GLsizei
        numberOfIndices(void)
        const
        {
            return _numIndices;
        } // numberOfIndices
        
    protected :
    
    private :
        
    public :
    
    protected :
    
    private :
        
        /*! @brief The OpenGL index for the vertices. */       
        GLuint _vertexBuffer;

        /*! @brief The OpenGL index for the vertex indices. */
        GLuint _indexBuffer;

        /*! @brief The number of indices. */
        GLsizei _numIndices;

        /*! @brief The OpenGL context that is being used. */
        OpenGLContext & _context;

        JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR(VertexBuffer)

    }; // VertexBuffer
    
} // CommonVisuals

#endif // ! defined(MpMVertexBuffer_HPP_)

//OwnedArray<VertexBuffer> vertexBuffers;
