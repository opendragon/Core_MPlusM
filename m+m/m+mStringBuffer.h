//--------------------------------------------------------------------------------------------------
//
//  File:       m+m/m+mStringBuffer.h
//
//  Project:    m+m
//
//  Contains:   The class declaration for a string buffer.
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
//  Created:    2015-07-23
//
//--------------------------------------------------------------------------------------------------

#if (! defined(MpMStringBuffer_H_))
# define MpMStringBuffer_H_ /* Header guard */

# include <m+m/m+mCommon.h>

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 @brief The class declaration for a string buffer. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

namespace MplusM
{
    namespace Common
    {
        /*! @brief The data constituting a string buffer. */
        class StringBuffer
        {
        public :

        protected :

        private :

        public :

            /*! @brief The constructor. */
            StringBuffer(void);

            /*! @brief The destructor. */
            virtual
            ~StringBuffer(void);

            /*! @brief Add a character to the buffer.
             @param aChar The character to add.
             @returns The StringBuffer object so that cascading can be done. */
            StringBuffer &
            addChar(const char aChar);

            /*! @brief Add a character string representation of a floating-point value to the
             buffer.
             @param aDouble The value to add.
             @returns The StringBuffer object so that cascading can be done. */
            StringBuffer &
            addDouble(const double aDouble);

            /*! @brief Add a character string representation of an integer value to the buffer.
             @param aLong The value to add.
             @returns The StringBuffer object so that cascading can be done. */
            StringBuffer &
            addLong(const int64_t aLong);

            /*! @brief Add a character string to the buffer.
             @param aString The value to add.
             @returns The StringBuffer object so that cascading can be done. */
            StringBuffer &
            addString(const char * aString);

            /*! @brief Add a character string to the buffer.
             @param aString The value to add.
             @returns The StringBuffer object so that cascading can be done. */
            StringBuffer &
            addString(const YarpString & aString);

            /*! @brief Add a horizontal tab character to the buffer.
             @returns The StringBuffer object so that cascading can be done. */
            StringBuffer &
            addTab(void);

            /*! @brief Return a pointer to the characters in the buffer as well as the number of
             valid characters present.
             @param length Set to the number of valid characters in the buffer.
             @returns A pointer to the characters in the buffer. */
            inline const char *
            getString(size_t & length)
            const
            {
                length = _currentLength;
                return _buffer;
            } // getString

            /*! @brief Return the number of valid charaacters in the buffer.
             @returns The number of valid characters in the buffer. */
            inline size_t
            length(void)
            const
            {
                return _currentLength;
            } // length

             /*! @brief Prepare the buffer for reuse.
             @returns The StringBuffer object so that cascading can be done. */
             StringBuffer & reset(void);

        protected :

        private :

            /*! @brief The copy constructor.
             @param other The object to be copied. */
            StringBuffer(const StringBuffer & other);
            
            /*! @brief The assignment operator.
             @param other The object to be copied.
             @returns The updated object. */
            StringBuffer &
            operator =(const StringBuffer & other);

            /*! @brief Increase the size of the internal buffer, copying the current contents into
             the new buffer.
             @param newSize The size for the new internal buffer. */
            void
            setSize(const size_t newSize);
            
        public :

        protected :

        private :

            /*! @brief The internal buffer used to hold the assembled text. */
            char * _buffer;

            /*! @brief The position of the last valid character in the internal buffer. */
            size_t _currentLength;

            /*! @brief The current size of the buffer. */
            size_t _currentSize;

            /*! @brief The length at which the buffer should be resized. */
            size_t _thresholdLength;

        }; // StringBuffer

    } // Common

} // MplusM

#endif // ! defined(MpMStringBuffer_H_)
