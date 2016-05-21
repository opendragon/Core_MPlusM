//--------------------------------------------------------------------------------------------------
//
//  File:       m+mColumnNameValidator.hpp
//
//  Project:    m+m
//
//  Contains:   The class declaration for the functionality required for an m+m field name matcher
//              for the Registry Service.
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
//  Created:    2014-03-24
//
//--------------------------------------------------------------------------------------------------

#if (! defined(MpMColumnNameValidator_HPP_))
# define MpMColumnNameValidator_HPP_ /* Header guard */

# include <m+m/m+mBaseNameValidator.hpp>

# if defined(__APPLE__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wunknown-pragmas"
#  pragma clang diagnostic ignored "-Wdocumentation-unknown-command"
# endif // defined(__APPLE__)
/*! @file
 @brief The class declaration for the functionality required for an m+m field name matcher for the
 Registry Service. */
# if defined(__APPLE__)
#  pragma clang diagnostic pop
# endif // defined(__APPLE__)

/*! @brief The named parameter for the 'channelname' column. */
# define CHANNELNAME_C_         "channelname"

/*! @brief The named parameter for the 'details' column. */
# define DESCRIPTION_C_         "description"

/*! @brief The named parameter for the 'details' column. */
# define DETAILS_C_             "details"

/*! @brief The named parameter for the 'executable' column. */
# define EXECUTABLE_C_          "executable"

/*! @brief The named parameter for the 'extrainfo' column. */
# define EXTRAINFO_C_           "extrainfo"

/*! @brief The named parameter for the 'input' column. */
# define INPUT_C_               "input"

/*! @brief The named parameter for the 'key' column. */
# define KEY_C_                 "key"

/*! @brief The named parameter for the 'keyword' column. */
# define KEYWORD_C_             "keyword"

/*! @brief The named parameter for the 'keywords_id' column. */
# define KEYWORDS_ID_C_         "keywords_id"

/*! @brief The named parameter for the 'name' column. */
# define NAME_C_                "name"

/*! @brief The named parameter for the 'output' column. */
# define OUTPUT_C_              "output"

/*! @brief The named parameter for the 'request' column. */
# define REQUEST_C_             "request"

/*! @brief The named parameter for the 'requestsdesciption' column. */
# define REQUESTSDESCRIPTION_C_ "requestsdescription"

/*! @brief The named parameter for the 'requests_id' column. */
# define REQUESTS_ID_C_         "requests_id"

/*! @brief The named parameter for the 'tag' column. */
# define TAG_C_                 "tag"

/*! @brief The named parameter for the 'version' column. */
# define VERSION_C_             "version"

/*! @brief The name of the 'Keywords' table. */
# define KEYWORDS_T_            "Keywords"

/*! @brief The name of the 'Requests' table. */
# define REQUESTS_T_            "Requests"

/*! @brief The name of the 'RequestsKeywords' table. */
# define REQUESTSKEYWORDS_T_    "RequestsKeywords"

/*! @brief The name of the 'Services' table. */
# define SERVICES_T_            "Services"

namespace MplusM
{
    namespace Registry
    {
        /*! @brief A convenience class to provide function objects for field name handling. */
        class ColumnNameValidator : public MplusM::Parser::BaseNameValidator
        {
        public :

        protected :

        private :

            /*! @brief The class that this class is derived from. */
            typedef BaseNameValidator inherited;

        public :

            /*! @brief The constructor. */
            ColumnNameValidator(void);

            /*! @brief The destructor. */
            virtual
            ~ColumnNameValidator(void);

        protected :

        private :

            /*! @brief The copy constructor.
             @param[in] other The object to be copied. */
            ColumnNameValidator(const ColumnNameValidator & other);

            /*! @brief Check a field name for validity.
             @param[in] aString The string to be checked.
             @returns @c true if the field name was valid or @c false if the field name was
             invalid. */
            virtual bool
            checkName(const char * aString);

            /*! @brief Get the 'true name' matching the name and its prefix and suffix strings.
             @param[in] aString The string to be checked.
             @param[in,out] prefixString The string to be used in the SQL prefix for this field.
             @param[in,out] suffixString The string to be used in the SQL suffix for this field.
             @returns The actual field name to be used or @c NULL if the field name was
             unmatched. */
            virtual const char *
            getPrefixAndSuffix(const char *   aString,
                               const char * & prefixString,
                               const char * & suffixString);

            /*! @brief The assignment operator.
             @param[in] other The object to be copied.
             @returns The updated object. */
            ColumnNameValidator &
            operator =(const ColumnNameValidator & other);

        public :

        protected :

        private :

        }; // ColumnNameValidator

    } // Registry

} // MplusM

#endif // ! defined(MpMColumnNameValidator_HPP_)
