//
//  YPPRequests.h
//  YarpPlusPlus
//
//  Created by Norman Jaffe on 2014-02-25.
//  Copyright (c) 2014 OpenDragon. All rights reserved.
//

#if (! defined(YPPREQUESTS_H_))
# define YPPREQUESTS_H_ /* */

/*! @brief The standard name for a 'list' request. */
# define YPP_LIST_REQUEST     "list"
/*! @brief The standard name for a 'register' request. */
# define YPP_REGISTER_REQUEST "register"

/*! @brief Request/response specification character - integer value. */
# define YPP_REQREP_INT        "i"
/*! @brief Request/response specification character - double value. */
# define YPP_REQREP_DOUBLE     "d"
/*! @brief Request/response specification character - string value. */
# define YPP_REQREP_STRING     "s"
/*! @brief Request/response specification character - start of list specification. */
# define YPP_REQREP_LIST_START "("
/*! @brief Request/response specification character - end of list specification. */
# define YPP_REQREP_LIST_END   ")"
/*! @brief Request/response specification character - start of dictionary specification. */
# define YPP_REQREP_DICT_START "["
/*! @brief Request/response specification character - end of dictionary specification. */
# define YPP_REQREP_DICT_END   "]"
/*! @brief Request/response specification character - key/value separator for dictionary. */
# define YPP_REQREP_DICT_SEP   ":"
/*! @brief Request/response specification character - one or more repetitions of preceding. */
# define YPP_REQREP_1_OR_MORE  "+"
/*! @brief Request/response specification character - zero or more repetitions of preceding. */
# define YPP_REQREP_0_OR_MORE  "*"
/*! @brief Request/response specification character - any element. */
# define YPP_REQREP_ANYTHING   "."

/*! @brief Request/response dictionary key - input specification. */
# define YPP_REQREP_DICT_INPUT_KEY  "input"
/*! @brief Request/response dictionary key - name specification. */
# define YPP_REQREP_DICT_NAME_KEY   "name"
/*! @brief Request/response dictionary key - output specification. */
# define YPP_REQREP_DICT_OUTPUT_KEY "output"

#endif // ! defined(YPPREQUESTS_H_)
