/*
 * =============================================================================
 *  Copyright(c) 2011-2013 Organic Motion, Inc. All Rights Reserved.
 *
 *  The coded instructions, statements, computer programs, and/or related
 *  material (collectively the "Code") in these files contain unpublished
 *  information proprietary to Organic Motion, Inc., which is protected by
 *  United States of America federal copyright law and by international
 *  treaties. Use, duplication, and or distribution only with written
 *  permission from Organic Motion, Inc.
 *
 *  THE CODE IS PROVIDED "AS IS" AND WITHOUT WARRANTY.
 * =============================================================================
 */
#ifndef OM_SDK2_DECLSPEC_H
#define OM_SDK2_DECLSPEC_H

#if defined(WIN32) || defined(_WIN32)
#  ifdef OMSDK2DLLEXPORT
#    define OMSDK2DECLSPEC __declspec(dllexport)
#  else
#    define OMSDK2DECLSPEC __declspec(dllimport)
#  endif
#else
#  define OMSDK2DECLSPEC
#endif

#endif //OM_SDK2_DECLSPEC_H
