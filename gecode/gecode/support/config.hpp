/* gecode/support/config.hpp.  Generated from config.hpp.in by configure.  */
/*
 *  Main authors:
 *     Guido Tack <tack@gecode.org>
 *
 *  Copyright:
 *     Guido Tack, 2008
 *
 *  Last modified:
 *     $Date: 2012-03-27 15:34:39 +0200 (Tue, 27 Mar 2012) $ by $Author: schulte $
 *     $Revision: 12635 $
 *
 *  This file is part of Gecode, the generic constraint
 *  development environment:
 *     http://www.gecode.org
 *
 *  Permission is hereby granted, free of charge, to any person obtaining
 *  a copy of this software and associated documentation files (the
 *  "Software"), to deal in the Software without restriction, including
 *  without limitation the rights to use, copy, modify, merge, publish,
 *  distribute, sublicense, and/or sell copies of the Software, and to
 *  permit persons to whom the Software is furnished to do so, subject to
 *  the following conditions:
 *
 *  The above copyright notice and this permission notice shall be
 *  included in all copies or substantial portions of the Software.
 *
 *  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 *  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 *  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 *  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 *  LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 *  OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 *  WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 */

/* Whether to include audit code */
/* #undef GECODE_AUDIT */

/* User-defined prefix of dll names */
#define GECODE_DLL_USERPREFIX ""

/* User-defined suffix of dll names */
#define GECODE_DLL_USERSUFFIX ""

/* Supported version of FlatZinc */
#define GECODE_FLATZINC_VERSION "1.6"

/* Whether gcc understands visibility attributes */
#define GECODE_GCC_HAS_CLASS_VISIBILITY /**/

/* whether __builtin_ffsl is available */
#define GECODE_HAS_BUILTIN_FFSL /**/

/* Whether to build FLOAT variables */
/* #undef GECODE_HAS_FLOAT_VARS */

/* Whether Gist is available */
/* #undef GECODE_HAS_GIST */

/* Whether GNU hash_map is available */
#define GECODE_HAS_GNU_HASH_MAP /**/

/* Whether to build INT variables */
#define GECODE_HAS_INT_VARS /**/

/* Whether MPFR is available */
/* #undef GECODE_HAS_MPFR */

/* Whether we have mtrace for memory leak debugging */
/* #undef GECODE_HAS_MTRACE */

/* Whether Qt is available */
/* #undef GECODE_HAS_QT */

/* Whether to build SET variables */
/* #undef GECODE_HAS_SET_VARS */

/* Whether unistd.h is available */
#define GECODE_HAS_UNISTD_H 1

/* Gecode version */
#define GECODE_LIBRARY_VERSION "4-0-0"

/* Heap memory alignment */
/* #undef GECODE_MEMORY_ALIGNMENT */

/* Whether we are compiling static libraries */
#define GECODE_STATIC_LIBS /**/

/* Whether we have posix threads */
#define GECODE_THREADS_PTHREADS 1

/* Whether we have windows threads */
/* #undef GECODE_THREADS_WINDOWS */

/* Use clock() for time-measurement */
/* #undef GECODE_USE_CLOCK */

/* Use gettimeofday for time-measurement */
#define GECODE_USE_GETTIMEOFDAY 1

/* Gecode version */
#define GECODE_VERSION "4.0.0"

/* Gecode version */
#define GECODE_VERSION_NUMBER 400000

/* How to tell the compiler to really, really inline */
#define forceinline inline __attribute__ ((__always_inline__))

// STATISTICS: support-any
