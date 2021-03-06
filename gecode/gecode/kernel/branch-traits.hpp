/* -*- mode: C++; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/*
 *  Main authors:
 *     Christian Schulte <schulte@gecode.org>
*
 *  Copyright:
 *     Christian Schulte, 2012
 *
 *  Last modified:
 *     $Date: 2012-09-07 11:29:57 +0200 (Fri, 07 Sep 2012) $ by $Author: schulte $
 *     $Revision: 13061 $
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

namespace Gecode {

  /**
   * \brief Traits for branching
   *
   * This class collects the traits for branching, depending on the
   * variable type.
   *
   * The traits used concern various functions:
   *     - <code>typedef Type Filter</code>  where \c Type is the type
   *       of an appropriate branch filter function for the variable type.
   *     - <code>typedef Type Merit</code>  where \c Type is the type
   *       of an appropriate branch merit function for the variable type.
   *     - <code>typedef Type Val</code>  where \c Type is the type
   *       of an appropriate branch value function for the variable type.
   *     - <code>typedef Type ValType</code>  where \c Type is the return type
   *       of the branch value function for the variable type.
   *     - <code>typedef Type Commit</code>  where \c Type is the type
   *       of an appropriate branch commit function for the variable type.
   */
  template<class Var>
  class BranchTraits {};

}

// STATISTICS: kernel-branch
