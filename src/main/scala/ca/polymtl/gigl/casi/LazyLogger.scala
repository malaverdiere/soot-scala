/*
    This file is part of soot-scala.

    Soot-scala is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Soot-scala is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with soot-scala.  If not, see <http://www.gnu.org/licenses/>.

  Copyright 2015 École Polytechnique de Montréal & Tata Consultancy Services.
 */

package ca.polymtl.gigl.casi

import org.slf4j.Logger



/**
 * Very lazy logger. All parameters are passed lazily and will be evaluated only if the log
 * level is evaluated.
 * All the methods with many args are specified this way because Scala doesn't allow lazy vargargs
 * @param underlying the underlying SLF4J logger
 * @author Marc-André Laverdière-Papineau
 */
class LazyLogger(underlying : Logger) {

  def error(msg : String) = underlying.error(msg)
  def error(msg : String, t : Throwable) = underlying.error(msg, t)
  def error(msg : String, arg1 : => Any) = if (underlying.isErrorEnabled()) underlying.error(msg, toStringVarargs(arg1) :_*)
  def error(msg : String, arg1 : => Any, arg2 : => Any) = if (underlying.isErrorEnabled()) underlying.error(msg, toStringVarargs(arg1.toString, arg2.toString) :_*)
  def error(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any) = if (underlying.isErrorEnabled()) underlying.error(msg, toStringVarargs(arg1, arg2, arg3) :_*)
  def error(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any, arg4 : => Any) = if (underlying.isErrorEnabled()) underlying.error(msg, toStringVarargs(arg1, arg2, arg3, arg4) :_*)
  def error(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any, arg4 : => Any, arg5 : => Any) = if (underlying.isErrorEnabled()) underlying.error(msg, toStringVarargs(arg1, arg2, arg3, arg4, arg5) :_*)
  def error(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any, arg4 : => Any, arg5 : => Any, arg6 : => Any) = if (underlying.isErrorEnabled()) underlying.error(msg, toStringVarargs(arg1, arg2, arg3, arg4, arg5, arg6) :_*)
  def error(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any, arg4 : => Any, arg5 : => Any, arg6 : => Any, arg7 : => Any) = if (underlying.isErrorEnabled()) underlying.error(msg, toStringVarargs(arg1, arg2, arg3, arg4, arg5, arg6, arg7) :_*)
  def error(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any, arg4 : => Any, arg5 : => Any, arg6 : => Any, arg7 : => Any, arg8 : => Any) = if (underlying.isErrorEnabled()) underlying.error(msg, toStringVarargs(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8) :_*)
  def error(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any, arg4 : => Any, arg5 : => Any, arg6 : => Any, arg7 : => Any, arg8 : => Any, arg9 : => Any) = if (underlying.isErrorEnabled()) underlying.error(msg, toStringVarargs(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9) :_*)
  def error(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any, arg4 : => Any, arg5 : => Any, arg6 : => Any, arg7 : => Any, arg8 : => Any, arg9 : => Any, arg10 : => Any) = if (underlying.isErrorEnabled()) underlying.error(msg, toStringVarargs(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10) :_*)
  def error(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any, arg4 : => Any, arg5 : => Any, arg6 : => Any, arg7 : => Any, arg8 : => Any, arg9 : => Any, arg10 : => Any, arg11 : => Any) = if (underlying.isErrorEnabled()) underlying.error(msg, toStringVarargs(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11) :_*)
  def error(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any, arg4 : => Any, arg5 : => Any, arg6 : => Any, arg7 : => Any, arg8 : => Any, arg9 : => Any, arg10 : => Any, arg11 : => Any, arg12 : => Any) = if (underlying.isErrorEnabled()) underlying.error(msg, toStringVarargs(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12) :_*)
  def error(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any, arg4 : => Any, arg5 : => Any, arg6 : => Any, arg7 : => Any, arg8 : => Any, arg9 : => Any, arg10 : => Any, arg11 : => Any, arg12 : => Any, arg13 : => Any) = if (underlying.isErrorEnabled()) underlying.error(msg, toStringVarargs(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13) :_*)
  def error(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any, arg4 : => Any, arg5 : => Any, arg6 : => Any, arg7 : => Any, arg8 : => Any, arg9 : => Any, arg10 : => Any, arg11 : => Any, arg12 : => Any, arg13 : => Any, arg14 : => Any) = if (underlying.isErrorEnabled()) underlying.error(msg, toStringVarargs(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14) :_*)
  def error(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any, arg4 : => Any, arg5 : => Any, arg6 : => Any, arg7 : => Any, arg8 : => Any, arg9 : => Any, arg10 : => Any, arg11 : => Any, arg12 : => Any, arg13 : => Any, arg14 : => Any, arg15 : => Any) = if (underlying.isErrorEnabled()) underlying.error(msg, toStringVarargs(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15) :_*)

  def warn(msg : String) = underlying.warn(msg)
  def warn(msg : String, t : Throwable) = underlying.warn(msg, t)
  def warn(msg : String, arg1 : => Any) = if (underlying.isWarnEnabled()) underlying.warn(msg, toStringVarargs(arg1) :_*)
  def warn(msg : String, arg1 : => Any, arg2 : => Any) = if (underlying.isWarnEnabled()) underlying.warn(msg, toStringVarargs(arg1, arg2) :_*)
  def warn(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any) = if (underlying.isWarnEnabled()) underlying.warn(msg, toStringVarargs(arg1, arg2, arg3) :_*)
  def warn(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any, arg4 : => Any) = if (underlying.isWarnEnabled()) underlying.warn(msg, toStringVarargs(arg1, arg2, arg3, arg4) :_*)
  def warn(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any, arg4 : => Any, arg5 : => Any) = if (underlying.isWarnEnabled()) underlying.warn(msg, toStringVarargs(arg1, arg2, arg3, arg4, arg5) :_*)
  def warn(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any, arg4 : => Any, arg5 : => Any, arg6 : => Any) = if (underlying.isWarnEnabled()) underlying.warn(msg, toStringVarargs(arg1, arg2, arg3, arg4, arg5, arg6) :_*)
  def warn(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any, arg4 : => Any, arg5 : => Any, arg6 : => Any, arg7 : => Any) = if (underlying.isWarnEnabled()) underlying.warn(msg, toStringVarargs(arg1, arg2, arg3, arg4, arg5, arg6, arg7) :_*)
  def warn(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any, arg4 : => Any, arg5 : => Any, arg6 : => Any, arg7 : => Any, arg8 : => Any) = if (underlying.isWarnEnabled()) underlying.warn(msg, toStringVarargs(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8) :_*)
  def warn(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any, arg4 : => Any, arg5 : => Any, arg6 : => Any, arg7 : => Any, arg8 : => Any, arg9 : => Any) = if (underlying.isWarnEnabled()) underlying.warn(msg, toStringVarargs(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9) :_*)
  def warn(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any, arg4 : => Any, arg5 : => Any, arg6 : => Any, arg7 : => Any, arg8 : => Any, arg9 : => Any, arg10 : => Any) = if (underlying.isWarnEnabled()) underlying.warn(msg, toStringVarargs(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10) :_*)
  def warn(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any, arg4 : => Any, arg5 : => Any, arg6 : => Any, arg7 : => Any, arg8 : => Any, arg9 : => Any, arg10 : => Any, arg11 : => Any) = if (underlying.isWarnEnabled()) underlying.warn(msg, toStringVarargs(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11) :_*)
  def warn(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any, arg4 : => Any, arg5 : => Any, arg6 : => Any, arg7 : => Any, arg8 : => Any, arg9 : => Any, arg10 : => Any, arg11 : => Any, arg12 : => Any) = if (underlying.isWarnEnabled()) underlying.warn(msg, toStringVarargs(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12) :_*)
  def warn(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any, arg4 : => Any, arg5 : => Any, arg6 : => Any, arg7 : => Any, arg8 : => Any, arg9 : => Any, arg10 : => Any, arg11 : => Any, arg12 : => Any, arg13 : => Any) = if (underlying.isWarnEnabled()) underlying.warn(msg, toStringVarargs(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13) :_*)
  def warn(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any, arg4 : => Any, arg5 : => Any, arg6 : => Any, arg7 : => Any, arg8 : => Any, arg9 : => Any, arg10 : => Any, arg11 : => Any, arg12 : => Any, arg13 : => Any, arg14 : => Any) = if (underlying.isWarnEnabled()) underlying.warn(msg, toStringVarargs(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14) :_*)
  def warn(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any, arg4 : => Any, arg5 : => Any, arg6 : => Any, arg7 : => Any, arg8 : => Any, arg9 : => Any, arg10 : => Any, arg11 : => Any, arg12 : => Any, arg13 : => Any, arg14 : => Any, arg15 : => Any) = if (underlying.isWarnEnabled()) underlying.warn(msg, toStringVarargs(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15) :_*)

  def info(msg : String) = underlying.info(msg)
  def info(msg : String, t : Throwable) = underlying.info(msg, t)
  def info(msg : String, arg1 : => Any) = if (underlying.isInfoEnabled()) underlying.info(msg, toStringVarargs(arg1) :_*)
  def info(msg : String, arg1 : => Any, arg2 : => Any) = if (underlying.isInfoEnabled()) underlying.info(msg, toStringVarargs(arg1, arg2) :_*)
  def info(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any) = if (underlying.isInfoEnabled()) underlying.info(msg, toStringVarargs(arg1, arg2, arg3) :_*)
  def info(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any, arg4 : => Any) = if (underlying.isInfoEnabled()) underlying.info(msg, toStringVarargs(arg1, arg2, arg3, arg4) :_*)
  def info(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any, arg4 : => Any, arg5 : => Any) = if (underlying.isInfoEnabled()) underlying.info(msg, toStringVarargs(arg1, arg2, arg3, arg4, arg5) :_*)
  def info(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any, arg4 : => Any, arg5 : => Any, arg6 : => Any) = if (underlying.isInfoEnabled()) underlying.info(msg, toStringVarargs(arg1, arg2, arg3, arg4, arg5, arg6) :_*)
  def info(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any, arg4 : => Any, arg5 : => Any, arg6 : => Any, arg7 : => Any) = if (underlying.isInfoEnabled()) underlying.info(msg, toStringVarargs(arg1, arg2, arg3, arg4, arg5, arg6, arg7) :_*)
  def info(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any, arg4 : => Any, arg5 : => Any, arg6 : => Any, arg7 : => Any, arg8 : => Any) = if (underlying.isInfoEnabled()) underlying.info(msg, toStringVarargs(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8) :_*)
  def info(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any, arg4 : => Any, arg5 : => Any, arg6 : => Any, arg7 : => Any, arg8 : => Any, arg9 : => Any) = if (underlying.isInfoEnabled()) underlying.info(msg, toStringVarargs(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9) :_*)
  def info(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any, arg4 : => Any, arg5 : => Any, arg6 : => Any, arg7 : => Any, arg8 : => Any, arg9 : => Any, arg10 : => Any) = if (underlying.isInfoEnabled()) underlying.info(msg, toStringVarargs(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10) :_*)
  def info(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any, arg4 : => Any, arg5 : => Any, arg6 : => Any, arg7 : => Any, arg8 : => Any, arg9 : => Any, arg10 : => Any, arg11 : => Any) = if (underlying.isInfoEnabled()) underlying.info(msg, toStringVarargs(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11) :_*)
  def info(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any, arg4 : => Any, arg5 : => Any, arg6 : => Any, arg7 : => Any, arg8 : => Any, arg9 : => Any, arg10 : => Any, arg11 : => Any, arg12 : => Any) = if (underlying.isInfoEnabled()) underlying.info(msg, toStringVarargs(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12) :_*)
  def info(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any, arg4 : => Any, arg5 : => Any, arg6 : => Any, arg7 : => Any, arg8 : => Any, arg9 : => Any, arg10 : => Any, arg11 : => Any, arg12 : => Any, arg13 : => Any) = if (underlying.isInfoEnabled()) underlying.info(msg, toStringVarargs(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13) :_*)
  def info(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any, arg4 : => Any, arg5 : => Any, arg6 : => Any, arg7 : => Any, arg8 : => Any, arg9 : => Any, arg10 : => Any, arg11 : => Any, arg12 : => Any, arg13 : => Any, arg14 : => Any) = if (underlying.isInfoEnabled()) underlying.info(msg, toStringVarargs(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14) :_*)
  def info(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any, arg4 : => Any, arg5 : => Any, arg6 : => Any, arg7 : => Any, arg8 : => Any, arg9 : => Any, arg10 : => Any, arg11 : => Any, arg12 : => Any, arg13 : => Any, arg14 : => Any, arg15 : => Any) = if (underlying.isInfoEnabled()) underlying.info(msg, toStringVarargs(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15) :_*)


  def debug(msg : String) = underlying.debug(msg)
  def debug(msg : String, t : Throwable) = underlying.debug(msg, t)
  def debug(msg : String, arg1 : => Any) = if (underlying.isDebugEnabled()) underlying.debug(msg, toStringVarargs(arg1) :_*)
  def debug(msg : String, arg1 : => Any, arg2 : => Any) = if (underlying.isDebugEnabled()) underlying.debug(msg, toStringVarargs(arg1, arg2) :_*)
  def debug(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any) = if (underlying.isDebugEnabled()) underlying.debug(msg, toStringVarargs(arg1, arg2, arg3) :_*)
  def debug(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any, arg4 : => Any) = if (underlying.isDebugEnabled()) underlying.debug(msg, toStringVarargs(arg1, arg2, arg3, arg4) :_*)
  def debug(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any, arg4 : => Any, arg5 : => Any) = if (underlying.isDebugEnabled()) underlying.debug(msg, toStringVarargs(arg1, arg2, arg3, arg4, arg5) :_*)
  def debug(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any, arg4 : => Any, arg5 : => Any, arg6 : => Any) = if (underlying.isDebugEnabled()) underlying.debug(msg, toStringVarargs(arg1, arg2, arg3, arg4, arg5, arg6) :_*)
  def debug(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any, arg4 : => Any, arg5 : => Any, arg6 : => Any, arg7 : => Any) = if (underlying.isDebugEnabled()) underlying.debug(msg, toStringVarargs(arg1, arg2, arg3, arg4, arg5, arg6, arg7) :_*)
  def debug(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any, arg4 : => Any, arg5 : => Any, arg6 : => Any, arg7 : => Any, arg8 : => Any) = if (underlying.isDebugEnabled()) underlying.debug(msg, toStringVarargs(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8) :_*)
  def debug(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any, arg4 : => Any, arg5 : => Any, arg6 : => Any, arg7 : => Any, arg8 : => Any, arg9 : => Any) = if (underlying.isDebugEnabled()) underlying.debug(msg, toStringVarargs(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9) :_*)
  def debug(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any, arg4 : => Any, arg5 : => Any, arg6 : => Any, arg7 : => Any, arg8 : => Any, arg9 : => Any, arg10 : => Any) = if (underlying.isDebugEnabled()) underlying.debug(msg, toStringVarargs(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10) :_*)
  def debug(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any, arg4 : => Any, arg5 : => Any, arg6 : => Any, arg7 : => Any, arg8 : => Any, arg9 : => Any, arg10 : => Any, arg11 : => Any) = if (underlying.isDebugEnabled()) underlying.debug(msg, toStringVarargs(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11) :_*)
  def debug(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any, arg4 : => Any, arg5 : => Any, arg6 : => Any, arg7 : => Any, arg8 : => Any, arg9 : => Any, arg10 : => Any, arg11 : => Any, arg12 : => Any) = if (underlying.isDebugEnabled()) underlying.debug(msg, toStringVarargs(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12) :_*)
  def debug(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any, arg4 : => Any, arg5 : => Any, arg6 : => Any, arg7 : => Any, arg8 : => Any, arg9 : => Any, arg10 : => Any, arg11 : => Any, arg12 : => Any, arg13 : => Any) = if (underlying.isDebugEnabled()) underlying.debug(msg, toStringVarargs(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13) :_*)
  def debug(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any, arg4 : => Any, arg5 : => Any, arg6 : => Any, arg7 : => Any, arg8 : => Any, arg9 : => Any, arg10 : => Any, arg11 : => Any, arg12 : => Any, arg13 : => Any, arg14 : => Any) = if (underlying.isDebugEnabled()) underlying.debug(msg, toStringVarargs(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14) :_*)
  def debug(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any, arg4 : => Any, arg5 : => Any, arg6 : => Any, arg7 : => Any, arg8 : => Any, arg9 : => Any, arg10 : => Any, arg11 : => Any, arg12 : => Any, arg13 : => Any, arg14 : => Any, arg15 : => Any) = if (underlying.isDebugEnabled()) underlying.debug(msg, toStringVarargs(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15) :_*)


  def trace(msg : String) = underlying.trace(msg)
  def trace(msg : String, t : Throwable) = underlying.trace(msg, t)
  def trace(msg : String, arg1 : => Any) = if (underlying.isTraceEnabled()) underlying.trace(msg, toStringVarargs(arg1) :_*)
  def trace(msg : String, arg1 : => Any, arg2 : => Any) = if (underlying.isTraceEnabled()) underlying.trace(msg, toStringVarargs(arg1, arg2) :_*)
  def trace(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any) = if (underlying.isTraceEnabled()) underlying.trace(msg, toStringVarargs(arg1, arg2, arg3) :_*)
  def trace(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any, arg4 : => Any) = if (underlying.isTraceEnabled()) underlying.trace(msg, toStringVarargs(arg1, arg2, arg3, arg4) :_*)
  def trace(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any, arg4 : => Any, arg5 : => Any) = if (underlying.isTraceEnabled()) underlying.trace(msg, toStringVarargs(arg1, arg2, arg3, arg4, arg5) :_*)
  def trace(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any, arg4 : => Any, arg5 : => Any, arg6 : => Any) = if (underlying.isTraceEnabled()) underlying.trace(msg, toStringVarargs(arg1, arg2, arg3, arg4, arg5, arg6) :_*)
  def trace(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any, arg4 : => Any, arg5 : => Any, arg6 : => Any, arg7 : => Any) = if (underlying.isTraceEnabled()) underlying.trace(msg, toStringVarargs(arg1, arg2, arg3, arg4, arg5, arg6, arg7) :_*)
  def trace(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any, arg4 : => Any, arg5 : => Any, arg6 : => Any, arg7 : => Any, arg8 : => Any) = if (underlying.isTraceEnabled()) underlying.trace(msg, toStringVarargs(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8) :_*)
  def trace(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any, arg4 : => Any, arg5 : => Any, arg6 : => Any, arg7 : => Any, arg8 : => Any, arg9 : => Any) = if (underlying.isTraceEnabled()) underlying.trace(msg, toStringVarargs(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9) :_*)
  def trace(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any, arg4 : => Any, arg5 : => Any, arg6 : => Any, arg7 : => Any, arg8 : => Any, arg9 : => Any, arg10 : => Any) = if (underlying.isTraceEnabled()) underlying.trace(msg, toStringVarargs(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10) :_*)
  def trace(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any, arg4 : => Any, arg5 : => Any, arg6 : => Any, arg7 : => Any, arg8 : => Any, arg9 : => Any, arg10 : => Any, arg11 : => Any) = if (underlying.isTraceEnabled()) underlying.trace(msg, toStringVarargs(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11) :_*)
  def trace(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any, arg4 : => Any, arg5 : => Any, arg6 : => Any, arg7 : => Any, arg8 : => Any, arg9 : => Any, arg10 : => Any, arg11 : => Any, arg12 : => Any) = if (underlying.isTraceEnabled()) underlying.trace(msg, toStringVarargs(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12) :_*)
  def trace(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any, arg4 : => Any, arg5 : => Any, arg6 : => Any, arg7 : => Any, arg8 : => Any, arg9 : => Any, arg10 : => Any, arg11 : => Any, arg12 : => Any, arg13 : => Any) = if (underlying.isTraceEnabled()) underlying.trace(msg, toStringVarargs(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13) :_*)
  def trace(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any, arg4 : => Any, arg5 : => Any, arg6 : => Any, arg7 : => Any, arg8 : => Any, arg9 : => Any, arg10 : => Any, arg11 : => Any, arg12 : => Any, arg13 : => Any, arg14 : => Any) = if (underlying.isTraceEnabled()) underlying.trace(msg, toStringVarargs(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14) :_*)
  def trace(msg : String, arg1 : => Any, arg2 : => Any, arg3 : => Any, arg4 : => Any, arg5 : => Any, arg6 : => Any, arg7 : => Any, arg8 : => Any, arg9 : => Any, arg10 : => Any, arg11 : => Any, arg12 : => Any, arg13 : => Any, arg14 : => Any, arg15 : => Any) = if (underlying.isTraceEnabled()) underlying.trace(msg, toStringVarargs(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15) :_*)


  /**
   * SLF4J won't digest an array of Any well, it will treat it as one argument to print.
   * So we call the .toString on all the Any arguments. Any nulls are converted to a string "null".
   * @param args the arguments
   * @return a sequence of strings that matches the given arguments
   */
  @inline
  private def toStringVarargs(args : Any*) : Seq[String] = args.collect {
    case null => "null"
    case anything => anything.toString
  }
}
