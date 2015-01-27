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

import org.slf4j.LoggerFactory

/**
 * Simple logging trait to mix in. A replacement over typesafe's that just gives you a plain slf4j logger wrapper and moves on
 * @author Marc-André Laverdière-Papineau
 */
trait Logging {

  /**
   * The logger object.
   */
  lazy protected val logger : LazyLogger = new LazyLogger(LoggerFactory.getLogger(loggerName()))

  /**
   * The name of the logger. Override this method if you are having a trait that gets
   * a weird name auto-generated.
   * @return the name for the logger.
   */
  def loggerName() : String = getClass.getName

}
