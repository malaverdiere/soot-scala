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
package soot.util

import soot.tagkit._
import soot.util.ScalaWrappers._
import soot.{SootClass, Unit => SootUnit}

/**
 * Utilities for dealing with Soot annotations
 *
 * @author Marc-André Laverdière-Papineau
 */
object SootAnnotationUtils {

  /**
   * Checks if the Soot annotation is present in the Soot body (SootMethod, SootClass...)
   * @param body the Soot body
   * @param clazz the Soot annotation type to find.
   * @return An Option for that annotation
   */
  def findAnnotation[A <: Tag](body: Host, clazz: Class[A]): Option[A] = body.tagOpt(clazz)


  /**
   * Checks if the annotation is present in the Soot body (SootMethod, SootClass...)
   * @param body the Soot body
   * @param annotation the annotation in Soot representation, not Java human-friendly form.
   * @return `true` if the annotation is found
   */
  def hasJavaAnnotation(body: Host, annotation: String): Boolean = {
    findJavaAnnotation(body, annotation).isDefined
  }

  /**
   * Checks if the annotation is present in the Soot body (SootMethod, SootClass...)
   * @param body the Soot body
   * @param annotations the annotation in Soot representation, not Java human-friendly form.
   * @return <code>True if the annotation is found</code>
   */
  def hasJavaAnnotation(body: Host, annotations: TraversableOnce[String]): Boolean = {
    val vat: Option[VisibilityAnnotationTag] = findAnnotation(body, classOf[VisibilityAnnotationTag])
    val javaAnnotationTypes: Set[String] = vat.toList.flatMap(_.annotations).map(_.getType).toSet
    annotations.exists(javaAnnotationTypes.contains)
  }

  /**
   * Checks if the Java annotation is present in the Soot body (SootMethod, SootClass...)
   * @param body the Soot body
   * @param annotation the annotation in Soot representation, not Java human-friendly form.
   * @return <code>True if the annotation is found</code>
   */
  def findJavaAnnotation(body: Host, annotation: String): Option[AnnotationTag] = {
    val vat = findAnnotation(body, classOf[VisibilityAnnotationTag])
    vat.flatMap(_.annotations.find(_.getType == annotation))
  }

  /**
   * Gets the elements of the given annotation and extracts their value based on their type.
   * As the type is not returned, the client is expected to know what to do with them based on their names.
   * @param ann the annotation
   * @return a map. The key is the name of the annotation.
   */
  def annotationElements(ann: AnnotationTag): Map[String, Any] = {
    ann.elements.collect {
      case elem: AnnotationStringElem => (elem.name, elem.getValue)
      case elem: AnnotationIntElem => (elem.name, elem.getValue)
      case elem: AnnotationBooleanElem => (elem.name, elem.getValue)
      case elem: AnnotationDoubleElem => (elem.name, elem.getValue)
      case elem: AnnotationFloatElem => (elem.name, elem.getValue)
      case elem: AnnotationLongElem => (elem.name, elem.getValue)
      case elem: AnnotationArrayElem => (elem.name, elem.getValues)
      case elem: AnnotationClassElem => (elem.name, elem.getDesc)
      case elem: AnnotationEnumElem => (elem.name, elem.getConstantName) //Do we need the type too?
      case elem: AnnotationAnnotationElem => (elem.name, elem.getValue)
    }.seq.toMap
  }

  /**
   * Utility method when one doesn't want the Soot annotation object, but only the elements it contains.
   * Its return value is identical to annotationElements
   * @param body the annotated
   * @param annotation the name of the annotation to lookup
   * @return an empty map if the annotation doesn't exist, otherwise it is identical as
   */
  def elementsForJavaAnnotation(body: Host, annotation: String): Map[String, Any] = {
    findJavaAnnotation(body, annotation).map(annotationElements(_)).getOrElse(Map())
  }

  /**
   * Converts from the human-friendly Java form (javax.blah) to
   * the Soot from (Ljavax/blah;)
   * @param javaForm the annotation name in Java form
   * @return the converted form
   */
  def javaToSootForm(javaForm: String): String = {
    if (javaForm == null) throw new NullPointerException("Null annotation")
    if (javaForm.isEmpty) throw new IllegalArgumentException("Empty annotation name")
    if (javaForm.startsWith("L") && javaForm.endsWith(";")) throw new IllegalArgumentException("Annotation already in Soot form")
    "L" + javaForm.replace('.', '/') + ';'
  }


  /**
   * Get the file where the given class is defined, provided that the information is available in the bytecode.
   * @param sc the class
   * @return an Option to the absolute file path
   */
  def sourceFile(sc: SootClass): Option[String] = findAnnotation(sc, classOf[SourceFileTag]).map(_.getAbsolutePath)

}
