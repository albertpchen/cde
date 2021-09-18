package cde

import scala.annotation.implicitNotFound
import scala.collection.mutable
import scala.language.dynamics

/** A value signifying a context where [[Up]] and [[Site]] lookups my happen
  */
@implicitNotFound("Could not find CdeUpdateContext make sure you are using CdeUpdate or :+=")
trait CdeUpdateContext derives CanEqual:
  private[cde] def getUpdateContextForId(id: CdeId)(using CdeSource): CdeUpdateContext

  /** The name of the field currently being elaborated
    */
  private[cde] def currentName: String

  /** Looks up the field in the parent [[Cde]]
    */
  private[cde] def up[T: Tag](name: String, subFields: Seq[String])(using CdeSource): T

  /** Looks up the field in the top-level [[Cde]]
    */
  private[cde] def site[T: Tag](name: String, subFields: Seq[String])(using CdeSource): T


/** Methods for performing "up" or "super" [[Cde]] lookups
  */
object Up extends Dynamic:
  /** Looks up the current field being elaborated in the parent [[Cde]]
    */
  def apply[T: Tag]()(using CdeUpdateContext, CdeSource): T =
    selectDynamic(summon[CdeUpdateContext].currentName)

  /** Looks up a field in the parent [[Cde]] by a string parameter
    */
  def apply[T: Tag](name: String)(using CdeUpdateContext, CdeSource): T =
    selectDynamic(name)

  /** Looks up a field in the parent [[Cde]]
    */
  def selectDynamic[T: Tag](name: String)(using CdeUpdateContext, CdeSource): T =
    summon[CdeUpdateContext].up[T](name, Seq.empty)


// final class SiteHandle private[cde] (builder: CdeBuilder) extends Dynamic:
//   def apply[T: Tag](name: String)(using src: CdeSource): T =
//     selectDynamic(name)
// 
//   def selectDynamic[T: Tag](name: String)(using src: CdeSource): T =
//     ctx.site[T](name)


// object SiteHandle:
//   given [T](using CdeUpdateContext, CdeSource): Conversion[SiteHandle[T], T] with
//     def apply(handle: SiteHandle[T]): T = handle()


sealed class SiteHandle[T: Tag](
  reverseSubFields: Seq[String]
) extends Dynamic:
  def apply()(using CdeUpdateContext, CdeSource): T =
    val ctx = summon[CdeUpdateContext]
    if reverseSubFields.isEmpty then
      ctx.site[T](ctx.currentName, reverseSubFields.reverse)
    else
      val rev = reverseSubFields.reverse
      ctx.site[T](rev.head, rev.tail)

  def selectDynamic(subField: String): SiteHandle[T] =
    new SiteHandle(subField +: reverseSubFields)

  def applyDynamic(subField: String)()(using CdeUpdateContext, CdeSource): T =
    selectDynamic(subField)()

def Site[T: Tag](using CdeUpdateContext, CdeSource): SiteHandle[T] =
  new SiteHandle[T](Seq.empty)

final class Path(reverseSubFields: Seq[String]) extends Dynamic:
  def selectDynamic(subField: String): Path = new Path(subField +: reverseSubFields)

// sealed class SiteHandle(
//   name: String,
//   reverseSubFields: Seq[String]
// )(using CdeUpdateContext, CdeSource) extends Dynamic:
//   def get[T: Tag]: T = summon[CdeUpdateContext].site[T](name, reverseSubFields.reverse)
//   def apply[T: Tag]: T = summon[CdeUpdateContext].site[T](name, reverseSubFields.reverse)
//   def selectDynamic(subField: String): SiteHandle = new SiteHandle(name, subField +: reverseSubFields)
//
// /** Methods for performing "site" or "self" [[Cde]] lookups
//   */
// object Site extends Dynamic:
//   /** Looks up a field from the top-level [[Cde]] by a string parameter
//     */
//   def apply(name: String)(using CdeUpdateContext, CdeSource): SiteHandle =
//     selectDynamic(name)
// 
//   /** Looks up a field from the top-level [[Cde]] using method syntax
//     */
//   def selectDynamic(name: String)(using CdeUpdateContext, CdeSource): SiteHandle =
//     new SiteHandle(name, Seq.empty)
// 
//   def apply()(using ctx: CdeUpdateContext): Dynamic = ??? //new SiteHandle(ctx)
/**
 * Cde {
 *   "project" :+= Cde {
 *     val project = self
 *     name := "cde",
 *     classesDir :+= s"${project.out[String]}/${project.name[String]}/${project.version[String]}/classes"
 *     scala :+= {
 *       Cde {
 *         analysis += {
 *         }
 *       }
 *     }
 *   }
 * }
 */
