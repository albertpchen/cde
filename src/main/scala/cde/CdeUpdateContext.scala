package cde

import scala.annotation.implicitNotFound
import scala.collection.mutable
import scala.language.dynamics

/** A value signifying a context where [[Up]] and [[Site]] lookups my happen
  */
@implicitNotFound("Could not find CdeUpdateContext make sure you are using CdeUpdate or :+=")
trait CdeUpdateContext derives CanEqual:
  /** The name of the field currently being elaborated
    */
  private[cde] def currentName: String

  /** Looks up the field in the parent [[Cde]]
    */
  private[cde] def up[T: Tag](name: String)(using CdeSource): T

  /** Looks up the field in the top-level [[Cde]]
    */
  private[cde] def site[T: Tag](name: String)(using CdeSource): T


/** Methods for performing "up" or "super" [[Cde]] lookups
  */
object Up extends Dynamic:
  /** Looks up the current field being elaborated in the parent [[Cde]]
    */
  def apply[T: Tag](using CdeUpdateContext, CdeSource): T =
    selectDynamic(summon[CdeUpdateContext].currentName)

  /** Looks up a field in the parent [[Cde]]
    */
  def selectDynamic[T: Tag](name: String)(using CdeUpdateContext, CdeSource): T =
    summon[CdeUpdateContext].up[T](name)


/** Methods for performing "site" or "self" [[Cde]] lookups
  */
object Site extends Dynamic:
  /** Looks up a field from the top-level [[Cde]] by a string parameter
    */
  def apply[T: Tag](name: String)(using CdeUpdateContext, CdeSource): T =
    summon[CdeUpdateContext].site[T](name)

  /** Looks up a field from the top-level [[Cde]] using method syntax
    */
  def selectDynamic[T: Tag](name: String)(using CdeUpdateContext, CdeSource): T =
    summon[CdeUpdateContext].site[T](name)
