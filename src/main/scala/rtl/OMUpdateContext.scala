package cde

import scala.collection.mutable
import scala.language.dynamics

trait CdeUpdateContext {
  private[cde] def currentName: String
  private[cde] def up[T: Tag](name: String): T
  private[cde] def site[T: Tag](name: String): T
}

object Up extends Dynamic {
  def apply[T: Tag](using CdeUpdateContext): T = {
    selectDynamic(summon[CdeUpdateContext].currentName)
  }
  def selectDynamic[T: Tag](name: String)(using CdeUpdateContext): T = {
    summon[CdeUpdateContext].up[T](name)
  }
}

object Site extends Dynamic {
  def apply[T: Tag](name: String)(using CdeUpdateContext): T = {
    summon[CdeUpdateContext].site[T](name)
  }

  def selectDynamic[T: Tag](name: String)(using CdeUpdateContext): T = {
    summon[CdeUpdateContext].site[T](name)
  }
}
