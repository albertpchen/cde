package cde

import scala.collection.mutable
import scala.language.dynamics

trait OMUpdateContext {
  private[cde] def up[T: Tag](name: String): T
  private[cde] def site[T: Tag](name: String): T
}

object Up extends Dynamic {
  def apply[T: Tag](name: String)(using OMUpdateContext): T = {
    selectDynamic(name)
  }
  def selectDynamic[T: Tag](name: String)(using OMUpdateContext): T = {
    summon[OMUpdateContext].up[T](name)
  }
}

object Site extends Dynamic {
  def apply[T: Tag](name: String)(using OMUpdateContext): T = {
    summon[OMUpdateContext].site[T](name)
  }

  def selectDynamic[T: Tag](name: String)(using OMUpdateContext): T = {
    summon[OMUpdateContext].site[T](name)
  }
}
