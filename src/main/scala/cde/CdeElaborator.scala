package cde

/** Type class for elaborating a [[Cde]]
  */
trait CdeElaborator[T]:

  /** Elaborates a [[Cde]]
    *
    * @param ctx contains all information about the [[Cde]] instance
    */
  def elaborate(ctx: Cde.Context): Either[Seq[CdeError], T]
