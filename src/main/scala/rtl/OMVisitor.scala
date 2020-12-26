package cde

trait CdeElaborator[T]:
  def elaborate(ctx: Cde.Context): Either[Seq[String], T]
