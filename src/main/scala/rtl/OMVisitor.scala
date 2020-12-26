package cde

trait OMVisitor[T]:
  type Builder
  def empty(): Builder
  def visit(builder: Builder, cmd: OMCmd): Builder
  def finish(builder: Builder): T

trait CdeElaborator[T]:
  def elaborate(ctx: Cde.Context): T
