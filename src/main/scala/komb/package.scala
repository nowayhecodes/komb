package object komb {
  type EnvT = List[EnvMapT]
  type EnvMapT = Map[String, ExprT]

  def EnvT() = List(EnvMapT())

  def EnvMapT(xs: (String, ExprT)*) = Map(xs: _*)

  def EnvT(xs: EnvMapT*) = List(xs: _*)
}
