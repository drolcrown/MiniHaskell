package ast

trait Valor extends Expressao

abstract class ValorConcreto[T](val valor : T) extends Valor {
  override def avaliar(): Valor = this
}

case class ValorBooleano(v : Boolean) extends ValorConcreto[Boolean](v)

case class ValorInteiro(v : Integer) extends ValorConcreto[Integer](v)

case class ValorNull(v : Null) extends ValorConcreto[Null](v)
