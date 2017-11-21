package ast

import scala.collection.mutable

trait Valor extends Expressao

abstract class ValorConcreto[T](val valor : T) extends Valor {
  override def avaliar(): Valor = this
}

case class ValorBooleano(v : Boolean) extends ValorConcreto[Boolean](v)

case class ValorInteiro(v : Integer) extends ValorConcreto[Integer](v)

case class ValorNull() extends ValorConcreto[Null](null)

case class Closure(val id : String, val corpo : Expressao, ambiente : mutable.HashMap[String, Valor]) extends Valor {
  override def avaliar(): Valor = this
}


