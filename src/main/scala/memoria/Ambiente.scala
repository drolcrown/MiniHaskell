package memoria

import ast._
import exceptions._

import scala.collection.mutable

object Ambiente {

  private val stack = new mutable.Stack[mutable.HashMap[String, Valor]]()

  private val funcoesDeclaradas = new mutable.HashMap[String, DecFuncao]

  def iniciar(): Unit = {
    stack.clear()
    stack.push(new mutable.HashMap[String, Valor]())
  }

  def declararFuncao(decFuncao: DecFuncao): Unit = {
    funcoesDeclaradas += (decFuncao.nome -> decFuncao)
  }

  def recuperarFuncao(nome: String) = funcoesDeclaradas(nome)

  def atualiza(variavel : String, valor :   Valor): Unit = {
    if(stack.isEmpty) {
      stack.push(new mutable.HashMap[String, Valor])
    }
    stack.top += (variavel -> valor)
  }

  def consulta(variavel : String) : Valor = {
    if(!stack.isEmpty) {
      return stack.top(variavel)
    }
    throw VariavelNaoDeclaradaException()
  }

  def novoAmbiente(): Unit = {
    novoAmbiente(new mutable.HashMap[String, Valor]())
  }
  def novoAmbiente(ambiente: mutable.HashMap[String, Valor]): Unit = {
    stack.push(ambiente)
  }
  def ambienteAtual() : mutable.HashMap[String, Valor] = {
    if (stack.isEmpty) {
      stack.push(new mutable.HashMap[String, Valor]())
    }
    return stack.top
  }

  def removeAmbiente(): Unit = {
    stack.pop()
  }
}