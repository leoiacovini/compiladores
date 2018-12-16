package basic

import scala.collection.mutable

class SymbolTable {

  val hashMap: mutable.HashMap[String, String] = mutable.HashMap()

  def setSymbol(symbol: String, value: String): Unit = {
    this.hashMap(symbol) = value
  }

  def getValue(symbol: String): String = {
    this.hashMap(symbol)
  }

}