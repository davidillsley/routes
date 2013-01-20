package org.i5y.routes

trait PathComponent
trait Param{
  def typ:String
}

case class PathLiteral(lit: String) extends PathComponent
case class PathWildcard() extends PathComponent
case class PathParam(name: String, typ: String) extends PathComponent with Param
case class QueryParam(name: String, typ: String) extends PathComponent with Param

object PathParser {

  def parse(path: String): Seq[PathComponent] = {
    val splitpath = path.split('?')
    val pathBits = parsePathPortion(splitpath.head)
    val queryBits = if (splitpath.length == 1) List.empty else parseQueryPortion(splitpath.last)
    pathBits ++ queryBits
  }

  private def parsePathPortion(pathPortion: String): Seq[PathComponent] = {
    val c = pathPortion.split('/').map { y =>
      if (y.contains(":")) {
        val h = y.split(':')
        PathParam(h.head, h.last.trim)
      } else {
        PathLiteral(pathPortion)
      }
    }
    c.toList
  }

  private def parseQueryPortion(pathPortion: String): Seq[PathComponent] = {
    val c = pathPortion.split('&').map { y =>
      if (y.contains("=")) {
        val h = y.split('=')
        QueryParam(h.head, h.last.trim)
      } else {
       throw new Exception("WTF")
      }
    }
    c.toList
  }
}