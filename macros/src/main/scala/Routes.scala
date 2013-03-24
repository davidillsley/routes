package org.i5y.routes

import scala.reflect.macros.Context
import scala.language.experimental.macros
import scala.annotation.implicitNotFound
import scala.concurrent.Future
import scala.util.Try
import scala.concurrent.ExecutionContext
import sun.misc.BASE64Decoder

@implicitNotFound(
  "Cannot write an instance of ${A} to HTTP response. Try to define a Writeable[${A}]")
case class Writeable[-A](transform: (A => HttpResponse), contentType: Option[String]) {
  def map[B](f: B => A): Writeable[B] = Writeable(b => transform(f(b)), contentType)
}

trait HttpRequest {
  def method: String
  def uri: String
  def path: String
  def query: String

  def headers: List[(String, String)]

}

trait HttpResponse {

}

trait Route {
  def method: String
  def fltr(f: HttpRequest => Either[HttpResponse, HttpRequest]): Route
}

class RouteImpl(val method: String, route: String, f: HttpRequest => Future[HttpResponse]) extends Route {
  override def toString = method + " " + route

  def fltr(f2: HttpRequest => Either[HttpResponse, HttpRequest]) = {
    new RouteImpl(method, route, { request =>
      f2(request) match {
        case Left(httpResponse) => Future.successful(httpResponse)
        case Right(t) => f(t)
      }
    })
  }
}

trait Partial

trait PR extends Partial {
  def mapped[C](x: => C)(implicit y: Writeable[C]): Route
  def wth[T](f: HttpRequest => Either[HttpResponse, T]): PR1[T]
  def fltr(f: HttpRequest => Either[HttpResponse, HttpRequest]): PR
}

trait PR1[T] extends Partial {
  def mapped[C](x: T => C)(implicit y: Writeable[C]): Route
}

trait PR2[T1, T2] extends Partial {
  def mapped[C](x: (T1, T2) => C)(implicit y: Writeable[C]): Route
  def mappedAsync[C](x: (T1, T2) => Future[C])(implicit y: Writeable[C], ex: ExecutionContext): Route
  def raw[C](x: HttpRequest => C)(implicit y: Writeable[C]): Route
  def rawAsync[C](x: HttpRequest => Future[C])(implicit y: Writeable[C], ex: ExecutionContext): Route
}

trait Routes {
  implicit val wString: Writeable[String] = Writeable[String](_ => new HttpResponse {}, None)
  implicit val wInt: Writeable[Int] = Writeable[Int](_ => new HttpResponse {}, None)
  implicit val wBytes: Writeable[Array[Byte]] = Writeable(_ => new HttpResponse {}, None)
  implicit val wUnit: Writeable[Unit] = Writeable(_ => new HttpResponse {}, None)

  def basicAuth(f: (String, String) => Boolean) = {
    def check(request: HttpRequest): Either[HttpResponse, HttpRequest] = {
      val success =
        request.headers.find {
          case (headerName, _) => headerName == "Authorization"
        }.exists {
          case (_, headerValue) =>
            val decoded = new String(new BASE64Decoder().decodeBuffer(headerValue))
            val sections = decoded.split(':')
            val username = sections(0)
            val password = sections(1)
            f(username, password)
        }
      if (success) Right(request) else Left(new HttpResponse {})
    }
    check _
  }

  def get(path: String, params: String*) = macro Routes.get_impl2
  def post(path: String, params: String*) = macro Routes.post_impl2
}

object Routes {

  def get_impl2(c: Context)(path: c.Expr[String], params: c.Expr[String]*): c.universe.Expr[_ <: Partial] =
    internal(c)(path, params: _*)("get")
  def post_impl2(c: Context)(path: c.Expr[String], params: c.Expr[String]*): c.universe.Expr[_ <: Partial] =
    internal(c)(path, params: _*)("post")
  def internal(c: Context)(path: c.Expr[String], params: c.Expr[String]*)(method:String): c.universe.Expr[_ <: Partial] = {
    import c.universe._

    val pathString = path.tree match {
      case (Literal(Constant(queryString: String))) => queryString
      case _ => c.abort(c.enclosingPosition, "query isn't a string literal")
    }

    val a = "\\{.*?\\}".r
    val pathComponents = a.findAllIn(pathString)
      .toList
      .map(_.stripPrefix("{").stripSuffix("}"))
      .map(parseTag(c)(_))

    val queryComponents = params.map(_.tree match {
      case (Literal(Constant(tag: String))) => parseTag(c)(tag)
      case _ => c.abort(c.enclosingPosition, "path component isn't a string literal")
    })

    val components = pathComponents ++ queryComponents

    def p[T: WeakTypeTag](implicit evd: WeakTypeTag[T]): c.Expr[String => T] = {
      if (evd == c.typeTag[Int]) reify[String => Int] { (x: String) => x.toInt }.asInstanceOf[c.Expr[String => T]]
      else if (evd == c.typeTag[String]) reify[String => String] { x: String => x }.asInstanceOf[c.Expr[String => T]]
      else if (evd == c.typeTag[Option[String]]) reify[String => Option[String]] { x: String => Some[String](x) }.asInstanceOf[c.Expr[String => T]]
      else if (evd == c.typeTag[Option[Int]]) reify[String => Option[Int]] { x: String => Some[Int](x.toInt) }.asInstanceOf[c.Expr[String => T]]
      else c.abort(c.enclosingPosition, "unsupported types")
    }

    def genRes(path: c.Expr[String], method: c.Expr[String]) =
      reify { new PRImpl(method.splice, path.splice) }
    def genRes1[T: WeakTypeTag](path: c.Expr[String], method: c.Expr[String]): c.universe.Expr[PR1[T]] = {
      val f = p[T]
      reify { new PR1Impl[T](method.splice, path.splice, f.splice) }
    }
    def genRes2[T: WeakTypeTag, U: WeakTypeTag](path: c.Expr[String], method: c.Expr[String]): c.universe.Expr[PR2[T, U]] = {
      val f = p[T]
      val f2 = p[U]
      reify { new PR2Impl[T, U](method.splice, path.splice, f.splice, f2.splice) }
    }

    components.size match {
      case 0 => genRes(path, c.literal(method))
      case 1 => genRes1(path, c.literal(method))(components.head)
      case 2 => genRes2(path, c.literal(method))(components.head, components.last)
      case _ => c.abort(c.enclosingPosition, "too many variable components")
    }
  }

  def parseTag(c: Context)(tag: String) = {
    val elts = tag.split(':')
    elts.length match {
      case 1 => c.typeTag[String]
      case 2 => elts(1) match {
        case "Int" => c.typeTag[Int]
        case "String" => c.typeTag[String]
        case "Option[Int]" => c.typeTag[Option[Int]]
        case "Option[String]" => c.typeTag[Option[String]]
        case x => {
          c.abort(c.enclosingPosition, "unsupported types" + x)
        }
      }
      case x => c.abort(c.enclosingPosition, "component has wrong number of parts: " + x)
    }
  }
}