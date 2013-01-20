package org.i5y.routes

import scala.reflect.macros.Context
import scala.language.experimental.macros
import scala.annotation.implicitNotFound
import scala.concurrent.Future
import scala.util.Try
import scala.concurrent.ExecutionContext

@implicitNotFound(
  "Cannot write an instance of ${A} to HTTP response. Try to define a Writeable[${A}]")
case class Writeable[-A](transform: (A => HttpResponse), contentType: Option[String]) {
  def map[B](f: B => A): Writeable[B] = Writeable(b => transform(f(b)), contentType)
}

trait HttpRequest {

}

trait HttpResponse {

}

trait Route {
}

class RouteImpl(route: String, f: HttpRequest => Future[HttpResponse]) extends Route {
  override def toString = route
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

class PRImpl(val route: String, filters: List[HttpRequest => Either[HttpResponse, HttpRequest]] = List.empty) extends PR {
  def execute[C](x: => C, y: Writeable[C])(request: HttpRequest) = {
    Future.successful(y.transform(x))
  }
  def mapped[C](x: => C)(implicit y: Writeable[C]): Route = new RouteImpl(route, execute(x, y))
  def wth[T](f: HttpRequest => Either[HttpResponse, T]) = new PR1Ext[T](this, f)
  def fltr(f: HttpRequest => Either[HttpResponse, HttpRequest]) = new PRImpl(route, filters :+ f)
}

trait PR1Base[T] extends PR1[T] {
  def route: String
  def ext(req: HttpRequest): Either[HttpResponse, T]

  def extract[C](x: T => C, y: Writeable[C])(request: HttpRequest) = {
    ext(request) match {
      case Left(httpResponse) => Future.successful(httpResponse)
      case Right(t) => Future.successful(y.transform(x(t)))
    }
  }
  def mapped[C](x: T => C)(implicit y: Writeable[C]): Route = new RouteImpl(route, extract(x, y))
}

class PR1Ext[T](pr: PRImpl, f: HttpRequest => Either[HttpResponse, T]) extends PR1Base[T] {
  def route = pr.route
  def ext(req: HttpRequest) = {
    f(req)
  }
}

class PR1Impl[T](val route: String, f: String => T) extends PR1Base[T] {
  def ext(req: HttpRequest): Either[HttpResponse, T] = {
    Right(f(""))
  }
}

trait PR2Base[T, U] extends PR2[T, U] {
  def ext(req: HttpRequest): Either[HttpResponse, (T, U)]
  def execute[C](x: (T, U) => C, y: Writeable[C])(request: HttpRequest) = {
    ext(request) match {
      case Left(httpResponse) => Future.successful(httpResponse)
      case Right((t, u)) => Future.successful(y.transform(x(t, u)))
    }
  }
  def executeAsync[C](x: (T, U) => Future[C], y: Writeable[C])(request: HttpRequest)(implicit ex: ExecutionContext) = {
    ext(request) match {
      case Left(httpResponse) => Future.successful(httpResponse)
      case Right((t, u)) => x(t, u).map(y.transform(_))
    }
  }
  def executeRaw[C](x: HttpRequest => C, y: Writeable[C])(request: HttpRequest) = {
    Future.successful(y.transform(x(request)))
  }
  def executeRawAsync[C](x: HttpRequest => Future[C], y: Writeable[C])(request: HttpRequest)(implicit ex: ExecutionContext) = {
    x(request).map(y.transform(_))
  }
  def route: String
  def mapped[C](x: (T, U) => C)(implicit y: Writeable[C]) = new RouteImpl(route, execute(x, y))
  def mappedAsync[C](x: (T, U) => Future[C])(implicit y: Writeable[C], ex: ExecutionContext) = new RouteImpl(route, executeAsync(x, y))
  def raw[C](x: HttpRequest => C)(implicit y: Writeable[C]): Route = new RouteImpl(route, executeRaw(x, y))
  def rawAsync[C](x: HttpRequest => Future[C])(implicit y: Writeable[C], ex: ExecutionContext): Route = new RouteImpl(route, executeRawAsync(x, y))
}

class PR2Ext[T, U](pr1: PR1Impl[T], f: HttpRequest => Either[HttpResponse, U]) extends PR2Base[T, U] {
  def route = pr1.route
  def ext(req: HttpRequest) = {
    for {
      t <- pr1.ext(req).right
      u <- f(req).right
    } yield (t, u)
  }
}

class PR2Impl[T, U](val route: String, f: String => T, f2: String => U) extends PR2Base[T, U] {
  def ext(req: HttpRequest) = {
    Right((f(""), f2("")))
  }
}

trait Routes {
  implicit val wString: Writeable[String] = Writeable[String](_ => new HttpResponse {}, None)
  implicit val wInt: Writeable[Int] = Writeable[Int](_ => new HttpResponse {}, None)
  implicit val wBytes: Writeable[Array[Byte]] = Writeable(_ => new HttpResponse {}, None)
  implicit val wUnit: Writeable[Unit] = Writeable(_ => new HttpResponse {}, None)

  def get(path: String) = macro Routes.get_impl
}

object Routes {

  def get_impl(c: Context)(path: c.Expr[String]): c.universe.Expr[_ <: Partial] = {
    import c.universe._

    val pathString = path.tree match {
      case (Literal(Constant(queryString: String))) => queryString
      case _ => c.abort(c.enclosingPosition, "query isn't a string literal")
    }

    val o = reify { x: String => x.toInt }

    val components = PathParser.parse(pathString)
      .filter(_.isInstanceOf[Param])
      .map(_.asInstanceOf[Param])
      .map {
        pathParam =>
          pathParam.typ match {
            case "Int" => (c.typeTag[Int], reify[String => Int] { (x: String) => x.toInt })
            case "String" => (c.typeTag[String], reify[String => String] { x: String => x })
            case _ => c.abort(c.enclosingPosition, "unsupported types")
          }
      }

    def p[T:WeakTypeTag](implicit evd: WeakTypeTag[T]): c.Expr[String => T] = {
      if( evd == c.typeTag[Int])  reify[String => Int] { (x: String) => x.toInt }.asInstanceOf[c.Expr[String => T]]
      else if( evd == c.typeTag[String])  reify[String => String] { x: String => x }.asInstanceOf[c.Expr[String => T]]
      else  c.abort(c.enclosingPosition, "unsupported types")
    }

    def genRes(path: c.Expr[String]) =
      reify { new PRImpl(path.splice) }
    def genRes1[T: WeakTypeTag](path: c.Expr[String]): c.universe.Expr[PR1[T]] ={
      val f = p[T]
      reify { new PR1Impl[T](path.splice, f.splice) }
    }
    def genRes2[T: WeakTypeTag, U: WeakTypeTag](path: c.Expr[String]): c.universe.Expr[PR2[T, U]] ={
      val f = p[T]
      val f2 = p[U]
      reify { new PR2Impl[T, U](path.splice, f.splice, f2.splice) }
    }

    components.size match {
      case 0 => genRes(path)
      case 1 => genRes1(path)(components.head._1)
      case 2 => genRes2(path)(components.head._1, components.last._1)
      case _ => c.abort(c.enclosingPosition, "too many variable components")
    }
  }
}