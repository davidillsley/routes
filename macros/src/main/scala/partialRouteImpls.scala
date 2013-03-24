package org.i5y.routes

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class PRImpl(val method:String, val route: String, filters: List[HttpRequest => Either[HttpResponse, HttpRequest]] = List.empty) extends PR {
  def execute[C](x: => C, y: Writeable[C])(request: HttpRequest) = {
    Future.successful(y.transform(x))
  }
  def mapped[C](x: => C)(implicit y: Writeable[C]): Route = new RouteImpl(method, route, execute(x, y))
  def wth[T](f: HttpRequest => Either[HttpResponse, T]) = new PR1Ext[T](this, f)
  def fltr(f: HttpRequest => Either[HttpResponse, HttpRequest]) = new PRImpl(method, route, filters :+ f)
}

trait PR1Base[T] extends PR1[T] {
  def method: String
  def route: String
  def ext(req: HttpRequest): Either[HttpResponse, T]

  def extract[C](x: T => C, y: Writeable[C])(request: HttpRequest) = {
    ext(request) match {
      case Left(httpResponse) => Future.successful(httpResponse)
      case Right(t) => Future.successful(y.transform(x(t)))
    }
  }
  def mapped[C](x: T => C)(implicit y: Writeable[C]): Route = new RouteImpl(method, route, extract(x, y))
}

class PR1Ext[T](pr: PRImpl, f: HttpRequest => Either[HttpResponse, T]) extends PR1Base[T] {
  def method = pr.method
  def route = pr.route
  def ext(req: HttpRequest) = {
    f(req)
  }
}

class PR1Impl[T](val method: String, val route: String, f: String => T) extends PR1Base[T] {
  def ext(req: HttpRequest): Either[HttpResponse, T] = {
    Right(f(""))
  }
}

trait PR2Base[T, U] extends PR2[T, U] {
  def method: String
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
  def mapped[C](x: (T, U) => C)(implicit y: Writeable[C]) = new RouteImpl(method, route, execute(x, y))
  def mappedAsync[C](x: (T, U) => Future[C])(implicit y: Writeable[C], ex: ExecutionContext) = new RouteImpl(method, route, executeAsync(x, y))
  def raw[C](x: HttpRequest => C)(implicit y: Writeable[C]): Route = new RouteImpl(method, route, executeRaw(x, y))
  def rawAsync[C](x: HttpRequest => Future[C])(implicit y: Writeable[C], ex: ExecutionContext): Route = new RouteImpl(method, route, executeRawAsync(x, y))
}

class PR2Ext[T, U](pr1: PR1Impl[T], f: HttpRequest => Either[HttpResponse, U]) extends PR2Base[T, U] {
  def method = pr1.method
  def route = pr1.route
  def ext(req: HttpRequest) = {
    for {
      t <- pr1.ext(req).right
      u <- f(req).right
    } yield (t, u)
  }
}

class PR2Impl[T, U](val method: String, val route: String, f: String => T, f2: String => U) extends PR2Base[T, U] {
  def ext(req: HttpRequest) = {
    Right((f(""), f2("")))
  }
}