import org.i5y.routes._

case class Payment(id: String)

trait Core {
  def purchase(productCode: Int) = { "" }
  def purchases(productCode: Int, page: Int) = { "" }
  def purchasesRaw(request: HttpRequest) = {}
  def payment(id: Int) = {
    Payment(id.toString)
  }
  def payments() = {
    (1 to 10).map(x => Payment(x.toString)).toList
  }
}

trait TestResource extends Routes with Core {
  implicit val wPayment: Writeable[Payment] = Writeable(_ => new HttpResponse {}, None)
  implicit def wListPayment[T](implicit f: Writeable[T]): Writeable[List[T]] = Writeable(_ => new HttpResponse {}, None)

  def passwordCheck(username: String, password: String) = username == password

  val passwordFilter = basicAuth(passwordCheck)

  val g = get("")
  val h = get("/path/{a:Int}/something-else")
  val i = get("/path/{a:Int}/something-else", "l:String")
  val j = get("/path/{a:Int}/{b:String}")

  val paymentsRoute = get("/payments").mapped(payments)

  val paymentRoute = get("/payment/{a:Int}").mapped(payment)

  def ext(req: HttpRequest) = Right(1)

  val q = get("/purchases").wth(ext).mapped(purchase)
  val r = get("/purchases/{productCode:Int}", "page:Int").mapped(purchases)
  val s = get("/purchases/{productCode:Int}", "page:Int").raw(purchasesRaw)
}

trait WebAppRoutes {
  val routes: Seq[Route]
}

abstract class WebApp {
  val routes: Seq[Route]

  def start = routes.foreach(println(_))
}

object Test extends App {

  trait WR extends WebAppRoutes with TestResource {
    val secured = List(q).map(_.fltr(passwordFilter))
    val unsecured = List(r, s, paymentRoute, paymentsRoute)
    
    val routes = unsecured ++ secured
  }

  val wa = new WebApp with WR
  wa.start
}