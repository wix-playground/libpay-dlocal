package com.wix.pay.dlocal

import com.google.api.client.http.UrlEncodedParser
import com.wix.pay.{PaymentErrorException, PaymentRejectedException}
import com.wix.pay.testkit.LibPayTestSupport
import org.specs2.matcher.Matcher
import org.specs2.matcher.MustThrownMatchers._
import spray.http._

import scala.util.{Random, Try}

trait DLocalTestSupport extends LibPayTestSupport {


  val merchantCredentials = DLocalMerchant("some merchant id", "some sub code")
  val merchantCredentialsAsString = JsonDLocalMerchantParser.stringify(merchantCredentials)

  val documentId = randomStringWithLength(26)

  val someErrorCode = "300"
  val someErrorDescription = "Invalid params x_login"
  val someRejectionDescription = "cc_rejected_insufficient_amount"
  val pendingFlowIsNotSupportedMessage = "Pending transactions are not supported"
  val someTransactionStatusCode = "6"
  val someDescription = "Invalid transaction"

  def randomStringWithLength(length: Int): String = Random.alphanumeric.take(length).mkString

  def beRequestWith(method: HttpMethod): Matcher[HttpRequest] = {
    equalTo(method) ^^ { r: HttpRequest => r.method }
  }

  def beRequestWith(url: String): Matcher[HttpRequest] = {
    equalTo(url) ^^ { r: HttpRequest => r.uri.toString() }
  }

  def beRequestThat(containsAllUrlEncodedParams: Seq[(String, String)]): Matcher[HttpRequest] = {
    def actualBody(request: HttpRequest): Seq[(String, Any)] = {
      val actualMap = new java.util.LinkedHashMap[String, java.util.List[_]]()
      UrlEncodedParser.parse(request.entity.asString, actualMap)
      import scala.collection.JavaConverters._
      actualMap.asScala.mapValues(_.asScala.toSeq).toSeq
    }

    val expectedContent: Seq[(String, Any)] = containsAllUrlEncodedParams.map(t => (t._1, Seq(t._2)))

    containAllOf(expectedContent) ^^ { r: HttpRequest => actualBody(r) }
  }


  def succeedWith(value: String): Matcher[Try[String]] = beSuccessfulTry.withValue(value)


  def failWith(message: String): Matcher[Try[String]] = beFailedTry.like { case e: PaymentErrorException => e.message must contain(message) }

  def beRejectedWith(description: String): Matcher[Try[String]] = beFailedTry.like { case e: PaymentRejectedException => e.message must contain(description) }


}
