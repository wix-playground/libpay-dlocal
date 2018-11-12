package com.wix.pay.dlocal


import akka.http.scaladsl.model._
import com.google.api.client.http.UrlEncodedParser
import com.wix.e2e.http.client.extractors.HttpMessageExtractors._
import com.wix.pay.testkit.LibPayTestSupport
import com.wix.pay.{PaymentErrorException, PaymentRejectedException}
import org.specs2.matcher.Matcher
import org.specs2.matcher.MustThrownMatchers._

import scala.util.{Random, Try}


trait DLocalTestSupport extends LibPayTestSupport {

  val merchant = DLocalMerchant("some merchant id", "some sub code", "some@email.com")
  val merchantAsString: String = DLocalMerchant.stringify(merchant)

  val authorization = DLocalAuthorization(authId = "some authorization id", invoiceId = "some invoice id", currency = "BRL")
  val authorizationAsString: String = DLocalAuthorization.stringify(authorization)

  val documentId: String = randomStringWithLength(26)

  val someErrorCode = "300"
  val someErrorDescription = "Invalid params x_login"
  val someRejectionDescription = "cc_rejected_insufficient_amount"
  val somePendingDescription = "in_process"
  val someTransactionStatusCode = "6"
  val someDescription = "Invalid transaction"

  val pendingFlowIsNotSupportedMessage = "Pending transactions are not supported"
  val internalServerErrorMessage = "500 Internal Server Error"

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
      UrlEncodedParser.parse(request.entity.extractAsString, actualMap)
      import scala.collection.JavaConverters._
      actualMap.asScala.mapValues(_.asScala.toSeq).toSeq
    }

    val expectedContent: Seq[(String, Any)] = containsAllUrlEncodedParams.map(t => (t._1, Seq(t._2)))

    containAllOf(expectedContent) ^^ { r: HttpRequest => actualBody(r) }
  }

  def notFail: AnyRef with Matcher[Any] = not(throwA[Exception])

  def beSucceedTryWith(value: String): Matcher[Try[String]] = beSuccessfulTry.withValue(value)

  def failWith(message: String, documentId: Option[String] = None): Matcher[Try[String]] = {
    beFailedTry.like {
      case e: PaymentErrorException =>
        e.message must contain(message)
        e.transactionId must equalTo(documentId)
    }
  }

  def beRejectedWith(description: String, documentId: Option[String] = None): Matcher[Try[String]] = {
    beFailedTry.like { case e: PaymentRejectedException =>
      e.message must contain(description)
      e.transactionId must equalTo(documentId)
    }
  }

  def failWithMissingField(fieldName: String): Matcher[Any] = {
    throwA[IllegalArgumentException](s"'$fieldName' must be given")
  }

  def beFailedTransactionWith(errorCode: String, errorDescription: String, documentId: Option[String] = None): Matcher[Try[String]] = {
    failWith(s"Transaction failed($errorCode): $errorDescription", documentId)
  }
}


object DLocalTestSupport extends DLocalTestSupport
