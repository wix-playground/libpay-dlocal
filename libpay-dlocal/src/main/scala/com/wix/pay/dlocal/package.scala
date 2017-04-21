package com.wix.pay

import com.wix.pay.creditcard.CreditCard
import com.wix.pay.model.{Customer, Deal, Payment}

package object dlocal {

  private val SuccessResponseStatus = "OK"
  private val NotPresent = "NA"

  private[dlocal] sealed abstract class TransactionStatus(val dLocalCode: String)

  private[dlocal] case object Canceled extends TransactionStatus(dLocalCode = "1")
  private[dlocal] case object Pending extends TransactionStatus(dLocalCode = "7")
  private[dlocal] case object Rejected extends TransactionStatus(dLocalCode = "8")
  private[dlocal] case object Approved extends TransactionStatus(dLocalCode = "9")
  private[dlocal] case object Authorized extends TransactionStatus(dLocalCode = "11")

  private[dlocal] trait DLocalRequest {
    def fields: Map[String, String]
  }

  private[dlocal] trait DLocalBaseRequest extends DLocalRequest {
    val mandatoryFields: Map[String, String]
    val optionalFields: Map[String, Option[String]]

    override def fields: Map[String, String] = {
      def unbox[T](request: Map[String, Option[T]]): Map[String, T] =
        request.filter(_._2.isDefined).mapValues(_.get)

      mandatoryFields ++ unbox(optionalFields)
    }
  }

  private[dlocal] case class DLocalSaleRequest(merchant: DLocalMerchant,
                                               creditCard: CreditCard,
                                               payment: Payment,
                                               customer: Option[Customer],
                                               deal: Option[Deal]) extends DLocalBaseRequest {

    val cardPublicFields = creditCard.additionalFields.flatMap(_.publicFields)
    val billingAddress = cardPublicFields.flatMap(_.billingAddressDetailed)

    val mandatoryFields = Map(
      "x_merchant_id" -> merchant.merchantId,
      "x_sub_code" -> merchant.subCode,
      "x_invoice" -> deal.flatMap(_.invoiceId) || failWithMissingField("Invoice Id"),
      "x_amount" -> payment.currencyAmount.amount.toString,
      "x_currency" -> payment.currencyAmount.currency,
      "x_description" -> deal.flatMap(_.description) || failWithMissingField("Deal Description"),
      "x_country" -> billingAddress.flatMap(_.countryCode).map(_.getCountry) || failWithMissingField("Billing Country"),
      "x_cpf" -> cardPublicFields.flatMap(_.holderId) || failWithMissingField("Card Holder Id"),
      "x_name" -> creditCard.holderName || failWithMissingField("Card Holder Name"),
      "x_email" -> customer.flatMap(_.email) || failWithMissingField("Customer Email"),
      "cc_number" -> creditCard.number,
      "cc_exp_month" -> creditCard.expiration.month.toString,
      "cc_exp_year" -> creditCard.expiration.year.toString,
      "cc_cvv" -> creditCard.csc || failWithMissingField("Card CSC")
    )

    val optionalFields = Map(
      "cc_installments" -> Some(payment.installments).map(_.toString),
      "x_ip" -> customer.flatMap(_.ipAddress),
      "x_address" -> billingAddress.map(_.composedAddress),
      "x_zip" -> billingAddress.flatMap(_.postalCode),
      "x_city" -> billingAddress.flatMap(_.city),
      "x_state" -> billingAddress.flatMap(_.state),
      "x_phone" -> customer.flatMap(_.phone)
    )
  }

  private[dlocal] case class DLocalCaptureRequest(authorization: DLocalAuthorization,
                                                  amount: Double) extends DLocalBaseRequest {
    val mandatoryFields: Map[String, String] = Map(
      "x_invoice" -> authorization.invoiceId,
      "x_currency" -> authorization.currency,
      "x_auth_id" -> authorization.authId
    )

    val optionalFields: Map[String, Option[String]] = Map(
      "x_amount" -> Some(amount).map(_.toString)
    )
  }

  private[dlocal] case class DLocalVoidAuthorizationRequest(authorization: DLocalAuthorization) extends DLocalBaseRequest {
    val mandatoryFields: Map[String, String] = Map(
      "x_invoice" -> authorization.invoiceId,
      "x_auth_id" -> authorization.authId
    )

    val optionalFields: Map[String, Option[String]] = Map.empty
  }

  private[dlocal] type ResponseFields = String => Option[String]

  private[dlocal] trait DLocalResponseHandler {
    def handle(responseFields: ResponseFields): String
  }

  private[dlocal] trait DLocalBaseResponseHandler extends DLocalResponseHandler {

    def expectedTransactionStatus: TransactionStatus

    def extractResult(responseFields: ResponseFields): String

    def handle(responseFields: ResponseFields): String = {
      assertResponseIsOk(responseFields)
      extractResult(responseFields)
    }

    private def assertResponseIsOk(responseFields: ResponseFields): Unit = {
      if (!responseFields("status").contains(SuccessResponseStatus)) {
        val errorCode = responseFields("error_code") || NotPresent
        val description = responseFields("desc") || NotPresent
        throw PaymentErrorException(s"Transaction failed($errorCode): $description")
      }

      if (responseFields("result").contains(Rejected.dLocalCode)) {
        val description = responseFields("desc") || NotPresent
        throw PaymentRejectedException(description)
      }

      if (responseFields("result").contains(Pending.dLocalCode)) {
        throw PaymentErrorException("Pending transactions are not supported")
      }

      if (!responseFields("result").contains(expectedTransactionStatus.dLocalCode)) {
        val result = responseFields("result") || NotPresent
        val description = responseFields("desc") || NotPresent
        throw PaymentErrorException(s"Transaction is not $expectedTransactionStatus" +
          s"(${expectedTransactionStatus.dLocalCode}), but ($result): $description")
      }
    }
  }

  private[dlocal] object DLocalSaleResponseHandler extends DLocalBaseResponseHandler {
    val expectedTransactionStatus = Approved

    def extractResult(responseFields: ResponseFields): String = extractOrFail(responseFields)("x_document")
  }

  private[dlocal] object DLocalAuthorizeResponseHandler extends DLocalBaseResponseHandler {
    val expectedTransactionStatus = Authorized

    def extractResult(responseFields: ResponseFields): String = {
      def value = extractOrFail(responseFields) _

      val authorization = DLocalAuthorization(value("x_auth_id"), value("x_invoice"), value("x_currency"))
      DLocalAuthorization.stringify(authorization)
    }
  }

  private[dlocal] object DLocalCaptureResponseHandler extends DLocalBaseResponseHandler {
    val expectedTransactionStatus = Approved

    def extractResult(responseFields: ResponseFields): String = extractOrFail(responseFields)("x_document")
  }

  private[dlocal] object DLocalVoidAuthorizationResponseHandler extends DLocalBaseResponseHandler {
    val expectedTransactionStatus = Canceled

    def extractResult(responseFields: ResponseFields): String = extractOrFail(responseFields)("x_auth_id")
  }

  private implicit class `(String, Option[String]) --> (String, String)`(entry: (String, Option[String])) {
    def ||(default: => String): (String, String) = (entry._1, entry._2.getOrElse(default))
  }

  private implicit class `Option[String] --> String`(option: Option[String]) {
    def ||(default: => String): String = option.getOrElse(default)
  }

  private def failWithMissingField(fieldName: String): String = {
    throw new IllegalArgumentException(s"'$fieldName' must be given")
  }

  private def extractOrFail(responseFields: ResponseFields)(field: String): String = {
    responseFields(field).getOrElse {
      throw PaymentErrorException(s"No $field in response")
    }
  }
}
