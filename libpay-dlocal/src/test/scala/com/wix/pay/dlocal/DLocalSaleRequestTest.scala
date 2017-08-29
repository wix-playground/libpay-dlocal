package com.wix.pay.dlocal

import java.util.Locale

import org.specs2.matcher.Matchers
import org.specs2.mutable.SpecWithJUnit
import org.specs2.specification.Scope

class DLocalSaleRequestTest extends SpecWithJUnit with Matchers {

  "dLocal sale request validation" should {

    "replace invoice id to NA if missing" in new ctx {
      requestWithNoInvoiceId.fields must havePair("x_invoice" -> "NA")
    }

    "replace description to NA if missing" in new ctx {
      requestWithNoDealDescription.fields must havePair("x_description" -> "NA")
    }

    "replace email to NA email if missing" in new ctx {
      requestWithNoCustomerEmail.fields must havePair("x_email" -> "example@example.org")
    }

    "not fail if optional phone is missing" in new ctx {
      requestWithNoCustomerPhone must notFail
    }

    "fail if billing country is missing" in new ctx {
      requestWithNoBillingCountry must failWithMissingField("Billing Country")
    }

    "fail if the request is from mexico and card holder name is missing" in new ctx {
      requestFromMexicoWithNoCardHolderName must failWithMissingField("Card Holder Name")
    }

    "fail if card holder name is missing" in new ctx {
      requestWithNoCardHolderName must failWithMissingField("Card Holder Name")
    }

    "fail if card CSC is missing" in new ctx {
      requestWithNoCsc must failWithMissingField("Card CSC")
    }
  }

  "dLocal sale request fields" should {
    "not return missing optional fields" in new ctx {
      someRequest.fields must haveKey("x_phone")

      requestWithNoCustomerPhone.fields must not(haveKey("x_phone"))
    }

    "contain x_invoice" in new ctx {
      someRequest.fields must havePair("x_invoice" -> someDeal.invoiceId.get)
    }

    "contain x_amount" in new ctx {
      someRequest.fields must havePair("x_amount" -> somePayment.amount.toString)
    }

    "contain x_currency" in new ctx {
      someRequest.fields must havePair("x_currency" -> somePayment.currency)
    }

    "contain x_description" in new ctx {
      someRequest.fields must havePair("x_description" -> someDeal.description.get)
    }

    "contain x_country for mexico" in new ctx {
      requestFromMexico.fields must havePair("x_country" -> "MX")
    }

    "contain XX as x_country for other countries" in new ctx {
      requestFromGermany.fields must havePair("x_country" -> "XX")
    }

    "contain x_cpf for mexico" in new ctx {
      requestFromMexico.fields must havePair("x_cpf" -> someCreditCard.additionalFields.get.publicFields.get.holderId.get)
    }

    "not contain x_cpf for other countries" in new ctx {
      requestFromGermany.fields must not haveKey("x_cpf")
    }

    "contain x_name" in new ctx {
      someRequest.fields must havePair("x_name" -> someCreditCard.holderName.get)
    }

    "contain x_email" in new ctx {
      someRequest.fields must havePair("x_email" -> someCustomer.email.get)
    }

    "contain cc_number" in new ctx {
      someRequest.fields must havePair("cc_number" -> someCreditCard.number)
    }

    "contain cc_exp_month" in new ctx {
      someRequest.fields must havePair("cc_exp_month" -> someCreditCard.expirationMonth.toString)
    }

    "contain cc_exp_year" in new ctx {
      someRequest.fields must havePair("cc_exp_year" -> someCreditCard.expirationYear.toString)
    }

    "contain cc_cvv" in new ctx {
      someRequest.fields must havePair("cc_cvv" -> someCreditCard.additionalFields.get.csc.get)
    }


    "contain cc_installments" in new ctx {
      someRequest.fields must havePair("cc_installments" -> somePayment.installments.toString)
    }

    "contain x_ip" in new ctx {
      someRequest.fields must havePair("x_ip" -> someCustomer.ipAddress.get)
    }

    "contain x_address" in new ctx {
      someRequest.fields must havePair("x_address" -> someCreditCard.additionalFields.get.billingAddress.get)
    }

    "contain x_zip" in new ctx {
      someRequest.fields must havePair("x_zip" -> someCreditCard.additionalFields.get.billingAddressDetailed.get.postalCode.get)
    }

    "contain x_city" in new ctx {
      someRequest.fields must havePair("x_city" -> someCreditCard.additionalFields.get.billingAddressDetailed.get.city.get)
    }

    "contain x_state" in new ctx {
      someRequest.fields must havePair("x_state" -> someCreditCard.additionalFields.get.billingAddressDetailed.get.state.get)
    }

    "contain x_phone" in new ctx {
      someRequest.fields must havePair("x_phone" -> someCustomer.phone.get)
    }

    "contain no merchant id" in new ctx {
      someRequest.fields must not haveValue merchant.merchantId
    }

    "contain x_sub_code" in new ctx {
      someRequest.fields must havePair("x_sub_code" -> merchant.subCode)
    }
  }

  trait ctx extends Scope with DLocalTestSupport {

    val someRequest = DLocalSaleRequest(merchant, someCreditCard, somePayment, Some(someCustomer), Some(someDeal))

    def requestWithNoInvoiceId = someRequest.copy(deal = Some(someDeal.withInvoiceId(None)))
    def requestWithNoDealDescription = someRequest.copy(deal = Some(someDeal.withDescription(None)))
    def requestWithNoCustomerEmail = someRequest.copy(customer = Some(someCustomer.withEmail(None)))
    def requestWithNoCustomerPhone = someRequest.copy(customer = Some(someCustomer.withPhone(None)))
    def requestWithNoBillingCountry = someRequest.copy(creditCard = someCreditCard.withBillingAddress(_.withCountryCode(None)))
    def requestWithNoCardHolderName = someRequest.copy(creditCard = someCreditCard.withHolderName(None))
    def requestWithNoCsc = someRequest.copy(creditCard = someCreditCard.withCsc(None))
    def requestFromMexico = someRequest.copy(creditCard = someCreditCard.withBillingAddress(_.withCountryCode(new Locale("", "MX"))))
    def requestFromMexicoWithNoCardHolderName = someRequest.copy(creditCard = requestFromMexico.creditCard.withHolderName(None))
    def requestFromGermany = someRequest.copy(creditCard = someCreditCard.withBillingAddress(_.withCountryCode(Locale.GERMANY)))
  }
}
