package com.wix.pay.dlocal

import org.specs2.matcher.Matchers
import org.specs2.mutable.SpecWithJUnit
import org.specs2.specification.Scope

class DLocalSaleRequestTest extends SpecWithJUnit with Matchers {

  "dLocal sale request validation" should {

    "fail if invoice id is missing" in new ctx {
      requestWithNoInvoiceId must failWithMissingField("Invoice Id")
    }

    "fail if description is missing" in new ctx {
      requestWithNoDealDescription must failWithMissingField("Deal Description")
    }

    "fail if email is missing" in new ctx {
      requestWithNoCustomerEmail must failWithMissingField("Customer Email")
    }

    "not fail if optional phone is missing" in new ctx {
      requestWithNoCustomerPhone must notFail
    }

    "fail if billing country is missing" in new ctx {
      requestWithNoBillingCountry must failWithMissingField("Billing Country")
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

    "contain x_country" in new ctx {
      someRequest.fields must havePair("x_country" -> someCreditCard.billingAddressDetailed.get.countryCode.get.getCountry)
    }

    "contain x_cpf" in new ctx {
      someRequest.fields must havePair("x_cpf" -> someCreditCard.additionalFields.get.publicFields.get.holderId.get)
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

    "contain x_merchant_id" in new ctx {
      someRequest.fields must havePair("x_merchant_id" -> merchant.merchantId)
    }

    "contain x_sub_code" in new ctx {
      someRequest.fields must havePair("x_sub_code" -> merchant.subCode)
    }
  }

  trait ctx extends Scope with DLocalTestSupport {

    val someRequest = DLocalSaleRequest(merchant, someCreditCard, somePayment, Some(someCustomer), Some(someDeal))

    def requestWithNoInvoiceId = someRequest.copy(deal = Some(someDeal.copy(invoiceId = None)))
    def requestWithNoDealDescription = someRequest.copy(deal = Some(someDeal.copy(description = None)))
    def requestWithNoCustomerEmail = someRequest.copy(customer = Some(someCustomer.copy(email = None)))
    def requestWithNoCustomerPhone = someRequest.copy(customer = Some(someCustomer.copy(phone = None)))
    def requestWithNoBillingCountry = someRequest.copy(creditCard = someCreditCard.copy(additionalFields = None))
  }
}
