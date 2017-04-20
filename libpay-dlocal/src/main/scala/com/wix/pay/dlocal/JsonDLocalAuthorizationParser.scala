package com.wix.pay.dlocal

import org.json4s.DefaultFormats
import org.json4s.native.Serialization

object JsonDLocalAuthorizationParser  {

  private implicit val formats = DefaultFormats

  def parse(authorizationKey: String): DLocalAuthorization = {
    Serialization.read[DLocalAuthorization](authorizationKey)
  }

  def stringify(authorization: DLocalAuthorization): String = {
    Serialization.write(authorization)
  }
}
