package com.wix.pay.dlocal

import org.json4s.DefaultFormats
import org.json4s.native.Serialization

object JsonDLocalMerchantParser {

  private implicit val formats = DefaultFormats

  def parse(merchantKey: String): DLocalMerchant = {
    Serialization.read[DLocalMerchant](merchantKey)
  }

  def stringify(merchant: DLocalMerchant): String = {
    Serialization.write(merchant)
  }
}
