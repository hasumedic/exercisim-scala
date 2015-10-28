class PhoneNumber(val phoneString: String) {

  lazy val number = {
    val digitsPhone = toDigits(phoneString)

    if (isValid(digitsPhone)) trimExtraDigits(digitsPhone)
    else "0000000000"
  }

  def areaCode: String = number.take(3)

  override def toString: String =
    s"(${areaCode}) ${number.substring(3,6)}-${number.substring(6)}"

  private def isValid(phoneString: String): Boolean =
    phoneString.length == 10 || (phoneString.length == 11 && phoneString.charAt(0) == '1')

  private def toDigits(phoneString: String): String =
    phoneString.filter(char => char.isDigit)

  private def trimExtraDigits(digitsPhone: String) = {
    if (digitsPhone.length == 10) digitsPhone
    else digitsPhone.drop(1)
  }
}
