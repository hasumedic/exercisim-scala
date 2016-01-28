object SecretHandshake {

  type Event = String

  trait HandshakeConverter[T] {
    def convert(value: T): List[Event]
  }

  object HandshakeConverter {

    type Binary = String

    private val mapping = List(
      "wink",
      "double blink",
      "close your eyes",
      "jump"
    )

    def formatBinary(binary: Binary): List[Event] = {
      def formatNext(binary: Binary, events: List[Event], accEvents: List[Event]): List[Event] = {
        if (binary.isEmpty) accEvents
        else {
          val nextEvents = if (binary.head == '1') accEvents :+ events.head else accEvents
          formatNext(binary.tail, events.tail, nextEvents)
        }
      }

      def hasToReverseEventOrder: Boolean = {
        binary.length == 5 && (binary.head == '1')
      }

      if (hasToReverseEventOrder) formatBinary(binary.tail).reverse
      else formatNext(binary.reverse, mapping, List())
    }

    implicit object IntHandshakeConverter extends HandshakeConverter[Int] {
      def convert(value: Int): List[Event] = {
        if (value == 0) List()
        else formatBinary(value.toBinaryString)
      }
    }

    implicit object BinaryHandshakeConverter extends HandshakeConverter[Binary] {
      def convert(value: Binary): List[Event] = {
        if (isInvalidInput(value)) List()
        else formatBinary(value)
      }

      def isInvalidInput(value: Binary): Boolean = {
        value == "0" || !value.matches( """^[01]{1,5}$""")
      }
    }

  }

  def handshake[T: HandshakeConverter](value: T): List[Event] = {
    implicitly[HandshakeConverter[T]].convert(value)
  }
}