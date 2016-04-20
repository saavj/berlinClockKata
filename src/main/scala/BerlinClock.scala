package com.sofiaa.clock

object BerlinClock {

  def convertToBerlinTime(time: String): Array[String] = {

    val sections = time.split(":")
    Array(
      seconds       (sections(2).toInt),
      topHours      (sections(0).toInt),
      bottomHours   (sections(0).toInt),
      topMinutes    (sections(1).toInt),
      bottomMinutes (sections(1).toInt)
    )
  }

  def seconds(int: Int): String = if (int % 2 == 0) "Y" else "O"

  def topHours(int: Int): String = inner(4, inner3(int), "R")

  def bottomHours(int: Int): String = inner(4, inner2(int), "R")

  def topMinutes(int: Int): String = inner(11, inner3(int),  "Y").replaceAll("YYY", "YYR")

  def bottomMinutes(int: Int): String = inner(4, inner2(int), "Y")

  def inner(lamps: Int, noOn: Int, onSign: String): String = {
    onSign * noOn + ("O"  * (lamps - noOn))
  }

  def inner2(int: Int): Int = {
    int % 5
  }

  def inner3(int: Int): Int = {
    int / 5
  }
}
