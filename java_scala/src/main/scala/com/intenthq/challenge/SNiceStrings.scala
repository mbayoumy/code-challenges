package com.intenthq.challenge

import scala.annotation.tailrec

object SNiceStrings {

  // From http://adventofcode.com/day/5
  //  --- Day 5: Doesn't He Have Intern-Elves For This? ---
  //
  //  Santa needs help figuring out which strings in his text file are naughty or nice.
  //
  //    A nice string is one with all of the following properties:
  //
  //    It contains at least three vowels (aeiou only), like aei, xazegov, or aeiouaeiouaeiou.
  //  It contains at least one letter that appears twice in a row, like xx, abcdde (dd), or aabbccdd (aa, bb, cc, or dd).
  //    It does not contain the strings ab, cd, pq, or xy, even if they are part of one of the other requirements.
  //    For example:
  //
  //    ugknbfddgicrmopn is nice because it has at least three vowels (u...i...o...), a double letter (...dd...), and none of the disallowed substrings.
  //  aaa is nice because it has at least three vowels and a double letter, even though the letters used by different rules overlap.
  //    jchzalrnumimnmhp is naughty because it has no double letter.
  //    haegwjzuvuyypxyu is naughty because it contains the string xy.
  //    dvszwmarrgswjxmb is naughty because it contains only one vowel.
  //    How many strings are nice?


  def nice(xs: List[String]): Int = {

    @tailrec
    def hasDuplicate(input: Array[Char], currentValue: Char): Boolean = {
      if (input.isEmpty)
        false
      else if (currentValue == input.head)
        true
      else hasDuplicate(input.tail, input.head)
    }

    val vowels = List('a', 'e', 'i', 'o', 'u')
    val badStrings = List("ab", "cd", "pq", "xy")

    xs
      .filter(s => s.count(vowels.contains) >= 3)
      .filter(s => {
        val array = s.toCharArray
        hasDuplicate(array.tail, array.head)
      })
      .filterNot(s => badStrings.exists(s.contains))
      .length
  }
}

