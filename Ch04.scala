object Ch04:
  // def makeEven(n: Int): Int =
  //   if n % 2 == 1 then n + 1
  //   else n

  // % is remainder!
  //
  // scala> -1 % 2
  // val res5: Int = -1

  //
  //    4.1
  //    
  def makeEven(n: Int): Int =
    if n % 2 == 0 then n
    else n + 1

  //
  //    4.2
  //    
  def further(x: Double): Double =
    if x > 0 then x + 1.0
    else if x < 0 then x - 1.0
    else x

  //
  //    4.4
  //    
  def ordered(a: Int, b: Int): List[Int] =
    if b < a then List(b, a)
    else List(a, b)

  //
  //    4.9
  //    
  def makeOdd(n: Int): Int =
    n + ((n + 1) * (n + 1) % 2)

  //
  //    4.10
  //    
  def constrain(x: Int, min: Int, max: Int): Int =
    x.min(max).max(min)

  //
  //    4.11
  //    
  def firstZero(ns: List[Int]): String =
    def checkIt(ns: List[Int], ls: List[String]): String =
      if ns.isEmpty && ls.isEmpty then "none"
      else if ns.head == 0 then ls.head
      else checkIt(ns.tail, ls.tail)

    checkIt(ns, List("first", "second", "third"))

  //
  //    4.12
  //    
  def cycle(n: Int, limit: Int): Int =
    n % limit + 1

  def howAlike(a: Int, b: Int): String =
    if a == b then "the same"
    else if a % 2 == 0 && b % 2 == 0 then "both even"
//    else if a % 2 == 1 && b % 2 == 1 then "both odd"
    // else if a.abs % 2 == 1 && b.abs % 2 == 1 then "both odd"
    else if a % 2 != 0 && b % 2 != 0 then "both odd"
    else if a < 0 && b < 0 then "both negative"
    else if a > 0 && b > 0 then "both positive"
    else "not alike"

  def sameSign1(x: Double, y: Double): Boolean =
    x == 0 && y == 0 ||
    x < 0 && y < 0   ||
    x > 0 && y > 0

  def sameSign2(x: Double, y: Double): Boolean =
    if x == 0 then y == 0
    else if x < 0 then y < 0
    else if x > 0 then y > 0
    else false

  def sameSign3(x: Double, y: Double): Boolean =
    x == 0 && y == 0  ||  x * y > 0

  def sameSign4(x: Double, y: Double): Boolean =
    x.sign == y.sign

  //
  //    4.16
  //    
  def fancy(x: Int): Int =
    if x % 2 != 0 && x > 0 then x * x
    else if x % 2 != 0 && x < 0 then 2 * x
    else x / 2

  //
  //    4.17
  //
  def categorize(sex: String, age: String): Boolean =
    if sex == "boy" || sex == "girl" then age == "child"
    else if sex == "man" || sex == "woman" then age == "adult"
    else false

  //
  //    4.18
  //
  // def play(first: String, second: String): String =
  //   if first == second then "tie"
  //   else
  //     if 
  //     (first match
  //       case "rock" => second == "paper"
  //       case "paper" => second == "scissors"
  //       case "scissors" => second == "rock"
  //       case _ => false)
  //     then "second wins"
  //   else "first wins"

  def play(first: String, second: String): String =
    if first == second then "tie"
    else
      val beats = first match
                    case "rock" => "paper"
                    case "paper" => "scissors"
                    case "scissors" => "rock"

      if beats == second then "second wins"
      else "first wins"
