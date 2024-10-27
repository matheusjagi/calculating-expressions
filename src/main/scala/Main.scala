import scala.io.StdIn

@main
def run(): Unit =
    // val A = Set(1, 2)
    // val B = Set(3, 4, 7)
    // val C = Set(5, 6)
    // val D = Set(1, 5, 9, 13)

    // val sets: Map[String, Set[Any]] = Map("A" -> Set(1, 2), "B" -> Set(3, 4, 7), "C" -> Set(5, 6), "D" -> Set(1, 5, 9, 13))
    // val expr = "~((A & B) | (C - D))"  //-> resultado HashSet(0, 5, 1, 9, 2, 7, 3, 8, 4)
    // val expr = "A | (B & C)"
    // val expr = "A | B & C"

    val sets: Map[String, Set[Any]] = Map("A" -> Set(1,2,3), "B" -> Set(3,4,7,9))
    // val expr = "P(A) - P(B)" // -> resultado HashSet(Set(1, 3), Set(2), Set(2, 3), Set(1, 2), Set(1), Set(1, 2, 3))
    val expr = "P(A & B) ^ P(A | B)" // -> resultado HashSet(Set(9, 7, 4), Set(9), Set(4), Set(1, 9, 3), Set(1, 2, 3), Set(1, 9, 2), Set(9, 3), Set(1, 3), Set(1, 9, 3, 4), Set(1, 2, 4), Set(2, 7, 3), Set(1, 7, 4), Set(1, 7, 3, 4), Set(2), Set(9, 7, 3, 4), Set(2, 4), HashSet(1, 2, 7, 3, 4), Set(7, 3), Set(1, 9), Set(2, 3), Set(9, 7), Set(9, 2, 3, 4), Set(1, 4), Set(9, 2, 7, 3), Set(1, 3, 4), Set(1, 2, 7, 4), Set(9, 2, 7, 4), HashSet(9, 2, 7, 3, 4), Set(7), Set(1, 2, 3, 4), Set(1, 7, 3), Set(1, 2, 7, 3), HashSet(1, 9, 2, 7, 3), Set(2, 3, 4), Set(9, 2, 
                                            // 4), Set(3, 4), Set(9, 2), Set(1, 9, 7, 4), Set(9, 7, 3), Set(1, 7), Set(1, 9, 2, 3), Set(1, 9, 2, 4), Set(1, 9, 2, 7), Set(2, 7, 4), Set(1, 9, 7, 3), Set(9, 2, 3), Set(1, 2), HashSet(1, 9, 7, 3, 4), HashSet(1, 9, 2, 7, 4), Set(9, 4), Set(1, 9, 7), HashSet(1, 9, 
                                            // 2, 3, 4), Set(7, 3, 4), Set(9, 2, 7), Set(2, 7), Set(7, 4), Set(1), Set(1, 9, 4), Set(2, 7, 3, 4), HashSet(1, 9, 2, 7, 3, 4), Set(9, 3, 4), Set(1, 2, 7))

    val result = SetParser.parse(expr).evaluate(sets)
    println(s"O resultado da expressao e: $result")

    // var continue: Boolean = true
    // var sets: Map[Char, Set[String]] = Map()

    // while continue do
    //     println("Insira o nome do conjunto de A a Z (ou 's' para finalizar o programa):")
    //     val setName = StdIn.readChar()

    //     if setName != 's' then
    //         println(s"Agora insira os valores do conjunto [$setName] separados por virgula (ex.: 1,5,7):")
    //         val setValues: Set[String] = StdIn.readLine().split(',').map(_.trim).toSet

    //         sets = sets :+ (setName, setValues)
    //         println(s"Os conjuntos ate agora sao: $sets")
    //     else
    //         continue = false