import scala.io.StdIn

@main
def run(): Unit =
    var continue: Boolean = true
    var sets: Map[String, Set[Any]] = Map()

    while continue do
        println("Insira o nome do conjunto de A a Z (ou 'c' para continuar):")
        val setName = StdIn.readLine().toUpperCase()

        if setName != "C" then
            println(s"Agora insira os valores do conjunto [$setName] separados por virgula (ex.: 1,5,7):")
            val setValues: Set[Any] = StdIn.readLine().split(',').map(_.trim).toSet

            sets += (setName -> setValues)
            println(s"\nOs conjuntos ate agora sao: $sets")
        else
            println("\nInforme a expressao que deseja calcular para os conjuntos (ex.:  A | (B & C) -> Respeite os espacos eos conjuntos): ")
            val expression = StdIn.readLine()
            val result = SetParser.parse(expression).evaluate(sets)
            println(s"\nO resultado da expressao [$expression] e: $result")

            println("\n\nPara finalizar o programa digite 'q' ou ENTER para continuar")
            val quit = StdIn.readLine().toUpperCase()
            if quit == "Q" then continue = false