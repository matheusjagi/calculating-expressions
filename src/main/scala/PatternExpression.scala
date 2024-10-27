import scala.util.matching.Regex
import scala.annotation.tailrec
import scala.io.StdIn

sealed trait SetExpr:
    def evaluate(sets: Map[String, Set[Any]], universal: Set[Any] = Set(0,1,2,3,4,5,6,7,8,9)): Set[Any]

case class SetVar(name: String) extends SetExpr:
    def evaluate(sets: Map[String, Set[Any]], universal: Set[Any]): Set[Any] =
        sets.getOrElse(name, Set.empty)

case class Union(left: SetExpr, right: SetExpr) extends SetExpr:
    def evaluate(sets: Map[String, Set[Any]], universal: Set[Any]): Set[Any] =
        left.evaluate(sets) union right.evaluate(sets)

case class Intersection(left: SetExpr, right: SetExpr) extends SetExpr:
    def evaluate(sets: Map[String, Set[Any]], universal: Set[Any]): Set[Any] =
        left.evaluate(sets) intersect right.evaluate(sets)

case class Difference(left: SetExpr, right: SetExpr) extends SetExpr:
    def evaluate(sets: Map[String, Set[Any]], universal: Set[Any]): Set[Any] =
        left.evaluate(sets) diff right.evaluate(sets)

case class SymmetricDifference(left: SetExpr, right: SetExpr) extends SetExpr:
    def evaluate(sets: Map[String, Set[Any]], universal: Set[Any]): Set[Any] =
        (left.evaluate(sets) diff right.evaluate(sets)) union
        (right.evaluate(sets) diff left.evaluate(sets))

case class CartesianProduct(left: SetExpr, right: SetExpr) extends SetExpr:
    def evaluate(sets: Map[String, Set[Any]], universal: Set[Any]): Set[Any] =
        left.evaluate(sets).flatMap(source => right.evaluate(sets).map(target => (source, target)))

case class Complement(expr: SetExpr) extends SetExpr:
    def evaluate(sets: Map[String, Set[Any]], universal: Set[Any]): Set[Any] =
        universal diff expr.evaluate(sets, universal)

case class Parts(expr: SetExpr) extends SetExpr:
    def evaluate(sets: Map[String, Set[Any]], universal: Set[Any]): Set[Any] =
        val set = expr.evaluate(sets, universal)
        setParts(set)

    @tailrec
    private def setParts(set: Set[Any], parts: Set[Any] = Set()): Set[Any] = 
        if (set.isEmpty)
            parts + Set()
        else
            val first = set.head
            val tail = set.tail
            val newParts = parts + Set(first) ++ parts.map(part => part.asInstanceOf[Set[Any]] + first)
            setParts(tail, newParts)

object SetParser:
    var tempExpressions: List[SetExpr] = Nil

    def variable(input: String): SetExpr = 
        """[A-Z]""".r.findFirstIn(input) match 
            case Some(value) => SetVar(value)
            case None => throw new IllegalArgumentException("Invalid Set")

    def parse(expression: String): SetExpr =
        parseParenthesized(if expression.contains("P(") then expression.replaceAllLiterally("P(", "(#") else expression)

    def parseParenthesized(expression: String): SetExpr =
        val parenthesized = """\(([^()]+)\)""".r

        parenthesized.findFirstMatchIn(expression) match
            case Some(result) =>
                // Resolve a subexpressão mais interna e substitui no input
                val innerExpression = parse(result.group(1))
                
                // Armazena a nova expressão temporária na lista
                val index = tempExpressions.length
                tempExpressions :+= innerExpression

                //Cria uma nova expressão trocando a expressão anteriormente resolvida para um código temporário com seu index da lista
                val newInput = expression.replace(result.group(0), s"_TEMP[$index]")

                parse(newInput)
            case None => parseNonParenthesized(expression)  

    def parseNonParenthesized(input: String): SetExpr =
        // Avalia a expressão da esquerda para a direita, priorizando apenas por ordem de aparição, criando os tokens
        val tokenized = """([A-Z]+|_TEMP\[\d+\]|[|&\-\^#~\*])""".r.findAllIn(input).toList
        parseTokens(tokenized)
    
    def parseTokens(tokens: List[String]): SetExpr =
        tokens match
            case left :: "|" :: right :: rest =>
                Union(parseSingleToken(left), parseTokens(right :: rest))
            case left :: "&" :: right :: rest =>
                Intersection(parseSingleToken(left), parseTokens(right :: rest))
            case left :: "-" :: right :: rest =>
                Difference(parseSingleToken(left), parseTokens(right :: rest))
            case left :: "^" :: right :: rest =>
                SymmetricDifference(parseSingleToken(left), parseTokens(right :: rest))
            case left :: "*" :: right :: rest =>
                CartesianProduct(parseSingleToken(left), parseTokens(right :: rest))
            case "~" :: right :: rest =>
                Complement(parseTokens(right :: rest))
            case "#" :: right :: rest =>
                Parts(parseTokens(right :: rest))
            case singleToken :: Nil => parseSingleToken(singleToken)
            case Nil => throw new IllegalArgumentException("Empty expression")
            case _ => throw new IllegalArgumentException("Empty expression")
        
    def parseSingleToken(token: String): SetExpr =
        token match
            case name if name.matches("[A-Z]") => variable(name)
            case expr if expr.startsWith("_TEMP[") =>
                // Recupera a subexpressão temporária já resolvida
                val indexExpression = expr.stripPrefix("_TEMP[").stripSuffix("]")
                tempExpressions(indexExpression.toInt)
            case _ => throw new IllegalArgumentException(s"Invalid token: $token")