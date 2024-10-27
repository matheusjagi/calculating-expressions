import scala.annotation.tailrec

sealed trait SetExpr:
    def evaluate(sets: Map[String, Set[Any]], universal: Set[Any]): Set[Any]
    
case class SetVar(name: String) extends SetExpr:
    def evaluate(sets: Map[String, Set[Any]], universal: Set[Any]): Set[Any] =
        sets.getOrElse(name, Set.empty)

case class Union(left: SetExpr, right: SetExpr) extends SetExpr:
    def evaluate(sets: Map[String, Set[Any]], universal: Set[Any]): Set[Any] =
        left.evaluate(sets, universal) union right.evaluate(sets, universal)

case class Intersection(left: SetExpr, right: SetExpr) extends SetExpr:
    def evaluate(sets: Map[String, Set[Any]], universal: Set[Any]): Set[Any] =
        left.evaluate(sets, universal) intersect right.evaluate(sets, universal)

case class Difference(left: SetExpr, right: SetExpr) extends SetExpr:
    def evaluate(sets: Map[String, Set[Any]], universal: Set[Any]): Set[Any] =
        left.evaluate(sets, universal) diff right.evaluate(sets, universal)

case class SymmetricDifference(left: SetExpr, right: SetExpr) extends SetExpr:
    def evaluate(sets: Map[String, Set[Any]], universal: Set[Any]): Set[Any] =
        (left.evaluate(sets, universal) diff right.evaluate(sets, universal)) union
        (right.evaluate(sets, universal) diff left.evaluate(sets, universal))

case class CartesianProduct(left: SetExpr, right: SetExpr) extends SetExpr:
    def evaluate(sets: Map[String, Set[Any]], universal: Set[Any]): Set[Any] =
        left.evaluate(sets, universal).flatMap(source => right.evaluate(sets, universal).map(target => (source, target)))

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