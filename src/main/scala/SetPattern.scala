import scala.annotation.tailrec

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