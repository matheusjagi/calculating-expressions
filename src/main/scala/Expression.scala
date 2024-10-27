import scala.annotation.tailrec

//Diferença simétrica
def symetricDifference[S](setSource: Set[S], setTarget: Set[S]): Set[S] =
    (setSource -- setTarget) | (setTarget -- setSource)

//Complemento, definindo conjunto Universo como U = {0,1,2,3,4,5,6,7,8,9}
def complement[S](set: Set[S], universe: Set[S] = Set(0,1,2,3,4,5,6,7,8,9)): Set[S] =
    universe -- set

//Produto Cartesiano de dois conjuntos
def cartesianProduct[S](setSource: Set[S], setTarget: Set[S]): Set[(S, S)] =
    setSource.flatMap(source => setTarget.map(target => (source, target)))

//Conjunto das partes
@scala.annotation.tailrec
def setParts(set: Set[Any], parts: Set[Set[Any]] = Set()): Set[Set[Any]] = 
    if (set.isEmpty)
        parts + Set()
    else
        val first = set.head
        val tail = set.tail
        val newParts = parts + Set(first) ++ parts.map(_ + first)
        setParts(tail, newParts)

// val setFoldLeft = parts + Set(first) + set
        // val updateParts = tail.foldLeft(setFoldLeft)((accumulator, element) => accumulator + Set(first, element) ++ accumulator.map(_ + element))