import scala.collection.mutable.{ Map => MutableMap }

object MapCombine {

  type MyMap[K, V] = Either[MutableMap[K, V], Map[K, V]]

  def apply[K, V](m1: MyMap[K, V], m2: MyMap[K, V], merge: (V, V) => V): MyMap[K, V] = {
    (m1, m2) match {
      case (null, null) => Right(Map())
      case (null, something@_) => something
      case (something@_, null) => something
      case (Right(m1), Right(m2)) => Right(m1 ++ m2.par.map({ case (k,v) => (k, m1.get(k).map(merge(_,v)).getOrElse(v)) }))
      case (Left(m1), Left(m2)) => Left(m1 ++ m2.par.map({ case (k,v) => (k, m1.get(k).map(merge(_,v)).getOrElse(v)) }))
      case _ => throw new IllegalArgumentException("Inconsistent mutability of arguments");
    }
  }
}
