
import PhraseType._

import scala.language.implicitConversions

// countable set; representing state of the store
class World

class PairOfWorlds[W1 <: World, W2 <: World](val w1: W1, val w2: W2) extends World

// (a) extract sub-part of the store
// (b) update a sub-part of the store
abstract class Lense[W1 <: World, W2 <: World] {
  def get(w: W1): W2
  def put(w2: W2, w1: W1): W1

  def o[W3 <: World](l2: Lense[W3, W1]): Lense[W3, W2] = {
    val l1: Lense[W1, W2] = this
    new Lense[W3, W2] {
      override def get(w: W3): W2 = l1.get(l2.get(w))

      override def put(w2: W2, w3: W3): W3 = {
        l2.put(l1.put(w2, l2.get(w3)), w3)
      }
    }
  }
}

class idLense[W <: World] extends Lense[W, W] {
  def get(w: W) = w
  def put(w: W, `w'`: W) = w
}

class LProj1[W1 <: World, W2 <: World] extends Lense[PairOfWorlds[W1, W2], W1] {
  def get(w: PairOfWorlds[W1, W2]) = w.w1

  def put(`w1'`: W1, p: PairOfWorlds[W1, W2]) = {
    new PairOfWorlds(`w1'`, p.w2)
  }
}

class LProj2[W1 <: World, W2 <: World] extends Lense[PairOfWorlds[W1, W2], W2] {
  def get(w: PairOfWorlds[W1, W2]) = w.w2

  def put(`w2'`: W2, p: PairOfWorlds[W1, W2]) = {
    new PairOfWorlds(p.w1, `w2'`)
  }
}

class EvalOfPhraseTypes[T <: PhraseType, W <: World] {
  type Type
}

class EvalOfCommandType[W <: World] extends EvalOfPhraseTypes[CommandType, W] {
  type Type = W => W
}

class EvalOfExpType[W <: World] extends EvalOfPhraseTypes[ExpType, W] {
  type Type = W => scala.Int
}

class EvalOfAccType[W <: World] extends EvalOfPhraseTypes[AccType, W] {
  type Type = scala.Int => W => W
}

class EvalOfPairType[T1 <: PhraseType, T2 <: PhraseType, W <: World] extends EvalOfPhraseTypes[T1 x T2, W] {
  // This is most likely not working ...
  type Type = ( EvalOfPhraseTypes[T1, W]#Type, EvalOfPhraseTypes[T2, W]#Type )
}

class EvalOfFunctionType[T1 <: PhraseType, T2 <: PhraseType, W <: World] extends EvalOfPhraseTypes[T1 -> T2, W] {
//  type Type = ???
}


object Eval {
  // interpretation of phrase types on lenses
  def eval[W1 <: World, W2 <: World](comm: CommandType,
                                     l: Lense[W1, W2]): (EvalOfCommandType[W2]#Type => EvalOfCommandType[W1]#Type) = {
    (c: (W2) => W2) => {
      sigma: W1 => {
        l.put(c(l.get(sigma)), sigma)
      }
    }
  }

  def eval[W1 <: World, W2 <: World](exp: ExpType,
                                     l: Lense[W1, W2]): (EvalOfExpType[W2]#Type => EvalOfExpType[W1]#Type) = {
    (e: (W2) => Int) => {
      sigma: W1 => {
        e(l.get(sigma))
      }
    }
  }

  def eval[W1 <: World, W2 <: World](acc: AccType,
                                     l: Lense[W1, W2]): (EvalOfAccType[W2]#Type => EvalOfAccType[W1]#Type) = {
    (a: (Int) => (W2) => W2) => {
      z: Int => {
        sigma: W1 => {
          l.put( a(z)(l.get(sigma)), sigma )
        }
      }
    }
  }

//  def eval[T1 <: PhraseType,
//           T2 <: PhraseType,
//           W1 <: World,
//           W2 <: World](pair: T1 x T2,
//                        l: Lense[W1, W2]): (EvalOfPairType[T1, T2, W2]#Type => EvalOfPairType[T1, T2, W1]#Type) = {
//    (p: (EvalOfPhraseTypes[T1, W2]#Type, EvalOfPhraseTypes[T2, W2]#Type)) => {
//      val x = p._1
//      val y = p._2
////      ( eval() , eval() )
//    }
//  }
}