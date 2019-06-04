package idealised.SurfaceLanguage.Types

import idealised.SurfaceLanguage

sealed trait Kind {
  type T
  type I <: Kind.Identifier
}

object Kind {
  trait Identifier {
    def name: String
  }
}

sealed trait TypeKind extends Kind {
  override type T = Type
}

sealed trait DataKind extends Kind {
  override type T = DataType
  override type I = DataTypeIdentifier
}

sealed trait NatKind extends Kind {
  override type T = SurfaceLanguage.Nat
  override type I = SurfaceLanguage.NatIdentifier
}
