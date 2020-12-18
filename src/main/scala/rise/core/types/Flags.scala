package rise.core.types

object Flags {
  /** This flag enables the type inference system to express explicit dependence
    * of type variables within dependent functions.
    *
    * The transformation is slow and interferes with advanced arithmetic simplification.
    * However, it is necessary to use Dependent Arrays properly.
    * */
  sealed abstract class ExplicitDependence
  object ExplicitDependence {
    case object On extends ExplicitDependence
    case object Off extends ExplicitDependence
  }

  sealed trait PrintTypesAndTypeHoles
  object PrintTypesAndTypeHoles {
    case object On extends PrintTypesAndTypeHoles
    case object Off extends PrintTypesAndTypeHoles
  }
}
