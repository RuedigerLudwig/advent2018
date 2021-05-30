package savinien.aoc18.day25

object Constellations:
  def mergeCoordinates(list: List[Point4D[Int]]): List[Set[Point4D[Int]]] =
    def merge(point: Point4D[Int], constellations: List[Set[Point4D[Int]]], distinct: List[Set[Point4D[Int]]], fitting: Set[Point4D[Int]]): List[Set[Point4D[Int]]] =
      constellations match
        case Nil => fitting :: distinct
        case constellation :: rest =>
          if constellation.exists(other => (other - point).absM <= 3) then merge(point, rest, distinct, fitting ++ constellation)
          else merge(point, rest, constellation :: distinct, fitting)

    def walkThrough(coordinates: List[Point4D[Int]], constellations: List[Set[Point4D[Int]]]): List[Set[Point4D[Int]]] =
      coordinates match
        case Nil => constellations
        case point :: rest =>
          val merged = merge(point, constellations, List.empty, Set(point))
          walkThrough(rest, merged)

    walkThrough(list, List.empty)