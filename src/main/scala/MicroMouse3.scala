import scala.collection.mutable
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.*
import ExecutionContext.Implicits.global
import scala.annotation.tailrec

object MicroMouse3 {

  private type Maze = Array[Array[Int]]
  private type Parent = mutable.Map[Cell, Cell]
  private type Vertexes = mutable.Set[Cell]
  private type BFSResult = (Parent, Vertexes)

  val directions: Seq[(Int, Int)] = List(
    (0, 1),  // Right
    (1, 0),  // Down
    (0, -1), // Left
    (-1, 0)  // Up
  )

  case class Cell(x: Int, y: Int)

  private def neighbors(cell: Cell, maze: Maze): Seq[Cell] = {

    def inBounds(x: Int, y: Int): Boolean = { x >= 0 && x < maze.length && y >= 0 && y < maze(0).length }

    directions.flatMap { case (x, y) =>
      val (x_, y_) = ( cell.x + x, cell.y + y )
      if (inBounds(x_, y_) && maze(x_)(y_) == 0) { Some(Cell(x_, y_)) } else { None }
    }
  }

  def parallelBFS(src: Cell, dst: Cell, maze: Maze): Parent = {

    val parent = mutable.Map[Cell, Cell]()
    val frontier = mutable.Set[Cell]()
    val visited = mutable.Set[Cell]()

    def expand(): Unit = {

      val frontier_ = mutable.Set[Cell]()

      val vertexes = frontier.map { v => Future {
        visited.synchronized { visited.add(v) }
        neighbors(v, maze).foreach { neighbor =>
          if (!visited.synchronized(visited.contains(neighbor))) {
            frontier_.synchronized { frontier_.add(neighbor) }
            visited.synchronized { visited.add(neighbor) }
            parent.synchronized { parent(neighbor) = v }
          }}}}

      Await.result(Future.sequence(vertexes), Duration.Inf)
      frontier.clear()
      frontier.addAll(frontier_)
    }

    @tailrec
    def iterate(): Parent = { if (frontier.isEmpty | parent.contains(dst)) { parent } else { expand(); iterate() } }

    parent.put(src, src)
    frontier.add(src)
    iterate()
//    while (frontier.nonEmpty) { expand() }
//    parent
  }

  def seqBFS(src: Cell, maze: Maze): Map[Cell, Cell] = {
    @tailrec
    def iterate(frontier: Set[Cell], visited: Set[Cell], parent: Map[Cell, Cell]): Map[Cell, Cell] = {
      if (frontier.isEmpty) { parent } else {
        val visited_ = visited ++ frontier
        val newParent = frontier.flatMap { v => neighbors(v, maze).filterNot(visited_).map(_ -> v) }.toMap
        iterate(newParent.keySet, visited_, parent ++ newParent)
      }
    }
    iterate(Set(src), Set(), Map(src -> src))
  }

  def aWayOut(parent: Parent, src: Cell, dst: Cell): Option[List[Cell]] = {
    @tailrec
    def path(cur: Cell, acc: List[Cell]): List[Cell] = {
      if (cur == src) { cur :: acc } else { path(parent(cur), cur :: acc) }
    }
    if (parent.contains(dst)) { Some(path(dst, List())) } else { None }
  }
}