MicroMouse BFS

    MicroMouse Path Finder is a Scala project that finds the shortest path for a mouse to reach the cheese in a maze.
The maze is represented as a 2D array of integers, where 0 represents an empty cell, 1 represents a wall.
The project uses the BFS algorithm to find the shortest path from the mouse to the cheese. Although the actual
program to run the MicroMouse is not implemented, this project focuses on the path-finding algorithm only.

Goals of the Project: To solve a maze as parallel as possible.

Assumption:

  - The complete layout of the maze is known.
  - The maze is a square or any rectangle by default. Other shapes can be represented by adding walls around the maze and
    treating it as a empty rectangle area with actual maze inside.
  - Valid move is only in 4 directions: up, down, left, right (no diagonal moves).
  - Initial position of the mouse and the cheese is known and must be inside the maze.
  - The hardware has multiple cores and large memory.

Methodology:

        BFS algorithm is used to find the shortest path from the mouse to the cheese. This implementation uses
   shared memory to store frontier, visited nodes, and a map of the parent node achieved  by using synchronized blocks
   to prevent data race. This program uses parallelism to explore the frontier nodes in parallel meaning that each
   vertex in the frontier is explored by a separate thread. The program uses a thread pool to manage the threads,
   and use Await to wait for the completion of all threads in each frontier level before moving to the next level.
   Therefore, the span of this algorithm is the number of levels in the BFS tree, and the work is the number of nodes.

Implementation:

    case class Cell(x: Int, y: Int) --- used to represent the cell in the maze.

    neighbor function --- returns the neighbors of a given cell.

    parallelBFS function --- returns the BFS tree of the maze with the mouse location as the root.

        Inside the parallelBFS function, there are helper function 'expand' and 'iterate.' Frontier vertexes,
    parent map, and visited vertexes are stored in parallelBFS scope. when an expand function is called,
    it create Future for each vertex, and each Future has three tasks:

        1. Call neighbor function on their vertex and store unexplored vertexes in the next frontier in local scope.
            The local 'frontier_' is shared among all threads and synchronized.
        2. Add all valid neighbors to 'visited,' which is shared and synchronized in parallelBFS scope.
        3. Add the parent of the vertex to the parent map, which is also shared and synchronized in parallelBFS scope.

    Then, the Await is called to wait for the completion of all threads in the current level. THe frontier is updated
    with the next 'frontier_' before expand exits. The iterate function is called to check if the frontier is empty or
    the cheese is found (base case). If the base case is not met, the iterate function calls expand again.

    seqBFS function --- returns the BFS tree of the maze with the mouse location as the root in a sequential manner.

    aWayOut function --- Find the shortest path from the cheese to the mouse. But by using tail recursion, the path
    is reversed. Therefore, it returns the option of the path from the mouse to the cheese if the cheese is reachable,
    otherwise, it returns None.

Evaluation:

        We use many test cases to evaluate the performance of the parallelBFS function. Only BFS part is evaluated and
    analyzed because aWayOut can not be done in parallel, therefore its performance is irrelevant to our objective.
    The performance of the parallelBFS function is compared with the seqBFS function. Here are some of the test output.

    --------------------------------------------   The case of small maze   --------------------------------------------

    ================   Parallel BFS in Maze   ================
    BFS time = 87.916812 ms
    Parent size: 1493
    ================   Sequence BFS in Maze   ================
    BFS time = 43.542923 ms
    Parent size: 1522

    ---------------------------------   The case of large empty maze (3000 by 3000)   ----------------------------------

    ================   Parallel BFS in Maze   ================
    BFS time = 27476.798154 ms
    Parent size: 8988004
    ================   Sequence BFS in Maze   ================
    BFS time = 54404.455179 ms
    Parent size: 8988004

    ---------------------------------   The case of large empty maze (4000 by 4000)   ----------------------------------

    ================ Parallel BFS in Maze ================
    BFS time = 64620.063118 ms
    Parent size: 15984004
    ================ Sequence BFS in Maze ================
    BFS time = 178364.211065 ms
    Parent size: 15984004

    -----------------------------------   The case where the cheese is unreachable   -----------------------------------

    ================   Parallel BFS in Maze   ================
    BFS time = 90.725655 ms
    Parent size: 1522
    ================   Sequence BFS in Maze   ================
    BFS time = 44.163855 ms
    Parent size: 1522

            The best case for parallelBFSis when the maze in empty because the BFS tree is shallow; we will see a
    significant improvement in performance as shown in large maze cases (the mouse is located on top left corner and
    the cheese is located on the bottom right corner). The worst case is when the maze is full of walls because the BFS
    tree is deep (every frontier level has single vertex meaning a span of the number of nodes). In this case, the
    performance of parallelBFS is worse than seqBFS because of the overhead of creating threads and managing them.
        There is also a diminishing return when the number of threads is increased or the size of maze is small because
    the cost of managing threads is higher than the benefit of parallelism and synchronization can block the pipeline
    when too many threads are trying to access shared resources.

Conclusion:

        While parallel BFS offers substantial performance gains in ideal conditions, its efficiency is highly
    dependent on the maze structure and the associated computational overhead. Future improvements could focus on
    optimizing thread management and reducing synchronization costs to enhance performance across a wider range of
    scenarios. This project underscores the potential and challenges of applying concurrent programming techniques
    to real-world robotic applications, providing a foundation for further research and development in this field.
