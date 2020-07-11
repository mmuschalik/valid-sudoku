import zio._

type Cell = Int
type Grid[C] = Chunk[Chunk[C]]


@main def main(): Unit = {
  
  val raw = Chunk(
    Chunk("8","3",".",".","7",".",".",".","."),
    Chunk("6",".",".","1","9","5",".",".","."),
    Chunk(".","9","8",".",".",".",".","6","."),
    Chunk("8",".",".",".","6",".",".",".","3"),
    Chunk("4",".",".","8",".","3",".",".","1"),
    Chunk("7",".",".",".","2",".",".",".","6"),
    Chunk(".","6",".",".",".",".","2","8","."),
    Chunk(".",".",".","4","1","9",".",".","5"),
    Chunk(".",".",".",".","8",".",".","7","9"))

  val transformed =
    raw.map(r => r.map(c => c.toIntOption))
  
  val answer = hasAnyDuplicates(buildCheckList(transformed, 3))
  
  println(answer)
}


def checkRows(size: Int) = 
  (0 until size)
    .map(r => (0 until size)
      .map(c => (r, c)))

def checkCols(size: Int) = 
  checkRows(size)
    .map(p => p
      .map(m => (m._2, m._1)))

def checkBox(size: Int) = 
  (0 until size)
    .flatMap(r => (0 until size)
      .map(c => (size * r, size * c)))
    .map(f => (0 until (size * size))
      .map(x => (x / size + f._1, x % size + f._2)))

def checkAll(size: Int) = 
  checkRows(size * size) ++
  checkCols(size * size) ++
  checkBox(size)

def hasDuplicates[C](cs: List[Option[C]]): Boolean =
  val removeNones = cs.flatMap(f => f.toList)
  removeNones.distinct.length != removeNones.length

def hasAnyDuplicates[C](cs: List[List[Option[C]]]): Boolean =
  cs.exists(hasDuplicates)

def buildCheckList[C](grid: Grid[C], size: Int) =
  checkAll(size)
    .map(r => r
      .map(c => grid(c._1)(c._2))
      .toList)
    .toList
