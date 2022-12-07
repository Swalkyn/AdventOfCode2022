import scala.io.Source

sealed trait FSElem:
  def size: Int
case class File(size: Int) extends FSElem
case class Dir(name: String, parent: Option[Dir], var elems: List[FSElem]) extends FSElem:
  lazy val size = elems.map(_.size).sum
  override def toString = s"Dir($name, ${parent.map(_.name).getOrElse("")}, ${elems})"

val input = Source.fromFile("src/main/resources/day7/input.txt").getLines().toList

val filePattern = "(\\d+) .+".r
val dirPattern = "dir (.+)".r
val cmdPattern = "\\$ (cd|ls) ?(.+)?".r


def parseFileSystem(terminal: List[String]): Dir =
  val root: Dir = Dir("/", None, List())

  def helper(terminal: List[String], pwd: Dir): Dir =
    println(terminal)
    terminal match
      case Nil => root
      case cmdPattern("cd", "..") :: other =>
        helper(other, pwd.parent.get)
      case cmdPattern("cd", "/") :: other =>
        helper(other, root)
      case cmdPattern("cd", name) :: other =>
        val newPwd = pwd.elems
          .collectFirst{ case d: Dir if d.name == name => d }
          .getOrElse(throw IllegalStateException(s"$name does not exist in $pwd: ${pwd.elems}"))
        helper(other, newPwd)
      case cmdPattern("ls", _) :: other =>
        val (result, more) = other.span(!_.startsWith("$"))
        pwd.elems = result.map(s => s match
          case filePattern(size) => File(Integer.parseInt(size))
          case dirPattern(name) => Dir(name, Some(pwd), List())
        )
        helper(more, pwd)
      case unexpected :: _ => throw IllegalArgumentException(s"Unexpected input: $unexpected")

  helper(terminal, root)

def findDirs(root: Dir, p: Dir => Boolean): Seq[Dir] =
  val init = if p(root) then Seq(root) else Seq()
  root.elems.collect { case d: Dir => d }.map(findDirs(_, p)).fold(init)(_ ++ _)

val fs = parseFileSystem(input)
val available = 70_000_000 - fs.size
val toFree = 30_000_000 - available

// Part 1
findDirs(fs, dir => dir.size <= 100000).map(_.size).sum

// Part 2
findDirs(fs, dir => dir.size > toFree).minBy(_.size).size
