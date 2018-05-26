import java.util

import scala.collection.immutable.IndexedSeq

val myStream = 1 #:: 2 #:: 3 #:: 4 #:: Stream.empty

val infiniteStream: Stream[Int] = 1 #:: infiniteStream
infiniteStream.take(10).toList

IndexedSeq(1, 2, 3)

val buffer = new scala.collection.mutable.ArrayBuffer[Int]()
buffer += 1
buffer += 1


"this is a string".map(_ => true)
Array(1, 2, 3).sum

val sequence1 = new scala.collection.immutable.WrappedString("foo")
sequence1.reverse

val sequence2 = new scala.collection.immutable.StringOps("foo")
sequence2.reverse


// Java conversions
import scala.collection.JavaConverters._

val javaCollection = Seq(1, 2, 3).asJava
val list: java.util.List[Int] = new util.ArrayList[Int]()
list.asScala += 5
list


val mutable= scala.collection.mutable.ArrayBuffer(1, 2, 3)
mutable.update(0, 4)
mutable
mutable(0) = 1
mutable

val animals = Seq("cat", "dog", "penguin")
val moreAnimals = "mouse" +: animals :+ "tyrannosaurus"
moreAnimals :+ 2

val mutableAnimals = scala.collection.mutable.ArrayBuffer("cat", "dog", "penguin")
// doesn't work since types arent compatible
//mutableAnimals(0) = 2
