val map = (('a' to 'z') zip ('z' to 'a')).toMap
map.foreach(println)

val range = ('a' to 'z')
val pipi = range zip range.reverse
pipi.toMap.foreach(println)