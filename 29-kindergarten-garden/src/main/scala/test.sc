//val input = "RC\nGG"
val input = "VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV"
val rows = input.split( """\n""")
val zipped = rows(0).grouped(2) zip rows(1).grouped(2)
val pepe = for(tuple <- zipped) yield tuple._1 + tuple._2
pepe.foreach(println)

