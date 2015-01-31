import fi.foyt.foursquare.api._
import fi.foyt.foursquare.api.entities._
import java.nio.file._
import java.io.IOException
import scala.collection.mutable.ArrayBuffer
import weka.core._
import java.util.ArrayList


object EatingRecognition {
  val FrameDuration = 30 	// length of a data frame in seconds
  
	def main(args: Array[String]) {
	  // create, in the respective directory that matches the timestamp,
	  // the class.txt file for the objects retrieved from foursquare checkins
	  // matchCheckins()
	  createInstanceObjects("../eating_data")	  
	}

	def createInstanceObjects(rootDir: String) {
	  val atts = createAttributes()
	  val instances = new Instances("eatingdata", atts, 0)
	  instances.setClassIndex(0)
	  var currInst: DenseInstance = null
	  var unixTimestamp: Long = 0
	  var accFeat: List[(Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double)] = null
	  var tempFeat: List[(Double, Double, Long)] = null
	  var label: String = ""
		
		
	  val visitor = new SimpleFileVisitor[Path] {
	    override def preVisitDirectory(dir: Path, attrs: attribute.BasicFileAttributes): FileVisitResult = {
			  // set instance timestamp from name
			  if (dir.getFileName.toString == "holdout") FileVisitResult.SKIP_SUBTREE
			  else {
			    if (dir.getFileName.toString != "eating_data") { 
				  unixTimestamp = dir.getFileName.toString.stripSuffix("_37db67").toLong + 3600
				  val date = new java.util.Date(unixTimestamp * 1000)
				  println(date.toGMTString)
			    }
			    FileVisitResult.CONTINUE
			  }
			}
			
			// only care about files ACC.csv and TEMP.csv and possibly class.txt
		override def visitFile(file: Path, attrs: attribute.BasicFileAttributes): FileVisitResult = {
			  // zip together
			  if (file.getFileName.toString == "ACC.csv") accFeat = accFeatures(file.toAbsolutePath.toString)
			  if (file.getFileName.toString == "TEMP.csv") tempFeat = tempFeatures(file.toAbsolutePath.toString)
			  if (file.getFileName.toString == "tags.csv") label = assignLabels(file.toAbsolutePath.toString)
			  FileVisitResult.CONTINUE
			}
			
			// when all files in a directory have been visited (= added to index)
		override def postVisitDirectory(dir: Path, e: IOException): FileVisitResult = {
			  // mark as processed by changing directory name/moving to subdirectory
			  // create ARFF file if not existing
			  // add instance to ARFF file/instance array
				if (dir.getFileName.toString != "eating_data") { 
				  for (instanceValues <- featureListToAttValues(accFeat, tempFeat, label, instances)) {
				    // setting attributes one at a time would be costly when dealing with many instances
				    currInst = new DenseInstance(1, instanceValues)
				  }
			    }
			    FileVisitResult.CONTINUE
			}
		}
		
		Files.walkFileTree(Paths.get(rootDir), visitor)
	}
	
	/*
	 * Weka functionality
	 */
	
	def createAttributes(): ArrayList[Attribute] = {
	  // class attributes
	  val featLabelNominal: ArrayList[String] = new ArrayList(4)
	  featLabelNominal.add("o1f0c") 	// out non-fastfood
	  featLabelNominal.add("o1f1c")		// out fastfood
	  featLabelNominal.add("o0fc0")		// at home alone
	  featLabelNominal.add("o0fc1")		// at home with company
	  val a1 = new Attribute("label", featLabelNominal)
	  val a2 = new Attribute("meanx"); val a3 = new Attribute("meany"); val a4 = new Attribute("meanz")
	  val a5 = new Attribute("variancex"); val a6 = new Attribute("variancey"); val a7 = new Attribute("variancez")
	  val a8 = new Attribute("corrx"); val a9 = new Attribute("corry"); val a10 = new Attribute("corrz")
	  val a11 = new Attribute("energyx"); val a12 = new Attribute("energyy"); val a13 = new Attribute("energyz")
	  val a14 = new Attribute("meantemp"); val a15 = new Attribute("variancetemp")
	  val a16 = new Attribute("mealDuration")
	  val allAtt = new ArrayList[Attribute](16)
	  allAtt.add(a1); allAtt.add(a2); allAtt.add(a3); allAtt.add(a4); allAtt.add(a5); allAtt.add(a6); allAtt.add(a7); allAtt.add(a8); 
	  allAtt.add(a9); allAtt.add(a10); allAtt.add(a11); allAtt.add(a12); allAtt.add(a13); allAtt.add(a14); allAtt.add(a15); allAtt.add(a16)
	  allAtt
	}
	
	def featureListToAttValues(
	    accFeat: List[(Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double)], 
	    tempFeat: List[(Double, Double, Long)], label: String, inst: Instances): List[Array[Double]] = {
	  for ( ((a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13),(a14,a15,a16)) <- (accFeat zip tempFeat) ) 
	    yield Array(inst.attribute("label").indexOfValue(label),a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16.toDouble)
	}
	
	/*
	 * Feature extraction
	 */
	
	def accFeatures(file: String): List[(Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double)] = {
	  // returns [(meanx, meany, meanz, variancex, variancey, variancez, 
	  // correlationxy, correlationyz, correlationxz, energyx, energyy, energyz)]; 
	  // one list entry = one 30 second window
	  val csvf = new CSVFile(file)
	  var features = new ArrayBuffer[(Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double, Double)]
	  var xi, xj, yi, yj, zi, zj = new Array[Double](FrameDuration*32)
	  var i, j = 0
	  for (row <- csvf.drop(2)) {
	    // 960 samples per 30 seconds = new instance every 480 samples (50% overlap)
	    xi(i) = row(0).toDouble; xj(j) = row(0).toDouble
	    yi(i) = row(1).toDouble; yj(j) = row(1).toDouble
	    zi(i) = row(2).toDouble; zj(j) = row(2).toDouble
	    if (i == (FrameDuration*32)/2-1 && j == (FrameDuration*32)/2-1) {
	      // first set of values
	      xj = new Array[Double](FrameDuration*32); yj = new Array[Double](FrameDuration*32); zj = new Array[Double](FrameDuration*32)
	      j = -1
	    } else if (i==(FrameDuration*32)/2-1 && j==FrameDuration*32-1) {
	      val meanx = mean(xj, FrameDuration*32); val meany = mean(yj, FrameDuration*32); val meanz = mean(zj, FrameDuration*32)
	      features.append((meanx, meany, meanz, variance(xj, meanx), variance(yj, meany), variance(zj, meanz),
	          correlation(xj, yj, meanx, meany), correlation(yj, zj, meany, meanz), correlation(xj, zj, meanx, meanz), 
	          energy(xj), energy(yj), energy(zj)))
	      xj = new Array[Double](FrameDuration*32); yj = new Array[Double](FrameDuration*32); zj = new Array[Double](FrameDuration*32)
	      j = -1
	    } else if (i==FrameDuration*32-1 && j==(FrameDuration*32)/2-1) {
	      val meanx = mean(xi, FrameDuration*32); val meany = mean(yi, FrameDuration*32); val meanz = mean(zi, FrameDuration*32)
	      features.append((meanx, meany, meanz, variance(xi, meanx), variance(yi, meany), variance(zi, meanz),
	          correlation(xi, yi, meanx, meany), correlation(yi, zi, meany, meanz), correlation(xi, zi, meanx, meanz), 
	          energy(xi), energy(yi), energy(zi)))
	      xi = new Array[Double](FrameDuration*32); yi = new Array[Double](FrameDuration*32); zi = new Array[Double](FrameDuration*32)
	      i = -1
	    }
	    i += 1; j += 1
	  }
	  println(features.toList.length + " " + features.toList.head)
	  features.toList
	}
	
	def tempFeatures(file: String): List[(Double, Double, Long)] = {
	  // returns [(mean, variance, totalDuration)]; one list entry = one 30 second window (4 samples per seconds)
	  // totalDuration of the eating session in seconds is added to each instance
	  val csvf = new CSVFile(file)
	  val totalDuration: Int = (csvf.size-2)/4
	  var features = new ArrayBuffer[(Double, Double, Long)]
	  var tempi, tempj = new Array[Double](FrameDuration*4)
	  var i, j = 0
	  for (row <- csvf.drop(2)) {
	    // 120 samples per 30 seconds = new instance every 60 samples (50% overlap)
	    tempi(i) = row(0).toDouble; tempj(j) = row(0).toDouble
	    if (i == (FrameDuration*4)/2-1 && j == (FrameDuration*4)/2-1) {
	      // first set of values
	      tempj = new Array[Double](FrameDuration*4)
	      j = -1
	    } else if (i==(FrameDuration*4)/2-1 && j==FrameDuration*4-1) {
	      features.append((mean(tempj, FrameDuration*4), variance(tempj, mean(tempj, FrameDuration*4)), totalDuration))
	      tempj = new Array[Double](FrameDuration*4)
	      j = -1
	    } else if (i==FrameDuration*4-1 && j==(FrameDuration*4)/2-1) {
	      features.append((mean(tempi, FrameDuration*4), variance(tempi, mean(tempi, FrameDuration*4)), totalDuration))
	      tempi = new Array[Double](FrameDuration*4)
	      i = -1
	    }
	    i += 1; j += 1
	  }
	  println(features.toList.length + " " + features.toList.head)
	  features.toList
	}
	
	/*
	 * Math functions
	 */
	
	def mean(vals: Array[Double], size: Int): Double = vals.reduceLeft(_ + _) / size
	
	def variance(vals: Array[Double], avg: Double): Double = vals match {
	  case vs => (0.0 /: vs) { (a,e) => a + sqr(e - avg) } / vals.size
	}
	
	def correlation(vals1: Array[Double], vals2: Array[Double], avg1: Double, avg2: Double): Double = {
	  (for ((v1, v2) <- (vals1 zip vals2)) yield (v1-avg1)*(v2-avg2)).reduceLeft(_ + _) /
	  	math.sqrt((for ((v1, v2) <- (vals1 zip vals2)) yield sqr(v1-avg1)*sqr(v2-avg2)).reduceLeft(_ + _))
	}
	
	def energy(vals: Array[Double]): Double = vals.map(sqr).reduceLeft(_ + _)
	
	def matchCheckins() { 
	  getCheckins()
	}
	
	def sqr(x: Double): Double = x*x
	
	/*
	 * Class labels and Foursquare checkins
	 */
	
	def assignLabels(file: String): String = {
	  val csvf = new CSVFile(file)
	  csvf.head(0)
	  "o0fc0"
	}
	
	def getCheckins()/*: Array[FsCheckin] =*/ {
	  var f = new FoursquareApi("", "", "https://github.com/v0idness/")
	  f.setVersion("20150113")
	  var result = f.venuesTrending("44.3,37.2", 50, 2000)
	  println(result.getMeta.getCode + "\n" + result.getMeta.getErrorType + "\n" + result.getMeta.getErrorDetail)
	  println(result.getResult.length)
	  for (venue <- result.getResult) println(venue.getName)
	  // return all of the user's foursquare checkins
	  /*
	   *var checkins = new Array[FsCheckin](1)
	   checkins(0) = new FsCheckin
	   return checkins
	   */
	}
}