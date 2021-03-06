import scala.io.Source
import scala.util.matching.Regex
import scala.collection.mutable.ArrayBuffer
import play.api.libs.json._
import play.api.libs.functional.syntax._
import java.util.ArrayList
import java.io.IOException
import java.nio.file._
import weka.core._
import weka.core.converters.ArffSaver
import weka.classifiers.trees.J48
import weka.classifiers.meta.FilteredClassifier
import weka.classifiers.Classifier
import weka.filters.supervised.attribute.AttributeSelection
import weka.attributeSelection.{Ranker,ReliefFAttributeEval}

object EatingRecognition {
  val FrameDuration = 10 	// length of a data frame in seconds
  val GMTOffset = 3600 		// all timestamps are in GMT, add 1h for local time
  
	def main(args: Array[String]) {
	  createModel("../eating_data")
	  println(classifyUnknown("../eating_data/unlabeled/1422818452_37db67"))
	}

	def createModel(rootDir: String) {
	  val checkins = getCheckins()
	  val atts = createAttributes()
	  val instances = new Instances("eatingdata", atts, 0)
	  instances.setClassIndex(0)
	  var accFeat: List[(Double, Double, Double, Double, Double, Double, Double, Double, Double,
	      Double, Double, Double, Double, Double, Double, Double, Double, Double)] = null
	  var tempFeat: List[(Double, Double, Double, Double, Long)] = null
	  var label: String = ""
	  var category: String = null
		
		
	  val visitor = new SimpleFileVisitor[Path] {
	    override def preVisitDirectory(dir: Path, attrs: attribute.BasicFileAttributes): FileVisitResult = {
			  if (dir.getFileName.toString == "unlabeled") FileVisitResult.SKIP_SUBTREE
			  else {
			    if (dir.getFileName.toString != "eating_data") { 
			      category = null
				  val timestamp = dir.getFileName.toString.stripSuffix("_37db67").toLong + GMTOffset
				  for ((time,cat) <- checkins) 		// compare if corresponding foursquare check-in exists
				    if (timestamp-900 < time && timestamp+900 > time) category = cat
			    }
			    FileVisitResult.CONTINUE
			  }
			}
			
		override def visitFile(file: Path, attrs: attribute.BasicFileAttributes): FileVisitResult = {
			  if (file.getFileName.toString == "ACC.csv") accFeat = accFeatures(file.toAbsolutePath.toString)
			  if (file.getFileName.toString == "TEMP.csv") tempFeat = tempFeatures(file.toAbsolutePath.toString)
			  if (file.getFileName.toString == "tags.csv") label = assignLabel(file.toAbsolutePath.toString, category)
			  FileVisitResult.CONTINUE
			}
			
		override def postVisitDirectory(dir: Path, e: IOException): FileVisitResult = {
				if (dir.getFileName.toString != "eating_data") { 
				  // setting attributes one at a time would be costly when dealing with many instances
				  for (instanceValues <- featureListToAttValues(accFeat, tempFeat, label, instances))
				    instances.add(new DenseInstance(1, instanceValues))
			    }
			    FileVisitResult.CONTINUE
			}
		}
		
		Files.walkFileTree(Paths.get(rootDir), visitor)
		
		// write to ARFF file
		val as = new ArffSaver()
		as.setInstances(instances)
		as.setFile(new java.io.File("labeled_instances.arff"))
		as.writeBatch
		
		// build and save model
		val ranker = new Ranker()
		ranker.setNumToSelect(7)
		val asel = new AttributeSelection()
		asel.setSearch(ranker)
		asel.setEvaluator(new ReliefFAttributeEval())
		
		val fc = new FilteredClassifier()
		fc.setFilter(asel)
		fc.setClassifier(new J48())
		fc.buildClassifier(instances)
		SerializationHelper.write("model_j48.ser", fc)
	}
	
	
	def classifyUnknown(dir: String): String = {
	  val cls: Classifier = SerializationHelper.read("model_j48.ser").asInstanceOf[Classifier]
	  val atts = createAttributes()
	  val unlabeled = new Instances("unlabeled", atts, 0)
	  unlabeled.setClassIndex(0)
	  val labels = new ArrayBuffer[Double]
	  
	  var accFeat = accFeatures(dir + "/ACC.csv")
	  var tempFeat = tempFeatures(dir + "/TEMP.csv")
	  
	  for (instanceValues <- featureListToAttValues(accFeat, tempFeat, "?", unlabeled)) {
	    unlabeled.add(new DenseInstance(1, instanceValues))
	    labels.append(cls.classifyInstance(unlabeled.instance(unlabeled.numInstances - 1)))
	  }
	  
	  // get the string label for the most commonly assigned (double) label
	  unlabeled.classAttribute.value((labels.map(l => (l, labels.count(_ == l)))).maxBy(_._2)._1.toInt)
	}
	
	/*
	 * Weka attribute creation
	 */
	
	def createAttributes(): ArrayList[Attribute] = {
	  // class attributes
	  val featLabelNominal: ArrayList[String] = new ArrayList(4)
	  featLabelNominal.add("o1f0c") 	// restaurant non-fastfood
	  featLabelNominal.add("o1f1c")		// restaurant fastfood
	  featLabelNominal.add("o0fc0")		// at home alone
	  featLabelNominal.add("o0fc1")		// at home with company
	  val allAtt = new ArrayList[Attribute](16)
	  allAtt.add(new Attribute("label", featLabelNominal))
	  allAtt.add(new Attribute("meanX")); allAtt.add(new Attribute("meanY")); allAtt.add(new Attribute("meanZ"))
	  allAtt.add(new Attribute("varianceX")); allAtt.add(new Attribute("varianceY")); allAtt.add(new Attribute("varianceZ"))
	  allAtt.add(new Attribute("corrX")); allAtt.add(new Attribute("corrY")); allAtt.add(new Attribute("corrZ"))
	  allAtt.add(new Attribute("energyX")); allAtt.add(new Attribute("energyY")); allAtt.add(new Attribute("energyZ"))
	  allAtt.add(new Attribute("minX")); allAtt.add(new Attribute("minY")); allAtt.add(new Attribute("minZ"))
	  allAtt.add(new Attribute("maxX")); allAtt.add(new Attribute("maxY")); allAtt.add(new Attribute("maxZ"))
	  allAtt.add(new Attribute("meanTemp")); allAtt.add(new Attribute("varianceTemp")); allAtt.add(new Attribute("minTemp"))
	  allAtt.add(new Attribute("maxTemp")); allAtt.add(new Attribute("mealDuration"))
	  allAtt
	}
	
	def featureListToAttValues(
	    accFeat: List[(Double, Double, Double, Double, Double, Double, Double, Double, Double, 
	        Double, Double, Double, Double, Double, Double, Double, Double, Double)], 
	    tempFeat: List[(Double, Double, Double, Double, Long)], label: String, inst: Instances): 
	    List[Array[Double]] = {
	  for ( ((a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19),(a20,a21,a22,a23,a24)) 
	      <- (accFeat zip tempFeat) ) 
	    yield Array(inst.attribute("label").indexOfValue(label),a2,a3,a4,a5,a6,a7,a8,a9,a10,
	        a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24.toDouble)
	}
	
	/*
	 * Feature computation
	 */
	
	def accFeatures(file: String): List[(Double, Double, Double, Double, Double, Double, Double, Double, Double, 
	    Double, Double, Double, Double, Double, Double, Double, Double, Double)] = {
	  // returns [(meanx, meany, meanz, variancex, variancey, variancez, 
	  // correlationxy, correlationyz, correlationxz, energyx, energyy, energyz,
	  // minx, miny, minz, maxx, maxy, maxz)]; 
	  // one list entry = one timeframe
	  val csvf = new CSVFile(file)
	  var features = new ArrayBuffer[(Double, Double, Double, Double, Double, Double, Double, Double, Double, 
	      Double, Double, Double, Double, Double, Double, Double, Double, Double)]
	  var xi, xj, yi, yj, zi, zj = new Array[Double](FrameDuration*32)
	  var i, j = 0
	  for (row <- csvf.drop(2)) {
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
	          energy(xj), energy(yj), energy(zj), xj.reduceLeft(min), yj.reduceLeft(min), zj.reduceLeft(min),
	          xj.reduceLeft(max), yj.reduceLeft(max), zj.reduceLeft(max)))
	      xj = new Array[Double](FrameDuration*32); yj = new Array[Double](FrameDuration*32); zj = new Array[Double](FrameDuration*32)
	      j = -1
	    } else if (i==FrameDuration*32-1 && j==(FrameDuration*32)/2-1) {
	      val meanx = mean(xi, FrameDuration*32); val meany = mean(yi, FrameDuration*32); val meanz = mean(zi, FrameDuration*32)
	      features.append((meanx, meany, meanz, variance(xi, meanx), variance(yi, meany), variance(zi, meanz),
	          correlation(xi, yi, meanx, meany), correlation(yi, zi, meany, meanz), correlation(xi, zi, meanx, meanz), 
	          energy(xi), energy(yi), energy(zi), xi.reduceLeft(min), yi.reduceLeft(min), zi.reduceLeft(min),
	          xi.reduceLeft(max), yi.reduceLeft(max), zi.reduceLeft(max)))
	      xi = new Array[Double](FrameDuration*32); yi = new Array[Double](FrameDuration*32); zi = new Array[Double](FrameDuration*32)
	      i = -1
	    }
	    i += 1; j += 1
	  }
	  features.toList
	}
	
	def tempFeatures(file: String): List[(Double, Double, Double, Double, Long)] = {
	  // returns [(mean, variance, min, max, totalDuration)]
	  // one list entry = one timeframe (at 4 samples per second)
	  // totalDuration of the eating session in seconds is added to each instance
	  val csvf = new CSVFile(file)
	  val totalDuration: Int = (csvf.size-2)/4
	  var features = new ArrayBuffer[(Double, Double, Double, Double, Long)]
	  var tempi, tempj = new Array[Double](FrameDuration*4)
	  var i, j = 0
	  for (row <- csvf.drop(2)) {
	    tempi(i) = row(0).toDouble; tempj(j) = row(0).toDouble
	    if (i == (FrameDuration*4)/2-1 && j == (FrameDuration*4)/2-1) {
	      // first set of values
	      tempj = new Array[Double](FrameDuration*4)
	      j = -1
	    } else if (i==(FrameDuration*4)/2-1 && j==FrameDuration*4-1) {
	      features.append((mean(tempj, FrameDuration*4), variance(tempj, mean(tempj, FrameDuration*4)), 
	          tempj.reduceLeft(min), tempj.reduceLeft(max), totalDuration))
	      tempj = new Array[Double](FrameDuration*4)
	      j = -1
	    } else if (i==FrameDuration*4-1 && j==(FrameDuration*4)/2-1) {
	      features.append((mean(tempi, FrameDuration*4), variance(tempi, mean(tempi, FrameDuration*4)), 
	          tempi.reduceLeft(min), tempi.reduceLeft(max), totalDuration))
	      tempi = new Array[Double](FrameDuration*4)
	      i = -1
	    }
	    i += 1; j += 1
	  }
	  features.toList
	}
	
	/*
	 * Math functions
	 */
	
	def mean(vals: Array[Double], size: Int): Double = vals.reduceLeft(_ + _) / size
	
	def variance(vals: Array[Double], avg: Double): Double = vals match {
	  case vs => (0.0 /: vs) { (a,e) => a + sqr(e - avg) } / vals.size
	}
	
	def max(v1: Double, v2: Double) = if (v1 > v2) v1 else v2
	
	def min(v1: Double, v2: Double) = if (v1 < v2) v1 else v2
	
	def correlation(vals1: Array[Double], vals2: Array[Double], avg1: Double, avg2: Double): Double = 
	  (for ((v1, v2) <- (vals1 zip vals2)) yield (v1-avg1)*(v2-avg2)).reduceLeft(_ + _) /
	  	math.sqrt((for ((v1, v2) <- (vals1 zip vals2)) yield sqr(v1-avg1)*sqr(v2-avg2)).reduceLeft(_ + _))
	
	def energy(vals: Array[Double]): Double = vals.map(sqr).reduceLeft(_ + _)
	
	
	def sqr(x: Double): Double = x*x
	
	/*
	 * Class labels and Foursquare checkins
	 */

	def assignLabel(file: String, category: String): String = {
	  val fastfood_cat = "(Fast Food|Falafel|Sandwiches)"
	  var out, fastfood, company = ""
	  val csvf = new CSVFile(file)
	  if (csvf.head(1) == "1" || 
	      category != null && new Regex(fastfood_cat).findAllIn(category).length > 0) fastfood = "f1" 
	    else fastfood = "f0"
	  if (csvf.head(2) == "0") company = "c0" else if (csvf.head(2) == "1") company = "c1"
	  if (csvf.head(0) == "0") { out = "o0"; fastfood = "f" } 
	  else if (csvf.head(0) == "1" || category != null) { out = "o1"; company = "c" }
	  out + fastfood + company
	}
	
	def getCheckins(): List[(Long,String)] = {
	  // for the prototype, the url is hard-coded
	  // for a multi-user application, it is necessary to authenticate each user and use their own URLs
	  val url = new java.net.URL("https://api.foursquare.com/v2/users/self/checkins?oauth_token=PLACE_TOKEN_HERE&v=20150131")
	  val response = Source.fromInputStream(url.openStream).getLines.mkString("\n")
	  val checkins = Json.parse(response)
	  
	  (for ((c, v) <- (checkins \ "response" \ "checkins" \ "items" \\ "createdAt").map(_.toString.toLong) 
	      zip (checkins \ "response" \ "checkins" \ "items" \\ "venue").map(_ \ "categories" \\ "shortName"))
	    yield (c + GMTOffset, v(0).toString.replaceAll("\"", ""))).toList
	}
}
