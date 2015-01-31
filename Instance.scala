class Instance {
	var classif: String = null
	var duration: Int = 0
	var timestamp: Long = 0
	
	var acc_x_mean: Double = 0.0
	var acc_x_stddev: Double = 0.0
	var acc_x: Double = 0.0
	
	def setClassif(c: String) = this.classif = c
	def setDuration(d: Int) = this.duration = d
	def setTimestamp(t: Long) = this.timestamp = t*1000	// sec to millisec conversion
	
	def setFeatures {
	  
	}
}