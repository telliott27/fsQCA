#written by Thomas Elliott
#University of California, Irvine
#http://thomaselliott.me

#use the direct transformation method described in Ragin's Redesigning Social Inquiry
#to transform continuous variables into fuzzy sets
fsCalibrate<-function(x,thresholds) {
	#x = a vector of values to be fuzzified
	#thresholds = a vector of length 3 containing the threshold values in the following order: (fully out,crossover,fully in)
	
	if( length(thresholds) != 3 ) stop("You must provide three threshold values")
	dev<-x-thresholds[2]
	us<-3/(thresholds[3]-thresholds[2])
	ls<--3/(thresholds[1]-thresholds[2])
	scal<-rep(0,length.out=length(x))
	scal[which(dev>=0)]<-us
	scal[which(dev<0)]<-ls
	prod<-dev*scal
	scores<-exp(prod)/(1+exp(prod))
	return(scores)
}
