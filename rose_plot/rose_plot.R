# Modified from David Finlayson
# rose -- from R-help archives
#
# Directional Vector Histogram (Rose Diagrams).
# -------------------------------------------
# David Finlayson (with help from Joerg Maeder and Ben Bolker)
# david_p_finlayson@hotmail.com
# Version 1.0
# November 23, 2001
#
# Use for plotting directional data such as wind direction or the
# angles of imbricated pebbles in rivers and streams. This is basically
# an extension of the hist(x) function though I did not implement all of
# hist. I have placed limits on the range of bins so that they always
# fall within 0 and 360 (i.e. directions of the compass). The standard
# color and line adjustment commands work as well but you will need to add
# annotation (i.e.. main, xlab, ylab) separately (see par).
#
# bins: Approximate number of bins see hist() function for details
# rscale: Ring Scale, the approximate number of rings for scaling see pretty()
# NULL value will call pretty() with default number of rings
# labels: (T/F) draw labels for the top 10% largest petals and the
#          cardinal dirctions?
# rings: (T/F) draw scale rings?
#
# example:
# test <- runif(30) * 360
# rose(test)
# rose(test, bins=10, rscale=2, labels=TRUE, rings=TRUE, col="cyan", lwd=2)
#

rose <- function(x, bins=14.4, rscale=NULL, labels=TRUE, rings=TRUE, ...){

 ### Ensure that this is directional data (0-360)
 if (max(x) > 360 || min(x) < 0) {
  stop("Data is out of range (0 - 360)")
 }

 ### Histogram Data
 histogram.out <- hist(x, breaks=seq(0,360,length=bins+1), plot=FALSE)
 pieMid <- histogram.out$mids # mid points of bins
 pieCount <- histogram.out$counts # count in each bin
 pieWidth <- 360 / length(pieMid) # width of each bin
 pieFreq <- histogram.out$density * pieWidth

 ### Initialize Plot Are
 oldpar <- par()
 par(pty="s")

 if (!is.null(rscale)){
  rscale <- pretty(pieFreq, rscale)
   } else {
  rscale <- pretty(pieFreq)
 }

 plotLimits <- c(-max(rscale), max(rscale)) * 1.04

 plot(0,0,
  ylim=plotLimits,
  xlim=plotLimits,
  axes=FALSE,
  xlab="",
  ylab="")

 abline(h=0)
 abline(v=0)

 ### Plot Rings
 if (rings == TRUE) {
  symbols(rep(0,length(rscale)), rep(0,length(rscale)),
   circles=rscale,inches=FALSE,add=TRUE,lty="dotted")
 }

 ### Plot Rose

 # Helper function to draw the pie slices
 pie <- function(h, k, direction, spread, magnitude, ...){

 # adjust coordinate from compass to polar
 direction <- 360 - direction + 90

 # calculate theta start and stop
 start <- (direction + 0.5 * spread) * pi / 180
 stop <- (direction - 0.5 * spread) * pi / 180

 # vertices
 x1 <- h
 y1 <- k

 x2 <- (magnitude * cos(start)) + h
 y2 <- (magnitude * sin(start)) + k

 x3 <- (magnitude * cos(stop)) + h
 y3 <- (magnitude * sin(stop)) + k

 # build pie slice
 x <- c(x1, x2, x3, x1)
 y <- c(y1, y2, y3, y1)

  polygon(x,y, ...)
 }

 # plot the slices
 for (i in 1:length(pieMid)){
   pie(0, 0, pieMid[i], pieWidth, pieFreq[i], col="grey",...)
 }
	
 # Plot Axes Labels

# if (labels == TRUE) {

 #    mtext("N", side=3, line=1)
  #   mtext("E", side=4, line=1)
   #  mtext("S", side=1, line=1)
    # mtext("W", side=2, line=1)

     ### Plot top frequency labels

     # calculate indices of top 10 percent of bins
   #  pie10percent <- round(length(pieMid) * 0.1) + 1 # how many is 10 percent
   #  pieRank <- length(pieCount) - rank(pieCount) # rank bins
    # top10 <- which( pieRank < pie10percent ) # index to top 10 percent

     # Plot labels for these bins
    # theta <- 360 - pieMid[top10] + 90 # compass to polar angles
  #   theta <- theta * pi/180 # degrees to rads

   #  x <- pieFreq[top10] * cos(theta)
   #  y <- pieFreq[top10] * sin(theta)

   #  text(x, y, format(pieFreq[top10], digits=2))

     ### Reset the par
     par <- oldpar

     ### Return Histogram Object
     histogram.out
 #}
}

data<-read.table("/Users/Zhixian/Desktop/LUKE/OD.txt")
mydata<-as.numeric(data$V1)
pdf(file="OD.pdf")
rose(mydata,labels=F)
dev.off()
