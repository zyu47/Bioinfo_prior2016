##codes adapted from original paper
## Load libraries 

library(ks)

## Define calculation space 

xlim2 <- c(0,440)
ylim2 <- c(0,483)
xlim <- c(0,440)
ylim <- c(0,483)
lwd <- 3
xmin <- -c(400,400,100)
xmax <-  c(400,400,100)

## Read in data files 

nuc<-read.table("/Users/Zhixian/Desktop/LUKE/nuc.txt")
ctr<-read.table("/Users/Zhixian/Desktop/LUKE/ctr.txt")
cd63<-read.table("/Users/Zhixian/Desktop/LUKE/cd63.txt")
fhat2d.nuc<-kde(nuc)
fhat2d.ctr<-kde(ctr)
fhat2d.cd63<-kde(cd63)

## Create 2D plots
## 2D plotting functions

crossbow.3d <- array(0, dim=c(350, 350, 32))
crossbowt.3d <- crossbow.3d

contour.crossbow.2d <- function()
{
	x.cross <- tail(seq(0, 300, length=351), -1)
	y.cross <- tail(seq(0, 300, length=351), -1)
	contour(x.cross, y.cross, crossbowt.3d[,,5], drawlabels=FALSE, lwd=4, col="grey", cex.lab=1.4, cex.axis=1.4, asp=1, xlim=xlim2, ylim=ylim2, xlab="x", ylab="y")
	##lines(c(-100-10000/64.5, -100), c(-300,-300), lwd=lwd)
	##text(-100-10000/64.5/2,-300, expression(10*" "*mu*m), pos=3, cex=1.4)
}

## 2D plots

##line style
pdf(file="test.pdf")
##par(mar=c(4,4,1,1), cex.main=1.4, lwd=lwd)
contour.crossbow.2d()
plot(fhat2d.cd63, cont=25, col="red", add=TRUE,lwd=0.5, drawlabel=FALSE)
plot(fhat2d.cd63, cont=50, col="red", add=TRUE,lwd=1.5, drawlabel=FALSE)
plot(fhat2d.cd63, cont=75, col="red", add=TRUE,lwd=2.5, drawlabel=FALSE)
plot(fhat2d.nuc, cont=25, col="blue", add=TRUE, lwd=0.5,drawlabel=FALSE)
plot(fhat2d.nuc, cont=50, col="blue", add=TRUE, lwd=1.5,drawlabel=FALSE)
plot(fhat2d.nuc, cont=75, col="blue", add=TRUE, lwd=2.5,drawlabel=FALSE)
plot(fhat2d.ctr, cont=25, col="green", add=TRUE,lwd=0.5, drawlabel=FALSE)
plot(fhat2d.ctr, cont=50, col="green", add=TRUE,lwd=1.5, drawlabel=FALSE)
plot(fhat2d.ctr, cont=75, col="green", add=TRUE,lwd=2.5, drawlabel=FALSE)
##plot(fhat2d.nuc, cont=60, col="blue", add=TRUE, lwd=1,drawlabel=FALSE)
##plot(fhat2d.ctr, cont=60, col="green", add=TRUE,lwd=1, drawlabel=FALSE)
##plot(fhat2d.cd63, cont=60, col="red", add=TRUE, lwd=1,drawlabel=FALSE)
##plot(fhat2d.od.centrosome, cont=80, col="purple", add=TRUE, drawlabel=FALSE)
##plot(fhat2d.od.centrosome, cont=100, col="dark blue", add=TRUE, drawlabel=FALSE)
legend("bottomright", title=NULL, legend=c("N", "OD"), lwd=2, col=c("blue", "green"), bty="n", cex=1.2)
dev.off()


##filled style
pdf(file="EB1 OD.pdf")
##par(mar=c(4,4,1,1), cex.main=1.4, lwd=lwd)
contour.crossbow.2d()
plot(fhat2d.cd63, cont=75, col=c(NA,rgb(1,0,0)), add=TRUE,display="filled.contour2",drawlabel=FALSE)
plot(fhat2d.cd63, cont=50, col=c(NA, rgb(1,0.4,0.4)), add=TRUE,display="filled.contour2",drawlabel=FALSE)
plot(fhat2d.cd63, cont=25, col=c(NA,rgb(1,0.65,0.65)), add=TRUE,display="filled.contour2", drawlabel=FALSE)
plot(nnnuc2d, cont=75, col=c(NA,rgb(0,0,1)), add=TRUE,display="filled.contour2" ,drawlabel=FALSE)
plot(nnnuc2d, cont=50, col=c(NA,rgb(0.3,0.3,1)), add=TRUE, display="filled.contour2",drawlabel=FALSE)
plot(nnnuc2d, cont=25, col=c(NA,rgb(0.65,0.65,1)), add=TRUE, display="filled.contour2",drawlabel=FALSE)
##plot(fhat2d.nuc, cont=60, col="blue1", add=TRUE,display="filled.contour2" ,drawlabel=FALSE)
plot(fhat2d.ctr, cont=75, col=c(NA,rgb(0,1,0)), add=TRUE,display="filled.contour2",drawlabel=FALSE)
plot(fhat2d.ctr, cont=50, col=c(NA,rgb(0.5,1,0.5)), add=TRUE,display="filled.contour2",drawlabel=FALSE)
plot(fhat2d.ctr, cont=25, col=c(NA,rgb(0.7,1,0.7)), add=TRUE,display="filled.contour2", drawlabel=FALSE)
##plot(fhat2d.ctr, cont=60, col="green1", add=TRUE,display="filled.contour2", drawlabel=FALSE)
##plot(fhat2d.cd63, cont=60, col="red1", add=TRUE, drawlabel=FALSE)
##plot(fhat2d.od.centrosome, cont=80, col="purple", add=TRUE, drawlabel=FALSE)
##plot(fhat2d.od.centrosome, cont=100, col="dark blue", add=TRUE, drawlabel=FALSE)
legend("bottomright", title="EB1 N", legend=c("nucleus", "centrosome","CD63"), pch=19, col=c("blue", "green", "red"), bty="n", cex=1.2)
dev.off()
