ggfy <- function(obj, ...) {
	p <- par(c("mar","mgp","tck","cex"))
	par(mar=c(3,3,2,1), mgp=c(2,.7,0), tck=-.01, cex = 0.7)
	plot(obj, ...)
	u <- par("usr")
	rect(u[1], u[3], u[2], u[4], col="#ebebeb", border="#ebebeb")
	grid(lty=1, col="white")
	par(new=TRUE)
	plot(obj, axes=FALSE, ...)
	par(p)
	invisible(NULL)
}