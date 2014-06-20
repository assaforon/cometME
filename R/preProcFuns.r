#' Identifying time gaps in a sensor dataset
##' @param outdat whether to return a data frame with values interpolated on a regular time sequence
##' @param targInterval  target regular time interval for output, in minutes. Ignored if \code{outdat=FALSE}.
gapMind<-function(dat,targInterval=10,outdat=TRUE)
{
require(lubridate)
diffs=as.integer(diff(dat[,1],units='mins'))
mindiff=min(diffs)
if(any(diffs%%mindiff != 0)) cat("Warning! Irregular Time Gaps. Gap report will be approximate.\n")

return(list(interval=mindiff,gaps=diffs%/%mindiff-1))
}

#' returning an interpolated reqular sequence of specified resolution
interpol<-function(dat,invars=c("Battery.Voltage","Wind.Speed","RPM"),outvars=c("Timestamp","Load","Windspeed","Solar"),targInterval=10)
{
require(lubridate)
dout=NULL
edges=range(dat[,1])
outtimes=seq(edges[1],edges[2],by=60*targInterval)
if(all(outtimes %in% dat[,1]))
{
	dout=dat[dat[,1] %in% outtimes,invars]
	names(dout)=outvars
} else {
	### interpolation
	dout=data.frame(Timestamp=outtimes)
	dout$Load=approx(dat[,1],dat[,invars[1]],xout=dout$Timestamp)$y
	dout$Windspeed=approx(dat[,1],dat[,invars[2]],xout=dout$Timestamp)$y
	dout$Solar=approx(dat[,1],dat[,invars[3]],xout=dout$Timestamp)$y
}
return(dout)		
}
#

