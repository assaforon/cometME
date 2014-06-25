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
#' First column is assumed to be time stamp
#' @param dat a data structure produced by \code{\link{batchRead}}
#' @param invars character vector with names of variables (=columns) to be read from the data. Excludes the first column which is assumed to contain the time stamp, and is read in any case
#' @param outvars character vector with names to replace the names in \code{invar}. If \code{NULL} (default), names will be left unchanged.
#' @param targInterval numeric, the time interval to be used for the output dataset, in minutes.
 
interpol<-function(dat,invars=c("Battery.Voltage","Wind.Speed","RPM"),outvars=NULL,targInterval=10)
{
require(lubridate)

if(is.null(outvars)) outvars=invars
if(length(outvars)!=length(invars)) stop("Variable names vectors have different lengths.\n")

dout=NULL
invars=c(names(dat)[1],invars)
outvars=c("Timestamp",outvars)

## Copying a "clean", "narrow" subset that has no critical missing entries
dat0=dat[complete.cases(dat[,invars]),invars]
edges=range(dat0[,1])
outtimes=seq(edges[1],edges[2],by=60*targInterval)

if(all(outtimes %in% dat0[,1]))
### All Clear!
{
	dout=dat0[dat0[,1] %in% outtimes,]
	names(dout)=outvars
} else {
	### interpolation
	dout=data.frame(Timestamp=outtimes)
	for (a in 2:length(invars))  dout=cbind(dout,approx(dat[,1],dat[,invars[a]],xout=dout$Timestamp)$y)
	names(dout)=outvars
}
names(dout)[1]="Timestamp"
return(dout[order(dout$Timestamp),])		
}
#

