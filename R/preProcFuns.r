
##' @param outdat whether to return a data frame with values interpolated on a regular time sequence
##' @param targInterval  target regular time interval for output, in minutes. Ignored if \code{outdat=FALSE}.
gapMind<-function(dat,targInterval=10,outdat=TRUE)
{
require(lubridate)


diffs=as.integer(diff(dat[,1],units='mins'))
mindiff=min(diffs)
if(any(diffs%%mindiff != 0)) cat("Warning! Irregular Time Gaps. Gap report will be approximate.\n")

gaps=sum(diffs%/%mindiff-1)

dout=NULL
if(outdat)  ### returning an interpolated reqular sequence of specified resolution
{
	edges=range(dat[,1])
	outtimes=seq(edges[1],edges[2],by=60*targInterval)
	if(all(outtimes %in% dat[,1]))
	{
		dout=dat[dat[,1] %in% outtimes,outvars]
		names(dout)[1]="DateTime"
	} else {
	### interpolation
		dout=data.frame(DateTime=outtimes)
	}
		
	

return(list(mindiff=mindiff,TotalMissing=gaps,outdata=dout))
}
#

