#' Produce a single 'Spaghetti' dataset from many similar-format input files


spaghetti<-function(datalist,...)
{
filenum=length(datalist$data)
outdat=interpol(datalist$data[[1]],...)
for (a in 2:filenum) outdat=rbind(outdat,interpol(datalist$data[[a]],...))

return(outdat)
}