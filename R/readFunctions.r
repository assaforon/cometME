##' Read sensor data, clean off excess text, and convert time stamp to date-time format

readSensor<-function(filename,alphSearch=10,...)
{
require(lubridate)
tmp=scan(filename,what='list',sep='\n',quiet=TRUE)
#cat(tmp[[1]],'\n')
numind=grep("^[0-9]",tmp)
firstInd=min(which(diff(numind)==1)) ## 1st pair of consecutive numbers
firstNum=numind[firstInd]
lastNum=max(numind)
if(firstNum<3) firstNum=3

### Identifying alphabetic entries at end of file
letterInd=grep('[A-Z]',substr(tmp,1,alphSearch))
letterInd=letterInd[letterInd>=firstNum]
if(length(letterInd)>0) lastNum=min(lastNum,min(letterInd)-1)
tmp0=tmp
tmp=read.csv(filename,skip=firstNum-2,nrow=lastNum-firstNum+1,as.is=TRUE,strip.white=TRUE,row.names=NULL,...)
tmp=tmp[!is.na(tmp[,1]),]

### In one format, actual names are 2 rows above data; let's grab this
if(substr(names(tmp)[1],1,2)=='hh')  {
	names(tmp)=strsplit(tmp0[[firstNum-2]],split=',')[[1]]
	names(tmp)=gsub('[+ /-]','.',names(tmp))
} else if(substr(names(tmp)[3],1,2)=='L1')  {
### In another format, it's at row 1
	names(tmp)=strsplit(tmp0[[1]],split=',')[[1]]
	names(tmp)=gsub('[+ /%°-]','.',names(tmp))
#	names(tmp)=gsub([:punct:],'.',names(tmp))
	names(tmp)=gsub('[[()]','',names(tmp))
	names(tmp)=gsub(']','',names(tmp))
	names(tmp)=gsub('[.][.]','.',names(tmp))
} 

if(grepl("[0-9][.][0-9]",tmp[1,1])) 
{
	tmp[,1]=parse_date_time(tmp[,1],'dmyhm')
} else if(grepl("[0-9][-][0-9]",tmp[1,1])) 
{
	tmp[,1]=parse_date_time(tmp[,1],'ymdhms')
} else {
	daytmp=strsplit(filename,"/")
	tmp[,1]=paste(substr(daytmp[[1]][length(daytmp[[1]])],1,10),tmp[,1])
	tmp[,1]=parse_date_time(tmp[,1],'ymdhm')
}
return(tmp)

}

### Batch read of multiple user-selected files via dialogue

batchRead<-function() {
require(tcltk)
require(lubridate)
fnames=tk_choose.files(caption="Select Sensor Files to Read",filters=matrix(c("CSV",".csv"),nrow=1))

dconv=lapply(fnames,readSensor)

return(list(data=dconv,filenames=fnames))
}
