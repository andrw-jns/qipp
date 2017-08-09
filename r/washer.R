# What is washer?
# Washer is a dodgy(?) but easy to implement method of checking for outliers amongst many time series.
# There are probably tons of better ways of looking for outliers than this, but it is only used as a guide
# I would not recommend formally incorporating it - it's just a quick way to eyeball potential problems
#
###############################################################################################
## Function washer in R-language (originally washer.AV)
## Original Author : Andrea Venturini (andrea.venturini@bancaditalia.it)
## Venturini, A. (2011). Time Series Outlier Detection: A New Non Parametric Methodology 
## (Washer). Statistica 71: 329-344.
################################################################################################
washer = function( dati ) #   p      t     i     y
{          # dati structure:  phenom./date/series/values/... other
           # example:    Phenomenon     Time    Zone    Value    ...
           #             -----------  --------   --     -----  --------  
           #             Temperature  20091231   A1      20.1    ...
           #             Temperature  20091231   A2      21.0    ...
           #                             ...
           #             Rain         20081231   B1     123.0    ...
           #                             ...
###############################################################################################
AV      =  function(y) {   # y matrix 3 columns (y1 y2 y3) and n rows
                    AV=array(0,length(y[,1]))
                   100*(2*y[,2]-y[,1]-y[,3])/(median(y[,1]+y[,2]+y[,3])+ y[,1]+y[,2]+y[,3]) }
        # output array AV
###############################################################################################
test.AV =  function(AV) {  # AV array n rows
   t(rbind(test.AV=abs(AV-median(AV))/mad(AV),AV=AV,n=length(AV),median.AV=median(AV),mad.AV=mad(AV) ,
                           madindex.AV=mad(AV)*1000/150  ))     }
           # col      1      2   3        5          6         7
           # output: test / AV / n /  median(AV) / mad(AV) / madindex
################################################################################################
if (min(dati[,4])> 0) {
dati=dati[which(!is.na(dati[,4])),]
dati=dati[order(dati[,1],dati[,3],dati[,2]),]
fen=rownames( table(dati[,1]) )
nfen=length(fen)
out= NA 
for ( fi in 1:nfen) 
{ #print(c("phenomenon:",fi) ,quote=FALSE)
  time=rownames( table(dati[which(fen[fi]==dati[,1]),2]) ) 
  n=length(time)
  for ( i in 2:(n-1) ) 
   {  c1=which(as.character(dati[,2])==time[i-1] & dati[,1] == fen[fi])
      c2=which(as.character(dati[,2])==time[i  ] & dati[,1] == fen[fi])
      c3=which(as.character(dati[,2])==time[i+1] & dati[,1] == fen[fi])
      mat=matrix(0,3,max(length(c1),length(c2),length(c3))+1)
      if (length(c1) > 5)
      {
      j=1
      for ( k in 1:length(c1) )   
             {  mat[1,j]=c1[k]
                if (!is.na(match(c1[k]+1,c2)))   { mat[2,j]=c1[k]+1 
                                               if(!is.na(match(c1[k]+2,c3))) {mat[3,j]=c1[k]+2
                                                                                j=j+1 }
                                                 }
             }
      mat=mat[,which(mat[3,]!=0)]
      y=cbind(dati[mat[1,],4],
              dati[mat[2,],4],
              dati[mat[3,],4])
      
      out=rbind(out,data.frame(fen=fen[fi],t.2=time[i],
                                     series=dati[mat[2,],3],y=y,test.AV(AV(y))))
      }
    }
}
rownames(out)=(1:length(out[,1])-1)
washer=out[2:length(out[,1]),]
# col      1      2      3     4  5  6      7    8  9     10         11       12
# output: rows /time.2/series/y1/y2/y3/test(AV)/AV/ n /median(AV)/mad(AV)/madindex(AV)
# end function washer
} else print(" . . . zero or negative y:  t r a n s l a t i o n   r e q u i r e d !!!")
}


###################################################################################
#### START EXAMPLE  
###################################################################################
##
### temperature e rain mesures in 4 periods of time and in 20 geographical zones
##
##phen= rep(c(rep("Temperature",20),rep("Rain",20)),4)
##zone=rep(c('a01','a02','a03','a04','a05','a06','a07','a08','a09','a10','a11','a12',
##           'a13','a14','a15','a16','a17','a18','a19','a20') , 4 )	
##time=c( rep(1,40),rep(2,40),rep(3,40),rep(4,40) )
##value=c(2,20,25,7,16,20,17,16,4,2,20,25,7,16,20,17,16,4,22,23,1.4,7.1,2.8,10.6,0.5,
##        1.9,8.7,5.0,6.0,5.8,1.5,4.5,1.8,7.4,2.1,1.6,4.6,5.5,3.3,7.6,3,19,24,7,17,20,
##        18,16,5,3,19,24,7,17,20,18,16,5,21,23,3.4,8.7,4.8,13.5,2.6,4.5,10.7,7.5,6.6,
##        8.1,3.1,6.6,2.8,8.7,3.2,2.5,5.5,6.3,4.4,9.1,4,21,23,8,16,18,19,17,4,4,21,23,
##        8,16,18,19,17,4,20,22,2.4,7.6,3.5,10.8,1.1,2.7,9.1,6.0,6.2,6.3,1.6,5.1,2.8,
##        7.7,2.8,2.4,5.2,5.8,3.6,8.0,5,20,24,8,17,17,21,18,5,5,20,24,8,17,17,21,18,
##        5,18,21,3.3,8.0,3.9,11.1,1.9,3.4,9.3,6.6,6.8,6.7,2.5,5.7,3.7,8.5,2.9,2.7,
##        5.9,5.9,4.6,8.5)
##
##
### data frame creation : col 2 position of time variable is essential!!! 
##dati = data.frame(phen,time,zone,value)
##
#### you can read from a csv file
#### dati=read.csv2("dati esempio.csv", header = TRUE, sep = ";", dec=",")
##
####  function washer returns a matrix with results
##out=washer(dati)
####   data visualisation
##out
##
#### look at data in a "wide" matrix shape!
##reshape(dati,v.names="value",idvar=c("phen","zone"),timevar="time",direction="wide")
##
#### let's put two outliers: first for "temperature" phenomenon ...
##
######  time=3 temperature value=20
##dati[99,4]=  9
##
#### ... and after for "rain" phenomenon!
######  time=3 rain value=5.8
##dati[118,4]=  17
##
####  this analises again the time series (this time with outliers)
##out=washer(dati)
####  all "three terms" time seires
##out
#### let's take a look at anomalous time series
##out[out[,7]>5,]
##
##
#################################################################
##
#### what about an anomaluos ending value ?
#### testing is less powerfull but if we fosus on the ending values
#### we can be aware of half the warning test values (test=5 and test = 10 can became test=2.5 and test=5)
##
####  we start again from the previous situation
##dati[118,4]=  5.8
##dati[99,4] =  20
##
####  let's put an anomalus ending point 
##dati[158,4]=  1
##out=washer(dati)
##out[out[,7]>5,]
##
###################################################################################
#### END EXAMPLE  
###################################################################################


