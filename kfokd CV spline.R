splineCV<-function(k,dat,y,x,d){
  #strings for generating formula 
  s1<- paste(deparse(substitute(y)),"~") #y~
  s2<-paste("te$",deparse(substitute(y)),sep="")
  sx<-deparse(substitute(x))
  sd<-deparse(substitute(d))
  s4<-paste("ns(",sx,", df=",d,")")
  s3<-paste(s1,s4) #formula for full model
  size=dim(dat)[1]
  ss=size/k
  s=sample(1:size,size) #shuffling
  index=1
  mspe<-vector()
  for(i in 1:k){
    #spliting into training and testing 
    end=index+ss-1
    spl=s[index:end]
    te<-dat[spl,]
    tr<-dat[-spl,]
    f.spline<-lm(as.formula(s3), data = tr)
    fx = predict(f.spline, new = te)
    te.y<-(eval(parse(text=s2)))
    m=((te.y-fx)^2)
    mspe=c(mspe,m)
    index=i*ss+1
  }
  return(mean(mspe))
}


