#======K-FOLD CV stepwise function====
#using a stepwise selection on each fold of the training to correct for selection bias
#stepwise selection starting from the null model
stepCV<-function(k,dat,y){
  #strings for generating formula 
  s1<- paste(deparse(substitute(y)),"~")
  s2<-paste("te$",deparse(substitute(y)),sep="")
  s3<-paste(s1,".") #formula for full model
  s4<-paste(s1,"1") #formula for null model
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
    
    f.full=lm(as.formula(s3), data=tr)
    f.null=lm(as.formula(s4),data=tr)
    f.step.null<- stepAIC(f.null,scope=list(upper=f.full, lower = ~ 1), trace =FALSE)

    fx = predict(f.step.null, new = te)
    te.y<-(eval(parse(text=s2)))
    m=((te.y-fx)^2)
    mspe=c(mspe,m)
    index=i*ss+1
  }
  return((mean(mspe)))
}

