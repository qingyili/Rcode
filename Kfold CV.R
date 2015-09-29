#======K-FOLD CV function====
CV<-function(k,f,dat,y){
  
  size=dim(dat)[1]
  ss=size/k
  s=sample(1:size,size) #shuffling
  index=1
  #extract names from fit 
  var= names(f$coefficients[-1])
  v= paste(var, collapse="+")
  s1<- paste(deparse(substitute(y)),"~")
  s2<-paste("te$",deparse(substitute(y)),sep="")
  mspe<-(1:k)
  formula=paste(s1,v)
  for(i in 1:k){
    end=index+ss-1
    spl=s[index:end]
    te<-dat[spl,]
    tr<-dat[-spl,]
    ff=lm(as.formula(formula), data =tr)
    fx = predict(ff, new = te)
    te.y<-(eval(parse(text=s2)))
    m=((te.y-fx)^2)
    mspe=c(mspe,m)
    index=i*ss+1
  }
  return((mean(mspe)))
}

