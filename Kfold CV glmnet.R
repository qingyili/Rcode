#cvRidge 
CVglmnet<-function(k,x,y,alp){
  size=dim(xx)[1]
  ss=size/k
  mspe<-(1:k)
  s=sample(1:size,size) #shuffling
  index=1
  for(i in 1:k){
    end=index+ss-1
    spl=s[index:end]
    x.te<-as.matrix(x[spl,])
    x.tr<-as.matrix(x[-spl,])
    y.te<-as.matrix(y[spl])
    y.tr<-as.matrix(y[-spl])
    
    tmp <- cv.glmnet(x=x.tr, y=y.tr,nfolds=7, alpha=alp)
    lo=tmp$lambda.min
    fit.best<- glmnet(x=x.tr, y=y.tr, lambda=lo,family='gaussian',alpha=alp)
    yhat = predict(fit.best, newx=x.te)
    m=((y.te-yhat)^2)
    mspe=c(mspe,m)
    index=i*ss+1
  }
  return((mean(mspe)))
}


