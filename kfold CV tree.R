#CV Tree 
CVtree<-function(k,dat,y){
  
  size=dim(dat)[1]
  ss=size/k
  s=sample(1:size,size) #shuffling
  index=1
  #extract names from fit 
  s1<- paste(deparse(substitute(y)),"~.")
  s2<-paste("te$",deparse(substitute(y)),sep="")
  mspe<-(1:k)
  for(i in 1:k){
    end=index+ss-1
    spl=s[index:end]
    te<-dat[spl,]
    tr<-dat[-spl,]
    tree.control(length(tr),mindev=0, minsize= 2)
    t=tree(as.formula(s1), data=tr, split='deviance')
    a<-cv.tree(t,K=k)
    ko<-(a$k)[which.min(a$dev)]
    t.prun<-prune.tree(t,k=ko)
    cat('opitmal prunning prarmeter');print(ko)
    fx = predict(t.prun, new = te)
    te.y<-(eval(parse(text=s2)))
    m=((te.y-fx)^2)
    mspe=c(mspe,m)
    index=i*ss+1
  }
  cat('k=',k)
  cat('opitmal prunning prarmeter');print(ko)
  return((mean(mspe)))
}