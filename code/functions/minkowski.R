### Function minkowski()
minkowski<- function(x,y,p=1) {
  if (length(x)==length(y) & is.numeric(p)==TRUE & p>=1) {
    term<-c()
    sump<-0
    for (i in 1:length(x))
      term<-((abs(x[i]-y[i]))^p)
    sump<-sump+term
    print(sump^(1/p))
  } else if (length(x)==length(y) & is.character(p)==TRUE & p=='max') {
    maxp<-0
    for (i in 1:length(x))
      term<-(abs(x[i]-y[i]))
    maxp<-max(term, maxp)
    print(maxp)
  } else if (length(x) != length(y)) {
    stop("x and y have different lengths")
  } else if (is.numeric(p)==TRUE & p<1) {
    stop("p cannot be less than 1")
  } else if (is.character(p)==TRUE & p!='max') {
    stop("invalid character value for p")
  }
}