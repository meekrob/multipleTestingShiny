discrim =  function(df,alpha=0.05,fun=function(x) {1-pnorm(x)}) { 

  
  res = data.frame();
  for (p.adjust.method in p.adjust.methods) {
      padj = p.adjust(fun(df[[1]]), method=p.adjust.method);
      P=sum(padj < alpha);
      N=sum(padj >= alpha);
      TP=sum(padj[df$real==T] < alpha);
      FP=sum(padj[df$real==F] < alpha);
      TN=sum(padj[df$real==F] >= alpha);
      FN=sum(padj[df$real==T] >= alpha);
      FPrate=FP/(FP+TP);
      FNrate=FN/(TN+FN);
      adj.critical.value = min(df[ padj < alpha,1]);
      rdf = data.frame(method=p.adjust.method,P,N,FPrate,FNrate,TP,FP,TN,FN,adj.critical.value);
      res=rbind(res,rdf);
  }
  return(res);
  
  
  
  if (FALSE) {
    positives=sum(fun(df[,1]) < alpha);
    negatives=sum(fun(df[,1]) >= alpha);
    TP=sum(fun(df[df$real==T,1]) < alpha);
    FP=sum(fun(df[df$real==F,1]) < alpha);
    TN=sum(fun(df[df$real==T,1]) >= alpha);
    FN=sum(fun(df[df$real==F,1]) >= alpha);
    mx=matrix(c(TP,TN,FP,FN), ncol = 2, byrow = T)
    rownames(mx) <- c('true','false')
    colnames(mx) <- c('positive','negative')
  padj = p.adjust(fun(df[[1]]), method='BY')
  df = cbind(df, padj)
  TP=sum(df[df$real==T,3] < alpha);
  FP=sum(df[df$real==F,3] < alpha);
  TN=sum(df[df$real==T,3] >= alpha);
  FN=sum(df[df$real==F,3] >= alpha);
  padj.mx=matrix(c(TP,TN,FP,FN), ncol = 2, byrow = T)
  rownames(padj.mx) <- c('true','false')
  colnames(padj.mx) <- c('positive','negative')
  padj.FPrate=FP/(FP+TP)
  padj.FNrate=FN/(TN+FN)
  
  return(list(mx=mx, 
              FPrate=FPrate,  
              FNrate=FNrate,
              padj.mx=padj.mx,
              padj.FPrate=padj.FPrate,
              padj.FNrate=padj.FNrate,
              #p=fun(df[[1]]),
              #padj=padj,
              N=nrow(df) 
              ))
  }
  }
