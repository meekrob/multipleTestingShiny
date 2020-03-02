do_t_test_on_two_rnorm_samples = function(n1,n2,mu1=0,mu2=0,sd1=1,sd2=1) {
  x1 = rnorm(n1,mu1,sd1);
  x2 = rnorm(n2,mu2,sd2);
  rval = t.test(x1,x2, alternative="greater");
   
  return(data.frame(p=rval$p.value, xbar=rval$estimate[[1]],ybar=rval$estimate[[2]]));
  
}

simulate_poisson_tests = function(k,lambda_null,lambda_alternative) {
  poisson_variates = rpois(k, lambda_alternative);
  p = ppois(poisson_variates, lambda_null,lower.tail=F);
  return(data.frame(variates=poisson_variates, p=p));
}

mix_positive_and_negative_tests = function(pos,neg) {
  df_pos = data.frame(pos, real=T);
  df_neg = data.frame(neg, real=F);
  return(rbind(df_pos, df_neg));
}

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
