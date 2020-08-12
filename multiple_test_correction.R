library('dplyr')

library('ggtext') # for element_markdown, to color the
                  # axis labels (the student's name) 
                  # in fiftyStudents()

library('glue')   # to interpolate a value into html via dplyr

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

fiftyStudents <- function(wt_mu, wt_sd,test_n=10, wt_n=10,alpha_reject=0.05) {
  cat("getting names\n")
  randomnames=strsplit(readLines("../randomnames.txt"), " ")[[1]]
  
  randomnames_factors = as.factor(randomnames)
  cat("test_n: ", test_n, "\nwt_n: ", wt_n,"\n")
  fp=replicate(50,
         t.test(
            rnorm(test_n, wt_mu, wt_sd),
            rnorm(wt_n,   wt_mu, wt_sd)
            )$p.value)
  cat("done replicating.\n")
  
  # https://stackoverflow.com/questions/59757585/conditional-change-of-axis-text-font-family-and-font-size-creates-unwanted-gap
  #
  
  as_tibble(fp) %>% mutate(rejectH0=value <= alpha_reject,
                           students=randomnames_factors,
                           students_markdown = ifelse(rejectH0,
                                                      glue("<span style = 'color: red;'>{randomnames}</span>"),
                                                      glue("<span style = 'color: black;'>{randomnames}</span>"))
                           ) -> allLabs
  
  plt <- ggplot(allLabs, aes(y=students_markdown,x=value,color=rejectH0))
  cat("set up aesthetic\n")
  plt <- plt + geom_rect(xmin=0,xmax=alpha_reject,ymin=0,ymax=Inf,fill='white', color='red',alpha=.1)
  cat("grid\n")
  plt <- plt + geom_point()
  cat("grid\n")
  plt <- plt +  scale_color_manual(values=c("#606060","red")) +
    theme_classic() +
    theme(
          #axis.text.y = element_text(color=ifelse(allLabs$rejectH0,'red','black'))
          axis.text.y = element_markdown()
          ) +
    geom_vline(xintercept=seq(from=.0,to=1,by=.05),size=.05) +
    labs(x="p-value", y="The 50 students") +
    scale_x_continuous(expand=c(0.01,0.01))
  print(plt)
}

fifty_students_pval_histogram <- function()
{
  if (FALSE){  # histogram of p-values
    fp=replicate(50,t.test(rnorm(10),rnorm(10))$p.value)
    as_tibble(fp) %>% mutate(rejectH0=value <= alpha_reject, 
                             students=as.factor(randomnames)) -> allLabs 
    histbreaks = seq(from=.0,to=1,by=alpha_reject)
    ggplot(allLabs, aes(x=value)) + 
      geom_histogram(data=subset(allLabs,value<=alpha_reject),breaks=histbreaks,center=0, fill="red") + 
      geom_histogram(data=subset(allLabs,value>alpha_reject),breaks=histbreaks,center=0, fill="#606060") 
  }
}

mix_positive_and_negative_tests = function(pos,neg) {
  df_pos = data.frame();
  df_neg = data.frame();
  if(! is.null(pos)) {df_pos = data.frame(pos, real=T);}
  if(! is.null(neg)) {df_neg = data.frame(neg, real=F);}
  return(rbind(df_pos, df_neg));
}

ppv = function(df, alpha=.05) {
  res = data.frame();
  for (p.adjust.method in p.adjust.methods) {
    padj = p.adjust(df$p, method=p.adjust.method);
    P=sum(padj < alpha);
    N=sum(padj >= alpha);
    TP=sum(padj[df$real==T] < alpha);
    FP=sum(padj[df$real==F] < alpha);
    TN=sum(padj[df$real==F] >= alpha);
    FN=sum(padj[df$real==T] >= alpha);
    FPrate=FP/(FP+TP);
    FNrate=FN/(TN+FN);
    
    rdf = data.frame(method=p.adjust.method,P,N,FPrate,FNrate,TP,FP,TN,FN);
    res=rbind(res,rdf);
  }
  return(res);
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
