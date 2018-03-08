getprob1<-function( Nsubj,
                    Nobs,
                    muX,      # population mean of Xi
                    varUi,    # var of Ui's
                    varXi,    # var of Xi's
                    varXij,   # var of Xij's around Xi's
                    beta0,    # overall intercept
                    beta1,    # slope on Xi
                    beta2     # slope on xdevij
){
  ID<-seq(Nsubj)
  ID<-rep(ID, each=Nobs)
  
  Ui<-rnorm(Nsubj,0,varUi)
  Xi<-rnorm(Nsubj,muX,varXi)
  
  #Generating Xij = Xi + xdevij
  xdevij <-rnorm(Nsubj*Nobs,0,varXij)
  x <- rep(Xi, each=Nobs) + xdevij
  
  #Generating logodds/Y with beta0=X, beta1=X
  logodds<-beta0+rep(Ui, each=Nobs)+beta1*rep(Xi, each=Nobs)+beta2*xdevij
  prob<-exp(logodds)/(1+exp(logodds))
  y<-rbinom(Nsubj*Nobs,1,prob=prob)
  list(y=y,x=x,prob=prob,Ui=Ui,Xi=Xi,xdevij=xdevij,logodds=logodds,ID=ID)
}

#return root only 
getroot<-function(mint,maxt,it){
  roott<-0
  if(mint==maxt&it<=2){roott<-1}
  return(roott)
}
