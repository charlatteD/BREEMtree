GREEMtreeop<-function(data,
                      y,
                      I,
                      threshold=.0001,
                      maxiter=10){
  m <- glmer(y ~ (1 | I), family = binomial, control = glmerControl(optimizer = "bobyqa"),
             nAGQ = 10)#Mixed effect model with a random intercept
  #Generating the "pearson's residual"
  #print(summary(m))
  
  re<-as.numeric(unlist(ranef(m)$`I`))
  re1<-rep(re, c(as.vector(table(I))))
  predprob <- exp(re1)/(1+exp(re1))
  p1<-(y-predprob)/(sqrt(predprob*(1-predprob)))#"pearson's residual
  sim<-data.frame(data,p1)
  
  iterations <- 0
  oldlik<- 0
  ContinueCondition <- TRUE
  mylogLik <- numeric(maxiter)
  while(ContinueCondition & iterations<maxiter){
    iterations <- iterations+1
    #print(iterations)
    treefit1<-rpart(p1~.,data=sim[,!(names(sim) %in%  c("nodeInd"))], method = "anova", control=list(minsplit=10,cp=0.01))#CART
    cventry <- which.min(treefit1$cptable[, "xerror"])
    cpcv <- treefit1$cptable[cventry, "CP"]
    tree <- prune(treefit1, cp = cpcv)
    sim[, "nodeInd"] <- tree$where
    #print(tree)
    
    if (min(tree$where) == max(tree$where)) {
      m2 <- m 
    }
    else
    {
      #Mixed effect model with a random intercept and the node indicator
      m2 <- glmer(y ~ (1 | I)+as.factor(nodeInd), data = sim, family = binomial, control = glmerControl(optimizer = "bobyqa"),
                  nAGQ = 10)
      #print(summary(m2))
    }
    re<-as.numeric(unlist(ranef(m2)$`I`))
    re1<-rep(re, c(as.vector(table(I))))
    predprob <- exp(re1)/(1+exp(re1))
    pnew<-(y-predprob)/(sqrt(predprob*(1-predprob)))#"pearson's residual
    sim[, "p1"] <- pnew
    
    newlik<-head(logLik(m2))
    ContinueCondition <- (abs(newlik - oldlik) > threshold)
    oldlik<-newlik
    mylogLik[iterations] <- newlik
    #print(newlik)
  }
  adjtarg <- unique(cbind(tree$where, predict(m2,re.form=NA)))#re.form=NA ==>fitted values, unconditional (level-0)
  tree$frame[adjtarg[, 1], ]$yval <- adjtarg[, 2]
  #print(tree)
  mint<-min(tree$where)
  maxt<-max(tree$where)
  return(list(fit0=m,fit=m2,tree=tree,mint=mint,maxt=maxt,iterations=iterations,logLik=mylogLik[1:iterations]))
}