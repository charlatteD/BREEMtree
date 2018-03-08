getauc1<-function(Ui,beta1,beta2,sub){
  
    out1<-getprob1(100,10,1,Ui,1,1,1,beta1,beta2)
    sim3<-data.frame(out1$x,out1$y,out1$ID,rep(out1$Xi,each=10),out1$xdevij,sub)
    colnames(sim3)<-c("xij","y","ID","xi","dxij","sub")
    sim_train2<-data.frame(sim3[sim3$sub==T,]$xij)
    sim_test2<-data.frame(sim3[sim3$sub==F,]$xij)
    colnames(sim_train2)<-c("xij")
    colnames(sim_test2)<-c("xij")
    y_train<-sim3[sim3$sub==T,]$y
    y_test<-sim3[sim3$sub==F,]$y
    ID_train<-sim3[sim3$sub==T,]$ID
    ID_test<-sim3[sim3$sub==F,]$ID
    
    #GREEMtree
    fit1<-tryCatch(GREEMtreeop(sim_train2,y_train,ID_train),
                   error=function(e) return(NA), 
                   warning=function(w) return(NA))
    GREEM_mint1<-tryCatch(fit1$mint,
                          error=function(e) return(NA), 
                          warning=function(w) return(NA))
    
    GREEM_maxt1<-tryCatch(fit1$maxt,
                          error=function(e) return(NA), 
                          warning=function(w) return(NA))
    
    GREEM_iter1<-tryCatch(fit1$iterations,
                          error=function(e) return(NA), 
                          warning=function(w) return(NA))
    GREEM_root1<-tryCatch(getroot(GREEM_mint1,GREEM_maxt1,GREEM_iter1),
                             error=function(e) return(NA), 
                             warning=function(w) return(NA))
    
    GREEM_train_pred1 <- tryCatch(predict(fit1$tree,data=sim_train2)+rep(unlist(ranef(fit1$fit)),c(as.vector(table(ID_train)))),
                                  error=function(e) return(NA), 
                                  warning=function(w) return(NA))
    GREEM_train_predprob1 <- tryCatch(exp(GREEM_train_pred1)/(1+exp(GREEM_train_pred1)),
                                      error=function(e) return(NA), 
                                      warning=function(w) return(NA))
    GREEM_train_auc1<-tryCatch(auc(y_train,GREEM_train_predprob1),
                                  error=function(e) return(NA), 
                                  warning=function(w) return(NA))
    
    
    GREEM_test_pred1 <- tryCatch(predict(fit1$tree,newdata=sim_test2)+rep(unlist(ranef(fit1$fit)),c(as.vector(table(ID_test)))),
                                 error=function(e) return(NA), 
                                 warning=function(w) return(NA))
    GREEM_test_predprob1 <- tryCatch(exp(GREEM_test_pred1)/(1+exp(GREEM_test_pred1)),
                                     error=function(e) return(NA), 
                                     warning=function(w) return(NA))
    GREEM_test_auc1<-tryCatch(auc(y_test,GREEM_test_predprob1),
                                 error=function(e) return(NA), 
                                 warning=function(w) return(NA))
    
    GREEM_test_pred1v2 <- tryCatch(predict(fit1$tree,newdata=sim_test2),
                                   error=function(e) return(NA), 
                                   warning=function(w) return(NA))
    GREEM_test_predprob1v2 <- tryCatch(exp(GREEM_test_pred1v2)/(1+exp(GREEM_test_pred1v2)),
                                       error=function(e) return(NA), 
                                       warning=function(w) return(NA))
    GREEM_test_auc1v2<-tryCatch(auc(y_test,GREEM_test_predprob1v2),
                                   error=function(e) return(NA), 
                                   warning=function(w) return(NA))
    
    sim_train3<-data.frame(sim3[sim3$sub==T,]$xi,sim3[sim3$sub==T,]$dxij)
    sim_test3<-data.frame(sim3[sim3$sub==F,]$xi,sim3[sim3$sub==F,]$dxij)
    colnames(sim_train3)<-c("xi","dxij")
    colnames(sim_test3)<-c("xi","dxij")
    
    fit2<-
      tryCatch(GREEMtreeop(sim_train3,y_train,ID_train),
               error=function(e) return(NA), 
               warning=function(w) return(NA))
    GREEM_mint2<-tryCatch(fit2$mint,
                          error=function(e) return(NA), 
                          warning=function(w) return(NA))
    
    GREEM_maxt2<-tryCatch(fit2$maxt,
                          error=function(e) return(NA), 
                          warning=function(w) return(NA))
    
    GREEM_iter2<-tryCatch(fit2$iterations,
                          error=function(e) return(NA), 
                          warning=function(w) return(NA))
    GREEM_root2<-tryCatch(getroot(GREEM_mint2,GREEM_maxt2,GREEM_iter2),
                             error=function(e) return(NA), 
                             warning=function(w) return(NA))
    
    GREEM_train_pred2 <- tryCatch(predict(fit2$tree,data=sim_train3)+rep(unlist(ranef(fit2$fit)),c(as.vector(table(ID_train)))),
                                  error=function(e) return(NA), 
                                  warning=function(w) return(NA))
    GREEM_train_predprob2 <- tryCatch(exp(GREEM_train_pred2)/(1+exp(GREEM_train_pred2)),
                                      error=function(e) return(NA), 
                                      warning=function(w) return(NA))
    GREEM_train_auc2<-tryCatch(auc(y_train,GREEM_train_predprob2),
                                  error=function(e) return(NA), 
                                  warning=function(w) return(NA))
    
    GREEM_test_pred2 <- tryCatch(predict(fit2$tree,newdata=sim_test3)+rep(unlist(ranef(fit2$fit)),c(as.vector(table(ID_test)))),
                                 error=function(e) return(NA), 
                                 warning=function(w) return(NA))
    GREEM_test_predprob2 <- tryCatch(exp(GREEM_test_pred2)/(1+exp(GREEM_test_pred2)),
                                     error=function(e) return(NA), 
                                     warning=function(w) return(NA))
    GREEM_test_auc2<-tryCatch(auc(y_test,GREEM_test_predprob2),
                                 error=function(e) return(NA), 
                                 warning=function(w) return(NA))
    GREEM_test_pred2v2 <- tryCatch(predict(fit2$tree,newdata=sim_test3),
                                   error=function(e) return(NA), 
                                   warning=function(w) return(NA))
    GREEM_test_predprob2v2 <- tryCatch(exp(GREEM_test_pred2v2)/(1+exp(GREEM_test_pred2v2)),
                                       error=function(e) return(NA), 
                                       warning=function(w) return(NA))
    GREEM_test_auc2v2<-tryCatch(auc(y_test,GREEM_test_predprob2v2),
                                   error=function(e) return(NA), 
                                   warning=function(w) return(NA))
    
    sim_train8<-data.frame(sim3[sim3$sub==T,]$xi)
    sim_test8<-data.frame(sim3[sim3$sub==F,]$xi)
    colnames(sim_train8)<-c("xi")
    colnames(sim_test8)<-c("xi")
    
    fit3<-
      tryCatch(GREEMtreeop(sim_train8,y_train,ID_train),
               error=function(e) return(NA), 
               warning=function(w) return(NA))
    GREEM_mint3<-tryCatch(fit3$mint,
                          error=function(e) return(NA), 
                          warning=function(w) return(NA))
    
    GREEM_maxt3<-tryCatch(fit3$maxt,
                          error=function(e) return(NA), 
                          warning=function(w) return(NA))
    
    GREEM_iter3<-tryCatch(fit3$iterations,
                          error=function(e) return(NA), 
                          warning=function(w) return(NA))
    GREEM_root3<-tryCatch(getroot(GREEM_mint3,GREEM_maxt3,GREEM_iter3),
                             error=function(e) return(NA), 
                             warning=function(w) return(NA))
    GREEM_train_pred3 <- tryCatch(predict(fit3$tree,data=sim_train8)+rep(unlist(ranef(fit3$fit)),c(as.vector(table(ID_train)))),
                                  error=function(e) return(NA), 
                                  warning=function(w) return(NA))
    GREEM_train_predprob3 <- tryCatch(exp(GREEM_train_pred3)/(1+exp(GREEM_train_pred3)),
                                      error=function(e) return(NA), 
                                      warning=function(w) return(NA))
    GREEM_train_auc3<-tryCatch(auc(y_train,GREEM_train_predprob3),
                                  error=function(e) return(NA), 
                                  warning=function(w) return(NA))
    
    GREEM_test_pred3 <- tryCatch(predict(fit3$tree,newdata=sim_test8)+rep(unlist(ranef(fit3$fit)),c(as.vector(table(ID_test)))),
                                 error=function(e) return(NA), 
                                 warning=function(w) return(NA))
    GREEM_test_predprob3 <- tryCatch(exp(GREEM_test_pred3)/(1+exp(GREEM_test_pred3)),
                                     error=function(e) return(NA), 
                                     warning=function(w) return(NA))
    GREEM_test_auc3<-tryCatch(auc(y_test,GREEM_test_predprob3),
                                 error=function(e) return(NA), 
                                 warning=function(w) return(NA))
    GREEM_test_pred3v2 <- tryCatch(predict(fit3$tree,newdata=sim_test8),
                                   error=function(e) return(NA), 
                                   warning=function(w) return(NA))
    GREEM_test_predprob3v2 <- tryCatch(exp(GREEM_test_pred3v2)/(1+exp(GREEM_test_pred3v2)),
                                       error=function(e) return(NA), 
                                       warning=function(w) return(NA))
    GREEM_test_auc3v2<-tryCatch(auc(y_test,GREEM_test_predprob3v2),
                                   error=function(e) return(NA), 
                                   warning=function(w) return(NA))
    
    sim_train9<-data.frame(sim3[sim3$sub==T,]$dxij)
    sim_test9<-data.frame(sim3[sim3$sub==F,]$dxij)
    colnames(sim_train9)<-c("dxij")
    colnames(sim_test9)<-c("dxij")
    
    fit4<-
      tryCatch(GREEMtreeop(sim_train9,y_train,ID_train),
               error=function(e) return(NA), 
               warning=function(w) return(NA))
    GREEM_mint4<-tryCatch(fit4$mint,
                          error=function(e) return(NA), 
                          warning=function(w) return(NA))
    
    GREEM_maxt4<-tryCatch(fit4$maxt,
                          error=function(e) return(NA), 
                          warning=function(w) return(NA))
    
    GREEM_iter4<-tryCatch(fit4$iterations,
                          error=function(e) return(NA), 
                          warning=function(w) return(NA))
    GREEM_root4<-tryCatch(getroot(GREEM_mint4,GREEM_maxt4,GREEM_iter4),
                             error=function(e) return(NA), 
                             warning=function(w) return(NA))
    GREEM_train_pred4 <- tryCatch(predict(fit4$tree,data=sim_train9)+rep(unlist(ranef(fit4$fit)),c(as.vector(table(ID_train)))),
                                  error=function(e) return(NA), 
                                  warning=function(w) return(NA))
    GREEM_train_predprob4 <- tryCatch(exp(GREEM_train_pred4)/(1+exp(GREEM_train_pred4)),
                                      error=function(e) return(NA), 
                                      warning=function(w) return(NA))
    GREEM_train_auc4<-tryCatch(auc(y_train,GREEM_train_predprob4),
                                  error=function(e) return(NA), 
                                  warning=function(w) return(NA))
    
    GREEM_test_pred4 <- tryCatch(predict(fit4$tree,newdata=sim_test9)+rep(unlist(ranef(fit4$fit)),c(as.vector(table(ID_test)))),
                                 error=function(e) return(NA), 
                                 warning=function(w) return(NA))
    GREEM_test_predprob4 <- tryCatch(exp(GREEM_test_pred4)/(1+exp(GREEM_test_pred4)),
                                     error=function(e) return(NA), 
                                     warning=function(w) return(NA))
    GREEM_test_auc4<-tryCatch(auc(y_test,GREEM_test_predprob4),
                                 error=function(e) return(NA), 
                                 warning=function(w) return(NA))
    
    GREEM_test_pred4v2 <- tryCatch(predict(fit4$tree,newdata=sim_test9),
                                   error=function(e) return(NA), 
                                   warning=function(w) return(NA))
    GREEM_test_predprob4v2 <- tryCatch(exp(GREEM_test_pred4v2)/(1+exp(GREEM_test_pred4v2)),
                                       error=function(e) return(NA), 
                                       warning=function(w) return(NA))
    GREEM_test_auc4v2<-tryCatch(auc(y_test,GREEM_test_predprob4v2),
                                   error=function(e) return(NA), 
                                   warning=function(w) return(NA))
    
    sim_train1<-data.frame(sim3[sim3$sub==T,]$xij,sim3[sim3$sub==T,]$y,sim3[sim3$sub==T,]$ID,sim3[sim3$sub==T,]$xi,sim3[sim3$sub==T,]$dxij)
    sim_test1<-data.frame(sim3[sim3$sub==F,]$xij,sim3[sim3$sub==F,]$y,sim3[sim3$sub==F,]$ID,sim3[sim3$sub==F,]$xi,sim3[sim3$sub==F,]$dxij)
    colnames(sim_train1)<-c("xij","y","ID","xi","dxij")
    colnames(sim_test1)<-c("xij","y","ID","xi","dxij")
    #GLM
    glm1<-glm(y~xij,family = binomial,data=sim_train1)
    GLM_train_pred1 <- predict(glm1,sim_train1,type="response")
    GLM_train_auc1<-auc(sim_train1$y,GLM_train_pred1)
    GLM_test_pred1 <- predict(glm1,newdata=sim_test1,type="response")
    GLM_test_auc1<-auc(sim_test1$y,GLM_test_pred1)
    
    glm2<-glm(y~xij+as.factor(ID),family = binomial,data=sim_train1)
    GLM_train_pred2 <- predict(glm2,sim_train1,type="response")
    GLM_train_auc2<-auc(sim_train1$y,GLM_train_pred2)
    GLM_test_pred2 <- tryCatch(predict(glm2,newdata=sim_test1,type="response"),
                               error=function(e) return(NA), 
                               warning=function(w) return(NA))
    GLM_test_auc2<-tryCatch(auc(sim_test1$y,GLM_test_pred2),
                               error=function(e) return(NA), 
                               warning=function(w) return(NA))
    
    glm3<-glm(y~xi+dxij,family = binomial,data=sim_train1)
    GLM_train_pred3 <- predict(glm3,sim_train1,type="response")
    GLM_train_auc3<-auc(sim_train1$y,GLM_train_pred3)
    GLM_test_pred3 <- predict(glm3,newdata=sim_test1,type="response")
    GLM_test_auc3<-auc(sim_test1$y,GLM_test_pred3)
    
    #GLMM
    glmm1 <-tryCatch(glmer(y~xij+(1|ID),data=sim_train1,family = binomial),
                     error=function(e) return(NA), 
                     warning=function(w) return(NA))
    GLMM_train_pred1 <- tryCatch(predict(glmm1,sim_train1,type="response"),
                                 error=function(e) return(NA), 
                                 warning=function(w) return(NA))
    GLMM_train_auc1<-tryCatch(auc(sim_train1$y,GLMM_train_pred1),
                                 error=function(e) return(NA), 
                                 warning=function(w) return(NA))
    GLMM_test_pred1 <- tryCatch(predict(glmm1,sim_test1,type="response",allow.new.levels=T),
                                error=function(e) return(NA), 
                                warning=function(w) return(NA))
    GLMM_test_auc1<-tryCatch(auc(sim_test1$y,GLMM_test_pred1),
                                error=function(e) return(NA), 
                                warning=function(w) return(NA))
    
    glmm2 <-tryCatch(glmer(y~xi+dxij+(1|ID),data=sim_train1,family = binomial),
                     error=function(e) return(NA), 
                     warning=function(w) return(NA))
    GLMM_train_pred2 <- tryCatch(predict(glmm2,sim_train1,type="response"),
                                 error=function(e) return(NA), 
                                 warning=function(w) return(NA))
    GLMM_train_auc2<-tryCatch(auc(sim_train1$y,GLMM_train_pred2),
                                 error=function(e) return(NA), 
                                 warning=function(w) return(NA))
    GLMM_test_pred2 <- tryCatch(predict(glmm2,sim_test1,type="response",allow.new.levels=T),
                                error=function(e) return(NA), 
                                warning=function(w) return(NA))
    GLMM_test_auc2<-tryCatch(auc(sim_test1$y,GLMM_test_pred2),
                                error=function(e) return(NA), 
                                warning=function(w) return(NA))
    
    glmm3 <-tryCatch(glmer(y~xi+(1|ID),data=sim_train1,family = binomial),
                     error=function(e) return(NA), 
                     warning=function(w) return(NA))
    GLMM_train_pred3 <- tryCatch(predict(glmm3,sim_train1,type="response"),
                                 error=function(e) return(NA), 
                                 warning=function(w) return(NA))
    GLMM_train_auc3<-tryCatch(auc(sim_train1$y,GLMM_train_pred3),
                                 error=function(e) return(NA), 
                                 warning=function(w) return(NA))
    GLMM_test_pred3 <- tryCatch(predict(glmm3,sim_test1,type="response",allow.new.levels=T),
                                error=function(e) return(NA), 
                                warning=function(w) return(NA))
    GLMM_test_auc3<-tryCatch(auc(sim_test1$y,GLMM_test_pred3),
                                error=function(e) return(NA), 
                                warning=function(w) return(NA))
    
    glmm4 <-tryCatch(glmer(y~dxij+(1|ID),data=sim_train1,family = binomial),
                     error=function(e) return(NA), 
                     warning=function(w) return(NA))
    GLMM_train_pred4 <- tryCatch(predict(glmm4,sim_train1,type="response"),
                                 error=function(e) return(NA), 
                                 warning=function(w) return(NA))
    GLMM_train_auc4<-tryCatch(auc(sim_train1$y,GLMM_train_pred4),
                                 error=function(e) return(NA), 
                                 warning=function(w) return(NA))
    GLMM_test_pred4 <- tryCatch(predict(glmm4,sim_test1,type="response",allow.new.levels=T),
                                error=function(e) return(NA), 
                                warning=function(w) return(NA))
    GLMM_test_auc4<-tryCatch(auc(sim_test1$y,GLMM_test_pred4),
                                error=function(e) return(NA), 
                                warning=function(w) return(NA))
    
    #Tree
    tree1<-rpart(y~xi,data=sim_train1,method="class")
    pfit<- prune(tree1, cp=tree1$cptable[which.min(tree1$cptable[,"xerror"]),"CP"])
    tree_root1<-0
    if(min(pfit$where) == max(pfit$where)){tree_root1<-1}
    TREE_train_pred1 <- predict(pfit,data=sim_train1)
    TREE_train_auc1<-auc(sim_train1$y,TREE_train_pred1[,1])
    TREE_test_pred1 <- predict(pfit,newdata=sim_test1)
    TREE_test_auc1<-auc(sim_test1$y,TREE_test_pred1[,1])
    
    tree2<-rpart(y~xi+as.factor(ID),data=sim_train1,method="class")
    pfit2<- prune(tree2, cp=tree2$cptable[which.min(tree2$cptable[,"xerror"]),"CP"])
    tree_root2<-0
    if(min(pfit2$where) == max(pfit2$where)){tree_root2<-1}
    TREE_train_pred2 <- predict(pfit2,data=sim_train1)
    TREE_train_auc2<-auc(sim_train1$y,TREE_train_pred2[,1])
    TREE_test_pred2 <- tryCatch(predict(pfit2,newdata=sim_test1),
                                error=function(e) return(NA), 
                                warning=function(w) return(NA))
    TREE_test_auc2<-tryCatch(auc(sim_test1$y,TREE_test_pred2[,1]),
                                error=function(e) return(NA), 
                                warning=function(w) return(NA))
    
    tree3<-rpart(y~xi+dxij,data=sim_train1,method="class")
    pfit3<- prune(tree3, cp=tree3$cptable[which.min(tree3$cptable[,"xerror"]),"CP"])
    tree_root3<-0
    if(min(pfit3$where) == max(pfit3$where)){tree_root3<-1}
    TREE_train_pred3 <- predict(pfit3,data=sim_train1)
    TREE_train_auc3<-auc(sim_train1$y,TREE_train_pred3[,1])
    TREE_test_pred3 <- predict(pfit3,newdata=sim_test1)
    TREE_test_auc3<-auc(sim_test1$y,TREE_test_pred3[,1])
    
    tree4<-rpart(y~xi+dxij+as.factor(ID),data=sim_train1,method="class")
    pfit4<- prune(tree4, cp=tree4$cptable[which.min(tree4$cptable[,"xerror"]),"CP"])
    tree_root4<-0
    if(min(pfit4$where) == max(pfit4$where)){tree_root4<-1}
    TREE_train_pred4 <- predict(pfit4,data=sim_train1)
    TREE_train_auc4<-auc(sim_train1$y,TREE_train_pred4[,1])
    TREE_test_pred4 <- tryCatch(predict(pfit4,newdata=sim_test1),
                                error=function(e) return(NA), 
                                warning=function(w) return(NA))
    TREE_test_auc4<-tryCatch(auc(sim_test1$y,TREE_test_pred4[,1]),
                                error=function(e) return(NA), 
                                warning=function(w) return(NA))
    
    
     outcomp<-data.frame(Ui,beta1,beta2,GREEM_train_auc1,GREEM_train_auc2,GREEM_train_auc3,GREEM_train_auc4,GLM_train_auc1,GLM_train_auc2,GLM_train_auc3,
                         GLMM_train_auc1,GLMM_train_auc2,GLMM_train_auc3,GLMM_train_auc4,TREE_train_auc1,TREE_train_auc2,TREE_train_auc3,TREE_train_auc4,
                         GREEM_test_auc1,GREEM_test_auc1v2,GREEM_test_auc2,GREEM_test_auc2v2,GREEM_test_auc3,GREEM_test_auc3v2,GREEM_test_auc4,GREEM_test_auc4v2,GLM_test_auc1,GLM_test_auc2,GLM_test_auc3,
                         GLMM_test_auc1,GLMM_test_auc2,GLMM_test_auc3,GLMM_test_auc4,TREE_test_auc1,TREE_test_auc2,TREE_test_auc3,
                         TREE_test_auc4,GREEM_root1,GREEM_root2,GREEM_root3,GREEM_root4,tree_root1,tree_root2,tree_root3,tree_root4)
  return(outcomp)
}

#a<-getauc1(1,-2,1,sub)