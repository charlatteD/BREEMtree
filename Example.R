#Example 
#Import the BREEMtree function 
source("/Users/huiyudeng/Dropbox/dissertation/GREEM_outputs/BREEMtree.R")

#Simulate the data (within-person prediction)
set.seed(222)
out1<-getprob1(100,10,1,1,1,1,1,-0.5,0.5)
sub <- rep(c(rep(TRUE, 8), rep(FALSE, 2)), 100)
sim1<-data.frame(out1$x,out1$y,out1$ID,rep(out1$Xi,each=10),out1$xdevij,sub)
colnames(sim1)<-c("xij","y","ID","xi","dxij","sub")
sim_train1<-subset(sim1, sub==TRUE)
sim_test1<-subset(sim1, sub==FALSE)
sim_train2<-data.frame(sim_train1$xij)
sim_test2<-data.frame(sim_test1$xij)
colnames(sim_train2)<-c("xij")
colnames(sim_test2)<-c("xij")
y_train<-sim_train1$y
y_test<-sim_test1$y
ID_train<-sim_train1$ID
ID_test<-sim_test1$ID

#Apply the BREEMtree function 
fit1<-BREEMtreeop(sim_train2,y_train,ID_train)
GREEM_test_pred1 <- predict(fit1$tree,newdata=sim_test2)+rep(unlist(ranef(fit1$fit)),c(as.vector(table(ID_test))))
GREEM_test_predprob1 <- exp(GREEM_test_pred1)/(1+exp(GREEM_test_pred1))
GREEM_test_auc<-auc(y_test,GREEM_test_predprob1)
GREEM_test_auc

library(rpart.plot)
rpart.plot(fit1$tree,main="BRE-EM tree", box.palette = 0, Margin=0,roundint=FALSE)

