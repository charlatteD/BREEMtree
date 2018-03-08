setwd("/Users/huiyudeng/Desktop/USC/Dissertation2016/Rcode_Feb/feb2018")
#######################################################
######Within person prediction#########################
#######################################################
nrep<-25
sub <- rep(c(rep(TRUE, 8), rep(FALSE, 2)), 100)
library(snow)
#######################################################
######1) Changing Ui###################################
#######################################################
start_time <- Sys.time()
seq_u<-c(seq(0.5,10,by=0.5),seq(15,100,by=5))
#seq_u<-c(seq(0.5,1,by=0.5))
n1_u<-length(seq_u)
outcomp_u<-matrix(NA,nrow=n1_u,ncol=45)
cl <- makeCluster(3)
clusterExport(cl, c("nrep","seq_u","n1_u","outcomp_u","sub"))
clusterEvalQ(cl, library(MASS))
clusterEvalQ(cl, library(rpart))
clusterEvalQ(cl, library(pROC))
clusterEvalQ(cl, library(lme4))
clusterEvalQ(cl, source("/Users/huiyudeng/Desktop/USC/Dissertation2016/Rcode_Feb/feb2018/funcs/GREEMtreeop.R"))
clusterEvalQ(cl, source("/Users/huiyudeng/Desktop/USC/Dissertation2016/Rcode_Feb/feb2018/funcs/sim1.R"))
clusterEvalQ(cl, source("/Users/huiyudeng/Desktop/USC/Dissertation2016/Rcode_Feb/feb2018/funcs/evalfunc1.R"))
nulls_u <- parLapply(cl, seq_len(nrep),function(i) {
                outcomp_u<-sapply(seq_along(seq_u),function(j) getauc1(seq_u[j],-2,1,sub))
              return(outcomp_u) })
stopCluster(cl)
up1_1<-t(do.call("cbind", nulls_u))
up1_2<-as.data.frame(sapply(as.data.frame(up1_1),unlist))
up1_3<-up1_2[order(up1_2$Ui),] 

mean_Up1<-aggregate(up1_3, list(up1_3$Ui), mean,na.rm=T)
write.csv(mean_Up1[,-1], file = "mean_Up1.csv")
nasum_Up1<-aggregate(is.na(up1_3[,-1]), list(up1_3$Ui), sum)
colnames(nasum_Up1)[1] <- "Ui"
write.csv(nasum_Up1[,-1], file = "nasum_Up1.csv")
end_time <- Sys.time()
end_time
end_time - start_time

#######################################################
######2) Changing Beta 1###############################
#######################################################
start_time <- Sys.time()
seq_b<-seq(-5,5,length=40)
n1_b<-length(seq_b)
outcomp_b<-matrix(NA,nrow=n1_b,ncol=45)
cl <- makeCluster(3)
clusterExport(cl, c("nrep","seq_b","n1_b","outcomp_b","sub"))
clusterEvalQ(cl, library(MASS))
clusterEvalQ(cl, library(rpart))
clusterEvalQ(cl, library(pROC))
clusterEvalQ(cl, library(lme4))
clusterEvalQ(cl, source("/Users/huiyudeng/Desktop/USC/Dissertation2016/Rcode_Feb/feb2018/funcs/GREEMtreeop.R"))
clusterEvalQ(cl, source("/Users/huiyudeng/Desktop/USC/Dissertation2016/Rcode_Feb/feb2018/funcs/sim1.R"))
clusterEvalQ(cl, source("/Users/huiyudeng/Desktop/USC/Dissertation2016/Rcode_Feb/feb2018/funcs/evalfunc1.R"))
nulls_b <- parLapply(cl, seq_len(nrep),function(i) {
  outcomp_b<-sapply(seq_along(seq_b),function(j) getauc1(1,seq_b[j],1,sub))
  return(outcomp_b) })
stopCluster(cl)
bp1_1<-t(do.call("cbind", nulls_b))
bp1_2<-as.data.frame(sapply(as.data.frame(bp1_1),unlist))
bp1_3<-bp1_2[order(bp1_2$beta1),] 

mean_Bp1<-aggregate(bp1_3, list(bp1_3$beta1), mean,na.rm=T)
write.csv(mean_Bp1, file = "mean_Bp1.csv")
nasum_Bp1<-aggregate(is.na(bp1_3[,-1]), list(bp1_3$beta1), sum)
colnames(nasum_Bp1)[1] <- "beta1"
write.csv(nasum_Bp1, file = "nasum_Bp1.csv")
end_time <- Sys.time()
end_time
end_time - start_time

#######################################################
######2) Changing Beta 2###############################
#######################################################
start_time <- Sys.time()
seq_b2<-seq(-5,5,length=40)
n1_b2<-length(seq_b2)
outcomp_b2<-matrix(NA,nrow=n1_b2,ncol=45)
cl <- makeCluster(3)
clusterExport(cl, c("nrep","seq_b2","n1_b2","outcomp_b2","sub"))
clusterEvalQ(cl, library(MASS))
clusterEvalQ(cl, library(rpart))
clusterEvalQ(cl, library(pROC))
clusterEvalQ(cl, library(lme4))
clusterEvalQ(cl, source("/Users/huiyudeng/Desktop/USC/Dissertation2016/Rcode_Feb/feb2018/funcs/GREEMtreeop.R"))
clusterEvalQ(cl, source("/Users/huiyudeng/Desktop/USC/Dissertation2016/Rcode_Feb/feb2018/funcs/sim1.R"))
clusterEvalQ(cl, source("/Users/huiyudeng/Desktop/USC/Dissertation2016/Rcode_Feb/feb2018/funcs/evalfunc1.R"))
nulls_b2 <- parLapply(cl, seq_len(nrep),function(i) {
  outcomp_b2<-sapply(seq_along(seq_b2),function(j) getauc1(1,-2,seq_b2[j],sub))
  return(outcomp_b2) })
stopCluster(cl)
b2p1_1<-t(do.call("cbind", nulls_b2))
b2p1_2<-as.data.frame(sapply(as.data.frame(b2p1_1),unlist))
b2p1_3<-b2p1_2[order(b2p1_2$beta2),] 

mean_B2p1<-aggregate(b2p1_3, list(b2p1_3$beta2), mean,na.rm=T)
write.csv(mean_B2p1, file = "mean_B2p1.csv")
nasum_B2p1<-aggregate(is.na(b2p1_3[,-1]), list(b2p1_3$beta2), sum)
colnames(nasum_B2p1)[1] <- "beta2"
write.csv(nasum_B2p1, file = "nasum_B2p1.csv")
end_time <- Sys.time()
end_time
end_time - start_time

#plot 
#Plot 1: # of NA for the GREEMtree
pdf("figure_nav2.pdf",width=20,height=10)
warningout_u<-read.csv("nasum_Up1.csv",header=T)
var1_ua<-c("GREEM_test_auc1","GREEM_test_auc2","GREEM_test_auc3","GREEM_test_auc4","GLMM_test_auc1","GLMM_test_auc2","GLMM_test_auc3","GLMM_test_auc4","Ui")
da1_u<-warningout_u[var1_ua]
labelx2<-expression("GREEM:Xij","GREEM:Xi+dev","GREEM:Xi","GREEM:dev","GLMM:Xij+Ui","GLMM:Xi+dev+Ui","GLMM:Xi+Ui","GLMM:dev+Ui")
par(mfrow=c(1,2))
par(mar=c(5.1, 5.1, 5.1, 5.1))
plot(da1_u$Ui,da1_u[,1],xlim=c(0,101),ylim=c(0,25),col=1,pch=19,type="b",ylab="Number of warnings",xlab="Var(Ui)",cex.lab=2)
for(k in 2:(dim(da1_u)[2]-1)){
  lines(da1_u$Ui,da1_u[,k],col=k)
}
legend(65,24, legend = labelx2, col = c(1:(dim(da1_u)[2]-1)),cex=1.5,pch=19)

warningout_b<-read.csv("mean_Bp1.csv",header=T)
var1_bb<-c("GREEM_Xij","GREEM_Xi_dev","GREEM_Xi","GREEM_dev","GLMM_Xij_Ui","GLMM_Xi_dev_Ui","GLMM_Xi_Ui","GLMM_dev_Ui","seq_b")
da1_b<-warningout_b[var1_bb]
plot(da1_b$seq_b,da1_b[,1],xlim=c(-6,6),ylim=c(0,25),col=1,pch=19,type="b",ylab="Number of warnings",xlab="Beta for Xi",cex.lab=2)
for(k in 2:(dim(da1_b)[2]-1)){
  lines(da1_b$seq_b,da1_b[,k],col=k)
}
#legend(-1.8,25, legend = labelx2, col = c(2:dim(warningout2)[2]-1),cex=0.8,pch=19)
dev.off()


#Plot 2: Plot the AUC 
pdf("figure_aucv2.pdf",width=20,height=10)
outcompend_u<-read.csv("outcompend_u.csv",header=T)
var1<-c("GREEM_test_auc1","GREEM_test_auc2","GLM_test_auc1","GLMM_test_auc1","TREE_test_auc1","TREE_test_auc2","seq_u")
da1<-outcompend_u[var1]
labelx2<-expression("GREEM:Xij","GREEM:Xi+dev","GLM:Xij","GLMM:xij+Ui","Rpart:Xij","Rpart:Xij+ID")
par(mfrow=c(1,2))
par(mar=c(5.1, 5.1, 5.1, 5.1))
plot(da1$seq_u,da1[,1],xlim=c(0,101),ylim=c(0.2,1),col=1,pch=19,ylab="AUC",xlab="Var(Ui)",cex.lab=2)
for(k in 2:dim(da1)[2]-1){
  lines(da1$seq_u,da1[,k],col=k)
}
legend(1,0.4, legend = labelx2, col = c(1:(dim(da1)[2]-1)),cex=2,pch=19,ncol=2)

outcompend_b<-read.csv("outcompend_b.csv",header=T)
var1_b<-c("GREEM_test_auc1","GREEM_test_auc2","GLM_test_auc1","GLMM_test_auc1","TREE_test_auc1","TREE_test_auc2","seq_b")
da1_b<-outcompend_b[var1_b]
plot(da1_b$seq_b,da1_b[,1],xlim=c(-6,6),ylim=c(0.3,1),col=1,pch=19,ylab="AUC",xlab="Beta for Xi",cex.lab=2)
for(k in 2:dim(da1_b)[2]-1){
  lines(da1_b$seq_b,da1_b[,k],col=k)
}
#legend(-6,0.6, legend = labelx2, col = c(1:dim(da1)[2]),cex=0.8,pch=19)
dev.off()

#Plot 3: Root only models: 
pdf("figure_rootv2.pdf",width=20,height=8)
rootout_u<-read.csv("rootout_u.csv",header=T)
var3_u<-c("GREEM_Xij","GREEM_Xi_dev","GREEM_Xi","GREEM_dev","Rpart_Xij","Rpart_Xij_ID","Rpart_Xij_dev","Rpart_Xij_dev_ID","seq_u")
da3_u<-rootout_u[var3_u]
labelx2<-expression("GREEM:Xij","GREEM:Xi+dev","GREEM:Xi","GREEM:dev","Rpart:Xij","Rpart:Xij+ID","Rpart:Xi+dev","Rpart:Xi+dev+ID")
par(mfrow=c(1,3))
par(mar=c(5.1, 5.1, 5.1, 5.1))
plot(da3_u$seq_u,da3_u[,1],xlim=c(0,105),ylim=c(0,1),col=1,pch=19,ylab="Number of roots",xlab="Var(Ui)",cex.lab=2)
for(k in 2:dim(da3_u)[2]-1){
  lines(da3_u$seq_u,da3_u[,k],col=k)
}
legend(60,0.4, legend = labelx2, col = c(1:(dim(da3_u)[2]-1)),cex=1.2,pch=19)


rootout_b<-read.csv("rootout_b.csv",header=T)
var3_b1<-c("GREEM_Xij","GREEM_Xi_dev","GREEM_Xi","GREEM_dev","Rpart_Xij","Rpart_Xij_ID","Rpart_Xij_dev","Rpart_Xij_dev_ID","seq_b")
da3_b<-rootout_b[var3_b1]
plot(da3_b$seq_b,da3_b[,1],xlim=c(-5,5),ylim=c(0,1),col=1,pch=19,ylab="Number of roots",xlab="Beta Xi",cex.lab=2)
for(k in 2:dim(da3_b)[2]-1){
  lines(da3_b$seq_b,da3_b[,k],col=k)
}
#legend(-10,25, legend = labelx2, col = c(1:dim(rootout2)[2]),cex=0.8,pch=19)

rootout_b2<-read.csv("rootout_b2.csv",header=T)
var3_b2<-c("GREEM_Xij","GREEM_Xi_dev","GREEM_Xi","GREEM_dev","Rpart_Xij","Rpart_Xij_ID","Rpart_Xij_dev","Rpart_Xij_dev_ID","seq_b2")
da3_b2<-rootout_b2[var3_b2]
plot(da3_b2$seq_b2,da3_b2[,1],xlim=c(-5,5),ylim=c(0,1),col=1,pch=19,ylab="Number of roots",xlab="Beta devxij",cex.lab=2)
for(k in 2:dim(da3_b2)[2]-1){
  lines(da3_b2$seq_b2,da3_b2[,k],col=k)
}
dev.off()

#######################################################
######Between person prediction########################
#######################################################
indxM<-rep(1:100,each=10)
nsamp<-sample(1:100,70)
sub<-ifelse(indxM %in% nsamp,TRUE,FALSE)
#######################################################
######1) Changing Ui###################################
#######################################################
start_time <- Sys.time()
outcomp_bu<-matrix(NA,nrow=n1_u,ncol=45)
cl <- makeCluster(3)
clusterExport(cl, c("nrep","seq_u","n1_u","outcomp_bu","sub"))
clusterEvalQ(cl, library(MASS))
clusterEvalQ(cl, library(rpart))
clusterEvalQ(cl, library(pROC))
clusterEvalQ(cl, library(lme4))
clusterEvalQ(cl, source("/Users/huiyudeng/Desktop/USC/Dissertation2016/Rcode_Feb/feb2018/funcs/GREEMtreeop.R"))
clusterEvalQ(cl, source("/Users/huiyudeng/Desktop/USC/Dissertation2016/Rcode_Feb/feb2018/funcs/sim1.R"))
clusterEvalQ(cl, source("/Users/huiyudeng/Desktop/USC/Dissertation2016/Rcode_Feb/feb2018/funcs/evalfunc1.R"))
nulls_bu <- parLapply(cl, seq_len(nrep),function(i) {
  outcomp_bu<-sapply(seq_along(seq_u),function(j) getauc1(seq_u[j],-2,1,sub))
  return(outcomp_bu) })
stopCluster(cl)
up2_1<-t(do.call("cbind", nulls_bu))
up2_2<-as.data.frame(sapply(as.data.frame(up2_1),unlist))
up2_3<-up2_2[order(up2_2$Ui),] 

mean_Up2<-aggregate(up2_3, list(up2_3$Ui), mean,na.rm=T)
write.csv(mean_Up2, file = "mean_Up2.csv")
nasum_Up2<-aggregate(is.na(up2_3[,-1]), list(up2_3$Ui), sum)
colnames(nasum_Up2)[1] <- "Ui"
write.csv(nasum_Up2, file = "nasum_Up2.csv")
end_time <- Sys.time()
end_time
end_time - start_time

#######################################################
######2) Changing Beta 1###############################
#######################################################
start_time <- Sys.time()
outcomp_bb<-matrix(NA,nrow=n1_b,ncol=45)
cl <- makeCluster(3)
clusterExport(cl, c("nrep","seq_b","n1_b","outcomp_bb","sub"))
clusterEvalQ(cl, library(MASS))
clusterEvalQ(cl, library(rpart))
clusterEvalQ(cl, library(pROC))
clusterEvalQ(cl, library(lme4))
clusterEvalQ(cl, source("/Users/huiyudeng/Desktop/USC/Dissertation2016/Rcode_Feb/feb2018/funcs/GREEMtreeop.R"))
clusterEvalQ(cl, source("/Users/huiyudeng/Desktop/USC/Dissertation2016/Rcode_Feb/feb2018/funcs/sim1.R"))
clusterEvalQ(cl, source("/Users/huiyudeng/Desktop/USC/Dissertation2016/Rcode_Feb/feb2018/funcs/evalfunc1.R"))
nulls_bb <- parLapply(cl, seq_len(nrep),function(i) {
  outcomp_bb<-sapply(seq_along(seq_b),function(j) getauc1(1,seq_b[j],1,sub))
  return(outcomp_bb) })
stopCluster(cl)
bp2_1<-t(do.call("cbind", nulls_bb))
bp2_2<-as.data.frame(sapply(as.data.frame(bp2_1),unlist))
bp2_3<-bp2_2[order(bp2_2$beta1),] 

mean_Bp2<-aggregate(bp2_3, list(bp2_3$beta1), mean,na.rm=T)
write.csv(mean_Bp2, file = "mean_Bp2.csv")
nasum_Bp2<-aggregate(is.na(bp2_3[,-1]), list(bp2_3$beta1), sum)
colnames(nasum_Bp2)[1] <- "beta1"
write.csv(nasum_Bp2, file = "nasum_Bp2.csv")
end_time <- Sys.time()
end_time
end_time - start_time

#######################################################
######2) Changing Beta 2###############################
#######################################################
start_time <- Sys.time()
outcomp_bb2<-matrix(NA,nrow=n1_b2,ncol=45)
cl <- makeCluster(3)
clusterExport(cl, c("nrep","seq_b2","n1_b2","outcomp_bb2","sub"))
clusterEvalQ(cl, library(MASS))
clusterEvalQ(cl, library(rpart))
clusterEvalQ(cl, library(pROC))
clusterEvalQ(cl, library(lme4))
clusterEvalQ(cl, source("/Users/huiyudeng/Desktop/USC/Dissertation2016/Rcode_Feb/feb2018/funcs/GREEMtreeop.R"))
clusterEvalQ(cl, source("/Users/huiyudeng/Desktop/USC/Dissertation2016/Rcode_Feb/feb2018/funcs/sim1.R"))
clusterEvalQ(cl, source("/Users/huiyudeng/Desktop/USC/Dissertation2016/Rcode_Feb/feb2018/funcs/evalfunc1.R"))
nulls_bb2 <- parLapply(cl, seq_len(nrep),function(i) {
  outcomp_bb2<-sapply(seq_along(seq_b2),function(j) getauc1(1,-2,seq_b2[j],sub))
  return(outcomp_bb2) })
stopCluster(cl)
b2p2_1<-t(do.call("cbind", nulls_bb2))
b2p2_2<-as.data.frame(sapply(as.data.frame(b2p2_1),unlist))
b2p2_3<-b2p2_2[order(b2p2_2$beta2),] 

mean_B2p2<-aggregate(b2p2_3, list(b2p2_3$beta2), mean,na.rm=T)
write.csv(mean_B2p2, file = "mean_B2p2.csv")
nasum_B2p2<-aggregate(is.na(b2p2_3[,-1]), list(b2p2_3$beta2), sum)
colnames(nasum_B2p2)[1] <- "beta1"
write.csv(nasum_B2p2, file = "nasum_B2p2.csv")
end_time <- Sys.time()
end_time
end_time - start_time