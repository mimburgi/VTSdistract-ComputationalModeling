library(rstan)
library(hBayesDM)

#create temporary output directory
dir.create(file.path(getwd(), "tmpinput"))

#number of times the model will be run
numIter = 20
#number of cores used
numCores = 4

#probability of changing to response incorrect, changing RT to mean
probchange = .05

#calculate mean RT across everything
alltrials <- read.table("voluntarytDCS-forDDM-allsubs.txt", header = TRUE)
meanRT<-mean(alltrials$RT)

#create dfs that will be used for parameter output
#tau
taudf<-data.frame(levels(alltrials$subjID), rep(NA, length(levels(alltrials$subjID))))
colnames(taudf)<-c("subjID", "mean")
for (i in 1:numIter){
  taudf<- cbind(taudf, rep(NA, length(taudf$subjID)))
  colnames(taudf)[i+2]<-as.character(i)
}
acc_an_tau<-taudf
acc_ca_tau<-taudf
fp_an_tau<-taudf
fp_ca_tau<-taudf
sham_tau<-taudf
#delta
deltadf<-data.frame(levels(alltrials$subjID), rep(NA, length(levels(alltrials$subjID))))
colnames(deltadf)<-c("subjID", "mean")
for (i in 1:numIter){
  deltadf<- cbind(deltadf, rep(NA, length(deltadf$subjID)))
  colnames(deltadf)[i+2]<-as.character(i)
}
acc_an_delta<-deltadf
acc_ca_delta<-deltadf
fp_an_delta<-deltadf
fp_ca_delta<-deltadf
sham_delta<-deltadf
#alpha
alphadf<-data.frame(levels(alltrials$subjID), rep(NA, length(levels(alltrials$subjID))))
colnames(alphadf)<-c("subjID", "mean")
for (i in 1:numIter){
  alphadf<- cbind(alphadf, rep(NA, length(alphadf$subjID)))
  colnames(alphadf)[i+2]<-as.character(i)
}
acc_an_alpha<-alphadf
acc_ca_alpha<-alphadf
fp_an_alpha<-alphadf
fp_ca_alpha<-alphadf
sham_alpha<-alphadf

#main loop
for (i in 1:numIter){
  changetrials = TRUE #this gets changed to false if the loop changes enough that we can use everyone
  tmpdf<-alltrials #this will be edited, broken up, saved out and fed into hBayesDM on each iteration
  while (changetrials == TRUE){ 
  for (trialNum in (1:length(tmpdf$choice))){
    if (runif(1, 0.0, 1.0) < probchange){ #generates a random number, changes trial if numebr is less than prob change
      tmpdf[trialNum,"choice"]<-1
      tmpdf[trialNum,"RT"]<-meanRT
    }#end if runif
  }#end for trial
  testacc<-aggregate(choice ~ subjID + condition + area, data = tmpdf, FUN = mean)
  if (max(testacc$choice) < 2){changetrials = FALSE} #if all participants have under 100% acc for each condition
  }#end while changetrials
  
  #create dfs for input to hbayesdm
  acc_an <- subset(tmpdf, condition == "A" & area == "AC")
  acc_ca <- subset(tmpdf, condition == "C" & area == "AC")
  fp_an <- subset(tmpdf, condition == "A" & area == "FP")
  fp_ca <- subset(tmpdf, condition == "C" & area == "FP")
  sham <- subset(tmpdf, condition ==  "S")
  
  #remove unused subjects from each dataset (subjects that were not in each condition)
  acc_an$subjID<-droplevels(acc_an$subjID)
  acc_ca$subjID<-droplevels(acc_ca$subjID)
  fp_an$subjID<-droplevels(fp_an$subjID)
  fp_ca$subjID<-droplevels(fp_ca$subjID)
  sham$subjID<-droplevels(sham$subjID)
  
  #write out dfs for input to hbayesdm
  write.table(acc_an, file = "tmpinput/acc_an.txt", row.names = FALSE)
  write.table(acc_ca, file = "tmpinput/acc_ca.txt", row.names = FALSE)
  write.table(fp_an, file = "tmpinput/fp_an.txt", row.names = FALSE)
  write.table(fp_ca, file = "tmpinput/fp_ca.txt", row.names = FALSE)
  write.table(sham, file = "tmpinput/sham.txt", row.names = FALSE)
  
  
  #run model
  acc_an_model<-choiceRTNoBeta_ddm("tmpinput/acc_an.txt", ncore = numCores, max_treedepth = 20)
  acc_ca_model<-choiceRTNoBeta_ddm("tmpinput/acc_ca.txt", ncore = numCores, max_treedepth = 20)
  fp_an_model<-choiceRTNoBeta_ddm("tmpinput/fp_an.txt", ncore = numCores, max_treedepth = 20)
  fp_ca_model<-choiceRTNoBeta_ddm("tmpinput/fp_ca.txt", ncore = numCores, max_treedepth = 20)
  sham_model<-choiceRTNoBeta_ddm("tmpinput/sham.txt", ncore = numCores, max_treedepth = 20)
  
  #export to parameter dataframes
  for (subj in levels(acc_an$subjID)){
    acc_an_tau[acc_an_tau$subjID == subj, i+2]<-acc_an_model$allIndPars$tau[acc_an_model$allIndPars$subjID == subj]
    acc_an_delta[acc_an_delta$subjID == subj, i+2]<-acc_an_model$allIndPars$delta[acc_an_model$allIndPars$subjID == subj]
    acc_an_alpha[acc_an_alpha$subjID == subj, i+2]<-acc_an_model$allIndPars$alpha[acc_an_model$allIndPars$subjID == subj]
  }#end acc an for loop
  for (subj in levels(acc_ca$subjID)){
    acc_ca_tau[acc_ca_tau$subjID == subj, i+2]<-acc_ca_model$allIndPars$tau[acc_ca_model$allIndPars$subjID == subj]
    acc_ca_delta[acc_ca_delta$subjID == subj, i+2]<-acc_ca_model$allIndPars$delta[acc_ca_model$allIndPars$subjID == subj]
    acc_ca_alpha[acc_ca_alpha$subjID == subj, i+2]<-acc_ca_model$allIndPars$alpha[acc_ca_model$allIndPars$subjID == subj]
  }#end acc ca for loop
  for (subj in levels(fp_an$subjID)){
    fp_an_tau[fp_an_tau$subjID == subj, i+2]<-fp_an_model$allIndPars$tau[fp_an_model$allIndPars$subjID == subj]
    fp_an_delta[fp_an_delta$subjID == subj, i+2]<-fp_an_model$allIndPars$delta[fp_an_model$allIndPars$subjID == subj]
    fp_an_alpha[fp_an_alpha$subjID == subj, i+2]<-fp_an_model$allIndPars$alpha[fp_an_model$allIndPars$subjID == subj]
  }#end fp an for loop
  for (subj in levels(fp_ca$subjID)){
    fp_ca_tau[fp_ca_tau$subjID == subj, i+2]<-fp_ca_model$allIndPars$tau[fp_ca_model$allIndPars$subjID == subj]
    fp_ca_delta[fp_ca_delta$subjID == subj, i+2]<-fp_ca_model$allIndPars$delta[fp_ca_model$allIndPars$subjID == subj]
    fp_ca_alpha[fp_ca_alpha$subjID == subj, i+2]<-fp_ca_model$allIndPars$alpha[fp_ca_model$allIndPars$subjID == subj]
  }#end fp ca for loop
  for (subj in levels(sham$subjID)){
    sham_tau[sham_tau$subjID == subj, i+2]<-sham_model$allIndPars$tau[sham_model$allIndPars$subjID == subj]
    sham_delta[sham_delta$subjID == subj, i+2]<-sham_model$allIndPars$delta[sham_model$allIndPars$subjID == subj]
    sham_alpha[sham_alpha$subjID == subj, i+2]<-sham_model$allIndPars$alpha[sham_model$allIndPars$subjID == subj]
  }#end sham for loop
  
}#end for i in numIter

#calculate mean parameters
acc_an_tau[2]<-rowMeans(acc_an_tau[,3:(numIter + 2)])
acc_an_delta[2]<-rowMeans(acc_an_delta[,3:(numIter + 2)])
acc_an_alpha[2]<-rowMeans(acc_an_alpha[,3:(numIter + 2)])
acc_ca_tau[2]<-rowMeans(acc_ca_tau[,3:(numIter + 2)])
acc_ca_delta[2]<-rowMeans(acc_ca_delta[,3:(numIter + 2)])
acc_ca_alpha[2]<-rowMeans(acc_ca_alpha[,3:(numIter + 2)])
fp_an_tau[2]<-rowMeans(fp_an_tau[,3:(numIter + 2)])
fp_an_delta[2]<-rowMeans(fp_an_delta[,3:(numIter + 2)])
fp_an_alpha[2]<-rowMeans(fp_an_alpha[,3:(numIter + 2)])
fp_ca_tau[2]<-rowMeans(fp_ca_tau[,3:(numIter + 2)])
fp_ca_delta[2]<-rowMeans(fp_ca_delta[,3:(numIter + 2)])
fp_ca_alpha[2]<-rowMeans(fp_ca_alpha[,3:(numIter + 2)])
sham_tau[2]<-rowMeans(sham_tau[,3:(numIter + 2)])
sham_delta[2]<-rowMeans(sham_delta[,3:(numIter + 2)])
sham_alpha[2]<-rowMeans(sham_alpha[,3:(numIter + 2)])

#add condition column to parameter dfs
acc_an_tau$condition<-"A"
acc_an_delta$condition<-"A"
acc_an_alpha$condition<-"A"
acc_ca_tau$condition<-"C"
acc_ca_delta$condition<-"C"
acc_ca_alpha$condition<-"C"
fp_an_tau$condition<-"A"
fp_an_delta$condition<-"A"
fp_an_alpha$condition<-"A"
fp_ca_tau$condition<-"C"
fp_ca_delta$condition<-"C"
fp_ca_alpha$condition<-"C"
sham_tau$condition<-"S"
sham_delta$condition<-"S"
sham_alpha$condition<-"S"

#merge each parameter into one df for ease of export, each participant should have one S condition and either an A or a C condition
alltau<-rbind(acc_an_tau, acc_ca_tau, fp_an_tau, fp_ca_tau, sham_tau)
alldelta<-rbind(acc_an_delta, acc_ca_delta, fp_an_delta, fp_ca_delta, sham_delta)
allalpha<-rbind(acc_an_alpha, acc_ca_alpha, fp_an_alpha, fp_ca_alpha, sham_alpha)

#export file
write.table(alltau, "alltau.txt", row.names = FALSE)
write.table(alldelta, "alldelta.txt", row.names = FALSE)
write.table(allalpha, "allalpha.txt", row.names = FALSE)

#remove tmpinput folder created for the files being read into hBayesDM
#will uncomment when I know this script works
#unlink("tmpinput", recursive = TRUE)

