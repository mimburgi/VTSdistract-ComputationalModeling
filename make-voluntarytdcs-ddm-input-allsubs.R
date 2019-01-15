
library(rstan)
library(hBayesDM)


###################
####VOLUNTARY######
###################

setwd("C:\\Users\\Mike\\Desktop\\School\\VTS_distract_analysis\\R")
all = read.table("ansummary.txt", header = TRUE, blank.lines.skip = FALSE)
all$subj<- tolower(all$subj)

#make rowShift
rowShift <- function(x, shiftLen = 1L) {
  r <- (1L + shiftLen):(length(x) + shiftLen)
  r[r<1] <- NA
  return(x[r])
}

#make E+1
all$E1[all$trialNum==1]<- -1
all$E1[(rowShift(all$acc, -1))=="hit"]<-0
all$E1[(rowShift(all$acc, -1))=="incorrect"]<-1
all$E1[(rowShift(all$acc, -1))=="miss"]<-1

#identify outliers
subRTs<-all[,c("subj", "taskRT")]
subSDs<-aggregate(taskRT~subj, data = subRTs, FUN = sd)
subMeans<-aggregate(taskRT~subj, data = subRTs, FUN = mean)
subSDs$taskRT<-subSDs$taskRT*3
all$ThreeSD<-subSDs[match(all$subj, subSDs$subj), 2]
all$RTmean<-subMeans[match(all$subj, subMeans$subj), 2]
all$pThreeSD<-all$RTmean + all$ThreeSD
all$mThreeSD<-all$RTmean - all$ThreeSD
all$ThreeSD<-all$outlier
all$outlier<-0
all$outlier[all$pThreeSD<=all$taskRT | all$mThreeSD>=all$taskRT | all$taskRT<200]<-1

#organize and prune data
use<-all[all$E1==0 & all$cue=="?" & all$acc!="miss" & all$trialNum!=1 & all$outlier==0, c("subj", "acc" , "taskRT", "altRep", "condition", "area")]

an<-use

all = read.table("cathsummary.txt", header = TRUE, blank.lines.skip = FALSE)
all$subj<- tolower(all$subj)

#make rowShift
rowShift <- function(x, shiftLen = 1L) {
  r <- (1L + shiftLen):(length(x) + shiftLen)
  r[r<1] <- NA
  return(x[r])
}

#make E+1
all$E1[all$trialNum==1]<- -1
all$E1[(rowShift(all$acc, -1))=="hit"]<-0
all$E1[(rowShift(all$acc, -1))=="incorrect"]<-1
all$E1[(rowShift(all$acc, -1))=="miss"]<-1

#identify outliers
subRTs<-all[,c("subj", "taskRT")]
subSDs<-aggregate(taskRT~subj, data = subRTs, FUN = sd)
subMeans<-aggregate(taskRT~subj, data = subRTs, FUN = mean)
subSDs$taskRT<-subSDs$taskRT*3
all$ThreeSD<-subSDs[match(all$subj, subSDs$subj), 2]
all$RTmean<-subMeans[match(all$subj, subMeans$subj), 2]
all$pThreeSD<-all$RTmean + all$ThreeSD
all$mThreeSD<-all$RTmean - all$ThreeSD
all$ThreeSD<-all$outlier
all$outlier<-0
all$outlier[all$pThreeSD<=all$taskRT | all$mThreeSD>=all$taskRT | all$taskRT<200]<-1

#organize and prune data
use<-all[all$E1==0 & all$cue=="?" & all$acc!="miss" & all$trialNum!=1 & all$outlier==0, c("subj", "acc" , "taskRT", "altRep", "condition", "area")]

cath<-use
################

################
total<-rbind(cath, an)
total<-subset(total, altRep == "1" | altRep == "2")
total$acc<-as.character(total$acc)
total$acc[total$acc == "hit"] = 2
total$acc[total$acc == "incorrect"] = 1
total$taskRT<-total$taskRT/1000
total$acc<-as.numeric(total$acc)

colnames(total)<-c("subjID", "choice", "RT", "altrep", "condition", "area")
write.table(total, "voluntarytDCS-forDDM-allsubs.txt", row.names = FALSE)



