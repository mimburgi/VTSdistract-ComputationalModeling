
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
use<-all[all$E1==0 & all$cue=="?" & all$acc!="miss" & all$condition == "S" & all$trialNum!=1 & all$outlier==0, c("subj", "acc" , "taskRT", "altRep")]

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
use<-all[all$E1==0 & all$cue=="?" & all$acc!="miss" & all$condition == "S" & all$trialNum!=1 & all$outlier==0, c("subj", "acc" , "taskRT", "altRep")]

cath<-use
################

###################
####EXPLICIT#######
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
use<-all[all$E1==0 & all$cue=="?" & all$acc!="miss" & all$condition == "S" & all$trialNum!=1 & all$outlier==0, c("subj", "acc" , "taskRT", "altRep")]

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
use<-all[all$E1==0 & all$cue=="?" & all$acc!="miss" & all$condition == "S" & all$trialNum!=1 & all$outlier==0, c("subj", "acc" , "taskRT", "altRep")]

cath<-use
################






















total_voluntary<-rbind(cath, an)
total_voluntary$acc<-as.character(total_voluntary$acc)
total_voluntary$acc[total_voluntary$acc == "hit"] = 2
total_voluntary$acc[total_voluntary$acc == "incorrect"] = 1
total_voluntary$taskRT<-total_voluntary$taskRT/1000
total_voluntary$acc<-as.numeric(total_voluntary$acc)

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
use<-all[all$E1==0 & all$cue!="?" & all$acc!="miss" & all$condition == "S" & all$trialNum!=1 & all$outlier==0, c("subj", "acc" , "taskRT", "altRep")]

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
use<-all[all$E1==0 & all$cue!="?" & all$acc!="miss" & all$condition == "S" & all$trialNum!=1 & all$outlier==0, c("subj", "acc" , "taskRT", "altRep")]

cath<-use
################
total_explicit<-rbind(cath, an)
total_explicit<-subset(total_explicit, altRep == "1" | altRep == "2")
total_explicit$acc<-as.character(total_explicit$acc)
total_explicit$acc[total_explicit$acc == "hit"] = 2
total_explicit$acc[total_explicit$acc == "incorrect"] = 1
total_explicit$taskRT<-total_explicit$taskRT/1000
total_explicit$acc<-as.numeric(total_explicit$acc)

colnames(total_explicit)<-c("subjID", "choice", "RT", "condition")
switch_explicit<-total_explicit[total_explicit$condition == 2, c("subjID", "choice", "RT")]
switch_explicit<-switch_explicit[order(switch_explicit$subjID),]
rep_explicit<-total_explicit[total_explicit$condition == 1, c("subjID", "choice", "RT")]
rep_explicit<-rep_explicit[order(rep_explicit$subjID),]

colnames(total_voluntary)<-c("subjID", "choice", "RT", "condition")
switch_voluntary<-total_voluntary[total_voluntary$condition == 2, c("subjID", "choice", "RT")]
switch_voluntary<-switch_voluntary[order(switch_voluntary$subjID),]
rep_voluntary<-total_voluntary[total_voluntary$condition == 1, c("subjID", "choice", "RT")]
rep_voluntary<-rep_voluntary[order(rep_voluntary$subjID),]

acctotals_switch_voluntary<-aggregate(choice ~ subjID, data = switch_voluntary, FUN = mean)
perfectsubs_switch_voluntary<-(acctotals_switch_voluntary$subjID[acctotals_switch_voluntary$acc == 2])
acctotals_rep_voluntary<-aggregate(choice ~ subjID, data = rep_voluntary, FUN = mean)
perfectsubs_rep_voluntary<-(acctotals_rep_voluntary$subjID[acctotals_rep_voluntary$acc == 2])

acctotals_switch_explicit<-aggregate(choice ~ subjID, data = switch_explicit, FUN = mean)
perfectsubs_switch_explicit<-(acctotals_switch_explicit$subjID[acctotals_switch_explicit$acc == 2])
acctotals_rep_explicit<-aggregate(choice ~ subjID, data = rep_explicit, FUN = mean)
perfectsubs_rep_explicit<-(acctotals_rep_explicit$subjID[acctotals_rep_explicit$acc == 2])

switch_voluntary<-subset(switch_voluntary, (!subjID %in% perfectsubs_switch_voluntary))
rep_voluntary<-subset(rep_voluntary, (!subjID %in% perfectsubs_rep_voluntary))
switch_explicit<-subset(switch_explicit, (!subjID %in% perfectsubs_switch_explicit))
rep_explicit<-subset(rep_explicit, (!subjID %in% perfectsubs_rep_explicit))


write.table(switch_voluntary, "switch-vol-forDDM-allsubs.txt", row.names = FALSE)
write.table(rep_voluntary, "repeat-vol-forDDM-allsubs.txt", row.names = FALSE)
write.table(switch_explicit, "switch-exp-forDDM-allsubs.txt", row.names = FALSE)
write.table(rep_explicit, "repeat-exp-forDDM-allsubs.txt", row.names = FALSE)



