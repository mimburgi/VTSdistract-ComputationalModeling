library(rstan)
library(hBayesDM)
library(here)

#number of times the model will be run
numIter = 2

#probability of changing to response incorrect, changing RT to mean
probchange = .05

#calculate mean RT across everything
alltrials <- read.table("voluntarytDCS-forDDM-allsubs.txt", header = TRUE)
meanRT<-mean(alltrials$RT)


for (i in 1:numIter){
  changetrials = TRUE #this gets changed to false if the loop changes enough that we can use everyone
  tmpdf<-alltrials #this will be edited, broken up, saved out and fed into hBayesDM on each iteration
  while (changetrials == TRUE){ 
  for (trialNum in length(tmpdf$choice)){
    if (runif(1, 0.0, 1.0) < probchange){ #generates a random number, changes trial if numebr is less than prob change
      tmpdf[i,"choice"]<-1
      tmpdf[i,"RT"]<-meanRT
    }#end if runif
  }#end for trial
  testacc<-aggregate(choice ~ subjID + condition + area, data = tmpdf, FUN = mean)
  if (max(testacc$choice) < 2){changetrials = FALSE} #if all participants have under 100% acc for each condition
  }#end while changetrials
  
  #create dfs for input to hbayesdm
  acc_an <- subset(tmpdf, condition == "A" & area == "AC")
  acc_an_sham<-subset(tmpdf, condition)
  acc_ca <- subset(tmpdf, condition == "C" & area == "C")
  
  
}#end for i in numIter

#below is for voluntary
switchddm_vol<-choiceRTNoBeta_ddm("switch-vol-forDDM.txt", ncore = 3, max_treedepth = 20)
repddm_vol<-choiceRTNoBeta_ddm("repeat-vol-forDDM.txt", max_treedepth = 20, ncore = 3)

#below is for explicit
switchddm_explicit<-choiceRTNoBeta_ddm("switch-exp-forDDM.txt", max_treedepth = 20, ncore = 3)
repddm_explicit<-choiceRTNoBeta_ddm("repeat-exp-forDDM.txt", max_treedepth = 20, ncore = 3)

#save parameters
volswitchpars<-volswitch$allIndPars
volreppars<-volrep$allIndPars
expswitchpars<-expswitch$allIndPars
expreppars<-exprep$allIndPars