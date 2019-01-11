setwd("/Users/bryanjackson/Desktop/Mike-modeling")
library(rstan)
library(hBayesDM)

#below is for voluntary
switchddm_vol<-choiceRTNoBeta_ddm("switch-vol-forDDM.txt", ncore = 3, max_treedepth = 20)
repddm_vol<-choiceRTNoBeta_ddm("repeat-vol-forDDM.txt", max_treedepth = 20, ncore = 3)

#below is for explicit
switchddm_explicit<-choiceRTNoBeta_ddm("switch-exp-forDDM.txt", max_treedepth = 20, ncore = 3)
repddm_explicit<-choiceRTNoBeta_ddm("repeat-exp-forDDM.txt", max_treedepth = 20, ncore = 3)

saveRDS(switchddm_vol, "volswitchddm_nobias.rds")
saveRDS(switchddm_explicit, "expswitchdd_nobias.rds")
saveRDS(repddm_explicit, "exprepddm_nobias.rds")
saveRDS(repddm_vol, "volrepddm_nobias.rds")

volswitch<-readRDS("volswitchddm_nobias.rds")
volrep<-readRDS("volrepddm_nobias.rds")
expswitch<-readRDS("expswitchdd_nobias.rds")
exprep<-readRDS("exprepddm_nobias.rds")

max(rhat(volswitch)$Rhat)
max(rhat(volrep)$Rhat)
max(rhat(expswitch)$Rhat)
max(rhat(exprep)$Rhat)
printFit(volswitch, volrep, expswitch, exprep)


volswitchpars<-volswitch$allIndPars
volreppars<-volrep$allIndPars
expswitchpars<-expswitch$allIndPars
expreppars<-exprep$allIndPars


volswitchpars$agency<-"V"
volreppars$agency<-"V"
expswitchpars$agency<-"E"
expreppars$agency<-"E"
volswitchpars$alt<-"S"
volreppars$alt<-"R"
expswitchpars$alt<-"S"
expreppars$alt<-"R"
allpars<-rbind(volswitchpars, volreppars, expswitchpars, expreppars)

summary(aov(delta ~ alt*agency + Error(subjID/(alt*agency)), data = allpars))
aggregate(delta ~ alt + agency, data = allpars, FUN = mean)
summary(aov(tau ~ agency*alt + Error(subjID/(alt*agency)), data = allpars))
aggregate(tau ~ alt + agency, data = allpars, FUN = mean)
summary(aov(alpha ~ agency*alt + Error(subjID/(alt*agency)), data = allpars))
aggregate(alpha ~ alt + agency, data = allpars, FUN = mean)

library(lme4)
library(emmeans)
library(lmerTest)
deltamod<-lmer(delta ~ alt + agency + (1|subjID), data = allpars)
anova(deltamod)
summary(pairs(emmeans(deltamod, ~ alt), adjust = "bonferroni"))
summary(pairs(emmeans(deltamod, ~ agency), adjust = "bonferroni"))

taumod<-lmer(tau ~ alt*agency + (1|subjID), data = allpars)
anova(taumod)
summary(pairs(emmeans(taumod, ~ agency), adjust = "bonferroni"))

alphamod<-lmer(alpha ~ alt*agency + (1|subjID), data = allpars)
anova(alphamod)
summary(pairs(emmeans(alphamod, ~ agency), adjust = "bonferroni"))

volpars<-subset(allpars, agency == "V")


deltamod<-lmer(delta ~ alt + (1|subjID), data = volpars)
anova(deltamod)
summary(pairs(emmeans(deltamod, ~ alt), adjust = "bonferroni"))

taumod<-lmer(tau ~ alt + (1|subjID), data = volpars)
anova(taumod)
summary(pairs(emmeans(taumod, ~ alt), adjust = "bonferroni"))

alphamod<-lmer(alpha ~ alt + (1|subjID), data = volpars)
anova(alphamod)
summary(pairs(emmeans(alphamod, ~ alt), adjust = "bonferroni"))



volpars<-subset(allpars, agency == "V")


summary(aov(delta ~ alt + Error(subjID/(alt)) , data = volpars))
summary(aov(tau ~ alt + Error(subjID/(alt)), data = volpars))
summary(aov(alpha ~ alt + Error(subjID/(alt)), data = volpars))


plot(volswitch, type = "trace")
plot(exprep, type = "trace")
plot(expswitch, type = "trace")
plot(exprep, type = "trace")

voldelta_diff<-volswitch$parVals$mu_delta - volrep$parVals$mu_delta
plotHDI(voldelta_diff)

expdelta_diff<-expswitch$parVals$mu_delta - exprep$parVals$mu_delta
plotHDI(expdelta_diff)

#############

voltau_diff<-volswitch$parVals$mu_tau - volrep$parVals$mu_tau
plotHDI(voltau_diff)

exptau_diff<-expswitch$parVals$mu_tau - exprep$parVals$mu_tau
plotHDI(exptau_diff)

#############

volalpha_diff<-volswitch$parVals$mu_alpha - volrep$parVals$mu_alpha
plotHDI(volalpha_diff)

expalpha_diff<-expswitch$parVals$mu_alpha - exprep$parVals$mu_alpha
plotHDI(expalpha_diff)
