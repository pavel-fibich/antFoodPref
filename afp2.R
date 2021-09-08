rm(list=ls())

#install.packages("ggeffects")
#install.packages("interactions")
#install.packages ("lme4")
#install.packages ("pscl")
# factorial design - 15 replicates
# Site - 3 localities
# Year - 3 years, 2017-9
# Season - 3 times in year
# Treatment - 4 treats, Protein, Oil, Control, Carbohydrate
#+ envi {Mean,Max,Min}.Temperature
# EXPLA
# log(1+picea)
# Visited_picea: 0,1 (possibly aggregate by in to proportion by factors)
# temperature can be used instead of year and season!

# research around
# na pocty glm s poisson distribuci 
# 3way https://www.datanovia.com/en/lessons/anova-in-r/
# 1way anova https://statsandr.com/blog/anova-in-r/
# transformations https://medium.com/analytics-vidhya/a-guide-to-data-transformation-9e5fa9ae1ca3
# posthoc for glm https://www.dataquest.io/blog/tutorial-poisson-regression-in-r/
# nice glm poisson https://www.middleprofessor.com/files/applied-biostatistics_bookdown/_book/generalized-linear-models-i-count-data.html
# negative binomial and zero inflated https://fukamilab.github.io/BIO202/04-C-zero-data.html
# model selection http://www.sthda.com/english/articles/37-model-selection-essentials-in-r/154-stepwise-regression-essentials-in-r/
# visual ggeffects https://strengejacke.github.io/ggeffects/articles/ggeffects.html
# summary se http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/

# get project to git
# Tools - Version control (choose inicialize to git)
# (terminal) git remote add origin https://github.com/pavel-fibich/antFoodPref.git
# (terminal) git branch -M main
# (terminal) git push -u origin main
# choose file to commit in github
# token auth
# install.packages("gitcreds")
if (FALSE) { #just for git commits/pulls
  library(gitcreds)
  system("cat ~/.Renviron")
  gitcreds_set() # paste token
}


#################
# read data set factors
afp<-read.csv("Picea-DATA.csv")
names(afp)
afp$piceal<-log(1+afp$picea)
afp$YearSeason<-paste(afp$Year,afp$Season,sep="_")
afp$SeasonSite<-paste(afp$Season,afp$Site,sep="_")
afp$SiteYear<-paste(afp$Site,afp$Year,sep="_")
allfa<-c("Site","Treatment","Season","Year")
for (i in c(allfa,"YearSeason","SeasonSite","SiteYear")) afp[,i] <- as.factor(afp[,i])
#for (i in c("Site","Treatment","Year")) afp[,i] <- as.factor(afp[,i])
#for (i in c("Season")) afp[,i] <- as.numeric(afp[,i])
afp$Treatment<-relevel(afp$Treatment,"Oil")
afp$Treatment<-relevel(afp$Treatment,"Control")
levels(afp$Treatment)
levels(afp$YearSeason)
table(afp$Visited_picea, afp$picea>0)

library(ggplot2)
theme_set(theme_light())
source("summarySE.R") # source file with summary functions

trcol<-c("blue","red","green","violet") # colors used in plots

########
################# vizualisation
########
afpse<-summarySE(afp, measurevar="picea", groupvars=c("Treatment","Season","Site","Year"))
ggplot(afpse, aes(Season, picea)) + 
  geom_errorbar(aes(ymin=picea-ci,ymax=picea+ci,colour=Treatment ), position=position_dodge(0.5)) +
  geom_point(aes(group=Treatment,color=Treatment,shape=Treatment),position=position_dodge(0.5)) + 
  ggtitle(paste("")) + facet_grid(Year ~ Site)+ 
  scale_color_manual(values = trcol)+theme_light() 
ggsave(paste0("data_ci.pdf"), width = 6, height = 9)
  
afpse<-summarySE(afp, measurevar="piceal", groupvars=c("Treatment","Season","Site","Year"))
ggplot(afpse, aes(factor(Season), piceal))+
  geom_errorbar(aes(ymin=piceal-ci,ymax=piceal+ci,colour=Treatment), position=position_dodge(0.5)) +
  geom_point(aes(group=Treatment,color=Treatment,shape=Treatment),position=position_dodge(0.5)) + 
  ggtitle(paste("log")) + 
  facet_grid(Year ~ Site)+
  scale_color_manual(values = trcol)+theme_light()
ggsave(paste0("data_ci_log.pdf"), width = 6, height = 9)
  
afpse<-summarySE(afp, measurevar="picea", groupvars=c("Treatment","Season","Site","Year"))
ggplot(afpse, aes(factor(Season), picea)) + 
  geom_errorbar(aes(ymin=picea-se,ymax=picea+se,colour=Treatment), position=position_dodge(0.5))+
  geom_point(aes(group=Treatment,color=Treatment,shape=Treatment),position=position_dodge(0.5)) + 
  ggtitle(paste("")) + 
  facet_grid(Year ~ Site) +
  scale_color_manual(values = trcol)+theme_light()
ggsave(paste0("data_se.pdf"), width = 6, height = 9)

afpse<-summarySE(afp, measurevar="piceal", groupvars=c("Treatment","Season","Site","Year"))
ggplot(afpse, aes(factor(Season), piceal))+
  geom_errorbar(aes(ymin=piceal-se,ymax=piceal+se,colour=Treatment), position=position_dodge(0.5)) +
  geom_point(aes(group=Treatment,color=Treatment,shape=Treatment),position=position_dodge(0.5)) + 
  ggtitle(paste("log")) + 
  facet_grid(Year ~ Site)  +
  scale_color_manual(values = trcol)+theme_light()
ggsave(paste0("data_se_log.pdf"), width = 6, height = 9)

afpse<-summarySE(afp, measurevar="Visited_picea", groupvars=c("Treatment","Season","Site","Year"))
ggplot(afpse, aes(Season, Visited_picea)) + 
  geom_errorbar(aes(ymin=Visited_picea-ci,ymax=Visited_picea+ci,colour=Treatment ), position=position_dodge(0.5)) +
  geom_point(aes(group=Treatment,color=Treatment,shape=Treatment),position=position_dodge(0.5)) + 
  ggtitle(paste("")) + facet_grid(Year ~ Site)+
  scale_color_manual(values = trcol)+theme_light()
ggsave(paste0("visdata_ci.pdf"), width = 6, height = 9)

ggplot(afpse, aes(Season, Visited_picea)) + 
  geom_errorbar(aes(ymin=Visited_picea-se,ymax=Visited_picea+se,colour=Treatment ), position=position_dodge(0.5)) +
  geom_point(aes(group=Treatment,color=Treatment,shape=Treatment),position=position_dodge(0.5)) + 
  ggtitle(paste("")) + facet_grid(Year ~ Site)+
  scale_color_manual(values = trcol)+theme_light()
ggsave(paste0("visdata_se.pdf"), width = 6, height = 9)

ggplot(afp, aes(factor(Season), picea))+
  geom_boxplot(aes(colour=Treatment), position=position_dodge(0.5)) +
  stat_summary(fun=mean, aes(group=Treatment,colour=Treatment),shape=8,geom="point", shape=20, size=2,position=position_dodge(.5)) +
  ggtitle(paste("log")) + 
  facet_grid(Year ~ Site)  +
  scale_color_manual(values = trcol)+theme_light()
ggsave(paste0("data_box.pdf"), width = 6, height = 9)

ggplot(afp, aes(factor(Season), piceal))+
  geom_boxplot(aes(colour=Treatment), position=position_dodge(0.5)) +
  stat_summary(fun=mean, aes(group=Treatment,colour=Treatment),shape=8,geom="point", shape=20, size=2,position=position_dodge(.5)) +
  ggtitle(paste("log")) + 
  facet_grid(Year ~ Site)  +
  scale_color_manual(values = trcol)+theme_light()
ggsave(paste0("data_box_log.pdf"), width = 6, height = 9)


#################
###################### no site and year
library(MASS)
library(multcomp)

ang<-glm.nb( picea~Treatment, data=afp)
anova(ang,test="Chisq")
summary(glht(ang,mcp(Treatment="Tukey")),test = adjusted("holm"))

ang<-glm.nb( picea~Season, data=afp)
anova(ang,test="Chisq")
summary(glht(ang,mcp(Season="Tukey")),test = adjusted("holm"))

ang<-glm.nb( picea~Site, data=afp)
anova(ang,test="Chisq")
summary(glht(ang,mcp(Site="Tukey")),test = adjusted("holm"))

ang<-glm.nb( picea~Year, data=afp)
anova(ang,test="Chisq")
summary(glht(ang,mcp(Year="Tukey")),test = adjusted("holm"))




library(interactions)
#################
######### all factors additive
#################
ang<-glm.nb( picea~Treatment+Season+Site+Year, data=afp)
anova(ang,test="Chisq")
#confint(glht(ang,mcp(Treatment="Tukey", interaction_average = T, covariate_average =T)),test = adjusted("holm"))
summary(glht(ang,mcp(Treatment="Tukey", interaction_average = T, covariate_average =T)),test = adjusted("holm"))
summary(glht(ang,mcp(Season="Tukey", interaction_average = T, covariate_average =T)),test = adjusted("holm"))
summary(glht(ang,mcp(Site="Tukey", interaction_average = T, covariate_average =T)),test = adjusted("holm"))
summary(glht(ang,mcp(Year="Tukey", interaction_average = T, covariate_average =T)),test = adjusted("holm"))

library(ggeffects)
library(ggplot2)

#ang<-glm.nb( picea~Treatment+Season+Site, data=afp)
#mydf <- ggpredict(ang, terms = c("Season","Treatment","Site"))
#ggplot(mydf)


ony=2017
mydf<-NULL
for( ony in c(2017:2019)){ # fitting year separately but making one data.frame from the resutls
  ang<-glm.nb( picea~Treatment+Site+Season, data=afp[ afp$Year ==ony,])
  if (is.null(mydf)){
    mydf <- ggpredict(ang, terms = c("Season","Treatment","Site"))
    mydf$Year<-ony
  }else{
    mydfx <- ggpredict(ang, terms = c("Season","Treatment","Site"))
    mydfx$Year<-ony
    mydf<-rbind(mydf,mydfx)
  }
}

plot(mydf) +facet_grid(Year~facet) +scale_color_manual(values = trcol) + theme_light()
ggsave(paste0("ifacs2years.pdf"), width = 6, height = 9)
plot(mydf) +facet_grid(Year~facet) +scale_color_manual(values = trcol) + theme_light()+ scale_y_log10() + ylab("log10 picea")
ggsave(paste0("ifacs2years_log.pdf"), width = 6, height = 9)



#################
####### focal treatment and season
#################
ang<-glm.nb( picea~Treatment*Season, data=afp)
anova(ang,test="Chisq")
summary(glht(ang,mcp(Treatment="Tukey", interaction_average = T, covariate_average =T)),test = adjusted("holm"))
summary(glht(ang,mcp(Season="Tukey", interaction_average = T, covariate_average =T)),test = adjusted("holm"))

mydf <- ggpredict(ang, terms = c("Season","Treatment"))
plot(mydf) + scale_color_manual(values = trcol) + theme_light()#+ ggtitle(ony) #+ scale_y_log10() + ylab("log10 picea")
ggsave(paste0("trse2_.pdf"), width = 4, height = 4)
plot(mydf) + scale_color_manual(values = trcol) + theme_light()+ scale_y_log10() + ylab("log10 picea")
ggsave(paste0("trse2_log.pdf"), width = 4, height = 4)
#pdf("trse.pdf")
#cat_plot(ang,pred=Season,modx=Treatment, data=afp)#[afp$Year == "2018",] )
#dev.off()


ang<-glm.nb( picea~Treatment*Season, data=afp[afp$Site=="BK",])
anova(ang,test="Chisq")
#pdf("trseBK.pdf")
#cat_plot(ang,pred=Season,modx=Treatment, data=afp[afp$Site=="BK",])#[afp$Year == "2018",] )
#dev.off()
mydf <- ggpredict(ang, terms = c("Season","Treatment"))
plot(mydf) + scale_color_manual(values = trcol) + theme_light()+ ggtitle("BK") #+ scale_y_log10() + ylab("log10 picea")
ggsave("trseBK2.pdf", width = 4, height = 4)
mydf <- ggpredict(ang, terms = c("Season","Treatment"))
plot(mydf) + scale_color_manual(values = trcol) + theme_light()+ ggtitle("BK") + scale_y_log10() + ylab("log10 picea")
ggsave("trseBK2_log.pdf", width = 4, height = 4)

mydf <- ggemmeans(ang, terms = c("Season","Treatment"))
plot(mydf) + ggtitle("BK") #+ scale_y_log10() + ylab("log10 picea")



ang<-glm.nb( picea~Treatment+Season, data=afp[afp$Site=="Rad",])
ang<-glm.nb( picea~Season+Treatment, data=afp[afp$Site=="Rad",])
anova(ang,test="Chisq")
#pdf("trseRad.pdf")
#cat_plot(ang,pred=Season,modx=Treatment, data=afp[afp$Site=="Rad",])#[afp$Year == "2018",] )
#dev.off()
mydf <- ggpredict(ang, terms = c("Season","Treatment"))
plot(mydf) + scale_color_manual(values = trcol) + theme_light()+ ggtitle("Rad") #+ scale_y_log10() + ylab("log10 picea")
ggsave("trseRad2.pdf", width = 4, height = 4)
plot(mydf) + scale_color_manual(values = trcol) + theme_light()+ ggtitle("Rad") + scale_y_log10() + ylab("log10 picea")
ggsave("trseRad2_log.pdf", width = 4, height = 4)

ang<-glm.nb( picea~Treatment*Season, data=afp[afp$Site=="Jan",])
anova(ang,test="Chisq")
#pdf("trseJan.pdf")
#cat_plot(ang,pred=Season,modx=Treatment, data=afp[afp$Site=="Jan",])#[afp$Year == "2018",] )
#dev.off()
mydf <- ggpredict(ang, terms = c("Season","Treatment"))
plot(mydf) + scale_color_manual(values = trcol) + theme_light()+ ggtitle("Jan") #+ scale_y_log10() + ylab("log10 picea")
ggsave("trseJan2.pdf", width = 4, height = 4)
plot(mydf) + scale_color_manual(values = trcol) + theme_light()+ ggtitle("Jan") + scale_y_log10() + ylab("log10 picea")
ggsave("trseJan2_log.pdf", width = 4, height = 4)


#################
# mixed models
#- with Temperature
library(car)
anm0<-glm.nb( picea~ Treatment*Season,data=afp)
anm<-glmer.nb( picea~ Treatment*Season +(1|Mean.Temperature),data=afp)
anova(anm,anm0, test="Chisq")

#- with Myrmica
library(car)
anm0<-glm.nb( picea~ Treatment*Season,data=afp)
anm<-glmer.nb( picea~ Treatment*Season +(1|Myrmica),data=afp)
anova(anm,anm0, test="Chisq")








################################## 
#### visited (binary)
##################################
avp<-glm( Visited_picea~Treatment,data=afp, family="binomial")
anova(avp,test="Chisq")
summary(glht(avp,mcp(Treatment="Tukey")),test = adjusted("holm"))

avp<-glm( Visited_picea~Season,data=afp, family="binomial")
anova(avp,test="Chisq")
summary(glht(avp,mcp(Season="Tukey")),test = adjusted("holm"))

avp<-glm( Visited_picea~Year,data=afp, family="binomial")
anova(avp,test="Chisq")
summary(glht(avp,mcp(Year="Tukey")),test = adjusted("holm"))

avp<-glm( Visited_picea~Site,data=afp, family="binomial")
anova(avp,test="Chisq")
summary(glht(avp,mcp(Site="Tukey")),test = adjusted("holm"))

#################
# focal model
avp<-glm( Visited_picea~Treatment*Season,data=afp, family="binomial")
anova(avp,test="Chisq")
mydf <- ggpredict(avp, terms = c("Season","Treatment"))
plot(mydf) +scale_color_manual(values = trcol) + theme_light()#+ scale_y_log10() + ylab("log10 picea")
ggsave(paste0("vis_focal.pdf"), width = 4, height = 4)

antm0<-glm( Visited_picea~ Treatment*Season,data=afp,family="binomial")
antm<-glmer( Visited_picea~ Treatment*Season + (1|Visited_Myrmica),data=afp,family="binomial")
anova(antm,antm0,test="Chisq");

summary(glht(antm0,mcp(Treatment="Tukey", interaction_average = T, covariate_average =T)),test = adjusted("holm"))
summary(glht(antm0,mcp(Season="Tukey", interaction_average = T, covariate_average =T)),test = adjusted("holm"))

summary(glht(antm,mcp(Treatment="Tukey", interaction_average = T, covariate_average =T)),test = adjusted("holm"))
summary(glht(antm,mcp(Season="Tukey", interaction_average = T, covariate_average =T)),test = adjusted("holm"))


#################
# mixed models
#-with Temperature
library(car)
anm0<-glm( Visited_picea~ Treatment*Season,data=afp, family="binomial")
anm<-glmer( Visited_picea~ Treatment*Season +(1|Site.Temperature),data=afp, family="binomial")
anova(anm,anm0, test="Chisq")

#- with Myrmica
anm0<-glm( Visited_picea~ Treatment*Season,data=afp, family="binomial")
anm<-glmer( Visited_picea~ Treatment*Season +(1|Visited_Myrmica),data=afp, family="binomial")
anova(anm,anm0, test="Chisq")
Anova(anm)
mydf <- ggpredict(anm, terms = c("Season","Treatment"))
plot(mydf) +scale_color_manual(values = trcol) + theme_light()#+ scale_y_log10() + ylab("log10 picea")
ggsave(paste0("vis_focalMyrmica.pdf"), width = 4, height = 4)
summary(glht(anm,mcp(Treatment="Tukey", interaction_average = T, covariate_average =T)),test = adjusted("holm"))
summary(glht(anm,mcp(Season="Tukey", interaction_average = T, covariate_average =T)),test = adjusted("holm"))




#################
# bigger model
an0<-glm.nb( picea~ Treatment+Season+Site+Year+
             Treatment:Season+Treatment:Site + Treatment:Year #+ Site:Year
            +Site:Season + Site:Year + Season:Year 
            ,data=afp)
anova(an0,test="Chisq")

# mixed model with Visited_Myrmica
library(lme4)
anm<-glmer.nb( picea~ Treatment+Site+Season+Year
             +Treatment:Site + Treatment:Season + Treatment:Year #+ Site:Year
             +Site:Season + Site:Year + Season:Year + (1|Visited_Myrmica) 
             #+Treatment:Site:Season + Treatment:Season:Year + Treatment:Site:Year
             #+Season:Year+Site:Season
             ,data=afp)
library(car)
Anova(anm)

ant<-glm.nb( picea~ Treatment+Site+Season+Year+Site.Temperature
             +Treatment:Site + Treatment:Season + Treatment:Year #+ Site:Year
             +Site:Season + Site:Year + Season:Year 
             #+Treatment:Site:Season + Treatment:Season:Year + Treatment:Site:Year
             #+Season:Year+Site:Season
             ,data=afp)
anova(ant,test="Chisq")

mydf <- ggpredict(avt, terms = c("Season","Treatment"))
plot(mydf) + ggtitle(onsite) #+ scale_y_log10() + ylab("log10 picea")
ggsave(paste0(onsite,"_vis.pdf"), width = 4, height = 4)
anova(an0,ant)

an<-glm.nb( picea~ Treatment+Season+Site+Year+
            Treatment:Season+Treatment:Site + Treatment:Year + Site:Year
            ,data=afp)
anova(an,test="Chisq")
anova(an,an0)

library(pscl)
az<-zeroinfl( picea ~ Treatment+Site+Season+Year+
                Treatment:Site + Treatment:Season + Treatment:Year
              +Site:Season + Site:Year + Season:Year  | Treatment,data=afp, dist="negbin")#,control = zeroinfl.control(method="CG"))

mydf <- ggpredict(az, terms = c("Season","Treatment","Site"))
plot(mydf)
mydf <- ggpredict(an0, terms = c("Season","Treatment","Site"))
plot(mydf)
summary(glht(an0,mcp(Season="Tukey", interaction_average = T, covariate_average =T)),test = adjusted("holm"))
#summary(glht(az,mcp(Season="Tukey", interaction_average = T, covariate_average =T)),test = adjusted("holm"))


ony=2017
mydf<-NULL
for( ony in c(2017:2019)){
  ang<-glm.nb( picea~Treatment+Site+Season
               +Treatment:Site + Treatment:Season, data=afp[ afp$Year ==ony,])
  if (is.null(mydf)){
    mydf <- ggpredict(ang, terms = c("Season","Treatment","Site"))
    mydf$Year<-ony
  }else{
    mydfx <- ggpredict(ang, terms = c("Season","Treatment","Site"))
    mydfx$Year<-ony
    mydf<-rbind(mydf,mydfx)
  }
}

plot(mydf) +facet_grid(Year~facet) +scale_color_manual(values = trcol) + theme_light()
ggsave(paste0("bigyears.pdf"), width = 6, height = 9)
plot(mydf) +facet_grid(Year~facet) +scale_color_manual(values = trcol) + theme_light()+ scale_y_log10() + ylab("log10 picea")
ggsave(paste0("bigyears_log.pdf"), width = 6, height = 9)

#plot(mydf) + scale_color_manual(values = trcol) + theme_light()+ ggtitle(ony) #+ scale_y_log10() + ylab("log10 picea")
#ggsave(paste0(ony,".pdf"), width = 4, height = 4)

  

# pdf("2017.pdf")
# cat_plot(an,pred=Site,modx=Treatment,mod2=Season, data=afp[ afp$Year =="2017",] )
# dev.off()
# pdf("2018.pdf")
# cat_plot(an,pred=Site,modx=Treatment,mod2=Season, data=afp[ afp$Year =="2018",] )
# dev.off()
# pdf("2019.pdf")
# cat_plot(an,pred=Site,modx=Treatment,mod2=Season, data=afp[ afp$Year =="2019",] )
# dev.off()


summary(glht(an,mcp(Treatment="Tukey", interaction_average = T, covariate_average =T)),test = adjusted("holm"))
summary(glht(an,mcp(Season="Tukey", interaction_average = T, covariate_average =T)),test = adjusted("holm"))
summary(glht(an,mcp(Year="Tukey", interaction_average = T, covariate_average =T)),test = adjusted("holm"))
summary(glht(an,mcp(Site="Tukey", interaction_average = T, covariate_average =T)),test = adjusted("holm"))


#################
###################### indiv localities
onsite="BK";
#ans<-glm.nb( picea~Treatment, data=afp[ afp$Site == onsite,])
#anova(ans,test="Chisq")
onsite="BK";ans<-glm.nb( picea~Treatment+Season+Year+Treatment:Season+Treatment:Year+Season:Year, data=afp[ afp$Site == onsite,])
anova(ans,test="Chisq")
onsite="BK";ans<-glm.nb( picea~Treatment+Season+Year+Treatment:Season+Treatment:Year, data=afp[ afp$Site == onsite,])
summary(glht(ans,mcp(Treatment="Tukey", interaction_average = T, covariate_average =T)),test = adjusted("holm"))
summary(glht(ans,mcp(Season="Tukey", interaction_average = T, covariate_average =T)),test = adjusted("holm"))
summary(glht(ans,mcp(Year="Tukey", interaction_average = T, covariate_average =T)),test = adjusted("holm"))
onsite="BK";ans<-glm.nb( picea~Treatment+Season+Year+Treatment:Season+Treatment:Year, data=afp[ afp$Site == onsite,])
#pdf("BK.pdf")
#cat_plot(ans,pred=Season,modx=Treatment,mod2=Year)
#dev.off()
mydf <- ggpredict(ans, terms = c("Season","Treatment","Year"))
plot(mydf) + ggtitle(onsite) +scale_color_manual(values = trcol) + theme_light()#+ scale_y_log10() + ylab("log10 picea")
ggsave(paste0(onsite,".pdf"), width = 4, height = 4)
mydf <- ggpredict(ans, terms = c("Season","Treatment","Year"))
plot(mydf) + ggtitle(onsite) + scale_y_log10() + ylab("log10 picea")+scale_color_manual(values = trcol) + theme_light()
ggsave(paste0(onsite,"_log.pdf"), width = 4, height = 4)



onsite="Rad";
ans<-glm.nb( picea~Treatment+Season+Year, data=afp[ afp$Site == onsite,])
onsite="Rad";ans<-glm.nb( picea~Treatment+Season+Year+Treatment:Season+Treatment:Year+Season:Year, data=afp[ afp$Site == onsite,])
#ans<-glm.nb( picea~Treatment*Season, data=afp[ afp$Site == onsite,])
anova(ans,test="Chisq")
onsite="Rad";ans<-glm.nb( picea~Treatment+Season+Year+Treatment:Year+Season:Year, data=afp[ afp$Site == onsite,])
anova(ans,test="Chisq")

summary(glht(ans,mcp(Treatment="Tukey", interaction_average = T, covariate_average =T)),test = adjusted("holm"))
summary(glht(ans,mcp(Season="Tukey", interaction_average = T, covariate_average =T)),test = adjusted("holm"))
summary(glht(ans,mcp(Year="Tukey", interaction_average = T, covariate_average =T)),test = adjusted("holm"))
#pdf("Rad.pdf")
#cat_plot(ans,pred=Season,modx=Treatment,mod2=Year)
#dev.off()
mydf <- ggpredict(ans, terms = c("Season","Treatment","Year"))
plot(mydf) + ggtitle(onsite) +scale_color_manual(values = trcol) + theme_light()#+ scale_y_log10() + ylab("log10 picea")
ggsave(paste0(onsite,".pdf"), width = 4, height = 4)
mydf <- ggpredict(ans, terms = c("Season","Treatment","Year"))
plot(mydf) + ggtitle(onsite) + scale_y_log10() + ylab("log10 picea")+scale_color_manual(values = trcol) + theme_light()
ggsave(paste0(onsite,"_log.pdf"), width = 4, height = 4)


onsite="Jan";
ans<-glm.nb( picea~Treatment+Season+Year, data=afp[ afp$Site == onsite,])
anova(ans,test="Chisq")
onsite="Jan";ans<-glm.nb( picea~Treatment+Season+Year+Treatment:Season+Treatment:Year+Season:Year, data=afp[ afp$Site == onsite,])

#ans<-glm.nb( picea~Treatment+Season+Year+
            #                Treatment:Season+Treatment:Year+Season:Year, data=afp[ afp$Site == onsite,])

anova(ans,test="Chisq")
summary(glht(ans,mcp(Treatment="Tukey", interaction_average = T, covariate_average =T)),test = adjusted("holm"))
summary(glht(ans,mcp(Season="Tukey", interaction_average = T, covariate_average =T)),test = adjusted("holm"))
summary(glht(ans,mcp(Year="Tukey", interaction_average = T, covariate_average =T)),test = adjusted("holm"))
#pdf("Jan.pdf")
#cat_plot(ans,pred=Season,modx=Treatment,mod2=Year)
#dev.off()
mydf <- ggpredict(ans, terms = c("Season","Treatment","Year"))
plot(mydf) + ggtitle(onsite) +scale_color_manual(values = trcol) + theme_light()#+ scale_y_log10() + ylab("log10 picea")
ggsave(paste0(onsite,".pdf"), width = 4, height = 4)
mydf <- ggpredict(ans, terms = c("Season","Treatment","Year"))
plot(mydf) + ggtitle(onsite) + scale_y_log10() + ylab("log10 picea")+scale_color_manual(values = trcol) + theme_light()
ggsave(paste0(onsite,"_log.pdf"), width = 4, height = 4)


#################
######################### visited
############ visited_picea
avp<-glm( Visited_picea~Treatment+Season+Site+Year,data=afp, family="binomial")
anova(avp,test="Chisq")
avp<-glm( Visited_picea~Treatment+Season+Site+Year+
            Treatment:Season+Treatment:Site + Treatment:Year+
            Season:Site+Year:Site+Year:Season
          ,data=afp, family="binomial")
anova(avp,test="Chisq")
avp<-glm( Visited_picea~Treatment+Season+Site+Year#+
            #+Treatment:Site + 
            +Treatment:Season #+ Treatment:Year+
            #Year:Season+Year:Site+Season:Site
          ,data=afp, family="binomial")
anova(avp,test="Chisq")
summary(glht(avp,mcp(Treatment="Tukey", interaction_average = T, covariate_average =T)),test = adjusted("holm"))
summary(glht(avp,mcp(Season="Tukey", interaction_average = T, covariate_average =T)),test = adjusted("holm"))
summary(glht(avp,mcp(Site="Tukey", interaction_average = T, covariate_average =T)),test = adjusted("holm"))
summary(glht(avp,mcp(Year="Tukey", interaction_average = T, covariate_average =T)),test = adjusted("holm"))

#pdf("vispic.pdf")
#cat_plot(avp,pred=Site,modx=Treatment,mod2=Season)#,int.type="prediction")
#dev.off()
#anova(an,test="Chisq")
mydf <- ggpredict(avp, terms = c("Season","Treatment","Site"))
plot(mydf) +scale_color_manual(values = trcol) + theme_light()#+ ggtitle(onsite) #+ scale_y_log10() + ylab("log10 picea")
ggsave(paste0("vispic.pdf"), width = 4, height = 4)


###################### indiv localities
onsite="BK";avp<-glm( Visited_picea~Season+Year+Treatment,data=afp[ afp$Site == onsite,], family="binomial")
anova(avp,test="Chisq")
avp<-glm( Visited_picea~Season+Treatment+Year+
            Treatment:Season + Treatment:Year+Year:Season
          ,data=afp[ afp$Site == onsite,], family="binomial")
anova(avp,test="Chisq")
avp<-glm( Visited_picea~Season+Treatment#+Year#+
          #+Treatment:Site + 
          +Treatment:Season #+ Treatment:Year+
          #Year:Season+Year:Site+Season:Site
          ,data=afp[ afp$Site == onsite,], family="binomial")
anova(avp,test="Chisq")
summary(glht(avp,mcp(Treatment="Tukey", interaction_average = T, covariate_average =T)),test = adjusted("holm"))
summary(glht(avp,mcp(Season="Tukey", interaction_average = T, covariate_average =T)),test = adjusted("holm"))

#pdf("BK_vis.pdf")
#cat_plot(avp,pred=Season,modx=Treatment,mod2=Year)
#dev.off()
mydf <- ggpredict(avp, terms = c("Season","Treatment"))
plot(mydf) + ggtitle(onsite) +scale_color_manual(values = trcol) + theme_light()#+ scale_y_log10() + ylab("log10 picea")
ggsave(paste0(onsite,"_vis.pdf"), width = 4, height = 4)

#PETR: I added the Fig with season*Treatment for RAD as intesresting that it does nt vary sign.!
mydf <- ggpredict(avp, terms = c("Season","Treatment"))
plot(mydf) + ggtitle(onsite) +scale_color_manual(values = trcol) + theme_light()#+ scale_y_log10() + ylab("log10 picea")
ggsave(paste0(onsite,"_vis-season.pdf"), width = 4, height = 4)


onsite="Rad";avp<-glm( Visited_picea~Season+Year+Treatment,data=afp[ afp$Site == onsite,], family="binomial")
anova(avp,test="Chisq")
avp<-glm( Visited_picea~Season+Treatment+Year+
            Treatment:Season + Treatment:Year+Year:Season
          ,data=afp[ afp$Site == onsite,], family="binomial")
anova(avp,test="Chisq")
avp<-glm( Visited_picea~Season+Treatment+Year#+
          #+Treatment:Site #+ 
#          +Treatment:Season #+ Treatment:Year+
          #Year:Season+Year:Site+Season:Site
          ,data=afp[ afp$Site == onsite,], family="binomial")
anova(avp,test="Chisq")
summary(glht(avp,mcp(Treatment="Tukey", interaction_average = T, covariate_average =T)),test = adjusted("holm"))
summary(glht(avp,mcp(Season="Tukey", interaction_average = T, covariate_average =T)),test = adjusted("holm"))
summary(glht(avp,mcp(Year="Tukey", interaction_average = T, covariate_average =T)),test = adjusted("holm"))



#pdf("Rad_vis.pdf")
#cat_plot(avp,pred=Season,modx=Treatment,mod2=Year)
#dev.off()
mydf <- ggpredict(avp, terms = c("Year","Treatment"))
plot(mydf) + ggtitle(onsite) +scale_color_manual(values = trcol) + theme_light()#+ scale_y_log10() + ylab("log10 picea")
ggsave(paste0(onsite,"_vis.pdf"), width = 4, height = 4)
mydf <- ggpredict(avp, terms = c("Season","Treatment"))
plot(mydf) + ggtitle(onsite) +scale_color_manual(values = trcol) + theme_light()#+ scale_y_log10() + ylab("log10 picea")
ggsave(paste0(onsite,"_vis-season.pdf"), width = 4, height = 4)




onsite="Jan";avp<-glm( Visited_picea~Season+Year+Treatment,data=afp[ afp$Site == onsite,], family="binomial")
anova(avp,test="Chisq")
avp<-glm( Visited_picea~Season+Treatment+Year+
            Treatment:Season + Treatment:Year+Year:Season
          ,data=afp[ afp$Site == onsite,], family="binomial")
anova(avp,test="Chisq")
avp<-glm( Visited_picea~Season+Treatment+Year#+
          #+Treatment:Site + 
          +Treatment:Season #+ Treatment:Year#+
          #Year:Season+Year:Site+Season:Site
          ,data=afp[ afp$Site == onsite,], family="binomial")
anova(avp,test="Chisq")

summary(glht(avp,mcp(Treatment="Tukey", interaction_average = T, covariate_average =T)),test = adjusted("holm"))
summary(glht(avp,mcp(Season="Tukey", interaction_average = T, covariate_average =T)),test = adjusted("holm"))
summary(glht(avp,mcp(Year="Tukey", interaction_average = T, covariate_average =T)),test = adjusted("holm"))

#pdf("Jan_vis.pdf")
#cat_plot(avp,pred=Season,modx=Treatment,mod2=Year)
#dev.off()
mydf <- ggpredict(avp, terms = c("Season","Treatment"))
plot(mydf) + ggtitle(onsite) +scale_color_manual(values = trcol) + theme_light()#+ scale_y_log10() + ylab("log10 picea")
ggsave(paste0(onsite,"_vis.pdf"), width = 4, height = 4)







##################################
# temperature
##################################
# site temp
ants<-glm.nb( picea~ Site.Temperature,data=afp)
anova(ants)
ants<-glm( Visited_picea~ Site.Temperature,data=afp,family="binomial")
anova(ants,test="Chisq")

# mean temp
antm<-glm.nb( picea~ Mean.Temperature,data=afp)
anova(antm)
ants<-glm( Visited_picea~ Mean.Temperature,data=afp,family="binomial")
anova(ants,test="Chisq")


# picea
antx<-glm.nb( picea~ Treatment*Season,data=afp)
ants<-glm.nb( picea~ Treatment*Site.Temperature,data=afp)
anova(ants)
anova(antx)
anova(ants,antx)

# mean temp
antm<-glm.nb( picea~ Treatment+Mean.Temperature,data=afp)
anova(antm)
mydf <- ggpredict(antm, terms = c("Mean.Temperature","Treatment"))
plot(mydf) + ggtitle("Mean.Temperature") + scale_color_manual(values = trcol) + theme_light()#+ scale_y_log10() + ylab("log10 picea")
ggsave(paste0("temp.pdf"), width = 4, height = 4)
plot(mydf) + ggtitle("Mean.Temperature") + scale_y_log10() + ylab("log10 picea") +scale_color_manual(values = trcol) + theme_light()
ggsave(paste0("temp_log.pdf"), width = 4, height = 4)


antm<-glm.nb( picea~ Treatment*Mean.Temperature,data=afp)
anova(antm)
mydf <- ggpredict(antm, terms = c("Mean.Temperature","Treatment"))
plot(mydf) + ggtitle("Mean.Temperature") + scale_color_manual(values = trcol) + theme_light()#+ scale_y_log10() + ylab("log10 picea")
ggsave(paste0("tempint.pdf"), width = 4, height = 4)
plot(mydf) + ggtitle("Mean.Temperature") + scale_y_log10() + ylab("log10 picea") +scale_color_manual(values = trcol) + theme_light()
ggsave(paste0("tempint_log.pdf"), width = 4, height = 4)


# not significant
antm<-glm.nb( picea~ Mean.Temperature,data=afp)
anova(antm)
mydf <- ggpredict(antm, terms = c("Mean.Temperature"))
plot(mydf) + ggtitle("Mean.Temperature") + scale_color_manual(values = trcol) + theme_light()
ggsave(paste0("temp_allt.pdf"), width = 4, height = 4)

antm<-glm.nb( picea~ Mean.Temperature,data=afp[afp$Treatment == "Control",])
anova(antm)
antm
mydf <- ggpredict(antm, terms = c("Mean.Temperature"))
plot(mydf) + ggtitle("Mean.Temperature, just Control") + scale_color_manual(values = trcol) + theme_light()
ggsave(paste0("temp_alltControl.pdf"), width = 4, height = 4)

antm<-glm.nb( picea~ Mean.Temperature,data=afp[afp$Treatment != "Control",])
anova(antm)
antm
mydf <- ggpredict(antm, terms = c("Mean.Temperature"))
plot(mydf) + ggtitle("Mean.Temperature, just Non Control") + scale_color_manual(values = trcol) + theme_light()
ggsave(paste0("temp_alltNonControl.pdf"), width = 4, height = 4)


antmo<-glm.nb( picea~ Mean.Temperature+Treatment+Season,data=afp[afp$Site =="Rad",])
anova(antmo)
mydf <- ggpredict(antmo, terms = c("Mean.Temperature","Treatment","Season"))
plot(mydf) + ggtitle("Rad")+ scale_color_manual(values = trcol) + theme_light()#+ scale_y_log10() + ylab("log10 picea")
ggsave(paste0("temp_Rad.pdf"), width = 4, height = 4)
plot(mydf) + ggtitle("Rad")+ scale_color_manual(values = trcol) + theme_light()+ scale_y_log10() + ylab("log10 picea")
ggsave(paste0("templogy_Rad.pdf"), width = 4, height = 4)


# PETR: picea controlled for Myrmcia abundances as covariate
ants0<-glm.nb( picea~ Treatment*Site.Temperature,data=afp)
ants<-glmer.nb( picea~ Treatment*Site.Temperature + (1|Myrmica),data=afp)
anova(ants,ants0,test="Chisq");

antm0<-glm.nb( picea~ Treatment*Mean.Temperature,data=afp)
antm<-glmer.nb( picea~ Treatment*Mean.Temperature + (1|Myrmica),data=afp)
anova(antm,antm0,test="Chisq");

summary(ants)
anova(antm)
summary(antm)
#works but gives only F values, not p-values (would need run probably seq. multiple models and compare one ot each other by AIC or p value compared to NULL model)



# Visited_picea
antm<-glm( Visited_picea~ Treatment*Mean.Temperature,data=afp,family="binomial")
anova(antm,test="Chisq")
antm


#mydf <- ggpredict(ants, terms = c("Site.Temperature"))
#plot(mydf) + ggtitle("Site.Temperature") + scale_color_manual(values = trcol) + theme_light()#+ scale_y_log10() + ylab("log10 picea")
#ggsave(paste0("sitetemp_all.pdf"), width = 4, height = 4)

ants<-glm( Visited_picea~Site.Temperature,data=afp,family="binomial")
anova(ants,test="Chisq")
mydf <- ggpredict(ants, terms = c("Site.Temperature"))
plot(mydf) + ggtitle("Site.Temperature") + scale_color_manual(values = trcol) + theme_light()
ggsave(paste0("sittemp_allt.pdf"), width = 4, height = 4)

antst<-glm( Visited_picea~ Site.Temperature,data=afp[afp$Treatment !="Control",],family="binomial")
anova(antst,test="Chisq")
antst
mydf <- ggpredict(antst, terms = c("Site.Temperature"))
plot(mydf) + ggtitle("Site.Temperature, just non Control") + scale_color_manual(values = trcol) + theme_light()
ggsave(paste0("sittemp_alltNonControl.pdf"), width = 4, height = 4)

antst<-glm( Visited_picea~ Site.Temperature,data=afp[afp$Treatment =="Control",],family="binomial")
anova(antst,test="Chisq")
antst
mydf <- ggpredict(antst, terms = c("Site.Temperature"))
plot(mydf) + ggtitle("Site.Temperature, just Control") + scale_color_manual(values = trcol) + theme_light()
ggsave(paste0("sittemp_alltControl.pdf"), width = 4, height = 4)



ants<-glm( Visited_picea~ Treatment*Site.Temperature,data=afp,family="binomial")
anova(ants,test="Chisq")
mydf <- ggpredict(ants, terms = c("Site.Temperature","Treatment"))
plot(mydf) + ggtitle("Site.Temperature") + scale_color_manual(values = trcol) + theme_light()#+ scale_y_log10() + ylab("log10 picea")
ggsave(paste0("sitetemp.pdf"), width = 4, height = 4)
plot(mydf) + ggtitle("Site.Temperature") + scale_y_log10() + ylab("log10 picea") +scale_color_manual(values = trcol) + theme_light()
ggsave(paste0("site_log.pdf"), width = 4, height = 4)

antst<-glm( Visited_picea~ Site.Temperature,data=afp[afp$Treatment == "Control",],family="binomial")
anova(antst,test="Chisq")
antst<-glm( Visited_picea~ Site.Temperature,data=afp[afp$Treatment == "Protein",],family="binomial")
anova(antst,test="Chisq")
antst<-glm( Visited_picea~ Site.Temperature,data=afp[afp$Treatment == "Carbohydrate",],family="binomial")
anova(antst,test="Chisq")

antst<-glm( Visited_picea~ Site.Temperature,data=afp[afp$Treatment != "Control",],family="binomial")
anova(antst,test="Chisq")

afp$Control<-ifelse(afp$Treatment == "Control",TRUE,FALSE)
#ants<-glm( Visited_picea~ Control*Site.Temperature,data=afp,family="binomial")
#anova(ants,test="Chisq")
ants<-glmer( Visited_picea~ Site.Temperature+(1|Control),data=afp,family="binomial")
Anova(ants)

ants0<-glm( Visited_picea~ Treatment*Site.Temperature,data=afp,family="binomial")
ants<-glmer( Visited_picea~ Treatment*Site.Temperature + (1|Visited_Myrmica),data=afp,family="binomial")
anova(ants,ants0,test="Chisq");

antm0<-glm( Visited_picea~ Treatment*Mean.Temperature,data=afp,family="binomial")
antm<-glmer( Visited_picea~ Treatment*Mean.Temperature + (1|Visited_Myrmica),data=afp,family="binomial")
anova(antm,antm0,test="Chisq");



###################
# beta regression
#library(betareg)
#betareg(Visited_picea~Treatment+Site+Season+Year,data=afp)
