#################
# R code for reproducing analyses and Figures 
# by Pavel Fibich and Petr Klimes
# for the manuscript 
#Feeding preferences of the black bog ant vary due to climatic and nutrient-limitation effects  

rm(list=ls())

if (FALSE) { #just for git commits/pulls
  library(gitcreds)
  system("cat ~/.Renviron")
  gitcreds_set() # paste token
}

#################
# load packages
library(ggplot2)
library(ggpubr)
library(MASS)
library(ggeffects)
library(egg)
library(car)
library(multcomp)
library (modEvA)


#################
# read data set factors
afp<-read.csv("Picea-DATA.csv")
names(afp)
afp$piceal<-log(1+afp$picea)
afp$picea1<-1+afp$picea
afp$YearSeason<-paste(afp$Year,afp$Season,sep="_")
afp$SeasonSite<-paste(afp$Season,afp$Site,sep="_")
afp$SiteYear<-paste(afp$Site,afp$Year,sep="_")
allfa<-c("Site","Treatment","Season","Year")
for (i in c(allfa,"YearSeason","SeasonSite","SiteYear")) afp[,i] <- as.factor(afp[,i])
afp$Treatment<-relevel(afp$Treatment,"Oil")
afp$Treatment<-relevel(afp$Treatment,"Control")
onedata<-afp[ (afp$Site =="BK") & (afp$Year == 2017) & (afp$Season ==1),]
theme_set(theme_light())
source("summarySE.R") # source file with summary functions
trcol<-c("blue","red","green","violet") # colors used in plots

text_piceal<-"Worker abundance"
text_visited<-"Proportions of visited baits"
logbreaks<-c(1,2,11,31,91)

########
# Figs
# Submitted figures should not exceed the print area of 174 X 234 mm (approx. 7 X 9.4 inches).
# Petr for Pavel - for all abundance figures look if possible re-scale log x axis with each line = an abundance number of rounded digit (e.g.1,5,10,20,100 etc.)

#Fig1
# Petr for Pavel - size ok now, but treatment legend and A/B overlap with Figure - possible to move legend in the bottom? 
afpse<-summarySE(afp, measurevar="picea1", groupvars=c("Treatment","Season"))
afpse$upp<-afpse[,4]+afpse$se
afpse$low<-ifelse(afpse[,4]-afpse$se<0,0,afpse[,4]-afpse$se)

a<-ggplot(afpse, aes(factor(Season), picea1))+
  geom_errorbar(aes(ymin=low,ymax=upp,colour=Treatment), position=position_dodge(0.5)) +
  geom_point(aes(group=Treatment,color=Treatment,shape=Treatment),position=position_dodge(0.5)) + 
  labs(x="Season",y=text_piceal)+
  scale_y_continuous(breaks=logbreaks, labels=logbreaks-1, trans="log10")+
  scale_color_manual(values = trcol)+theme_light()+theme(legend.position = c(0.2, 0.4))

afpse2<-summarySE(afp, measurevar="Visited_picea", groupvars=c("Treatment","Season"))
b<-ggplot(afpse2, aes(factor(Season), Visited_picea))+
  geom_errorbar(aes(ymin=Visited_picea-ci,ymax=Visited_picea+ci,colour=Treatment), position=position_dodge(0.5)) +
  geom_point(aes(group=Treatment,color=Treatment,shape=Treatment),position=position_dodge(0.5)) + 
  labs(x="Season",y=text_visited)+
  scale_color_manual(values = trcol)+theme_light()+theme(legend.position = "none")

ggpubr::ggarrange(a, b, labels = c("(a)", "(b)"), ncol = 2, nrow = 1)
ggsave(paste0("Fig1.pdf"), width = 140, height = 100, units = "mm")


####################################FIGURES##################################################
# tag_facet breaks ggplot theme
# Petr for Pavel - fig and font size ok, but possible yet to move legend of treatment to bottom?, also letters missing (a), (b) brackets in the panel,if difficult Petr can modify in pdf manually
tag_facet2 <- function(p, open = "(", close = ")", tag_pool = letters, x = -Inf, y = Inf, 
                       hjust = -0.5, vjust = 1.5, fontface = 2, family = "", ...) {
  gb <- ggplot_build(p)
  lay <- gb$layout$layout
  tags <- cbind(lay, label = paste0(open, tag_pool[lay$PANEL], close), x = x, y = y)
  p + geom_text(data = tags, aes_string(x = "x", y = "y", label = "label"), ..., hjust = hjust, 
                vjust = vjust, fontface = fontface, family = family, inherit.aes = FALSE) 
}

afpse<-summarySE(afp, measurevar="picea1", groupvars=c("Treatment","Season","Site","Year"))
afpse$upp<-afpse[,6]+afpse$se
afpse$low<-ifelse(afpse[,6]-afpse$se<0,0,afpse[,6]-afpse$se)


p<-ggplot(afpse, aes(factor(Season), picea1))+
  geom_errorbar(aes(ymin=low,ymax=upp,colour=Treatment), position=position_dodge(0.5)) +
  geom_point(aes(group=Treatment,color=Treatment,shape=Treatment),position=position_dodge(0.5)) + 
  facet_grid(Year~Site)+labs(x="Season",y=text_piceal)+
  scale_color_manual(values = trcol)+theme_light()+
  scale_y_continuous(breaks=logbreaks, labels=logbreaks-1, trans="log10")
p
tag_facet2(p,open="",close="",tag_pool = letters)
ggsave(paste0("Fig2.pdf"), width = 7, height = 9.4)

#Fig3
ants<-glm.nb( Visited_picea~ Treatment*Site.Temperature,data=afp)
mydf <- ggpredict(ants, terms = c("Site.Temperature","Treatment"))
b<-plot(mydf) + xlab("Site temperature [deg. C]")+ylab(text_visited) +scale_color_manual(values = trcol) + theme_light()+theme(legend.position = "none")+ggtitle("")

ggsave(paste0("Fig3_temp.pdf"), width = 120, height = 110, units = "mm")

#FigS1
# Petr for Pavel - size ok now, but treatment legend overlap with Figure - possible to move legend in the bottom? 
afpse<-summarySE(afp, measurevar="picea1", groupvars=c("Treatment"))
afpse$upp<-afpse[,3]+afpse$se
afpse$low<-ifelse(afpse[,3]-afpse$se<0,0,afpse[,3]-afpse$se)
a<-ggplot(afpse, aes(factor(Treatment), picea1))+
  geom_errorbar(aes(ymin=low,ymax=upp,colour=Treatment), position=position_dodge(0.5)) +
  geom_point(aes(group=Treatment,color=Treatment,shape=Treatment),position=position_dodge(0.5)) + 
  labs(x="Treatment",y=text_piceal)+
  scale_y_continuous(breaks=logbreaks[1:4], labels=logbreaks[1:4]-1, trans="log10")+
  scale_color_manual(values = trcol)+theme_light()+theme(legend.position = c(0.2,0.6))

afpse<-summarySE(afp, measurevar="Visited_picea", groupvars=c("Treatment"))
b<-ggplot(afpse, aes(factor(Treatment), Visited_picea))+
  geom_errorbar(aes(ymin=Visited_picea-ci,ymax=Visited_picea+ci,colour=Treatment), position=position_dodge(0.5)) +
  geom_point(aes(group=Treatment,color=Treatment,shape=Treatment),position=position_dodge(0.5)) + 
  labs(x="Treatment",y=text_visited)+
  scale_color_manual(values = trcol)+theme_light()+theme(legend.position = "none")
ggpubr::ggarrange(a, b, 
                  labels = c("(a)", "(b)"),
                  ncol = 2, nrow = 1)
ggsave(paste0("FigS1.pdf"), width = 7, height = 5)

#FigS2
# Petr for Pavel - size ok now, but treatment legend and A/B overlap with Figure - possible to move legend in the bottom? / Site legend - Bily Kamen, Jankov, Radostin
afpse<-summarySE(afp, measurevar="picea1", groupvars=c("Treatment","Site"))
afpse$upp<-afpse[,4]+afpse$se
afpse$low<-ifelse(afpse[,4]-afpse$se<0,0,afpse[,4]-afpse$se)

a<-ggplot(afpse, aes(factor(Site), picea1))+
  geom_errorbar(aes(ymin=low,ymax=upp,colour=Treatment), position=position_dodge(0.5)) +
  geom_point(aes(group=Treatment,color=Treatment,shape=Treatment),position=position_dodge(0.5)) + 
  labs(x="Site",y=text_piceal)+
  scale_y_continuous(breaks=logbreaks, labels=logbreaks-1, trans="log10")+
  scale_color_manual(values = trcol)+theme_light()+theme(legend.position = c(0.2, 0.4))

afpse2<-summarySE(afp, measurevar="Visited_picea", groupvars=c("Treatment","Site"))
b<-ggplot(afpse2, aes(factor(Site), Visited_picea))+
  geom_errorbar(aes(ymin=Visited_picea-ci,ymax=Visited_picea+ci,colour=Treatment), position=position_dodge(0.5)) +
  geom_point(aes(group=Treatment,color=Treatment,shape=Treatment),position=position_dodge(0.5)) + 
  labs(x="Site",y=text_visited)+
  scale_color_manual(values = trcol)+theme_light()+theme(legend.position = "none")

ggpubr::ggarrange(a, b, labels = c("(a)", "(b)"), ncol = 2, nrow = 1)
ggsave(paste0("FigS2.pdf"), width = 7, height = 5)

#FigS3
# Petr for Pavel / Treatment legend better to bottom, Site legend to full = Bily Kamen, Jankov, Radostin in full, letters apear in automate letters without brackets (a) (b) / if difficult change Petr can adapt in pdf
afpse<-summarySE(afp, measurevar="Visited_picea", groupvars=c("Treatment","Season","Site"))
p<-ggplot(afpse, aes(factor(Season), Visited_picea))+
  geom_errorbar(aes(ymin=Visited_picea-se,ymax=Visited_picea+se,colour=Treatment), position=position_dodge(0.5)) +
  geom_point(aes(group=Treatment,color=Treatment,shape=Treatment),position=position_dodge(0.5)) + 
  facet_grid(cols=vars(Site))+
  labs(x="Season",y=text_visited)+
  scale_color_manual(values = trcol)+theme_light()
tag_facet2(p,open="",close="",tag_pool=letters)
ggsave(paste0("FigS3.pdf"), width = 7, height = 5)

####################################MODELS##################################################
# Main response variables - picea (=worker abundance), Visited_picea (=bait occuopancy)
# Main Explanatory variables - Treatment+Season+Site+Year
# Temperatures - Site.Temperature, Mean.Temperature

# Temperature models - sole effects only on worker abundance and bait occupancy
antSiteT<-glm.nb( picea~ Site.Temperature ,data=afp)
anova(antSiteT,test="Chisq")

antMeanT<-glm.nb( picea~ Mean.Temperature ,data=afp)
anova(antMeanT,test="Chisq")

avpSiteT<-glm( Visited_picea~ Site.Temperature ,data=afp,family="binomial")
anova(avpSiteT,test="Chisq")

avpMeanT<-glm( Visited_picea~ Mean.Temperature ,data=afp, family="binomial")
anova(avpMeanT,test="Chisq")

#Table 1
# full model on worker abundances (picea) with 4 main factors of interest, and their interaction
an<-glm.nb( picea~ Treatment+Season+Site+Year+
               Treatment:Season+Treatment:Site + Treatment:Year
             +Site:Season + Site:Year + Season:Year 
             ,data=afp)

Anova (an, type=3)
summary(an)
deviance(an)

#analysis with incl. Site.Temperature to full model (all temperature effects n.s.)
anT<-glm.nb( picea~ Treatment+Season+Site+Year+Site.Temperature+
               Treatment:Season+Treatment:Site + Treatment:Year
             +Site:Season + Site:Year + Season:Year+
               Site.Temperature:Treatment+Site.Temperature:Season+Site.Temperature:Site+Site.Temperature:Year
             ,data=afp)
Anova(anT, type=3)
summary (anT)

#analysis with incl. Mean.Temperature to full model (temperature effects n.s. except Site:Mean.Temperature)
anT2<-glm.nb( picea~ Treatment+Season+Site+Year+Mean.Temperature+
                Treatment:Season+Treatment:Site + Treatment:Year
              +Site:Season + Site:Year + Season:Year+
                Mean.Temperature:Treatment+Mean.Temperature:Season+Mean.Temperature:Site+Mean.Temperature:Year
              ,data=afp)

Anova(anT2, type=3)
summary (anT2)

# full model on bait occupancy with 4 main factors of interest, and their interaction
avp<-glm( Visited_picea~Treatment+Season+Site+Year+
            Treatment:Season+Treatment:Site + Treatment:Year+
            Season:Site+Year:Site+Year:Season
          ,data=afp, family="binomial")

Anova(avp, data = afp, type=3)
summary (avp)

#analysis with incl. Site.Temperature to full model (all temperature effects n.s.)
avpT<-glm(Visited_picea~ Treatment+Season+Site+Year+Site.Temperature+
            Treatment:Season+Treatment:Site + Treatment:Year 
          +Site:Season + Site:Year + Season:Year+
            Site.Temperature:Treatment+Site.Temperature:Season+Site.Temperature:Site+Site.Temperature:Year
          ,data=afp, family="binomial")

Anova(avpT, type=3)
summary (avpT)

#analysis with incl. Mean.Temperature to full model (all temperature effects n.s.)
avpT2<-glm( Visited_picea~Treatment+Season+Site+Year+Mean.Temperature+
              Treatment:Season+Treatment:Site + Treatment:Year
            +Site:Season + Site:Year + Season:Year+
              Mean.Temperature:Treatment+Mean.Temperature:Season+Mean.Temperature:Site+Mean.Temperature:Year
            ,data=afp, family="binomial")
Anova(avpT2, type=3)
summary (avpT2)

#Table S2
summary(glht(an0,mcp(Treatment="Tukey", interaction_average = T, covariate_average =T)),test = adjusted("holm"))
summary(glht(an0,mcp(Season="Tukey", interaction_average = T, covariate_average =T)),test = adjusted("holm"))
summary(glht(an0,mcp(Site="Tukey", interaction_average = T, covariate_average =T)),test = adjusted("holm"))
summary(glht(an0,mcp(Year="Tukey", interaction_average = T, covariate_average =T)),test = adjusted("holm"))

#Table S3
summary(glht(avp,mcp(Treatment="Tukey", interaction_average = T, covariate_average =T)),test = adjusted("holm"))
summary(glht(avp,mcp(Season="Tukey", interaction_average = T, covariate_average =T)),test = adjusted("holm"))
summary(glht(avp,mcp(Site="Tukey", interaction_average = T, covariate_average =T)),test = adjusted("holm"))

#Table S4 (+ also other combination of tests of Treatment*Temperature effects)
antm<-glm.nb( picea~ Treatment*Mean.Temperature,data=afp)
Anova(antm, data = afp, type=3)
summary(antm)

antm2<-glm.nb( picea~ Treatment*Site.Temperature,data=afp)
Anova(antm2, data = afp, type=3)
summary(antm2)

antmp <-glm( Visited_picea~ Treatment*Mean.Temperature,data=afp,family="binomial")
Anova(antmp, data = afp, type=3)
summary(antmp)

# sign. model for Figure 3 
antmp2 <-glm( Visited_picea~ Treatment*Site.Temperature,data=afp,family="binomial")
Anova(antmp2, data = afp, type=3)
summary(antmp2)


