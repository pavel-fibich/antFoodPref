rm(list=ls())

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
afp$picea1<-1+afp$picea
afp$YearSeason<-paste(afp$Year,afp$Season,sep="_")
afp$SeasonSite<-paste(afp$Season,afp$Site,sep="_")
afp$SiteYear<-paste(afp$Site,afp$Year,sep="_")
allfa<-c("Site","Treatment","Season","Year")
for (i in c(allfa,"YearSeason","SeasonSite","SiteYear")) afp[,i] <- as.factor(afp[,i])
afp$Treatment<-relevel(afp$Treatment,"Oil")
afp$Treatment<-relevel(afp$Treatment,"Control")
onedata<-afp[ (afp$Site =="BK") & (afp$Year == 2017) & (afp$Season ==1),]

library(ggplot2)
library(ggpubr)
theme_set(theme_light())
source("summarySE.R") # source file with summary functions
trcol<-c("blue","red","green","violet") # colors used in plots

text_piceal<-"Worker abundance"
text_visited<-"Proportions of visited baits"
logbreaks<-c(1,2,11,31,91)

########
# Figs
#Fig1
afpse<-summarySE(afp, measurevar="picea1", groupvars=c("Treatment","Season"))
# a<-ggplot(afpse, aes(factor(Season), piceal))+
#   geom_errorbar(aes(ymin=piceal-ci,ymax=piceal+ci,colour=Treatment), position=position_dodge(0.5)) +
#   geom_point(aes(group=Treatment,color=Treatment,shape=Treatment),position=position_dodge(0.5)) + 
#   labs(x="Season",y=text_piceal)+#scale_y_log10() 
#   scale_color_manual(values = trcol)+theme_light()+theme(legend.position = c(0.2, 0.4))
#afpse<-summarySEnon(afp, measurevar="picea", groupvars=c("Treatment","Season"))
#afpse<-summarySEmed(afp, measurevar="picea", groupvars=c("Treatment","Season"))
afpse$upp<-afpse[,4]+afpse$se
afpse$low<-ifelse(afpse[,4]-afpse$se<0,0,afpse[,4]-afpse$se)
#afpse$upp<-afpse[,4]+afpse$ciU
#afpse$low<-ifelse(afpse[,4]-afpse$ciL<0,0,afpse[,4]-afpse$ciL)

a<-ggplot(afpse, aes(factor(Season), picea1))+
    geom_errorbar(aes(ymin=low,ymax=upp,colour=Treatment), position=position_dodge(0.5)) +
    geom_point(aes(group=Treatment,color=Treatment,shape=Treatment),position=position_dodge(0.5)) + 
    labs(x="Season",y=text_piceal)+#scale_y_log10() +
  scale_y_continuous(breaks=logbreaks, labels=logbreaks-1, trans="log10")+
    scale_color_manual(values = trcol)+theme_light()+theme(legend.position = c(0.2, 0.4))
#a  
afpse2<-summarySE(afp, measurevar="Visited_picea", groupvars=c("Treatment","Season"))
b<-ggplot(afpse2, aes(factor(Season), Visited_picea))+
  geom_errorbar(aes(ymin=Visited_picea-ci,ymax=Visited_picea+ci,colour=Treatment), position=position_dodge(0.5)) +
  geom_point(aes(group=Treatment,color=Treatment,shape=Treatment),position=position_dodge(0.5)) + 
  labs(x="Season",y=text_visited)+
  scale_color_manual(values = trcol)+theme_light()+theme(legend.position = "none")

ggpubr::ggarrange(a, b, labels = c("A", "B"), ncol = 2, nrow = 1)
ggsave(paste0("Fig1_data_se.pdf"), width = 8, height = 5)

#Fig2
library(egg)
# tag_facet breaks ggplot theme
tag_facet2 <- function(p, open = "(", close = ")", tag_pool = letters, x = -Inf, y = Inf, 
                      hjust = -0.5, vjust = 1.5, fontface = 2, family = "", ...) {
  gb <- ggplot_build(p)
  lay <- gb$layout$layout
  tags <- cbind(lay, label = paste0(open, tag_pool[lay$PANEL], close), x = x, y = y)
  p + geom_text(data = tags, aes_string(x = "x", y = "y", label = "label"), ..., hjust = hjust, 
                vjust = vjust, fontface = fontface, family = family, inherit.aes = FALSE) 
}
# afpse<-summarySE(afp, measurevar="piceal", groupvars=c("Treatment","Season","Site","Year"))
# p<-ggplot(afpse, aes(factor(Season), piceal))+
#   geom_errorbar(aes(ymin=piceal-ci,ymax=piceal+ci,colour=Treatment), position=position_dodge(0.5)) +
#   geom_point(aes(group=Treatment,color=Treatment,shape=Treatment),position=position_dodge(0.5)) + 
#   facet_grid(Year~Site)+labs(x="Season",y=text_piceal)+
#   scale_color_manual(values = trcol)+theme_light()
afpse<-summarySE(afp, measurevar="picea1", groupvars=c("Treatment","Season","Site","Year"))
afpse$upp<-afpse[,6]+afpse$se
afpse$low<-ifelse(afpse[,6]-afpse$se<0,0,afpse[,6]-afpse$se)
# afpse<-summarySEmed(afp, measurevar="picea1", groupvars=c("Treatment","Season","Site","Year"))
# afpse<-summarySEnon(afp, measurevar="picea1", groupvars=c("Treatment","Season","Site","Year"))
# afpse$upp<-afpse[,6]+afpse$ciU
# afpse$low<-ifelse(afpse[,6]-afpse$ciL<0,0,afpse[,6]-afpse$ciL)
p<-ggplot(afpse, aes(factor(Season), picea1))+
  geom_errorbar(aes(ymin=low,ymax=upp,colour=Treatment), position=position_dodge(0.5)) +
  geom_point(aes(group=Treatment,color=Treatment,shape=Treatment),position=position_dodge(0.5)) + 
  facet_grid(Year~Site)+labs(x="Season",y=text_piceal)+#scale_y_log10() +
  scale_color_manual(values = trcol)+theme_light()+
  scale_y_continuous(breaks=logbreaks, labels=logbreaks-1, trans="log10")
p
tag_facet2(p,open="",close="",tag_pool=LETTERS)#,vjust=0)
ggsave(paste0("Fig2_data_se.pdf"), width = 6, height = 9)

#Fig3
library(MASS)
library(ggeffects)
antm<-glm.nb( picea~ Treatment*Mean.Temperature,data=afp)
mydf <- ggpredict(antm, terms = c("Mean.Temperature","Treatment"),type="zero_inflated")
a<-plot(mydf) + scale_y_log10() + xlab("Mean temperature [deg. C]")+ylab(text_piceal) +
  scale_color_manual(values = trcol) + theme_light()+theme(legend.position = c(0.2, 0.65))+
  ggtitle("")#+ylim(0,15)


ants<-glm.nb( Visited_picea~ Treatment*Site.Temperature,data=afp)
mydf <- ggpredict(ants, terms = c("Site.Temperature","Treatment"))
b<-plot(mydf) + xlab("Site temperature [deg. C]")+ylab(text_visited) +scale_color_manual(values = trcol) + theme_light()+theme(legend.position = "none")+ggtitle("")


ggpubr::ggarrange(a, b, 
          labels = c("A", "B"),
          ncol = 2, nrow = 1)
ggsave(paste0("Fig3_temp.pdf"), width = 8, height = 5)


#FigS1
# afpse<-summarySE(afp, measurevar="piceal", groupvars=c("Treatment"))
# a<-ggplot(afpse, aes(factor(Treatment), piceal))+
#   geom_errorbar(aes(ymin=piceal-ci,ymax=piceal+ci,colour=Treatment), position=position_dodge(0.5)) +
#   geom_point(aes(group=Treatment,color=Treatment,shape=Treatment),position=position_dodge(0.5)) + 
#   labs(x="Treatment",y=text_piceal)+
#   scale_color_manual(values = trcol)+theme_light()+theme(legend.position = c(0.2,0.4))
afpse<-summarySE(afp, measurevar="picea1", groupvars=c("Treatment"))
afpse$upp<-afpse[,3]+afpse$se
afpse$low<-ifelse(afpse[,3]-afpse$se<0,0,afpse[,3]-afpse$se)
a<-ggplot(afpse, aes(factor(Treatment), picea1))+
  geom_errorbar(aes(ymin=low,ymax=upp,colour=Treatment), position=position_dodge(0.5)) +
  geom_point(aes(group=Treatment,color=Treatment,shape=Treatment),position=position_dodge(0.5)) + 
  labs(x="Treatment",y=text_piceal)+#scale_y_log10() +
  scale_y_continuous(breaks=logbreaks[1:4], labels=logbreaks[1:4]-1, trans="log10")+
  scale_color_manual(values = trcol)+theme_light()+theme(legend.position = c(0.2,0.6))

afpse<-summarySE(afp, measurevar="Visited_picea", groupvars=c("Treatment"))
b<-ggplot(afpse, aes(factor(Treatment), Visited_picea))+
  geom_errorbar(aes(ymin=Visited_picea-ci,ymax=Visited_picea+ci,colour=Treatment), position=position_dodge(0.5)) +
  geom_point(aes(group=Treatment,color=Treatment,shape=Treatment),position=position_dodge(0.5)) + 
  labs(x="Treatment",y=text_visited)+
  scale_color_manual(values = trcol)+theme_light()+theme(legend.position = "none")
ggpubr::ggarrange(a, b, 
          labels = c("A", "B"),
          ncol = 2, nrow = 1)
ggsave(paste0("FigS1_data_se.pdf"), width = 8, height = 5)

#FigS2 (added by Petr - treatement times Site difference)
afpse<-summarySE(afp, measurevar="picea1", groupvars=c("Treatment","Site"))
afpse$upp<-afpse[,4]+afpse$se
afpse$low<-ifelse(afpse[,4]-afpse$se<0,0,afpse[,4]-afpse$se)

a<-ggplot(afpse, aes(factor(Site), picea1))+
  geom_errorbar(aes(ymin=low,ymax=upp,colour=Treatment), position=position_dodge(0.5)) +
  geom_point(aes(group=Treatment,color=Treatment,shape=Treatment),position=position_dodge(0.5)) + 
  labs(x="Site",y=text_piceal)+#scale_y_log10() +
  scale_y_continuous(breaks=logbreaks, labels=logbreaks-1, trans="log10")+
  scale_color_manual(values = trcol)+theme_light()+theme(legend.position = c(0.2, 0.4))

afpse2<-summarySE(afp, measurevar="Visited_picea", groupvars=c("Treatment","Site"))
b<-ggplot(afpse2, aes(factor(Site), Visited_picea))+
  geom_errorbar(aes(ymin=Visited_picea-ci,ymax=Visited_picea+ci,colour=Treatment), position=position_dodge(0.5)) +
  geom_point(aes(group=Treatment,color=Treatment,shape=Treatment),position=position_dodge(0.5)) + 
  labs(x="Site",y=text_visited)+
  scale_color_manual(values = trcol)+theme_light()+theme(legend.position = "none")

ggpubr::ggarrange(a, b, labels = c("A", "B"), ncol = 2, nrow = 1)
ggsave(paste0("FigS2_data_se.pdf"), width = 8, height = 5)


#FigS3
afpse<-summarySE(afp, measurevar="Visited_picea", groupvars=c("Treatment","Season","Site"))
p<-ggplot(afpse, aes(factor(Season), Visited_picea))+
  geom_errorbar(aes(ymin=Visited_picea-se,ymax=Visited_picea+se,colour=Treatment), position=position_dodge(0.5)) +
  geom_point(aes(group=Treatment,color=Treatment,shape=Treatment),position=position_dodge(0.5)) + 
  facet_grid(cols=vars(Site))+
  labs(x="Season",y=text_visited)+#scale_y_log10() +
  scale_color_manual(values = trcol)+theme_light()
tag_facet2(p,open="",close="",tag_pool=LETTERS)#,vjust=0)
ggsave(paste0("FigS3_data_se.pdf"), width = 6, height = 5)

#Petr:FigS3-A (abundance version if we decide to add it to the Suppl. too)
afpse<-summarySE(afp, measurevar="picea1", groupvars=c("Treatment","Season","Site"))
p<-ggplot(afpse, aes(factor(Season), picea1))+
  geom_errorbar(aes(ymin=picea1-se,ymax=picea1+se,colour=Treatment), position=position_dodge(0.5)) +
  geom_point(aes(group=Treatment,color=Treatment,shape=Treatment),position=position_dodge(0.5)) + 
  facet_grid(cols=vars(Site))+
  labs(x="Season",y=text_piceal)+scale_y_log10() +
  scale_color_manual(values = trcol)+theme_light()
tag_facet2(p,open="",close="",tag_pool=LETTERS,vjust=0)#fig. works but does not add ABC letters
ggsave(paste0("FigS3-A_data_se.pdf"), width = 6, height = 5)


## Tables

#Table 1
# bigger model on abundances (picea)
an0<-glm.nb( picea~ Treatment+Season+Site+Year+
               Treatment:Season+Treatment:Site + Treatment:Year #+ Site:Year
             +Site:Season + Site:Year + Season:Year 
             ,data=afp)
anova(an0,test="Chisq")

#Table S1
avp<-glm( Visited_picea~Treatment+Season+Site+Year+
            Treatment:Season+Treatment:Site + Treatment:Year+
            Season:Site+Year:Site+Year:Season
          ,data=afp, family="binomial")
anova(avp,test="Chisq")

#Table S2
library(multcomp)
summary(glht(an0,mcp(Treatment="Tukey", interaction_average = T, covariate_average =T)),test = adjusted("holm"))
summary(glht(an0,mcp(Season="Tukey", interaction_average = T, covariate_average =T)),test = adjusted("holm"))
summary(glht(an0,mcp(Site="Tukey", interaction_average = T, covariate_average =T)),test = adjusted("holm"))
summary(glht(an0,mcp(Year="Tukey", interaction_average = T, covariate_average =T)),test = adjusted("holm"))

#Table S3
summary(glht(avp,mcp(Treatment="Tukey", interaction_average = T, covariate_average =T)),test = adjusted("holm"))
summary(glht(avp,mcp(Season="Tukey", interaction_average = T, covariate_average =T)),test = adjusted("holm"))
summary(glht(avp,mcp(Site="Tukey", interaction_average = T, covariate_average =T)),test = adjusted("holm"))


#Table S4
antm<-glm.nb( picea~ Treatment*Mean.Temperature,data=afp)
anova(antm)

#Table S5
antm<-glm( Visited_picea~ Treatment*Site.Temperature,data=afp,family="binomial")
anova(antm,test="Chisq")
