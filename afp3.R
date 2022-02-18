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
afp$YearSeason<-paste(afp$Year,afp$Season,sep="_")
afp$SeasonSite<-paste(afp$Season,afp$Site,sep="_")
afp$SiteYear<-paste(afp$Site,afp$Year,sep="_")
allfa<-c("Site","Treatment","Season","Year")
for (i in c(allfa,"YearSeason","SeasonSite","SiteYear")) afp[,i] <- as.factor(afp[,i])
afp$Treatment<-relevel(afp$Treatment,"Oil")
afp$Treatment<-relevel(afp$Treatment,"Control")

library(ggplot2)
library(ggpubr)
theme_set(theme_light())
source("summarySE.R") # source file with summary functions
trcol<-c("blue","red","green","violet") # colors used in plots

text_piceal<-"log Worker abundance"
text_visited<-"Proportions of visited baits"

########
# Figs
#Fig1
afpse<-summarySE(afp, measurevar="piceal", groupvars=c("Treatment","Season"))
a<-ggplot(afpse, aes(factor(Season), piceal))+
  geom_errorbar(aes(ymin=piceal-se,ymax=piceal+se,colour=Treatment), position=position_dodge(0.5)) +
  geom_point(aes(group=Treatment,color=Treatment,shape=Treatment),position=position_dodge(0.5)) + 
  labs(x="Season",y=text_piceal)+
  scale_color_manual(values = trcol)+theme_light()+theme(legend.position = c(0.2, 0.4))

afpse2<-summarySE(afp, measurevar="Visited_picea", groupvars=c("Treatment","Season"))
b<-ggplot(afpse2, aes(factor(Season), Visited_picea))+
  geom_errorbar(aes(ymin=Visited_picea-se,ymax=Visited_picea+se,colour=Treatment), position=position_dodge(0.5)) +
  geom_point(aes(group=Treatment,color=Treatment,shape=Treatment),position=position_dodge(0.5)) + 
  labs(x="Season",y=text_visited)+
  scale_color_manual(values = trcol)+theme_light()+theme(legend.position = "none")


ggarrange(a, b, 
          labels = c("A", "B"),
          ncol = 2, nrow = 1)
ggsave(paste0("Fig1_data_se.pdf"), width = 8, height = 5)

#Fig2
afpse<-summarySE(afp, measurevar="piceal", groupvars=c("Treatment","Season","Site","Year"))
ggplot(afpse, aes(factor(Season), piceal))+
  geom_errorbar(aes(ymin=piceal-se,ymax=piceal+se,colour=Treatment), position=position_dodge(0.5)) +
  geom_point(aes(group=Treatment,color=Treatment,shape=Treatment),position=position_dodge(0.5)) + 
  facet_grid(Site ~ Year)+labs(x="Season",y=text_piceal)+
  scale_color_manual(values = trcol)+theme_light()
ggsave(paste0("Fig2_data_se.pdf"), width = 6, height = 9)
