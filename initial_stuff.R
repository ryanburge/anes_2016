library(readr)
library(broom)
library(Hmisc)
library(haven)
library(dplyr)
library(ggplot2)
library(car)

anes <- read_dta("D:/anes_2016/anes_timeseries_2016.dta")


anes$bagain <- Recode(anes$V161263, "1=1; else=0")

anes$prot <- Recode(anes$V161247a, "1=1; else=0")

anes$white <- Recode(anes$V161310a, "1=1; else =0")
anes$black <- Recode(anes$V161310b, "1=1; else =0")
anes$asian <- Recode(anes$V161310d, "1=1; else =0")

anes$whtbaprot <- anes$white + anes$bagain + anes$prot
anes$whtbaprot <- Recode(anes$whtbaprot, "3=1; else=0")

anes$attend <-  anes$V161245
anes$attend <- Recode(anes$attend, "-9=0; -1=0; 1=6; 2=4; 3=3; 4=2; 5=1")

anes$hiattend <- Recode(anes$V161245a, "1=-1; else=0")
anes$attend <- anes$hiattend + anes$attend


anes$vote16 <- anes$V162062x

anes$vote16<-Recode(anes$vote16,"1='Hillary Clinton';
   2='Donald Trump';
   3='Gary Johnson';
   4='Jill Stein'; else =0")

votetotal <- anes %>% filter(white ==1 & prot ==1 & bagain ==1) %>% 
  count(vote16, wt = V160102) %>% mutate(weight = prop.table(n)) %>% arrange(desc(weight))

votetotal$vote16 <-as.numeric(votetotal$vote16)

votetotal$vote16 <- Recode(votetotal$vote16,"1='Hillary Clinton';
                       2='Donald Trump';
                       3='Gary Johnson';
                       4='Jill Stein'; 
                       5='Other Candidate';
                      -1= 'No Vote';
                      -9='Refused'")

ggplot(votetotal, aes(x=reorder(vote16, -weight), y= weight)) + geom_col()


anes$trumptherm <- anes$V162079
anes$trumptherm[anes$trumptherm == -9] <- NA
anes$trumptherm[anes$trumptherm == -7] <- NA
anes$trumptherm[anes$trumptherm == -6] <- NA
anes$trumptherm[anes$trumptherm == 998] <- NA

ggplot(anes %>% filter(whtbaprot==1), aes(x=trumptherm)) + 
  geom_histogram(fill="red", colour="black", binwidth = 5) + 
  xlab("Trump Thermometer Scores") + ylab("Number of Respondents") + ggtitle("Among White Born Again Protestants")

anes$clintontherm <- anes$V162078

anes$clintontherm[anes$clintontherm == -9] <- NA
anes$clintontherm[anes$clintontherm == -7] <- NA
anes$clintontherm[anes$clintontherm == -6] <- NA
anes$clintontherm[anes$clintontherm == 998] <- NA

t1 <- table(whtbaprot$clintontherm)
t1 <- tidy(t1)
143/441
t2 <- table(whtbaprot$trumptherm)
t2 <- tidy(t2)
58/440


anes$hrccut <- as.numeric(cut2(anes$clintontherm, g=5))
anes$djtcut <- as.numeric(cut2(anes$trumptherm, g=5))

t1 <- table(whtbaprot$hrccut)
t1 <- tidy(t1)

t1$Var1 <- Recode(t1$Var1, "1='0-20';
                            2='21-40';
                            3='41-60';
                            4='61-80';
                            5='81-100'")

t1 <- t1 %>% mutate(pct = round(Freq/sum(Freq),2))

ggplot(t1, aes(x=Var1, y=pct*100)) + geom_col(fill = "dodgerblue3", color = "black") + 
  xlab("Clinton Thermometer Scores") + ylab("Percent of Respondents") + ggtitle("Among White Born Again Protestants") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=18, family="KerkisSans"))


t2 <- table(whtbaprot$djtcut)
t2 <- tidy(t2)

t2 <- t2 %>% mutate(pct = round(Freq/sum(Freq),2))

t2$Var1 <- Recode(t2$Var1, "1='0-20';
                            2='21-40';
                            3='41-60';
                            4='61-80';
                            5='81-100'")

ggplot(t2, aes(x=Var1, y=pct*100)) + geom_col(fill = "firebrick2", color = "black") + 
  xlab("Trump Thermometer Scores") + ylab("Percent of Respondents") + ggtitle("Among White Born Again Protestants") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=18, family="KerkisSans"))


anes$trumpvid <- anes$V162127
anes$trumpvid <- Recode(anes$trumpvid, "1=5; 2=4; 3=3; 4=2; 5=1; else=0")

anes$hrcabort <- anes$V162181
anes$djtabort <- anes$V162182

anes$hrcabort[anes$hrcabort == -9] <- NA
anes$hrcabort[anes$hrcabort == -7] <- NA
anes$hrcabort[anes$hrcabort == -6] <- NA
anes$hrcabort[anes$hrcabort == -8] <- NA

anes$djtabort[anes$djtabort == -9] <- NA
anes$djtabort[anes$djtabort == -7] <- NA
anes$djtabort[anes$djtabort == -6] <- NA
anes$djtabort[anes$djtabort == -8] <- NA

t1 <- table(anes$hrcabort)
t2 <- table(whtbaprot$hrcabort)

t1 <- tidy(t1)
t2 <- tidy(t2)

t1$Var1 <- Recode(t1$Var1, "1='Complete Ban';
                            2='Rape, Incest, Life of the Mother';
                            3='Clearly Established Need';
                            4='Always Permitted'")

t2$Var1 <- Recode(t2$Var1, "1='Complete Ban';
                            2='Rape, Incest, Life of the Mother';
                            3='Clearly Established Need';
                            4='Always Permitted'")

t1 <- t1 %>% mutate(pct = round(Freq/sum(Freq),2))
t2 <- t2 %>% mutate(pct = round(Freq/sum(Freq),2))

t1$label  <- c("Entire Sample")
t2$label  <- c("BA + Prot")

hrcabort <- rbind(t1, t2)

hrcabort$Var1 <- factor(hrcabort$Var1, levels=unique(hrcabort$Var1))


ggplot(hrcabort, aes(1, pct*100)) + geom_col(aes(fill= Var1), colour = "black") +
  xlab("Abortion Scenarios") + ylab("Percent of Respondents") + ggtitle("Perception of Clinton on Abortion") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=18, family="KerkisSans")) + coord_flip() + theme(axis.title.y = element_blank()) + 
  theme(axis.ticks = element_blank(), axis.text.y = element_blank()) + theme(legend.position="bottom") +  
  guides(fill = guide_legend(reverse = TRUE)) + labs(fill="")+ facet_grid(label ~ .)  


t1 <- table(anes$djtabort)
t2 <- table(whtbaprot$djtabort)

t1 <- tidy(t1)
t2 <- tidy(t2)

t1$Var1 <- Recode(t1$Var1, "1='Complete Ban';
                            2='Rape, Incest, Life of the Mother';
                            3='Clearly Established Need';
                            4='Always Permitted'")

t2$Var1 <- Recode(t2$Var1, "1='Complete Ban';
                            2='Rape, Incest, Life of the Mother';
                            3='Clearly Established Need';
                            4='Always Permitted'")

t1 <- t1 %>% mutate(pct = round(Freq/sum(Freq),2))
t2 <- t2 %>% mutate(pct = round(Freq/sum(Freq),2))

t1$label  <- c("Entire Sample")
t2$label  <- c("BA + Prot")

djtabort <- rbind(t1, t2)

djtabort$Var1 <- factor(djtabort$Var1, levels=unique(djtabort$Var1))


ggplot(djtabort, aes(1, pct*100)) + geom_col(aes(fill= Var1), colour = "black") +
  xlab("Abortion Scenarios") + ylab("Percent of Respondents") + ggtitle("Perception of Trump on Abortion") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=18, family="KerkisSans")) + coord_flip() + theme(axis.title.y = element_blank()) + 
  theme(axis.ticks = element_blank(), axis.text.y = element_blank()) + theme(legend.position="bottom") +  
  guides(fill = guide_legend(reverse = TRUE)) + labs(fill="")+ facet_grid(label ~ .)  


