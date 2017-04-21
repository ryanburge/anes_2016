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


anes$blackdis <- anes$V162357

anes$blackdis[anes$blackdis == -9] <- NA
anes$blackdis[anes$blackdis == -7] <- NA
anes$blackdis[anes$blackdis == -6] <- NA
anes$blackdis[anes$blackdis == -5] <- NA

anes$blackdis <- 6- anes$blackdis

anes$hispdis <- anes$V162358

anes$hispdis[anes$hispdis == -9] <- NA
anes$hispdis[anes$hispdis == -7] <- NA
anes$hispdis[anes$hispdis == -6] <- NA
anes$hispdis[anes$hispdis == -5] <- NA

anes$hispdis <- 6- anes$hispdis


anes$asiandis <- anes$V162359

anes$asiandis[anes$asiandis == -9] <- NA
anes$asiandis[anes$asiandis == -7] <- NA
anes$asiandis[anes$asiandis == -6] <- NA
anes$asiandis[anes$asiandis == -5] <- NA

anes$asiandis <- 6- anes$asiandis


anes$whitedis <- anes$V162360

anes$whitedis[anes$whitedis == -9] <- NA
anes$whitedis[anes$whitedis == -7] <- NA
anes$whitedis[anes$whitedis == -6] <- NA
anes$whitedis[anes$whitedis == -5] <- NA

anes$whitedis <- 6- anes$whitedis


anes$lgbtdis <- anes$V162361

anes$lgbtdis[anes$lgbtdis == -9] <- NA
anes$lgbtdis[anes$lgbtdis == -7] <- NA
anes$lgbtdis[anes$lgbtdis == -6] <- NA
anes$lgbtdis[anes$lgbtdis == -5] <- NA

anes$lgbtdis <- 6- anes$lgbtdis


anes$womendis <- anes$V162362

anes$womendis[anes$womendis == -9] <- NA
anes$womendis[anes$womendis == -7] <- NA
anes$womendis[anes$womendis == -6] <- NA
anes$womendis[anes$womendis == -5] <- NA

anes$womendis <- 6- anes$womendis


anes$mendis <- anes$V162363

anes$mendis[anes$mendis == -9] <- NA
anes$mendis[anes$mendis == -7] <- NA
anes$mendis[anes$mendis == -6] <- NA
anes$mendis[anes$mendis == -5] <- NA

anes$mendis <- 6- anes$mendis


anes$muslimdis <- anes$V162364

anes$muslimdis[anes$muslimdis == -9] <- NA
anes$muslimdis[anes$muslimdis == -7] <- NA
anes$muslimdis[anes$muslimdis == -6] <- NA
anes$muslimdis[anes$muslimdis == -5] <- NA

anes$muslimdis <- 6- anes$muslimdis


anes$xtndis <- anes$V162365

anes$xtndis[anes$xtndis == -9] <- NA
anes$xtndis[anes$xtndis == -7] <- NA
anes$xtndis[anes$xtndis == -6] <- NA
anes$xtndis[anes$xtndis == -5] <- NA

anes$xtndis <- 6- anes$xtndis


anes$transdis <- anes$V162366

anes$transdis[anes$transdis == -9] <- NA
anes$transdis[anes$transdis == -7] <- NA
anes$transdis[anes$transdis == -6] <- NA
anes$transdis[anes$transdis == -5] <- NA

anes$transdis <- 6- anes$transdis


whtbaprot <- filter(anes, whtbaprot ==1)

t1 <- table(whtbaprot$blackdis)
t1 <- tidy(t1)
t1$group <- c("Blacks")
t1 <- t1 %>% mutate(pct = round(Freq/sum(Freq),2))


t2 <- table(whtbaprot$hispdis)
t2 <- tidy(t2)
t2$group <- c("Hispanics")
t2 <- t2 %>% mutate(pct = round(Freq/sum(Freq),2))

t3 <- table(whtbaprot$asiandis)
t3 <- tidy(t3)
t3$group <- c("Asians")
t3 <- t3 %>% mutate(pct = round(Freq/sum(Freq),2))

t4 <- table(whtbaprot$whitedis)
t4 <- tidy(t4)
t4$group <- c("Whites")
t4 <- t4 %>% mutate(pct = round(Freq/sum(Freq),2))

t5 <- table(whtbaprot$lgbtdis)
t5 <- tidy(t5)
t5$group <- c("LGBT")
t5 <- t5 %>% mutate(pct = round(Freq/sum(Freq),2))

t6 <- table(whtbaprot$womendis)
t6 <- tidy(t6)
t6$group <- c("Women")
t6 <- t6 %>% mutate(pct = round(Freq/sum(Freq),2))


t7 <- table(whtbaprot$mendis)
t7 <- tidy(t7)
t7$group <- c("Men")
t7 <- t7 %>% mutate(pct = round(Freq/sum(Freq),2))

t8 <- table(whtbaprot$muslimdis)
t8 <- tidy(t8)
t8$group <- c("Muslims")
t8 <- t8 %>% mutate(pct = round(Freq/sum(Freq),2))

t9 <- table(whtbaprot$xtndis)
t9 <- tidy(t9)
t9$group <- c("Christians")
t9 <- t9 %>% mutate(pct = round(Freq/sum(Freq),2))

t10 <- table(whtbaprot$transdis)
t10 <- tidy(t10)
t10$group <- c("Transgender")
t10 <- t10 %>% mutate(pct = round(Freq/sum(Freq),2))

dis <- rbind(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10)

dis$Var1 <- Recode(dis$Var1, "1='None at all';
                  2='A little';
                  3='A moderate amount';
                  4='A lot';
                  5='A great deal'")

dis$Var1 <- factor(dis$Var1, levels=unique(dis$Var1))


ggplot(dis, aes(1, pct*100)) + geom_col(aes(fill= fct_rev(Var1)), colour = "black") +
  xlab("Abortion Scenarios") + ylab("Percent of Respondents") + ggtitle("Born Again Protestants Views on Discrimination") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=12, family="KerkisSans")) + coord_flip() + theme(axis.title.y = element_blank()) + 
  theme(axis.ticks = element_blank(), axis.text.y = element_blank()) + theme(legend.position="bottom") +  
  guides(fill = guide_legend(reverse = TRUE)) + labs(fill="")+ facet_grid(group ~ .)  
