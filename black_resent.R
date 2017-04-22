library(scales)
library(gridExtra)

anes$black1 <- anes$V162211
anes$black2 <- anes$V162212
anes$black3 <- anes$V162213
anes$black4 <- anes$V162214


anes$black1[anes$black1 == -9] <- NA
anes$black1[anes$black1 == -8] <- NA
anes$black1[anes$black1 == -7] <- NA
anes$black1[anes$black1 == -6] <- NA

anes$black2[anes$black2 == -9] <- NA
anes$black2[anes$black2 == -8] <- NA
anes$black2[anes$black2 == -7] <- NA
anes$black2[anes$black2 == -6] <- NA

anes$black3[anes$black3 == -9] <- NA
anes$black3[anes$black3 == -8] <- NA
anes$black3[anes$black3 == -7] <- NA
anes$black3[anes$black3 == -6] <- NA

anes$black4[anes$black4 == -9] <- NA
anes$black4[anes$black4 == -8] <- NA
anes$black4[anes$black4 == -7] <- NA
anes$black4[anes$black4 == -6] <- NA

anes$black1 <- 6- anes$black1
anes$black4 <- 6- anes$black4

anes$resent <- anes$black1 + anes$black2 + anes$black3 + anes$black4

anes$resent <- anes$resent/20


anes$bagain <- Recode(anes$V161263, "1=1; else=0")

anes$prot <- Recode(anes$V161247a, "1=1; else=0")

anes$white <- Recode(anes$V161310a, "1=1; else =0")
anes$black <- Recode(anes$V161310b, "1=1; else =0")
anes$asian <- Recode(anes$V161310d, "1=1; else =0")

anes$whtbaprot <- anes$white + anes$bagain + anes$prot
anes$whtbaprot <- Recode(anes$whtbaprot, "3=1; else=0")

anes$whtnonba <- anes$white + anes$prot
anes$whtnonba <- Recode(anes$whtnonba, "2=1; else=0")
anes$batest <- Recode(anes$bagain, "1=10; else=0")
anes$whtnonba <- anes$whtnonba + anes$batest
anes$whtnonba <- Recode(anes$whtnonba, "1=1; else=0")

anes$attend <-  anes$V161245
anes$attend <- Recode(anes$attend, "-9=0; -1=0; 1=6; 2=4; 3=3; 4=2; 5=1")

anes$hiattend <- Recode(anes$V161245a, "1=-1; else=0")
anes$attend <- anes$hiattend + anes$attend

anes$catholic <- Recode(anes$V161247a, "2=1; else=0")
anes$jewish <- Recode(anes$V161247a, "3=1; else=0")

anes$blackprot <- anes$black + anes$prot
anes$blackprot <- Recode(anes$blackprot, "2=1; else=0")

anes$r1 <- Recode(anes$whtbaprot, "1=1; else=0")
anes$r2 <- Recode(anes$whtnonba, "1=2; else=0")
anes$r3 <- Recode(anes$blackprot, "1=3; else=0")
anes$whtcath <- anes$catholic + anes$white
anes$r4 <- Recode(anes$whtcath, "2=4;else=0")
anes$whtjew <- anes$jewish + anes$white
anes$r5 <- Recode(anes$whtjew, "2=5; else=0")

anes$reltrad <- anes$r1 + anes$r2 + anes$r3 + anes$r4 + anes$r5

anes$reltrad <- Recode(anes$reltrad, "1='White Born Again Protestants';
                       2='White Non Born Again Protestants';
                       3='Black Protestants';
                       4='white Catholic'; 
                       5='White Jewish';
                       0= 'Other'")

resent <- anes %>% group_by(reltrad) %>% summarise(mean = mean(resent, na.rm = TRUE))

g1 <- ggplot(anes %>% filter(r1 ==1), aes(x=resent)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)*100), fill = "#FC009C", color = "black") + 
  theme(text=element_text(size=18, family="KerkisSans")) + xlab("Black Resentment Scale") + ylab("Percent of Respondents") + 
  ggtitle("White Born Again Protestants") +
  theme(plot.title = element_text(hjust = 0.5)) + geom_vline(xintercept = .739, linetype =2)

g2 <- ggplot(anes %>% filter(r2 ==2), aes(x=resent)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)*100), fill = "#674E60", color = "black") + 
  theme(text=element_text(size=18, family="KerkisSans")) + xlab("Black Resentment Scale") + ylab("Percent of Respondents") + 
  ggtitle("White Non Born Again Protestants") +
  theme(plot.title = element_text(hjust = 0.5)) + geom_vline(xintercept = .648, linetype =2)

g3 <- ggplot(anes %>% filter(r4 ==4), aes(x=resent)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)*100), fill = "#365D25", color = "black") + 
  theme(text=element_text(size=18, family="KerkisSans")) + xlab("Black Resentment Scale") + ylab("Percent of Respondents") + 
  ggtitle("White Catholics") +
  theme(plot.title = element_text(hjust = 0.5)) + geom_vline(xintercept = .678, linetype =2)

g4 <- ggplot(anes %>% filter(white ==1), aes(x=resent)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)*100), fill = "grey", color = "black") + 
  theme(text=element_text(size=18, family="KerkisSans")) + xlab("Black Resentment Scale") + ylab("Percent of Respondents") + 
  ggtitle("Entire White Sample") +
  theme(plot.title = element_text(hjust = 0.5)) + geom_vline(xintercept = .661, linetype =2)

grid.arrange(g1, g2, g3, g4, ncol =1)

