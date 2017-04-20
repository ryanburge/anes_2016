library(readr)
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




anes$trumpvid <- anes$V162127
anes$trumpvid <- Recode(anes$trumpvid, "1=5; 2=4; 3=3; 4=2; 5=1; else=0")



