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
anes$r4 <- Recode(anes$catholic, "1=4; else=0")
anes$r5 <- Recode(anes$jewish, "1=5; else=0")

anes$reltrad <- anes$r1 + anes$r2 + anes$r3 + anes$r4 + anes$r5

anes$reltrad <- Recode(anes$reltrad, "1='White Born Again Protestants';
                       2='White Non Born Again Protestants';
                       3='Black Protestants';
                       4='Catholic'; 
                       5='Jewish';
                       0= 'Other'")

## Simple Vote Totals

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






 



