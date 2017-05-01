
library(readr)
library(broom)
library(Hmisc)
library(forcats)
library(haven)
library(dplyr)
library(ggplot2)
library(extrafont)
library(car)
library(scales)
library(gridExtra)
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

anes <- as_factor(anes)






anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V161005) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161006) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V161021a) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161026) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161027) %>% mutate(pct = prop.table(n))

anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161028) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V161029a) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V161029b) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161031) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161032) %>% mutate(pct = prop.table(n))

anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161068) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161069) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V161070a) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161071) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161072) %>% mutate(pct = prop.table(n))

anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V161073a) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161074) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161075) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V161076a) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161077) %>% mutate(pct = prop.table(n))

anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161078) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V161079a) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161086) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161087) %>% mutate(pct = prop.table(n))

anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161116) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161117) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161118) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161119) %>% mutate(pct = prop.table(n))

anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161120) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161121) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161122) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161123) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161124) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161125) %>% mutate(pct = prop.table(n))

anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161145) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161146) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161147) %>% mutate(pct = prop.table(n))

anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V161150a) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V161150b) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V161151a) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V161151x) %>% mutate(pct = prop.table(n))


anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161159) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161160) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161161) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161162) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161163) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161164) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161165) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161166) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161167) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161168) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161169) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161170) %>% mutate(pct = prop.table(n))

anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161192) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161193) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V161193a) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V161194x) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161195) %>% mutate(pct = prop.table(n))

anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V161195a) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V161195x) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161196) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V161196a) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V161196x) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161214) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V161214a) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V161214x) %>% mutate(pct = prop.table(n))

anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161226) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V161226a) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V161226x) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161227) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V161227a) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V161227x) %>% mutate(pct = prop.table(n))

anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161229) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V161229a) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V161229x) %>% mutate(pct = prop.table(n))

anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161230) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161231) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161232) %>% mutate(pct = prop.table(n))

anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161241) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161242) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161243) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161244) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161245) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V161245a) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161246) %>% mutate(pct = prop.table(n))

anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V161247a) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V161247b) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161248) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161249) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161250) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161251) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161252) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161253) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161254) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161255) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161258) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161256) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161257) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161259) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161260) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161261) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161263) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V161266a) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V161266b) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V161266c) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V161266d) %>% mutate(pct = prop.table(n))

anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161270) %>% mutate(pct = prop.table(n))

anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V161308x) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161309) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V161310a) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V161310b) %>% mutate(pct = prop.table(n))

anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V161318) %>% mutate(pct = prop.table(n))

anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V161331x) %>% mutate(pct = prop.table(n))

anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V162018c) %>% mutate(pct = prop.table(n))

anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V162018e) %>% mutate(pct = prop.table(n))

anes %>% filter(reltrad == "White Born Again Protestants")   %>% group_by(reltrad) %>% count(V162034a) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V162035) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V162036) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")   %>% group_by(reltrad) %>% count(V162036a) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V162037) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")   %>% group_by(reltrad) %>% count(V162037a) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V162038) %>% mutate(pct = prop.table(n))

anes %>% filter(reltrad == "White Born Again Protestants")   %>% group_by(reltrad) %>% count(V162062x) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")   %>% group_by(reltrad) %>% count(V162063x) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")   %>% group_by(reltrad) %>% count(V162064x) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")   %>% group_by(reltrad) %>% count(V162065x) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")   %>% group_by(reltrad) %>% count(V162066x) %>% mutate(pct = prop.table(n))

anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V162078) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V162079) %>% mutate(pct = prop.table(n))

anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V162094) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V162095) %>% mutate(pct = prop.table(n))

anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V162106) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V162107) %>% mutate(pct = prop.table(n))

anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V162110) %>% mutate(pct = prop.table(n))

anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V162112) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V162113) %>% mutate(pct = prop.table(n))

anes %>% filter(reltrad == "White Born Again Protestants")   %>% group_by(reltrad) %>% count(V162122a) %>% mutate(pct = prop.table(n))

anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V162126) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V162127) %>% mutate(pct = prop.table(n))

anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V162132) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V162141) %>% mutate(pct = prop.table(n))


anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V162153) %>% mutate(pct = prop.table(n))

anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V162155x) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V162156x) %>% mutate(pct = prop.table(n))

anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V162157) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V162158) %>% mutate(pct = prop.table(n))

anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V162160) %>% mutate(pct = prop.table(n))

anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V162171) %>% mutate(pct = prop.table(n))

anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V162174) %>% mutate(pct = prop.table(n))


anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V162217) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V162218) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V162219) %>% mutate(pct = prop.table(n))


anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V162239) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V162240) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V162241) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V162242) %>% mutate(pct = prop.table(n))


anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V162248) %>% mutate(pct = prop.table(n))


anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V162249) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V162250) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V162251) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V162252) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V162253) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V162255) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V162255a) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V162255x) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V162258) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V162259) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V162260) %>% mutate(pct = prop.table(n))


anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V162261) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V162262) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V162263) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V162268) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V162269) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V162270) %>% mutate(pct = prop.table(n))

anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V162283) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V162284) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V162285) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V162286) %>% mutate(pct = prop.table(n))

anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V162294) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants") %>% group_by(reltrad) %>% count(V162295) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V162295a) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V162295b) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V162295x) %>% mutate(pct = prop.table(n))

anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V162298) %>% mutate(pct = prop.table(n))

anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V162313) %>% mutate(pct = prop.table(n))

anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V162327) %>% mutate(pct = prop.table(n))

anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V162332) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V162333) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V162334) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V162335) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V162336) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V162337) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V162338) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V162339) %>% mutate(pct = prop.table(n))

anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V162340) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V162341) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V162342) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V162343) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V162344) %>% mutate(pct = prop.table(n))

anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V162353) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V162354) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V162357) %>% mutate(pct = prop.table(n))

anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V162361) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V162362) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V162364) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V162365) %>% mutate(pct = prop.table(n))
anes %>% filter(reltrad == "White Born Again Protestants")  %>% group_by(reltrad) %>% count(V162367) %>% mutate(pct = prop.table(n))





















