anes$obamatherm <- anes$V161092
anes$obamatherm[anes$obamatherm == -99] <- NA
anes$obamatherm[anes$obamatherm == -89] <- NA
anes$obamatherm[anes$obamatherm == -88] <- NA


anes$clintontherm <- anes$V161087
anes$clintontherm[anes$clintontherm == -9] <- NA
anes$clintontherm[anes$clintontherm == -7] <- NA
anes$clintontherm[anes$clintontherm == -6] <- NA
anes$clintontherm[anes$clintontherm == 998] <- NA

anes$hrccut <- as.numeric(cut2(anes$clintontherm, g=3))
anes$djtcut <- as.numeric(cut2(anes$obamatherm, g=3))

anes$bhocut <- as.numeric(cut2(anes$obamatherm, g=3))

whtbaprot <- filter(anes, whtbaprot ==1)


t1 <- table(whtbaprot$bhocut)
t1 <- tidy(t1)

t1$Var1 <- Recode(t1$Var1, "1='0-33';
                            2='34-66';
                            3='67-100'")

t1 <- t1 %>% mutate(pct = round(Freq/sum(Freq),2))

ggplot(t1, aes(x=Var1, y=pct*100)) + geom_col(fill = "firebrick2", color = "black") + 
  xlab("Trump Thermometer Scores (Pre-Survey)") + ylab("Percent of Respondents") + ggtitle("Among White Born Again Protestants") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=18, family="KerkisSans")) 




anes$trumptherm <- anes$V161087
anes$trumptherm[anes$trumptherm == -9] <- NA
anes$trumptherm[anes$trumptherm == -7] <- NA
anes$trumptherm[anes$trumptherm == -6] <- NA
anes$trumptherm[anes$trumptherm == 998] <- NA

anes$djtcut <- as.numeric(cut2(anes$trumptherm, g=3))

whtbaprot <- filter(anes, whtbaprot ==1)


t1 <- table(whtbaprot$djtcut)
t1 <- tidy(t1)

t1$Var1 <- Recode(t1$Var1, "1='0-33';
                            2='34-66';
                            3='67-100'")

t1 <- t1 %>% mutate(pct = round(Freq/sum(Freq),2))

ggplot(t1, aes(x=Var1, y=pct*100)) + geom_col(fill = "firebrick2", color = "black") + 
  xlab("Trump Thermometer Scores (Pre-Survey)") + ylab("Percent of Respondents") + ggtitle("Among White Born Again Protestants") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=18, family="KerkisSans")) 


evan$imp1 <- Recode(evan$V161241, "1=1; else=0")
evan$imp2 <- Recode(evan$V161242, "3=1; else=0")
evan$imp3 <- Recode(evan$V161245, "1=1; else=0")
evan$import <- evan$imp1 + evan$imp2 + evan$imp3
evan$import <- Recode(evan$import, "3=1; else=0")

table(evan$import)

evan$imp1 <- Recode(evan$V161241, "2=1; else=0")
evan$imp2 <- Recode(evan$V161242, "1:2=1; else=0")
evan$imp3 <- Recode(evan$V161245, "2:4=1; else=0")
evan$import <- evan$imp1 + evan$imp2 + evan$imp3
evan$import <- Recode(evan$import, "3=1; else=0")

table(evan$import)
