## Thermometer Scores

anes$trumptherm <- anes$V162079
anes$trumptherm[anes$trumptherm == -9] <- NA
anes$trumptherm[anes$trumptherm == -7] <- NA
anes$trumptherm[anes$trumptherm == -6] <- NA
anes$trumptherm[anes$trumptherm == 998] <- NA


anes$clintontherm <- anes$V162078

anes$clintontherm[anes$clintontherm == -9] <- NA
anes$clintontherm[anes$clintontherm == -7] <- NA
anes$clintontherm[anes$clintontherm == -6] <- NA
anes$clintontherm[anes$clintontherm == 998] <- NA

whtbaprot <- filter(anes, whtbaprot ==1)


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