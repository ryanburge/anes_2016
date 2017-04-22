
## ABORTION


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