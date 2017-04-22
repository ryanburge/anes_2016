anes$trumpwomen <- anes$V162188x
anes$clintonwomen <- anes$V162189x

anes$clintonwomen[anes$clintonwomen == -9] <- NA
anes$clintonwomen[anes$clintonwomen == -8] <- NA
anes$clintonwomen[anes$clintonwomen == -7] <- NA
anes$clintonwomen[anes$clintonwomen == -6] <- NA

anes$trumpwomen[anes$trumpwomen == -9] <- NA
anes$trumpwomen[anes$trumpwomen == -8] <- NA
anes$trumpwomen[anes$trumpwomen == -7] <- NA
anes$trumpwomen[anes$trumpwomen == -6] <- NA

clinton <- anes %>% group_by(reltrad) %>% summarise(mean = mean(clintonwomen, na.rm= TRUE))
clinton$label <- c("Clinton")

trump <- anes %>% group_by(reltrad) %>% summarise(mean = mean(trumpwomen, na.rm= TRUE))
trump$label <- c("Trump")

women <- rbind(clinton, trump)

ggplot(women, aes(x=mean, y =label)) + geom_point(color = "black", shape=21, size =4, aes(fill = factor(reltrad))) + 
  theme(legend.title=element_blank()) +
  theme(legend.position = "bottom") +
  scale_x_continuous(limits = c(1,7), breaks = c(1,2,3,4,5,6,7), labels = c("Extremely Poorly", "Moderately Poorly", "Slightly Poorly", "Neither", "Slightly Well", "Moderately Well", "Extremely Well")) +  
  scale_fill_manual(values = c("#866097","#365D25","#252F99","#00CCFF", "#FC009C", "#674E60")) + 
  theme(text=element_text(size=18, family="KerkisSans")) + xlab("") + ylab("") + ggtitle("How Does Each Candidate Treat Women?") +
  theme(plot.title = element_text(hjust = 0.5))



