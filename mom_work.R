anes$momwork <- anes$V162230

anes$momwork[anes$momwork == -9] <- NA
anes$momwork[anes$momwork == -8] <- NA
anes$momwork[anes$momwork == -7] <- NA
anes$momwork[anes$momwork == -6] <- NA

anes$momwork <- Recode(anes$momwork, "1=1; 2=-1; 3=0")

mom <- anes %>% group_by(reltrad) %>% summarise(mean = mean(momwork, na.rm = TRUE))

mom$label <- c("Mom Doesn't Work")

ggplot(mom, aes(x=mean, y =label)) + geom_point(color = "black", shape=21, size =4, aes(fill = factor(reltrad))) + 
  theme(legend.title=element_blank()) +
  theme(legend.position = "bottom") +
  scale_x_continuous(limits = c(-1,1), breaks = c(-1,0,1), labels = c("Worse", "Makes No Difference", "Better")) +  
  scale_fill_manual(values = c("#866097","#365D25","#252F99","#00CCFF", "#FC009C", "#674E60")) + 
  theme(text=element_text(size=18, family="KerkisSans")) + xlab("") + ylab("") + ggtitle("If the Woman Stays Home is that:") +
  theme(plot.title = element_text(hjust = 0.5))