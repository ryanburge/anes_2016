anes$trust_vote <- anes$V162219

anes$trust_vote[anes$trust_vote == -9] <- NA
anes$trust_vote[anes$trust_vote == -8] <- NA
anes$trust_vote[anes$trust_vote == -7] <- NA
anes$trust_vote[anes$trust_vote == -6] <- NA

anes$trust_vote <- 6 - anes$trust_vote

#anes$trust_vote <- Recode(anes$trust_vote, "1= 'Never';
     #                                       2= 'Some of the time';
      #                                      3= 'About half of the time';
     #                                       4= 'Most of the time';
      #                                      5= 'All of the time'")


trust <- anes %>% group_by(reltrad) %>% summarise(mean = mean(trust_vote, na.rm = TRUE))
trust$label <- c("Trust Voting Process")

ggplot(trust, aes(x=mean, y =label)) + geom_point(color = "black", shape=21, size =4, aes(fill = factor(reltrad))) + 
  theme(legend.title=element_blank()) +
  theme(legend.position = "bottom") +
  scale_x_continuous(limits = c(1,5), breaks = c(1,2,3,4,5), labels = c("Never", "Some of the time", "About half of the time", "Most of the time", "All of the time")) +  
  scale_fill_manual(values = c("#866097","#365D25","#252F99","#00CCFF", "#FC009C", "#674E60")) + 
  theme(text=element_text(size=18, family="KerkisSans")) + xlab("") + ylab("") + ggtitle("Do you think the video should matter to voters?") +
  theme(plot.title = element_text(hjust = 0.5))