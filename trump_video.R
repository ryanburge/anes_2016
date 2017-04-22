
## TRUMP VIDEO 

anes$trumpvid <- anes$V162127
anes$trumpvid <- Recode(anes$trumpvid, "1=5; 2=4; 3=3; 4=2; 5=1; else=0")

vid <- anes %>% group_by(reltrad) %>% summarise(mean = mean(trumpvid, na.rm = TRUE))

vid$label <- c("Trump Video")


ggplot(vid, aes(x=mean, y =label)) + geom_point(color = "black", shape=21, size =4, aes(fill = factor(reltrad))) + 
  theme(legend.title=element_blank()) +
  theme(legend.position = "bottom") +
  scale_x_continuous(limits = c(1,5), breaks = c(1,2,3,4,5), labels = c("Not at all", "A little", "A moderate amount", "A lot", "A great deal")) +  
  scale_fill_manual(values = c("#4DAF4A" , "#984EA3", "#FF7F00" , "#FFFF33" , "#A65628", "seagreen1")) + 
  theme(text=element_text(size=18, family="KerkisSans")) + xlab("") + ylab("") + ggtitle("Do you think the video should matter to voters?") +
  theme(plot.title = element_text(hjust = 0.5))