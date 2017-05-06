hc <- anes %>% filter(V161113 >0) %>%  group_by(reltrad) %>% count(V161113) %>% mutate(pct = prop.table(n))
hc$V161113 <- Recode(hc$V161113, "1=1; 2=-1; 3=0")
hc$label <- c("Support ACA")


hc$V161113 <- Recode(hc$V161113, "-1 = 'Oppose'; 0 = 'Neither'; 1 = 'Favor'" )


ggplot(hc, aes(1, pct*100)) + geom_col(aes(fill = fct_rev(V161113)), colour = "black") +
  xlab("Abortion Scenarios") + ylab("Percent of Respondents") + ggtitle("Religious Traditions' Views of the ACA") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=18, family="KerkisSans")) + coord_flip() + theme(axis.title.y = element_blank()) + 
  theme(axis.ticks = element_blank(), axis.text.y = element_blank()) + theme(legend.position="bottom") +  
  guides(fill = guide_legend(reverse = TRUE)) + labs(fill="")+ facet_grid(reltrad ~ .)  

ggsave(file="aca_opinion.png", type = "cairo-png", width = 9, height = 10)


hc <- anes %>% filter(hc >0) %>%  group_by(reltrad) %>% count(hc) %>% mutate(pct = prop.table(n))
#hc$V161113 <- Recode(hc$V161113, "1=1; 2=-1; 3=0")
#hc$label <- c("Support ACA")


hc$hc <- Recode(hc$hc, "1 = 'Strongly Favor'; 
                        2 = 'Moderately Favor'; 
                        3 = 'Favor A Little';
                        4 =  'Neither'; 
                        5= 'Oppose A Litte';
                        6= 'Moderately Oppose';
                        7= 'Strongly Oppose'"  )


hc$hc <- factor(hc$hc, levels=unique(hc$hc))


ggplot(hc, aes(1, pct*100)) + geom_col(aes(fill = fct_rev(hc)), colour = "black") +
  xlab("Abortion Scenarios") + ylab("Percent of Respondents") + ggtitle("Religious Traditions' Views of the ACA") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=18, family="KerkisSans")) + coord_flip() + theme(axis.title.y = element_blank()) + 
  theme(axis.ticks = element_blank(), axis.text.y = element_blank()) + theme(legend.position="bottom") +  
  guides(fill = guide_legend(reverse = TRUE)) + labs(fill="")+ facet_grid(reltrad ~ .)  + scale_fill_brewer(palette="RdBu")

ggsave(file="aca_opinion.png", type = "cairo-png", width = 9, height = 10)

## Vaccines

vax <- anes %>% filter(V162161 >0) %>%  group_by(reltrad) %>% count(V162161) %>% mutate(pct = prop.table(n))
vax$V162161 <- Recode(vax$V162161, "1=1; 3=0; 2=-1")
#hc$label <- c("Support ACA")


vax$V162161 <- Recode(vax$V162161, "1 = 'Benefits Outweigh Risks'; 
                         0  = 'No Difference';
                        -1 = 'Risks Outweigh Benefits'
                        "  )


#vax$V162161 <- factor(vax$V162161, levels=unique(vax$V162161))


ggplot(vax, aes(1, pct*100)) + geom_col(aes(fill = fct_rev(V162161)), colour = "black") +
  xlab("Abortion Scenarios") + ylab("Percent of Respondents") + ggtitle("Religious Traditions' Views of Vaccines") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=18, family="KerkisSans")) + coord_flip() + theme(axis.title.y = element_blank()) + 
  theme(axis.ticks = element_blank(), axis.text.y = element_blank()) + theme(legend.position="bottom") +  
  guides(fill = guide_legend(reverse = TRUE)) + labs(fill="")+ facet_grid(reltrad ~ .)  

ggsave(file="vax.png", type = "cairo-png", width = 9, height = 10)
