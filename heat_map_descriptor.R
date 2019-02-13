heat <- anes %>% 
  group_by(reltrad) %>% 
  select(fund, charis, bagain, evan, traditional, mainline, progress, nontrad, secular, agnostic, atheist, butnot, none) %>% 
  summarise_all(mean) %>% 
  ungroup(reltrad) %>% 
  mutate(reltrad = frcode(reltrad ==2 ~ "Evangelical",
                          reltrad ==1 ~ "Mainline",
                          reltrad ==3 ~ "Black\nProtestant",
                          reltrad ==5 ~ "Just\nChristian",
                          reltrad ==4 ~ "Catholic", 
                          reltrad ==6 ~ "Jewish",
                          reltrad ==7 ~ "Other\nReligion",
                          reltrad ==8 ~ "Not\nReligious"))


heat <- heat %>% 
  melt() %>% as.tibble() %>% 
  mutate(descrip = variable) %>% 
  mutate(pct = value)



heat %>% 
  filter(reltrad != "NA") %>% 
  filter(variable != "NA") %>% 
  mutate(value = round(value, 3)) %>% 
  ggplot(., aes(y = variable, x = reltrad)) +
  geom_tile(aes(fill = value), color  = "black") +
  theme_gg("Abel") +
  scale_fill_gradient(low = "#556270", high = "#FF6B6B") +
  geom_text(aes(y= variable , x = reltrad, label = paste0(value*100, '%')), family = "font") +
  scale_x_discrete(position = "top")  +
  scale_y_discrete(labels = c("Fundamentalist", "Charismatic", "Born Again", "Evangelical", "Traditional", "Mainline", "Progressive", "Non-\nTraditional", "Secular", "Agnostic", "Atheist", "Spiritual\nBut Not\nReligious", "None of\nthe Above")) + 
  labs(x = "Religious Tradition", y = "Selected Descriptor", title = "How Do People Identify Themselves Religiously?", captionn = "Data: ANES 2016") +
  ggsave("D://anes_2016/descrip_heat.png", width = 10, height =10)



  