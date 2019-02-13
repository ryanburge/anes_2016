library(socsci)
library(car)
library(labelled)
library(haven)

anes <- read_dta("D:/anes_2016/anes_timeseries_2016.dta")

anes %>% 
  mutate(ques = to_factor(V161266a)) %>% 
  ct(ques)


## Church Attendance ####
anes <- anes %>% 
  mutate(neveratt = car::recode(V161244, "2=1; else =0")) %>% 
  mutate(att = case_when(V161245  == 2 ~ 4,
                         V161245  == 3 ~ 3, 
                         V161245  == 4 ~ 2, 
                         V161245  == 5 ~ 1,
                         V161245a == 1 ~ 5,
                         V161245a == 2 ~ 6)) %>% 
  mutate(att = att/6)

## View of Bible ###

anes <- anes %>% 
  mutate(bible = car::recode(V161243, "1=3; 2=2; 3=1; else=NA")) %>% 
  mutate(literal = car::recode(V161243, "1=1; 2:3=0; else=NA")) %>% 
  mutate(inspired = car::recode(V161243, "2=1; 1=0; 3=0; else=NA")) %>% 
  mutate(fables = car::recode(V161243, "3=1; 1:2=0; else = NA")) %>% 
  mutate(bible = bible/3)

## Religious Guidance #### 

anes <- anes %>% 
  mutate(relguide = car::recode(V161242, "1=2; 2=3; 3=4; -1=1; else = NA")) %>% 
  mutate(relguide = relguide/4)


  
## Religious Descriptors ####
anes <- anes %>% mutate(fund = car::recode(V161266a, "1=1; else=0"))
anes <- anes %>% mutate(charis = car::recode(V161266b, "1=1; else=0"))
anes <- anes %>% mutate(bagain = car::recode(V161266c, "1=1; else=0"))
anes <- anes %>% mutate(evan = car::recode(V161266d, "1=1; else=0"))
anes <- anes %>% mutate(traditional = car::recode(V161266e, "1=1; else=0"))
anes <- anes %>% mutate(mainline = car::recode(V161266f, "1=1; else=0"))
anes <- anes %>% mutate(progress = car::recode(V161266g, "1=1; else=0"))
anes <- anes %>% mutate(nontrad = car::recode(V161266h, "1=1; else=0"))
anes <- anes %>% mutate(secular = car::recode(V161266j, "1=1; else=0"))
anes <- anes %>% mutate(agnostic = car::recode(V161266k, "1=1; else=0"))
anes <- anes %>% mutate(atheist = car::recode(V161266m, "1=1; else=0"))
anes <- anes %>% mutate(butnot = car::recode(V161266n, "1=1; else=0"))
anes <- anes %>% mutate(none = car::recode(V161266p, "1=1; else=0"))


## Born Again Question ####

anes <- anes %>% 
  mutate(reborn = car::recode(V161263, "1=1; else =0"))

## Cleaning up Reltrad ####  
anes <- anes %>% 
  mutate(reltrad = car::recode(V161265x, "-2 = NA"))

# # A tibble: 9 x 3
# ques                              n   pct
# <fct>                         <int> <dbl>
#   1 -2. Missing, item nonresponse    48 0.011
# 2 1. Mainline Protestant          625 0.146
# 3 2. Evangelical Protestant       765 0.179
# 4 3. Black Protestant              31 0.007
# 5 4. Roman Catholic               937 0.219
# 6 5. Undifferentiated Christian   674 0.158
# 7 6. Jewish                        84 0.02 
# 8 7. Other religion               229 0.054
# 9 8. Not religious                877 0.205

##  Discrimination ####

anes <- anes %>% 
  mutate(discrim_xtn = car::recode(V162365, "1=5; 2=4; 3=3; 4=2; 5=1; else = NA")) %>% 
  mutate(discrim_xtn = discrim_xtn/5) %>% 
  mutate(discrim_muslim = car::recode(V162364, "1=5; 2=4; 3=3; 4=2; 5=1; else = NA")) %>% 
  mutate(discrim_muslim = discrim_muslim/5)
  

## Feeling Therms ####
anes <- anes %>% 
  mutate(therm_fundie = car::recode(V162095, "-9:-1= NA; 101:999 = NA")) %>% 
  mutate(therm_fundie = therm_fundie/100) %>% 
  mutate(therm_pope = car::recode(V162049, "-9:-1= NA; 101:999 = NA")) %>% 
  mutate(therm_pope = therm_pope/100) %>% 
  mutate(therm_muslim = car::recode(V162106, "-9:-1= NA; 101:999 = NA")) %>% 
  mutate(therm_muslim = therm_muslim/100) %>% 
  mutate(therm_xtn = car::recode(V162107, "-9:-1= NA; 101:999 = NA")) %>% 
  mutate(therm_xtn = therm_xtn/100) %>% 
  mutate(therm_jews = car::recode(V162108, "-9:-1= NA; 101:999 = NA")) %>% 
  mutate(therm_jews = therm_jews/100)

## Morality ####
  
anes <- anes %>% 
  mutate(moral1 = car::recode(V162207, "-9:-1=NA")) %>% 
  mutate(moral2 = car::recode(V162208, "5=1; 4=2; 3=3; 2=4; 1=5; else = NA")) %>% 
  mutate(moral3 = car::recode(V162209, "-9:-1=NA")) %>% 
  mutate(moral4 = car::recode(V162210, "5=1; 4=2; 3=3; 2=4; 1=5; else = NA")) %>% 
  mutate(moral = moral1 + moral2 + moral3 + moral4) %>% 
  mutate(moral = moral/20)


