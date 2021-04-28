
# load packages -----------------------------------------------------------

library(tidyverse)


# read data ---------------------------------------------------------------

fish <- read_csv("chap12q19ElectricFish.csv")


# put data in tidy format ------------------------------------------------

fish_long <- 
  pivot_longer(fish, speciesUpstream:speciesDownstream,
               names_to = "location",
               values_to = "species") %>% 
  mutate(location = str_remove(location, c("species"))) %>% 
  print()


# do stuff ----------------------------------------------------------------

fish_ttest <- t.test(species ~ location, data = fish_long)
fish_ttest$estimate







#A. What is the mean difference in the number of species between areas upstream and downstream of a tributary? What is the 95% confidence interval of this mean difference. Show your code and write a sentence giving your answer


fish_mean_diff <-
  fish_long %>% 
  group_by(location) %>% 
  summarize(
    n = n(),
    mean = mean(species),
    sd = sd(species),
    sem = sd/sqrt(n),
    upper = mean + 1.96 * sem,
    lower = mean - 1.96 * sem
  ) %>% 
  print()


#B. Test the hypothesis that the tributaries have no effect on the number of species of electric fish.

fish_ttest <- t.test(species ~ location, data = fish_long)
fish_ttest$estimate

fish_ttest

#Interpreting the t test:
  #Mean of Downstream : 16.41667
  #Mean of Upstream: 14.58333
  #Difference is: 1.83334
  #Standard deviation of both groups are not the same so it is not a two sample t test because the third assumption is violated 

  
  
#C. State the assumptions that you had to make to complete parts (A) and (B). Create a graph to assess whether one of those assumptions was met.

fish_long %>%
  ggplot(aes(x = species)) +
  geom_histogram(
    aes(fill = location), 
    bins = 5, 
    alpha = 0.5, 
    position = "identity"
  ) +
  facet_wrap(~ location)
summary(fish_long)



#Ttest Question 

crabs <- read_csv("chap15q27FiddlerCrabFans.txt")
crabs

# D) 
crabs %>%
  ggplot(aes(x = bodyTemperature)) +
  geom_histogram(
    aes(fill = crabType ), 
    bins = 5, 
    alpha = 0.5, 
    position = "identity"
  ) +
  facet_wrap(~ crabType)


# E) 

# H0: Body temperature is equal among all crab types
# HA: At least one crab type will be different from the others 

aov_crabs <- 
  aov(bodyTemperature ~ crabType, data = crabs)
aov_crabs

summary(aov_crabs)

#Interpreting the ANOVA
# Mean of crabType: .8804
# Mean of Residuals: .0433

Call:
  aov(formula = bodyTemperature ~ crabType, data = crabs)

Terms:
 # crabType Residuals
#Sum of Squares  2.641310  3.467619
#Deg. of Freedom        3        80


