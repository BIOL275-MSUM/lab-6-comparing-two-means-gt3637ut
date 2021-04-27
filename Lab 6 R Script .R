
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

#C. State the assumptions that you had to make to complete parts (A) and (B). Create a graph to assess whether one of those assumptions was met.

fish_long %>%
  ggplot(aes(x = species)) +
  geom_histogram(
    aes(fill = location), 
    bins = 5, 
    alpha = 0.5, 
    position = "identity"
  ) 
