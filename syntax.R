## "From an offender-based to an offense-based justice:
## changes in the juvenile justice system in SP between 1990 and 2006"
## Thiago R. Oliveira

library(tidyverse)
library(lme4)
library(texreg)
library(RColorBrewer)
options(scipen=999)

# read data set
load('data/dataset.RData')

## MODELLING

m1 <- glm(int ~ year + offense + local + p1,
           data = dataset, family = binomial(link = 'logit'))
m2 <- glm(int ~ year + p2 + offense + local +p1,
           data = dataset, family = binomial(link = 'logit'))
m3 <- glm(int ~ year * offense + p2 + local +p1,
           data = dataset ,family = binomial(link = 'logit'), na.action = na.exclude)

# Table 1:
list(
  'Model 1' = m1,
  'Model 2' = m2,
  'Model 3' = m3
) %>%
  screenreg(omit.coef = 'p1')

##############
dataset <-
  dataset %>%
  mutate(prob = predict(m3, type = "response"),
         year = year + 1991)

graph.prob <-  dataset %>%
  mutate(offense = na_if(offense, "no offense")) %>%
  mutate(offense = factor(offense, levels = c('violent offense', 'drogas', 'non-violent offense', 'no info'))) %>%
  filter(!is.na(offense)) %>%
  ggplot() +
  aes(x = year, y = prob, colour = factor(offense,levels =  c('violent offense', 'drogas', 'non-violent offense', 'no info'))) +
  geom_smooth(method = "glm",
              aes(x = year, y= prob),
              method.args = list(family = "binomial"),
              se = FALSE) +
  labs(x = 'Year of trial', y = 'Predicted probability', 
       title = 'Probability of conviction with a detention disposition') +
  scale_colour_brewer(name="Offense",
                      labels=c("Violent offenses", "Drug-related offenses", "Non-violent offenses", "No offense information"),
                      palette = "Set1") + 
  theme(panel.background = element_blank())

pdf('plots/graph_prob.pdf')
graph.prob
dev.off()

####
