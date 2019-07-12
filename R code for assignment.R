rm(list = ls())

setwd("C:/stata assignments/almost complete/masters for march/dr bundi/the assignment")
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(broom)
library(purrr)
library(ggthemes)

#load data
data <- read_csv("mobilemoney_data.csv", col_names = T, cols(district = col_factor(),urban = col_factor(),
gender = col_factor()))
#View(data)

#transform in tidy data
my_data <- data %>% gather(Year, Revenue,-c(County,`% increment (13/14 & 14/15`)) %>% select(-`% increment (13/14 & 14/15`)

#model
modelx <- my_data %>% filter(!is.na(County))%>% group_by(County) %>% nest(-County) %>% mutate(models = map(data, ~lm(Year~Revenue, .))) %>% mutate(tidied = map(models, tidy)) %>% unnest(tidied) %>% p.adjust(p.value > .5)#%>% ggplot(aes(x = Year))#%>% filter(term =="years") %>% select(estimate) %>% rename(expenses = "estimate" )



                
