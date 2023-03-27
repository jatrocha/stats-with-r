setwd("~/Workspaces/TemplesOfSyrinx/DS")
getwd()

library(data.table)
library(ggplot2)
library(dplyr)

system.time(population <- fread("./Data/sensors-jan-2023.csv"))

cl80 <- 1.28
cl85 <- 1.44
cl90 <- 1.65
cl95 <- 1.96
cl99 <- 2.58

#confidence.interval (aka Margin of Error)
#ci 90, confidence.interval = 0.10
#ci 95, confidence.interval = 0.05
#ci 99, confidence.interval = 0.01

confidence.level <- cl95
confidence.interval <- 5/100

mr = na.omit(population) %>%
  filter(location == 'MR') %>%
  select(temperature, outside_temperature) %>%
  mutate(sample_id = '1')

population.size <- nrow(mr)

sample.size <- ((confidence.level^2) * 0.25) / (confidence.interval)^2
sample.size <- sample.size/(1+(sample.size-1)/population.size)
sample.size <- round(sample.size, 0)
sample.size
margin.error <- sqrt(( confidence.level^2 * confidence.interval * ( 1 - confidence.interval ) ) / population.size)
margin.error

sample1 = na.omit(mr) %>%
  select(temperature, outside_temperature) %>%
  mutate(sample_id = '1') %>%
  sample_n(sample.size)

fr = na.omit(population) %>%
  filter(location == 'FR') %>%
  select(temperature, outside_temperature) %>%
  mutate(sample_id = '2')

population.size <- nrow(fr)

sample.size <- ((confidence.level^2) * 0.25) / (confidence.interval)^2
sample.size <- sample.size/(1+(sample.size-1)/population.size)
sample.size <- round(sample.size, 0)
sample.size
margin.error <- sqrt(( confidence.level^2 * confidence.interval * ( 1 - confidence.interval ) ) / population.size)
margin.error

sample2 = na.omit(fr) %>%
  select(temperature, outside_temperature) %>%
  mutate(sample_id = '2') %>%
  sample_n(sample.size)

## Joining both sample1 and 2
samples = rbind(sample1, sample2)

##### Confidence Interval
cl95 <- 1.96
cl99 <- 2.58
cl <- cl95

## Confidence Interval - Sample 1
se1 = sd(sample1$temperature) / sqrt(nrow(sample1))
lower = mean(sample1$temperature) - cl * se1
upper = mean(sample1$temperature) + cl * se1
ci1 = c(lower, upper)
mean(sample1$temperature)
ci1

## Confidence Interval - Sample 2
se2 = sd(sample2$temperature) / sqrt(nrow(sample2))
lower = mean(sample2$temperature) - cl * se2
upper = mean(sample2$temperature) + cl * se2
ci2 = c(lower, upper)
mean(sample2$temperature)
ci2

toPlot = summarise(group_by(samples, sample_id), mean = mean(temperature))
toPlot = mutate(toPlot, lower = ifelse(toPlot$sample_id == 1, ci1[1], ci2[1]))
toPlot = mutate(toPlot, upper = ifelse(toPlot$sample_id == 1, ci1[2], ci2[2]))
ggplot(toPlot, aes(x = sample_id, y = mean, colour= sample_id)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = .1)


t.test(sample1$temperature, sample2$temperature, alternative="g")
t.test(sample1$outside_temperature, sample2$outside_temperature, alternative="g")
