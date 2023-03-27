setwd("~/Workspaces/TemplesOfSyrinx/DS")
getwd()

library(data.table)
library(ggplot2)
library(dplyr)
library(car)
library(FSA)
library(nortest)
library(ggstatsplot)

system.time(pop2021 <- fread("./Data/sensors-jan-2021.csv"))
system.time(pop2022 <- fread("./Data/sensors-jan-2022.csv"))
system.time(pop2023 <- fread("./Data/sensors-jan-2023.csv"))

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

mr2021 = na.omit(pop2021) %>%
  filter(location == 'Master Room') %>%
  select(temperature) %>%
  mutate(sample_id = '2021')

population.size <- nrow(mr2021)

sample.size <- ((confidence.level^2) * 0.25) / (confidence.interval)^2
sample.size <- sample.size/(1+(sample.size-1)/population.size)
sample.size <- round(sample.size, 0)
sample.size
margin.error <- sqrt(( confidence.level^2 * confidence.interval * ( 1 - confidence.interval ) ) / population.size)
margin.error

sample1 = na.omit(mr2021) %>%
  select(temperature) %>%
  mutate(sample_id = '2021') %>%
  sample_n(sample.size)

mr2022 = na.omit(pop2022) %>%
  filter(location == "Master Room") %>%
  select(temperature) %>%
  mutate(sample_id = '2022')

population.size <- nrow(mr2022)

sample.size <- ((confidence.level^2) * 0.25) / (confidence.interval)^2
sample.size <- sample.size/(1+(sample.size-1)/population.size)
sample.size <- round(sample.size, 0)
sample.size
margin.error <- sqrt(( confidence.level^2 * confidence.interval * ( 1 - confidence.interval ) ) / population.size)
margin.error

sample2 = na.omit(mr2022) %>%
  select(temperature) %>%
  mutate(sample_id = '2022') %>%
  sample_n(sample.size)

mr2023 = na.omit(pop2023) %>%
  filter(location == "MR") %>%
  select(temperature) %>%
  mutate(sample_id = '2023')

population.size <- nrow(mr2023)

sample.size <- ((confidence.level^2) * 0.25) / (confidence.interval)^2
sample.size <- sample.size/(1+(sample.size-1)/population.size)
sample.size <- round(sample.size, 0)
sample.size
margin.error <- sqrt(( confidence.level^2 * confidence.interval * ( 1 - confidence.interval ) ) / population.size)
margin.error

sample3 = na.omit(mr2023) %>%
  select(temperature) %>%
  mutate(sample_id = '2023') %>%
  sample_n(sample.size)


## Joining both sample1 and 2
samples = rbind(sample1, sample2)
samples = rbind(samples, sample3)

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

## Confidence Interval - Sample 3
se3 = sd(sample3$temperature) / sqrt(nrow(sample3))
lower = mean(sample3$temperature) - cl * se3
upper = mean(sample3$temperature) + cl * se3
ci3 = c(lower, upper)
mean(sample3$temperature)
ci3

toPlot = summarise(group_by(samples, sample_id), mean = mean(temperature))
toPlot = mutate(toPlot, lower = ifelse(toPlot$sample_id == '2022', ci1[1], ci2[1]))
toPlot = mutate(toPlot, upper = ifelse(toPlot$sample_id == '2022', ci1[2], ci2[2]))
ggplot(toPlot, aes(x = sample_id, y = mean, colour= sample_id)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = .1)

# Existe diferenca significativa na media das temperaturas nos dois quartos?

# Para aplicar o Teste t primeiro precisamos validar as 5 suposições do Teste.

# 1- Os dados são aleatórios e representativos da população.
# 2- A variável dependente é contínua.
# 3- Ambos os grupos são independentes (ou seja, grupos exaustivos e excludentes).
# 4- Os resíduos do modelo são normalmente distribuídos.
# 5- A variância residual é homogênea (princípio da homocedasticidade).

# Para o nosso exemplo neste estudo de caso, iremos considerar como verdadeiras as 
# suposições de 1 a 3 e validaremos as suposições 4 e 5. Para a suposição 4 usaremos 
# o Teste de Shapiro-Wilk e para a suposição 5 usaremos o Teste F.

# Vejamos um resumo estatístico do dataset
samples %>% group_by(sample_id) %>%
  summarise(
    count = n(),
    mean = mean(temperature, na.rm = TRUE),
    sd = sd(temperature, na.rm = TRUE),
    var = var(temperature, na.rm = TRUE),
    cov = (sd(temperature, na.rm = TRUE) / mean(temperature, na.rm = TRUE) * 100),
    median = median(temperature, na.rm = TRUE),
    min = min(temperature, na.rm = TRUE),
    max = max(temperature, na.rm = TRUE),
    range = (max(temperature, na.rm = TRUE) - min(temperature, na.rm = TRUE)),
    p95 = quantile(temperature, 0.95, na.rm = TRUE),
    p99 = quantile(temperature, 0.99, na.rm = TRUE)
  )

ggplot(samples) +
  aes(x = sample_id, y = temperature) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

hist(subset(samples, sample_id == "2021")$temperature,
     main = "MR 2022 temperature readings",
     xlab = "Celsius",
     breaks = 10     
)

hist(subset(samples, sample_id == "2022")$temperature,
     main = "MR 2022 temperature readings",
     xlab = "Celsius",
     breaks = 10     
)

hist(subset(samples, sample_id == "2023")$temperature,
     main = "MR 2023 temperature readings",
     xlab = "Celsius",
     breaks = 10
)

##### Normalitiy tests

res_aov <- aov(temperature ~ sample_id, 
               data = samples
)

hist(res_aov$residuals)

qqPlot(res_aov$residuals,
       id = FALSE # id = FALSE to remove point identification
)


# Shapiro-Wilk test: The Shapiro-Wilk test is a statistical test that tests the 
# null hypothesis that a sample is drawn from a normal distribution. If the 
# p-value of the test is greater than the significance level (e.g., 0.05), we 
# fail to reject the null hypothesis and conclude that the sample is 
# approximately normally distributed.
shapiro.test(samples$temperature) # p-value = 0.00000003086

# Anderson-Darling test: The Anderson-Darling test is another statistical test 
# that tests the null hypothesis that a sample is drawn from a normal 
# distribution. It is similar to the Shapiro-Wilk test but is more powerful 
# for detecting deviations from normality in the tails of the distribution. 
ad.test(samples$temperature) # p-value = 0.00000001896

# Analise: parece nao seguir uma distribuicao normal, e agora?

# the null and alternative hypothesis of the Wilcoxon test are as follows:
# H0: the 2 groups are equal in terms of the variable of interest
# H1: the 2 groups are different in terms of the variable of interest

# Applied to our research question we have:
# H0: temperature of MR and FR rooms are equal
# H1: temperature of MR and FR rooms are different

kruskal.test(temperature ~ sample_id, 
             data = samples)

dunnTest(temperature ~ sample_id,
         data = samples,
         method = "holm"
)

samples$sample_id <- as.factor(samples$sample_id)

ggbetweenstats(
  data = samples,
  x = sample_id,
  y = temperature,
  type = "nonparametric", # ANOVA or Kruskal-Wallis
  plot.type = "boxviolin",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = TRUE,
  bf.message = TRUE,
  outlier.tagging = TRUE
)

