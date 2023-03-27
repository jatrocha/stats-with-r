setwd("~/Workspaces/TemplesOfSyrinx/DS")
getwd()

library(data.table)
library(ggplot2)
library(dplyr)
library(car)
library(FSA)
library(nortest)
library(ggstatsplot)

system.time(pop2022 <- fread("./Data/buoyData-2022.csv"))

# Population summary.
pop2022 %>% group_by(station) %>%
  summarise(
    count = n(),
    mean = mean(air_temperature, na.rm = TRUE),
    sd = sd(air_temperature, na.rm = TRUE),
    var = var(air_temperature, na.rm = TRUE),
    cov = (sd(air_temperature, na.rm = TRUE) / mean(air_temperature, na.rm = TRUE) * 100),
    median = median(air_temperature, na.rm = TRUE),
    min = min(air_temperature, na.rm = TRUE),
    max = max(air_temperature, na.rm = TRUE),
    range = (max(air_temperature, na.rm = TRUE) - min(air_temperature, na.rm = TRUE)),
    p95 = quantile(air_temperature, 0.95, na.rm = TRUE),
    p99 = quantile(air_temperature, 0.99, na.rm = TRUE)
  )

ggplot(pop2022) +
  aes(x = station, y = air_temperature) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

##### Confidence level Z-Score
cl80 <- 1.28
cl85 <- 1.44
cl90 <- 1.65
cl95 <- 1.96
cl99 <- 2.58

#confidence.interval (aka Margin of Error)
ci90 <- 0.10
ci95 <- 0.05
ci99 <- 0.01

confidence.level <- cl95
confidence.interval <- ci95
set.seed(1000)

get_sample_size <- function(data, confidence.level, confidence.interval) {
  
  population.size <- nrow(data)
  
  sample.size <- ((confidence.level^2) * 0.25) / (confidence.interval)^2
  sample.size <- sample.size/(1+(sample.size-1)/population.size)
  sample.size <- round(sample.size, 0)
  
  #margin.error <- sqrt(( confidence.level^2 * confidence.interval * ( 1 - confidence.interval ) ) / population.size)
  #margin.error
  
  return(sample.size)
  
}

sample1 = na.omit(pop2022) %>%
  filter(station == 'M2') %>%
  select(air_temperature, station) %>%
  sample_n(get_sample_size(subset(pop2022, station == "M2"), confidence.level, confidence.interval))

sample2 = na.omit(pop2022) %>%
  filter(station == 'M3') %>%
  select(air_temperature, station) %>%
  sample_n(get_sample_size(subset(pop2022, station == "M3"), confidence.level, confidence.interval))

sample3 = na.omit(pop2022) %>%
  filter(station == 'M4') %>%
  select(air_temperature, station) %>%
  sample_n(get_sample_size(subset(pop2022, station == "M4"), confidence.level, confidence.interval))

sample4 = na.omit(pop2022) %>%
  filter(station == 'M5') %>%
  select(air_temperature, station) %>%
  sample_n(get_sample_size(subset(pop2022, station == "M5"), confidence.level, confidence.interval))

sample5 = na.omit(pop2022) %>%
  filter(station == 'M6') %>%
  select(air_temperature, station) %>%
  sample_n(get_sample_size(subset(pop2022, station == "M6"), confidence.level, confidence.interval))

## Joining all the samples...
samples = rbind(sample1, sample2)
samples = rbind(samples, sample3)
samples = rbind(samples, sample4)
samples = rbind(samples, sample5)

# Dataset summary.
samples %>% group_by(station) %>%
  summarise(
    count = n(),
    mean = mean(air_temperature, na.rm = TRUE),
    sd = sd(air_temperature, na.rm = TRUE),
    var = var(air_temperature, na.rm = TRUE),
    cov = (sd(air_temperature, na.rm = TRUE) / mean(air_temperature, na.rm = TRUE) * 100),
    median = median(air_temperature, na.rm = TRUE),
    min = min(air_temperature, na.rm = TRUE),
    max = max(air_temperature, na.rm = TRUE),
    range = (max(air_temperature, na.rm = TRUE) - min(air_temperature, na.rm = TRUE)),
    p95 = quantile(air_temperature, 0.95, na.rm = TRUE),
    p99 = quantile(air_temperature, 0.99, na.rm = TRUE)
  )


#### Confidence intervals...
my_data <- data.frame(
  station = character(),
  mean = numeric(),
  lower = numeric(),
  upper = numeric(),
  stringsAsFactors = FALSE # optional, prevents automatic conversion of strings to factors
)

## Confidence Interval - Sample 1
se1 = sd(sample1$air_temperature) / sqrt(nrow(sample1))
lower = mean(sample1$air_temperature) - confidence.level * se1
upper = mean(sample1$air_temperature) + confidence.level * se1
ci1 = c(lower, upper)
my_data[1, ] <- c('M2', round(mean(sample1$air_temperature), 2), round(ci1[1], 2), round(ci1[2], 2)) 

## Confidence Interval - Sample 2
se2 = sd(sample2$air_temperature) / sqrt(nrow(sample2))
lower = mean(sample2$air_temperature) - confidence.level * se2
upper = mean(sample2$air_temperature) + confidence.level * se2
ci2 = c(lower, upper)
my_data[2, ] <- c('M3', round(mean(sample2$air_temperature), 2), round(ci2[1], 2), round(ci2[2], 2)) 

## Confidence Interval - Sample 3
se3 = sd(sample3$air_temperature) / sqrt(nrow(sample3))
lower = mean(sample3$air_temperature) - confidence.level * se3
upper = mean(sample3$air_temperature) + confidence.level * se3
ci3 = c(lower, upper)
my_data[3, ] <- c('M4', round(mean(sample3$air_temperature), 2), round(ci3[1], 2), round(ci3[2], 2)) 

## Confidence Interval - Sample 4
se4 = sd(sample4$air_temperature) / sqrt(nrow(sample4))
lower = mean(sample4$air_temperature) - confidence.level * se4
upper = mean(sample4$air_temperature) + confidence.level * se4
ci4 = c(lower, upper)
my_data[4, ] <- c('M5', round(mean(sample4$air_temperature), 2), round(ci4[1], 2), round(ci4[2], 2)) 

## Confidence Interval - Sample 5
se5 = sd(sample5$air_temperature) / sqrt(nrow(sample5))
lower = mean(sample5$air_temperature) - confidence.level * se5
upper = mean(sample5$air_temperature) + confidence.level * se5
ci5 = c(lower, upper)
my_data[5, ] <- c('M6', round(mean(sample5$air_temperature), 2), round(ci5[1], 2), round(ci5[2], 2)) 

ggplot(my_data, aes(x = station, y = mean, colour= station)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = .1)

ggplot(samples) +
  aes(x = station, y = air_temperature) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

hist(subset(samples, station == "M2")$air_temperature,
     main = "M2 Air temperature",
     xlab = "Celsius",
     breaks = 10
)

hist(subset(samples, station == "M3")$air_temperature,
     main = "M3 Air temperature",
     xlab = "Celsius",
     breaks = 10
)

hist(subset(samples, station == "M4")$air_temperature,
     main = "M4 Air temperature",
     xlab = "Celsius",
     breaks = 10
)

hist(subset(samples, station == "M5")$air_temperature,
     main = "M5 Air temperature",
     xlab = "Celsius",
     breaks = 10
)

hist(subset(samples, station == "M6")$air_temperature,
     main = "M6 Air temperature",
     xlab = "Celsius",
     breaks = 10
)

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


##### Normalitiy tests
res_aov <- aov(air_temperature ~ station, data = samples)

par(mfrow = c(1, 2)) # combine plots

# 1. Homogeneity of variances 
plot(res_aov, which = 3)

# 2. Normality 
plot(res_aov, which = 2)

hist(res_aov$residuals)

qqPlot(res_aov$residuals,
       id = FALSE # id = FALSE to remove point identification
)

par(mfrow = c(1, 1)) # back to normal

# Shapiro-Wilk test: The Shapiro-Wilk test is a statistical test that tests the 
# null hypothesis that a sample is drawn from a normal distribution. If the 
# p-value of the test is greater than the significance level (e.g., 0.05), we 
# fail to reject the null hypothesis and conclude that the sample is 
# approximately normally distributed.
shapiro.test(sample1$air_temperature) # p-value = 0.0000001036
shapiro.test(sample2$air_temperature) # p-value = 0.00000001722

# Anderson-Darling test: The Anderson-Darling test is another statistical test 
# that tests the null hypothesis that a sample is drawn from a normal 
# distribution. It is similar to the Shapiro-Wilk test but is more powerful 
# for detecting deviations from normality in the tails of the distribution. 
ad.test(sample1$air_temperature) # p-value = 0.0000007717
ad.test(sample2$air_temperature) # p-value = 0.0000000004449

# Kolmogorov-Smirnov test: The Kolmogorov-Smirnov test is a non-parametric test 
# that tests the null hypothesis that a sample is drawn from a specified 
# distribution. If the p-value of the test is greater than the significance 
# level (e.g., 0.05), we fail to reject the null hypothesis and conclude 
# that the sample is approximately normally distributed
ks.test(sample1$air_temperature, sample2$air_temperature) # p-value = 0.03315

# O valor-p do teste de cada grupo é menor que 0.05 e então rejeitamos a H0.
# Nao podemos assumir que os dados seguem uma distribuição normal.

# Analise: parece nao seguir uma distribuicao normal, e agora?
# Wilcoxon test FTW!

# the null and alternative hypothesis of the Wilcoxon test are as follows:
# H0: the 2 groups are equal in terms of the variable of interest
# H1: the 2 groups are different in terms of the variable of interest

# Applied to our research question we have:
# H0: temperature of 2022 and 2023 rooms are equal
# H1: temperature of 2022 and 2023 rooms are different

kruskal.test(air_temperature ~ station,
             data = samples
)

dunnTest(air_temperature ~ station,
         data = samples,
         method = "holm")

ggbetweenstats(
  data = samples,
  x = station,
  y = air_temperature,
  type = "nonparametric", # ANOVA or Kruskal-Wallis
  plot.type = "boxviolin",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = TRUE,
  bf.message = TRUE,
  outlier.tagging = TRUE,
  title = "Sea level air temperature Comparison",
  ylab = "Celsius",
  xlab = "Station"
)

library(coin)
conover_test(air_temperature ~ as.factor(station), data = samples)

#samples$station <- as.factor(samples$station)

pairwise.wilcox.test(samples$air_temperature, samples$station, p.adjust.method = "holm")
