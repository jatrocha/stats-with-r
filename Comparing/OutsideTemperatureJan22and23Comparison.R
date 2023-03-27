setwd("~/Workspaces/TemplesOfSyrinx/DS")
getwd()

library(data.table)
library(ggplot2)
library(dplyr)
library(car)
library(FSA)
library(nortest)
library(ggstatsplot)
library(outliers)

system.time(pop2022 <- fread("./Data/sensors-jan-2022.csv"))
system.time(pop2023 <- fread("./Data/sensors-jan-2023.csv"))

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

get_sample_size <- function(data, confidence.level, confidence.interval) {

  population.size <- nrow(data)
  
  sample.size <- ((confidence.level^2) * 0.25) / (confidence.interval)^2
  sample.size <- sample.size/(1+(sample.size-1)/population.size)
  sample.size <- round(sample.size, 0)
  
  #margin.error <- sqrt(( confidence.level^2 * confidence.interval * ( 1 - confidence.interval ) ) / population.size)
  #margin.error
  
  return(sample.size)
    
}

mr = na.omit(pop2022) %>%
  select(outside_temperature) %>%
  mutate(sample_id = '2022')

sample1 = na.omit(mr) %>%
  select(outside_temperature) %>%
  mutate(sample_id = '2022') %>%
  sample_n(get_sample_size(mr, cl95, ci95))

fr = na.omit(pop2023) %>%
  select(outside_temperature) %>%
  mutate(sample_id = '2023')

sample2 = na.omit(fr) %>%
  select(outside_temperature) %>%
  mutate(sample_id = '2023') %>%
  sample_n(get_sample_size(fr, cl95, ci95))

## Joining both sample1 and 2
samples = rbind(sample1, sample2)

## Confidence Interval - Sample 1
se1 = sd(sample1$outside_temperature) / sqrt(nrow(sample1))
lower = mean(sample1$outside_temperature) - confidence.level * se1
upper = mean(sample1$outside_temperature) + confidence.level * se1
ci1 = c(lower, upper)

## Confidence Interval - Sample 2
se2 = sd(sample2$outside_temperature) / sqrt(nrow(sample2))
lower = mean(sample2$outside_temperature) - confidence.level * se2
upper = mean(sample2$outside_temperature) + confidence.level * se2
ci2 = c(lower, upper)

toPlot = summarise(group_by(samples, sample_id), mean = mean(outside_temperature))
toPlot = mutate(toPlot, lower = ifelse(toPlot$sample_id == '2022', ci1[1], ci2[1]))
toPlot = mutate(toPlot, upper = ifelse(toPlot$sample_id == '2022', ci1[2], ci2[2]))
ggplot(toPlot, aes(x = sample_id, y = mean, colour= sample_id)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = .1)

# Dataset summary.
samples %>% group_by(sample_id) %>%
  summarise(
    count = n(),
    mean = mean(outside_temperature, na.rm = TRUE),
    sd = sd(outside_temperature, na.rm = TRUE),
    var = var(outside_temperature, na.rm = TRUE),
    cov = (sd(outside_temperature, na.rm = TRUE) / mean(outside_temperature, na.rm = TRUE)),
    median = median(outside_temperature, na.rm = TRUE),
    min = min(outside_temperature, na.rm = TRUE),
    max = max(outside_temperature, na.rm = TRUE),
    range = (max(outside_temperature, na.rm = TRUE) - min(outside_temperature, na.rm = TRUE)),
    p95 = quantile(outside_temperature, 0.95, na.rm = TRUE),
    p99 = quantile(outside_temperature, 0.99, na.rm = TRUE)
  )

##### Grubs's test
hist(samples$outside_temperature, breaks = 6)

grubbs.test(pop2023$outside_temperature)

ggplot(samples) +
  aes(x = sample_id, y = outside_temperature) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

hist(subset(samples, sample_id == "2022")$outside_temperature,
     main = "Outside 2022 temperature",
     xlab = "Celsius",
     breaks = 10
)

hist(subset(samples, sample_id == "2023")$outside_temperature,
     main = "Outside 2023 temperature",
     xlab = "Celsius",
     breaks = 10
)

ggbetweenstats(
  data = samples,
  x = sample_id,
  y = outside_temperature,
  type = "nonparametric", # ANOVA or Kruskal-Wallis
  plot.type = "boxviolin",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = TRUE,
  bf.message = FALSE,
  outlier.tagging = TRUE,
  title = "Jan 2022/2023 Outside Temperature Comparison",
  ylab = "Celsius",
  xlab = "Year"
)

ggwithinstats(
  data = samples,
  x = sample_id,
  y = outside_temperature,
  type = "nonparametric", # for student's t-test
  centrality.plotting = TRUE, # remove mean
  title = "Jan 2022/2023 Outside Temperature Comparison",
  ylab = "Celsius",
  xlab = "Year"
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
res_aov <- aov(outside_temperature ~ sample_id, data = samples)

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
shapiro.test(sample1$outside_temperature) # p-value = 0.0000001036
shapiro.test(sample2$outside_temperature) # p-value = 0.00000001722

# Anderson-Darling test: The Anderson-Darling test is another statistical test 
# that tests the null hypothesis that a sample is drawn from a normal 
# distribution. It is similar to the Shapiro-Wilk test but is more powerful 
# for detecting deviations from normality in the tails of the distribution. 
ad.test(sample1$outside_temperature) # p-value = 0.0000007717
ad.test(sample2$outside_temperature) # p-value = 0.0000000004449

# Kolmogorov-Smirnov test: The Kolmogorov-Smirnov test is a non-parametric test 
# that tests the null hypothesis that a sample is drawn from a specified 
# distribution. If the p-value of the test is greater than the significance 
# level (e.g., 0.05), we fail to reject the null hypothesis and conclude 
# that the sample is approximately normally distributed
ks.test(sample1$outside_temperature, sample2$outside_temperature) # p-value = 0.03315

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

wilcox.test(samples$outside_temperature ~ samples$sample_id)
# we reject the null hyphothesis and the temperatures are significantly different between the two rooms.

wilcox.test(samples$outside_temperature ~ samples$sample_id,
            alternative = "greater") 

wilcox.test(samples$outside_temperature ~ samples$sample_id, 
            correct = FALSE, 
            exact = FALSE)
