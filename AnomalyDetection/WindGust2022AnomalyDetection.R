setwd("~/Workspaces/TemplesOfSyrinx/DS")
getwd()

library(data.table)
library(nortest)
library(outliers)
library(anomalize)
library(tidyverse)
library(car)

system.time(pop2022 <- fread("./data/wind-2-2022.csv"))
pop2022$date <- paste(pop2022$year, pop2022$month, pop2022$day, sep = "-")
pop2022$time <- paste(pop2022$hour, pop2022$minute, "00", sep=":")
pop2022$timestamp <- paste(pop2022$date, pop2022$time, sep=" ")
pop2022$timestamp <- as.POSIXct(pop2022$timestamp)
pop2022<- pop2022[order(timestamp),]

head(pop2022)

# Population summary.
pop2022 %>% 
  summarise(
    count = n(),
    mean = mean(wind_gust, na.rm = TRUE),
    sd = sd(wind_gust, na.rm = TRUE),
    var = var(wind_gust, na.rm = TRUE),
    cov = (sd(wind_gust, na.rm = TRUE) / mean(wind_gust, na.rm = TRUE) * 100),
    median = median(wind_gust, na.rm = TRUE),
    min = min(wind_gust, na.rm = TRUE),
    max = max(wind_gust, na.rm = TRUE),
    range = (max(wind_gust, na.rm = TRUE) - min(wind_gust, na.rm = TRUE)),
    p95 = quantile(wind_gust, 0.95, na.rm = TRUE),
    p99 = quantile(wind_gust, 0.99, na.rm = TRUE)
  )

ggplot(pop2022) +
  aes(x = as.factor(year), y = wind_gust) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

hist(pop2022$wind_gust)

# alpha - for comparison
alpha <- 0.05

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

sample = na.omit(pop2022) %>%
  select(wind_gust, timestamp, year) %>%
  sample_n(get_sample_size(pop2022, confidence.level, confidence.interval))

# Dataset summary.
sample %>% 
  summarise(
    count = n(),
    mean = mean(wind_gust, na.rm = TRUE),
    sd = sd(wind_gust, na.rm = TRUE),
    var = var(wind_gust, na.rm = TRUE),
    cov = (sd(wind_gust, na.rm = TRUE) / mean(wind_gust, na.rm = TRUE) * 100),
    median = median(wind_gust, na.rm = TRUE),
    min = min(wind_gust, na.rm = TRUE),
    max = max(wind_gust, na.rm = TRUE),
    range = (max(wind_gust, na.rm = TRUE) - min(wind_gust, na.rm = TRUE)),
    p95 = quantile(wind_gust, 0.95, na.rm = TRUE),
    p99 = quantile(wind_gust, 0.99, na.rm = TRUE)
  )

boxplot(sample$wind_gust)
hist(sample$wind_gust)

ggplot(sample) +
  aes(x = as.factor(year), y = wind_gust) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

##### Normalitiy tests
res_aov <- aov(wind_gust ~ year, data = sample)

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
result <- shapiro.test(sample$wind_gust)
print(result$p.value > alpha)

# Anderson-Darling test: The Anderson-Darling test is another statistical test 
# that tests the null hypothesis that a sample is drawn from a normal 
# distribution. It is similar to the Shapiro-Wilk test but is more powerful 
# for detecting deviations from normality in the tails of the distribution. 
result <- ad.test(sample$wind_gust)
print(result$p.value > alpha)

##### Grubs's test (for data follwing a normal distribution)
### If the p-value is less than the chosen significance level (e.g., 0.05), 
### we reject the null hypothesis of no outliers and conclude that at least one 
### observation in the data set is an outlier.
result <- grubbs.test(pop2022$wind_gust)
result
result$p.value < alpha

##### MAD-based outlier detection
# 1 Compute the median and MAD of your data using the median() and mad()
my_median <- median(pop2022$wind_gust)
my_mad <- mad(pop2022$wind_gust)

# 2 Define a threshold based on a multiple of the MAD. One common threshold 
# is 3 or 3.5 times the MAD
threshold <- 3.5 * my_mad

# 3 Identify outliers as observations that are more than the threshold distance 
# from the median:
outliers <- pop2022$wind_gust[abs(pop2022$wind_gust - my_median) > threshold]

# 4 Visualization
plot(pop2022$wind_gust)

points(which(abs(pop2022$wind_gust- my_median) > threshold), 
       pop2022$wind_gust[abs(pop2022$wind_gust - my_median) > threshold], 
       col = "red")


pop2022_tibble = na.omit(pop2022) %>%
  select(wind_gust, timestamp) %>%
  as_tibble()

##### Detecting anomalies using anomalize - Provides a comprehensive set of 
##### tools for detecting and correcting anomalies in time series data.

# Classical STL (seasonal and trend decomposition using Loess) algorithm to 
# decompose time series data into its seasonal, trend, and remainder components. 
# The stl method is a well-established method for seasonal decomposition, and 
# it works well for many time series data sets. However, it can be sensitive to 
# extreme values and outliers in the data, which can result in inaccurate 
# decompositions.
# ps.: takes around 00:43:51 to complete
pop2022_tibble %>% 
  time_decompose(wind_gust, method = "stl", frequency = "auto", trend = "auto") %>%  
  anomalize(remainder, method = "gesd", alpha = 0.05, max_anoms = 0.1) %>% 
  plot_anomaly_decomposition()

# The twitter method  is an adaptation of the STL algorithm that was developed 
# by the data science team at Twitter. The twitter method uses a robust version 
# of the STL algorithm that is less sensitive to extreme values and outliers in 
# the data. This can lead to more accurate decompositions for time series data 
# sets that contain outliers or other anomalies.
pop2022_tibble %>% 
  time_decompose(wind_gust, method = "twitter", frequency = "auto", trend = "auto") %>%  
  anomalize(remainder, method = "gesd", alpha = 0.05, max_anoms = 0.1) %>% 
  plot_anomaly_decomposition()

anomalies1=pop2022_tibble %>% 
  time_decompose(wind_gust) %>%  
  anomalize(remainder) %>%  
  time_recompose() %>%  
  filter(anomaly == 'Yes')


a <- time_decompose(wind_gust, method = "twitter", frequency = "1 week", trend = "1 month", data = sampley)
system.time(b <- anomalize(remainder, method = "gesd", alpha = 0.05, max_anoms = 0.1, data = a))

# Visualize anomalies
plot_anomalies(b)


system.time(pop22 <- fread("/Users/jatrocha/wind-2022.csv"))
pop22$date <- paste(pop22$year, pop22$month, pop22$day, sep = "-")
pop22$date <- as.POSIXct(pop22$date, format="%Y-%m-%d")
str(pop22)


library(AnomalyDetection)

samplex = na.omit(pop2022_tibble) %>%
  select(timestamp, wind_gust)

anomalies = AnomalyDetectionTs(samplex, direction="pos", plot=TRUE)

AnomalyDetectionVec(samplex[,2], max_anoms=0.02, period=1440, direction='both', plot=TRUE)

anomalies$plot

anomalies$anoms$timestamp <- as.POSIXct(anomalies$anoms$timestamp)

head(anomalies$anoms)

