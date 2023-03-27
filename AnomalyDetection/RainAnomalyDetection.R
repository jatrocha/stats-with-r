setwd("~/Workspaces/TemplesOfSyrinx/DS")
getwd()

library(AnomalyDetection)
library(tidyverse)
library(anomalize)
library(data.table)
library(ggplot2)

##### Loading an transforming the data.
system.time(population <- fread("./data/rain-22-to-23.csv"))
population$date <- paste(population$year, population$month, population$day, sep = "-")
population$date <- as.POSIXct(population$date, format="%Y-%m-%d")

### Where are we at?

# Population summary.
population %>% 
  summarise(
    count = n(),
    mean = mean(daily_precipitation, na.rm = TRUE),
    sd = sd(daily_precipitation, na.rm = TRUE),
    var = var(daily_precipitation, na.rm = TRUE),
    cov = (sd(daily_precipitation, na.rm = TRUE) / mean(daily_precipitation, na.rm = TRUE) * 100),
    median = median(daily_precipitation, na.rm = TRUE),
    min = min(daily_precipitation, na.rm = TRUE),
    max = max(daily_precipitation, na.rm = TRUE),
    range = (max(daily_precipitation, na.rm = TRUE) - min(daily_precipitation, na.rm = TRUE)),
    p95 = quantile(daily_precipitation, 0.95, na.rm = TRUE),
    p99 = quantile(daily_precipitation, 0.99, na.rm = TRUE)
  )

ggplot(population, aes(x=date, y=daily_precipitation, color=daily_precipitation)) + geom_line()

hist(population$daily_precipitation)

population %>% 
  summarise(
    count = n(),
    mean = mean(daily_duration, na.rm = TRUE),
    sd = sd(daily_duration, na.rm = TRUE),
    var = var(daily_duration, na.rm = TRUE),
    cov = (sd(daily_duration, na.rm = TRUE) / mean(daily_duration, na.rm = TRUE) * 100),
    median = median(daily_duration, na.rm = TRUE),
    min = min(daily_duration, na.rm = TRUE),
    max = max(daily_duration, na.rm = TRUE),
    range = (max(daily_duration, na.rm = TRUE) - min(daily_duration, na.rm = TRUE)),
    p95 = quantile(daily_duration, 0.95, na.rm = TRUE),
    p99 = quantile(daily_duration, 0.99, na.rm = TRUE)
  )

ggplot(population, aes(x=date, y=daily_duration, color=daily_duration)) + geom_line()

hist(population$daily_duration)

##### MAD-based outlier detection
# 1 Compute the median and MAD of your data using the median() and mad()
my_median <- median(population$daily_precipitation)
my_mad <- mad(population$daily_precipitation)

# 2 Define a threshold based on a multiple of the MAD. One common threshold 
# is 3 or 3.5 times the MAD
threshold <- 3.5 * my_mad

# 3 Identify outliers as observations that are more than the threshold distance 
# from the median:
daily_precipitation_outliers <- population$daily_precipitation[abs(population$daily_precipitation - my_median) > threshold]

# 4 Visualization
plot(population$daily_precipitation)

points(which(abs(population$daily_precipitation- my_median) > threshold), 
       population$daily_precipitation[abs(population$daily_precipitation - my_median) > threshold], 
       col = "red")

##### MAD-based outlier detection
# 1 Compute the median and MAD of your data using the median() and mad()
my_median <- median(population$daily_duration)
my_mad <- mad(population$daily_duration)

# 2 Define a threshold based on a multiple of the MAD. One common threshold 
# is 3 or 3.5 times the MAD
threshold <- 3.5 * my_mad

# 3 Identify outliers as observations that are more than the threshold distance 
# from the median:
daily_duration_outliers <- population$daily_duration[abs(population$daily_duration - my_median) > threshold]

# 4 Visualization
plot(population$daily_duration)

points(which(abs(population$daily_duration- my_median) > threshold), 
       population$daily_duration[abs(population$daily_duration - my_median) > threshold], 
       col = "red")

##### Detecting anomalies in time series data using a statistical method known 
##### as Seasonal Hybrid ESD (S-H-ESD)

precipitation <- population %>%
  select(date, daily_precipitation)

duration <- population %>%
  select(date, daily_duration)

### max_anoms parameter to 0.02, which specifies that we want to identify the 
### top 2% of anomalous data points. We also set the direction parameter to 
### "both", which specifies that we want to identify both positive and negative 
### anomalies.
precipitation_anomalies_ts = AnomalyDetectionTs(precipitation, max_anoms=0.02, direction="both", plot=TRUE)
precipitation_anomalies_ts$plot
precipitation_anomalies_ts$anoms

precipitation_anomalies_vec <- AnomalyDetectionVec(precipitation[,2], max_anoms=0.02, period=7, direction='both', plot=TRUE)
precipitation_anomalies_vec$anoms
precipitation_anomalies_vec$plot

duration_anomalies_ts = AnomalyDetectionTs(duration, direction="pos", plot=TRUE)
duration_anomalies_ts$anoms
duration_anomalies_ts$plot

duration_anomalies_vec <- AnomalyDetectionVec(duration[,2], max_anoms=0.02, period=7, direction='both', plot=TRUE)
duration_anomalies_vec$plot
duration_anomalies_vec$anoms

##### Detecting anomalies in time series data using a statistical method known 
##### as STL (Seasonal and Trend decomposition using Loess)

precipitation_tibble = na.omit(population) %>%
  select(daily_precipitation, date) %>%
  as_tibble()

### using the STL algorithm (method = "stl"), and scale the remainder component 
### using the median absolute deviation (mad()) function. We also set the alpha 
### parameter to 0.05, which controls the significance level for identifying 
### anomalies, and the max_anoms parameter to 0.1, which specifies the maximum 
### proportion of data points that can be identified as anomalies.

# Classical STL (seasonal and trend decomposition using Loess) algorithm to 
# decompose time series data into its seasonal, trend, and remainder components. 
# The stl method is a well-established method for seasonal decomposition, and 
# it works well for many time series data sets. However, it can be sensitive to 
# extreme values and outliers in the data, which can result in inaccurate 
# decompositions.

precipitation_anomalies_stl=precipitation_tibble %>% 
  time_decompose(daily_precipitation, method = "stl", frequency = "auto", trend = "auto") %>%  
  anomalize(remainder, method = "gesd", alpha = 0.05, max_anoms = 0.1) %>% 
  time_recompose() %>%  
  filter(anomaly == 'Yes')

precipitation_anomalies_stl

precipitation_tibble %>% 
  time_decompose(daily_precipitation, method = "stl", frequency = "auto", trend = "auto") %>%  
  anomalize(remainder, method = "gesd", alpha = 0.05, max_anoms = 0.1) %>% 
  plot_anomaly_decomposition()

precipitation_tibble %>% 
  time_decompose(daily_precipitation, method = "stl", frequency = "auto", trend = "auto") %>%  
  anomalize(remainder, method = "gesd", alpha = 0.05, max_anoms = 0.1) %>% 
  plot_anomalies()

### using the Twitter algorithm (method = "twitter")
### The twitter method  is an adaptation of the STL algorithm that was developed 
### by the data science team at Twitter. The twitter method uses a robust version 
### of the STL algorithm that is less sensitive to extreme values and outliers in 
### the data. This can lead to more accurate decompositions for time series data 
### sets that contain outliers or other anomalies.

precipitation_anomalies_twitter=precipitation_tibble %>% 
  time_decompose(daily_precipitation, method = "twitter", frequency = "auto", trend = "auto") %>%  
  anomalize(remainder, method = "gesd", alpha = 0.05, max_anoms = 0.1) %>% 
  time_recompose() %>%  
  filter(anomaly == 'Yes')

print(precipitation_anomalies_twitter)

precipitation_tibble %>% 
  time_decompose(daily_precipitation, method = "twitter", frequency = "auto", trend = "auto") %>%  
  anomalize(remainder, method = "gesd", alpha = 0.05, max_anoms = 0.1) %>% 
  plot_anomaly_decomposition()

precipitation_tibble %>% 
  time_decompose(daily_precipitation, method = "twitter", frequency = "auto", trend = "auto") %>%  
  anomalize(remainder, method = "gesd", alpha = 0.05, max_anoms = 0.1) %>% 
  plot_anomalies()

