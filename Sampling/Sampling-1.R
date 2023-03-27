setwd("~/Workspaces/TemplesOfSyrinx/DS")
getwd()

library(data.table)
library(TeachingSampling)
library('samplingbook')

# Eliminando notacao cientifica
#options(scipen = 9)

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

population.size <- nrow(population)

sample.size <- ((confidence.level^2) * 0.25) / (confidence.interval)^2
sample.size <- sample.size/(1+(sample.size-1)/population.size)
sample.size <- round(sample.size, 0)
sample.size
margin.error <- sqrt(( confidence.level^2 * confidence.interval * ( 1 - confidence.interval ) ) / population.size)
margin.error

##### Amostragem aleatoria simples
sam=S.SI(population.size,sample.size)
sample1=population[sam,]
# Estimador HT para estimar o total populacional (ou media) e sua variancia
E.SI(population.size, sample.size, sample1$temperature)

##### Amostragem sistematica
a=population.size/sample.size
# Selecao de elementos
sam=S.SY(population.size,a)
# Amostra selecionada
sample2=population[sam,]
# Estimador HT para o total e para a media populacional
E.SY(population.size, a, sample2$temperature)

library(pastecs)
stat.desc(sample1, norm=TRUE)
stat.desc(sample2, norm=TRUE)

library(summarytools) 
descr(sample1,
      headings = FALSE, # remove headings
      stats = "common" # most common descriptive statistics 
)

descr(sample2,
      headings = FALSE, # remove headings
      stats = "common" # most common descriptive statistics 
)


st=numeric()
for(i in 1:sample.size) {
  x=sample(population$outside_temperature, 1000, replace=TRUE)
  st[i]=mean(x)
}
hist(st)

stat.desc(st, norm=TRUE)
library(car)
qqPlot(st)

shapiro.test(st)
shapiro.test(sample1$outside_temperature)
shapiro.test(sample2$outside_temperature)

hist(st, prob=TRUE, ylim = c(0,5.0), breaks=20)
curve(dnorm(x, mean(st), sd(st)), add=TRUE, col="red", lwd=1)
curve(dnorm(x, mean(st), sd(st) *2), add=TRUE, col="blue", lwd=1)

pnorm(2.12, mean(st), sd(st), lower.tail = TRUE)

f <- dnorm(st, mean(st), sd(st))
plot(st,f, lwd = 2, col = "blue", ylab="", xlab="temperature" )
abline(v = mean(st))


lb <- min(st)
ub <- 20

x2 <- seq(lb, ub, length=10)
y <- dnorm(x2, mean(st), sd(st))

plot(st, f, type = "l", lwd = 2, col = "blue", ylab = "", xlab = "Temperature")
abline(v = ub) 

polygon(c(lb, x2, ub), c(0, y, 0), col = rgb(0, 0, 1, alpha = 0.5))
text(995, 0.01, "84.13%")

normal_area <- function(mean = 0, sd = 1, lb, ub, acolor = "lightgray", ...) {
  x <- seq(mean - 3 * sd, mean + 3 * sd, length = 100) 
  
  if (missing(lb)) {
    lb <- min(x)
  }
  if (missing(ub)) {
    ub <- max(x)
  }
  
  x2 <- seq(lb, ub, length = 100)    
  plot(x, dnorm(x, mean, sd), type = "n", ylab = "")
  
  y <- dnorm(x2, mean, sd)
  polygon(c(lb, x2, ub), c(0, y, 0), col = acolor)
  lines(x, dnorm(x, mean, sd), type = "l", ...)
}

normal_area(mean=mean(sample1$outside_temperature), sd = sd(sample1$outside_temperature), lb=min(sample1$outside_temperature), ub=max(sample1$outside_temperature), lwd=2)

pnorm(25, mean(sample1$outside_temperature), sd(sample1$outside_temperature), lower.tail = FALSE)



# Bootstrap 95% CI for R-Squared
library(boot)

# function to obtain R-Squared from the data 
rsq <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample 
  fit <- lm(formula, data=d)
  return(summary(fit)$r.square)
} 
# bootstrapping with 1000 replications 
results <- boot(data=population, statistic=rsq, 
                R=1000, formula=temperature~feels_like)

# view results
results 
plot(results)

# get 95% confidence interval 
boot.ci(results, type="bca")

nrow(results$data)


# Bootstrap 95% CI for regression coefficients 
library(boot)
# function to obtain regression weights 
bs <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample 
  fit <- lm(formula, data=d)
  return(coef(fit)) 
} 
# bootstrapping with 1000 replications 
results <- boot(data=mtcars, statistic=bs, 
                R=1000, formula=mpg~wt+disp)

# view results
results
plot(results, index=1) # intercept 
plot(results, index=2) # wt 
plot(results, index=3) # disp 

# get 95% confidence intervals 
boot.ci(results, type="bca", index=1) # intercept 
boot.ci(results, type="bca", index=2) # wt 
boot.ci(results, type="bca", index=3) # disp

?boot


