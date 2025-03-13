#######################################################            Lab 7              ###############################################################################
#####################################################################################################################################################################

library(tidyverse)

#### Task 1 ####     **********export plots and data frame of values(xtable) to the write up************
alpha <- 2
beta<- 5
mean <- alpha / (alpha + beta)
variance <- (alpha * beta) / ((alpha + beta)^2 * (alpha + beta + 1))
skew <- (2 * (beta - alpha) * sqrt(alpha + beta + 1)) / ((alpha + beta + 2) * sqrt(alpha * beta))
kurt <- (6 * ((alpha - beta)^2 * (alpha + beta + 1) - alpha * beta * (alpha + beta + 2))) / (alpha * beta * (alpha + beta + 2) * (alpha + beta + 3))
two.and.five.df <- data.frame(Statistic = c("Mean", "Variance", "Skewness", "Kurtosis"), Value = c(mean, variance, skew, kurt))
two.and.five.dist <- tibble(x = seq(-0.25, 1.25, length.out=1000)) %>%
  mutate(beta.pdf = dbeta(x, alpha, beta), norm.pdf = dnorm(x, mean = alpha/(alpha+beta), sd = sqrt((alpha*beta)/((alpha+beta)^2*(alpha+beta+1))))) 
ggplot(data= two.and.five.dist) +
  geom_line(aes(x=x, y=beta.pdf, color="Beta(2,5)")) + geom_line(aes(x=x, y=norm.pdf, color="Gaussian(0.2857, 0.0255)")) + geom_hline(yintercept=0) +
  theme_bw() +
  xlab("x") +
  ylab("Density") +
  scale_color_manual("", values = c("black", "grey")) + theme(legend.position = "bottom")

alpha<-5 
mean <- alpha / (alpha + beta)
variance <- (alpha * beta) / ((alpha + beta)^2 * (alpha + beta + 1))
skew <- (2 * (beta - alpha) * sqrt(alpha + beta + 1)) / ((alpha + beta + 2) * sqrt(alpha * beta))
kurt <- (6 * ((alpha - beta)^2 * (alpha + beta + 1) - alpha * beta * (alpha + beta + 2))) / (alpha * beta * (alpha + beta + 2) * (alpha + beta + 3))
five.and.five.df <- data.frame(Statistic = c("Mean", "Variance", "Skewness", "Kurtosis"), Value = c(mean, variance, skew, kurt))
five.and.five.dist <- tibble(x = seq(-0.25, 1.25, length.out=1000)) %>%
  mutate(beta.pdf = dbeta(x, alpha, beta), norm.pdf = dnorm(x, mean = alpha/(alpha+beta), sd = sqrt((alpha*beta)/((alpha+beta)^2*(alpha+beta+1))))) 
ggplot(data= five.and.five.dist) +
  geom_line(aes(x=x, y=beta.pdf, color="Beta(2,5)")) + geom_line(aes(x=x, y=norm.pdf, color="Gaussian(0.2857, 0.0255)")) + geom_hline(yintercept=0) +
  theme_bw() +
  xlab("x") +
  ylab("Density") +
  scale_color_manual("", values = c("black", "grey")) + theme(legend.position = "bottom")

beta<- 2
mean <- alpha / (alpha + beta)
variance <- (alpha * beta) / ((alpha + beta)^2 * (alpha + beta + 1))
skew <- (2 * (beta - alpha) * sqrt(alpha + beta + 1)) / ((alpha + beta + 2) * sqrt(alpha * beta))
kurt <- (6 * ((alpha - beta)^2 * (alpha + beta + 1) - alpha * beta * (alpha + beta + 2))) / (alpha * beta * (alpha + beta + 2) * (alpha + beta + 3))
five.and.two.df <- data.frame(Statistic = c("Mean", "Variance", "Skewness", "Kurtosis"), Value = c(mean, variance, skew, kurt))
five.and.two.dist <- tibble(x = seq(-0.25, 1.25, length.out=1000)) %>%
  mutate(beta.pdf = dbeta(x, alpha, beta), norm.pdf = dnorm(x, mean = alpha/(alpha+beta), sd = sqrt((alpha*beta)/((alpha+beta)^2*(alpha+beta+1))))) 
ggplot(data= five.and.two.dist) +
  geom_line(aes(x=x, y=beta.pdf, color="Beta(2,5)")) + geom_line(aes(x=x, y=norm.pdf, color="Gaussian(0.2857, 0.0255)")) + geom_hline(yintercept=0) +
  theme_bw() +
  xlab("x") +
  ylab("Density") +
  scale_color_manual("", values = c("black", "grey")) + theme(legend.position = "bottom")

alpha <- 0.5
beta <- 0.5
mean <- alpha / (alpha + beta)
variance <- (alpha * beta) / ((alpha + beta)^2 * (alpha + beta + 1))
skew <- (2 * (beta - alpha) * sqrt(alpha + beta + 1)) / ((alpha + beta + 2) * sqrt(alpha * beta))
kurt <- (6 * ((alpha - beta)^2 * (alpha + beta + 1) - alpha * beta * (alpha + beta + 2))) / (alpha * beta * (alpha + beta + 2) * (alpha + beta + 3))
half.and.half.df <- data.frame(Statistic = c("Mean", "Variance", "Skewness", "Kurtosis"), Value = c(mean, variance, skew, kurt))
half.and.half.dist <- tibble(x = seq(-0.25, 1.25, length.out=1000)) %>%
  mutate(beta.pdf = dbeta(x, alpha, beta), norm.pdf = dnorm(x, mean = alpha/(alpha+beta), sd = sqrt((alpha*beta)/((alpha+beta)^2*(alpha+beta+1))))) 
ggplot(data= half.and.half.dist) +
  geom_line(aes(x=x, y=beta.pdf, color="Beta(2,5)")) + geom_line(aes(x=x, y=norm.pdf, color="Gaussian(0.2857, 0.0255)")) + geom_hline(yintercept=0) +
  theme_bw() +
  xlab("x") +
  ylab("Density") +
  scale_color_manual("", values = c("black", "grey")) + theme(legend.position = "bottom")



########################################################################################################################################################################################################################################### 
#### Task 2 ####
beta.moment <- function(alpha,beta,k,centered){
  if(centered == FALSE){
     uncentered.moment <- integrate(function(x){(x^k)*dbeta(x, alpha, beta)},lower=0,upper=1)
     return(uncentered.moment$value)
  }else{
    mu <-integrate(function(x){(x)*dbeta(x, alpha, beta)},lower=0,upper=1)$value
    centered.moment <-integrate(function(x){((x-mu)^k)*dbeta(x, alpha, beta)},lower=0,upper=1)
    return(centered.moment$value)
  }
}

alpha <- 2
beta <- 5
# uncentered
MEAN = beta.moment(alpha,beta,1,FALSE)
#centered
VAR = beta.moment(alpha,beta,2,TRUE)
SKEW = beta.moment(alpha,beta,3,TRUE)/ ((beta.moment(alpha,beta,2,TRUE)^(2/3)))
E.KURT = (beta.moment(alpha,beta,4,TRUE) /((beta.moment(alpha,beta,2,TRUE)^2)))-3



############################################################################################################################################################################################ 
######################################################## Task 3 ######################################################################################################################################
n <- 500
set.seed(7272)

############################################### alpha is 2 and beta is 5 sample ###############################################
alpha <- 2
beta <- 5

# Generate beta sample
sample_data <- rbeta(n, alpha, beta)

# Create data frame
df <- data.frame(x = sample_data)

# Plot histogram with density estimates
p <- ggplot(df, aes(x = x)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "green", color = "black", alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  stat_function(fun = dbeta, args = list(shape1 = alpha, shape2 = beta), color = "darkblue", size = 1) +
  ggtitle("Beta Distribution Sample, alpha=2, beta =5") +
  theme_minimal()

print(p)

# Numerical summary
summary_stats <- df |>
  summarize(
    Mean = mean(x),
    Variance = var(x),
    Skewness = beta.moment(alpha,beta,3,TRUE)/ ((beta.moment(alpha,beta,2,TRUE)^(2/3))),
    Excess_Kurtosis = (beta.moment(alpha,beta,4,TRUE) /((beta.moment(alpha,beta,2,TRUE)^2)))-3
  )

print(summary_stats)

############################################### alpha is 5 and beta is 2 sample ###############################################
alpha <- 5
beta <- 2

# Generate beta sample
sample_data <- rbeta(n, alpha, beta)

# Create data frame
df <- data.frame(x = sample_data)

# Plot histogram with density estimates
p <- ggplot(df, aes(x = x)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "green", color = "black", alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  stat_function(fun = dbeta, args = list(shape1 = alpha, shape2 = beta), color = "darkblue", size = 1) +
  ggtitle("Beta Distribution Sample (alpha=5, beta=2") +
  theme_minimal()

print(p)

# Numerical summary
summary_stats <- df |>
  summarize(
    Mean = mean(x),
    Variance = var(x),
    Skewness = beta.moment(alpha,beta,3,TRUE)/ ((beta.moment(alpha,beta,2,TRUE)^(2/3))),
    Excess_Kurtosis = (beta.moment(alpha,beta,4,TRUE) /((beta.moment(alpha,beta,2,TRUE)^2)))-3
  )

print(summary_stats)

############################################### alpha is 5 and beta is 5 sample ###############################################
alpha <- 5
beta <- 5

# Generate beta sample
sample_data <- rbeta(n, alpha, beta)

# Create data frame
df <- data.frame(x = sample_data)

# Plot histogram with density estimates
p <- ggplot(df, aes(x = x)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "green", color = "black", alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  stat_function(fun = dbeta, args = list(shape1 = alpha, shape2 = beta), color = "darkblue", size = 1) +
  ggtitle("Beta Distribution Sample where alpha and beta are both 5") +
  theme_minimal()

print(p)

# Numerical summary
summary_stats <- df |>
  summarize(
    Mean = mean(x),
    Variance = var(x),
    Skewness = beta.moment(alpha,beta,3,TRUE)/ ((beta.moment(alpha,beta,2,TRUE)^(2/3))),
    Excess_Kurtosis = (beta.moment(alpha,beta,4,TRUE) /((beta.moment(alpha,beta,2,TRUE)^2)))-3
  )

print(summary_stats)

############################################### alpha is 1/2 and beta is 1/2 sample ###############################################
alpha <- 0.5
beta <- 0.5

# Generate beta sample
sample_data <- rbeta(n, alpha, beta)

# Create data frame
df <- data.frame(x = sample_data)

# Plot histogram with density estimates
p <- ggplot(df, aes(x = x)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "green", color = "black", alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  stat_function(fun = dbeta, args = list(shape1 = alpha, shape2 = beta), color = "darkblue", size = 1) +
  ggtitle("Beta Distribution Sample where alpha and beta are both 1/2") +
  theme_minimal()

print(p)

# Numerical summary
summary_stats <- df |>
  summarize(
    Mean = mean(x),
    Variance = var(x),
    Skewness = beta.moment(alpha,beta,3,TRUE)/ ((beta.moment(alpha,beta,2,TRUE)^(2/3))),
    Excess_Kurtosis = (beta.moment(alpha,beta,4,TRUE) /((beta.moment(alpha,beta,2,TRUE)^2)))-3
  )

print(summary_stats)






