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


############################################################################################################################################################################################ 
######################################################## Task 4 ######################################################################################################################################
install.packages("cumstats")
library(cumstats)

install.packages("patchwork")
library(patchwork)



set.seed(7272)

initial_sample <- rbeta(1000, 2, 5)

cumulative_stats <- tibble(
  n = 1:1000,
  Mean = cummean(initial_sample),
  Variance = cumvar(initial_sample),
  Skewness = cumskew(initial_sample),
  Kurtosis = cumkurt(initial_sample)
) %>%
  pivot_longer(-n, names_to = "Statistic", values_to = "Value")

true_values <- tibble(
  Statistic = c("Mean", "Variance", "Skewness", "Kurtosis"),
  TrueValue = c(2/7, (2*5)/((2+5)^2 * (2+5+1)), 
                (2*(5-2)*sqrt(2+5+1)) / ((2+5+2) * sqrt(2*5)),
                (6 * ((2-5)^2 * (2+5+1) - 2*5*(2+5+2))) / (2*5*(2+5+2)*(2+5+3)))
)

p <- ggplot(cumulative_stats, aes(x = n, y = Value)) +
  geom_line() +
  geom_hline(data = true_values, aes(yintercept = TrueValue), linetype = "dashed", color = "red") +
  facet_wrap(~Statistic, scales = "free") +
  theme_minimal()

for (i in 2:50) {
  set.seed(7272 + i)
  new_sample <- rbeta(1000, 2, 5)
  
  new_stats <- tibble(
    n = 1:1000,
    Mean = cummean(new_sample),
    Variance = cumvar(new_sample),
    Skewness = cumskew(new_sample),
    Kurtosis = cumkurt(new_sample)
  ) %>%
    pivot_longer(-n, names_to = "Statistic", values_to = "Value")
  
  p <- p + geom_line(data = new_stats, aes(x = n, y = Value), color = i)
}

print(p)



############################################################################################################################################################################################ 
######################################################## Task 5 ######################################################################################################################################

alpha <- 2
beta <- 5
n <- 500
iterations <- 1000

mean_values <- numeric(iterations)
variance_values <- numeric(iterations)
skewness_values <- numeric(iterations)
excess_kurtosis_values <- numeric(iterations)

set.seed(7272)

for (i in 1:iterations) {
  set.seed(7272 + i)
  
  sample_data <- rbeta(n, alpha, beta)
  
  mean_values[i] <- mean(sample_data)
  variance_values[i] <- var(sample_data)
  skewness_values[i] <- skewness(sample_data)
  excess_kurtosis_values[i] <- kurtosis(sample_data) - 3  # Excess Kurtosis = kurtosis - 3
}

results_df <- data.frame(
  Mean = mean_values,
  Variance = variance_values,
  Skewness = skewness_values,
  Excess_Kurtosis = excess_kurtosis_values
)

summary_stats <- results_df %>%
  summarise(
    Mean_of_Means = mean(Mean),
    Var_of_Means = var(Mean),
    Mean_of_Variance = mean(Variance),
    Var_of_Variance = var(Variance),
    Mean_of_Skewness = mean(Skewness),
    Var_of_Skewness = var(Skewness),
    Mean_of_Excess_Kurtosis = mean(Excess_Kurtosis),
    Var_of_Excess_Kurtosis = var(Excess_Kurtosis)
  )

print(summary_stats)

# Mean Distribution
p_mean <- ggplot(results_df, aes(x = Mean)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "green", color = "black", alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  ggtitle("Sampling Distribution of the Mean") +
  theme_minimal()

# Variance Distribution
p_variance <- ggplot(results_df, aes(x = Variance)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "blue", color = "black", alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  ggtitle("Sampling Distribution of the Variance") +
  theme_minimal()

# Skewness Distribution
p_skewness <- ggplot(results_df, aes(x = Skewness)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "purple", color = "black", alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  ggtitle("Sampling Distribution of the Skewness") +
  theme_minimal()

# Excess Kurtosis Distribution
p_kurtosis <- ggplot(results_df, aes(x = Excess_Kurtosis)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "orange", color = "black", alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  ggtitle("Sampling Distribution of Excess Kurtosis") +
  theme_minimal()

(p_mean + p_variance + p_skewness + p_kurtosis) + plot_layout(ncol = 2)






