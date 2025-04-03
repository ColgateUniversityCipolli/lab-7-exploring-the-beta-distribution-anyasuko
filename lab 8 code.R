library(tidyverse)

####################################################### Task 6 ###########################################################################################################
dat.death.rates <- read_csv("API_SP.DYN.CDRT.IN_DS2_en_csv_v2_76451.csv")

dat.death.rates <- dat.death.rates |>
  select("Country Name", "2022")

dat.death.rates <- dat.death.rates |>
  mutate("DeathRates"= `2022`/1000)


# view(dat.death.rates)


###################################################### Task 7 ##################################################################################################################
install.packages("nleqslv")
library(nleqslv)

###################
# MOM
###################
MOM.beta <- function(data, par){
  alpha <- par[1]
  beta <- par[2]
  
  EX1 <- alpha/(alpha+beta)
  EX2 <- alpha*(alpha+1)/((alpha+beta+1)*(alpha+beta))
  
  m1 <- mean(data, na.rm=TRUE)
  m2 <- mean(data^2, na.rm=TRUE)
  
  return( c(EX1 - m1, EX2 - m2) )
}

(moms<- nleqslv(x = c(1, 1),
                fn = MOM.beta,
                data=dat.death.rates$DeathRates))

print(alpha.hat.mom <- moms$x[1])
print(beta.hat.mom <- moms$x[2])


###################
# MLE
###################
llbeta <- function(data, par, neg=F){
  alpha <- par[1]
  beta <- par[2]
  
  loglik <- sum(log(dbeta(x=data, shape1=alpha, shape2=beta)), na.rm= TRUE)
  
  return(ifelse(neg, -loglik, loglik))
}

(mles <- optim(par = c(1,1),
               fn = llbeta,
               data=dat.death.rates$DeathRates,
               neg=T))

print(alpha.hat.mle <- mles$par[1])
print(beta.hat.mle <- mles$par[2])


###################
# Histogram of Data with distributions super-imposed
###################

ggdat.death.rates <- tibble(x=seq(0,.0225, length.out=1000)) |>
  mutate(pdf = dbeta(x=x, shape1=alpha.hat.mle, shape2=beta.hat.mle),
         pdf2 = dbeta(x=x, shape1=alpha.hat.mom, shape2=beta.hat.mom))


ggplot()+
  geom_histogram(data = dat.death.rates,
                 aes(x = DeathRates,
                     y = after_stat(density)),
                 breaks = seq(0, 0.0225, .002),
                 color = "black",
                 fill = "lightblue")+
  geom_line(data=ggdat.death.rates,
             aes(x=x,y=pdf),
             color = "blue")+
  geom_line(data=ggdat.death.rates,
             aes(x=x, y=pdf2),
             color="red")



###################################################### Task 8 ##################################################################################################################

alpha <- 8
beta <- 950

#make place to store data to
mom.estimates <- tibble(alpha=numeric(1000), beta=numeric(1000))
mle.estimates <- tibble(alpha=numeric(1000), beta=numeric(1000))

#for loop to generate 1000 estimates
for (i in 1:1000){
  set.seed(7272+i)
  
  # make data set for a beta dist
  data.loop <- rbeta(266, alpha, beta)
  
  # MOM estimation
  MOM.beta <- function(data, par){
    alpha <- par[1]
    beta <- par[2]
    
    EX1 <- alpha/(alpha+beta)
    EX2 <- alpha*(alpha+1)/((alpha+beta+1)*(alpha+beta))
    
    m1 <- mean(data, na.rm=TRUE)
    m2 <- mean(data^2, na.rm=TRUE)
    
    return( c(EX1 - m1, EX2 - m2) )
  }
  
  moms<- nleqslv(x = c(1, 1),
                  fn = MOM.beta,
                  data=data.loop)
  
  # MLE estimation
  llbeta <- function(data, par, neg=F){
    alpha <- par[1]
    beta <- par[2]
    
    loglik <- sum(log(dbeta(x=data, shape1=alpha, shape2=beta)), na.rm= TRUE)
    
    return(ifelse(neg, -loglik, loglik))
  }
  
  mles <- optim(par = c(1,1),
                 fn = llbeta,
                 data = data.loop,
                 neg=T)
 
  #add estimate values to tibble
  mom.estimates$alpha[i] <- moms$x[1]
  mom.estimates$beta[i] <- moms$x[2]
  
  mle.estimates$alpha[i] <-mles$par[1]
  mle.estimates$beta[i] <- mles$par[2]
}
 
view(mom.estimates)
view(mle.estimates)



#plot the densities for both
mom.alpha.plot <- ggplot(mom.estimates, aes(x = alpha)) +
  geom_density(fill = "darkblue", alpha = 0.5) +
  labs(title = "MOM Alpha Estimates", x = "Alpha", y = "Density")
(mom.alpha.plot)

mom.beta.plot <- ggplot(mom.estimates, aes(x = beta)) +
  geom_density(fill = "lightblue", alpha = 0.5) +
  labs(title = "MOM Beta Estimates", x = "Beta", y = "Density")
(mom.beta.plot)

mle.alpha.plot <- ggplot(mle.estimates, aes(x=alpha)) +
  geom_density(fill= "lightgreen", alpha = 0.5)+
  labs(title = "MLE Alpha Estimates", x= "Alpha", y = "Density")
(mle.alpha.plot)

mle.beta.plot <- ggplot(mle.estimates, aes(x=beta))+
  geom_density(fill = "darkgreen", alpha = 0.5)+
  labs(title = "MLE Beta Estimates", x = "Beta", y = "Density")
(mle.beta.plot)


# Compute the bias, precision, and mean squared error for the estimates. Report them in a table.
  
# MOM alpha
mom.alpha.theta.hats <- mom.estimates$alpha
mom.alpha.bias <- mean(mom.alpha.theta.hats) - alpha
mom.alpha.precision <- 1/var(mom.alpha.theta.hats)
mom.alpha.mse <- var(mom.alpha.theta.hats) + bias^2
(bias)
(precision)
(mse)

# MOM beta
mom.beta.theta.hats <- mom.estimates$beta
mom.beta.bias <- mean(mom.beta.theta.hats) - beta
mom.beta.precision <- 1/var(mom.beta.theta.hats)
mom.beta.mse <- var(mom.beta.theta.hats) + bias^2
(bias)
(precision)
(mse)

# MLE alpha
mle.alpha.theta.hats <- mle.estimates$alpha
mle.alpha.bias <- mean(mle.alpha.theta.hats) - alpha
mle.alpha.precision <- 1/var(mle.alpha.theta.hats)
mle.alpha.mse <- var(mle.alpha.theta.hats) + bias^2
(bias)
(precision)
(mse)

# MLE beta
mle.beta.theta.hats <- mle.estimates$beta
mle.beta.bias <- mean(mle.beta.theta.hats) - beta
mle.beta.precision <- 1/var(mle.beta.theta.hats)
mle.beta.mse <- var(mle.beta.theta.hats) + bias^2
(bias)
(precision)
(mse)

# ***put into a table in write up with xtable***
summary.table <- tibble(
  Method = c("MOM", "MOM", "MLE", "MLE"),
  Parameter = c("Alpha", "Beta", "Alpha", "Beta"),
  Bias = c(mom.alpha.bias, mom.beta.bias, mle.alpha.bias, mle.beta.bias),
  Precision = c(mom.alpha.precision, mom.beta.precision, mle.alpha.precision, mle.beta.precision),
  MSE = c(mom.alpha.mse, mom.beta.mse, mle.alpha.mse, mle.beta.mse)
)

print(xtable(summary.table))









