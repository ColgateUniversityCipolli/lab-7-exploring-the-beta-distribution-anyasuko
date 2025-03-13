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

(alpha.hat.mom <- moms$x[1])
(beta.hat.mom <- moms$x[2])


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

(alpha.hat.mle <- mles$par[1])
(beta.hat.mle <- mles$par[2])


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