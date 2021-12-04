
#' @title Generation of data for experiments with allocation algorithms
#' @description Artificial population is generated with many strata for 
#'   experiments with optimal allocation algorithms in stratified sampling.
#'   Simulated population is used for computing parameters needed for allocation
#'   algorithms.
#'
#' @param Nrep - number of iterations for sequential generation of population
#'
#' @return list with parameters of generated population i.e.
#'  Nh - vector of population sizes in strata
#'  Sh - vector of population standard deviations in strata
#'
#'
#' @examples 
#'  pop <- gen_population(Nrep=20)
#'  Nh<-pop$Nh
#'  Sh<-pop$Sh
#   NROW(Nh) # number of generated strata
#' 
#' @export
#'


gen_population <- function(Nrep=10) 
{
  
 require(dplyr)
 require(stratification)
 d<-NULL

 Ls <- 10 # number of strata in single generation step 

 for (i in 1:Nrep) {
   sigma <- log(i+1)
   cat("iteration ",i," sigma ",sigma,"\n")
   di <- data.frame(x=exp(rnorm(10000, sd=sigma)))
   #di <- data.frame(x= sqrt(abs(rcauchy(10000, scale=sigma))))
   ##di <- data.frame(x=abs(rnorm(10000, sd=1)))
   
   bh<-strata.geo(x=di$x, CV=0.05, Ls=Ls)$bh
   ##bh<-strata.cumrootf(x=di$x, CV=0.05, Ls=Ls)$bh
   di$h<-(i-1)*Ls + as.numeric(cut(di$x,breaks = c(0,bh,Inf)))
   d<-bind_rows(d,di)
 }
 d$h<-as.factor(d$h)
 
 pop<-group_by(d,h) %>% summarise(Sh=sqrt(var(x)),Nh=n()) %>% na.omit()
 
 # additional random shuffling of data
 s <- sample(1:nrow(pop),nrow(pop))
 pop <- pop[s,]

 return(list(Nh=pop$Nh,Sh=pop$Sh))

}


