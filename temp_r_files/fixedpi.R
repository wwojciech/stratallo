#' @title Optimal univariate allocation under box constraints for stratified sampling
#' @description Algorithm for optimal allocation in stratified sampling with lower and upper 
#' constraints based on fixed point iteration
#'
#' @param n -  target sample size for allocation
#' @param Nh - population sizes in strata
#' @param Sh - standard deviations for given variable in strata
#' @param mh - lower constraints for sample sizes in strata
#' @param Mh - upper constraints for sample sizes in strata
#' @param lambda0 - starting value of parameter 'lambda'
#' @param maxiter - maximal nunber of iterations
#'
#' @return  vector of optimal allocation sizes,
#'  and number of iterations 
#'
#' @references Ralf T. Munnich, Ekkehard W. Sachs, Matthias Wagner (2012),
#' Numerical solution of optimal allocation problems in stratified sampling under box 
#'  constraints, AStA Adv Stat Anal (2012) 96:435?450, DOI 10.1007/s10182-011-0176-z
#'
#' @export

fixedpi <- function(n, Nh, Sh, mh=NULL, Mh=NULL, lambda0=NULL, maxiter=100)
{
  H <- length(Nh)
  dh <- Sh*Nh
  
  L <- NULL
  U <- NULL
  nh <- n*dh/sum(dh) # Neyman solution
  
  if (is.null(lambda0)) 
  {
    # (lambda<-(sum(dh)^2)/(n^2)) # from paper MSW
    # starting interval for bisection - according to idea JW
    r <- c(dh/mh,dh/Mh)
    a <- min(r)^2
    b <- max(r)^2
    lambda <- uniroot(function(x) glambda(x,n,Nh,Sh,mh,Mh),lower = a, upper=b, 
                      maxiter = maxiter)$root
    #lambda <- uniroot(function(x) glambda(x,n,Nh,Sh,mh,Mh),lower = a, upper=b)$root
    #lambda <- (a+b)/2  # the simplest variant for bisection
  }
  else lambda <- lambda0
  
  
  iter <- 0
  while (1) {
    iter <- iter+1
    L<-which((dh^2)/(mh^2) <= lambda)
    U<-which((dh^2)/(Mh^2) >= lambda)
    Hc<-setdiff(1:H,union(L,U))
    
    if (n-sum(mh[L])-sum(Mh[U])==0) break # rozwiazanie brzegowe
      
    lambda_n <- (sum(dh[Hc])/(n-sum(mh[L])-sum(Mh[U])))^2
    if ( abs(lambda_n-lambda)<.Machine$double.eps*1000 ) break
    #if ( abs(lambda_n-lambda)<1e-6 ) break
    lambda <- lambda_n
    #cat("iteracja ",iter," lambda ",lambda,"\n")
    
  }
  
  nh <- dh/sqrt(lambda)
  nh[L]<-mh[L]
  nh[U]<-Mh[U]
  
  #v <- sum(Nh * (Nh - nh) * Sh^2 / nh)
  
  #cat("Number of iterations = ",iter,"\n")
  
  return(list(nh = nh, iter = iter))
  
}


# auxiliary functions

glambda<-function(lambda,n,Nh, Sh, mh=NULL, Mh=NULL)
{
  H <- length(Nh)
  dh <- Sh*Nh
  
  L<-which((dh^2)/(mh^2) <= lambda)
  U<-which((dh^2)/(Mh^2) >= lambda)
  Hc<-setdiff(1:H,union(L,U))

  nh <- dh/sqrt(lambda)
  nh[L]<-mh[L]
  nh[U]<-Mh[U]

return(sum(nh)-n)

}



philambda<-function(lambda,n,Nh, Sh, mh=NULL, Mh=NULL)
{
  H <- length(Nh)
  dh <- Sh*Nh
  
  L<-which((dh^2)/(mh^2) <= lambda)
  U<-which((dh^2)/(Mh^2) >= lambda)
  Hc<-setdiff(1:H,union(L,U))
  
  #cat("L: ",L," U: ",U,"\n")
  
  lambda_n <- (sum(dh[Hc])/(n-sum(mh[L])-sum(Mh[U])))^2

  return(lambda_n)
  
}



