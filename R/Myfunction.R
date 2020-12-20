#' @title K
#' @description helper function
#' @param u the argument(dim=1)
#' @param h the bandwidth
#' @return The function value
#' @export
K <- function(u,h){###Gaussian kernel function
  K_value <- exp(-(u/h)^2/2)/(h*sqrt(2*pi))
  return(K_value)
}

#' @title Tstat
#' @description helper function
#' @param X the first sample(dim=1)
#' @param Y the second sample(dim=1)
#' @param h the bandwidth
#' @return The test statistic
#' @export
Ts <- function(X,Y,h){#Test statistics
  len_X <- length(X)
  len_Y <- length(Y)
  C <- K(0,1)*(len_X+len_Y)/(h*len_X*len_Y)
  I1 <- 0
  I2 <- 0
  I3 <- 0
  for (i in 1:len_X){
    for (j in 1:len_X){
      K1 <- K(X[i]-X[j],h)/(len_X*len_X)
      I1 <- I1 + K1
    }
  }
  for (i in 1:len_Y){
    for (j in 1:len_Y){
      K2 <- K(Y[i]-Y[j],h)/(len_Y*len_Y)
      I2 <- I2 + K2
    }
  }
  for (i in 1:len_X){
    for (j in 1:len_Y){
      K3 <- -2*K(X[i]-Y[j],h)/(len_X*len_Y)
      I3 <- I3 + K3
    }
  }
  I <- I1 + I2 + I3
  sigma2_1 <- 0
  sigma2_2 <- 0
  sigma2_3 <- 0
  for (i in 1:len_X){
    for (j in 1:len_X){
      s1 <- (K(X[i]-X[j],h))^2/(len_X*len_X)
      sigma2_1 <- sigma2_1 + s1
    }
  }
  for (i in 1:len_Y){
    for (j in 1:len_Y){
      s2 <- (K(Y[i]-Y[j],h))^2/(len_Y*len_Y)
      sigma2_2 <- sigma2_2 + s2
    }
  }
  for (i in 1:len_X){
    for (j in 1:len_Y){
      s3 <- 2*(K(X[i]-Y[j],h))^2/(len_X*len_Y)
      sigma2_3 <- sigma2_3 + s3
    }
  }
  sigma2<- h*(sigma2_1+sigma2_2+sigma2_3)
  sigma_hat <- sqrt(sigma2)
  result <-sqrt(len_X*len_Y*h)*(I-C)/sigma_hat
  return(result)
}

#' @title Density Test
#' @description Test two samples have the same density or not
#' @param X the first sample(dim=1)
#' @param Y the second sample(dim=1)
#' @param alpha the confidence coefficient
#' @return The list of result(1 stand for equal,0 stand for inequal),Tstatistic,Standard
#' @examples
#' \dontrun{
#'  n1 <- 30
#'  n2 <- 40
#'  X1 <- rnorm(n1,0,1)
#'  X2 <- rnorm(n2,0,1)
#'  density_test(X1,X2,0.05)
#' }
#' @export
density_test <- function(X,Y,alpha){
  len_x <- length(X)
  len_y <- length(Y)
  S <- c(X,Y)
  sd_hat <- sd(S)
  bw <- sd_hat*1.06*(len_y+len_x)^(-0.2)
  T1 <- Ts(X,Y,bw)
  T0 <- qnorm(1-alpha,0,1)
  return_value <- list(result=T0>T1,Tstatistic=T1,Standard=T0)
  return(return_value)
}

#' @title Density Test
#' @description Test two samples have the same density or not
#' @param X the first sample(dim=1)
#' @param Y the second sample(dim=1)
#' @param alpha the confidence coefficient
#' @param bw  the bandwidth,only use in bw_density_test
#' @return The list of result(1 stand for equal,0 stand for inequal),Tstatistic,Standard
#' @examples
#' \dontrun{
#'  n1 <- 30
#'  n2 <- 40
#'  X1 <- rnorm(n1,0,1)
#'  X2 <- rnorm(n2,0,1)
#'  bw <- 0.49
#'  bw_density_test(X1,X2,0.05,0.49)
#' }
#' @export
bw_density_test <- function(X,Y,alpha,bw){## User can choice the bandwidth by himself
  T1 <- Ts(X,Y,bw)
  T0 <- qnorm(1-alpha,0,1)
  return_value <- list(result=T0>T1,Tstatistic=T1,Standard=T0)
  return(return_value)
}


#' @title f1
#' @description helper function
#' @param X the first sample(dim=1)
#' @param h the bandwidth
#' @param m the number of the leave-out
#' @return The function value
#' @export
f1 <- function(X,h,m){
  re <- 0
  lX <-length(X)
  for (i in 1:lX) {
    re <- re + K(X[i]-X[m],h)
  }
  re <- re - K(0,h)
  re <- re/(lX-1)
  return(re)
}

#' @title f2
#' @description helper function
#' @param X the first sample(dim=1)
#' @param Y the second sample(dim=1)
#' @param h1 the bandwidth of X
#' @param h2 the bandwidth of Y
#' @param m the number of the leave-out
#' @return The function value
#' @export
f2 <- function(X,Y,h1,h2,m){
  res <- 0
  lX <-length(X)
  for (i in 1:lX) {
    res <- res + K(X[i]-X[m],h1)*K(Y[i]-Y[m],h2)
  }
  res <- res - K(0,h1)*K(0,h2)
  res <- res/(lX-1)
  return(res)
}

#' @title T-stat
#' @description helper function
#' @param X the first sample(dim=1)
#' @param Y the second sample(dim=1)
#' @param h1 the bandwidth of X
#' @param h2 the bandwidth of Y
#' @return T-statistic
#' @export
Ts2 <- function(X,Y,h1,h2){#Test statistics
  len_X <- length(X)
  len_X <- length(X)
  I1 <- 0
  I2 <- 0
  I3 <- 0
  for(i in 1:len_X){
    I1 <- I1 + f2(X,Y,h1,h2,i)
    I3 <- I3 + f1(X,h1,i)*f1(Y,h2,i)
    for (j in 1:len_X){
      I2 <- I2 + f1(X,h1,i)*f1(Y,h2,j)
    }
  }
  I1 <- I1/len_X
  I2 <- I2/(len_X)^2
  I3 <- -2*I3/len_X
  I_hat <- I1 + I2 + I3
  len_X <- length(X)
  s1 <- 0
  s2 <- 0
  for (i in 1:len_X){
    s1 <- s1 + (K(0,h1)*K(0,h2))^2
    for(j in 1:len_X){
      s2 <- s2+ (K(X[j]-X[i],h1)*K(Y[j]-Y[i],h2))^2
    }
  }
  s_hat <- s2-s1
  s_hat <- 2*s_hat/(len_X^2-len_X)
  sigma_hat <- sqrt(s_hat)
  result <- len_X*sqrt(h1*h2)*I_hat/sigma_hat
  return(result)
}


#' @title Independence Test
#' @description Test two samples(the same sample size) are indenpendent or not
#' @param X the first sample(dim=1)
#' @param Y the second sample(the same sample size with X,dim=1)
#' @param alpha the confidence coefficient
#' @return The list of result(independent,dependent,the different sample size),T-statistic,Standard
#' @examples
#' \dontrun{
#'  n1 <- 30
#'  n2 <- 40
#'  X1 <- rnorm(n1,0,1)
#'  X2 <- rnorm(n2,0,1)
#'  Independence_test(X1,X2,0.05)
#' }
#' @export
Independence_test <- function(X,Y,alpha){
  n <- length(X)
  r <- numeric(3)
  r[2] <- NA
  r[3] <- NA
  if(n==length(Y)){
    h1 <- 1.06*sd(X)*n^(-0.2)##suitable bandwith
    h2 <- 1.06*sd(Y)*n^(-0.2)
    r[2] <- qnorm(1-alpha,0,1)
    r[3] <- Ts2(X,Y,h1,h2)
    if(r[3]>r[2]){
      r1 <- "Dependence"
    }
    else{
      r1 <- "Independence"
    }
  }
  else{
    r1 <- "different length"
  }
  return_value <- list(result=r1,Tstatistic=r[3],Standard=r[2])
  return(return_value)
}

#' @title Independence Test
#' @description Test two samples(the same sample size) are indenpendent or not
#' @param X the first sample(dim=1)
#' @param Y the second sample(the same sample size with X,dim=1)
#' @param alpha the confidence coefficient
#' @param bw1  the bandwidth of X,only use in bw_Independence_test
#' @param bw2  the bandwidth of Y,only use in bw_Independence_test
#' @return The list of result(independent,dependent,the different sample size),T-statistic,Standard
#' @examples
#' \dontrun{
#'  n1 <- 30
#'  X1 <- rnorm(n1,0,1)
#'  X4 <- X1^2
#'  h1 <- 0.687
#'  h4 <- 1.262
#'  bw_Independence_test(X1,X4,0.05,h1,h4)
#' }
#' @export
bw_Independence_test <- function(X,Y,alpha,bw1,bw2){## User can choice the bandwidth by himself
  n <- length(X)
  r <- numeric(3)
  r[2] <- NA
  r[3] <- NA
  if(n==length(Y)){
    r[2] <- qnorm(1-alpha,0,1)
    r[3] <- Ts2(X,Y,bw1,bw2)
    if(r[3]>r[2]){
      r1 <- "Dependence"
    }
    else{
      r1 <- "Independence"
    }
  }
  else{
    r1 <- "different length"
  }
  return_value <- list(result=r1,Tstatistic=r[3],Standard=r[2])
  return(return_value)
}


#' @title Sort_bubble
#' @description From the blog(https://www.cnblogs.com/onepixel/articles/7674659.html)
#' @param X the array needed to sort
#' @return the array has been sorted
#' @examples
#' \dontrun{
#' X <- sample(1:50,size=20,replace=T)
#' X
#' Sort_bubble(X)
#' }
#' @export
Sort_bubble <- function(X){
Y <- X
n <- length(Y)
if(n==1) return(Y)
for(i in 1:(n-1)){
  for (j in 1:i){
    if(Y[j+1]<Y[j]){
      temp <- Y[j+1]
      Y[j+1] <- Y[j]
      Y[j] <- temp
    }
  }
}
return(Y)
}

#' @title Sort_selection
#' @description From the blog(https://www.cnblogs.com/onepixel/articles/7674659.html)
#' @param X the array needed to sort
#' @return the array has been sorted
#' @examples
#' \dontrun{
#' X <- sample(1:50,size=20,replace=T)
#' X
#' Sort_selection(X)
#' }
#' @export
Sort_selection <- function(X){
  Y <- X
  n <- length(Y)
  if(n==1) return(Y)
  for(i in 1:(n-1)){
    Index <- i
    for (j in (i+1):n){
      if (Y[Index] > Y[j]){
        Index <- j
      }
    }
    temp <- Y[i]
    Y[i] <- Y[Index]
    Y[Index] <- temp
  }
  return(Y)
}

#' @title Sort_merge
#' @description From the blog(https://www.cnblogs.com/onepixel/articles/7674659.html)
#' @param X the array needed to sort
#' @return the array has been sorted
#' @examples
#' \dontrun{
#' X <- sample(1:50,size=20,replace=T)
#' X
#' Sort_merge(X)
#' }
#' @export
Sort_merge <- function(X){
  Y <- X
  n <- length(Y)
  Z <- numeric(n)
  if(n == 1){return(Y)}
  else if (n == 2){
    if(Y[1]>Y[2]){
      temp <- Y[1]
      Y[1] <- Y[2]
      Y[2] <- temp
    }
    return(Y)
  }
  else{
    m <- n%/%2
    Y1 <- Sort_merge(Y[1:m])
    Y2 <- Sort_merge(Y[(m+1):n])
    i <- 1
    j <- 1
    for(k in 1:n){
      if (i + j == n+1){
        if(Y1[m]<Y2[n-m]) Z[n] <- Y2[n-m]
        else Z[n] <- Y1[m]
      }
      else if(i == m+1){
        Z[k] <- Y2[j]
        j <- j+1
      }
      else if(j == n-m+1){
        Z[k] = Y1[i]
        i <- i+1
      }
      else if (Y1[i]<Y2[j]){
        Z[k] <- Y1[i]
        i <- i+1
      }
      else{
        Z[k] <- Y2[j]
        j <- j+1
      }
    }
    Y <- Z
  }
  return(Y)
}

#' @title Sort_quick
#' @description From the blog(https://www.cnblogs.com/onepixel/articles/7674659.html)
#' @param X the array needed to sort
#' @return the array has been sorted
#' @examples
#' \dontrun{
#' X <- sample(1:50,size=20,replace=T)
#' X
#' Sort_quick(X)
#' }
#' @export
Sort_quick <- function(X){
  Y <- X
  n <- length(Y)
  if(n==1) return(Y)
  Z <- Yright <- Yleft <- numeric(n)
  n2 <- n1 <- 0
  for(i in 2:n){
    if(Y[i]>Y[1]){
      n2 <- n2 +1
      Yright[n2] <- Y[i]
    }
    else{
      n1 <- n1 +1
      Yleft[n1] <- Y[i]
    }
  }
  if(n1==0){
    Zright <- Yright[1:n2]
    Zright <- Sort_quick(Zright)
    Z <- c(Y[1],Zright)
  }
  else if(n2==0){
    Zleft <- Yleft[1:n1]
    Zleft <- Sort_quick(Zleft)
    Z <- c(Zleft,Y[1])
  }
  else{
    Zright <- Yright[1:n2]
    Zleft <- Yleft[1:n1]
    Zleft <- Sort_quick(Zleft)
    Zright <- Sort_quick(Zright)
    Z <- c(Zleft,Y[1],Zright)
  }
  Y <- Z
  return(Y)
}

#' @title Sort_counting_Integer
#' @description From the blog(https://www.cnblogs.com/onepixel/articles/7674659.html)
#' @param X the integer array needed to sort
#' @return the array has been sorted
#' @examples
#' \dontrun{
#' X <- sample(1:50,size=20,replace=T)
#' X
#' Sort_counting_Integer(X)
#' }
#' @export
Sort_counting_Integer <- function(X){## Only useful for integer
  Y <- X
  n <- length(Y)
  if(n==1) return(Y)
  for(i in 1:n){
    Y[i] <- Y[i]%/%1
  }
  M <- m <- Y[1]
  for(i in 2:n){
    if(m>Y[i]) m <- Y[i]
    if(M<Y[i]) M <- Y[i]
  }
  l <- M-m+1
  C <- numeric(l)
  for(i in 1:n){
    C[Y[i]-m+1] <- C[Y[i]-m+1]+1
  }
  Z <- rep(m,C[1])
  for(j in 1:(M-m)){
    if (C[j+1]!=0){
      Z <- c(Z,rep(m+j,C[j+1]))
    }
  }
  Y <- Z
  return(Y)
}

#' @title n_digit
#' @description From the blog(https://www.cnblogs.com/onepixel/articles/7674659.html)
#' @param a the Postive Integer
#' @param n the number of digit
#' @return the function value
#' @export
n_digit <- function(a,n){
  if(n==1){
    return(a%%10)
  }
  else{
    a1 <- a%%(10^n)
    re <- a1%/%(10^(n-1))
    return(re)
  }
}

#' @title Sort_radix_PostiveInteger
#' @description From the blog(https://www.cnblogs.com/onepixel/articles/7674659.html)
#' @param X the postive integer array needed to sort
#' @return the array has been sorted
#' @examples
#' \dontrun{
#' X <- sample(1:50,size=20,replace=T)
#' X
#' Sort_radix_PostiveInteger(X)
#' }
#' @export
Sort_radix_PostiveInteger <-function(X){## Only useful for postive integer
  Y <- X
  n <- length(Y)
  if (n == 1) return(Y)
  for(i in 1:n){
    Y[i] <- abs(Y[i])
  }
  M <- Y[1]
  for(i in 2:n){
    if(Y[i]>M) M <- Y[i]
  }
  if(M==0){return(Y)}
  k <- 1
  a <- M%/%(10^(k-1))
  while(a!=0){
    k <- k+1
    a <- M%/%(10^(k-1))
  }
  k <- k-1
  U <- Y
  for(i in 1:k){
    C <- rep(0,10)
    r <- matrix(0:0,nrow=10,ncol=n)
    for(j in 1:n){
      t <- n_digit(U[j],i)
      C[t+1] <- C[t+1]+1
      r[t+1,C[t+1]] <- U[j]
    }
    t <- 1
    while(C[t]==0){
      t <- t+1
    }
    r1 <- r[t,]
    r1 <- r1[1:C[t]]
    Z <- r1
    t <- t+1
    while(t<11){
      if(C[t]!=0){
        r2 <- r[t,]
        r2 <- r2[1:C[t]]
        Z <- c(Z,r2)
      }
      t <- t+1
    }
    U <- Z
  }
  Y <- U
  return(Y)
}

#' @title Sort_Insert
#' @description From the blog(https://www.cnblogs.com/onepixel/articles/7674659.html)
#' @param X the array needed to sort
#' @return the array has been sorted
#' @examples
#' \dontrun{
#' X <- sample(1:50,size=20,replace=T)
#' X
#' Sort_Insert(X)
#' }
#' @export
Sort_Insert <- function(X){
  Y <- X
  n <- length(Y)
  if(n==1) return(Y)
  for(i in 2:n){
    temp <- Y[i]
    j <- i-1
    while(j > 0&&Y[j]>temp){
      j <- j-1
      Y[j+2] <- Y[j+1]
    }
    Y[j+1] <- temp
  }
  return(Y)
}

#' @title Sort_Shell
#' @description From the blog(https://www.cnblogs.com/onepixel/articles/7674659.html)
#' @param X the array needed to sort
#' @return the array has been sorted
#' @examples
#' \dontrun{
#' X <- sample(1:50,size=20,replace=T)
#' X
#' Sort_Shell(X)
#' }
#' @export
Sort_Shell <- function(X){##Using Sort_Insert
  Y <- X
  n <- length(Y)
  if(n==1) return(Y)
  d <- n%/%2
  while(d!=0){
    m <- n%/%d
    for(i in 1:d){
      arrayd <- rep(0,m)
      for(j in 1:m){
        arrayd[j] <- Y[(j-1)*d+i]
      }
      arrayd <- Sort_Insert(arrayd)
      for(j in 1:m){
        Y[(j-1)*d+i] <- arrayd[j]
      }
    }
    d <- d%/%2
  }
  return(Y)
}

#' @title Sort_Heap
#' @description From the blog(https://www.cnblogs.com/onepixel/articles/7674659.html)
#' @param X the array needed to sort
#' @return the array has been sorted
#' @examples
#' \dontrun{
#' X <- sample(1:50,size=20,replace=T)
#' X
#' Sort_Heap(X)
#' }
#' @export
Sort_Heap <-function(X){
  Y <- X
  n <- length(Y)
  if(n==1) return(Y)
  else if(n==2){
    if(Y[1]>Y[2]){
      temp <- Y[1]
      Y[1] <- Y[2]
      Y[2] <- temp
    }
    return(Y)
  }
  else{
    m <- n%/%2
    for(i0 in 1:m){
      i <- m + 1 - i0
      if (Y[i]<Y[2*i]){
        temp <- Y[2*i]
        Y[2*i] <- Y[i]
        Y[i] <- temp
      }
      if(2*i+1<n+1){
        if (Y[i]<Y[2*i+1]){
          temp <- Y[2*i+1]
          Y[2*i+1] <- Y[i]
          Y[i] <- temp
        }
      }
    }
    Yn <- Y[1]
    Y[1] <- Y[n]
    Z <- Y[1:(n-1)]
    Z <- Sort_Heap(Z)
    Y <- c(Z,Yn)
    return(Y)
  }
}

#' @title Naive estimate density
#' @description Compute the adaptive estimate density by computing the suitable bandwidth
#' @param Sample the sample
#' @param x the function's argument
#' @param h the initial bandwidth
#' @return the naive density function
#' @examples
#' \dontrun{
#' set.seed(2020)
#' rsample <- function(N){
#'  lnY <- rnorm(N,mean = 0,sd=1)
#'  Y <- exp(lnY)
#'  return(Y)
#' }
#' true_density <- function(t){
#'  return_value <- dnorm(log(t),0,1)/t
#'  return(return_value)
#' }
#' h <- 0.3
#' n <- 200
#' X <- rsample(n)
#' y <- seq(0.002,16,0.002)
#' plot(y, true_density(y), "l", main = "h =const")
#' lines(y,fhat_simple(X,y,h),"l",col =  2)
#' }
#' @export
fhat_simple <- function(Sample,x,h){
  N <- length(Sample)
  K <- numeric(N)
  L <- length(x)
  fhat <- numeric(L)
  for(j in 1:L){
    for(i in 1:N){
      if(Sample[i]>x[j]+h){
        K[i] <- 0
      }
      else if(Sample[i]<x[j]-h){
        K[i] <- 0
      }
      else{
        K[i] <- 1/2
      }
    }
    fhat[j] <- sum(K)/(N*h)
  }
  return(fhat)
}

#' @title Adaptive estimate density
#' @description Compute the adaptive estimate density by computing the suitable bandwidth
#' @param Sample the sample
#' @param x the function's argument
#' @param h the initial bandwidth
#' @return the estimate density function
#' @examples
#' \dontrun{
#' set.seed(2020)
#' rsample <- function(N){
#'  lnY <- rnorm(N,mean = 0,sd=1)
#'  Y <- exp(lnY)
#'  return(Y)
#' }
#' true_density <- function(t){
#'  return_value <- dnorm(log(t),0,1)/t
#'  return(return_value)
#' }
#' h <- 0.3
#' n <- 200
#' X <- rsample(n)
#' y <- seq(0.002,16,0.002)
#' plot(y, true_density(y), "l", main = "adaptive")
#' lines(y,fhat_adaptive(X,y,h),"l",col=2)
#' }
#' @export
fhat_adaptive <- function(Sample,x,h){
  N <- length(Sample)
  s <- numeric(N)
  c <- 0.5
  L <- length(x)
  f_hat <- numeric(L)
  f_simple <- fhat_simple(Sample,Sample,h)
  T_s <- (prod(f_simple))^(1/N)
  for(i in 1:N){s[i] <- (f_simple[i]/T_s)^(-c)}
  K_hat <- numeric(N)
  for(j in 1:L){
    for(i in 1:N){
      if(Sample[i]>x[j]+h*s[i]){
        K_hat[i] <- 0
      }
      else if(Sample[i]<x[j]-h*s[i]){
        K_hat[i] <- 0
      }
      else{
        K_hat[i] <- 1/(2*N*h*s[i])
      }
    }
    f_hat[j] <- sum(K_hat)
  }
  return(f_hat)
}
