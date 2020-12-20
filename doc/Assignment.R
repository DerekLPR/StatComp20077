## -----------------------------------------------------------------------------
curve(sin(x), 0, 4*pi)
abline(h=0, lty=3)
curve(cos(x), 0, 4*pi)
abline(h=0, lty=3)

## ----eval=FALSE,fig.width=10/2.54, fig.height=10/2.54, out.width="45%"--------
#  ##The figure is hard to use in R packages,so eval=FALSE
#  knitr::include_graphics("smartphone/mi10u.png")
#  knitr::include_graphics("smartphone/P40.png")
#  knitr::include_graphics("smartphone/findX2Pro.png")

## -----------------------------------------------------------------------------
n <- 1000
u <- runif(n)
x <- sqrt(4/(1-u)) #simulate a random sample from the Pareto(2, 2) distribution.
hist(x, prob = TRUE, main = expression(f(x)==8/x^{3}))#density histogram of sample
y <- seq(2, 25, .01)
lines(y, (2/y)^3) #density curve pdf

## -----------------------------------------------------------------------------
n <- 1000
k <- 0 #counter for accepted
j <- 0 #iterations,it's not necessary
y <- numeric(n)
while (k < n) {
u <- runif(1)
j <- j + 1
z <- runif(1) 
x <- 2*z-1 #random variate from g(x)
if ( (1-x^2) >u ) {#when accept x
#we accept x
k <- k + 1
y[k] <- x
}
}
j#the number of variable X
hist(y, prob = TRUE)#the histogram of Y

## -----------------------------------------------------------------------------
n <- 1000
u <- runif(n)
x <- (16/(1-u))^(1/4)-2 #simulate a random sample from the Pareto(2, 2) distribution with 1000 observations
hist(x, prob = TRUE, main = expression(f(x)==64/(2+x)^5))#density histogram of sample(empirical distribution)
y <- seq(0, 4, .01)
lines(y, 2*(2/(y+2))^5)#density curve pdf(theoretical distribution)

## -----------------------------------------------------------------------------
set.seed(79)
m <- 100000
x <- runif(m)##x~U(0,1)
t <- x*pi/3##t~U(0,pi/3)
theta.hat <- mean(sin(t))## the estimate
print(theta.hat)##output the estimate 

## -----------------------------------------------------------------------------
print((theta.hat-0.5)/0.5)##the percent of bias

## -----------------------------------------------------------------------------
print(-2*exp(2)+6*exp(1)-2)## the theoretical Covariance

## -----------------------------------------------------------------------------
print(-3*exp(2)+10*exp(1)-5)##the theoretical Variance

## -----------------------------------------------------------------------------
print((-3*exp(2)+10*exp(1)-5)/4)

## -----------------------------------------------------------------------------
set.seed(20077)
n <-10000
u <-runif(n)
MC.simple <-exp(u)##the smaple of Simple Monte Carlo
Mc.antithetic <-(exp(u)+exp(1-u))/2##the smaple of Antithetic Variables
theta.s <-mean(MC.simple)##the estimate of  Simple Monte Carlo
theta.a <-mean(Mc.antithetic)##the estimate of Antithetic Variables
sd.s <-sd(MC.simple)##the sd of Simple Monte Carlo
sd.a <-sd(Mc.antithetic)##the sd of Antithetic Variables
print(theta.s)##the estimate of  Simple Monte Carlo
print(theta.a)##the estimate of  Simple Monte Carlo
print(sd.s)##the sd of Simple Monte Carlo
print(sd.a)##the sd of Antithetic Variables
print((sd.s-sd.a)/sd.s)##the percent reduction in variance using the antithetic

## -----------------------------------------------------------------------------
print(exp(1)-1)##the theoretical value

## -----------------------------------------------------------------------------
set.seed(20077)
n <- 10000
theta.hat <- se <- numeric(2)
gf_1 <- numeric(n)
gf_2 <- numeric(n)
gf1 <- function(z) {##function g/f1
z/sqrt(2*pi*exp(1))
}
gf2 <- function(z) {##function g/f2
5*exp(z-z^2/2)/sqrt(2*pi*exp(2))
}
y <- runif(n)##y~U(0,1)
x <- 1/y##x~U(1,\infty)
gf_1 <- gf1(x)
gf_2 <- gf2(x)
theta.hat[1] <- mean(gf_1)
theta.hat[2] <- mean(gf_2)
se[1] <- sd(gf_1)
se[2] <- sd(gf_2)
rbind(theta.hat, se)

## -----------------------------------------------------------------------------
set.seed(20077)
M <- 50000##number of replicates
theta.hat <- var.hat<- numeric(5)
h <- function(x,k){(exp(-(k-1)/5)-exp(-(k)/5))/(1+x^2)}##importance function on each subinterval
for(i in 1:5){
  x <- runif(M/5, (i-1)/5, i/5)
  y <- h(x,i)
  theta.hat[i]=mean(y)##the estimate on each subinterval
  var.hat[i]=var(y)##the variance on each subinterval
}
theta_hat=sum(theta.hat)
var_hat=sum(var.hat)
theta_hat##output the estimate
var_hat##output the variance
sqrt(var_hat)##the standard deviation

## -----------------------------------------------------------------------------
set.seed(20077)
n <- 25##Sample size of each experiments
m <- 10000##Number of experiments
alpha <- 0.05
Y <- matrix(c(0:0) ,nrow=m , ncol=n)
t <- qt(1-(alpha/2),n-1)##t(alpha/2),use qt function
te <- c(1:m)## te=1 if mu in confidence region,else te=0
for (i in 1:m) {
  Y[i,] <- rnorm(n , mean=0 , sd=1)##normal distribution
  mu <- mean(Y[i,]) 
  se <- sqrt(var(Y[i,]))
  min_Y <- mu-se*t/sqrt(n)##lower limit of confidence region
  max_Y <- mu+se*t/sqrt(n)##upper limit of confidence region
  if(0<min_Y) {te[i]=0}
  else if(0>max_Y) {te[i]=0}
  else {te[i]=1}## te=1 if mu in confidence region,else te=0
}
conlevel <- mean(te)##empirical estimate of the confidence level
print(conlevel)##print empirical estimate of the confidence level
cat("[",min_Y,",",max_Y,"]") ##One of the confidence levels

## -----------------------------------------------------------------------------
set.seed(20077)
n <- 20##Sample size of each experiments
m <- 10000##Number of experiments
alpha <- 0.05
Y <- matrix(c(0:0) ,nrow=m , ncol=n)
t <- qt(1-(alpha/2),n-1)##t(alpha/2),use qt function
te <- c(1:m)## te=1 if mu in confidence region,else te=0
for (i in 1:m) {
  Y[i,] <- rchisq(n , df=2)##chi-square distribution
  mu <- mean(Y[i,]) 
  se <- sqrt(var(Y[i,]))
  min_Y <- mu-se*t/sqrt(n)##lower limit of confidence region
  max_Y <- mu+se*t/sqrt(n)##upper limit of confidence region
  if(2<min_Y) {te[i]=0}
  else if(2>max_Y) {te[i]=0}
  else {te[i]=1}## te=1 if mu in confidence region,else te=0
}
conlevel <- mean(te)##t-interval results
print(conlevel)##print t-interval results

## -----------------------------------------------------------------------------
set.seed(20077)
m <- 10000##Number of experiments
n <- 20##Sample size of each experiments
alpha <- .05
test <- numeric(m)
for(i in 1:m){
  x <- rnorm(n, mean=0, sd=2)##normal distribution
  UCL <- (n-1) * var(x) / qchisq(alpha, df=n-1)## the upper confidence limit
  if(4<UCL) test[i]<-1
  else test[i]<-0## te=1 if mu in confidence region,else te=0
}
t<-mean(test)
t##simulation results in Example 6.4

## -----------------------------------------------------------------------------
set.seed(20077)
alpha <- 0.1
m <- 1000
n <- 20 ##Sample size
a <- c(seq(0.05,1,0.05),seq(1,97,4))##parameter of beta distribution
N <- length(a)
power_sk <- numeric(N)
power_t <- numeric(N)
cv <- qnorm(1-alpha/2, 0, sqrt(6*(n-2) / ((n+1)*(n+3))))
cv_t <- qt(1-alpha,n-1)

skew <- function(y){
  mean_y <- mean (y)
  m_2 <- mean((y-mean_y)^2)
  m_3 <- mean((y-mean_y)^3)
  return(sqrt(((m_3)^2)/((m_2)^3)))
}

ft <- function(z){
  m1<-mean(z)
  m2<-mean((z-m1)^2)
  return(sqrt(20/m2)*(m1-0.5))
}

for (j in 1:N) { 
para <- a[j]
sktests <- numeric(m)
for (i in 1:m) { 
x <- rbeta(n,para,para)
sktests[i] <- as.integer(abs(skew(x)) > cv)
}
power_sk[j] <- mean(sktests)

}

plot(a, power_sk, type = "b",
xlab = bquote(a), ylim = c(0,1))
abline(h = .1, lty = 3)
se_sk <- sqrt(power_sk * (1-power_sk) / m) 
lines(a, power_sk+se_sk, lty = 3)
lines(a, power_sk-se_sk, lty = 3)

for (j in 1:N) { 
para <- a[j]
t_test <- numeric(m)
for (i in 1:m) { 
x <- rbeta(n,para,para)
t_test[i] <- as.integer( ft(x) > cv_t)
}
power_t[j] <- mean(t_test)
}

plot(a, power_t, type = "b",
xlab = bquote(a), ylim = c(0,1))
abline(h = .1, lty = 3)
se_t <- sqrt(power_t * (1-power_t) / m) 
lines(a, power_t+se_t, lty = 3)
lines(a, power_t-se_t, lty = 3)

## -----------------------------------------------------------------------------
set.seed(20077)
m<- 10000
n <- c(20,100,500)
alpha <- 0.055

c5test <- function(x, y) {
X <- x - mean(x)
Y <- y - mean(y)
outx <- sum(X > max(Y)) + sum(X < min(Y))
outy <- sum(Y > max(X)) + sum(Y < min(X))
# return 1 (reject) or 0 (do not reject H0)
return(as.integer(max(c(outx, outy)) > 5))
}




Ftest <- function(x, y, i) {
V_x  <- var(x)
V_y  <- var(y)
k <- c(20,100,500)
qF <- c(qf(1-alpha,k[1],k[1]),qf(1-alpha,k[2],k[3]),qf(1-alpha,k[3],k[3]))
return(as.integer( V_x/V_y > qF[i]))
}


sigma1 <- 1
sigma2 <- 1.1
power_c5<-numeric(3)
power_F<-numeric(3)
pF<- pc5<- numeric(m)
for(i in 1:3){
  for(j in 1:m){
      x <- rnorm(n[i], 0, sigma1)
      y <- rnorm(n[i], 0, sigma2)
      pF[j] <- Ftest(x,y,i)
      pc5[j] <- c5test(x,y)
  }
  power_c5[i] <- mean(pc5)
  power_F[i]<- mean(pF)
}
print(power_c5)
print(power_F)


## -----------------------------------------------------------------------------
set.seed(2007)
library("MASS")
alpha <- 0.05
m <- 1000
n <- 20
cv_1 <- qchisq(1-alpha/2,2)
cv_2 <- qchisq(alpha/2,2)
Sigma <- matrix(c(10,3,3,2),2,2)

sk <- function(x,n){
  l <- nrow(x)
  x1 <- x[,1]
  x2 <- x[,2]
  mean_X1 <- mean(x1)
  mean_X2 <- mean(x2)
  m_11 <- sum((x1-mean_X1)^2)
  m_12 <- sum((x1-mean_X1)*((x2-mean_X2)))
  m_21 <- sum((x1-mean_X1)*(x2-mean_X2))
  m_22 <- sum((x2-mean_X2)^2)
  M <- matrix(c(m_11,m_12,m_21,m_22),nrow=2,ncol=2)
  Mm<-solve(M)##Inverse matrix
  result <- 0
  for(i in 1:n){
    for(j in 1:n){
      a <- matrix(c(x1[i]-mean_X1,x2[i]-mean_X2),nrow=1,ncol=2)
       b <- matrix(c(x1[j]-mean_X1,x2[j]-mean_X2),nrow=2,ncol=1)
       result <- result+a%*%Mm%*%b 
    }
  }
  return(result)
}

re<-numeric(m)
for(j in 1:m){
  x <- mvrnorm(n=20, rep(0, 2), Sigma)
  ske <- sk(x,n)
  if(ske>cv_1) {re[j]=1}
  else if(ske<cv_2) {re[j]=1}
  else {re[j]=0}
}
p <- mean(ske)
print(p)


para <- seq(9,11,0.1)##parameter change
lp <- length(para)
power <- numeric(lp)

for(i in 1:lp){
  for(j in 1:m){
  x <- mvrnorm(n=20, rep(0, 2), matrix(c(para[i],3,3,2),2,2))
  ske <- sk(x,n)
  if(ske>cv_1) {re[j]=1}
  else if(ske<cv_2) {re[j]=1}
  else {re[j]=0}
}
power[i] <- mean(ske)
}

##Curve
plot(para, power, type = "b",
xlab = bquote(para), ylim = c(0,1))
abline(h = .1, lty = 3)
se <- sqrt(power * (1-power) / m) 
lines(para, power+se, lty = 3)
lines(para, power-se, lty = 3)

## -----------------------------------------------------------------------------
set.seed(20077)
library(bootstrap)##the data of law
n <- nrow(law)
cor_j <- numeric(n)
cor_hat <- cor(law$LSAT,law$GPA)

for(i in 1:n){##Jackknife Sample
  law_j <- law[-i,]
  cor_j[i] <-cor(law_j$LSAT,law_j$GPA)
}

cor_jack <- mean(cor_j)
bias_jack <- (n-1)*(cor_jack-cor_hat)
se_jack <-  sqrt((n-1)/n*sum((cor_j-cor_jack)^2))


result<-matrix(c(cor_jack,bias_jack,se_jack),nrow=1,ncol=3)
rownames(result) <- c('Jackknife')
colnames(result) <- c('estimate','bias', 'se')
result


## -----------------------------------------------------------------------------
set.seed(20077)
library(boot)##the data of aircondit
B <- 10000
alpha <- 0.05 
len <- nrow(aircondit)
air <- numeric(len)
for(i in 1:len){
  air[i] <- aircondit[i,]
}
time_boot<-numeric(B)
time_hat<-mean(air)


for(i in 1:B){
  air_boot<- sample(air,len,replace=TRUE)
  time_boot[i] <- mean(air_boot)
}## the Bootstrap sample


boot.n <- function(th,th_hat,conf){## normal confidence interval 
  se_hat <- sd(th)
  z <- qnorm(1-conf/2,mean=0,sd=1)
  result_norm<-matrix(c(th_hat-z*se_hat,th_hat+z*se_hat),nrow=1,ncol=2)
  colnames(result_norm) <- c('0.025','0.975')
  rownames(result_norm) <- c('norm')
  print(result_norm)
}

boot.p <- function(th,conf) {##percentile confidence interval 
quantile(th,probs = c((1-conf)/2,(1+conf)/2))
} 


boot.b <- function(th,th_hat,conf) {##basic confidence interval 
  th_minus <- 2*th_hat-th
  quantile(th_minus,probs=c((1-conf)/2,(1+conf)/2))
} 


boot.BCa <-##the BCa function from textbook
function(x, th0, th, stat, conf = .95) {
# bootstrap with BCa bootstrap confidence interval
# th0 is the observed statistic
# th is the vector of bootstrap replicates
# stat is the function to compute the statistic
x <- as.matrix(x)
n <- nrow(x) 
N <- 1:n
alpha <- (1 + c(-conf, conf))/2
zalpha <- qnorm(alpha)
z0 <- qnorm(sum(th < th0) / length(th))
th.jack <- numeric(n)
for (i in 1:n) {
J <- N[1:(n-1)]
th.jack[i] <- stat(x[-i, ], J)
}
L <- mean(th.jack) - th.jack
a <- sum(L^3)/(6 * sum(L^2)^1.5)
adj.alpha <- pnorm(z0 + (z0+zalpha)/(1-a*(z0+zalpha)))
limits <- quantile(th, adj.alpha, type=6)
return(list("est"=th0, "BCa"=limits))
}

time.boot <- function(dat, index){
# input: dat, data; index, a vector of the bootstrap index
# output: theta, the estimated theta using the bootstrap sample
air.boot <- dat[index]
time.hat<-mean(air.boot)
return(time.hat)
}



boot.n(time_boot,time_hat,1-alpha)##normal confidence interval 

boot.p(time_boot,1-alpha)##percentile confidence interval 

boot.b(time_boot,time_hat,1-alpha)##basic confidence interval 

boot.BCa(air,th0 = time_hat,th = time_boot,stat = time.boot)##BCa confidence interval 

## -----------------------------------------------------------------------------
set.seed(20077)
library(bootstrap) #for the score data
B<-1000
n<-nrow(scor)
m<-ncol(scor)
lambda_hat <- eigen(cov(scor))$values
theta_hat <- lambda_hat[1] / sum(lambda_hat)

theta_jack <- numeric(n)
for (i in 1:n) {
scor_jack <- scor [-i,]
lambda_jack <- eigen(cov(scor_jack))$values
theta_jack[i] <- lambda_jack[1] / sum(lambda_jack)
}
bias_hat_jack <- (n - 1) * (mean(theta_jack) - theta_hat)
se_hat_jack <-  sqrt((n-1)/n*sum((theta_jack-mean(theta_jack))^2))

result<-matrix(c(mean(theta_jack),bias_hat_jack,se_hat_jack),nrow=1,ncol=3)
rownames(result) <- c('Jackknife')
colnames(result) <- c('estimate','bias', 'se')
result


## -----------------------------------------------------------------------------
set.seed(20077)
library(DAAG); attach(ironslag)
a <- seq(10, 40, .1) #sequence for plotting fits
L1 <- lm(magnetic ~ chemical)
plot(chemical, magnetic, main="Linear", pch=16)
yhat1 <- L1$coef[1] + L1$coef[2] * a
lines(a, yhat1, lwd=2)
L2 <- lm(magnetic ~ chemical + I(chemical^2))
plot(chemical, magnetic, main="Quadratic", pch=16)
yhat2 <- L2$coef[1] + L2$coef[2] * a + L2$coef[3] * a^2
lines(a, yhat2, lwd=2)
L3 <- lm(log(magnetic) ~ chemical)
plot(chemical, magnetic, main="Exponential", pch=16)
logyhat3 <- L3$coef[1] + L3$coef[2] * a
yhat3 <- exp(logyhat3)
lines(a, yhat3, lwd=2)
L4 <- lm(log(magnetic) ~ log(chemical))
plot(log(chemical), log(magnetic), main="Log-Log", pch=16)
logyhat4 <- L4$coef[1] + L4$coef[2] * log(a)
lines(log(a), logyhat4, lwd=2)


n <- length(magnetic) #in DAAG ironslag
e1 <- e2 <- e3 <- e4 <- numeric(n*(n-1)/2)
# for n-fold cross validation
# fit models on leave-one-out samples

k <- 0

for (j in 2:n) {
  for(i in 1:(j-1)){
    k <- k+1
    y1 <- magnetic[-j]
    x1 <- chemical[-j]
    x <- x1[-i]
    y <- y1[-i]


    J1 <- lm(y ~ x)
    es1_i <- J1$coef[1] + J1$coef[2] * chemical[i]
    es1_j <- J1$coef[1] + J1$coef[2] * chemical[j]
    e1[k] <- abs(magnetic[i] - es1_i)+abs(magnetic[j] - es1_j)

    J2 <- lm(y ~ x + I(x^2))
    es2_i <- J2$coef[1] + J2$coef[2] * chemical[i] +
J2$coef[3] * chemical[i]^2
    es2_j <- J2$coef[1] + J2$coef[2] * chemical[j] +
J2$coef[3] * chemical[j]^2
    e2[k] <- abs(magnetic[i] - es2_i)+abs(magnetic[j] - es2_j)

    J3 <- lm(log(y) ~ x)
    es3_i <- exp(J3$coef[1] + J3$coef[2] * chemical[i])
    es3_j <- exp(J3$coef[1] + J3$coef[2] * chemical[j])
    e3[k] <- abs(magnetic[i] - es3_i)+abs(magnetic[j] - es3_j)

    J4 <- lm(log(y) ~ log(x))
    es4_i <- exp(J4$coef[1] + J4$coef[2] * log(chemical[i]))
    es4_j <- exp(J4$coef[1] + J4$coef[2] * log(chemical[j]))
    e4[k] <- abs(magnetic[i] - es4_i)+abs(magnetic[j] - es4_j)
  }
}


c(mean(e1^2), mean(e2^2), mean(e3^2), mean(e4^2))



## -----------------------------------------------------------------------------
set.seed(20077)
alpha <- 0.05
B <- 10000
n_X <- 20
n_Y <- 30


count_test <- function(X_1,X_2,alpha_a) {
n_1 <- length(X_1)
n_2 <- length(X_2)
m_1 <- log(alpha_a/2)/(log(n_1)-log(n_1+n_2))
m_2 <- log(alpha_a/2)/(log(n_2)-log(n_1+n_2))
c_1 <- X_1 - mean(X_1)
c_2 <- X_2 - mean(X_2)
out_1 <- sum(c_1 > max(c_2)) + sum(c_1 < min(c_2))
out_2 <- sum(c_2 > max(c_1)) + sum(c_2 < min(c_1))
# return 1 (reject) or 0 (do not reject H0)
return(max(c(as.integer(out_1>m_1),as.integer(out_2>m_2))))
}

test_re <- numeric(B) 
for(b in 1:B){
  X <- rnorm(n_X,0,1)
  Y <- rnorm(n_Y,0,1)
  test_re[b] <- count_test(X,Y,alpha)
}
p_count <- mean(test_re)
p_count## output the power

## -----------------------------------------------------------------------------
set.seed(20077)
library(energy)
library(Ball)
library(RANN)
library(boot)
B <- 100
m <- 30##sample size of X
n <- 30##sample size of Y
L <- c(m,n)
p_energy <- p_ball<- p_NN <-numeric(B)
alpha <- 0.1


T_n <- function(z, ix, len,k) {
  n_1 <- len[1]; 
  n_2 <- len[2]; 
  n <- n_1 + n_2
  if(is.vector(z)) z <- data.frame(z,0);
  z <- z[ix, ];
  NN <- nn2(data=z, k=k+1)
  block_1 <- NN$nn.idx[1:n_1,-1]
  block_2 <- NN$nn.idx[(n_1+1):n,-1]
  count_1 <- sum(block_1 < n_1 + 0.5); 
  count_2 <- sum(block_2 > n_1+0.5)
  (count_1 + count_2) / (k * n)
}



for(b in 1:B){
  X <- rnorm(m,0.3,1)
  Y <- rnorm(n,0.8,1)
  Z <- c(X,Y)
  boot.obs <- eqdist.etest(Z, sizes=L, R=99)
  p_energy[b] <- boot.obs$p.value
  p_ball[b] <- bd.test(x = X, y = Y,num.permutations=99)$p.value

  boot.obj <- boot(data = Z, statistic = T_n, R = 99,
  sim = "permutation", len = L,k=3)
  test_re <- c(boot.obj$t0,boot.obj$t)
  p_NN[b] <- mean(test_re>=test_re[1])
}

print(power_NN<-mean(p_NN<alpha))##power of NN method
print(power_energy<-mean(p_energy<alpha))##  power of energy method
print(power_ball<-mean(p_ball<alpha))## power of ball method

## -----------------------------------------------------------------------------
set.seed(20077)
library(energy)
library(Ball)
library(RANN)
library(boot)
B <- 100
m <- 30##sample size of X
n <- 30##sample size of Y
L <- c(m,n)
p_energy <- p_ball<- p_NN <-numeric(B)
alpha <- 0.1


T_n <- function(z, ix, len,k) {
  n_1 <- len[1]; 
  n_2 <- len[2]; 
  n <- n_1 + n_2
  if(is.vector(z)) z <- data.frame(z,0);
  z <- z[ix, ];
  NN <- nn2(data=z, k=k+1)
  block_1 <- NN$nn.idx[1:n_1,-1]
  block_2 <- NN$nn.idx[(n_1+1):n,-1]
  count_1 <- sum(block_1 < n_1 + 0.5); 
  count_2 <- sum(block_2 > n_1+0.5)
  (count_1 + count_2) / (k * n)
}



for(b in 1:B){
  X <- rnorm(m,0.3,2)
  Y <- rnorm(n,0.8,1.5)
  Z <- c(X,Y)
  boot.obs <- eqdist.etest(Z, sizes=L, R=99)
  p_energy[b] <- boot.obs$p.value
  p_ball[b] <- bd.test(x = X, y = Y,num.permutations=99)$p.value

  boot.obj <- boot(data = Z, statistic = T_n, R = 99,
  sim = "permutation", len = L,k=3)
  test_re <- c(boot.obj$t0,boot.obj$t)
  p_NN[b] <- mean(test_re>=test_re[1])
}

print(power_NN<-mean(p_NN<alpha))##power of NN method
print(power_energy<-mean(p_energy<alpha))##  power of energy method
print(power_ball<-mean(p_ball<alpha))## power of ball method

## -----------------------------------------------------------------------------
set.seed(20077)
library(energy)
library(Ball)
library(RANN)
library(boot)
B <- 100
m <- 30##sample size of X
n <- 30##sample size of Y
L <- c(m,n)
p_energy <- p_ball<- p_NN <-numeric(B)
alpha <- 0.1


T_n <- function(z, ix, len,k) {
  n_1 <- len[1]; 
  n_2 <- len[2]; 
  n <- n_1 + n_2
  if(is.vector(z)) z <- data.frame(z,0);
  z <- z[ix, ];
  NN <- nn2(data=z, k=k+1)
  block_1 <- NN$nn.idx[1:n_1,-1]
  block_2 <- NN$nn.idx[(n_1+1):n,-1]
  count_1 <- sum(block_1 < n_1 + 0.5); 
  count_2 <- sum(block_2 > n_1+0.5)
  (count_1 + count_2) / (k * n)
}



for(b in 1:B){
  X <- rt(m,df=1)
  Y_1 <- rnorm(n,0,1)
  Y_2 <- rnorm(n,0,0.8)
  Y <- numeric(n)
  p <- runif(n)
  for(i in 1:n){
    if(p[i]<0.5) {Y[i]=Y_1[i]}
    else{Y[i]=Y_2[i]}
  }##generate mixture of two normal   distributions
  Z <- c(X,Y)
  boot.obs <- eqdist.etest(Z, sizes=L, R=99)
  p_energy[b] <- boot.obs$p.value
  p_ball[b] <- bd.test(x = X, y = Y,num.permutations=99)$p.value

  boot.obj <- boot(data = Z, statistic = T_n, R = 99,
  sim = "permutation", len = L,k=3)
  test_re <- c(boot.obj$t0,boot.obj$t)
  p_NN[b] <- mean(test_re>=test_re[1])
}

print(power_NN<-mean(p_NN<alpha))##power of NN method
print(power_energy<-mean(p_energy<alpha))##  power of energy method
print(power_ball<-mean(p_ball<alpha))## power of ball method

## -----------------------------------------------------------------------------
set.seed(20077)
library(energy)
library(Ball)
library(RANN)
library(boot)
B <- 10
m <- 10##sample size of X
n <- 100##sample size of Y
L <- c(m,n)
p_energy <- p_ball<- p_NN <-numeric(B)
alpha <- 0.1

T_n <- function(z, ix, len,k) {
  n_1 <- len[1]; 
  n_2 <- len[2]; 
  n <- n_1 + n_2
  if(is.vector(z)) z <- data.frame(z,0);
  z <- z[ix, ];
  NN <- nn2(data=z, k=k+1)
  block_1 <- NN$nn.idx[1:n_1,-1]
  block_2 <- NN$nn.idx[(n_1+1):n,-1]
  count_1 <- sum(block_1 < n_1 + 0.5); 
  count_2 <- sum(block_2 > n_1+0.5)
  (count_1 + count_2) / (k * n)
}



for(b in 1:B){
  X <- rnorm(m,0,1)
  Y <- rnorm(n,0,1)
  Z <- c(X,Y)
  boot.obs <- eqdist.etest(Z, sizes=L, R=999)
  p_energy[b] <- boot.obs$p.value
  p_ball[b] <- bd.test(x = X, y = Y,num.permutations=999)$p.value

  boot.obj <- boot(data = Z, statistic = T_n, R = 999,
  sim = "permutation", len = L,k=3)
  test_re <- c(boot.obj$t0,boot.obj$t)
  p_NN[b] <- mean(test_re>=test_re[1])
}

print(power_NN<-mean(p_NN<alpha))##power of NN method
print(power_energy<-mean(p_energy<alpha))##  power of energy method
print(power_ball<-mean(p_ball<alpha))## power of ball method

## -----------------------------------------------------------------------------
set.seed(20077)
N <- 2000
Sample_Laplace <- function(n,X_1,sigma){
  p <- runif(n-1)
  Sample <- numeric(n)
  count <- 0
  Sample[1] <- X_1
  for(j in 1:(n-1)){
    delta <- rnorm(1,Sample[j],sigma)
    r <- exp(abs(Sample[j])-abs(delta))
    if (p[j]>r){
      Sample[j+1]<-Sample[j]
      count <- count+1
    }
    else{
      Sample[j+1] <- delta
    }
  }
  acceptrate <- count/n
  return(list(acceptrate=acceptrate,Sample=Sample))
}

sigma <- c(0.2,0.6,1.8,5.4)
L <- length(sigma)
x_1 <- 5
accept_rate <- numeric(L)

LSample1<-Sample_Laplace(N,x_1,sigma[1])
LSample2<-Sample_Laplace(N,x_1,sigma[2])
LSample3<-Sample_Laplace(N,x_1,sigma[3])
LSample4<-Sample_Laplace(N,x_1,sigma[4])


accept_rate[1] <- LSample1$acceptrate
accept_rate[2] <- LSample2$acceptrate
accept_rate[3] <- LSample3$acceptrate
accept_rate[4] <- LSample4$acceptrate


sample1 <- LSample1$Sample
sample2 <- LSample2$Sample
sample3 <- LSample3$Sample
sample4 <- LSample4$Sample



result <- matrix(c(sigma,accept_rate),nrow=L,ncol=2,dimnames=list(c("1","2","3","4"),c("sigma","accept rate")))
result


plot(sample1,type = "l",main="sigma=0.2")
plot(sample2,type = "l",main="sigma=0.6")
plot(sample3,type = "l",main="sigma=1.8")
plot(sample4,type = "l",main="sigma=5.4")


## -----------------------------------------------------------------------------
set.seed(20077)


Gelman_Rubin <- function(psi) {##the function from textbook
psi <- as.matrix(psi)
n <- ncol(psi)
k <- nrow(psi)
psi.means <- rowMeans(psi) 
B <- n * var(psi.means) 
psi.w <- apply(psi, 1, "var") 
W <- mean(psi.w) 
Var_hat <- W*(n-1)/n + (B/n)
R_hat <- Var_hat / W 
return(R_hat)
}


N <- 2000
Sample_Laplace <- function(n,X_1,sigma){
  p <- runif(n-1)
  Sample <- numeric(n)
  Sample[1] <- X_1
  for(j in 1:(n-1)){
    delta <- rnorm(1,Sample[j],sigma)
    r <- exp(abs(Sample[j])-abs(delta))
    if (p[j]>r){
      Sample[j+1]<-Sample[j]
    }
    else{
      Sample[j+1] <- delta
    }
  }
  return(Sample)
}

sigma <- c(0.2,0.6,1.8,5.4)
L <- length(sigma)
x_1 <- 5
accept_rate <- numeric(L)
sample <- matrix(c(0:0),nrow=L,ncol=N)
for(l in 1:L){
  sample[l,] <- Sample_Laplace(N,x_1,sigma[l])
}

b <- 100
psi <- t(apply(sample, 1, cumsum))
psi_row <- nrow(psi)
psi_col <- ncol(psi)
for (i in 1:psi_row)
psi[i,] <- psi[i,] / (1:psi_col)
print(Gelman_Rubin(psi))


#par(mfrow=c(2,2))
for (i in 1:L)
plot(psi[i, (b+1):N], type="l",
xlab=i, ylab=bquote(psi))
#par(mfrow=c(1,1)) #restore default
#plot the sequence of R-hat statistics
Rhat <- rep(0, N)
for (j in (b+1):N)
Rhat[j] <- Gelman_Rubin(psi[,1:j])
plot(Rhat[(b+1):N], type="l", xlab="", ylab="R")
abline(h=1.2, lty=2)
print(Gelman_Rubin(psi))

## -----------------------------------------------------------------------------
set.seed(20077)
df <- c(4:25,100,500,1000)
L <- length(df)
root_Bisection <- root_Brent <- numeric(L)
N <- 1000


h <- function(a,k){##the function need to get the root 
  re <- 0
  if(k<2){re <- 0}
  else if(k<=a){re <- 0}
  else if(a<=0){re <- 0}
  else{
    re <-(1-pt(sqrt(a*a*(k-1)/(k-a^2)),df=k-1))-(1-pt(sqrt(a*a*k/(k+1-a^2)),df=k))
  }
  return(re)
}




for(i in 1:L){
  low <- 0.000001
  up <- sqrt(df[i])-0.000001
  root_Brent[i] <- uniroot(function(a) {(1-pt(sqrt(a*a*(df[i]-1)/(df[i]-a^2)),df=df[i]-1))-(1-pt(sqrt(a*a*df[i]/(df[i]+1-a^2)),df=df[i]))},
interval = c(0.000001, sqrt(df[i])-0.000001))$root
  
  y <- numeric(3)
  y[1] <- h(low,df[i])
  y[3] <- h(up,df[i])
  if(y[1]*y[3]>0){
    root_Bisection[i] <- sqrt(df[i])-0.000001
  }
  else{
    count <- 0
    low <- 0.00001
    up <- sqrt(df[i])-0.00001
    epsilon <- 0.00001
    r <- (up+low)/2
    y[2] <- h(r,df[i])
    while(abs(y[2])>epsilon && count < N){
      r <- (up+low)/2
      y[1] <- h(low,df[i])
      y[3] <- h(up,df[i])
      y[2] <- h(r,df[i])
      if(y[1]*y[2]>0){
        low <- r
      }
      else{
        up <- r
      }
      count <- count + 1
    }
    if(count<N){root_Bisection[i] <- r}
    else{root_Bisection[i]<-sqrt(df[i])-0.000001}
  }
}



result <- matrix(c(df,root_Bisection,root_Brent),nrow=L,ncol=3)
colnames(result)<-c("df","Bisection","Brent")
result

## -----------------------------------------------------------------------------
set.seed(20077)

n_At <- 444
n_Bt <- 132
n_OO <- 361
n_AB <- 63
n <- n_At+n_Bt+n_OO+n_AB
  
  
N <- 15
p_hat <- q_hat <- r_hat <- l<- numeric(N)##l is the log-likelihood without constant
p_hat[1] <- 0.2 
q_hat[1] <- 0.3 
r_hat[1] <- 1-p_hat[1]-q_hat[1]
l[1] <- 2*n_OO*log(r_hat[1])+n_AB*log(p_hat[1]*q_hat[1]) +n_At*log(2*p_hat[1]*r_hat[1]+p_hat[1]*p_hat[1])+n_Bt*log(2*q_hat[1]*r_hat[1]+q_hat[1]*q_hat[1])


for(j in 2:N){
  p_hat[j] <- n_AB/(2*n)+n_At/n*(1-q_hat[j-1])/(2*r_hat[j-1]+p_hat[j-1])
  q_hat[j] <- n_AB/(2*n)+n_Bt/n*(1-p_hat[j-1])/(2*r_hat[j-1]+q_hat[j-1])
  r_hat[j] <- 1-p_hat[j]-q_hat[j]
  l[j] <- 2*n_OO*log(r_hat[j])+n_AB*log(p_hat[j]*q_hat[j]) +n_At*log(2*p_hat[j]*r_hat[j]+p_hat[j]*p_hat[j])+n_Bt*log(2*q_hat[j]*r_hat[j]+q_hat[j]*q_hat[j])
} 

re <- matrix(c(p_hat,q_hat,r_hat,l),nrow=N,ncol=4)
colnames(re) <- c("p_hat","q_hat","r_hat","log-likelihood")
re


## -----------------------------------------------------------------------------
set.seed(20077)

formulas <- list(
mpg ~ disp,
mpg ~ I(1 / disp),
mpg ~ disp + wt,
mpg ~ I(1 / disp) + wt
)

l1 <- list
l2 <- list
l3 <- list
l4 <- list
Lm <- list(l1,l2,l3,l4)



for(i in 1:4){
mtcars.list <- split(mtcars, mtcars$cyl)
mylm <- function(x, form, me) lm(form, x, method = me)
Lm[[i]] <- lapply(mtcars.list, mylm, formulas[[i]], "qr")
}
Lm


## -----------------------------------------------------------------------------
set.seed(20077)
trials <- replicate(
100,
t.test(rpois(10, 10), rpois(7, 10)),
simplify = FALSE
)
sapply(trials,function(x) x[["p.value"]])


## -----------------------------------------------------------------------------
set.seed(20077)

new_lapply<-function(x,y,f,f.value,...){
  L <- list(x,y)
  l<-Map(unlist,L)
  re <- vapply(l,f,f.value,...)
  return(re)
}

z_1 <- c(26,43,51,57,28,73)
z_2 <- c(35,60,53,27,48,23)
new_lapply(z_1,z_2,sum,double(1))

## -----------------------------------------------------------------------------
set.seed(20077)
library("Rcpp")
library("microbenchmark")


R_Lap <- function(sigma, x0, N){
  p <- runif(N)
  count <- numeric(N-1)
  Sample <- numeric(N)
  Sample[1] <- x0
 
 for (i in 1:(N-1)) {
  y <- rnorm(1, Sample[i], sigma)
  r <- exp(abs(Sample[i])-abs(y))
  if (p[i] > r) {
    Sample[i+1] <- Sample[i]
    count[i] <- 0
  }  
  else {
    Sample[i+1] <- y
    count[i] <- 1
  }
 }
 sum_count <- sum(count)
 ac <- sum_count/N
 return(list(Sample = Sample, ac = ac))
}


sourceCpp(code='
#include <Rcpp.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#define PI 3.141592654

using namespace Rcpp;
//[[Rcpp::export]]

double C_runif(double min, double max){
  double x,t;
  double z=RAND_MAX+1.0;
  x = rand()/z;
  t = min+ (max-min)*x;
  return t;
}

//[[Rcpp::export]]

double C_rnorm(double mu,double sigma){
  double U,V;
  double Z,norm;
  U = C_runif(0,1);
  V = C_runif(0,1);
  Z = sqrt(-2.0 * log(U))* sin(2.0 * PI * V);
  norm = mu+Z*sigma;
  return(norm);
}

//[[Rcpp::export]]

NumericVector C_Lap(double sigma,double x0,int N){
NumericVector Sample(N+1);
Sample[0] = x0;
int count=0;
double y,p,r;
for(int i=1; i<N; i++){
  p = C_runif(0,1);
  y = C_rnorm(Sample[i-1],sigma);
  r = exp(fabs(Sample[i-1])-fabs(y));
  if (p <= r) {
    Sample[i]=y;
    count = count+1;
  }
  else{
    Sample[i] = Sample[i-1];
  }
}
double c=count;
Sample[N]=c/N;
return Sample;
} ')## the acceptance rate is the last element in the array


N <- 3000
sigma <- c(0.1,0.6,3.6,12)
L <- length(sigma)
x_1 <- 2
accept_rate <- numeric(L)
C_ac <- numeric(L)

set.seed(20077)
C1 <- C_Lap(sigma[1],x_1,N) 
C_ac[1] <- C1[N+1]
C_Sample1 <- C1[1:N]
set.seed(20077)
C2 <- C_Lap(sigma[2],x_1,N) 
C_ac[2] <- C2[N+1]
C_Sample2 <- C2[1:N]
set.seed(20077)
C3 <- C_Lap(sigma[3],x_1,N) 
C_ac[3] <- C3[N+1]
C_Sample3 <- C3[1:N]
set.seed(20077)
C4 <- C_Lap(sigma[4],x_1,N) 
C_ac[4] <- C4[N+1]
C_Sample4 <- C4[1:N]

print_C_ac  <- matrix(c(sigma,C_ac),nrow=L,ncol=2,dimnames=list(c("1","2","3","4"),c("sigma","accept rate")))
print_C_ac

#par(mfrow=c(2,2)) 
Sample_C = cbind(C_Sample1, C_Sample2, C_Sample3,  C_Sample4)
for (j in 1:4) {
  plot(Sample_C[,j], type="l",
  xlab=bquote(sigma == .(round(sigma[j],3))),
        ylab="X", ylim=range(Sample_C[,j]))
}


R_Sample1 <- R_Lap(sigma[1],x_1,N)$Sample
R_Sample2 <- R_Lap(sigma[2],x_1,N)$Sample
R_Sample3 <- R_Lap(sigma[3],x_1,N)$Sample
R_Sample4 <- R_Lap(sigma[4],x_1,N)$Sample

#par(mfrow=c(2,2))
qqplot(C_Sample1,R_Sample1)
lines(C_Sample1,C_Sample1,type="l",col="red")
qqplot(C_Sample2,R_Sample2)
lines(C_Sample2,C_Sample2,type="l",col="red")
qqplot(C_Sample3,R_Sample3)
lines(C_Sample3,C_Sample3,type="l",col="red")
qqplot(C_Sample4,R_Sample4)
lines(C_Sample4,C_Sample4,type="l",col="red")




a <- sigma[1]
b <- sigma[2]
c <- sigma[3]
d <- sigma[4]
z <- x_1 

microbenchmark::microbenchmark(C_Lap(a,z,N),R_Lap(a,z,N),C_Lap(b,z,N),R_Lap(b,z,N),C_Lap(c,z,N),R_Lap(c,z,N),C_Lap(d,z,N),R_Lap(d,z,N))   

