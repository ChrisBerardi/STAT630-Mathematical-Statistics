# 4.1.11
pdf(file = "C:\\Users\\Saistout\\Desktop\\630 Stuff\\homework\\hw06plots.pdf", 8.5, 11)
par(mfrow=c(2,1))
maxima <- rep(0,1000)
for (i in 1:1000) maxima[i] <- max(rnorm(10))
(mean(maxima))
(sqrt(var(maxima)))
hist(maxima, main="Sample size 10", xlab="max value", breaks=50)
rm(i)


maxima <- rep(0,1000)
for (i in 1:1000) maxima[i] <- max(rnorm(20))
(mean(maxima))
(sqrt(var(maxima)))
hist(maxima, main="Sample size 20", xlab="max value", breaks=50)
rm(i)
dev.off();

#4.2.12
interval <-0
value <- rep(0,100000)
for (i in 1:100000) value[i] <- mean(rexp(20, rate =5))
rm(i)
for (i in 1:100000) if (value[i] <.21 & value[i] > .19) interval = interval+1
(interval/length(value))
rm(i)

interval <-0
for (i in 1:100000) value[i] <- mean(rexp(50, rate =5))
rm(i)
for (i in 1:100000) if (value[i] <.21 & value[i] > .19) interval = interval+1
(interval/length(value))
rm(i)

# 4.4.12
(pgamma(2.5*16, 16, rate=.5))
(pgamma(2.5*32, 32, rate=.5))
(pgamma(2.5*100, 100, rate=.5))

# 4.4.16
interval <-0
uniform <- rep(0,100000)
for (i in 1:100000) uniform[i] <- mean(runif(20, min=-20, max = 10))
for (i in 1:100000) if (uniform[i] <= -5) interval = interval+1
(interval/length(uniform))


# 4.5.14
n=100000
 x=runif(n)  
temp=cos(x^3)*sin(x^4)
sum(temp)/n
integrand=function(x){sin(x^4)*cos(x^3)}
integrate(integrand, lower = 0, upper = 1)

