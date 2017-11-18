#5.1.11
n = 10000
count <- 0
x=rnorm(n)
func = x^4+x^3-3
for (i in 1:10000) if (func[i] >1 && func[i] <2)count=count+1
(count/n)

pdf(file = "C:\\Users\\Saistout\\Desktop\\630 Stuff\\homework\\hw07plots.pdf")
#5.5.19
sample = rchisq(50, 1)
boxplot(sample)


#5.4.11
y=rnorm(1000,mean=3, sd=sqrt(2))
plot.ecdf(y)
x=seq(-2, 8, length=1001)
lines(x, pnorm(x,mean=3, sd=sqrt(2)), col=2)

par(mfrow=c(2,1))
x=seq(-5, 10, length=1001)
hist(rnorm(1000, 3 , sqrt(2)), breaks=seq(-5,10, length=15), freq=FALSE)
lines(x, dnorm(x,mean=3, sd=sqrt(2)), col=2)
hist(rnorm(1000, 3 , sqrt(2)), breaks=seq(-5,10, by=.1), freq=FALSE)
lines(x, dnorm(x,mean=3, sd=sqrt(2)), col=2)

dev.off();

#5.5.20
sample = rnorm(40, mean=4, sd=1)
sample = sort.int(sample)
#using 5.5.3
(sample[35]+40*(sample[36]-sample[37])*.025)
(summary(sample))
mean(sample)+sqrt(sd(sample))*1.2816