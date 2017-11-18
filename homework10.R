#7.1.14

mu = 2
tau = 1
sigma = 1
n = 20
x = 8.2
mean_r= (mu/tau^2+n*x/sigma^2)/(1+tau^2+n/sigma^2)
sd_r = sqrt(1/(1/tau^2+n/sigma^2))
B=10000
temp=rep(0,B)
for (i in 1:B) temp[i]=rnorm(1, mean=mean_r, sd = sd_r)

(mean(1/temp))
(sd(1/temp))

7.3.10
pdf(file = "C:\\Users\\Saistout\\Desktop\\630 Stuff\\homework\\hw106plots.pdf", 8.5, 11)
gamma.post=function(alpha1,beta1,alpha2,beta2){
lamda=seq(0,1,length=1001)
par(lwd=2)
upper=max(c(dgamma(lamda,alpha1+20,beta1+20*5.1),dgamma(lamda,alpha2+20,beta2+20*5.1)))
plot(c(0,1),c(0,upper),type="n",xlab=expression(lamda),ylab="density",main="Posterior Densities")
lines(lamda,dgamma(lamda,alpha1+20,beta1+20*5.1),col=1)
lines(lamda,dgamma(lamda,alpha2+20,beta2+20*5.1),col=2)
legend(c(0.75,1),c(.85*upper,upper),lty=rep(1,2),col=1:2,legend=c("Posterior i","Posterior ii"))
}
gamma.post(.25,.5,.25,1/40)

Problem A
par(lwd=2)
alpha=.0456
mu0=50
sig=5
mu=seq(0,1,length=1001)

power=pnorm((mu0/5-20*mu)/(sig)-qnorm(1-alpha/2))+1-pnorm((mu0/5-20*mu)/(sig)+qnorm(1-alpha/2))
plot(mu,power)

legend(c(1.7,1.9),c(0,0.25),lty=rep(1,4),col=1:4,legend=c("n=25","n=50","n=100","n=200"))

dev.off()
