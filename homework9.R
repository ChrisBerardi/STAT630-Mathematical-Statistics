#6.4.18, adapted from R file used in lectures

{x = c(3.27, -1.24, 3.97, 2.25, 3.47, -0.09, 7.45, 6.20, 3.74,
		4.12, 1.42, 2.75, -1.48, 4.97, 8.00, 3.26, 0.15, -3.64,
		4.88, 4.55)}
B=10000
temp=rep(0,B)
xbar=mean(x)
xsd = sd(x)

#Non-parametic bootstrap
for (i in 1:B)temp[i]=1/mean(sample(x,replace=TRUE))
mean(temp)
sd(temp)
quantile(temp,c(0.025,0.975))
1/xbar-1.96*sd(temp)
1/xbar+1.96*sd(temp)

#Parametric bootstrap
for (i in 1:B)temp[i]=1/mean(rnorm(length(x), mean = xbar, sd=xsd))
mean(temp)
sd(temp)
quantile(temp,c(0.025,0.975))
1/xbar-1.96*sd(temp)
1/xbar+1.96*sd(temp)

#6.5.4 adapted from R file used in lecture

coverage.poisson=function(lamda,n,N,alpha)
{
sumwald=0
sumscore=0
sumscorebetter=0
sumwaldbetter=0
sumbothcorrect=0
sumbothwrong=0




for (i in 1:N){
y=rpois(n,lamda)
phat=mean(y)
z=qnorm(1-(alpha/2))
lower=phat-z*sqrt(phat/n)
upper=phat+z*sqrt(phat/n)
term=sqrt((2*phat+z^2/n)^2-4*phat^2)
lower1=(2*phat+z^2/n-term)/2
upper1=(2*phat+z^2/n+term)/2



waldcorrect=(lower<lamda)*(lamda<upper)
scorecorrect=(lower1<lamda)*(lamda<upper1)


sumwald=sumwald+waldcorrect
sumscore=sumscore+scorecorrect


sumbothcorrect=sumbothcorrect+waldcorrect*scorecorrect
sumbothwrong=sumbothwrong+(1-waldcorrect)*(1-scorecorrect)
sumscorebetter=sumscorebetter+scorecorrect*(1-waldcorrect)
sumwaldbetter=sumwaldbetter+(1-scorecorrect)*waldcorrect




summary=matrix(c(sumbothcorrect,sumwaldbetter,sumscorebetter,sumbothwrong),2,2)


rownames(summary)=c("scoreyes","scoreno")
colnames(summary)=c("waldyes","waldno")


}


return(list(sumwald=sumwald,sumscore=sumscore,summary=summary))
}
coverage.poisson(9.65,20,10000,.05)

