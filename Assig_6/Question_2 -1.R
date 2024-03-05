a<-c();
n = 1000
for (i in 1:n) {
  a[i]<-rnorm(1,mean = 1.5,sd=2)
}
print(a)
mle<-sum(a)/n
print(mle)
sigma<-sqrt(sum((a-mle)^2)/n)
print(sigma)
