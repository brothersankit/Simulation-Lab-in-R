library(nleqslv)
n<-50
c<-1
k<-0.5
sample<-function(k,c,n){
  x<-c()
  for(i in 1:n){
    y<-runif(1)
    x[i]<-((1-(1/1-y)^(1/k))^(1/c))
  }
  return(x)
}
x<-sample(k,c,n)
x<-sort(x)
print(x)

fn<-function(c){
  sum0=0
  sum1=0
  sum2=0
  for(i in 1:n){
    sum0=sum0+log(x[i])
    sum1=sum1+log(1+x[i]^c)
    sum2=sum2+(((x[i]^c)*log(c))/(1+x[i]^c))
  }
  f<-n/c+sum0-((n/sum1)+1)*sum2
  return(f)
}
mle_c<-nleqslv(c,fn)$x
print(mle_c)

sum3<-0
for(i in 1:n){
  sum3<-sum3+(n/log(1+x[i]^mle_c))
}
mle_k<-n/sum3
print(mle_k)


bias<-(mle_c-c)+(mle_k-k)
print(bias)

mse<-(bias^2)/n
print(mse)




