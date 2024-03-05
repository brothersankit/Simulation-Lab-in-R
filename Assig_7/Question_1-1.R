n=30;
y =runif(n,2,5)
y=sort(y)
uniform_cdf <- function(n) {
  samples <- numeric(n)
  for (i in 1:n) {
    samples[i] <- (y[i]-2)/3
  }
  return(samples)
}
cdf <-uniform_cdf(n)
print(cdf)

a<-numeric(n)
for(j in 1:n){
  a[j]<-j/n
}
print(a)

c<-numeric(n)
for(l in 1:n){
  c[l]<-abs(cdf[l]-a[l])
}
print(c)
d_plus_max<-max(c)
print(d_plus_max)

d<-numeric(10)
d[1]=0
for(m in 2:10){
  d[m]<-a[m-1]
}
print(d)
d_minus<-numeric(10)
for(q in 1:10){
  d_minus[q]=abs(cdf[q]-d[q])
}
d_minus_max<-max(d_minus)
print(d_minus_max)
max_1<-max(d_minus,d_plus_max)
print(max_1)
hypo<-0.240
if(hypo>max_1){
  print("Accepted")
}else {
  print("Rejected")
}

