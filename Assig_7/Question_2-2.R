n=40;
alpha <- 1.2
beta <- 1.2
create_function_for_x <- function(n){
  x <- c();
  for(i in 1:n){
    y <- runif(1)
    x[i] <- -((log(1-y^(1/alpha)))/beta)
  }
  return(x)
}
x <- create_function_for_x(n)
x <- sort(x)
print(x)
create_function_for_F0 <- function(n){
  F0 <- c()
  for(i in 1:n){
    F0[i] <- (1 - exp(-beta*x[i]))^alpha
  }
  return(F0)
}
F0 <- create_function_for_F0(n)
print(F0)

a<-numeric(n)
for(j in 1:n){
  a[j]<-j/n
}
print(a)

c<-numeric(n)
for(l in 1:n){
  c[l]<-abs(F0[l]-a[l])
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
  d_minus[q]=abs(F0[q]-d[q])
}
d_minus_max<-max(d_minus)
print(d_minus_max)
max_1<-max(d_minus,d_plus_max)
print(max_1)
hypo<-0.215
if(hypo>max_1){
  print("Accepted")
}else {
  print("Rejected")
}

