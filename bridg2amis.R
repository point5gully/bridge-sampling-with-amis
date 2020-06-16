x=abs(rnorm(nx<-1e6))
y=rexp(ny<-2e3,rate=2)
f=function(x)exp(-x^2/2)
g=function(x)exp(-2*x)
ronc=sqrt(2*pi)*2/2

ric=mean(f(y)/g(y))
rric=1/mean(g(x)/f(x))
oroc=mean(f(y)/(nx*dnorm(y)+ny*dexp(y,2)))/
  mean(g(x)/(nx*dnorm(x)+ny*dexp(x,2)))
morc=(sum(f(y)/(nx*dnorm(y)+ny*dexp(y,2)))+
            sum(f(x)/(nx*dnorm(x)+ny*dexp(x,2))))/(
  sum(g(x)/(nx*dnorm(x)+ny*dexp(x,2)))+
         sum(g(y)/(nx*dnorm(y)+ny*dexp(y,2))))