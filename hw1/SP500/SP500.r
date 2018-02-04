# Code written by Neil Shephard, 3 Nov 2017. 
#install.packages("curl")
#install.packages("quantmod", method = "curl")

# for those interested in automatically downloading financial data 
   require("quantmod")
   X<- getSymbols("SPY",env=NULL,src="yahoo",from=as.Date("1986-01-04"),to=as.Date("2017-11-01"));
#   xRet <- 100*diff(log(Ad(X))) # geometric returns scaled to be roughly % returns
   xRet <- 100*diff(Ad(X))/lag(Ad(X),1) # arithmetic returns scaled % returns
   xRet <- as.numeric(na.omit(xRet)) # throw away rubbish datapoints 
   save(xRet,file = "sp500.rda")

# core code   

setwd("C:/Users/shephard/Dropbox/NeilWork/Teaching/TEACHING/2017/Stat111/NeilR")
load("sp500.rda")
write.csv(xRet, file = "sp500.csv")

mean(xRet); sd(xRet);
quantile(xRet,probs=c(0.05,0.5,0.95));
Fn <-ecdf(xRet); # compute EDF

pdf("xRetHist.pdf")
   hist(xRet,xlim=c(-6,6),breaks=100,xlab="Arithmetic returns",freq=FALSE,main="Histogram of arithmetic returns")
dev.off()

pdf("xRet.pdf")
   plot(Fn,main="EDF of daily returns",xlim=c(-5,5),xlab="Geometric financial daily returns",ylab="EDF")
   abline(h=c(0.05,0.5,0.95),col="blue",lwd=5)
dev.off()

pdf("xRetDen001.pdf")
   plot(density(xRet,bw=0.01,kernel="rectangular",from=-7,to=7),ylim=c(0,0.56),main="Density proxy: daily returns, h=0.01",xlab="Arithmetic financial daily returns")
dev.off()

h = 1.06*(length(xRet)^-0.2)*sd(xRet)
pdf("xRetDenSilverman.pdf")
   plot(density(xRet,bw=h,kernel="rectangular",from=-7,to=7),ylim=c(0,0.56),main="Kernel density estimator: daily returns, h automatic",xlab="Arithmetic financial daily returns")
   abline(v=c(0.0),col="blue",lwd=2)
dev.off()

pdf("xRetDen3.pdf")
   plot(density(xRet,bw=3.0,kernel="rectangular",from=-7,to=7),ylim=c(0,0.56),main="Density proxy: daily returns, h=3.0",xlab="Arithmetic financial daily returns")
dev.off()

