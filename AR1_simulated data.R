#-5/22 Simulation-------------------------------

sd<-c(2:10) #9 sigma values


phi <- c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9) #9 coefficients



#Get the data
simul_fun <- function(sigma, phi, length){
  #set seed:
  set.seed(1)
  ar1 <- arima.sim(n = length, list(ar = c(phi)), sd = sigma)
  return(ar1)
  
}

data_list <- list() #empty data
my_data = NULL
for(val in sd){
  for(val2 in phi){
    data = as.data.frame(simul_fun(val, val2, 50000))
    my_data = c(my_data, data)
    
    
  }
}
my_data[1] #length = 81
View(my_data)

ts.plot(my_data[1])




set.seed(1)
ar1 <- arima.sim(n = 20000, list(ar = c(0.6)), sd = 2)
plot(ar1, pch = 19, lty = 2)
acf(ar1)
pacf(ar1)

#make the dataframe and plot
#by default, span = 0 
ar1 <- read.table("C:/Users/haily/Desktop/research project/AR-1-Model-Simulation-Data/AR_Pseudodata/mynewwholepseudodata.txt.col_6")

dat1 <- data.frame("Time" = 1:50000, "AR1" = ar1[1:50000,])

plot(dat1, pch = 19,lty = 2, ylim=c(-0.3, 0.3))

model1 = loess(AR1~Time, data = dat1, span = 0.2)
lines(dat1$Time,model1$fit,col='red',lty=2,lwd=4)

model2 = loess(AR1~Time, data = dat1, span = 0.3)
lines(dat1$Time,model2$fit,col='blue',lty=2,lwd=4)

model3 = loess(AR1~Time, data = dat1, span = 0.4)
lines(dat1$Time,model3$fit,col='green',lty=2,lwd=4)
model4 = loess(AR1~Time, data = dat1, span = 0.8)
lines(dat1$Time,model4$fit,col='yellow',lty=2,lwd=4)


for(val in sd){
  for(val2 in phi){
    mydat <- simul_fun(val, val2, 50000)
    mydf <- data.frame("Time" = 1:50000, "AR1" = mydat)
    
    plot(mydat, pch = 19,lty = 2)
    model1 = loess(AR1~Time, data = mydf, span = 0.4)
    lines(mydf$Time,model1$fit,col='red',lty=2,lwd=2)
    model2 = loess(AR1~Time, data = mydf, span = 0.8)
    lines(mydf$Time,model2$fit,col='blue',lty=2,lwd=2)
  }
}