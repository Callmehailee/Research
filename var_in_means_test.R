#Variance in mean for t.s data testing
#try phi = 0.1, sd = 2
#Simulate data from AR(1) process
sigma1 = 2
phi1 = 0.1
sim1 = arima.sim(n = 500, list(ar = phi1), sd = sigma1)
ts.plot(sim1)

#Calculate sample mean for the first three observations:
#x_bar = 1/3*(sim1[1] + sim1[2] + sim1[3])

#first three observations:
#sim1_three = sim1[1:3]


#Variance of X_t function
var_fun <- function(phi, sigma){
  var_xt = (sigma)^2/(1-phi^2)
  return(var_xt)
}

#Autocovariance function
autocov_fun <- function(phi, sigma, k){
  acovf = ((phi)^k)*(sigma^2/(1-phi^2))
  return(acovf)
}


#Variance in means function:
#initialize sum
sum1 = 0
sum2 = 0

var_in_means <- function(n){
  for (i in 1:length(sim1)){
    for (j in 1:length(sim1)){
      if(j == i){
        sum1 = sum1 + var_fun(phi1, sigma1)
      }#variance case
      else{
        diff = abs(j-i) #this gives the k-value
        sum2 = sum2 + autocov_fun(phi1, sigma1, diff)
      }#covariance case
    }
  }
  sum = (1/n^2)*(sum1 + sum2)
  return(sum)
}

#For a single simulation, n = 500
var_in_means(500)


