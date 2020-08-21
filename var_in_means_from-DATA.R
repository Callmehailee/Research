##Variance in mean for t.s data testing
##try phi = 0.1, sd = 2
##Simulate data from AR(1) process
sigma1 = 2
phi1 = 0.1
#sim1 = arima.sim(n = 10000, list(ar = phi1), sd = sigma1)
#ts.plot(sim1)

#thisData=read.table("mynewwholepseudodata.txt.col_3",header=FALSE,sep=" ")
thisData=read.table("test.dataset",header=FALSE,sep=" ")
sim1=thisData[[1]]


#Read in multiple of files:
filenames = NULL
wholeData = list() #This will update until contains all the data

#i and j will determine which pair of phi and theta will be using

#Loop for determine the pair of phi/theta and its corresponding data file to be read in:
sd<-c(2:10) #9 sigma values

phi <- c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9) #9 coefficients

k = 0
myval = NULL
mysd = NULL
myphi = NULL
for(i in 1:length(sd)){
  for(j in 1:length(phi)){
    k = k+1 #k determines the file index
    myval = c(myval, k)
    #filenames = c(filenames, paste0("C:/Users/haily/Desktop/research project/AR-1-Model-Simulation-Data/AR_Pseudodata/", "mynewwholepseudodata.txt.col_", toString(k)))
    #wholeData[k] = read.table(filenames[k], header = FALSE, sep = " ")
    #Tell us which phi and sd values are using:
    #cat("(",sd[i],",",phi[j],")", ";")
    mysd = c(mysd, sd[i])
    myphi = c(myphi, phi[j])
  }
}

for(i in 1:length(myval)){
  filenames = c(filenames, paste0("C:/Users/haily/Desktop/research project/AR-1-Model-Simulation-Data/AR_Pseudodata/", "mynewwholepseudodata.txt.col_", toString(myval[i])))
}



mymatrix = matrix(nrow = 81, ncol = 3)
mymatrix[,1] = mysd
mymatrix[,2] = myphi
mymatrix[,3] = filenames
mymatrix

#wholeData = read.table(mymatrix[81,3], header = FALSE, sep = " ")
for (i in 1:81){
  wholeData[i] = read.table(mymatrix[i, 3], header = FALSE, sep = " ")
}

#Decide the amount of data to read in for analysis:
#This could be user-input:
#set n = 50
#From the first dataset, select the first column, and select the first 50 data points
wholeData[1][,1][1:50]










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
#Initialize the sum:
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

#Test the function:
This_var=var_in_means(length(sim1))

print(This_var)


mydata_i = read.table(mymatrix[3,3], header = FALSE, sep = " ")
mydata_i[,1][1:40]
