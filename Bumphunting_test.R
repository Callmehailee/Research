##Bump hunting Algorithm

#Existing package: PRIMsrc

#Install the latest version
install.packages("devtools")
library("devtools")
devtools::install_github("jedazard/PRIMsrc")
library("PRIMsrc")
PRIMsrc.news()
?sbh()


#Read in the data
mydat <- read.table("C:/Users/haily/Desktop/research project/AR-1-Model-Simulation-Data/AR_Pseudodata/this-method-1-20000_VMI_PlotData.txt")
#mydat <- read.table("C:/Users/haily/Desktop/research project/AR-1-Model-Simulation-Data/AR_Pseudodata/this-method-1-20000_VMI_PlotData.txt", skip = 11)
#Only select the first two column:
#col_names <- c("Num Bins, nb", "Mean Var over nb")
mydat <- mydat[,1:2]
colnames(mydat) <- c("Num Bins, nb", "Mean Var over nb")
head(mydat)
mydat["nb"]
mydat[,1]

#sbh(mydat[,1], mydat[,2])


#Prim 
library(prim)
x <- log(as.matrix(mydat[,1])) #Log-transformed
y <- mydat[,2]
mydat.prim <- prim.box(x = x, y = y, threshold.type = 1)

summary(mydat.prim, print.box = TRUE)

s = mydat.prim





summary(mydat.prim)
plot(x,y, xlab = "Number of bins, in log-scale", ylab = "Var in means")

#plot(mydat.prim, col="transparent")  #Does not work...because the univariate input variables

rect(xleft=-0.1585721,xright = 7.1197417,ybottom = min(mydat.prim$y[[1]]), ytop = max(mydat.prim$y[[1]]), density=10, col = "blue")
#min(mydat.prim$y[[2]])



