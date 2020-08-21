#Bumphunting algorithm---------------------------

#Read in the data----

#col58, 20000 pts:


#mydat <- read.table("C:/Users/17038/Desktop/research project/AR-1-Model-Simulation-Data/This_Method/Col-58_NP-20000_VMI_PlotData.txt", skip = 10)

#col61, 50000 pts:
mydat <- read.table("C:/Users/17038/Desktop/research project/AR-1-Model-Simulation-Data/This_Method/Col-61_NP-50000_VMI_PlotData.txt", skip = 10)

#Only select the first two columns:
mydat <- mydat[,1:2] 
colnames(mydat) <- c("Num Bins, nb", "Mean Var over nb")
head(mydat)

#Set x and y axis of the plot box:
x <- mydat[,1] #Log-transformed & coerce it as a matrix
y <- mydat[,2]

N <- dim(mydat)[1] #N (number of pts) = 998

#Step1: Enclose all the data points in a box:
plot(log(as.matrix(mydat[,1])),y, xlab = "Number of bins(nb), in log-scale", ylab = "Mean Variance over nb",  xlim = c(1, 10), ylim = c(0, 0.02))

#Set x and y axis limits:
x_min <- min(log(x))
x_max <- max(log(x))
y_min <- min(y)
y_max <- max(y)

#Show the box:
rect(xleft = x_min,xright = x_max,ybottom = y_min, ytop = y_max, density = 10, col = "red")


#Set alpha(peeling-alpha, number of points removed) and beta(target number of points remained)
#alpha = 0.1
#Each time alpha*N of points are removed, alpha*N will be truncated into an integer:
#beta = 0.1 (normally)


#Initialization:
#high_remained:the remained box after cutting from the high-end
#low_remained:the remained box after cutting from the low end 

high_remained_x <- x #start from all x value
high_remained_y <- y #start from all y values
low_remained_x <- x
low_remained_y <- y

final_x <- NULL #receive the instantaneous x values
final_y <- NULL #receive the instantaneous y values



#Stop condition: pts remained <= target pts_rm
Tg_pts_rm <- 0.9*N  #pts remained


#Start the iteration:
pts_rm <- Tg_pts_rm #Instantaneous pts remained, start with all of the pts
pts_rmv <- trunc(0.01*N) #pts removed

while(pts_rm >= Tg_pts_rm){
  
  #Each time alpha*N of points are removed, alpha*N will be truncated into an integer:

  #Start from the high end of x:
  #Remove the last 100 points:
  high_remained_x <- head(high_remained_x, -pts_rmv)
  high_remained_y <- head(high_remained_y, -pts_rmv)
 
  #Then for the low end of x:
  #Remove the first 100 points:
  low_remained_x <- tail(low_remained_x, -pts_rmv)
  low_remained_y <- tail(low_remained_y, -pts_rmv)
  
  #Calculate the average of y of the leftover box region:
  #Pick the box with the largest average of y:
  avg_high <- mean(high_remained_y)
  avg_low <- mean(low_remained_y)
  
  if(avg_high > avg_low){
    final_x <- high_remained_x
    final_y <- high_remained_y
    pts_rm <- length(final_x) #current number of points remained
   # print(final_x) #number of bins used
    print(paste0("Removing data points from the high end has the larger average left over", avg_high))
  }
  else{
    final_x <- low_remained_x
    final_y <- low_remained_y
    pts_rm <- length(final_x)
    #print(final_x) #number of bins used
    print(paste0("Removing data points from the low end has the larger average left over", avg_low))
  }
  
}

final_x
length(final_x)
final_y
length(final_y)

#Plot the new box:
plot(log(as.matrix(mydat[,1])),y, xlab = "Number of bins(nb), in log-scale", ylab = "Mean Variance over nb",  xlim = c(1, 10), ylim = c(0, 0.02))

#Set x and y axis limits:
final_xmin <- min(log(final_x))
final_xmax <- max(log(final_x))
final_ymin <- min(final_y)
final_ymax <- max(final_y)

#Show the box:
rect(xleft = final_xmin,xright = final_xmax, ybottom = final_ymin, ytop = final_ymax, density = 10, col = "red")
