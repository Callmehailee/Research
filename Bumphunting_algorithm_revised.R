#Bumphunting algorithm  7/30---------------------------

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




#Initialization:
#high_remained:the remained box after cutting from the high-end
#low_remained:the remained box after cutting from the low end 

high_remained_x <- NULL 
high_remained_y <- NULL 
low_remained_x <- NULL
low_remained_y <- NULL

final_x <- x #Start with all data points
final_y <- y #Start with all data points



#Stop condition: pts remained <= target pts_rm
Tg_pts_rm <- 0.1*N  #pts remained


#Start the iteration:
pts_rm <- Tg_pts_rm #Instantaneous pts remained, start with all of the pts
pts_rmv <- trunc(0.01*N) #pts removed


#First start with the whole box of data, cutting either from the high end or from the low end
#After each iteration, based on the left-over mean, the final box will be determined to be either the low remained or the high remained
#The new iteration will be implemented on the new final box, same procedures will be applied
#Cutting from the both side, high end and low end
#So that the final box should be initialized with the whole box of data


while(pts_rm >= Tg_pts_rm){
  
  #Each time alpha*N of points are removed, alpha*N will be truncated into an integer:
  
  #Start from the high end of x:
  #Remove the last 100 points:
  high_remained_x <- head(final_x, -pts_rmv)
  high_remained_y <- head(final_y, -pts_rmv)
  
  #Then for the low end of x:
  #Remove the first 100 points:
  low_remained_x <- tail(final_x, -pts_rmv)
  low_remained_y <- tail(final_y, -pts_rmv)
  
  #Calculate the average of y of the leftover box region:
  #Pick the box with the largest average of y:
  avg_high <- mean(high_remained_y)
  avg_low <- mean(low_remained_y)
  
  if(avg_high > avg_low){
    final_x <- high_remained_x
    final_y <- high_remained_y
    pts_rm <- length(final_x) #current number of points remained
    # print(final_x) #number of bins used
    
    print(paste0("Removing data points from the high end has the larger average left over ", avg_high))
  }
  else{
    final_x <- low_remained_x
    final_y <- low_remained_y
    pts_rm <- length(final_x)
    #print(final_x) #number of bins used
    print(paste0("Removing data points from the low end has the larger average left over ", avg_low))
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
