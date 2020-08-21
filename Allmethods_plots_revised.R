#---------------------------------------
#Bumphunting Method:
#---------------------------------------

#for each column:
#number of points: 500, 1000, 2000, 5000, 10000, 20000, 50000

#file name: Col-i_NP-#pts_VMI_PlotData.txt

#81 columns for each choice in total:
#->for each choice of number of points, there are 81 path names

pathnames = NULL

num_pts <- c(500, 1000, 2000, 5000, 10000, 20000, 50000)

for(i in 1:length(num_pts)){
  for(j in 1:81){
    pathnames = c(pathnames, paste0("C:/Users/17038/Desktop/research project/Bumphunting/This_Method/", "Col-", toString(j), "_NP-",
                                    toString(num_pts[i]), "_VMI_PlotData.txt"))
  }
}
length(pathnames) #567

pathnames[1]
pathnames[567]

#Separated pathnames:
path_500 <- pathnames[1:81]
path_1000 <- pathnames[82:162]
path_2000 <- pathnames[163:243]
path_5000 <- pathnames[244:324]
path_10000 <- pathnames[325:405]
path_20000 <- pathnames[406:486]
path_50000 <- pathnames[487:567]




#Do the calculations:
All_VIM <- NULL

for (i in 1:81){
  #Read in the data:
  mydat <- read.table(path_50000[i], skip = 10)
  
  #Only select the first two columns:
  mydat <- mydat[,1:2] 
  colnames(mydat) <- c("Num Bins, nb", "Mean Var over nb")
  head(mydat)
  
  #Set x and y axis of the plot box:
  x <- mydat[,1] #coerce it as a matrix
  y <- mydat[,2]
  
  N <- dim(mydat)[1] #N (number of pts) = 998
  
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
      #print(paste0("Removing data points from the high end has the larger average left over ", avg_high))
    }
    else{
      final_x <- low_remained_x
      final_y <- low_remained_y
      pts_rm <- length(final_x)
      #print(paste0("Removing data points from the low end has the larger average left over ", avg_low))
    }
    
    #Plot the new box:
    #plot(log(as.matrix(mydat[,1])),y, xlab = "Number of bins(nb), in log-scale", ylab = "Mean Variance over nb",  xlim = c(1, 10), ylim = c(0, 0.02))
    
    #Set x and y axis limits:
    #final_xmin <- min(log(final_x))
    #final_xmax <- max(log(final_x))
    #final_ymin <- min(final_y)
    #final_ymax <- max(final_y)
    
    #Show the box:
    #rect(xleft = final_xmin,xright = final_xmax, ybottom = final_ymin, ytop = final_ymax, density = 10, col = "red")
  }
  #save the bumphunting suggested variance in means value:
  All_VIM <- c(All_VIM, mean(final_y))
}

All_VIM #81




#-----------------------------------------------
#Theoretical Method:
#-----------------------------------------------


#Read in all outputs of theoretical method:
all_outputs <- read.table("C:/Users/17038/Desktop/research project/Bumphunting/All_AR-VM_for-R.txt")
colnames(all_outputs) <- c("Sigma", "Phi", "NumPts", "arVarMean", "col")


library(tidyverse)
#number of points: 500, 1000, 2000, 5000, 10000, 20000, 50000

#pts = 500
dat_500 <- filter(all_outputs, NumPts == "500")
dat_500 <- dat_500[order(dat_500$Sigma),] #reorder in ascending order
vim_ar_500 <- as.vector(t(select(dat_500, arVarMean))) #ar method
All_VIM_500 <- All_VIM #bumphunting method
df_500 <- data.frame("x" = vim_ar_500, "y" = All_VIM_500)


#pts = 1000
dat_1000 <- filter(all_outputs, NumPts == "1000")
dat_1000 <- dat_1000[order(dat_1000$Sigma),] #reorder in ascending order
vim_ar_1000 <- as.vector(t(select(dat_1000, arVarMean))) #ar method
All_VIM_1000 <- All_VIM
df_1000 <- data.frame("x" = vim_ar_1000, "y" = All_VIM_1000)



#pts = 2000
dat_2000 <- filter(all_outputs, NumPts == "2000")
dat_2000 <- dat_2000[order(dat_2000$Sigma),] #reorder in ascending order
vim_ar_2000 <- as.vector(t(select(dat_2000, arVarMean))) #ar method
All_VIM_2000 <- All_VIM
df_2000 <- data.frame("x" = vim_ar_2000, "y" = All_VIM_2000)



#pts = 5000
dat_5000 <- filter(all_outputs, NumPts == "5000")
dat_5000 <- dat_5000[order(dat_5000$Sigma),] #reorder in ascending order
vim_ar_5000 <- as.vector(t(select(dat_5000, arVarMean))) #ar method
All_VIM_5000 <- All_VIM
df_5000 <- data.frame("x" = vim_ar_5000, "y" = All_VIM_5000)



#pts = 10000
dat_10000 <- filter(all_outputs, NumPts == "10000")
dat_10000 <- dat_10000[order(dat_10000$Sigma),] #reorder in ascending order
vim_ar_10000 <- as.vector(t(select(dat_10000, arVarMean))) #ar method
All_VIM_10000 <- All_VIM
df_10000 <- data.frame("x" = vim_ar_10000, "y" = All_VIM_10000)


#pts = 20000
dat_20000 <- filter(all_outputs, NumPts == "20000")
dat_20000 <- dat_20000[order(dat_20000$Sigma),] #reorder in ascending order
vim_ar_20000 <- as.vector(t(select(dat_20000, arVarMean))) #ar method
All_VIM_20000 <- All_VIM
df_20000 <- data.frame("x" = vim_ar_20000, "y" = All_VIM_20000)


#pts = 50000
dat_50000 <- filter(all_outputs, NumPts == "50000")
dat_50000 <- dat_50000[order(dat_50000$Sigma),] #reorder in ascending order
#delete repeat elements rows:
dat_50000 <- dat_50000[-c(2, 4, 6, 8, 10, 12, 14, 16), ]
vim_ar_50000 <- as.vector(t(select(dat_50000, arVarMean))) #ar method
All_VIM_50000 <- All_VIM
df_50000 <- data.frame("x" = vim_ar_50000, "y" = All_VIM_50000)



#Show all the data frame for different number of points used:
df_500
df_1000
df_2000
df_5000
df_10000
df_20000
df_50000


# Correlation: 
cor(vim_ar_500, All_VIM_500)
cor(vim_ar_1000, All_VIM_1000)
cor(vim_ar_2000, All_VIM_2000)
cor(vim_ar_5000, All_VIM_5000)
cor(vim_ar_10000, All_VIM_10000)
cor(vim_ar_20000, All_VIM_20000)
cor(vim_ar_50000, All_VIM_50000)


# Make scatterplot: in log scale, for 500pts, 1000pts, 2000pts, 5000pts, 10000pts, 20000pts, 50000pts
library(ggplot2)
colors <- c("500 pts" = "blue", "1000 pts" = "red", "2000 pts" = "purple", "5000 pts" = "green", "10000 pts" = "yellow", "20000 pts" = "brown",
            "50000 pts" = "pink")

p <- ggplot(data = df_500, aes(x = log(x), y = log(y), color = "500 pts")) + geom_point()
p+ geom_point(data = df_1000, aes(x = log(x), y = log(y), color = "1000 pts")) +
  geom_point(data = df_2000, aes(x = log(x), y = log(y), color = "2000 pts")) +
  geom_point(data = df_5000, aes(x = log(x), y = log(y), color = "5000 pts")) +
  geom_point(data = df_10000, aes(x = log(x), y = log(y), color = "10000 pts")) +
  geom_point(data = df_20000, aes(x = log(x), y = log(y), color = "20000 pts"))+
  geom_point(data = df_50000, aes(x = log(x), y = log(y), color = "50000 pts"))+
  geom_abline(slope = 1)+
  labs(x = "AR method VAR in means",
       y = "PRIM method VAR in means",
       color = "Legend") +
  scale_color_manual(values = colors)
