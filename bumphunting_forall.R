#Load the package:
library(prim)

#----------------------------------------------------------------

#for each column:
#number of points: 500, 1000, 2000, 5000, 10000, 20000, 50000

#file name: Col-i_NP-#pts_VMI_PlotData.txt
  
#81 columns for each choice in total:
#->for each choice of number of points, there are 81 path names

pathnames = NULL

num_pts <- c(500, 1000, 2000, 5000, 10000, 20000, 50000)

for(i in 1:length(num_pts)){
  for(j in 1:81){
    pathnames = c(pathnames, paste0("C:/Users/haily/Desktop/research project/Bumphunting/This_Method/", "Col-", toString(j), "_NP-",
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



#Read in multiple files, using 500 points for example:
#Use path_500 as the pathnames:


All_VIM <- NULL
All_MIN <- NULL
All_MAX <- NULL
All_x <- NULL
All_y <- NULL

#Read in all the data and apply prim.box function to all data files

for (i in 1:81){
  #Read in the data:
  mydat <- read.table(path_1000[i], skip = 10)
  
  
  #Only select the first two columns:
  mydat <- mydat[,1:2] 
  colnames(mydat) <- c("Num Bins, nb", "Mean Var over nb")
  #head(mydat)
  
  #prim.box function requires the input variable in a matrix form
  x <- log(as.matrix(mydat[,1])) #Log-transformed & coerce it as a matrix
  All_x <- c(All_x, x)
  y <- mydat[,2]
  All_y <- c(All_y, y)
  mydat.result <- prim.box(x = x, y = y, threshold.type = 1)
  
  
  #save the bumphunting suggested variance in means value:
  VIM_suggested <- mydat.result$y.fun[1]
  All_VIM <- c(All_VIM, VIM_suggested)
  
  #Note, here I select the first box, but not sure if the method will always set the first box as the optimal box
  min = mydat.result$box[[1]][1,1] #min of the selected box
  All_MIN <- c(All_MIN, min)
  max = mydat.result$box[[1]][2,1] #max of the selected box
  All_MAX <- c(All_MAX, max)
}



All_MIN
length(All_MIN)
All_MAX
length(All_MAX)
All_VIM
length(All_VIM)

all <- c(All_MIN, All_MAX, All_VIM)
my_summary <- matrix(all, nrow = 3, byrow = TRUE)
colnames(my_summary) <- paste("Col", 1:81, sep = "")
rownames(my_summary) <- c("Min of the box", "Max of the box", "Suggested Var in Means")
View(my_summary)



All_x <- matrix(All_x, nrow = 193, byrow = FALSE) #All x are the same (x axis value)

All_y <- matrix(All_y, nrow = 193, byrow = FALSE) #y values are different for each column
colnames(All_y) <- paste("Col", 1:81, sep = "")
View(All_y)


#All_y[,1] #select first col, col1 data

#Make plots:
#Use col1 data:
#Plot the data, and the box of data points selected through PRIM:
plot(All_x[,1],All_y[,1], xlab = "Number of bins(nb), in log-scale", ylab = "Mean Variance over nb")

#Show the box:
#I set the ytop as the max of the overal var in means and ybottom as the min of the overal var in means
rect(xleft = All_MIN[1],xright = All_MAX[1],ybottom = min(All_y[,1]), ytop = max(All_y[,1]), density = 10, col = "red")





#my_summary contains all the var in means value for each columns, using PRIM method
#var_mat contains all the var in means value for each columns, using AR calculation method




#Make data frames for plotting:
# pts = 500
vim_ar_500 <- as.vector(t(var_mat_500))
All_VIM_500 <- All_VIM
df_500 <- data.frame("x" = vim_ar_500, "y" = All_VIM_500)


# pts = 1000
vim_ar_1000 <- as.vector(t(var_mat_1000))
All_VIM_1000 <- All_VIM
df_1000 <- data.frame("x" = vim_ar_1000, "y" = All_VIM_1000)


# pts = 2000
vim_ar_2000 <- as.vector(t(var_mat_2000))
All_VIM_2000 <- All_VIM
df_2000 <- data.frame("x" = vim_ar_2000, "y" = All_VIM_2000)

#------------------------------------------------------------
#Have not run these: 
# pts = 5000
vim_ar_5000 <- as.vector(t(var_mat_5000))
All_VIM_5000 <- All_VIM
df_5000 <- data.frame("x" = vim_ar_5000, "y" = All_VIM_5000)


# pts = 10000
vim_ar_10000 <- as.vector(t(var_mat_10000))
All_VIM_10000 <- All_VIM
df_10000 <- data.frame("x" = vim_ar_10000, "y" = All_VIM_10000)


# pts = 20000
vim_ar_20000 <- as.vector(t(var_mat_20000))
All_VIM_20000 <- All_VIM
df_20000 <- data.frame("x" = vim_ar_20000, "y" = All_VIM_20000)


# pts = 50000
vim_ar_50000 <- as.vector(t(var_mat_50000))
All_VIM_50000 <- All_VIM
df_50000 <- data.frame("x" = vim_ar_50000, "y" = All_VIM_50000)


# Correlation: 
cor(vim_ar_500, All_VIM_500)
cor(vim_ar_1000, All_VIM_1000)
cor(vim_ar_2000, All_VIM_2000)



# Make scatterplot: in log scale, for 500, 1000, 2000 pts
library(ggplot2)
colors <- c("500 pts" = "blue", "1000 pts" = "red", "2000 pts" = "purple")

p <- ggplot(data = df_500, aes(x = log(x), y = log(y), color = "500 pts")) + geom_point()
p+ geom_point(data = df_1000, aes(x = log(x), y = log(y), color = "1000 pts")) +
  geom_point(data = df_2000, aes(x = log(x), y = log(y), color = "2000 pts"))
  labs(x = "AR method VAR in means",
       y = "PRIM method VAR in means",
       color = "Legend") +
  scale_color_manual(values = colors)

       