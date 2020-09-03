# this .R file provides the complete algorithm for plant height measurement
# and growth curve fitting.

setwd("/Users/apple/Work/RA/Prof_Nettleton/image_analysis/Github/Project1")

library(EBImage)
library(png)
library(jpeg)
library(ggplot2)
library(changepoint)
library(reshape2)
library(monreg)

################## Functions ##################

## read jpeg black and white image
readJPEG_bw  <- function(name) {
  out <- round( as.matrix(readJPEG(name)) )
  return( out )
}

## mode function
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

## change file names and record 
change_name <- function(name0){
  name1 <- sub(".*_", "", name0)
  name2 <- sub("[\\.].*", "", name1)
  return(name2)
}


## change file names into time
find_time <- function(img_folder_path){
  file_names <- list.files(path = img_folder_path)
  nn <- length(file_names)
  time <- NULL
  for (i in 1:nn){
    time <- c(time,
              change_name(file_names[i])
    )
  }
  return(as.POSIXlt(time,format="%Y-%m-%d-%H-%M"))
}


## find first row peak  
# cr0: row threshold;
# cr1 & cr2: left and right column threshold;
first_row_peak <- function(img, cr0=0.2, cr1=0.05, cr2=0.075, sep=10){
  n1 <- dim(img)[1]
  row_mean <- apply(img, 1, mean)[n1:1]                            
  D_row_mean <- data.frame(x = 1:n1, y = row_mean)
  s_row_mean <- predict(loess(y~x,data = D_row_mean, span = 0.05), 
                        D_row_mean$x)
  s_row_mean <- pmax(s_row_mean, 0)
  ind1 <- which( s_row_mean > max(s_row_mean) * cr0 )              
  temp <- which( (ind1[-1]-ind1[-length(ind1)]) > sep )
  if (length(temp)==0){
    ind2 <- ind1[length(ind1)]                                      
  } else {                                                       
    ind2 <- temp[1]                                            
  }
  ind3 <- which.max(s_row_mean[ind1][1:ind2])                   
  ind_peak <- ind1[ind3]                                 
  ind_lb <- which(s_row_mean[1:ind_peak] > cr1 * s_row_mean[ind_peak])[1]
  if (is.na(ind_lb)){
    ind_lb <- 1                                  
  }       
  ind_ub <- ind_peak + which( s_row_mean[ ind_peak:n1 ] < cr2 * s_row_mean[ind_peak] )[1] 
  if (is.na(ind_ub)){
    ind_ub <- n1
  }
  return( c(n1 - ind_ub + 1, n1 - ind_lb + 1) )
}


## find when to stop recording (first and second row not separable)
find_stop_place <- function(seg_folder_path, cr10=0.2, cr11=0.05, cr12=0.075, sep1=10, time0){
  file_names <- list.files(path = seg_folder_path)
  nn <- length(file_names)
  height <- rep(0, nn)
  cut <- matrix(0, nn, 2)
  for (i in 1:nn){
    seg_name <- paste(seg_folder_path, "/", file_names[i], sep = "")
    seg <- readJPEG_bw(seg_name) 
    cut_ind <- first_row_peak(seg, cr10, cr11, cr12, sep1)
    height[i] <- cut_ind[2] - cut_ind[1]
    cut[i,] <- cut_ind
  }
  change_ind <- cpts(cpt.mean(height, Q=1))-1
  bound <- quantile(height[-(1:change_ind)], 0.5)
  plant_use_ind0 <- which(height[1:change_ind] < bound)
  ## plot 
  plot(time0, height, pch = 16, xlab = "time", ylab = "row height", 
       main = "Change Point Detection")
  abline(v = as.POSIXct(time0[change_ind]), col = "red")
  abline(h = bound, col = "red")
  
  return(list(change_ind = change_ind, height = height, 
              cut = cut, plant_use_ind0 = plant_use_ind0))
}


## column cut
column_peaks <- function(img, row_ind, p = 2, cr0 = 0.2, sep = 50){
  img1 <- img[ row_ind[1]:row_ind[2], ]
  col_mean <- apply(img1, 2, mean)
  ind1 <- which( col_mean^p > max(col_mean^p) * cr0 )
  ind2 <- which( (ind1[-1] - ind1[-length(ind1)]) > sep )
  n_peak <- length(ind2) + 1
  ind2 <- c(0, ind2, length(ind1))
  col_peaks <- NULL
  for (i in 1:(n_peak)){
    ind3 <- which.max(col_mean[ind1][(ind2[i]+1):(ind2[i+1])])
    col_peaks[i] <- ind1[(ind2[i]+1):(ind2[i+1])][ind3]
  }
  
  return(list(n_plant = n_peak, col_peaks = col_peaks))
  
}


## row & column cut for early stage plants
column_row_early <- function(seg_folder_path, cr10=0.2, cr11=0.05, cr12=0.075, sep1=10,
                             p = 2, cr20 = 0.2, sep2 = 50, time0){
  file_names <- list.files(path = seg_folder_path)
  x.list <- find_stop_place(seg_folder_path, cr10, cr11, cr12, sep1, time0)
  change_ind <- x.list$change_ind
  height <- x.list$height
  row_ind <- x.list$cut
  plant_use_ind0 <- x.list$plant_use_ind0
  n_plant <- rep(0, change_ind)
  col_peaks <- vector("list", change_ind)
  for (i in 1:change_ind){
    seg_name <- paste(seg_folder_path, "/", file_names[i], sep = "")
    seg <- readJPEG_bw(seg_name) 
    y.list <- column_peaks(seg, row_ind[i,], p, cr20, sep2)
    n_plant[i] <- y.list$n_plant
    col_peaks[[i]] <- y.list$col_peaks
  }
  
  plant_num <- Mode(n_plant)
  plant_use_ind1 <- which(plant_num == n_plant)
  plant_use_ind <- intersect(plant_use_ind0, plant_use_ind1)
  col_mat <- matrix(unlist(col_peaks[plant_use_ind]), byrow = TRUE, 
                    ncol = plant_num)
  mean_col_peaks <- apply(col_mat, 2, mean)
  de_plant <- apply(col_mat, 1, median) - median(mean_col_peaks)
  final_col_peaks <- rep(1, length(plant_use_ind)) %*% t( mean_col_peaks) + 
    de_plant %*% t(rep(1, plant_num))
  
  ## column cut
  final_col_cuts <- matrix(0, length(plant_use_ind), plant_num + 1)
  for (i in 1:length(plant_use_ind)){
    final_cut0 <- floor( (final_col_peaks[i,-1] + final_col_peaks[i,-plant_num])/2 )
    delta <- max(final_cut0[-1] - final_cut0[-length(final_cut0)])
    final_cut <- c(
      max(1, final_cut0[1] - delta),  final_cut0,
      min(ncol(seg), final_cut0[length(final_cut0)] + delta)
    )
    final_col_cuts[i,] <- final_cut
  }
  
  
  ##plot
  plot(time0[1:change_ind], n_plant, xlab = "time", ylab = "number of plants",
       main = "Check Number of Plants", 
       pch = 16, col = ifelse(n_plant == plant_num, 2, 1))
  
  
  plot(time0, height, pch = 16, xlab = "time", ylab = "row height", main = "Valid photos")
  points(time0[1:change_ind][plant_use_ind], height[1:change_ind][plant_use_ind],
         pch = 16, col = "red")
  
  plot(rep(time0[1:change_ind][plant_use_ind], plant_num), c(col_mat), 
       xlab = "time", ylab = "column index",
       main = "Check Column Peak Distributions", pch = 16, cex = 0.75, 
       ylim = c(1, dim(seg)[2]))
  for (j in 1:plant_num){
    lines(time0[1:change_ind][plant_use_ind], final_col_peaks[,j], col = "red")
  }
  for (j in 1:(plant_num+1)){
    lines(time0[1:change_ind][plant_use_ind], final_col_cuts[,j], col = "blue", lty = 2)
  }
  
  
  output <- list(change_ind = x.list$change_ind, row_ind = row_ind[1:change_ind,][plant_use_ind,],
                 plant_num = plant_num, col_ind = final_col_cuts, plant_use_ind = plant_use_ind)
  
}



## height measurement for one plant (img = img for single plant)
height_measure <- function(img, cr1 = 0.1, cr2 = 0.1, cr = 0.05){
  if (sum(img) == 0){
    return(c(NA, NA, NA))
  }
  else{
    n1 <- nrow(img)
    n2 <- ncol(img)
    col_mean <- apply(img, 2, mean)
    max_col <- max(col_mean)
    m_ind <- which.max(col_mean)
    flag_lb <- col_mean[1:m_ind] < cr1 * max_col
    col_lb <- ifelse(sum(flag_lb)>0, which(flag_lb)[sum(flag_lb)], 1)
    flag_ub <- col_mean[m_ind:n2] < cr2 * max_col
    col_ub <- ifelse(sum(flag_ub)>0, which(flag_ub)[1]+m_ind-1, n2)
    
    row_mean <- apply(img[,col_lb:col_ub], 1, mean)
    max_row <- max(row_mean)
    plant_rows <- which(row_mean > cr * max_row)
    row_lb <- plant_rows[1]
    row_ub <- plant_rows[length(plant_rows)]
    return(c(m_ind, row_lb, row_ub))
  }
}


## height measurement for a sequence of photos
height_measure_seq <- function(seg_folder_path, cr10=0.2, cr11=0.05, cr12=0.075, sep1=10,
                               p = 2, cr20 = 0.2, sep2 = 50, cr31=0.1, cr32=0.1,
                               cr3=0.05){
  file_names <- list.files(path = seg_folder_path)
  time0 <- find_time(seg_folder_path)
  
  par(mfrow = c(2,2))
  para <- column_row_early(seg_folder_path, cr10, cr11, cr12, sep1,
                           p, cr20, sep2, time0)
  par(mfrow = c(1,1))
  
  change_ind <- para$change_ind
  plant_num <- para$plant_num
  plant_use_ind <- para$plant_use_ind
  
  height_mat <- matrix(0, plant_num, length(plant_use_ind))
  
  for (i in 1:length(plant_use_ind)){
    seg_name <- paste(seg_folder_path, "/", file_names[plant_use_ind[i]], sep = "")
    seg <- readJPEG_bw(seg_name) 
    row_ind <- para$row_ind[i,1]:para$row_ind[i,2]
    
    for (j in 1:plant_num){
      col_ind <- para$col_ind[i,j]:para$col_ind[i,j+1]
      seg_single <- seg[row_ind, col_ind]
      para1 <- height_measure(seg_single, cr31, cr32, cr3)
      height_mat[j,i] <- para1[3] - para1[2]
    }
  }
  
  time <- find_time(seg_folder_path)[1:change_ind][plant_use_ind]
  return(list(time=time, height_mat=height_mat, nPlant = plant_num))
}










xx <- height_measure_seq("segmentation/CAM322_seg", cr10=0.1, cr11=0.025, cr12=0.075, sep1=10,
                         p = 2, cr20 = 0.2, sep2 = 50, cr31=0.1, cr32=0.1,
                         cr3=0.025)


### smoothing spline curve estimation
par(mfrow=c(2,3))
for (i in 1:xx$nPlant){
  x <- xx$time
  y <- xx$height_mat[i,]
  ind.use <- which(!is.na(y))
  res <- smooth.spline(x[ind.use], y[ind.use], df = 4)
  plot(x, y, main = paste("plant", i), 
       xlab = "time", ylab = "height")
  lines(res, col = "red")
}
par(mfrow=c(1,1))





### monotone curve estimation
par(mfrow=c(2,3))
for (i in 1:xx$nPlant){
  x <- xx$time
  y <- xx$height_mat[i,]
  ind.use <- which(!is.na(y))
  x <- as.numeric(x[ind.use])
  y <- y[ind.use]
  xs <- (x - min(x)) / diff(range(x))
  
  res <- monreg(xs, y, hr = 0.4, hd = 1)
  
  plot(xs, y, main = paste("plant", i), 
       xlab = "time", ylab = "height")
  lines(res$t, res$estimation, col = "red")
}
par(mfrow=c(1,1))




### plot both curves
par(mfrow=c(2,3))
for (i in 1:xx$nPlant){
  t1 <- xx$time
  y <- xx$height_mat[i,]
  ind.use <- which(!is.na(y))
  t1 <- t1[ind.use]
  y <- y[ind.use]
  res1 <- smooth.spline(t1, y, df = 4)
  
  x <- as.numeric(t1)
  xs <- (x - min(x)) / diff(range(x))
  res2 <- monreg(xs, y, hr = 0.3, hd = 1)
  
  t2 <- as.POSIXct( res2$t * diff(range(x)) + min(x) , origin = "1970/01/01")
  
  plot(t1, y, main = paste("plant", i), 
       xlab = "time", ylab = "height")
  lines(res1, col = "red", lwd = 2)
  lines(t2, res2$estimation, col = "blue", lwd = 2)
}
par(mfrow=c(1,1))

