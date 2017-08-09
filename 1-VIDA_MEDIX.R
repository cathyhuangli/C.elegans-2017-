
#---------------------------------------------------------------------------------#
angleBetween<- function(x,y, unit = c("deg","rad")){
  #' Calculate the angle between two vectors
  #' 
  #' @param    x: a vector of coordinate
     #' @param    y: a vector of coordinate
     #' @param unit: option to choose between degree or radian c("deg","rad")\
     #' @return The angle between 2 vector \code{x} and \code{y}
     #' @examples
     #' > angleBetween( c(1,0),c(0,1) ) 
     #' > 90
     #' > angleBetween( c(1,0),c(1,-sqrt(3)) )
     #' > 60
     
     if( all(length(x)==length(y)) && all(x==y)  ){
       return(0)
     }else{
       dot.prod <- as.numeric(x%*%y) 
       norm.x <- norm(x,type="2")
       norm.y <- norm(y,type="2")
       theta <- as.numeric(acos( pmin(pmax(dot.prod/(norm.x * norm.y),-1.0),1.0 ) ))
       u <- match.arg(unit)
       if(u=="deg"){ return( theta * 360/(2*pi))}
       if(u=="rad"){return ( theta )}
     }
}
#---------------------------------------------------------------------------------#
intervalMarker <- function(intlength=60,lst){
  #' Mark the time interval within a given interval range
  #' 
  #' @param intlength: interval range in seconds
     #' @param        lst: list/vector that has time stamp
     #' @return the dataframe with the interval marker column added
     #' @examples
     #' > ncol(dataframe)
     #' > 5
     #' > dataframe <- intervalMarker( 60,dataframe,"timeStamp") 
     #' > ncol(dataframe)
     #' > 6
     
     start <- 0
     end <- start  + intlength
     intervalnumber <- 1
     rows <- length(lst)
     
     retLst <- numeric(rows)
     
     for (i in 1:rows) {
       if (lst[i] < end) {
         retLst[i] <- intervalnumber
       } else {
         start <- start + intlength
         end <- end + intlength
         intervalnumber <- intervalnumber + 1
         retLst[i] <- intervalnumber
       } 
     }
     return(retLst)
}

#---------------------------------------------------------------------------------#
isMoved <- function(lstX,lstY){
  #' Check whether a list of consecutive coordiates is the same 
  #' 
  #' @param lstX: a list of coordinate x-coordinate
     #' @param lstY: a list of coordinate y-coordinate
     #' @return a vector of logical value that match the index of the given list
     #' @examples
     #' > x <- c(1,1,2,3)
     #' > y <- c(1,1,2,2)
     #' > isMoved(x,y)
     #' > [1]  TRUE FALSE  TRUE  TRUE
     
     rowsX <- length(lstX)
     rowsY <- length(lstY)
     if(rowsX != rowsY || rowsX==0 || rowsY ==0){
       stop("2 arguments must have the same length and positive")
     } else{
       retLst <- c(TRUE,mapply(function(x2,x1,y2,y1) !(x2==x1 && y2==y1) ,
                               lstX[1:rowsX-1], 
                               lstX[2:rowsX],
                               lstY[1:rowsY-1], 
                               lstY[2:rowsY],
                               SIMPLIFY=TRUE) )          
     }
} 

#---------------------------------------------------------------------------------#
frameSelection <- function(lst,sampletime = 1){
  #' robust frame selection method that pick frame at a given time gap
  #' default unit for sampletime is in second 
  #' 
     #' @param        lst: a list of time stamp
     #' @param sampletime: a minimum given time gap between two frames in second
     #' @return a vector of logical value that match the index of the given list
     #' @examples
     #' > x <- c(1,1,2,3)
     #' > y <- c(1,1,2,2)
     #' > [1]  TRUE FALSE  TRUE  TRUE
     rows <- length(lst)
     if(rows<1){stop("Arguments must have enough data to process")}
     initialtime <- lst[1]
     retLst  <- logical(rows)
     for(i in 2:rows){
       if ((lst[i]-initialtime) >= sampletime) {
         initialtime <- lst[i]
         retLst[i]  <- TRUE 
       } 
     }
     return(retLst)
}

#---------------------------------------------------------------------------------#
distanceTo <- function(from_coor, to_coor, unit = c("none","pixel","mm") ){
  #' Calcultate the distance between 2 given coordinates
  #' 
  #' @param from_coor: a cooridinate (x,y)
     #' @param   to_coor: a cooridinate (x,y)  
     #' @param      unit: an option to choose unit of distance, default is none 
     #' @return Eulid distance between 2 coordinates
     #' @examples
     #' > distanceTo(c(0,0),c(3,4))
     #' > [1] 5
     #' > distanceTo(c(2,2),c(0,0)) # return 2*sqrt(2)
     #' > [1] 2.828427 
     
     PIXEL.OVER.MM.RATIO <- 70
     u <- match.arg(unit)
     retLst <- 0
     if(u == "none") {
       retLst <-  sqrt((to_coor[1]-from_coor[1])**2 + (to_coor[2]- from_coor[2])**2) 
     }     
     if(u == "pixel") {
       retLst <-  sqrt((to_coor[1]-from_coor[1])**2 + (to_coor[2]- from_coor[2])**2) 
     }
     if(u=="mm"){
       retLst <- sqrt((to_coor[1]-from_coor[1])**2 + (to_coor[2]- from_coor[2])**2) / PIXEL.OVER.MM.RATIO
     }
     return( retLst )
}


#---------------------------------------------------------------------------------#
speed  <- function(lstX,lstY, timeStamp, unit=c("mm/s","pixel/s")){
  #' Calcultate the movement speed between 2 time series
  #' 
     #' @param lstX: a cooridinate (x,y)
     #' @param lstY: a cooridinate (x,y)
     #'  @param timeStamp: a list of timeStamp corresponding to the coordinates  
     #' @return return movement speed over a duration of time, default unit is in mm/s
     #' @examples
     #' > speed(seq(1,10)*70,rep(0,10),seq(10))
     #' > [1] 0 1 1 1 1 1 1 1 1 1
     #' > speed(seq(1,20,2)*70,rep(0,10),seq(10))
     #' > [1] 0 2 2 2 2 2 2 2 2 2
     u = match.arg(unit)
     u = gsub(pattern="*/s","",x=u)
     rowsX <- length(lstX)
     rowsY <-  length(lstY)
     rows <- length(timeStamp)
     
     if( rowsX != rowsY || rowsX==0 || rowsY ==0){
       stop("3 arguments must have the same length and positive")
     } else {
       distance <- mapply(function(x_from,y_from,x_to,y_to) distanceTo(c(x_from,y_from), c(x_to,y_to), unit ="mm" ), 
                          lstX[1:rowsX-1], lstY[1:rowsY-1],
                          lstX[2:rowsX],   lstY[2:rowsY],
                          SIMPLIFY = TRUE )
       timeDelta <- duration(timeStamp)[-1]
       retLst <- distance/timeDelta
       return (c(0,retLst))
     }
}

#---------------------------------------------------------------------------------#

duration <- function(lst){
  #' Calcultate the duration between 2 time series
  #' 
  #' @param lst: a list of time stamp 
     #' @return return a duration between 2 time series .i.e t2- t1
     #' @examples
     #' > duration(c(1,2,3))
     #' > [1] 1 1 1
     #' > duration(c(1.1, 2.3, 10.11))
     #' > [1] 1.10 1.20 7.81
     rows <- length(lst)
     if(rows < 2){
       stop("Not enough data to calcuate duration")
     }
     
     retLst <- mapply(function(t1,t2) t2-t1, lst[1:rows-1],lst[2:rows])
     return(c(lst[1],retLst))
}

#---------------------------------------------------------------------------------#

stepLength <- function(lstX,lstY, unit=c("pixel","mm")){
  #' Calcultate the duration between 2 time series
  #' 
  #' @param lstX: a list x-coordinate 
     #' @param lstY: a list y-coordinate 
     #' @return return a list distance of each turning events
     #' @examples
     #' > stepLength(c(1,2,3), c(1,3,5))
     #' > 0.00000000 0.03194383 0.03194383
     
     rowsX <- length(lstX)
     rowsY <-  length(lstY)
     
     
     if( rowsX != rowsY || rowsX==0 || rowsY ==0){
       stop("3 arguments must have the same length and positive")
     } else {
       u <- match.arg(unit)
       retLst <- mapply(function(x_from,y_from,x_to,y_to) distanceTo(c(x_from,y_from), c(x_to,y_to), unit =u ), 
                        lstX[1:rowsX-1], lstY[1:rowsY-1],
                        lstX[2:rowsX],   lstY[2:rowsY],
                        SIMPLIFY = TRUE )
       return (c(0,retLst))
     }
}

#---------------------------------------------------------------------------------#




removeBackslash <- function(x){
  #' Helper function to remove the last backslash that mistakenly input by the user in joinPaths function
  #' 
  #' @param x: a string object
     #' @return : return the string witouth thelast backlash in the end of the string
     #' @example
     #' > joinPaths(c("C:","a/b/","cadfasdf/"))
     #' [1] "C:/a/b/cadfasdf"
     #' > joinPaths(c("C:","a/b","cadfasdf////"))
     #' [1] "C:/a/b/cadfasdf"
     #' 
     len <- nchar(x)
     if(len>0){
       last <- substr(x,len,len)
       if(last =="\\" || last == "/"){
         x <- substr(x,1,len-1)
       }
     }
     return(x)  
}



joinPaths <- function(lst){
  #' return the file path from sequence of directories, independent from platform.
  #' An alternative version of  file.path function in base package where only 2 args are permitted
  #' 
  #' @param lst: A list of directory to join path
     #' @return return the string representation of file path from sequence of directories
     #' @examples
     #' > joinPaths(c("D:","b","c","d","e.csv")) 
     #' > "D:/b/c/d/e.csv"
     
     lst <- as.character(lst)
     len = length(lst)
     lst <- sapply(lst,removeBackslash)
     if(len==0){
       return ('')
     }else if(len ==1){
       return (lst[1])
     }else if(len == 2){
       return ( file.path(lst[1],lst[2]) )
     }else{
       head <- joinPaths(lst[1])
       tail <- joinPaths(lst[-1])
       return ( file.path(head,tail) )
     }
}

#---------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------------#
#                        Traverse Directory in Medixsvr funciton                                        #
#-------------------------------------------------------------------------------------------------------#

traverseDir <- function(v_folders_name, output_path){
  
  medix_path <- "//medixsrv/Nematodes/data"
  
  dir.create(file.path(output_path, v_folders_name))
  output_fig_path <- file.path(file.path(output_path, v_folders_name), "Images")
  dir.create(output_fig_path)
  
  analyis_files_path <- file.path(file.path(output_path, v_folders_name), "Analysis_files")
  dir.create(analyis_files_path)
  
  library(data.table)
  library(xlsx)
  movement_path <- file.path(medix_path,file.path(v_folders_name,"data/movementFeatures.csv"))
  
  #-----------------------------------------------------------------------------------#
  #                        MOVEMENT FEATURE DATA                                      #
  #-----------------------------------------------------------------------------------#
  
  n2_f5 <- read.csv(movement_path, header=FALSE)
  setnames(n2_f5, old = c('V1','V2','V3','V4','V5','V6','V7','V8','V9'), new = c('Frame.Num','Time.Elapsed', 'Time.Since.Last.Frame', 'x','y','Speed','Accelerations','Angle','Angular.Velocity'))
  
  n2_f5$Time.Elapsed <- as.numeric(n2_f5$Time.Elapsed)
  n2_f5$Time.Since.Last.Frame <- as.numeric(n2_f5$Time.Since.Last.Frame)
  n2_f5$x <- as.numeric(n2_f5$x)
  n2_f5$y <- as.numeric(n2_f5$y)
  
  #Sampled---------------------------------------------------------------------------------------------------------
  iterations <- nrow(n2_f5)
  variables <- 6
  n <- 1
  sampletime <- 1
  initialtime <- n2_f5$Time.Elapsed[1]
  output <- matrix(ncol=variables, nrow=iterations)
  
  
  
  for(i in 2:iterations){
    if ((n2_f5$Time.Elapsed[i]-initialtime) >= sampletime) {
      initialtime = n2_f5$Time.Elapsed[i]
      output[n,1] = n2_f5$Frame.Num[i]
      output[n,2] = n2_f5$Time.Elapsed[i]
      output[n,3] = n2_f5$Time.Since.Last.Frame[i]
      output[n,4] = n2_f5$x[i]
      output[n,5] = n2_f5$y[i]
      output[n,6] = n2_f5$Speed[i]
      
      n = n + 1
    } 
  }
  
  sampled <- as.data.frame(output)
  sampled <- sampled[complete.cases(sampled),]
  setnames(sampled, old = c('V1','V2','V3','V4','V5','V6'), new = c('Frame.Num', 'Time.Elapsed', 'Time.Since.Last.Frame', 
                                                                    'x','y','speed'))
  
  
  #Add interval marker to sampled
  
  intlength <- 60
  intervalnumber <- 1
  start <- 0
  end <- start + intlength
  
  
  for (i in 1:nrow(sampled)) {
    if (sampled$Time.Elapsed[i] < end) {
      sampled$interval[i] <- intervalnumber
      #print(c(i, intervalnumber))
    } else {
      start <- start + intlength
      end <- end + intlength
      intervalnumber <- intervalnumber + 1
      sampled$interval[i] <- intervalnumber
    }
    
  }
  
  #Delete rows where (x,y) coordinates are the same consecutively
  
  count <- 0
  zerovector <- vector()
  size <- nrow(sampled)-1
  checkdup <- FALSE
  for (i in 1:size) {
    if (sampled[i,4]==sampled[i+1,4] & sampled[i,5]==sampled[i+1,5]) {
      count <- count + 1
      zerovector <- c(zerovector, i)
      checkdup <- TRUE
      #print(i)
    }
  }
  
  #print(checkdup)
  #sampled <- sampled[-zerovector,] # Do not need if there are no duplicates
  
  if (checkdup == TRUE) {
    sampled <- sampled[-zerovector,] # Do not need if there are no duplicates
  }
  
  
  #turningsampled3##################################################################################################
  delta = 10 #Angle Threshold
  iheading = 0
  
  #Calculate initial heading
  idistance = sqrt( (sampled$x[2] - sampled$x[1])**2 + (sampled$y[2] - sampled$y[1])**2 )
  #iangle = (acos((sampled$x[2]-sampled$x[1])/idistance)*360)/(2*pi)
  iangle = acos((sampled$x[2]-sampled$x[1])/idistance)
  if ((sampled$x[2] - sampled$x[1]) > 0 & (sampled$y[2] - sampled$y[1]) < 0) {
    #iangle = 360 - iangle
    iangle = 2*pi - iangle
  } else if ((sampled$x[2] - sampled$x[1]) < 0 & (sampled$y[2] - sampled$y[1]) < 0) {
    #iangle = 180 + (180-iangle)
    iangle = 2*pi - iangle
  }
  
  iterations = nrow(sampled)
  variables = 13
  n <- 1
  record <- 0
  output3 <- matrix(ncol=variables, nrow=iterations)
  angleBetween <- numeric(iterations-1)
  angleBetween[1] <- iangle*180/pi
  
  i <- 3
  while(i < iterations) {
    
    #Calculate heading of subsequent sample data to compare with initial heading
    
    distance = sqrt((sampled$x[i] - sampled$x[n])**2 + (sampled$y[i] - sampled$y[n])**2)
    #angle = (acos((sampled$x[i]-sampled$x[n])/distance)*360)/(2*pi)
    angle = acos((sampled$x[i]-sampled$x[n])/distance)
    if ((sampled$x[i] - sampled$x[n]) > 0 & (sampled$y[i] - sampled$y[n]) < 0) {
      #angle = 360 - angle
      angle = 2*pi - angle
    } else if ((sampled$x[i] - sampled$x[n]) < 0 & (sampled$y[i] - sampled$y[n]) < 0) {
      #angle = 180 + (180-angle)
      angle = 2*pi - angle
    } else if ((sampled$x[i] - sampled$x[n])==0 & (sampled$y[i] - sampled$y[n]) < 0) {
      angle = (3/2)*pi
    } else if ((sampled$x[i] - sampled$x[n]) < 0 & (sampled$y[i] - sampled$y[n])==0) {
      angle = pi
    }  
    
    if (distance > 0) {
      #threshold1 = (abs(iangle - angle)*360)/(2*pi)
      threshold2 = (abs(atan2(sin(angle-iangle), cos(angle-iangle)))*360/(2*pi))
      angledirection = atan2(sin(angle-iangle), cos(angle-iangle))*360/(2*pi)
      
      angleBetween[i-1] <- threshold2
      
      if (threshold2 > delta) {
        
        record = record + 1
        p_n = n
        n = i-1
        p_angle = iangle
        
        #Calculate heading of next initial vector
        idistance = sqrt((sampled$x[i] - sampled$x[n])**2 + (sampled$y[i] - sampled$y[n])**2) 
        iangle = acos((sampled$x[i]-sampled$x[n])/idistance)
        if ((sampled$x[i] - sampled$x[n]) > 0 & (sampled$y[i] - sampled$y[n]) < 0) {
          iangle = 2*pi - iangle
        } else if ((sampled$x[i] - sampled$x[n]) < 0 & (sampled$y[i] - sampled$y[n]) < 0) {
          #angle = 180 + (180-angle)
          iangle = 2*pi - iangle
        } else if ((sampled$x[i] - sampled$x[n])==0 & (sampled$y[i] - sampled$y[n]) < 0) {
          iangle = (3/2)*pi
        } else if ((sampled$x[i] - sampled$x[n]) < 0 & (sampled$y[i] - sampled$y[n])==0) {
          iangle = pi
        }  
        
        #Calculate step length
        step.length = sqrt((sampled$x[n] - sampled$x[p_n])**2 + (sampled$y[n] - sampled$y[p_n])**2)
        
        #Calculate turn direction
        heading = acos((sampled$x[n]-sampled$x[p_n])/step.length)
        if ((sampled$x[n] - sampled$x[p_n]) > 0 & (sampled$y[n] - sampled$y[p_n]) < 0) {
          heading = 2*pi - heading
        } else if ((sampled$x[n] - sampled$x[p_n]) < 0 & (sampled$y[n] - sampled$y[p_n]) < 0) {
          #angle = 180 + (180-angle)
          heading = 2*pi - heading
        } else if ((sampled$x[n] - sampled$x[p_n])==0 & (sampled$y[n] - sampled$y[p_n]) < 0) {
          heading = (3/2)*pi
        } else if ((sampled$x[n] - sampled$x[p_n]) < 0 & (sampled$y[n] - sampled$y[p_n])==0) {
          heading = pi
        }  
        turn.angle = atan2(sin(heading - iheading), cos(heading - iheading))*360/(2*pi)
        
        output3[record, 1] = sampled$Frame.Num[n]
        output3[record, 2] = sampled$Time.Elapsed[n]
        output3[record, 3] = sampled$Time.Since.Last.Frame[n]
        output3[record, 4] = sampled$x[p_n]
        output3[record, 5] = sampled$y[p_n]
        output3[record, 6] = sampled$x[n]
        output3[record, 7] = sampled$y[n]
        output3[record, 8] = heading*360/(2*pi)
        output3[record, 9] = threshold2
        output3[record, 10] = turn.angle
        output3[record, 11] = step.length
        output3[record, 12] = sampled$interval[n]
        output3[record, 13] = sampled$speed[n]
        
        iheading = heading
        
      }
      i <- i + 1
    } else {
      i <- i + 1
    }
  } 
  
  turningsampled3 <- as.data.frame(output3)
  turningsampled3 <- turningsampled3[complete.cases(turningsampled3),]
  setnames(turningsampled3, old = c('V1','V2','V3','V4','V5', 'V6', 'V7', 'V8', 'V9', 'V10', 'V11', 'V12', 'V13'), 
           new = c('Frame.Num','Time.Elapsed', 'Time.Since.Last.Frame', 'x_start','y_start', 'x_end', 'y_end',
                   'Heading', 'threshold', 'turn.angle', 'step.length', 'interval', 'speed' ))
  
  
  
  
  #Interval Step Length Analysis---------------------------------------------------------------------------------
  
  #for turningsampled3
  
  step.length.intervals <- data.frame(intervalnum=numeric(), 
                                      lower=numeric(), 
                                      upper=numeric(),
                                      mean=numeric(), stringsAsFactors=FALSE) 
  
  for (i in 1:max(turningsampled3$interval)) {
    interval <- subset(turningsampled3, turningsampled3$interval == i)
    interval2 <- subset(sampled, sampled$interval == i)
    if (nrow(interval) > 0) {
      #hist(interval$step.length, na.rm = TRUE)
      #boxplot(interval$step.length)   
      lower = 0
      max.step.length = max(interval$step.length)
      avg.step.length = mean(interval$step.length)
      avg.step.length.mm = avg.step.length/70 
      med.step.length = median(interval$step.length)
      sum.step.length = sum(interval$step.length)
      mean.speed = mean(interval2$speed)
      mean.speed.mm = mean.speed/70
      median.speed = median(interval2$speed)
      newrow=c(i, lower, max.step.length, avg.step.length, avg.step.length.mm,  
               med.step.length, sum.step.length, mean.speed, mean.speed.mm, median.speed)
      step.length.intervals <- rbind(step.length.intervals, newrow) 
    } 
  }
  
  colnames(step.length.intervals) <- c("intervalnum", "lower", "upper", "mean", "mean.mm", "median", "sum", 
                                       "mean.speed", "mean.speed.mm", "median.speed")
  
  
  
  #-------------------------------------------------------------------------------#
  #              Proximity Ratio Exploration and Plotting figures                 #
  #-------------------------------------------------------------------------------#
  
  # Plot location using time interval of 4 minutes
  time.interval <- 4
  sampled.interval <- subset(sampled, sampled$interval == time.interval)
  colfunc <- colorRampPalette(c("red", "blue"))
  filename <- paste(v_folders_name,"Location_using_time_interval_of_4_minutes.jpg",sep="_")
  
  jpeg(file = file.path(output_fig_path,filename),width = 10, height = 10, units = 'in', res = 400)
  plot(sampled.interval$x,
       sampled.interval$y,col=colfunc(nrow(sampled.interval)),pch=19,
       xlab= "x-coordinates", ylab= "y-coordinates")
  title(main="Location using time interval of 4 minutes", font.main = 4)
  dev.off()
  
  # Plot location from 90th to 120th seconds
  start.time <- 90
  end.time <- 120
  colfunc <- colorRampPalette(c("red", "blue"))
  filename <- paste(v_folders_name,"Location_from_90th_to_120th_seconds.jpg",sep="_")
  
  jpeg(file = file.path(output_fig_path, filename),width = 10, height = 10, units = 'in', res = 400)
  plot(sampled$x[sampled$Time.Elapsed > start.time & sampled$Time.Elapsed <= end.time],
       sampled$y[sampled$Time.Elapsed > start.time & sampled$Time.Elapsed <= end.time],
       col=colfunc(end.time-start.time),pch=19, 
       xlab = 'x-coordinate', ylab='y-coordinate')
  title(main="Location from 90th to 120th seconds",  font.main = 4)
  dev.off()
  
  
  # Plot location from 60th to 120th seconds
  start.time <-60
  end.time <- 120
  sampled.interval <- subset(n2_f5, n2_f5$Time.Elapsed > start.time & n2_f5$Time.Elapsed <= end.time)
  colfunc <- colorRampPalette(c("red", "blue"))
  
  filename <- paste(v_folders_name,'Location_from_60th_to_120th_seconds.jpg',sep="_")
  
  jpeg(file = file.path(output_fig_path, filename),width = 10, height = 10, units = 'in', res = 400)
  plot(n2_f5$x[n2_f5$Time.Elapsed > start.time & n2_f5$Time.Elapsed <= end.time],
       n2_f5$y[n2_f5$Time.Elapsed > start.time & n2_f5$Time.Elapsed <= end.time],
       col=colfunc(nrow(sampled.interval)),pch=19,
       xlab = 'x-coordinate', ylab='y-coordinate')
  title(main = 'Location from 60th to 120th seconds', font.main = 4)
  dev.off()
  
  
  xlim <- range(step.length.intervals$intervalnum)
  ylim <- range(step.length.intervals$mean.mm)
  
  filename <- paste(v_folders_name,"Mean_Step_Length.jpg",sep="_")
  
  jpeg(file = file.path(output_fig_path,filename),width = 10, height = 10, units = 'in', res = 400)
  plot(step.length.intervals$intervalnum, step.length.intervals$mean.mm, xlab="Time Elapsed (1 unit = 1 minute)", 
       ylab="Mean Step Lengths (1 unit = 1 mm)",
       type="b", col="red", pch = 19, xlim=xlim, ylim=ylim)
  
  points(step.length.intervals$intervalnum, step.length.intervals$mean.mm, type = "p", col="blue", pch = 19, xlim=xlim, ylim=ylim)
  
  title(main = "Mean Step Length", font.main = 4)
  dev.off()
  
  
  
  #-----------------------------------------------------------------------------------#
  #                        CELL OCCUPANCY DATA                                        #
  #-----------------------------------------------------------------------------------#
  
  library(data.table)
  
  #Create Occupancy File with Appropriate Intervals for Merging with step.length.interval data.frame
  occ_path <- file.path(medix_path, file.path(v_folders_name,"data/occupancy.csv"))
  occ <- read.csv(occ_path, header=FALSE)
  setnames(occ, old = c('V1','V2'), 
           new = c('Time.Elapsed', 'cells.visited'))
  
  occ$Time.Elapsed <- as.numeric(occ$Time.Elapsed)
  occ$cells.visited <- as.numeric(occ$cells.visited)
  
  occ$mean.occupancy <- NA
  occ$median.occupancy <- NA
  int.length <- 1
  count <- 1
  occ.vector <- vector()
  iterations <- (nrow(occ) - (nrow(occ)%%int.length)) + 1
  #print(iterations)
  
  for (i in 1:iterations) {
    if (count <= int.length) {
      occ.vector <- c(occ.vector, occ$cells.visited[i])
      #print(occ.vector)
      count <- count + 1
    } else {
      occ$mean.occupancy[i-1] <- mean(occ.vector)
      occ$median.occupancy[i-1] <- median(occ.vector)
      count <- 2
      occ.vector <- vector()
      occ.vector <- c(occ.vector, occ$cells.visited[i])
      #print(i)
    } 
  } 
  
  if (nrow(occ)%%int.length != 0) {
    occ.vector <- vector()
    for (i in iterations:nrow(occ)) {
      occ.vector <- c(occ.vector, occ$cells.visited[i])
    }
    occ$mean.occupancy[nrow(occ)] <- mean(occ.vector)
    occ$median.occupancy[nrow(occ)] <- median(occ.vector)
  }
  
  occ <- occ[complete.cases(occ),] #subset complete cases
  
  for (i in 1:nrow(occ)) { #creates interval number 
    occ$intervalnum[i] <- i
  }
  
  #Merge step.length.intervals with occ
  
  occ.step.length <- merge(step.length.intervals, occ, by = "intervalnum")
  
  for (i in 1:nrow(occ.step.length)) { #For 1 minute, only mean
    occ.step.length$sum.occ.ratio.mean[i] <- occ.step.length$sum[i]/occ.step.length$mean.occupancy[i]
    occ.step.length$sum.occ.ratio.median[i] <- occ.step.length$sum[i]/occ.step.length$median.occupancy[i]
    occ.step.length$speed.occ.ratio.meanspeed[i] <- occ.step.length$mean.speed[i]/occ.step.length$mean.occupancy[i]
    occ.step.length$speed.occ.ratio.meanspeed.mm[i] <- occ.step.length$mean.speed.mm[i]/occ.step.length$mean.occupancy[i]
    occ.step.length$speed.occ.ratio.medianspeed[i] <- occ.step.length$median.speed[i]/occ.step.length$mean.occupancy[i]
    occ.step.length$search.efficiency[i] <- occ.step.length$mean.occupancy[i]/occ.step.length$mean.speed[i]
  }
  
  filename <- paste(v_folders_name,'Proximity_Ratio.jpg',sep="_")
  
  jpeg(file = file.path(output_fig_path,filename),width = 10, height = 10, units = 'in', res = 400)
  plot(occ.step.length$intervalnum, occ.step.length$speed.occ.ratio.meanspeed, 
       pch = 19,type = 'o', col = 'blue',
       xlab = "Time Elapsed (minutes)",ylab = "Proximity Ratio (E[V]/Occupancy)")
  
  title(main='Proximity Ratio', font.main = 4)
  dev.off()
  
  keep <- c("intervalnum", "speed.occ.ratio.meanspeed.mm")
  #write.xlsx(occ.step.length[keep], "//medixsrv/Workspace/Max/Worm Project/Proximity Ratio Exploration/n2nf17_E[v].xlsx") 
  
  
  
  #-------------------------------------------------------------------------------#
  #                 Write analysis statictis to files:                            #
  #-------------------------------------------------------------------------------#
  
  write.csv(step.length.intervals, file.path(analyis_files_path, "steplength_interval.csv"), row.names=FALSE)
  write.csv(turningsampled3, file.path(analyis_files_path, "turningAngle_headings.csv"), row.names=FALSE)
  write.csv(occ.step.length, file.path(analyis_files_path, "Proximity_analysis.csv"), row.names=FALSE)
  write.csv(sampled, file.path(analyis_files_path, "sampled.csv"), row.names=FALSE)
  
  
  rm(list=ls())
}# END traverseDir function

#-------------------------------------------------------------------------------------------------------

createMasterFiles <- function(inFile_path, outFile_path,type, delta = 10 ){
  #' Generate statistics data file for each video file
  #' @Dependency: run VIDA.MEDIX.R first
     #' angle calculation methods is using angle between vectors
     #' NOTE: THIS FUNCTION REQUIRE THERE ARE 2 file sitting on the server: "movementFeatures.csv and occupancy.csv"
     #'      THESE 2 FILES MUST BE AVAILABLE TO PROCESS
     #' @param inFile_path: input path that contain the movementFeatures.csv and occupancy.csv file
     #' @param outFile_path: wirte files to output path.
     #' @param type: a string represent a type of worm
     #' @param delta: a numeric value of angel threshold
     #' @return: NONE, write file to output
     #' 
     #' @example: # replace evrything inside "<"">" to your legitimate value
     #' set.wd("C://Users/<username>/Desktop")
     #' dir.create("Analysis file") # this will  create a new folder at "C://Users/<username>/Desktop/Analysis file" 
     #' outFile_path <- "C://Users/<username>/Desktop/Analysis file" # you can choose any out put destination as long as IT IS EXISTED IN YOUR MACHINE
     #' inFile_path <- "<path/to/medix/server>"
     #' 
     #' createMasterFiles(inFile_path, outfile_path, "N2_f", delta = 60) # this will create analisys files for "N2_f" worm with angle threshold of 60 degree
     #' createMasterFiles(inFile_path, outfile_path, "N2_nf", delta = 5) # this will create analisys files for "N2_nf" worm with angle threshold of 5 degree
     #' createMasterFiles(inFile_path, outfile_path, "N2_nnf", delta = 40) # this will create analisys files for "N2_nnf" worm with angle threshold of 40 degree
     #' createMasterFiles(inFile_path, outfile_path, "tph1_f", delta = 30) # this will create analisys files for "tph1_f" worm with angle threshold of 30 degree
     #' createMasterFiles(inFile_path, outfile_path, "tph1_nf", delta = 170) # this will create analisys files for "tph1_nf" worm with angle threshold of 170 degree
     
     
     options(scipen=999) # disable scientific notation
     require(data.table)
     #require(VIDA.MEDIX)
     #check if files are stayed in Medix server
     # Otherwise assuming that the file is locate directly in the inFile_path
     if( grepl("movementFeatures.csv|occupancy.csv",inFile_path)){
       movementFeafture  <- joinPaths(c(inFile_path,"movementFeatures.csv"))
       occFile <- joinPaths(c(inFile_path,"occupancy.csv"))  
     }else{
       movementFeafture  <- joinPaths(c(inFile_path,"data","movementFeatures.csv"))
       occFile <- joinPaths(c(inFile_path,"data","occupancy.csv"))
     }
     df.movement.read <- read.csv(movementFeafture, header=FALSE)
     setnames(df.movement.read, old = c('V1','V2','V3','V4','V5','V6','V7','V8','V9'), new = c('Frame.Num','Time.Elapsed', 'Time.Since.Last.Frame', 'x','y','Speed','Accelerations','Angle','Angular.Velocity'))
     
     df.occ.read <- read.csv(occFile, header = FALSE)
     setnames(df.occ.read, old=c("V1","V2"), new=c('Time.Elapsed', 'cells.visited'))
     
     # Select sample time in 1 second 
     df.sampled <- df.movement.read[,c("Frame.Num","Time.Elapsed","x","y")]
     filter <- frameSelection(df.sampled$Time.Elapsed)
     df.sampled <- df.sampled[filter,]
     
     #Delete rows where (x,y) coordinates are the same consecutively
     filter <- isMoved(df.sampled$x,df.sampled$y)
     df.sampled <- df.sampled[filter,]
     rm(filter)
     
     # Remove redundan NA rows
     df.sampled <- df.sampled[complete.cases(df.sampled),]
     
     #--------------------------------------------------------------------#
     # create new data frame that contains vector angle from 3 consecutive points
     df.sampled.rows <- nrow(df.sampled)
     
     df.sampled.angle.vector <- df.sampled[2:df.sampled.rows,c("Frame.Num","Time.Elapsed")]
     df.sampled.angle.vector$x_start  <- df.sampled[1:df.sampled.rows - 1, "x"]
     df.sampled.angle.vector$y_start  <- df.sampled[1:df.sampled.rows - 1, "y"]
     df.sampled.angle.vector$x_end  <- df.sampled[2:df.sampled.rows, "x"]
     df.sampled.angle.vector$y_end  <- df.sampled[2:df.sampled.rows, "y"]
     
     rm(df.sampled.rows)
     
     
     df.sampled.angle.vector.rows <- nrow(df.sampled.angle.vector)
     angle <- mapply(function (a2,a1,b2,b1,c2,c1,d2,d1) { 
       v1 = c((a2-a1),(b2-b1))
       v2 = c((c2-c1),(d2-d1))
       angleBetween(v1 , v2) } , 
       df.sampled.angle.vector$x_end[1:df.sampled.angle.vector.rows-1], df.sampled.angle.vector$x_start[1:df.sampled.angle.vector.rows-1] , 
       df.sampled.angle.vector$y_end[1:df.sampled.angle.vector.rows-1], df.sampled.angle.vector$y_start[1:df.sampled.angle.vector.rows-1] ,
       df.sampled.angle.vector$x_end[2:df.sampled.angle.vector.rows], df.sampled.angle.vector$x_start[2:df.sampled.angle.vector.rows] , 
       df.sampled.angle.vector$y_end[2:df.sampled.angle.vector.rows], df.sampled.angle.vector$y_start[2:df.sampled.angle.vector.rows] )
     
     rm(df.sampled.angle.vector.rows)
     
     df.sampled.angle.vector$Angle.between.2.vectors <-c(0,angle)
     rm(angle)
     
     #delta <- 10
     df.sampled.angle.vector$Reversal <- df.sampled.angle.vector[,"Angle.between.2.vectors"] > 90
     
     #--------------------------------------------------------------
     # Calcuate angle between steplength and next 2 line segment (next vector)
     angle.between.step.length.and.movement.vectors <- c(0)
     rowsFilter  <-  c(1)
     
     x <- df.sampled$x
     y <- df.sampled$y
     rows <- nrow(df.sampled)
     p_i <- 1
     n_i  <- 2
     
     for(i in 2:rows-1){
       heading.vec <- c( ( x[n_i] - x[p_i]), (y[n_i] - y [p_i]) )    
       next.vec  <- c( (x[i+1] - x[i]), (y[i+1] - y[i]) )
       angle  <- angleBetween(heading.vec, next.vec)
       if(is.na(angle)){
         next
       }
       if (angle > delta) {
         p_i  <-  i
         n_i  <-  i+1
         angle.between.step.length.and.movement.vectors <- c(angle.between.step.length.and.movement.vectors,angle)
         rowsFilter <- c(rowsFilter,i) # filter the index in df
       }
     }
     rm(n_i,p_i,rows,x,y,i,heading.vec,next.vec,angle)
     
     df.step.length.angle <- data.frame(Frame.num = df.sampled$Frame.Num[rowsFilter],
                                        Time.Elapsed = df.sampled$Time.Elapsed[rowsFilter],
                                        x = df.sampled$x[rowsFilter],
                                        y = df.sampled$y[rowsFilter],
                                        Angle.between.steplength.and.movement.vectors = angle.between.step.length.and.movement.vectors,
                                        stringsAsFactors=FALSE)
     rm(rowsFilter, angle.between.step.length.and.movement.vectors)
     
     #---------------- Angle between 2 steplength ------
     
     rows <- nrow(df.step.length.angle)
     
     angle.between.2.step.length <- mapply(function (a2,a1,b2,b1,c2,c1,d2,d1) { 
       v1 = c((a2-a1),(b2-b1))
       v2 = c((c2-c1),(d2-d1))
       angleBetween(v1 , v2) } , 
       df.step.length.angle$x[1:(rows-2)], df.step.length.angle$x[1:(rows-2)] , 
       df.step.length.angle$y[1:(rows-2)], df.step.length.angle$y[2:(rows-1)] ,
       df.step.length.angle$x[3:(rows)], df.step.length.angle$x[2:(rows-1)] , 
       df.step.length.angle$y[3:(rows)], df.step.length.angle$y[2:(rows-1)] )
     
     df.step.length.angle$Angle.between.2.step.lengths <- c(c(0),c(angle.between.2.step.length,c(0)))
     #---------------------------------------------------------------------------------------------------#
     # Add more attribute to data "turningAngle.csv"
     df.step.length.angle$Step.length.in.pixel <- stepLength(df.step.length.angle$x,df.step.length.angle$y)
     df.step.length.angle$Step.length.in.mm <- stepLength(df.step.length.angle$x,df.step.length.angle$y, unit="mm")
     df.step.length.angle$Duration  <- duration(df.step.length.angle$Time.Elapsed)
     df.step.length.angle$Speed.in.mm.per.sec  <- speed(df.step.length.angle$x,df.step.length.angle$y,df.step.length.angle$Time.Elapsed)
     df.step.length.angle$Reversal  <- df.step.length.angle$Angle.between.steplength.and.movement.vectors > 90
     df.step.length.angle$Interval  <- intervalMarker(lst = df.step.length.angle$Time.Elapsed)
     
     # group df by the same interval and aggregate data and generate basic statistics data
     iterFactors <- max(df.step.length.angle$Interval)
     
     df.step.length.angle.aggregate = data.frame(Interval = numeric(), 
                                                 Mean.step.length.in.Pixel = numeric(),
                                                 Mean.step.length.in.mm = numeric(),
                                                 Std.step.length.in.mm = numeric(),
                                                 Median.step.length = numeric(),
                                                 Min.step.length = numeric(),
                                                 Max.step.length = numeric(),
                                                 Mean.speed.mm.per.sec = numeric(),
                                                 Median.speed = numeric(),
                                                 Min.speed = numeric(),
                                                 Max.speed = numeric(),
                                                 Mean.angle.between.step.length = numeric(),
                                                 Reversal.count = numeric(),
                                                 stringsAsFactors=FALSE)
     for(i in seq(iterFactors)){
       df = subset(df.step.length.angle, df.step.length.angle$Interval == i)
       mean.sl.pixel   <- mean(df$Step.length.in.pixel, na.rm = TRUE)
       mean.sl.mm      <- mean(df$Step.length.in.mm, na.rm = TRUE)
       sd.sl.mm        <- sd(df$Step.length.in.mm, na.rm = TRUE)
       median.sl       <- median(df$Step.length.in.mm, na.rm = TRUE)
       min.sl          <- min(df$Step.length.in.mm, na.rm = TRUE)
       max.sl          <- max(df$Step.length.in.mm, na.rm = TRUE)
       
       mean.speed      <- mean(df$Speed.in.mm.per.sec, na.rm = TRUE)
       median.speed    <- median(df$Speed.in.mm.per.sec, na.rm = TRUE)
       min.speed       <- min(df$Speed.in.mm.per.sec, na.rm = TRUE)
       max.speed       <- max(df$Speed.in.mm.per.sec, na.rm = TRUE)
       mean.angle      <- mean(na.omit(df$Angle.between.2.step.lengths))
       reversal.count  <- sum(df$Reversal, na.rm = TRUE)
       newrow <- c(i,mean.sl.pixel, mean.sl.mm, sd.sl.mm, median.sl,min.sl, max.sl,
                   mean.speed, median.speed, min.speed, max.speed, mean.angle, reversal.count)
       df.step.length.angle.aggregate<- rbind(df.step.length.angle.aggregate,newrow)
       
       #rm(i,iterFactors,reversal.count,newrow,mean.sl.pixel,mean.sl.mm,median.sl,min.sl,max.sl,mean.speed,median.speed, min.speed, max.speed)
     }
     colnames(df.step.length.angle.aggregate) <- c("Interval","Mean.step.length.in.pixel","Mean.step.length.in.mm","Std.step.length.in.mm","Median.step.length","Min.step.length","Max.step.length",
                                                   "Mean.speed","Median.speed","Min.speed","Max.speed","Mean.angle.between.step.length","Reversal.count")
     
     #------------------------------------------------------------------#
     df.occ.read$Interval <- as.integer(df.occ.read$Time.Elapsed/60)
     
     # interval in occupancy file and movementFeature is somtimes different
     # So we need to check for missing data to properly merge them
     
     interval.occ <- max(df.occ.read$Interval)
     interval.sl <- max(df.step.length.angle.aggregate$Interval)
     if(interval.occ<interval.sl){
       for (i in 1:(interval.sl-interval.occ)){
         columns <- ncol(df.occ.read)
         df.occ.read<- rbind(df.occ.read,rep(NA,columns))
       }
     }else{
       df.occ.read <- df.occ.read[1:interval.sl,]
     }
     
     
     df.tmp <- df.step.length.angle.aggregate[-1]/df.occ.read$cells.visited
     colnames(df.tmp)  <- c("Mean.sl.pixel.over.occ","Mean.sl.mm.over.occ","Std.sl.mm.over.occ","Median.sl.mm.over.occ","Min.sl.mm.over.occ","Max.sl.mm.over.occ",
                            "Mean.speed.mm.over.occ","Median.speed.mm.over.occ","Min.speed.mm.over.occ","Max.speed.mm.over.occ","Mean.angle.sl.over.occ","Reversal.over.occ")
     df.tmp <- cbind(Interval=df.step.length.angle.aggregate$Interval,df.tmp)
     
     df.proximity <- merge(x = df.step.length.angle.aggregate,y=df.tmp,by="Interval")
     
     
     rm(df.occ.read, interval.occ,interval.sl,df.tmp)
     
     write.csv(x= df.sampled[complete.cases(df.sampled),] , file = joinPaths(c(outFile_path, "sampled.csv")), row.names=FALSE)
     write.csv(x=df.step.length.angle[complete.cases(df.step.length.angle),],file= file.path(outFile_path, "turningAngle.csv"), row.names=FALSE)
     write.csv(x=df.proximity[complete.cases(df.proximity),], file = file.path(outFile_path, "Proximity_analysis.csv"), row.names=FALSE)
     write.csv(x=df.step.length.angle.aggregate[complete.cases(df.step.length.angle.aggregate),], file =  file.path(outFile_path, "angle_aggregate.csv"), row.names=FALSE)
     write.csv(x = df.sampled.angle.vector[complete.cases(df.sampled.angle.vector),],file =  file.path(outFile_path, "angle_between_2vectors.csv"), row.names=FALSE)
     
     # -------------------------------------------------------------------------------------
     #                                   Plot step length
     require(ggplot2)
     require(grid)
     require(gridExtra)
     x = df.proximity$Interval
     y = df.proximity$Mean.step.length.in.mm
     y.proximity = df.proximity$Mean.sl.mm.over.occ
     
     xlimits = range(0, max(x))
     ylimits= range(0, ceiling(max(y, na.rm = TRUE)))
     
     xbreaks = seq(0, max(x), by = 5)
     ybreaks = seq(0,ceiling(max(y, na.rm = TRUE)), by = 0.1)
     
     g1 <- ggplot(df.step.length.angle.aggregate, environment = environment()) +
       
       geom_line(aes(x = x, y = y), color = 'red') +
       geom_point(aes(x = x, y = y), size = 3, color = 'blue', shape = 19) +
       
       scale_x_continuous(breaks = xbreaks, name= "") +
       scale_y_continuous(breaks = ybreaks, name= "\nMean step length in mm\n") +
       expand_limits(y= ylimits, x = xlimits) +
       
       ggtitle(paste(type,"mean step length") ) +
       theme( plot.title=element_text(size = 40, face = "bold"),
              axis.title = element_text(size=28),
              axis.text = element_text(size = 14),
              legend.text = element_text(size = 20), 
              legend.key.size= unit(3,"cm") )
     
     
     
     y.proximity = df.proximity$Mean.sl.mm.over.occ
     
     xlimits = range(0, max(x))
     ylimits= range(0, max(y.proximity, na.rm = TRUE)*1.1)
     
     xbreaks = seq(0, max(x), by = 5)
     ybreaks = seq(0,max(y.proximity, na.rm = TRUE)*1.1, by = 0.02)
     
     g2 <- ggplot(df.step.length.angle.aggregate, environment = environment()) +
       
       geom_line(aes(x = x, y = y.proximity), color = 'blue') +
       geom_point(aes(x = x, y = y.proximity), size = 3, color = 'red', shape = 17) +
       
       
       scale_x_continuous(breaks = xbreaks, name= "") +
       scale_y_continuous(breaks = ybreaks, name= "\nMean step length over cell occupancy\n") +
       expand_limits(y= ylimits, x = xlimits) +
       
       ggtitle(paste(type,"proximity") ) +
       theme( plot.title=element_text(size = 40, face = "bold"),
              axis.title = element_text(size=28),
              axis.text = element_text(size = 14),
              legend.text = element_text(size = 20), 
              legend.key.size= unit(3,"cm") )
     
     
     y.reversal = df.proximity$Reversal.count
     
     xlimits = range(0, max(x))
     ylimits= range(0, ceiling(max(y.reversal, na.rm = TRUE)))
     
     xbreaks = seq(0, max(x), by = 5)
     ybreaks = seq(0,ceiling(max(y.reversal, na.rm = TRUE)), by = 1)
     
     g3 <- ggplot(df.step.length.angle.aggregate, environment = environment()) +
       
       geom_line(aes(x = x, y = y.reversal), color = 'green4') +
       geom_point(aes(x = x, y = y.reversal), size = 3, color = 'black', shape = 15) +
       
       
       scale_x_continuous(breaks = xbreaks, name= "\nTime (unit = 1 minute)") +
       scale_y_continuous(breaks = ybreaks, name= "\nNumber of reversal\n") +
       expand_limits(y= ylimits, x = xlimits) +
       
       ggtitle(paste(type,"reversal") ) +
       theme( plot.title=element_text(size = 40, face = "bold"),
              axis.title = element_text(size=28),
              axis.text = element_text(size = 14),
              legend.text = element_text(size = 20), 
              legend.key.size= unit(3,"cm") )
     
     
     
     fileName = joinPaths(c(outFile_path, paste(type,"mean_step_length_proximity_reversal.png",sep="_") ) )
     
     jpeg(filename = fileName, width=60, height=60,units= "cm", res=300)
     grid.arrange(g1,g2,g3,nrow=3)
     dev.off()
     print(fileName)
     
     # Plot angle histogram
     angle.between.sl <- df.step.length.angle$Angle.between.2.step.lengths
     
     ggplot(data= df.step.length.angle,aes(x=Angle.between.2.step.lengths)) + 
       geom_histogram(aes(y=..density.., fill = ..count..), alpha = 0.7, binwidth= 0.5) + 
       geom_density() +
       ggtitle(paste(type,"histogram")) +
       theme( plot.title=element_text(size = 40, face = "bold"),
              axis.title = element_text(size=28),
              axis.text = element_text(size = 14),
              legend.text = element_text(size = 20), 
              legend.key.size= unit(1,"cm") )
     
     fileName = joinPaths(c(outFile_path, paste(type,"Angle between step length.tiff",sep="_") ) )
     ggsave(fileName, width=20, height=20, dpi=300)
     print(fileName)
     # -------------------------------------------------------------------------------------
     
}# End createMasterFiles funciton


createAllFilesFromAllVideos <- function(inPath = "//medixsrv/Nematodes/data", outPath, wormList,delta)
{
  #' Generate statistics data file for ALL video file AT ONCE
  #' @Dependency: run VIDA.MEDIX.R first
     #' NOTE: THIS FUNCTION REQUIRE THERE ARE 2 file sitting on the server: "movementFeatures.csv and occupancy.csv"
     #'      THESE 2 FILES MUST BE AVAILABLE TO PROCESS
     #' @param inPath: input path that contain the movementFeatures.csv and occupancy.csv file
     #' @param outPath: wirte files to output path.
     #' @param type: a string represent a type of worm
     #' @param delta: a numeric value of angel threshold
     #' @return: NONE, write file to output
     #' if  
     #' @example: # replace evrything inside "<"">" to your legitimate value
     #' outPath <- "C://Users/htran15/Desktop/40Test"
     #' createAllFilesFromAllVideos(inPath ="//medixsrv/Nematodes/data",outPath = outPath, wormList = c("n2_f","n2_nf","n2_nnf","tph1_f","tph1_nf"), delta = 40 )
     
     data_folders <- list.files("//medixsrv/Nematodes/data")
     sapply(wormList, function(y)
     {
       v_folders  <- data_folders[grepl(pattern = y,x=data_folders,ignore.case=TRUE)]  
       
       sapply(v_folders,function(x){
         inFile_path <- paste("//medixsrv/Nematodes/data",x, sep="/")
         outFile_path  <- paste(outPath, x, sep="/")
         dir.create(path=outFile_path, showWarnings = FALSE)
         createMasterFiles(inFile_path,outFile_path, x, delta = delta)
       })
     })
}

