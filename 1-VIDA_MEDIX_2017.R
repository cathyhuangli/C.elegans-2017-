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



createMasterFiles <- function(inFile_path, outFile_path,type){
 
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
     
     #require(VIDA.MEDIX)
     
     cak  <- joinPaths(c(inFile_path,"matlab","ContourAndSkel.csv"))
     if (file.exists(cak)){
       df.ContourAndSkel.read <- read.csv(cak, header = TRUE)
       write.csv(x=df.ContourAndSkel.read, file = joinPaths(c(outFile_path, "ContourAndSkel.csv")), row.names=TRUE)
       
     }
     af  <- joinPaths(c(inFile_path,"matlab","AllFeatures.csv"))
     if (file.exists(af)){
       df.AllFeatures.read <- read.csv(af, header = TRUE)
       write.csv(x=df.AllFeatures.read,file= file.path(outFile_path, "AllFeatures.csv"), row.names=TRUE)
       }
}# End createMasterFiles funciton


createAllFilesFromAllVideos <- function(inPath = "//Cdm-medixsrv/Nematodes/data", outPath, wormList)
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
     #' createAllFilesFromAllVideos(inPath ="//Cdm-medixsrv/Nematodes/data",outPath = outPath, wormList = c("n2_f","n2_nf","n2_nnf","tph1_f","tph1_nf"), delta = 40 )
     
     data_folders <- list.files("//Cdm-medixsrv/Nematodes/data")
     sapply(wormList, function(y)
     {
       v_folders  <- data_folders[grepl(pattern = y,x=data_folders,ignore.case=TRUE)]  
       
       sapply(v_folders,function(x){
         inFile_path <- paste("//Cdm-medixsrv/Nematodes/data",x, sep="/")
         outFile_path  <- paste(outPath, x, sep="/")
         dir.create(path=outFile_path, showWarnings = FALSE)
         createMasterFiles(inFile_path,outFile_path, x)
       })
     })
}

