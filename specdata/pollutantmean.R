pollutantmean<-function(directory, pollutant, csvfrom, csvto) {
  
  #Setting Working Directory
directory<-"C:/Users/Nathan/Desktop/specdata"
  
#The CSVs range from 1:332 However one is listed as 001 so I must use sprintf
id<- sprintf('%0.3d', csvfrom: csvto)

#Creating empty dataframe
dataMerge <- data.frame()

#Setting working Directory
setwd(directory)
  #for loop for all the CSVs.
  for ( i in seq_along(id)){

    index<-paste(id[i], ".csv", sep="")
    
    Readin<-read.csv(index)
    dataMerge<-rbind.data.frame(dataMerge, Readin)
}

sulfatemean<-mean(dataMerge$sulfate, na.rm = TRUE)
nitratemean<-mean(dataMerge$nitrate, na.rm = TRUE)

return(c(sulfatemean, nitratemean))

}
    
  mean
    
    mean<-pollutantmean(pollutant= sulfate, csvfrom = 1, csvto=10)
#####################################################################   

    iscomplete<-function(csvfrom, csvto) {
      
      #Setting Working Directory
      directory<-"C:/Users/Nathan/Desktop/specdata"
      
      #The CSVs range from 1:332 However one is listed as 001 so I must use sprintf
      id<- sprintf('%0.3d', csvfrom: csvto)
   
      #Creating empty dataframe
      dataMerge <- data.frame(matrix(ncol = 2, nrow = 0))
      x <- c("idnumber", "complete")
         colnames(dataMerge) <- x
      
      #Setting working Directory
      setwd(directory)
      #for loop for all the CSVs.
      for ( i in seq_along(id)){
        
        index<-paste(id[i], ".csv", sep="")
        
        Readin<-read.csv(index)
        
        dataMerge[i,1]<-id[i]
        dataMerge[i,2]<-sum(complete.cases(Readin))
      
      }
      
      
      
      return(dataMerge)
             
    }
    
    set.seed(42)
    cases<-iscomplete(csvfrom = 54, csvto=54)
    use<-sample(332,10)
    print(cases[use, "nobs"])
##################   
    correlation<-function(csvfrom, csvto, threshold) {
      
      #Setting Working Directory
      directory<-"C:/Users/Nathan/Desktop/specdata"
      
      #The CSVs range from 1:332 However one is listed as 001 so I must use sprintf
      id<- sprintf('%0.3d', csvfrom: csvto)
      
      #Creating empty dataframe
      dataMerge <- data.frame(matrix(ncol = 2, nrow = 0))
      x <- c("idnumber", "complete")
      colnames(dataMerge) <- x
 
      #Setting working Directory
      setwd(directory)
      #for loop for all the CSVs.
      for ( i in seq_along(id)){
        
        index<-paste(id[i], ".csv", sep="")
        
        Readin<-read.csv(index)
        
        dataMerge[i,1]<-id[i]
       if (sum(complete.cases(Readin)) > threshold) {dataMerge[i,2]<-cor(Readin$sulfate,Readin$nitrate, use = "pairwise.complete.obs")}
       else {dataMerge[i,2]<-NA}
        
      }
      
      
  
      return(dataMerge)
      
    }
    
    
    correlation<-correlation(csvfrom = 332, csvto=1, threshold=0)
    correlation<-sort(correlation)
    
########################################
    setwd("C:/Users/Nathan/Desktop/specdata") 
    df<-read.csv("001.csv")
    
   cor(df$sulfate,df$nitrate, use = "pairwise.complete.obs")
 
    