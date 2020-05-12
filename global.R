library(stringdist)
library(dplyr)
library(base)
#guidelines <- read.csv("Guidelines.csv",stringsAsFactors=F,header=T)
longitude <- c("longitude","Longitude","x","X","MonitoringLocationLongitude")
latitude <- c("latitude","Latitude","y","Y","MonitoringLocationLatitude")
date <- c("Date","date","Datetime","ActivityStartDate","DATE_TIME_HEURE","Date_Time")
site <- c("MonitoringLocationName","Site_Code","Site_Name","Location_Name","Site","Station","Location")
parameters <- c("CharacteristicName","ParameterName","VARIABLE","Variable","Parameter_Name","VARIABLE_NAME","Variable_Name")
results <- c("ResultValue","Value","MeasurementValue","VALUE","VALUE_VALEUR","Result")
#gdf <- read.csv("Guidelines.csv",stringsAsFactors=F)
#guidepars <- c((t(guidelines$Name)))
gyc <- function(data){
  prevy <- data.frame(stringsim(names(data),latitude[1]))
  for (i in 2:length(latitude)){
    sname <- latitude[i]
    output <- data.frame(stringsim(names(data),sname))
    if (max(output[,1])> max(prevy[,1])){
      prevy <- output
    }
  }
  latcol <- as.integer(which.max(prevy[,1]))
  return(latcol)
}

gxc <- function(data){
  prevx <- data.frame(stringsim(names(data),longitude[1])) 
  for (i in 2:length(longitude)){
    sname <- longitude[i]
    output <- data.frame(stringsim(names(data),sname))
    if (max(output[,1])> max(prevx[,1])){
      prevx <- output
    }
  }
  loncol <- as.integer(which.max(prevx[,1]))
  return(loncol)
}

gdc <- function(data){
  prevd <- data.frame(stringsim(names(data),date[1]))
  for (i in 2:length(date)){
    sname <- date[i]
    output <- data.frame(stringsim(names(data),sname))
    if (max(output[,1])> max(prevd[,1])){
      prevd <- output
    }
  }
  dacol <- as.integer(which.max(prevd[,1]))
  return(dacol)
}

gsc <- function(data){
  prevn <- data.frame(stringsim(names(data),site[1]))
  for (i in 2:length(site)){
    sname <- site[i]
    output <- data.frame(stringsim(names(data),sname))
    if (max(output[,1])> max(prevn[,1])){
      prevn <- output
    }
  }
  sncol <- as.integer(which.max(prevn[,1]))
  return(sncol)
}

gpc <- function(data){
  prevp <- data.frame(stringsim(names(data),parameters[1]))
  for (i in 2:length(parameters)){
    sname <- parameters[i]
    output <- data.frame(stringsim(names(data),sname))
    if (max(output[,1])> max(prevp[,1])){
      prevp <- output
    }
  }
  pcol <- as.integer(which.max(prevp[,1]))
  return(pcol)
}

grc <- function(data){
  prevp <- data.frame(stringsim(names(data),results[1]))
  for (i in 2:length(results)){
    sname <- results[i]
    output <- data.frame(stringsim(names(data),sname))
    if (max(output[,1])> max(prevp[,1])){
      prevp <- output
    }
  }
  rcol <- as.integer(which.max(prevp[,1]))
  return(rcol)
}
# 
# ccme1 <- function(poi){
#   #pmatch(poi,guidepars,duplicates.ok=TRUE)
#   out <- stringsim(poi,guidepars)
#   m <- which.max(out)
#   return(m)
# }
# 
# 
# getGuideM <- function(poi,dp,df,fi,d,v,a,ph ){
#   
#   gdf2i <- data.frame(subset(gdf,gdf$Name==poi))
#   goTo <- as.integer(gdf2i[1,2])
#   
#   
#   if (goTo==3){ ## Upper limit only avaliable
#     value1 <- as.numeric(gdf2i[1,3])
#     value2 <- NA
#     
#   }
#   
#   if (goTo==4){ ## Lower Limit only avaliable
#     value1 <- as.numeric(gdf2i[1,4])
#     value2 <- NA
#   }
#   
#   if (goTo==5) { ## Upper and Lower limit avaliable, in columns 6 and 7 respectively
#     value1 <- as.numeric(gdf2i[1,6])
#     value2 <- as.numeric(gdf2i[1,7])
#   }
#   
#   list <- list(goTo = "goTo",value1="value1", value2="value2")
#   return(list)
# }



#conditionalPars <- list("CADMIUM_TOTAL_ugL_ST","CADMIUM_TOTAL_ugL_LT")

















css<- HTML("
           #table_id  {
           
           transform:rotateX(180deg);
           }
           #tablet_id {
           transform:rotateX(180deg);
           }
           #DataTables_Table_0_wrapper {
           
           transform:rotateX(180deg);
           }
           
           
           #DataTables_Table_1_wrapper {
           
           transform:rotateX(180deg);
           }
           #DataTables_Table_2_wrapper{
           transform:rotateX(180deg);
           }
           #DataTables_Table_3_wrapper{
           transform:rotateX(180deg);
           }
           #DataTables_Table_4_wrapper{
           transform:rotateX(180deg);
           }
           
           ")



psctest <- function(Rfinal,colr,Tfinal,colt){
  Rfinal <- Rfinal
  Tfinal <- Tfinal
  
  xbarR <- mean(Rfinal[,colr])
  sR <- sd(Rfinal[,colr])
  nR <- nrow(Rfinal)
  
  xbarT <- mean(Tfinal[,colt]) 
  nT <- nrow(Tfinal) 
  
  if (nT > 1){
    sT <- sd(Tfinal[,colt])
    sdP <- sqrt(((nR-1)*sR^2+(nT-1)*sT^2)/(nR+nT-2))
    seP <- sdP*sqrt(1/nR +1/nT)
    Diff <- xbarT - xbarR
    Es <- Diff/sdP ## in standard deviations, this is the critical effect size
    Fobs <- Diff^2/seP^2
    ncp <- (-1*xbarR/sdP+1*xbarT/sdP)^2/(1/nR+1/nT)
  }
  
  else {
    v<- xbarT
    Fobs <- (xbarR - v)^2/(sR*(sqrt(1/nR)))^2
    ncp <- (xbarR/sR)/(1/nR)
  }
  
  
  df1 <- 1
  df2 <- nR + nT - 2
  
  Fc <- qf(0.05,df1,df2,log=FALSE,lower.tail = FALSE) 
  Feq <- qf(0.05,df1,df2,ncp=ncp) ## This is correct when I use the ncp from the excel examples, so the function is right at least
  Fint <- qf(0.95,df1,df2,ncp=ncp) ## This is correct when I use the ncp from the excel examples, so the function is right, at least
  
  ## lower.tail = FALSE : P[X > x] aka 1-CDF aka FINV in Excel...?????? :c
  ## If Fobs > Fc, indicates that difference in var is significantly different from 0
  
  if (Fobs > Fc){
    a <- "The difference between the test and reference datasets is siginificantly different from 0. This means that the datasets do not likely have the same mean."
  }
  else {
    a <- "The difference between the test and reference means is not significanctly different from 0. It is very likely that the datasets have the same mean."
  }
  
  if (Fobs < Feq & Fobs < Fint){
    b <- "The test data likely falls within the normal range of the reference data."
  }
  else if (dplyr::between(Fobs,Feq,Fint)){
    b <- "The test data could be near the edge of the normal range of the reference data, but it likely falls within the normal range of the reference data."
  }
  else if(Fobs > Feq & Fobs > Fint){
    b <- "The test data is likely outside of the normal range of the reference data."
  }
  
  text <- paste(b,"Look at the graph below to see the data being compared. Remember that, for this test to work, the variables and units MUST MATCH.")
  
  return(text)
  
}



