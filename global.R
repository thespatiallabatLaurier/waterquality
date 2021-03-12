library(stringdist)
library(dplyr)
library(base)
library(shinycssloaders)
library(shinyalert)
#guidelines <- read.csv("Guidelines.csv",stringsAsFactors=F,header=T)
longitude <- c("LONGITUDE","Longitude","longitude","x","X","MonitoringLocationLongitude")
latitude <- c("LATITUDE","Latitude","latitude","y","Y","MonitoringLocationLatitude")
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

##testing
# setwd("C:/Users/Annie/Desktop")
# Rfinal <- read.csv("RefData.csv",header = FALSE)
# Tfinal <- read.csv("TestData.csv",header=FALSE)
# colr <- 1
# colt <- 1
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
    #ncp <- (-1*xbarR/sdP+1*xbarT/sdP)^2/(1/nR+1/nT)
    ncp <- (((-1*0)/1)+((1*2)/1))^2/((-1)^2/nR+1^2/nT)
  }
  
  else {
    v<- xbarT
    Fobs <- (xbarR - v)^2/(sR*(sqrt(1/nR)))^2
    ncp <- (xbarR/sR)/(1/nR)
  }
  
  
  df1 <- 1
  df2 <- nR + nT - 2
  
  Fc <- qf(0.05,df1,df2,log=FALSE,lower.tail = FALSE) ## critical central Fc
  #Feq <- qf(0.05,df1,df2,ncp=ncp) ## This is correct when I use the ncp from the excel examples, so the function is right at least
  #Fint <- qf(0.95,df1,df2,ncp=ncp) ## This is correct when I use the ncp from the excel examples, so the function is right, at least
  
  ## The
  ##observed F ratio (23.1) for this 2-sample case was greater than
  ##the critical central Fc (4.41), indicating that the difference in %
  ##EPT from reference and exposure reaches in 2011 was
  ##significantly different from zero 
  
  
  
  if (Fobs > Fc){
    a <- "The difference between the test and reference datasets is siginificantly different from 0. This means that the datasets do not likely have the same mean."
  }
  else {
    a <- "The difference between the test and reference means is not significanctly different from 0. It is very likely that the datasets have the same mean."
  }
  
  ####### Now for the normal range stuff - the previous lines established if there is a significant difference, now we find out how much
  
  ncp2 <- (-1*0/1+1*1.645/1)^2/(1/nR+1/nT)
  Feq2 <- qf(0.05,df1,df2,ncp=ncp2) ## This is correct when I use the ncp from the excel examples, so the function is right at least
  Fint2 <- qf(0.95,df1,df2,ncp=ncp2) ## This is correct when I use the ncp from the excel examples, so the function is right, at least
  
  
  if (Fobs < Feq2 & Fobs < Fint2){
    b <- "The test data likely falls within the normal range of the reference data."
  }
   if (dplyr::between(Fobs,Feq2,Fint2)){
    b <- "The test data could be near the edge of the normal range of the reference data, but it likely falls within the normal range of the reference data."
  }
  if(Fobs > Feq2 & Fobs > Fint2){
    b <- "The test data is likely outside of the normal range of the reference data."
  }
  
  text <- paste(a,b,"Look at the graph below to see the data being compared. Remember that, for this test to work, the variables and units MUST MATCH.")
  
  return(text)
  
}



