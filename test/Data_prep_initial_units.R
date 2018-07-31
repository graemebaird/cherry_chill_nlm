require(lme4)
require(merTools)
require(openxlsx)
require(ggplot2)
require(magrittr)
require(dplyr)
require(plotly)
require(chillR)


a1 <- read.xlsx("MEDICIONES BROTACION G.xlsx",
               sheet=4, startRow = 3, cols = 2:35)
a2 <- read.xlsx("MEDICIONES BROTACION G.xlsx",
                sheet=5, startRow = 3, cols = 2:35)
a3 <- read.xlsx("MEDICIONES BROTACION G.xlsx",
                sheet=6, startRow = 3, cols = 2:35)
a4 <- read.xlsx("MEDICIONES BROTACION G.xlsx",
                sheet=7, startRow = 3, cols = 2:35)
a5 <- read.xlsx("MEDICIONES BROTACION G.xlsx",
                sheet=8, startRow = 3, cols = 2:35)
a6 <- read.xlsx("MEDICIONES BROTACION G.xlsx",
                sheet=9, startRow = 3, cols = 2:35)
a7 <- read.xlsx("MEDICIONES BROTACION G.xlsx",
                sheet=10, startRow = 3, cols = 2:35)
a8 <- read.xlsx("MEDICIONES BROTACION G.xlsx",
                sheet=11, startRow = 3, cols = 2:35)
a9 <- read.xlsx("MEDICIONES BROTACION G.xlsx",
                sheet=12, startRow = 3, cols = 2:35)

d <- a1[0,1:19]

sub <- rep(1,dim(a1)[1])
d <- rbind(d, cbind(a1[,1:19], sub))
sub <-  rep(2,dim(a1)[1])
d <- rbind(d, cbind(a1[,c(1:4,20:34)], sub))
sub <-  rep(1,dim(a2)[1])
d <- rbind(d, cbind(a2[,1:19], sub))
sub <-  rep(2,dim(a2)[1])
d <- rbind(d, cbind(a2[,c(1:4,20:34)], sub))
sub <- rep(1,dim(a3)[1])
d <- rbind(d, cbind(a3[,1:19], sub ))
sub <- rep(2,dim(a3)[1])
d <- rbind(d, cbind(a3[,c(1:4,20:34)],  sub))
sub <- rep(1,dim(a4)[1])
d <- rbind(d, cbind(a4[,1:19], sub ))
sub <- rep(2,dim(a4)[1])
d <- rbind(d, cbind(a4[,c(1:4,20:34)], sub ))
sub <- rep(1,dim(a5)[1])
d <- rbind(d, cbind(a5[,1:19], sub ))
sub <- rep(2,dim(a5)[1])
d <- rbind(d, cbind(a5[,c(1:4,20:34)],  sub))
sub <- rep(1,dim(a6)[1])
d <- rbind(d, cbind(a6[,1:19], sub ))
sub <-  rep(2,dim(a6)[1])
d <- rbind(d, cbind(a6[,c(1:4,20:34)], sub))
sub <- rep(1,dim(a7)[1])
d <- rbind(d, cbind(a7[,1:19], sub ))
sub <- rep(2,dim(a7)[1])
d <- rbind(d, cbind(a7[,c(1:4,20:34)],  sub))
sub <- rep(1,dim(a8)[1])
d <- rbind(d, cbind(a8[,1:19], sub ))
sub <- rep(2,dim(a8)[1])
d <- rbind(d, cbind(a8[,c(1:4,20:34)],  sub))
sub <- rep(1,dim(a9)[1])
d <- rbind(d, cbind(a9[,1:19],  sub))
sub <- rep(2,dim(a9)[1])
d <- rbind(d, cbind(a9[,c(1:4,20:34)], sub ))


colnames(d) <- c("variety", "chill", "tree", "week",
                 "s1", "s2", "s3", "s4", "s5", "s6", 
                 "s7", "s8", "s9", "s10", "s11",
                 "s12", "s13", "s14", "s15", "sub")

countnas <- function(x) return(length(which(!is.na(x))))
countones <- function(x) return(length(which(x == 1 & !is.na(x))))
counttwos <- function(x) return(length(which(x == 2 & !is.na(x))))

d$trials <- apply(d[,5:19], 1, countnas)
d$gtip <- apply(d[,5:19], 1, countones)
d$full <- apply(d[,5:19], 1, counttwos)

d$both <- d$gtip + d$full
d$tree_u <- paste(d$tree,d$variety,d$chill,d$sub, sep="")
d$tree_sub <- paste(d$tree,d$variety,d$chill, sep="")
d$chill.c <- (d$chill - mean(d$chill))/sd(d$chill)
d$prop <- d$both / d$trials
d$propGT <- d$gtip / d$trials
d$propLO <- d$full / d$trials

write.csv(d, file="Raw_combined_excel_cunits.csv")







e <- data.frame(variety=character(0), chill=numeric(0), tree=numeric(0), 
                week=character(0), sub = integer(0), 
                trials=numeric(0),  
                tree_u = character(0), 
                tree_sub=character(0), 
                timetog = integer(0), timetob = integer(0), 
                timeto5 = integer(0), timeto2 = integer(0), timeto5l = integer(0), 
                gtip = integer(0), full = integer(0), both = integer(0), 
                prop = numeric(0))

vecun <- unique(d$tree_u)
for(i in 1:length(unique(vecun))){
  tempf <- d[d$tree_u == vecun[i],]
  tempt <- tempf[5,]
  tempt$timetog <- min(tempf$week[tempf$both != 0])
  tempt$timetob <- min(tempf$week[tempf$full != 0])
  tempt$timeto5 <- NA
  tempt$timeto5 <- min(tempf$week[tempf$both/tempf$trials > .5])
  tempt$timeto9 <- NA
  tempt$timeto9 <- min(tempf$week[tempf$both/tempf$trials > .9])
  tempt$timeto2 <- NA
  tempt$timeto2 <- min(tempf$week[tempf$both/tempf$trials > .2])
  tempt$timeto5l <- NA
  tempt$timeto5l <- min(tempf$week[tempf$full/tempf$trials > .5])
  e <- rbind(e, tempt[,c(
    "variety", "chill", "tree", "week", "sub", "trials", "tree_u", 
    "tree_sub", "timetog", "timetob", "timeto5", "timeto2", "timeto5l", 
    "gtip", "full", "both", "prop", "timeto9"
  )]) 
}

e$timeto2[is.infinite(e$timeto2)] <- NA
e$timeto5[is.infinite(e$timeto5)] <- NA
e$timeto9[is.infinite(e$timeto9)] <- NA
e$timetob[is.infinite(e$timetob)] <- NA
e$timetog[is.infinite(e$timetog)] <- NA
e$timeto5l[is.infinite(e$timetog)] <- NA
e$timegap5 <- e$timeto5l - e$timeto5
e$timegap9 <- e$timeto5l - e$timeto9
e$ratio <- e$full / e$gtip

write.csv(e, file="Summary_stats_cunits.csv")








w1 <- read.xlsx("PORCIONES DE FRIO.xlsx",
                sheet=2, startRow = 10, cols = 1:12)
## Lapins + Kordia C
w1 <- w1[-1:-2,]
w1$date <- as.Date(w1$X1,
                   origin = "1899-12-30")
w1$hour <- rep(c(14:23,0:13),length.out=length(w1$date))



w2 <- read.xlsx("PORCIONES DE FRIO.xlsx",
                sheet=3, startRow = 10, cols = 1:12)
## Bing
w2 <- w2[-1:-2,]
w2$date <- as.Date(w2$X1,
                   origin = "1899-12-30")
w2$hour <- rep(c(14:23,0:13),length.out=length(w2$date))


w3 <- read.xlsx("PORCIONES DE FRIO.xlsx",
                sheet=4, startRow = 10, cols = 1:12)
## Regina + Skeena + Kordia M
w3 <- w3[-1:-2,]
w3$date <- as.Date(w3$X1,
                   origin = "1899-12-30")
w3$hour <- rep(c(14:23,0:13),length.out=length(w3$date))


w4 <- read.xlsx("PORCIONES DE FRIO.xlsx",
                sheet=5, startRow = 10, cols = 1:12)
## Santina
w4 <- w4[-1:-2,]
w4$date <- as.Date(w4$X1,
                   origin = "1899-12-30")
w4$hour <- rep(c(14:23,0:13),length.out=length(w4$date))


w5 <- read.xlsx("PORCIONES DE FRIO.xlsx",
                sheet=6, startRow = 10, cols = 1:12)
## Sweetheart and Rainier
w5 <- w5[-1:-2,]
w5$date <- as.Date(w5$X1,
                   origin = "1899-12-30")
w5$hour <- rep(c(14:23,0:13),length.out=length(w5$date))



append_weather_f <- read.xlsx("PORCIONES DE FRIO.xlsx", sheet=1, startRow=12, 
                                cols=1:2)
append_weather_vec <- append_weather_f$`12`[725:(725+746)]
append_weather_dates <- as.Date(append_weather_f$X1, origin = "1899-12-30")[725:(725+746)]

append_weather_vec2 <- append_weather_f$`12`[725:(725+746+334)]
append_weather_dates2 <- as.Date(append_weather_f$X1, origin = "1899-12-30")[725:(725+746+334)]




coll_dates <- data.frame(oCP = rep(NA,10), dates = rep(NA,10))
coll_dates$oCP <- c(
  "16",
  "26",
  "31",
  "37",
  "42",
  "47",
  "52",
  "57",
  "63",
  "70"
)

coll_dates$dates <- c(
  "17-5-2017",
  "29-5-2017",
  "8-6-2017",
  "14-6-2017",
  "21-6-2017",
  "28-6-2017",
  "5-7-2017",
  "11-7-2017",
  "19-7-2017",
  "26-7-2017"
)
coll_dates$dates <- as.Date(coll_dates$dates, origin = "1899-12-30", format="%d-%m-%Y")



td_ind <- data.frame(test_dates = rep(NA,14))

td_ind$test_dates <- c(
  "1-6-2017",
  "8-6-2017",
  "15-6-2017",
  "22-6-2017",
  "29-6-2017",
  "6-7-2017",
  "13-7-2017",
  "20-7-2017",
  "27-7-2017",
  "3-8-2017",
  "10-8-2017",
  "17-8-2017",
  "24-8-2017",
  "31-8-2017"
)


td_ind$test_dates <- as.Date(td_ind$test_dates, origin = "1899-12-30", format="%d-%m-%Y")


td_ind$cp1 <- c(1:5,rep(NA,9))
td_ind$cp2 <- c(NA, 1:5,rep(NA,8))
td_ind$cp3 <- c(rep(NA,2), 1:5,rep(NA,7))
td_ind$cp4 <- c(rep(NA,3), 1:5,rep(NA,6))
td_ind$cp5 <- c(rep(NA,4), 1:5,rep(NA,5))
td_ind$cp6 <- c(rep(NA,5), 1:5,rep(NA,4))
td_ind$cp7 <- c(rep(NA,6), 1:5,rep(NA,3))
td_ind$cp8 <- c(rep(NA,7), 1:5,rep(NA,2))
td_ind$cp9 <- c(rep(NA,8), 1:5,rep(NA,1))
td_ind$cp10 <- c(rep(NA,9), 1:5)



gh <- read.xlsx("PORCIONES DE FRIO.xlsx",
                sheet=7, startRow = 2, cols = 2:3)
gh$date <- as.Date(gh$Fecha,
                   origin = "1899-12-30")
gh$hour <- rep(c(0:23),length.out=length(gh$Fecha))









d$CP <- NA
d$GDH <- NA




## Lapins + Kordia C

repvar <- "Lapins"
templist <- list()


for(i in 1:10){
  intdate <- coll_dates$dates[i]
  
for(j in 1:5){
  enddate <- td_ind$test_dates[td_ind[paste("cp",i,sep="")] == j &
                                 !is.na(td_ind[paste("cp",i,sep="")])]
  
  intpoint1 <- min(which(as.character(w1$date) == intdate)) -1
  intpoint2 <- min(which(as.character(gh$date) == intdate))
  endpoint <- max(which(as.character(gh$date) == enddate))

  fieldvec <- w1$`Temp(F)`[1:intpoint1]
  ghvec <- gh$`F°`[intpoint2:endpoint]
  
  tempvec <- c(fieldvec,ghvec)
  tempvec <- (tempvec - 32)/1.8
  tempvec <- c(append_weather_vec,tempvec)
  templist[[i]] <- tempvec
  tempdate <- c(w1$date[1:intpoint1], gh$date[intpoint2:endpoint])
  tempvec <- tempvec[!is.na(tempvec)]
  ghvec <- (ghvec - 32)/1.8
  
  cp_trans <- max(Utah_Model(tempvec))
  gd_trans <- max(GDH(ghvec))
  
  d$CP[d$chill==coll_dates$oCP[i] & d$week==j & d$variety == repvar] <- cp_trans
  d$GDH[d$chill==coll_dates$oCP[i] & d$week==j & d$variety == repvar] <- gd_trans
}
}


tempmat <- matrix(NA, nrow = length(templist[[10]]), ncol = 10)
for (i in 1:10) tempmat[,i] <- c(templist[[i]], rep(NA, length(templist[[10]]) - length(templist[[i]])))

tempdf <- melt(data.frame(tempmat))
tempdf$time <- c(append_weather_dates,tempdate)


levels(tempdf$variable) <- d$CP[d$variety == "Lapins" & !is.na(d$CP)] %>% unique() %>% round()

saveRDS(tempdf, file="La_wdf.Rda")




repvar <- "Kordia (C )"
templist <- list()


for(i in 1:10){
  intdate <- coll_dates$dates[i]
  
  for(j in 1:5){
    enddate <- td_ind$test_dates[td_ind[paste("cp",i,sep="")] == j &
                                   !is.na(td_ind[paste("cp",i,sep="")])]
    
    intpoint1 <- min(which(as.character(w1$date) == intdate)) -1
    intpoint2 <- min(which(as.character(gh$date) == intdate))
    endpoint <- max(which(as.character(gh$date) == enddate))
    
    fieldvec <- w1$`Temp(F)`[1:intpoint1]
    ghvec <- gh$`F°`[intpoint2:endpoint]
    
    tempvec <- c(fieldvec,ghvec)
    tempvec <- (tempvec - 32)/1.8
    tempvec <- c(append_weather_vec,tempvec)
    templist[[i]] <- tempvec
    tempdate <- c(w1$date[1:intpoint1], gh$date[intpoint2:endpoint])
    tempvec <- tempvec[!is.na(tempvec)]
    ghvec <- (ghvec - 32)/1.8
    
    cp_trans <- max(Utah_Model(tempvec))
    gd_trans <- max(GDH(ghvec))
    
    d$CP[d$chill==coll_dates$oCP[i] & d$week==j & d$variety == repvar] <- cp_trans
    d$GDH[d$chill==coll_dates$oCP[i] & d$week==j & d$variety == repvar] <- gd_trans
  }
}


tempmat <- matrix(NA, nrow = length(templist[[10]]), ncol = 10)
for (i in 1:10) tempmat[,i] <- c(templist[[i]], rep(NA, length(templist[[10]]) - length(templist[[i]])))

tempdf <- melt(data.frame(tempmat))
tempdf$time <- c(append_weather_dates,tempdate)


levels(tempdf$variable) <- d$CP[d$variety == "Kordia (C )" & !is.na(d$CP)] %>% unique() %>% round()

saveRDS(tempdf, file="KoC_wdf.Rda")



## Bing


repvar <- "Bing"
templist <- list()


for(i in 1:10){
  intdate <- coll_dates$dates[i]
  
  for(j in 1:5){
    enddate <- td_ind$test_dates[td_ind[paste("cp",i,sep="")] == j &
                                   !is.na(td_ind[paste("cp",i,sep="")])]
    
    intpoint1 <- min(which(as.character(w1$date) == intdate)) -1
    intpoint2 <- min(which(as.character(gh$date) == intdate))
    endpoint <- max(which(as.character(gh$date) == enddate))
    
    fieldvec <- w2$`Temp(F)`[1:intpoint1]
    ghvec <- gh$`F°`[intpoint2:endpoint]
    
    tempvec <- c(fieldvec,ghvec)
    tempvec <- (tempvec - 32)/1.8
    tempvec <- c(append_weather_vec,tempvec)
    templist[[i]] <- tempvec
    tempdate <- c(w1$date[1:intpoint1], gh$date[intpoint2:endpoint])
    tempvec <- tempvec[!is.na(tempvec)]
    ghvec <- (ghvec - 32)/1.8
    
    cp_trans <- max(Utah_Model(tempvec))
    gd_trans <- max(GDH(ghvec))
    
    d$CP[d$chill==coll_dates$oCP[i] & d$week==j & d$variety == repvar] <- cp_trans
    d$GDH[d$chill==coll_dates$oCP[i] & d$week==j & d$variety == repvar] <- gd_trans
  }
}


tempmat <- matrix(NA, nrow = length(templist[[10]]), ncol = 10)
for (i in 1:10) tempmat[,i] <- c(templist[[i]], rep(NA, length(templist[[10]]) - length(templist[[i]])))

tempdf <- melt(data.frame(tempmat))
tempdf$time <- c(append_weather_dates,tempdate)


levels(tempdf$variable) <- d$CP[d$variety == "Bing" & !is.na(d$CP)] %>% unique() %>% round()

saveRDS(tempdf, file="Bi_wdf.Rda")


## Regina + Skeena + Kordia M


repvar <- "Regina"
templist <- list()


for(i in 1:10){
  intdate <- coll_dates$dates[i]
  
  for(j in 1:5){
    enddate <- td_ind$test_dates[td_ind[paste("cp",i,sep="")] == j &
                                   !is.na(td_ind[paste("cp",i,sep="")])]
    
    intpoint1 <- min(which(as.character(w1$date) == intdate)) -1
    intpoint2 <- min(which(as.character(gh$date) == intdate))
    endpoint <- max(which(as.character(gh$date) == enddate))
    
    fieldvec <- w3$`Temp(F)`[1:intpoint1]
    ghvec <- gh$`F°`[intpoint2:endpoint]
    
    tempvec <- c(fieldvec,ghvec)
    tempvec <- (tempvec - 32)/1.8
    tempvec <- c(append_weather_vec,tempvec)
    templist[[i]] <- tempvec
    tempdate <- c(w1$date[1:intpoint1], gh$date[intpoint2:endpoint])
    tempvec <- tempvec[!is.na(tempvec)]
    ghvec <- (ghvec - 32)/1.8
    
    cp_trans <- max(Utah_Model(tempvec))
    gd_trans <- max(GDH(ghvec))
    
    d$CP[d$chill==coll_dates$oCP[i] & d$week==j & d$variety == repvar] <- cp_trans
    d$GDH[d$chill==coll_dates$oCP[i] & d$week==j & d$variety == repvar] <- gd_trans
  }
}


tempmat <- matrix(NA, nrow = length(templist[[10]]), ncol = 10)
for (i in 1:10) tempmat[,i] <- c(templist[[i]], rep(NA, length(templist[[10]]) - length(templist[[i]])))

tempdf <- melt(data.frame(tempmat))
tempdf$time <- c(append_weather_dates,tempdate)


levels(tempdf$variable) <- d$CP[d$variety == "Regina" & !is.na(d$CP)] %>% unique() %>% round()

saveRDS(tempdf, file="Re_wdf.Rda")


repvar <- "Skeena"
templist <- list()


for(i in 1:10){
  intdate <- coll_dates$dates[i]
  
  for(j in 1:5){
    enddate <- td_ind$test_dates[td_ind[paste("cp",i,sep="")] == j &
                                   !is.na(td_ind[paste("cp",i,sep="")])]
    
    intpoint1 <- min(which(as.character(w1$date) == intdate)) -1
    intpoint2 <- min(which(as.character(gh$date) == intdate))
    endpoint <- max(which(as.character(gh$date) == enddate))
    
    fieldvec <- w3$`Temp(F)`[1:intpoint1]
    ghvec <- gh$`F°`[intpoint2:endpoint]
    
    tempvec <- c(fieldvec,ghvec)
    tempvec <- (tempvec - 32)/1.8
    tempvec <- c(append_weather_vec,tempvec)
    templist[[i]] <- tempvec
    tempdate <- c(w1$date[1:intpoint1], gh$date[intpoint2:endpoint])
    tempvec <- tempvec[!is.na(tempvec)]
    ghvec <- (ghvec - 32)/1.8
    
    cp_trans <- max(Utah_Model(tempvec))
    gd_trans <- max(GDH(ghvec))
    
    d$CP[d$chill==coll_dates$oCP[i] & d$week==j & d$variety == repvar] <- cp_trans
    d$GDH[d$chill==coll_dates$oCP[i] & d$week==j & d$variety == repvar] <- gd_trans
  }
}


tempmat <- matrix(NA, nrow = length(templist[[10]]), ncol = 10)
for (i in 1:10) tempmat[,i] <- c(templist[[i]], rep(NA, length(templist[[10]]) - length(templist[[i]])))

tempdf <- melt(data.frame(tempmat))
tempdf$time <- c(append_weather_dates,tempdate)


levels(tempdf$variable) <- d$CP[d$variety == "Skeena" & !is.na(d$CP)] %>% unique() %>% round()

saveRDS(tempdf, file="Sk_wdf.Rda")



repvar <- "Kordia (M)"
templist <- list()


for(i in 1:10){
  intdate <- coll_dates$dates[i]
  
  for(j in 1:5){
    enddate <- td_ind$test_dates[td_ind[paste("cp",i,sep="")] == j &
                                   !is.na(td_ind[paste("cp",i,sep="")])]
    
    intpoint1 <- min(which(as.character(w1$date) == intdate)) -1
    intpoint2 <- min(which(as.character(gh$date) == intdate))
    endpoint <- max(which(as.character(gh$date) == enddate))
    
    fieldvec <- w3$`Temp(F)`[1:intpoint1]
    ghvec <- gh$`F°`[intpoint2:endpoint]
    
    tempvec <- c(fieldvec,ghvec)
    tempvec <- (tempvec - 32)/1.8
    tempvec <- c(append_weather_vec,tempvec)
    templist[[i]] <- tempvec
    tempdate <- c(w3$date[1:intpoint1], gh$date[intpoint2:endpoint])
    tempvec <- tempvec[!is.na(tempvec)]
    ghvec <- (ghvec - 32)/1.8
    
    cp_trans <- max(Utah_Model(tempvec))
    gd_trans <- max(GDH(ghvec))
    
    d$CP[d$chill==coll_dates$oCP[i] & d$week==j & d$variety == repvar] <- cp_trans
    d$GDH[d$chill==coll_dates$oCP[i] & d$week==j & d$variety == repvar] <- gd_trans
  }
}


tempmat <- matrix(NA, nrow = length(templist[[10]]), ncol = 10)
for (i in 1:10) tempmat[,i] <- c(templist[[i]], rep(NA, length(templist[[10]]) - length(templist[[i]])))

tempdf <- melt(data.frame(tempmat))
tempdf$time <- c(append_weather_dates,tempdate)


levels(tempdf$variable) <- d$CP[d$variety == "Kordia (M)" & !is.na(d$CP)] %>% unique() %>% round()

saveRDS(tempdf, file="KoM_wdf.Rda")



## Santina


repvar <- "Santina"
templist <- list()


for(i in 1:10){
  intdate <- coll_dates$dates[i]
  
  for(j in 1:5){
    enddate <- td_ind$test_dates[td_ind[paste("cp",i,sep="")] == j &
                                   !is.na(td_ind[paste("cp",i,sep="")])]
    
    intpoint1 <- min(which(as.character(w4$date) == intdate)) -1
    intpoint2 <- min(which(as.character(gh$date) == intdate))
    endpoint <- max(which(as.character(gh$date) == enddate))
    
    fieldvec <- w4$`Temp(F)`[1:intpoint1]
    ghvec <- gh$`F°`[intpoint2:endpoint]
    
    tempvec <- c(fieldvec,ghvec)
    tempvec <- (tempvec - 32)/1.8
    tempvec <- c(append_weather_vec2,tempvec)
    templist[[i]] <- tempvec
    tempdate <- c(w4$date[1:intpoint1], gh$date[intpoint2:endpoint])
    tempvec <- tempvec[!is.na(tempvec)]
    ghvec <- (ghvec - 32)/1.8
    
    cp_trans <- max(Utah_Model(tempvec))
    gd_trans <- max(GDH(ghvec))
    
    d$CP[d$chill==coll_dates$oCP[i] & d$week==j & d$variety == repvar] <- cp_trans
    d$GDH[d$chill==coll_dates$oCP[i] & d$week==j & d$variety == repvar] <- gd_trans
  }
}


tempmat <- matrix(NA, nrow = length(templist[[10]]), ncol = 10)
for (i in 1:10) tempmat[,i] <- c(templist[[i]], rep(NA, length(templist[[10]]) - length(templist[[i]])))

tempdf <- melt(data.frame(tempmat))
tempdf$time <- c(append_weather_dates2,tempdate)


levels(tempdf$variable) <- d$CP[d$variety == "Santina" & !is.na(d$CP)] %>% unique() %>% round()

saveRDS(tempdf, file="Sa_wdf.Rda")


## Sweetheart and Rainier



repvar <- "Sweetheart"
templist <- list()


for(i in 1:10){
  intdate <- coll_dates$dates[i]
  
  for(j in 1:5){
    enddate <- td_ind$test_dates[td_ind[paste("cp",i,sep="")] == j &
                                   !is.na(td_ind[paste("cp",i,sep="")])]
    
    intpoint1 <- min(which(as.character(w5$date) == intdate)) -1
    intpoint2 <- min(which(as.character(gh$date) == intdate))
    endpoint <- max(which(as.character(gh$date) == enddate))
    
    fieldvec <- w5$`Temp(F)`[1:intpoint1]
    ghvec <- gh$`F°`[intpoint2:endpoint]
    
    tempvec <- c(fieldvec,ghvec)
    tempvec <- (tempvec - 32)/1.8
    tempvec <- c(append_weather_vec2,tempvec)
    templist[[i]] <- tempvec
    tempdate <- c(w5$date[1:intpoint1], gh$date[intpoint2:endpoint])
    tempvec <- tempvec[!is.na(tempvec)]
    ghvec <- (ghvec - 32)/1.8
    
    cp_trans <- max(Utah_Model(tempvec))
    gd_trans <- max(GDH(ghvec))
    
    d$CP[d$chill==coll_dates$oCP[i] & d$week==j & d$variety == repvar] <- cp_trans
    d$GDH[d$chill==coll_dates$oCP[i] & d$week==j & d$variety == repvar] <- gd_trans
  }
}


tempmat <- matrix(NA, nrow = length(templist[[10]]), ncol = 10)
for (i in 1:10) tempmat[,i] <- c(templist[[i]], rep(NA, length(templist[[10]]) - length(templist[[i]])))

tempdf <- melt(data.frame(tempmat))
tempdf$time <- c(append_weather_dates2,tempdate)


levels(tempdf$variable) <- d$CP[d$variety == "Sweetheart" & !is.na(d$CP)] %>% unique() %>% round()

saveRDS(tempdf, file="Sw_wdf.Rda")



repvar <- "Rainier"
templist <- list()


for(i in 1:10){
  intdate <- coll_dates$dates[i]
  
  for(j in 1:5){
    enddate <- td_ind$test_dates[td_ind[paste("cp",i,sep="")] == j &
                                   !is.na(td_ind[paste("cp",i,sep="")])]
    
    intpoint1 <- min(which(as.character(w5$date) == intdate)) -1
    intpoint2 <- min(which(as.character(gh$date) == intdate))
    endpoint <- max(which(as.character(gh$date) == enddate))
    
    fieldvec <- w5$`Temp(F)`[1:intpoint1]
    ghvec <- gh$`F°`[intpoint2:endpoint]
    
    tempvec <- c(fieldvec,ghvec)
    tempvec <- (tempvec - 32)/1.8
    tempvec <- c(append_weather_vec2,tempvec)
    templist[[i]] <- tempvec
    tempdate <- c(w5$date[1:intpoint1], gh$date[intpoint2:endpoint])
    tempvec <- tempvec[!is.na(tempvec)]
    ghvec <- (ghvec - 32)/1.8
    
    cp_trans <- max(Utah_Model(tempvec))
    gd_trans <- max(GDH(ghvec))
    
    d$CP[d$chill==coll_dates$oCP[i] & d$week==j & d$variety == repvar] <- cp_trans
    d$GDH[d$chill==coll_dates$oCP[i] & d$week==j & d$variety == repvar] <- gd_trans
  }
}


tempmat <- matrix(NA, nrow = length(templist[[10]]), ncol = 10)
for (i in 1:10) tempmat[,i] <- c(templist[[i]], rep(NA, length(templist[[10]]) - length(templist[[i]])))

tempdf <- melt(data.frame(tempmat))
tempdf$time <- c(append_weather_dates2,tempdate)


levels(tempdf$variable) <- d$CP[d$variety == "Rainier" & !is.na(d$CP)] %>% unique() %>% round()

saveRDS(tempdf, file="Ra_wdf.Rda")


cent <- function(testvec){
  outvec <- ((testvec - mean(testvec, na.rm = T)) / sd(testvec, na.rm = T))
  return(outvec)  
}

d$CP.c <- cent(d$CP)
d$GDH.c <- cent(d$GDH)



write.csv(d, file="Raw_combined_excel_cunits.csv")






templist <- list()

for(i in 1:10){
  intdate <- coll_dates$dates[i]
  
    enddate <- td_ind$test_dates[td_ind[paste("cp",i,sep="")] == 5 &
                                   !is.na(td_ind[paste("cp",i,sep="")])]
    
    intpoint1 <- min(which(as.character(w1$date) == intdate)) -1
    intpoint2 <- min(which(as.character(gh$date) == intdate))
    endpoint <- max(which(as.character(gh$date) == enddate))
    
    fieldvec <- w1$`Temp(F)`[1:intpoint1]
    ghvec <- gh$`F°`[intpoint2:endpoint]
    
    tempvec <- c(fieldvec,ghvec)
    tempvec <- (tempvec - 32)/1.8
    tempvec <- c(append_weather_vec,tempvec)
    
    templist[[i]] <- tempvec
    tempdate <- c(w1$date[1:intpoint1], gh$date[intpoint2:endpoint])
  
  }

tempmat <- matrix(NA, nrow = length(templist[[10]]), ncol = 10)
for (i in 1:10) tempmat[,i] <- c(templist[[i]], rep(NA, length(templist[[10]]) - length(templist[[i]])))

tempdf <- melt(data.frame(tempmat))
tempdf$time <- c(append_weather_dates,tempdate)

saveRDS(tempdf, file="weatherdf.Rda")

  



