########################################################
### PORTFOLIO REBALANCING vs. SIGNALLING CHANNEL #######

### IF SIGNALLING, ALL YIELDS SHOULD GO DOWN SIMILARLY (NOT ONLY THOSE BEING BOUGHT)
### LET'S LOOK AT EONIA (OIS) vs. GOV. YIELDS
rm(list=ls())
#install.packages("tidyverse")
library(tidyverse)
library(systemfit)
library(vars) # Load package
library(forecast)
library(xts)
library(ggplot2)
library(YieldCurve)
library(magick)
library(scales)

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

n = 4
cols = gg_color_hue(n)

input_dir <- "//10.2.112.201/Bruegel/RESEARCH/Research Assistants/Catarina/Monetary Policy Channels/"
names_sheets <- openxlsx::getSheetNames(paste0(input_dir,"EOINA_DE_IT_FR_ES.xlsx"))

dataset_list <- list()

for (i in 1:(length(names_sheets))) {
  dataset_list[[i]] <- openxlsx::read.xlsx(xlsxFile=paste0(input_dir,"EOINA_DE_IT_FR_ES.xlsx"),sheet=i,na.strings=c("","0","0.00"),detectDates=T)
}

names(dataset_list) <- names_sheets

dataset_list[[1]][1,1] <- "2003-01-01"
dataset_list[[2]][1,1] <- "2003-01-01"
dataset_list[[3]][1,1] <- "2003-01-01"
dataset_list[[4]][1,1] <- "2003-01-01"

dataset_list[[5]][1,1] <- "2005-06-13"
dataset_list[[6]][1,1] <- "2005-07-19"

dataset_list[[7]] <- dataset_list[[7]][-1,]

for (i in 1:6) {
  names(dataset_list[[i]])[1] <- "DATE"
}

US_y <- list()

###View(dataset_list[[7]])
indices <- grep("^X",names(dataset_list[[7]]))

for (i in 1:(length(indices)) ) {
  
  ind <- indices[i]
  if (ind!=indices[length(indices)]) {
    ind_next <- indices[i+1]
  } else {ind_next <- ind+2}
  
  print(ind)
  print(ind_next)
  
  US_y[[i]] <- dataset_list[[7]][,(ind:(ind_next-1) )]
  names(US_y[[i]])[1] <- "DATE"
  
}

US_y[[1]][1,1] <- "2003-01-01"
US_y[[2]][1,1] <- "2003-01-01"
US_y[[3]][1,1] <- "2003-01-01"
US_y[[4]][1,1] <- "2008-06-04"
US_y[[5]][1,1] <- "2003-01-01"
US_y[[6]][1,1] <- "2003-01-01"
US_y[[7]][1,1] <- "2009-02-26"
US_y[[8]][1,1] <- "2003-01-01"

merged_US <- US_y %>% reduce(left_join, by = "DATE")
merged_EONIA <- dataset_list[1:6] %>% reduce(left_join, by = "DATE")

DE_y <- list()

##View(dataset_list[[8]])
indices <- grep("^X",names(dataset_list[[8]]))

for (i in 1:(length(indices)) ) {
  
  ind <- indices[i]
  if (ind!=indices[length(indices)]) {
    ind_next <- indices[i+1]
  } else {ind_next <- ind+2}
  
  print(ind)
  print(ind_next)
  
  DE_y[[i]] <- dataset_list[[8]][,(ind:(ind_next-1) )]
  names(DE_y[[i]])[1] <- "DATE"
  
}

DE_y[[1]][1,1] <- "2016-08-29"
DE_y[[2]][1,1] <- "2003-01-01"
DE_y[[3]][1,1] <- "2003-01-01"
DE_y[[4]][1,1] <- "2003-01-01"
DE_y[[5]][1,1] <- "2003-01-01"
DE_y[[6]][1,1] <- "2003-01-01"
DE_y[[7]][1,1] <- "2003-01-01"

merged_DE <- list(DE_y[[3]],DE_y[[1]],DE_y[[2]],DE_y[[4]],DE_y[[5]],DE_y[[6]],DE_y[[7]]) %>% reduce(left_join, by = "DATE")

#################################
#################################

IT_y <- list()

##View(dataset_list[[9]])
indices <- grep("^X",names(dataset_list[[9]]))

for (i in 1:(length(indices)) ) {
  
  ind <- indices[i]
  if (ind!=indices[length(indices)]) {
    ind_next <- indices[i+1]
  } else {ind_next <- ind+2}
  
  print(ind)
  print(ind_next)
  
  IT_y[[i]] <- dataset_list[[9]][,(ind:(ind_next-1) )]
  names(IT_y[[i]])[1] <- "DATE"
  
}

IT_y[[1]][1,1] <- "2003-01-01"
IT_y[[2]][1,1] <- "2003-01-02"
IT_y[[3]][1,1] <- "2003-01-01"
IT_y[[4]][1,1] <- "2003-01-01"
IT_y[[5]][1,1] <- "2003-01-01"
IT_y[[6]][1,1] <- "2003-01-01"

merged_IT <- merge(IT_y[[4]],IT_y[[1]],by="DATE",all.x=T)
merged_IT <- merge(merged_IT,IT_y[[2]],by="DATE",all.x=T)

empty <- apply(merged_IT,1,FUN= function(x) { sum(is.na(x))} )
to_keep <- as.numeric(which(empty!=4))
merged_IT <- merged_IT[to_keep,]

merged_IT <- merge(merged_IT,IT_y[[3]],by="DATE",all.x=T)
merged_IT <- merge(merged_IT,IT_y[[5]],by="DATE",all.x=T)
merged_IT <- merge(merged_IT,IT_y[[6]],by="DATE",all.x=T)

#################################
#################################

FR_y <- list()

##View(dataset_list[[10]])
indices <- grep("^X",names(dataset_list[[10]]))

for (i in 1:(length(indices)) ) {
  
  ind <- indices[i]
  if (ind!=indices[length(indices)]) {
    ind_next <- indices[i+1]
  } else {ind_next <- ind+2}
  
  print(ind)
  print(ind_next)
  
  FR_y[[i]] <- dataset_list[[10]][,(ind:(ind_next-1) )]
  names(FR_y[[i]])[1] <- "DATE"
  
}

FR_y[[1]][1,1] <- "2016-08-29"
FR_y[[2]][1,1] <- "2003-01-01"
FR_y[[3]][1,1] <- "2003-01-01"
FR_y[[4]][1,1] <- "2003-01-01"
FR_y[[5]][1,1] <- "2003-01-01"
FR_y[[6]][1,1] <- "2003-01-01"
FR_y[[7]][1,1] <- "2003-01-01"

merged_FR <- list(FR_y[[5]],FR_y[[1]],FR_y[[2]],FR_y[[4]],FR_y[[3]],FR_y[[6]],FR_y[[7]]) %>% reduce(left_join, by = "DATE")


#################################
#################################

ES_y <- list()

##View(dataset_list[[11]])
indices <- grep("^X",names(dataset_list[[11]]))

for (i in 1:(length(indices)) ) {
  
  ind <- indices[i]
  if (ind!=indices[length(indices)]) {
    ind_next <- indices[i+1]
  } else {ind_next <- ind+2}
  
  print(ind)
  print(ind_next)
  
  ES_y[[i]] <- dataset_list[[11]][,(ind:(ind_next-1) )]
  names(ES_y[[i]])[1] <- "DATE"
  
}

ES_y[[1]][1,1] <- "2011-08-08"
ES_y[[2]][1,1] <- "2011-08-08"
ES_y[[3]][1,1] <- "2011-08-08"
ES_y[[4]][1,1] <- "2003-01-01"
ES_y[[5]][1,1] <- "2003-01-01"
ES_y[[6]][1,1] <- "2003-01-01"

merged_ES <- list(ES_y[[5]],ES_y[[1]],ES_y[[2]],ES_y[[4]],ES_y[[3]],ES_y[[6]]) %>% reduce(left_join, by = "DATE")

#################################
#################################

PT_y <- list()

##View(dataset_list[[12]])
indices <- grep("^X",names(dataset_list[[12]]))

for (i in 1:(length(indices)) ) {
  
  ind <- indices[i]
  if (ind!=indices[length(indices)]) {
    ind_next <- indices[i+1]
  } else {ind_next <- ind+2}
  
  print(ind)
  print(ind_next)
  
  PT_y[[i]] <- dataset_list[[12]][,(ind:(ind_next-1) )]
  names(PT_y[[i]])[1] <- "DATE"
  
}

PT_y[[1]][1,1] <- "2004-03-29"
PT_y[[2]][1,1] <- "2004-03-29"
PT_y[[3]][1,1] <- "2004-03-29"

PT_y[[4]][1,1] <- "2003-01-01"
PT_y[[5]][1,1] <- "2003-01-01"
PT_y[[6]][1,1] <- "2003-01-01"

merged_PT <- merge(PT_y[[4]],PT_y[[1]],by="DATE",all.x=T)
merged_PT <- merge(merged_PT,PT_y[[2]],by="DATE",all.x=T)

empty <- apply(merged_PT,1,FUN= function(x) { sum(is.na(x))} )
to_keep <- as.numeric(which(empty!=3))
merged_PT <- merged_PT[to_keep,]

merged_PT <- merge(merged_PT,PT_y[[3]],by="DATE",all.x=T)
merged_PT <- merge(merged_PT,PT_y[[5]],by="DATE",all.x=T)
merged_PT <- merge(merged_PT,PT_y[[6]],by="DATE",all.x=T)


rm(US_y)
rm(DE_y)
rm(IT_y)
rm(ES_y)
rm(FR_y)
rm(PT_y)

#######################################
#######################################

### US OIS

names_sheets <- openxlsx::getSheetNames(paste0(input_dir,"OIS_US.xlsx"))

OIS_list <- list()

for (i in 1:(length(names_sheets))) {
  OIS_list[[i]] <- openxlsx::read.xlsx(xlsxFile=paste0(input_dir,"OIS_US.xlsx"),sheet=i,startRow = 2,na.strings=c("","0","0.00"),detectDates=T)
  names(OIS_list[[i]])[1] <- "DATE"
}

names(OIS_list) <- names_sheets

merged_OIS <- list(OIS_list[[4]],OIS_list[[1]],OIS_list[[2]],OIS_list[[5]],OIS_list[[3]],OIS_list[[6]],OIS_list[[7]]) %>% reduce(left_join, by = "DATE")
rm(OIS_list)

#######################################
#######################################

## EVENTS
#names_sheets <- openxlsx::getSheetNames(paste0(input_dir,"Monetary_Policy_Events.xlsx"))
events <- openxlsx::read.xlsx(xlsxFile=paste0(input_dir,"Monetary_Policy_Events.xlsx"),sheet="EVENTS",startRow = 1,detectDates=T)
events[is.na(events)] <- 0 
names(events)[1] <- "DATE"

#######################################
#######################################

## EXCESS LIQUIDITY
## US 
exc_res_US <- read.csv(file=paste0(input_dir,"USA_Excess_Reserves_weekly.csv"),header=T)

## ECB ALL LIQUIDITY VARIABLES
ecb_liq <- openxlsx::read.xlsx(xlsxFile=paste0(input_dir,"ECB_Excess liquidity_euro area 07.09.2018.xlsx"),sheet="liq_daily_1999.csv",startRow = 2,detectDates=T)
drop_index <- grep("alternative",names(ecb_liq))
ecb_liq <- ecb_liq[,1:(drop_index-1)]
names(ecb_liq)[1] <- "DATE"

## ECB RATES
ecb_rates <- openxlsx::read.xlsx(xlsxFile=paste0(input_dir,"ECB_Rates.xlsx"),sheet="ECB_RATES",startRow = 2,detectDates=T)
ref_ops <- ecb_rates[,1:2]
names(ref_ops)[1] <- "DATE"

mg_lending <- ecb_rates[,3:4]
names(mg_lending)[1] <- "DATE"

dep_facility <- ecb_rates[,5:6]
names(dep_facility)[1] <- "DATE"
rm(ecb_rates)

#############################################
#############################################
### EURIBOR
data_IT <- openxlsx::read.xlsx(xlsxFile="//10.2.112.201/Bruegel/RESEARCH/Research Assistants/Catarina/IT_Contagion/complete_data.xlsx",detectDates=T)
data_IT_important <- data_IT[,-c(2,3,5)]
grep("Open",names(data_IT_important))
grep("Excess",names(data_IT_important))
data_IT_important <- data_IT_important[,-seq(71,80,1)]

data_IT_important$DATE <- as.Date(data_IT_important$DATE,format="%d/%m/%Y")


#############################################
#############################################
### ZERO-COUPON YIELDS

zero_names_sheets <- openxlsx::getSheetNames(paste0(input_dir,"BBG - Zero coupon yields.xlsx"))

zero_list <- list()

for (i in 6:9) {
  
  df <- openxlsx::read.xlsx(xlsxFile=paste0(input_dir,"BBG - Zero coupon yields.xlsx"),sheet=i,na.strings=c("","0","0.00"),detectDates=T,startRow=4)
  names(df) <- c("DATE",paste0( toupper( zero_names_sheets[i] ), "_", c("0.25", "0.5", seq(1,10,1), "15", "20" ) ))
  zero_list[[i-5]] <- df
}

names(zero_list) <- zero_names_sheets[6:9]

zero_data <- zero_list %>% reduce(left_join, by = "DATE")

zero_merged_IT <- zero_list[[1]]
zero_merged_ES <- zero_list[[2]]
zero_merged_FR <- zero_list[[3]]
zero_merged_DE <- zero_list[[4]]

#######################################
#######################################

### FULL DATASET

final_dataset <- merge(merged_OIS,merged_EONIA,by="DATE",all.x=T)

final_dataset <- merge(final_dataset,merged_IT,by="DATE",all.x=T)
final_dataset <- merge(final_dataset,merged_DE,by="DATE",all.x=T)
final_dataset <- merge(final_dataset,merged_US,by="DATE",all.x=T)
final_dataset <- merge(final_dataset,merged_FR,by="DATE",all.x=T)
final_dataset <- merge(final_dataset,merged_ES,by="DATE",all.x=T)
final_dataset <- merge(final_dataset,merged_PT,by="DATE",all.x=T)

final_dataset <- merge(final_dataset,zero_data,by="DATE",all.x=T)

final_dataset <- merge(final_dataset,dep_facility,by="DATE",all.x=T)
rm(dep_facility)
final_dataset <- merge(final_dataset,mg_lending,by="DATE",all.x=T)
rm(mg_lending)
final_dataset <- merge(final_dataset,ref_ops,by="DATE",all.x=T)
rm(ref_ops)

final_dataset <- merge(final_dataset,events,by="DATE",all.x=T)
rm(events)

final_dataset <- merge(final_dataset,ecb_liq,by="DATE",all.x=T)
rm(ecb_liq)

final_dataset <- merge(final_dataset,exc_res_US,by="DATE",all.x=T)
rm(exc_res_US)

final_dataset$DATE <- as.Date(final_dataset$DATE,format="%d/%m/%Y")
final_dataset <- merge(final_dataset,data_IT_important,by="DATE",all.x=T)

#####################################
### YIELD CURVES

### IT
IT <- reshape(zero_merged_IT,direction="long",sep="_",varying=names(zero_merged_IT)[-1])
IT <- IT[with(IT, order(DATE, time)), ]

### ES
ES <- reshape(zero_merged_ES,direction="long",sep="_",varying=names(zero_merged_ES)[-1])
ES <- ES[with(ES, order(DATE, time)), ]

### DE
DE <- reshape(zero_merged_DE,direction="long",sep="_",varying=names(zero_merged_DE)[-1])
DE <- DE[with(DE, order(DATE, time)), ]

### FR
FR <- reshape(zero_merged_FR,direction="long",sep="_",varying=names(zero_merged_FR)[-1])
FR <- FR[with(FR, order(DATE, time)), ]

### OIS
OIS <- reshape(merged_OIS,direction="long",v.names="USSO",varying=list(2:8))

OIS[which(OIS$time==1),"time"] <- 12
OIS[which(OIS$time==2),"time"] <- 1
#US[which(US$time==3),"time"] <- 3
OIS[which(OIS$time==4),"time"] <- 24
OIS[which(OIS$time==6),"time"] <- 60
OIS[which(OIS$time==5),"time"] <- 6
OIS[which(OIS$time==7),"time"] <- 120

OIS <- OIS[with(OIS, order(DATE, time)), ]

### EONIA
EONIA <- reshape(merged_EONIA,direction="long",v.names="EUSWE",varying=list(2:7))

EONIA[which(EONIA$time==4),"time"] <- 1 # 12
EONIA[which(EONIA$time==5),"time"] <- 5 # 60
EONIA[which(EONIA$time==6),"time"] <- 10 # 120
EONIA[which(EONIA$time==3),"time"] <- 0.5 # 6 
EONIA[which(EONIA$time==2),"time"] <- 0.25 # 3

EONIA <- EONIA[with(EONIA, order(DATE, time)), ]

########################################
########################################
#### DATA VISUALIZATON - ANIMATIONS
########################################
########################################

## ECB ANNOUNCEMENTS
#DATE OF ANNOUNCEMENTS
ECB_event_date <- final_dataset[which(final_dataset$ECB_ALL_EVENTS==1),"DATE"]
ECB_event <- final_dataset[which(final_dataset$ECB_ALL_EVENTS==1),"ECB_EVENT"]

####################
####################
## PSPP

# img <- image_graph(800, 600, res = 96)
# 
# j <- 8 # PSPP Announced
# day_of_interest_DE <- DE$id[grep(ECB_event_date[j],DE$DATE)[1]]
# day_of_interest_IT <- IT$id[grep(ECB_event_date[j],IT$DATE)[1]]
# day_of_interest_FR <- FR$id[grep(ECB_event_date[j],FR$DATE)[1]]
# day_of_interest_ES <- ES$id[grep(ECB_event_date[j],ES$DATE)[1]]
# day_of_interest_EONIA <- EONIA$id[grep(ECB_event_date[j],EONIA$DATE)[1]]
# 
# day_of_interest_DE_enf <- DE$id[grep(ECB_event_date[j+1],DE$DATE)[1]]
# day_of_interest_IT_enf <- IT$id[grep(ECB_event_date[j+1],IT$DATE)[1]]
# day_of_interest_FR_enf <- FR$id[grep(ECB_event_date[j+1],FR$DATE)[1]]
# day_of_interest_ES_enf <- ES$id[grep(ECB_event_date[j+1],ES$DATE)[1]]
# day_of_interest_EONIA_enf <- EONIA$id[grep(ECB_event_date[j+1],EONIA$DATE)[1]]
# 
# ids_important <- cbind(seq(from=(day_of_interest_DE-10),to=(day_of_interest_DE_enf+10),by=1),
#                        seq(from=(day_of_interest_IT-10),to=(day_of_interest_IT_enf+10),by=1),
#                        seq(from=(day_of_interest_FR-10),to=(day_of_interest_FR_enf+10),by=1),
#                        seq(from=(day_of_interest_ES-10),to=(day_of_interest_ES_enf+10),by=1),
#                        seq(from=(day_of_interest_EONIA-10),to=(day_of_interest_EONIA_enf+10),by=1))
# 
# for (k in 1:nrow(ids_important) ) {
#   
#   print(k)
#   
#   i_DE <- ids_important[k,1]
#   i_IT <- ids_important[k,2]
#   i_FR <- ids_important[k,3]
#   i_ES <- ids_important[k,4]
#   i_EONIA <- ids_important[k,5]
#   
#   Title_imp <- ""
#   if (i_DE<day_of_interest_DE) {
#     Title_imp <- "Before Announcement"
#   } else if (i_DE==day_of_interest_DE) {
#     Title_imp <- "ANNOUNCEMENT"
#   } else if (i_DE > day_of_interest_DE & i_DE<day_of_interest_DE_enf) {
#     Title_imp <- "Before enforcement"
#   } else if (i_DE == day_of_interest_DE_enf) {
#     Title_imp <- "ENFORCEMENT"
#   } else if (i_DE > day_of_interest_DE_enf) {
#     Title_imp <- "After enforcement"
#   }
#   
#   dataframe <- data.frame(cbind(DE[which(DE$id==i_DE),1:ncol(DE)],
#                                 IT[which(IT$id==i_IT),1:ncol(IT)],
#                                 FR[which(FR$id==i_FR),1:ncol(FR)],
#                                 ES[which(ES$id==i_ES),1:ncol(ES)]))
#   
#   p <- ggplot(dataframe, aes(time)) +
#     
#     geom_line (aes(y = ITALY, colour="IT")) + geom_point (aes(y = ITALY, colour="IT")) +
#     geom_line (aes(y = SPAIN, colour="ES")) + geom_point (aes(y = SPAIN, colour="ES")) +
#     geom_line (aes(y = FRANCE, colour="FR")) + geom_point (aes(y = FRANCE, colour="FR")) +
#     geom_line (aes(y = GERMANY, colour="DE")) + geom_point (aes(y = GERMANY, colour="DE")) +
#     
#     
#     ggtitle(paste0("PSPP ",Title_imp," - ",dataframe[1,"DATE"]) ) + xlab("Maturity (years)")+ scale_y_continuous(name="yield",limits=c(-0.5,3.5)) +
#     scale_colour_manual(name="Security",
#                         values=c(hue_pal()(4)[1:4]),
#                         breaks=c("IT", "ES","FR","DE"))
#   print(p)
#   
# }
# 
# dev.off()
# animation_PSPP_spd <- image_animate(img, fps = 2)
# print(animation_PSPP_spd)
# 
# output_ECB <- "//10.2.112.201/Bruegel/RESEARCH/Research Assistants/Catarina/Monetary Policy Channels/Animations/ECB"
# image_write(`animation_PSPP_spd`,paste0(output_ECB,"/PSPP_zero.gif"))
# 
# #########################
# #########################
# ##### ALL EVENTS
# 
# for (j in 1:length(ECB_event_date)) {
# 
#   img <- image_graph(800, 600, res = 96)
# 
#   print(ECB_event[j])
#   print(j)
#   
#   day_of_interest_DE <- DE$id[grep(ECB_event_date[j],DE$DATE)[1]]
#   day_of_interest_IT <- IT$id[grep(ECB_event_date[j],IT$DATE)[1]]
#   day_of_interest_FR <- FR$id[grep(ECB_event_date[j],FR$DATE)[1]]
#   day_of_interest_ES <- ES$id[grep(ECB_event_date[j],ES$DATE)[1]]
#   day_of_interest_EONIA <- EONIA$id[grep(ECB_event_date[j],EONIA$DATE)[1]]
# 
#   ids_important <- cbind(seq(from=(day_of_interest_DE - 10), to=(day_of_interest_DE + 10), by=1),
#                          seq(from=(day_of_interest_IT - 10), to=(day_of_interest_IT + 10), by=1),
#                          seq(from=(day_of_interest_FR - 10), to=(day_of_interest_FR + 10), by=1),
#                          seq(from=(day_of_interest_ES - 10), to=(day_of_interest_ES + 10), by=1),
#                          seq(from=(day_of_interest_EONIA-10), to=(day_of_interest_EONIA + 10), by=1))
# 
#   limit_sup <- max (
#   max(DE[which(DE$id %in% ids_important[,1]),"GERMANY"]),
#   max(IT[which(IT$id %in% ids_important[,2]),"ITALY"]),
#   max(FR[which(FR$id %in% ids_important[,3]),"FRANCE"]),
#   max(ES[which(ES$id %in% ids_important[,4]),"SPAIN"])
#   ) + 0.2
#   
#   limit_inf <- min (
#     min(DE[which(DE$id %in% ids_important[,1]),"GERMANY"]),
#     min(IT[which(IT$id %in% ids_important[,2]),"ITALY"]),
#     min(FR[which(FR$id %in% ids_important[,3]),"FRANCE"]),
#     min(ES[which(ES$id %in% ids_important[,4]),"SPAIN"]),-0.3
#   ) - 0.2
#   
#   
#   for (k in 1:nrow(ids_important) ) {
# 
#     print(k)
#     
#     i_DE <- ids_important[k,1]
#     i_IT <- ids_important[k,2]
#     i_FR <- ids_important[k,3]
#     i_ES <- ids_important[k,4]
#     i_EONIA <- ids_important[k,5]
# 
#     dataframe <- data.frame(cbind(DE[which(DE$id==i_DE),1:ncol(DE)],
#                                   IT[which(IT$id==i_IT),1:ncol(IT)],
#                                   FR[which(FR$id==i_FR),1:ncol(FR)],
#                                   ES[which(ES$id==i_ES),1:ncol(ES)]))
#                                   
#     p <- ggplot(dataframe, aes(time)) +
#       
#       geom_line (aes(y = ITALY, colour="IT")) + geom_point (aes(y = ITALY, colour="IT")) +
#       geom_line (aes(y = SPAIN, colour="ES")) + geom_point (aes(y = SPAIN, colour="ES")) +
#       geom_line (aes(y = FRANCE, colour="FR")) + geom_point (aes(y = FRANCE, colour="FR")) +
#       geom_line (aes(y = GERMANY, colour="DE")) + geom_point (aes(y = GERMANY, colour="DE")) +
#       ggtitle(paste0(ECB_event[j]," (t= ",k-11," )"," - ",dataframe[1,"DATE"]) ) + xlab("Maturity (years)")+ scale_y_continuous(name="yield",limits=c(limit_inf,limit_sup)) +
#       scale_colour_manual(name="Security",
#                           values=c(hue_pal()(4)[1:4]),
#                           breaks=c("IT", "ES","FR","DE"))
#     print(p)
# 
#   }
# 
#   #plot.new()
#   frame()
#   dev.off()
#   animation <- image_animate(img, fps = 2)
#   
#   print(animation)
# 
#   image_write(animation,paste0(output_ECB,"/",ECB_event[j],".gif"))
#   
# }


#########################################
#########################################
### PRELIMINARY SUMMARY STATISTICS
#########################################
#########################################  

####################################################
### EFFECTS OF ECB ANNOUNCEMENTS ON THE BOND YIELDS
ECB_event_date <- final_dataset[which(final_dataset$ECB_ALL_EVENTS==1),"DATE"]
ECB_event <- final_dataset[which(final_dataset$ECB_ALL_EVENTS==1),"ECB_EVENT"]  
final_dataset$ECB_EXP_ANNOUNCEMENTS <- final_dataset$ECB_ONLY_ANNOUNCEMENTS*final_dataset$ECB_EXP

##table(final_dataset$ECB_EXP_ANNOUNCEMENTS)

diff_days_IT <- matrix(0,nrow=length(ECB_event_date),ncol=14)
diff_days_DE <- matrix(0,nrow=length(ECB_event_date),ncol=14)
diff_days_FR <- matrix(0,nrow=length(ECB_event_date),ncol=14)
diff_days_ES <- matrix(0,nrow=length(ECB_event_date),ncol=14)

diff_days_EONIA <- matrix(0,nrow=length(ECB_event_date),ncol=6)

for (j in 1:length(ECB_event_date)) {
  
  #j<-1
  ##DE
  index <- grep(ECB_event_date[j],zero_merged_DE$DATE)
  days <- zero_merged_DE[(index-1):(index+1),]
  days[,-1] <- days[,-1]*100
  diff_days_DE[j,] <- as.matrix(days[2,-1]-days[1,-1])
  
  ##IT
  index <- grep(ECB_event_date[j],zero_merged_IT$DATE)
  days <- zero_merged_IT[(index-1):(index+1),]
  days[,-1] <- days[,-1]*100
  diff_days_IT[j,] <- as.matrix(days[2,-1]-days[1,-1])
  
  ##FR
  index <- grep(ECB_event_date[j],zero_merged_FR$DATE)
  days <- zero_merged_FR[(index-1):(index+1),]
  days[,-1] <- days[,-1]*100
  diff_days_FR[j,] <- as.matrix(days[2,-1]-days[1,-1])
  
  ##ES
  index <- grep(ECB_event_date[j],zero_merged_ES$DATE)
  days <- zero_merged_ES[(index-1):(index+1),]
  days[,-1] <- days[,-1]*100
  diff_days_ES[j,] <- as.matrix(days[2,-1]-days[1,-1])
  
  ## EONIA
  index <- grep(ECB_event_date[j],merged_EONIA$DATE)
  days<- merged_EONIA[(index-1):(index+1),]
  days[,-1] <- days[,-1]
  diff_days_EONIA[j,] <- as.matrix((days[2,-1]-days[1,-1])*100)
  
}

change_DE <- data.frame(cbind(diff_days_DE,event=(ECB_event),date=data.frame(ECB_event_date) ))
change_IT <- data.frame(cbind(diff_days_IT,event=(ECB_event),date=data.frame(ECB_event_date) ))
change_FR <- data.frame(cbind(diff_days_FR,event=(ECB_event),date=data.frame(ECB_event_date) ))
change_ES <- data.frame(cbind(diff_days_ES,event=(ECB_event),date=data.frame(ECB_event_date) ))
change_EONIA <- data.frame(cbind(diff_days_EONIA,event=(ECB_event),date=data.frame(ECB_event_date) ))

names(change_DE)[1:14] <- c("0.25","0.5",seq(1,10,1),"15","20")
names(change_IT)[1:14] <- c("0.25","0.5",seq(1,10,1),"15","20")
names(change_FR)[1:14] <- c("0.25","0.5",seq(1,10,1),"15","20")
names(change_ES)[1:14] <- c("0.25","0.5",seq(1,10,1),"15","20")

output_dir <- "//10.2.112.201/Bruegel/RESEARCH/Research Assistants/Catarina/Monetary Policy Channels/Results/"

list_of_tables <- list("DE"=change_DE,"IT"=change_IT,"FR"=change_FR,
                        "ES"=change_ES,"EONIA"=change_EONIA)

openxlsx::write.xlsx(list_of_tables,file=paste0(output_dir,"DIFFS_zero.xlsx"),row.names=T)

#########################################
#########################################
### NAIVE, NON-FINANCIAL ANALYSIS
#########################################
#########################################
#########################################################################################
### AFTER CONTROLLING FOR ANNOUNCEMENTS, IS THERE ANY EFFECT OF THE ACTUAL PURCHASES?
### IF NOT, THEN, MAYBE ENDING QE / RAISING INTEREST RATES CAN HAVE LIMITED EFFECT IF EXPECTATIONS HAVE ADJUSTED
  
#ndiffs(final_dataset$GTDEM1Y.Govt,alpha = 0.05, test = c("adf"))
ndiffs(final_dataset$EXCESS.LIQUIDITY,alpha = 0.05, test = c("adf"))
ndiffs(final_dataset$EUORDEPO.Index,alpha = 0.05, test = c("adf"))
ndiffs(final_dataset$VIX_DAY,alpha = 0.05, test = c("adf"))

diff_DE <- diff(final_dataset$GERMANY_1,1)
diff_IT <- diff(final_dataset$ITALY_1,1)

diff_liq_L1 <- diff(as.numeric(final_dataset$EXCESS.LIQUIDITY),1)[-length(diff(final_dataset$EXCESS.LIQUIDITY,1))]
diff_depo_L1 <- diff(final_dataset$EUORDEPO.Index,1)[-length(diff(final_dataset$EUORDEPO.Index,1))]

diff_DE_L1 <- diff_DE[-length(diff_DE)]
diff_VIX_L1 <- diff(final_dataset$VIX_DAY,1)[-length(diff(final_dataset$VIX_DAY,1))]
diff_PSPP_L1 <- diff(final_dataset$Public.Sector.Purchase.Programme,1)[-length(diff(final_dataset$Public.Sector.Purchase.Programme,1))]
 
# auto.arima(diff_DE)
# log(as.numeric(diff_DE))
# 
# final_dataset$ECB_EXP_ANNOUNCEMENTS <- final_dataset$ECB_ONLY_ANNOUNCEMENTS*final_dataset$ECB_EXP
# 
# ols_wrong <- lm(diff_DE[-1] ~  diff_liq_L1 + diff_depo_L1 + final_dataset$ECB_ONLY_ANNOUNCEMENTS[-c(1,2)] + diff_DE_L1 + diff_VIX_L1)
# summary(ols_wrong)
# 
# auto.arima(diff_IT[-1],stepwise=FALSE,approx=FALSE)
# ols_wrong <- lm(diff_IT[-1] ~  diff_liq_L1 + diff_depo_L1 + final_dataset$ECB_EXP_ANNOUNCEMENTS[-c(1,2)] + diff_DE_L1 + diff_VIX_L1)
# summary(ols_wrong)
# 
# arimaIT <- Arima(diff_IT[-1], order=c(2,0,5), xreg=cbind(diff_depo_L1,announc=final_dataset$ECB_ONLY_ANNOUNCEMENTS[-c(1,2)],diff_VIX_L1) )
# coeftest(arimaIT)
# 
# diff_depo_L1[is.na(diff_depo_L1)] <- 0
# arimaIT <- Arima(diff_IT[-1], order=c(2,0,2), xreg=cbind(diff_liq_L1,diff_depo_L1,announc=final_dataset$ECB_EXP_ANNOUNCEMENTS[-c(1,2)],diff_VIX_L1) )
# coeftest(arimaIT)
# 
# summary(arimaIT)
# arimaIT$var.coef

#########################################
#########################################
### NAIVE FINANCIAL ANALYSIS
#########################################
#########################################

################################################################################
######## BASIC IDEA: ESTIMATE TYPICAL YIELD FORECASTING MODELS
######## AND SEE IF PUTTING EXCESS LIQUIDITY + ANNOUNCEMENTS IMPROVES THEM
################################################################################

######## PART 1: SEE IF THEY ARE SIGNIFICANT FOR THE LOADING FACTORS
#rate_IT <- as.matrix(merged_IT[,-1])
IT_rate_ts <- xts(zero_merged_IT,order.by=zero_merged_IT$DATE )[,-1]
maturity_IT <- c(3/12,0.5,seq(1,10,1),15,20)

DE_rate_ts <- xts(zero_merged_DE, order.by=zero_merged_DE$DATE )[,-1]
maturity_DE <- c(3/12,0.5,seq(1,10,1),15,20)

FR_rate_ts <- xts(zero_merged_FR, order.by=zero_merged_FR$DATE )[,-1]
maturity_FR <- c(3/12,0.5,seq(1,10,1),15,20)

ES_rate_ts <- xts(zero_merged_ES, order.by=zero_merged_ES$DATE )[,-1]
maturity_ES <- c(3/12,0.5,seq(1,10,1),15,20)

loadings <- function(maturity,ind) {
  
  level <- series_parameters[(ind),1]
  slope <- series_parameters[(ind),2]*(1-exp(-series_parameters[(ind),4]*maturity) )/(series_parameters[(ind),4]*maturity)
  curve <- series_parameters[(ind),3]*( (1-exp(-series_parameters[(ind),4]*maturity) )/(series_parameters[(ind),4]*maturity) - exp(-series_parameters[(ind),4]*maturity)  )
  
  return(cbind(level,slope,curve))
}

diff_days_param <- matrix(0,nrow=length(ECB_event_date),ncol=4)

M3_mat <- matrix(0,nrow=length(ECB_event_date),ncol=4)
M6_mat <- matrix(0,nrow=length(ECB_event_date),ncol=4)
Y1_mat <- matrix(0,nrow=length(ECB_event_date),ncol=4)
Y5_mat <- matrix(0,nrow=length(ECB_event_date),ncol=4)
Y10_mat <- matrix(0,nrow=length(ECB_event_date),ncol=4)

M3_matL1 <- matrix(0,nrow=length(ECB_event_date),ncol=4)
M6_matL1 <- matrix(0,nrow=length(ECB_event_date),ncol=4)
Y1_matL1 <- matrix(0,nrow=length(ECB_event_date),ncol=4)
Y5_matL1 <- matrix(0,nrow=length(ECB_event_date),ncol=4)
Y10_matL1 <- matrix(0,nrow=length(ECB_event_date),ncol=4)

ind_country <- 0

for (l in c("IT","ES","DE","FR")) {

  print(l)
  ind_country <- ind_country + 1
  
  NSparameters <- Nelson.Siegel( rate= get(paste0(l,"_rate_ts")), maturity=get(paste0("maturity_",l)))
  data_of_interest <- get(paste0("zero_merged_",l))
  
  ############################################
  ### PARAMETER SHIFTS AROUND ANNOUNCEMENTS

  series_parameters <- NSparameters
  View(series_parameters)
  df_param <- data.frame(DATE=index(series_parameters), coredata(series_parameters))
  
  ECB_event_date <- final_dataset[which(final_dataset$ECB_ALL_EVENTS==1),"DATE"]
  ECB_event <- final_dataset[which(final_dataset$ECB_ALL_EVENTS==1),"ECB_EVENT"] 
  
  for (j in 1:length(ECB_event_date)) {
    
    #j<-3
    ##IT
    index <- grep(ECB_event_date[j],df_param$DATE)
    days <- df_param[(index-1):(index+1),]
    diff_days_param[j,] <- as.matrix(days[2,-1]-days[1,-1])*100
    
    pred_y <- NSrates(series_parameters[(index-1):(index+1),], maturity_ES)
    
    ind_merged <- which(data_of_interest$DATE==ECB_event_date[j])
    
    actual_y3M <- data_of_interest[ind_merged, paste0(c("ITALY","SPAIN","GERMANY","FRANCE")[ind_country],"_0.25")]
    actual_y6M <- data_of_interest[ind_merged, paste0(c("ITALY","SPAIN","GERMANY","FRANCE")[ind_country],"_0.5")]
    actual_y1Y <- data_of_interest[ind_merged, paste0(c("ITALY","SPAIN","GERMANY","FRANCE")[ind_country],"_1")]
    actual_y5Y <- data_of_interest[ind_merged, paste0(c("ITALY","SPAIN","GERMANY","FRANCE")[ind_country],"_5")]
    actual_y10Y <- data_of_interest[ind_merged, paste0(c("ITALY","SPAIN","GERMANY","FRANCE")[ind_country],"_10")]
    
    actual_y3ML1 <- data_of_interest[ind_merged-1, paste0(c("ITALY","SPAIN","GERMANY","FRANCE")[ind_country],"_0.25")]
    actual_y6ML1 <- data_of_interest[ind_merged-1, paste0(c("ITALY","SPAIN","GERMANY","FRANCE")[ind_country],"_0.5")]
    actual_y1YL1 <- data_of_interest[ind_merged-1, paste0(c("ITALY","SPAIN","GERMANY","FRANCE")[ind_country],"_1")]
    actual_y5YL1 <- data_of_interest[ind_merged-1, paste0(c("ITALY","SPAIN","GERMANY","FRANCE")[ind_country],"_5")]
    actual_y10YL1 <- data_of_interest[ind_merged-1, paste0(c("ITALY","SPAIN","GERMANY","FRANCE")[ind_country],"_10")]
    
    M3_mat[j,] <- cbind(loadings(0.25,index),actual_y3M-pred_y[2,1]) #-loadings(0.25,(index-1)),pred_y)
    M6_mat[j,] <- cbind(loadings(0.5,index),actual_y6M-pred_y[2,2]) #-loadings(0.5,(index-1))
    Y1_mat[j,] <- cbind(loadings(1,index),actual_y1Y-pred_y[2,3]) #-loadings(1,(index-1))
    Y5_mat[j,] <- cbind(loadings(5,index),actual_y5Y-pred_y[2,7]) #-loadings(5,(index-1))
    Y10_mat[j,] <-cbind(loadings(10,index),actual_y10Y-pred_y[2,12]) #-loadings(10,(index-1))
    
    M3_matL1[j,] <- cbind(loadings(0.25,ind=(index-1)),actual_y3ML1-pred_y[1,1]) #-loadings(0.25,(index-1)),pred_y)
    M6_matL1[j,] <- cbind(loadings(0.5,ind=(index-1)),actual_y6ML1-pred_y[1,2]) #-loadings(0.5,(index-1))
    Y1_matL1[j,] <- cbind(loadings(1,ind=(index-1)),actual_y1YL1-pred_y[1,3]) #-loadings(1,(index-1))
    Y5_matL1[j,] <- cbind(loadings(5,ind=(index-1)),actual_y5YL1-pred_y[1,7]) #-loadings(5,(index-1))
    Y10_matL1[j,] <- cbind(loadings(10,ind=(index-1)),actual_y10YL1-pred_y[1,12]) #-loadings(10,(index-1))
    
  }

  assign(paste0("diff_days_param","_",l),diff_days_param)
  
  assign(paste0("M3_mat","_",l),M3_mat)
  assign(paste0("M6_mat","_",l),M6_mat)
  assign(paste0("Y1_mat","_",l),Y1_mat)
  assign(paste0("Y5_mat","_",l),Y5_mat)
  assign(paste0("Y10_mat","_",l),Y10_mat)
  
  assign(paste0("M3_matL1","_",l),M3_matL1)
  assign(paste0("M6_matL1","_",l),M6_matL1)
  assign(paste0("Y1_matL1","_",l),Y1_matL1)
  assign(paste0("Y5_matL1","_",l),Y5_matL1)
  assign(paste0("Y10_matL1","_",l),Y10_matL1)
 
  assign(paste0("NSparameters","_",l),NSparameters)
  
}

diff_days_param_IT <- data.frame(cbind(diff_days_param_IT,event=(ECB_event),date=data.frame(ECB_event_date),diff_days_IT,M3_mat_IT,M3_matL1_IT,
                                       M6_mat_IT,M6_matL1_IT,Y1_mat_IT,Y1_matL1_IT,Y5_mat_IT,Y5_matL1_IT,Y10_mat_IT,Y10_matL1_IT))

diff_days_param_DE <- data.frame(cbind(diff_days_param_DE,event=(ECB_event),date=data.frame(ECB_event_date),diff_days_DE,M3_mat_DE,M3_matL1_DE,
                                       M6_mat_DE,M6_matL1_DE,Y1_mat_DE,Y1_matL1_DE,Y5_mat_DE,Y5_matL1_DE,Y10_mat_DE,Y10_matL1_DE))

diff_days_param_FR <- data.frame(cbind(diff_days_param_FR,event=(ECB_event),date=data.frame(ECB_event_date),diff_days_FR,M3_mat_FR,M3_matL1_FR,
                                       M6_mat_FR,M6_matL1_FR,Y1_mat_FR,Y1_matL1_FR,Y5_mat_FR,Y5_matL1_FR,Y10_mat_FR,Y10_matL1_FR))

diff_days_param_ES <- data.frame(cbind(diff_days_param_ES,event=(ECB_event),date=data.frame(ECB_event_date),diff_days_ES,M3_mat_ES,M3_matL1_ES,
                                       M6_mat_ES,M6_matL1_ES,Y1_mat_ES,Y1_matL1_ES,Y5_mat_ES,Y5_matL1_ES,Y10_mat_ES,Y10_matL1_ES))

rename_df <- function(df) {
names(df)[1:4] <- c("beta_0","beta_1","beta_2","lambda")
names(df)[7:20] <- c("0.25","0.5",seq(1,10,by=1),"15","20")
names(df)[21:24] <- c("load_0_M3","load_1_M3","load_2_M3","error_M3")
names(df)[25:28] <- c("load_0_M3L1","load_1_M3L1","load_2_M3L1","error_M3L1")
names(df)[29:32] <- c("load_0_M6","load_1_M6","load_2_M6","error_M6")
names(df)[33:36] <- c("load_0_M6L1","load_1_M6L1","load_2_M6L1","error_M6L1")
names(df)[37:40] <- c("load_0_Y1","load_1_Y1","load_2_Y1","error_Y1")
names(df)[41:44] <- c("load_0_Y1L1","load_1_Y1L1","load_2_Y1L1","error_Y1L1")
names(df)[45:48] <- c("load_0_Y5","load_1_Y5","load_2_Y5","error_Y5")
names(df)[49:52] <- c("load_0_Y5L1","load_1_Y5L1","load_2_Y5L1","error_Y5L1")
names(df)[53:56] <- c("load_0_Y10","load_1_Y10","load_2_Y10","error_Y10")
names(df)[57:60] <- c("load_0_Y10L1","load_1_Y10L1","load_2_Y10L1","error_Y10L1")
df
}

list_countries <- list("IT"=diff_days_param_IT,
                       "DE"=diff_days_param_DE,
                       "FR"=diff_days_param_FR,
                       "ES"=diff_days_param_ES)

list_countries <- lapply(list_countries,rename_df)

openxlsx::write.xlsx(list_countries,file=paste0(output_dir,"DIFFS_loadings.xlsx"),row.names=T)

##########################################
##### ECONOMETRICS #######################
##########################################

## IT
NSparameters <- NSparameters_IT
ndiffs(NSparameters[,1],alpha = 0.05, test = c("adf"))
ndiffs(NSparameters[,2],alpha = 0.05, test = c("adf"))
ndiffs(NSparameters[,3],alpha = 0.05, test = c("adf"))
ndiffs(NSparameters[,4],alpha = 0.05, test = c("adf"))

ndiffs(final_dataset$EXCESS.LIQUIDITY,alpha = 0.05, test = c("adf"))
ndiffs(final_dataset$EUORDEPO.Index,alpha = 0.05, test = c("adf"))

series_parameters_IT <- NSparameters_IT
series_parameters_DE <- NSparameters_DE
series_parameters_FR <- NSparameters_FR
series_parameters_ES <- NSparameters_ES

series_parameters_diff_IT <- diff(NSparameters_IT,1)
series_parameters_diff_DE <- diff(NSparameters_DE,1)
series_parameters_diff_FR <- diff(NSparameters_FR,1)
series_parameters_diff_ES <- diff(NSparameters_ES,1)

names(series_parameters_diff) <- paste0(names(series_parameters_diff),"_diff")
final_dataset_ts <- xts(final_dataset,order.by = final_dataset$DATE)

# nrow(series_parameters)
# nrow(VAR_dataset$EXCESS.LIQUIDITY)

VAR_dataset <- merge.xts(series_parameters_IT,series_parameters_DE,series_parameters_FR,series_parameters_ES,final_dataset_ts,join="left")
#VAR_dataset <- merge.xts(VAR_dataset,series_parameters_diff,join="left")
nrow(VAR_dataset)

VAR_dataset$liq_diff <- diff(VAR_dataset$EXCESS.LIQUIDITY,1)
VAR_dataset$depo_diff <- diff(VAR_dataset$EUORDEPO.Index,1)


######## PART 1: GRANGER CAUSALITY 

VARselect(series_parameters_IT) # tells you what the lag is, to go down there in command VAR
var.basic <- VAR(series_parameters_IT,7,type="none") # the 1 here is the lag order.
summary(var.basic)

mon_policy_vars <- xts(cbind(VAR_dataset$liq_diff,VAR_dataset$ECB_EXP_ANNOUNCEMENTS,VAR_dataset$depo_diff))
#mon_policy_vars <- xts(cbind(VAR_dataset$liq_diff,VAR_dataset$depo_diff))
lag_mon_policy_vars <- mon_policy_vars[-nrow(mon_policy_vars),]
series_parameters <- xts(cbind(VAR_dataset$beta_0,VAR_dataset$beta_1,VAR_dataset$beta_2))[-1,]

all <- cbind(series_parameters,lag_mon_policy_vars)
go <- apply(all,1, function(x) {sum(is.na(x))})
all2 <- all[which(go==0),]

#######################
#### ALL EXOGENOUS ####
var.liq <- VAR(cbind(all2$beta_0,all2$beta_1,all2$beta_2),7,type="none",exogen=cbind(all2$liq_diff,all2$depo_diff,all2$ECB_EXP_ANNOUNCEMENTS))
summary(var.liq)
# RESULT: NO GRANGER CAUSALITY
## HOWEVER, NOTE THAT MOVES IN THESE CORRELATE WITH HIGHER LEVELS OF YIELDS; EITHER INCLUDE OTHER "CONTROL VARIABLES" OR WORK IN DIFFERENCES

#########################
#### ENDOGENOUS, ALL ####
var.liq_end <- VAR(all2,7,type="none")
summary(var.liq_end)

### EXCESS LIQUIDITY DIFFERENCES
IRF_liq_beta0 <- irf(var.liq_end,impulse="liq_diff",response="beta_0",cumulative = T) ## NO CLEAR LEVEL CHANGE, NEGATIVE
IRF_liq_beta1 <- irf(var.liq_end,impulse="liq_diff",response="beta_1",cumulative = T) ## REDUCES SLOPE
IRF_liq_beta2 <- irf(var.liq_end,impulse="liq_diff",response="beta_2",cumulative = T) ## REDUCES CURVATURE

### DEPOSIT FACILITY INTEREST RATE DIFFERENCES
IRF_depo_beta0 <- irf(var.liq_end,impulse="depo_diff",response="beta_0",cumulative = T) ## NO CLEAR LEVEL CHANGE, NEGATIVE
IRF_depo_beta1 <- irf(var.liq_end,impulse="depo_diff",response="beta_1",cumulative = T) ## NO CLEAR SLOPE CHANGE, POSITIVE
IRF_depo_beta2 <- irf(var.liq_end,impulse="depo_diff",response="beta_2",cumulative = T) ## NO CLEAR CURVATURE CHANGE, NEGATIVE

IRF_liq_beta0 <- cbind(data.frame(IRF_liq_beta0$irf),data.frame(IRF_liq_beta0$Lower),data.frame(IRF_liq_beta0$Upper))
IRF_liq_beta1 <- cbind(data.frame(IRF_liq_beta1$irf),data.frame(IRF_liq_beta1$Lower),data.frame(IRF_liq_beta1$Upper))
IRF_liq_beta2 <- cbind(data.frame(IRF_liq_beta2$irf),data.frame(IRF_liq_beta2$Lower),data.frame(IRF_liq_beta2$Upper))

end_ <- data.frame(cbind(IRF_liq_beta0,IRF_liq_beta1,IRF_liq_beta2))

#############################################
#### ENDOGENOUS, ANNOUNCEMENTS EXOGENOUS ####
var.liq_end_xannounc <- VAR(all2[,-5],7,type="none",exogen=all2$ECB_EXP_ANNOUNCEMENTS)
summary(var.liq_end_xannounc)

### EXCESS LIQUIDITY DIFFERENCES
IRF_liq_beta0_xannounc <- irf(var.liq_end_xannounc,impulse="liq_diff",response="beta_0",cumulative = T) ## NO CLEAR LEVEL CHANGE, NEGATIVE
IRF_liq_beta1_xannounc <- irf(var.liq_end_xannounc,impulse="liq_diff",response="beta_1",cumulative = T) ## REDUCES SLOPE
IRF_liq_beta2_xannounc <- irf(var.liq_end_xannounc,impulse="liq_diff",response="beta_2",cumulative = T) ## REDUCES CURVATURE EXTREMELY

### DEPOSIT FACILITY INTEREST RATE DIFFERENCES
IRF_depo_beta0_xannounc <- irf(var.liq_end_xannounc,impulse="depo_diff",response="beta_0",cumulative = T) ## NO CLEAR LEVEL CHANGE, NEGATIVE
IRF_depo_beta1_xannounc <- irf(var.liq_end_xannounc,impulse="depo_diff",response="beta_1",cumulative = T) ## INCREASES SLOPE AFTER SOME DAYS (??)
IRF_depo_beta2_xannounc <- irf(var.liq_end_xannounc,impulse="depo_diff",response="beta_2",cumulative = T) ## NO CLEAR CURVATURE CHANGE, NEGATIVE

IRF_liq_beta0_xannounc <- cbind(data.frame(IRF_liq_beta0_xannounc$irf),data.frame(IRF_liq_beta0_xannounc$Lower),data.frame(IRF_liq_beta0_xannounc$Upper))
IRF_liq_beta1_xannounc <- cbind(data.frame(IRF_liq_beta1_xannounc$irf),data.frame(IRF_liq_beta1_xannounc$Lower),data.frame(IRF_liq_beta1_xannounc$Upper))
IRF_liq_beta2_xannounc <- cbind(data.frame(IRF_liq_beta2_xannounc$irf),data.frame(IRF_liq_beta2_xannounc$Lower),data.frame(IRF_liq_beta2_xannounc$Upper))

end_xannounc <- data.frame(cbind(IRF_liq_beta0_xannounc,IRF_liq_beta1_xannounc,IRF_liq_beta2_xannounc))

View(IRF_liq_beta1_xannounc)

#############################################
#### ENDOGENOUS, NO ANNOUNCEMENTS ###########
var.liq_end_noannounc <- VAR(all2[,-5],7,type="none")

### EXCESS LIQUIDITY DIFFERENCES
IRF_liq_beta0_noannounc <- irf(var.liq_end_noannounc,impulse="liq_diff",response="beta_0",cumulative = T) ## NO CLEAR LEVEL CHANGE, NEGATIVE
IRF_liq_beta1_noannounc <- irf(var.liq_end_noannounc,impulse="liq_diff",response="beta_1",cumulative = T) ## REDUCES SLOPE
IRF_liq_beta2_noannounc <- irf(var.liq_end_noannounc,impulse="liq_diff",response="beta_2",cumulative = T) ## REDUCES CURVATURE EXTREMELY

### DEPOSIT FACILITY INTEREST RATE DIFFERENCES
IRF_depo_beta0_noannounc <- irf(var.liq_end_noannounc,impulse="depo_diff",response="beta_0",cumulative = T) ## NO CLEAR LEVEL CHANGE, NEGATIVE
IRF_depo_beta1_noannounc <- irf(var.liq_end_noannounc,impulse="depo_diff",response="beta_1",cumulative = T) ## NO CLEAR SLOPE CHANGE, POSITIVE
IRF_depo_beta2_noannounc <- irf(var.liq_end_noannounc,impulse="depo_diff",response="beta_2",cumulative = T) ## NO CLEAR CURVATURE CHANGE, NEGATIVE

IRF_liq_beta0_noannounc <- cbind(data.frame(IRF_liq_beta0_noannounc$irf),data.frame(IRF_liq_beta0_noannounc$Lower),data.frame(IRF_liq_beta0_noannounc$Upper))
IRF_liq_beta1_noannounc <- cbind(data.frame(IRF_liq_beta1_noannounc$irf),data.frame(IRF_liq_beta1_noannounc$Lower),data.frame(IRF_liq_beta1_noannounc$Upper))
IRF_liq_beta2_noannounc <- cbind(data.frame(IRF_liq_beta2_noannounc$irf),data.frame(IRF_liq_beta2_noannounc$Lower),data.frame(IRF_liq_beta2_noannounc$Upper))

end_noannounc <- data.frame(cbind(IRF_liq_beta0_noannounc,IRF_liq_beta1_noannounc,IRF_liq_beta2_noannounc))

#################################################
#### ENDOGENOUS ONLY EXCESS LIQUIDITY ###########
var.liq_endliq <- VAR(all2[,-c(5,6)],7,type="none")

### EXCESS LIQUIDITY DIFFERENCES
IRF_liq_beta0_liq <- irf(var.liq_endliq,impulse="liq_diff",response="beta_0",cumulative = T) ## NO CLEAR LEVEL CHANGE, POSITIVE
IRF_liq_beta1_liq <- irf(var.liq_endliq,impulse="liq_diff",response="beta_1",cumulative = T) ## REDUCES SLOPE
IRF_liq_beta2_liq <- irf(var.liq_endliq,impulse="liq_diff",response="beta_2",cumulative = T) ## REDUCES CURVATURE

IRF_liq_beta0_liq <- cbind(data.frame(IRF_liq_beta0_liq$irf),data.frame(IRF_liq_beta0_liq$Lower),data.frame(IRF_liq_beta0_liq$Upper))
IRF_liq_beta1_liq <- cbind(data.frame(IRF_liq_beta1_liq$irf),data.frame(IRF_liq_beta1_liq$Lower),data.frame(IRF_liq_beta1_liq$Upper))
IRF_liq_beta2_liq <- cbind(data.frame(IRF_liq_beta2_liq$irf),data.frame(IRF_liq_beta2_liq$Lower),data.frame(IRF_liq_beta2_liq$Upper))

end_liq <- data.frame(cbind(IRF_liq_beta0_liq,IRF_liq_beta1_liq,IRF_liq_beta2_liq))

openxlsx::write.xlsx()

################################################
#### IN DIFFERENCES ############################


######## PART 2: STRUCTURAL BREAKS 

######## PART 3: SEE IF THEY IMPROVE FORECASTS (MSPE), SEPARATELY FOR DIFFERENT MATURITIES
