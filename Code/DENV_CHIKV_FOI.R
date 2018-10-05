### Mailo Numazu, Stanford University

## Data: Expanded Visits AIC (Acutely Ill COhort) Surveys 

## Purpose: Find FOI for DENV/CHIKV for all four sites & observe travel patterns of + individuals




# Create additional infection and exposure dates & subset by site --------------------------------------------------------------------

rm(list=ls()) #remove previous variable assignments

# Set working directory
setwd("C:/Users/mailo/OneDrive/Desktop/Stanford 18-19/BSURPS/Working Directory")

# expanded_aic <- read.csv("C:/Users/Jamie/Box Sync/FOI Kenya Project/aic_results_for_mailo_all_visits.csv", header = T, stringsAsFactors = F)
EV_aic <- read.csv("C:/Users/mailo/OneDrive/Desktop/Stanford 18-19/BSURPS/Working Directory/expanded_visits_aic.csv", header = T, stringsAsFactors = F)

# convert interview date to date class 
# expanded_aic$interview_date_aic <- as.Date(expanded_aic$interview_date_aic, "%Y-%m-%d") #"%m/%d/%Y"
EV_aic$interview_date_aic <- as.Date(EV_aic$interview_date_aic, "%m/%d/%Y") #"%m/%d/%Y"

# all aic kids are febrile so if days with symptoms is na replace with zero as they had fever on day of visit 
EV_aic$date_symptom_onset[is.na(EV_aic$date_symptom_onset)] <- 0

# if kids had fever for more than 7 days treat as day 0
EV_aic$date_symptom_onset[EV_aic$date_symptom_onset>7] <- 0

# subset out any 'NA' dates
EV_aic <- subset(EV_aic, !is.na(interview_date_aic))

# exposure and infection start and end dates
EV_aic$date_start_infectious <- (EV_aic$interview_date_aic - EV_aic$date_symptom_onset)
EV_aic$date_end_infectious <- EV_aic$date_start_infectious + 4
EV_aic$date_start_exposed <- EV_aic$date_start_infectious - 7
EV_aic$date_end_exposed <- EV_aic$date_start_infectious - 1

# add seroconversion for igg at last visit (none yet)
EV_aic$hi_denv_stfd_igg <- 0
EV_aic$hi_chikv_stfd_igg <- 0

# create dataframe for infectious period for all kids
visitNames <- c("a", "b", "c", "d", "e", "f", "g", "h", "i")

# make NAs for DENV and CHIKV test results zeros 
EV_aic[,c(18:49)][is.na(EV_aic[,c(18:49)])] <- 0

for (i in 1:nrow(EV_aic)){
  visit <- substr(EV_aic$redcap_event_name[i], 7,7)
  d.seroconv <- paste0(visit, visitNames[which(visitNames==visit)+1], "_denv_stfd_igg")
  EV_aic$denv[i] <- ifelse(EV_aic[i,d.seroconv]==1|EV_aic$result_pcr_denv_kenya[i]==1|EV_aic$result_pcr_denv_stfd[i]==1|EV_aic$denv_result_ufi[i]==1, 1, 0)
  c.seroconv <- paste0(visit, visitNames[which(visitNames==visit)+1], "_chikv_stfd_igg")
  EV_aic$chikv[i] <- ifelse(EV_aic[i,c.seroconv]==1|EV_aic$result_pcr_chikv_kenya[i]==1|EV_aic$result_pcr_chikv_stfd[i]==1|EV_aic$chikv_result_ufi[i]==1, 1, 0)
}

# number weeks
EV_aic$epiwk <- strftime(EV_aic$date_start_infectious, format = "%Y-%W")
EV_aic$Date <- substr(EV_aic$date_start_infectious, 1, 7)

# aggregate data by epiweek
library(plyr)
epiweeks.C <- ddply(EV_aic, .(id_site, epiwk), summarise, chikv_positive = sum(chikv==1), chikv_negative = sum(chikv==0), proportion = chikv_positive/(chikv_negative + chikv_positive))
epiweeks.D <- ddply(EV_aic, .(id_site, epiwk), summarise, denv_positive = sum(denv==1), denv_negative = sum(denv==0), proportion = denv_positive/(denv_negative + denv_positive))

#x <- merge(epiweeks, expanded_aic[,c("epiwk", "Date")], by = "epiwk", all.x = T)

# CHIKV subset all 4 cities
msambweni.C <- subset(epiweeks.C, id_site == "Msambweni")
barplot(msambweni.C$chikv_positive)
chulaimbo.C <- subset(epiweeks.C, id_site == "Chulaimbo")
barplot(chulaimbo.C$chikv_positive)
kisumu.C <- subset(epiweeks.C, id_site == "Kisumu")
barplot(kisumu.C$chikv_positive)
ukunda.C <- subset(epiweeks.C, id_site == "Ukunda")
barplot(ukunda.C$chikv_positive)

# DENV subset all 4 cities
msambweni.D <- subset(epiweeks.D, id_site == "Msambweni")
barplot(msambweni.D$denv_positive)
chulaimbo.D <- subset(epiweeks.D, id_site == "Chulaimbo")
barplot(chulaimbo.D$denv_positive)
kisumu.D <- subset(epiweeks.D, id_site == "Kisumu")
barplot(kisumu.D$denv_positive)
ukunda.D <- subset(epiweeks.D, id_site == "Ukunda")
barplot(ukunda.D$denv_positive)



# Calculate FOI
# weighted values and model based on http://currents.plos.org/outbreaks/article/estimating-drivers-of-autochthonous-transmission-of-chikungunya-virus-in-its-invasion-of-the-americas/
weights <- c(0.011, 0.187, 0.432, 0.287, 0.083)
S.inits <- c(14444, 75357, 216479, 10000) # msambweni, ukunda, kisumu, chulaimbo(no pop. data for chulaimbo)

# make sure that dates are continuous(for missing weeks, find case numbers by averaging b/w previous and after week) ------------------
# install.packages("DataCombine")
library(DataCombine)
# Create new rows

# Chulaimbo
#CHIKV
wk14.51.c.C <- c("Chulaimbo", "2014-51", 0, 4, 0.000)
wk14.52.c.C <- c("Chulaimbo", "2014-52", 0, 2, 0.000)
wk15.41.c.C <- c("Chulaimbo", "2015-51", 0, 5, 0.000)
wk15.45.c.C <- c("Chulaimbo", "2015-45", 0, 4, 0.000)
wk15.51.c.C <- c("Chulaimbo", "2015-51", 0, 2, 0.000)
wk15.52.c.C <- c("Chulaimbo", "2015-52", 0, 3, 0.000)
wk16.52.c.C <- c("Chulaimbo", "2016-52", 0, 3, 0.000)
wk17.31.c.C <- c("Chulaimbo", "2017-31", 0, 2, 0.000)
wk17.32.c.C <- c("Chulaimbo", "2017-32", 0, 3, 0.000)
wk17.51.c.C <- c("Chulaimbo", "2017-51", 0, 3, 0.000)
wk17.52.c.C <- c("Chulaimbo", "2017-52", 0, 5, 0.000)
wk18.09.c.C <- c("Chulaimbo", "2018-09", 0, 6, 0.000)
wk18.19.c.C <- c("Chulaimbo", "2018-19", 0, 6, 0.000)
#DENV
wk14.51.c.D <- c("Chulaimbo", "2014-51", 0, 4, 0.000)
wk14.52.c.D <- c("Chulaimbo", "2014-52", 0, 2, 0.000)
wk15.41.c.D <- c("Chulaimbo", "2015-51", 0, 5, 0.000)
wk15.45.c.D <- c("Chulaimbo", "2015-45", 0, 4, 0.000)
wk15.51.c.D <- c("Chulaimbo", "2015-51", 0, 2, 0.000)
wk15.52.c.D <- c("Chulaimbo", "2015-52", 0, 3, 0.000)
wk16.52.c.D <- c("Chulaimbo", "2016-52", 0, 3, 0.000)
wk17.31.c.D <- c("Chulaimbo", "2017-31", 0, 2, 0.000)
wk17.32.c.D <- c("Chulaimbo", "2017-32", 0, 3, 0.000)
wk17.51.c.D <- c("Chulaimbo", "2017-51", 0, 3, 0.000)
wk17.52.c.D <- c("Chulaimbo", "2017-52", 0, 5, 0.000)
wk18.09.c.D <- c("Chulaimbo", "2018-09", 0, 6, 0.000)
wk18.19.c.D <- c("Chulaimbo", "2018-19", 0, 6, 0.000)

# Kisumu
#CHIKV
wk14.51.k.C <- c("Kisumu", "2014-51", 0, 4, 0.000)
wk14.52.k.C <- c("Kisumu", "2014-52", 0, 2, 0.000)
wk15.41.k.C <- c("Kisumu", "2015-51", 0, 5, 0.000)
wk15.45.k.C <- c("Kisumu", "2015-45", 0, 4, 0.000)
wk15.51.k.C <- c("Kisumu", "2015-51", 0, 2, 0.000)
wk15.52.k.C <- c("Kisumu", "2015-52", 0, 3, 0.000)
wk16.52.k.C <- c("Kisumu", "2016-52", 0, 3, 0.000)
wk17.31.k.C <- c("Kisumu", "2017-31", 0, 2, 0.000)
wk17.32.k.C <- c("Kisumu", "2017-32", 0, 3, 0.000)
wk17.51.k.C <- c("Kisumu", "2017-51", 0, 3, 0.000)
wk17.52.k.C <- c("Kisumu", "2017-52", 0, 5, 0.000)
wk18.09.k.C <- c("Kisumu", "2018-09", 0, 6, 0.000)
wk18.19.k.C <- c("Kisumu", "2018-19", 0, 6, 0.000)
#DENV
wk14.51.k.D <- c("Kisumu", "2014-51", 0, 4, 0.000)
wk14.52.k.D <- c("Kisumu", "2014-52", 0, 2, 0.000)
wk15.41.k.D <- c("Kisumu", "2015-51", 0, 5, 0.000)
wk15.45.k.D <- c("Kisumu", "2015-45", 0, 4, 0.000)
wk15.51.k.D <- c("Kisumu", "2015-51", 0, 2, 0.000)
wk15.52.k.D <- c("Kisumu", "2015-52", 0, 3, 0.000)
wk16.52.k.D <- c("Kisumu", "2016-52", 0, 3, 0.000)
wk17.31.k.D <- c("Kisumu", "2017-31", 0, 2, 0.000)
wk17.32.k.D <- c("Kisumu", "2017-32", 0, 3, 0.000)
wk17.51.k.D <- c("Kisumu", "2017-51", 0, 3, 0.000)
wk17.52.k.D <- c("Kisumu", "2017-52", 0, 5, 0.000)
wk18.09.k.D <- c("Kisumu", "2018-09", 0, 6, 0.000)
wk18.19.k.D <- c("Kisumu", "2018-19", 0, 6, 0.000)

# Msambweni
#CHIKV
wk15.51.m.C <- c("Msambweni", "2015-51", 0, 22, 0.000)
wk15.52.m.C <- c("Msambweni", "2015-52", 0, 12, 0.000)
wk16.49.m.C <- c("Msambweni", "2016-49", 0, 12, 0.000)
wk16.50.m.C <- c("Msambweni", "2016-50", 0, 9, 0.000)
wk16.51.m.C <- c("Msambweni", "2016-51", 0, 6, 0.000)
wk16.52.m.C <- c("Msambweni", "2016-52", 0, 3, 0.000)
wk17.51.m.C <- c("Msambweni", "2017-51", 2, 4, 0.415)
wk18.23.m.C <- c("Msambweni", "2018-23", 0, 6, 0.000)
#DENV
wk15.51.m.D <- c("Msambweni", "2015-51", 0, 22, 0.000)
wk15.52.m.D <- c("Msambweni", "2015-52", 0, 12, 0.000)
wk16.49.m.D <- c("Msambweni", "2016-49", 0, 12, 0.000)
wk16.50.m.D <- c("Msambweni", "2016-50", 0, 9, 0.000)
wk16.51.m.D <- c("Msambweni", "2016-51", 0, 6, 0.000)
wk16.52.m.D <- c("Msambweni", "2016-52", 0, 3, 0.000)
wk17.51.m.D <- c("Msambweni", "2017-51", 0, 7, 0.000)
wk18.23.m.D <- c("Msambweni", "2018-23", 0, 6, 0.000)

# Ukunda
#CHIKV
wk16.49.u.C <- c("Ukunda", "2014-51", 0, 12, 0.000)
wk16.50.u.C <- c("Ukunda", "2014-52", 0, 15, 0.000)
wk16.51.u.C <- c("Ukunda", "2015-51", 0, 18, 0.000)
wk16.52.u.C <- c("Ukunda", "2015-45", 0, 21, 0.000)
wk17.51.u.C <- c("Ukunda", "2015-51", 2, 2, 0.402)
wk17.52.u.C <- c("Ukunda", "2015-52", 6, 4, 0.544)
#DENV
wk16.49.u.D <- c("Ukunda", "2014-51", 0, 12, 0.000)
wk16.50.u.D <- c("Ukunda", "2014-52", 0, 15, 0.000)
wk16.51.u.D <- c("Ukunda", "2015-51", 0, 18, 0.000)
wk16.52.u.D <- c("Ukunda", "2015-45", 0, 21, 0.000)
wk17.51.u.D <- c("Ukunda", "2015-51", 0, 5, 0.000)
wk17.52.u.D <- c("Ukunda", "2015-52", 0, 10, 0.000)

# Insert new rows
# Chulaimbo CHIKV
chulaimbo.C <- InsertRow(chulaimbo.C, NewRow = wk14.51.c.C, RowNum = 222)
chulaimbo.C <- InsertRow(chulaimbo.C, NewRow = wk14.52.c.C, RowNum = 223)
chulaimbo.C <- InsertRow(chulaimbo.C, NewRow = wk15.41.c.C, RowNum = 224)
chulaimbo.C <- InsertRow(chulaimbo.C, NewRow = wk15.45.c.C, RowNum = 225)
chulaimbo.C <- InsertRow(chulaimbo.C, NewRow = wk15.51.c.C, RowNum = 226)
chulaimbo.C <- InsertRow(chulaimbo.C, NewRow = wk15.52.c.C, RowNum = 227)
chulaimbo.C <- InsertRow(chulaimbo.C, NewRow = wk16.52.c.C, RowNum = 228)
chulaimbo.C <- InsertRow(chulaimbo.C, NewRow = wk17.31.c.C, RowNum = 229)
chulaimbo.C <- InsertRow(chulaimbo.C, NewRow = wk17.32.c.C, RowNum = 230)
chulaimbo.C <- InsertRow(chulaimbo.C, NewRow = wk17.51.c.C, RowNum = 231)
chulaimbo.C <- InsertRow(chulaimbo.C, NewRow = wk17.52.c.C, RowNum = 232)
chulaimbo.C <- InsertRow(chulaimbo.C, NewRow = wk18.09.c.C, RowNum = 233)
chulaimbo.C <- InsertRow(chulaimbo.C, NewRow = wk18.19.c.C, RowNum = 234)
# Chulaimbo DENV
chulaimbo.D <- InsertRow(chulaimbo.D, NewRow = wk14.51.c.D, RowNum = 222)
chulaimbo.D <- InsertRow(chulaimbo.D, NewRow = wk14.52.c.D, RowNum = 223)
chulaimbo.D <- InsertRow(chulaimbo.D, NewRow = wk15.41.c.D, RowNum = 224)
chulaimbo.D <- InsertRow(chulaimbo.D, NewRow = wk15.45.c.D, RowNum = 225)
chulaimbo.D <- InsertRow(chulaimbo.D, NewRow = wk15.51.c.D, RowNum = 226)
chulaimbo.D <- InsertRow(chulaimbo.D, NewRow = wk15.52.c.D, RowNum = 227)
chulaimbo.D <- InsertRow(chulaimbo.D, NewRow = wk16.52.c.D, RowNum = 228)
chulaimbo.D <- InsertRow(chulaimbo.D, NewRow = wk17.31.c.D, RowNum = 229)
chulaimbo.D <- InsertRow(chulaimbo.D, NewRow = wk17.32.c.D, RowNum = 230)
chulaimbo.D <- InsertRow(chulaimbo.D, NewRow = wk17.51.c.D, RowNum = 231)
chulaimbo.D <- InsertRow(chulaimbo.D, NewRow = wk17.52.c.D, RowNum = 232)
chulaimbo.D <- InsertRow(chulaimbo.D, NewRow = wk18.09.c.D, RowNum = 233)
chulaimbo.D <- InsertRow(chulaimbo.D, NewRow = wk18.19.c.D, RowNum = 234)
# Kisumu CHIKV
kisumu.C <- InsertRow(kisumu.C, NewRow = wk14.51.k.C, RowNum = 221)
kisumu.C <- InsertRow(kisumu.C, NewRow = wk14.52.k.C, RowNum = 222)
kisumu.C <- InsertRow(kisumu.C, NewRow = wk15.41.k.C, RowNum = 223)
kisumu.C <- InsertRow(kisumu.C, NewRow = wk15.45.k.C, RowNum = 224)
kisumu.C <- InsertRow(kisumu.C, NewRow = wk15.51.k.C, RowNum = 225)
kisumu.C <- InsertRow(kisumu.C, NewRow = wk15.51.k.C, RowNum = 226)
kisumu.C <- InsertRow(kisumu.C, NewRow = wk16.52.k.C, RowNum = 227)
kisumu.C <- InsertRow(kisumu.C, NewRow = wk17.31.k.C, RowNum = 228)
kisumu.C <- InsertRow(kisumu.C, NewRow = wk17.32.k.C, RowNum = 229)
kisumu.C <- InsertRow(kisumu.C, NewRow = wk17.51.k.C, RowNum = 230)
kisumu.C <- InsertRow(kisumu.C, NewRow = wk17.52.k.C, RowNum = 231)
kisumu.C <- InsertRow(kisumu.C, NewRow = wk18.09.k.C, RowNum = 232)
kisumu.C <- InsertRow(kisumu.C, NewRow = wk18.19.k.C, RowNum = 233)
# Kisumu DENV
kisumu.D <- InsertRow(kisumu.D, NewRow = wk14.51.k.D, RowNum = 221)
kisumu.D <- InsertRow(kisumu.D, NewRow = wk14.52.k.D, RowNum = 222)
kisumu.D <- InsertRow(kisumu.D, NewRow = wk15.41.k.D, RowNum = 223)
kisumu.D <- InsertRow(kisumu.D, NewRow = wk15.45.k.D, RowNum = 224)
kisumu.D <- InsertRow(kisumu.D, NewRow = wk15.51.k.D, RowNum = 225)
kisumu.D <- InsertRow(kisumu.D, NewRow = wk15.51.k.D, RowNum = 226)
kisumu.D <- InsertRow(kisumu.D, NewRow = wk16.52.k.D, RowNum = 227)
kisumu.D <- InsertRow(kisumu.D, NewRow = wk17.31.k.D, RowNum = 228)
kisumu.D <- InsertRow(kisumu.D, NewRow = wk17.32.k.D, RowNum = 229)
kisumu.D <- InsertRow(kisumu.D, NewRow = wk17.51.k.D, RowNum = 230)
kisumu.D <- InsertRow(kisumu.D, NewRow = wk17.52.k.D, RowNum = 231)
kisumu.D <- InsertRow(kisumu.D, NewRow = wk18.09.k.D, RowNum = 232)
kisumu.D <- InsertRow(kisumu.D, NewRow = wk18.19.k.D, RowNum = 233)
# Msambweni CHIKV
msambweni.C <- InsertRow(msambweni.C, NewRow = wk15.51.m.C, RowNum = 231)
msambweni.C <- InsertRow(msambweni.C, NewRow = wk15.52.m.C, RowNum = 232)
msambweni.C <- InsertRow(msambweni.C, NewRow = wk16.49.m.C, RowNum = 233)
msambweni.C <- InsertRow(msambweni.C, NewRow = wk16.50.m.C, RowNum = 234)
msambweni.C <- InsertRow(msambweni.C, NewRow = wk16.51.m.C, RowNum = 235)
msambweni.C <- InsertRow(msambweni.C, NewRow = wk16.52.m.C, RowNum = 236)
msambweni.C <- InsertRow(msambweni.C, NewRow = wk17.51.m.C, RowNum = 237)
msambweni.C <- InsertRow(msambweni.C, NewRow = wk18.23.m.C, RowNum = 238)
# Msambweni DENV
msambweni.D <- InsertRow(msambweni.D, NewRow = wk15.51.m.D, RowNum = 231)
msambweni.D <- InsertRow(msambweni.D, NewRow = wk15.52.m.D, RowNum = 232)
msambweni.D <- InsertRow(msambweni.D, NewRow = wk16.49.m.D, RowNum = 233)
msambweni.D <- InsertRow(msambweni.D, NewRow = wk16.50.m.D, RowNum = 234)
msambweni.D <- InsertRow(msambweni.D, NewRow = wk16.51.m.D, RowNum = 235)
msambweni.D <- InsertRow(msambweni.D, NewRow = wk16.52.m.D, RowNum = 236)
msambweni.D <- InsertRow(msambweni.D, NewRow = wk17.51.m.D, RowNum = 237)
msambweni.D <- InsertRow(msambweni.D, NewRow = wk18.23.m.D, RowNum = 238)
# Ukunda CHIKV
ukunda.C <- InsertRow(ukunda.C, NewRow = wk16.49.u.C, RowNum = 232)
ukunda.C <- InsertRow(ukunda.C, NewRow = wk16.50.u.C, RowNum = 233)
ukunda.C <- InsertRow(ukunda.C, NewRow = wk16.51.u.C, RowNum = 234)
ukunda.C <- InsertRow(ukunda.C, NewRow = wk16.52.u.C, RowNum = 235)
ukunda.C <- InsertRow(ukunda.C, NewRow = wk17.51.u.C, RowNum = 236)
ukunda.C <- InsertRow(ukunda.C, NewRow = wk17.52.u.C, RowNum = 237)
# Ukunda DENV
ukunda.D <- InsertRow(ukunda.D, NewRow = wk16.49.u.D, RowNum = 232)
ukunda.D <- InsertRow(ukunda.D, NewRow = wk16.50.u.D, RowNum = 233)
ukunda.D <- InsertRow(ukunda.D, NewRow = wk16.51.u.D, RowNum = 234)
ukunda.D <- InsertRow(ukunda.D, NewRow = wk16.52.u.D, RowNum = 235)
ukunda.D <- InsertRow(ukunda.D, NewRow = wk17.51.u.D, RowNum = 236)
ukunda.D <- InsertRow(ukunda.D, NewRow = wk17.52.u.D, RowNum = 237)




chulaimbo.C$chikv_positive <- as.numeric(chulaimbo.C$chikv_positive)
chulaimbo.D$denv_positive <- as.numeric(chulaimbo.D$denv_positive)
kisumu.C$chikv_positive <- as.numeric(kisumu.C$chikv_positive)
kisumu.D$denv_positive <- as.numeric(kisumu.D$denv_positive)
msambweni.C$chikv_positive <- as.numeric(msambweni.C$chikv_positive)
msambweni.D$denv_positive <- as.numeric(msambweni.D$denv_positive)
ukunda.C$chikv_positive <- as.numeric(ukunda.C$chikv_positive)
ukunda.D$denv_positive <- as.numeric(ukunda.D$denv_positive)

chulaimbo.C <- na.omit(chulaimbo.C)
chulaimbo.D <- na.omit(chulaimbo.D)
kisumu.C <- na.omit(kisumu.C)
kisumu.D <- na.omit(kisumu.D)
msambweni.C <- na.omit(msambweni.C)
msambweni.D <- na.omit(msambweni.D)
ukunda.C <- na.omit(ukunda.C)
ukunda.D <- na.omit(ukunda.D)

# CHIKV FOI -------------------------------------------------------------------------------------
CHIKVepidata <- list(chulaimbo.C, kisumu.C, msambweni.C, ukunda.C)
## Chulaimbo
i=1
foi.c.C.df <- CHIKVepidata[[i]]
foi.c.C.df$S <- NA
foi.c.C.df$S[1] <- S.inits[i]
foi.c.C.df$week.number <- c(1:nrow(foi.c.C.df))
foi.c.C.df$Ieff <- NA

# calculate susceptible population and effective infectious population
for(j in 2:nrow(foi.c.C.df)){
  foi.c.C.df$S[j] <- S.inits[i] - sum(foi.c.C.df$chikv_positive[1:(j-1)])
  if (j > 5){
    Ieff <- c(foi.c.C.df$chikv_positive[j:(j-4)])
    foi.c.C.df$Ieff[j] <- sum(Ieff*weights,na.rm=T)
  }
}
## Kisumu
i=2
foi.k.C.df <- CHIKVepidata[[i]]
foi.k.C.df$S <- NA
foi.k.C.df$S[1] <- S.inits[i]
foi.k.C.df$week.number <- c(1:nrow(foi.k.C.df))
foi.k.C.df$Ieff <- NA

# calculate susceptible population and effective infectious population
for(j in 2:nrow(foi.k.C.df)){
  foi.k.C.df$S[j] <- S.inits[i] - sum(foi.k.C.df$chikv_positive[1:(j-1)])
  if (j > 5){
    Ieff <- c(foi.k.C.df$chikv_positive[j:(j-4)])
    foi.k.C.df$Ieff[j] <- sum(Ieff*weights,na.rm=T)
  }
}
## Msambweni
i=3
foi.m.C.df <- CHIKVepidata[[i]]
foi.m.C.df$S <- NA
foi.m.C.df$S[1] <- S.inits[i]
foi.m.C.df$week.number <- c(1:nrow(foi.m.C.df))
foi.m.C.df$Ieff <- NA

# calculate susceptible population and effective infectious population
for(j in 2:nrow(foi.m.C.df)){
  foi.m.C.df$S[j] <- S.inits[i] - sum(foi.m.C.df$chikv_positive[1:(j-1)])
  if (j > 5){
    Ieff <- c(foi.m.C.df$chikv_positive[j:(j-4)])
    foi.m.C.df$Ieff[j] <- sum(Ieff*weights,na.rm=T)
  }
}
## Ukunda
i=4
foi.u.C.df <- CHIKVepidata[[i]]
foi.u.C.df$S <- NA
foi.u.C.df$S[1] <- S.inits[i]
foi.u.C.df$week.number <- c(1:nrow(foi.u.C.df))
foi.u.C.df$Ieff <- NA

# calculate susceptible population and effective infectious population
for(j in 2:nrow(foi.u.C.df)){
  foi.u.C.df$S[j] <- S.inits[i] - sum(foi.u.C.df$chikv_positive[1:(j-1)])
  if (j > 5){
    Ieff <- c(foi.u.C.df$chikv_positive[j:(j-4)])
    foi.u.C.df$Ieff[j] <- sum(Ieff*weights,na.rm=T)
  }
}

## Chulaimbo
# subset data for model (week)
foi.c.C.df <- foi.c.C.df[6:nrow(foi.c.C.df),]
S <- foi.c.C.df$S
I <- foi.c.C.df$chikv_positive + 0.0001
Ieff <- foi.c.C.df$Ieff + 0.0001
N <- S.inits[i]
week.number <- foi.c.C.df$week.number

# run model
model<-glm(log(I)-log(S)+log(N)~-1+as.factor(week.number)+log(Ieff))
foi.c.C.df$betas <- NA
foi.c.C.df$betas<-coef(model)[-length(coef(model))]
foi.c.C.df$betas[foi.c.C.df$betas < 0] <- 0

# plot
plot.ts(foi.c.C.df$betas)
library(ggplot2)
ggplot(data = foi.c.C.df, aes(x = week.number, y = betas)) + geom_line() + 
  labs(
    x = "Week Number",
    y = "FOI"
  ) +
  theme(plot.title = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.3)),
        axis.text.x = element_text(size = rel(1.2)),
        axis.text.y = element_text(size = rel(1.2)),
        panel.background = element_rect("gray96"),
        panel.grid.major.x = element_line(colour = "gray88"),
        panel.grid.major.y = element_line(colour = "gray88")) +
  ggtitle("FOI Chulaimbo")
ggsave("CHIKV_chulaimbo_FOI.jpeg")

## Kisumu
# subset data for model (week)
foi.k.C.df <- foi.k.C.df[6:nrow(foi.k.C.df),]
S <- foi.k.C.df$S
I <- foi.k.C.df$chikv_positive + 0.0001
Ieff <- foi.k.C.df$Ieff + 0.0001
N <- S.inits[i]
week.number <- foi.k.C.df$week.number

# run model
model<-glm(log(I)-log(S)+log(N)~-1+as.factor(week.number)+log(Ieff))
foi.k.C.df$betas <- NA
foi.k.C.df$betas<-coef(model)[-length(coef(model))]
foi.k.C.df$betas[foi.k.C.df$betas < 0] <- 0

# plot
plot.ts(foi.k.C.df$betas)
library(ggplot2)
ggplot(data = foi.k.C.df, aes(x = week.number, y = betas)) + geom_line() + 
  labs(
    x = "Week Number",
    y = "FOI"
  ) +
  theme(plot.title = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.3)),
        axis.text.x = element_text(size = rel(1.2)),
        axis.text.y = element_text(size = rel(1.2)),
        panel.background = element_rect("gray96"),
        panel.grid.major.x = element_line(colour = "gray88"),
        panel.grid.major.y = element_line(colour = "gray88")) +
  ggtitle("FOI Kisumu")
ggsave("CHIKV_kisumu_FOI.jpeg")

## Msambweni
# subset data for model (week)
foi.m.C.df <- foi.m.C.df[6:nrow(foi.m.C.df),]
S <- foi.m.C.df$S
I <- foi.m.C.df$chikv_positive + 0.0001
Ieff <- foi.m.C.df$Ieff + 0.0001
N <- S.inits[i]
week.number <- foi.m.C.df$week.number

# run model
model<-glm(log(I)-log(S)+log(N)~-1+as.factor(week.number)+log(Ieff))
foi.m.C.df$betas <- NA
foi.m.C.df$betas<-coef(model)[-length(coef(model))]
foi.m.C.df$betas[foi.m.C.df$betas < 0] <- 0

# plot
plot.ts(foi.m.C.df$betas)
library(ggplot2)
ggplot(data = foi.m.C.df, aes(x = week.number, y = betas)) + geom_line() + 
  labs(
    x = "Week Number",
    y = "FOI"
  ) +
  theme(plot.title = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.3)),
        axis.text.x = element_text(size = rel(1.2)),
        axis.text.y = element_text(size = rel(1.2)),
        panel.background = element_rect("gray96"),
        panel.grid.major.x = element_line(colour = "gray88"),
        panel.grid.major.y = element_line(colour = "gray88")) +
  ggtitle("FOI Msambweni")
ggsave("CHIKV_full_msambweni_FOI.jpeg")

## Ukunda
# subset data for model (week)
foi.u.C.df <- foi.u.C.df[6:nrow(foi.u.C.df),]
S <- foi.u.C.df$S
I <- foi.u.C.df$chikv_positive + 0.0001
Ieff <- foi.u.C.df$Ieff + 0.0001
N <- S.inits[i]
week.number <- foi.u.C.df$week.number

# run model
model<-glm(log(I)-log(S)+log(N)~-1+as.factor(week.number)+log(Ieff))
foi.u.C.df$betas <- NA
foi.u.C.df$betas<-coef(model)[-length(coef(model))]
foi.u.C.df$betas[foi.u.C.df$betas < 0] <- 0

# plot
plot.ts(foi.u.C.df$betas)
ggplot(data = foi.u.C.df, aes(x = week.number, y = betas)) + geom_line() + 
  labs(
    x = "Week Number",
    y = "FOI"
  ) +
  theme(plot.title = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.3)),
        axis.text.x = element_text(size = rel(1.2)),
        axis.text.y = element_text(size = rel(1.2)),
        panel.background = element_rect("gray96"),
        panel.grid.major.x = element_line(colour = "gray88"),
        panel.grid.major.y = element_line(colour = "gray88")) +
  ggtitle("FOI Ukunda")
ggsave("CHIKV_full_ukunda_FOI.jpeg")




# DENV FOI ---------------------------------------------------------------------------------------
DENVepidata <- list(chulaimbo.D, kisumu.D, msambweni.D, ukunda.D)
## Chulaimbo
i=1
foi.c.D.df <- DENVepidata[[i]]
foi.c.D.df$S <- NA
foi.c.D.df$S[1] <- S.inits[i]
foi.c.D.df$week.number <- c(1:nrow(foi.c.D.df))
foi.c.D.df$Ieff <- NA

# calculate susceptible population and effective infectious population
for(j in 2:nrow(foi.c.D.df)){
  foi.c.D.df$S[j] <- S.inits[i] - sum(foi.c.D.df$denv_positive[1:(j-1)])
  if (j > 5){
    Ieff <- c(foi.c.D.df$denv_positive[j:(j-4)])
    foi.c.D.df$Ieff[j] <- sum(Ieff*weights,na.rm=T)
  }
}
## Kisumu
i=2
foi.k.D.df <- DENVepidata[[i]]
foi.k.D.df$S <- NA
foi.k.D.df$S[1] <- S.inits[i]
foi.k.D.df$week.number <- c(1:nrow(foi.k.D.df))
foi.k.D.df$Ieff <- NA

# calculate susceptible population and effective infectious population
for(j in 2:nrow(foi.k.D.df)){
  foi.k.D.df$S[j] <- S.inits[i] - sum(foi.k.D.df$denv_positive[1:(j-1)])
  if (j > 5){
    Ieff <- c(foi.k.D.df$denv_positive[j:(j-4)])
    foi.k.D.df$Ieff[j] <- sum(Ieff*weights,na.rm=T)
  }
}
## Msambweni
i=3
foi.m.D.df <- DENVepidata[[i]]
foi.m.D.df$S <- NA
foi.m.D.df$S[1] <- S.inits[i]
foi.m.D.df$week.number <- c(1:nrow(foi.m.D.df))
foi.m.D.df$Ieff <- NA

# calculate susceptible population and effective infectious population
for(j in 2:nrow(foi.m.D.df)){
  foi.m.D.df$S[j] <- S.inits[i] - sum(foi.m.D.df$denv_positive[1:(j-1)])
  if (j > 5){
    Ieff <- c(foi.m.D.df$denv_positive[j:(j-4)])
    foi.m.D.df$Ieff[j] <- sum(Ieff*weights,na.rm=T)
  }
}
## Ukunda
i=4
foi.u.D.df <- DENVepidata[[i]]
foi.u.D.df$S <- NA
foi.u.D.df$S[1] <- S.inits[i]
foi.u.D.df$week.number <- c(1:nrow(foi.u.D.df))
foi.u.D.df$Ieff <- NA

# calculate susceptible population and effective infectious population
for(j in 2:nrow(foi.u.D.df)){
  foi.u.D.df$S[j] <- S.inits[i] - sum(foi.u.D.df$denv_positive[1:(j-1)])
  if (j > 5){
    Ieff <- c(foi.u.D.df$denv_positive[j:(j-4)])
    foi.u.D.df$Ieff[j] <- sum(Ieff*weights,na.rm=T)
  }
}

## Chulaimbo
# subset data for model (week)
foi.c.D.df <- foi.c.C.df[6:nrow(foi.c.D.df),]
S <- foi.c.D.df$S
I <- foi.c.D.df$denv_positive + 0.0001
Ieff <- foi.c.D.df$Ieff + 0.0001
N <- S.inits[i]
week.number <- foi.c.D.df$week.number

# run model
model<-glm(log(I)-log(S)+log(N)~-1+as.factor(week.number)+log(Ieff))
foi.c.D.df$betas <- NA
foi.c.D.df$betas<-coef(model)[-length(coef(model))]
foi.c.D.df$betas[foi.c.D.df$betas < 0] <- 0

# plot
plot.ts(foi.c.D.df$betas)
library(ggplot2)
ggplot(data = foi.c.D.df, aes(x = week.number, y = betas)) + geom_line() + 
  labs(
    x = "Week Number",
    y = "FOI"
  ) +
  theme(plot.title = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.3)),
        axis.text.x = element_text(size = rel(1.2)),
        axis.text.y = element_text(size = rel(1.2)),
        panel.background = element_rect("gray96"),
        panel.grid.major.x = element_line(colour = "gray88"),
        panel.grid.major.y = element_line(colour = "gray88")) +
  ggtitle("FOI Chulaimbo")
ggsave("DENV_chulaimbo_FOI.jpeg")

## Kisumu
# subset data for model (week)
foi.k.C.df <- foi.k.C.df[6:nrow(foi.k.C.df),]
S <- foi.k.C.df$S
I <- foi.k.C.df$chikv_positive + 0.0001
Ieff <- foi.k.C.df$Ieff + 0.0001
N <- S.inits[i]
week.number <- foi.k.C.df$week.number

# run model
model<-glm(log(I)-log(S)+log(N)~-1+as.factor(week.number)+log(Ieff))
foi.k.C.df$betas <- NA
foi.k.C.df$betas<-coef(model)[-length(coef(model))]
foi.k.C.df$betas[foi.k.C.df$betas < 0] <- 0

# plot
plot.ts(foi.k.C.df$betas)
library(ggplot2)
ggplot(data = foi.k.C.df, aes(x = week.number, y = betas)) + geom_line() + 
  labs(
    x = "Week Number",
    y = "FOI"
  ) +
  theme(plot.title = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.3)),
        axis.text.x = element_text(size = rel(1.2)),
        axis.text.y = element_text(size = rel(1.2)),
        panel.background = element_rect("gray96"),
        panel.grid.major.x = element_line(colour = "gray88"),
        panel.grid.major.y = element_line(colour = "gray88")) +
  ggtitle("FOI Kisumu")
ggsave("kisumu_FOI.jpeg")

## Msambweni
# subset data for model (week)
foi.m.C.df <- foi.m.C.df[6:nrow(foi.m.C.df),]
S <- foi.m.C.df$S
I <- foi.m.C.df$chikv_positive + 0.0001
Ieff <- foi.m.C.df$Ieff + 0.0001
N <- S.inits[i]
week.number <- foi.m.C.df$week.number

# run model
model<-glm(log(I)-log(S)+log(N)~-1+as.factor(week.number)+log(Ieff))
foi.m.C.df$betas <- NA
foi.m.C.df$betas<-coef(model)[-length(coef(model))]
foi.m.C.df$betas[foi.m.C.df$betas < 0] <- 0

# plot
plot.ts(foi.m.C.df$betas)
library(ggplot2)
ggplot(data = foi.m.C.df, aes(x = week.number, y = betas)) + geom_line() + 
  labs(
    x = "Week Number",
    y = "FOI"
  ) +
  theme(plot.title = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.3)),
        axis.text.x = element_text(size = rel(1.2)),
        axis.text.y = element_text(size = rel(1.2)),
        panel.background = element_rect("gray96"),
        panel.grid.major.x = element_line(colour = "gray88"),
        panel.grid.major.y = element_line(colour = "gray88")) +
  ggtitle("FOI Msambweni")
ggsave("full_msambweni_FOI.jpeg")

## Ukunda
# subset data for model (week)
foi.u.C.df <- foi.u.C.df[6:nrow(foi.u.C.df),]
S <- foi.u.C.df$S
I <- foi.u.C.df$chikv_positive + 0.0001
Ieff <- foi.u.C.df$Ieff + 0.0001
N <- S.inits[i]
week.number <- foi.u.C.df$week.number

# run model
model<-glm(log(I)-log(S)+log(N)~-1+as.factor(week.number)+log(Ieff))
foi.u.C.df$betas <- NA
foi.u.C.df$betas<-coef(model)[-length(coef(model))]
foi.u.C.df$betas[foi.u.C.df$betas < 0] <- 0

# plot
plot.ts(foi.u.C.df$betas)
ggplot(data = foi.u.C.df, aes(x = week.number, y = betas)) + geom_line() + 
  labs(
    x = "Week Number",
    y = "FOI"
  ) +
  theme(plot.title = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.3)),
        axis.text.x = element_text(size = rel(1.2)),
        axis.text.y = element_text(size = rel(1.2)),
        panel.background = element_rect("gray96"),
        panel.grid.major.x = element_line(colour = "gray88"),
        panel.grid.major.y = element_line(colour = "gray88")) +
  ggtitle("FOI Ukunda")
ggsave("full_ukunda_FOI.jpeg")



# extract avg case # for each city
mean(msambweni.outbreak$chikv_positive) # 2.556818
mean(ukunda.outbreak$chikv_positive) # 3.441667

# extract total case # for each city
sum(msambweni.outbreak$chikv_positive) # 112.5
sum(ukunda.outbreak$chikv_positive) # 154.875

# extract proportion positive for each city
msambweni.outbreak$chikv_negative <- as.numeric(msambweni.outbreak$chikv_negative)
ukunda.outbreak$chikv_negative <- as.numeric(ukunda.outbreak$chikv_negative)

(sum(msambweni.outbreak$chikv_positive))/(sum(msambweni.outbreak$chikv_positive)+sum(msambweni.outbreak$chikv_negative))
# 0.191164
(sum(ukunda.outbreak$chikv_positive))/(sum(ukunda.outbreak$chikv_positive)+sum(ukunda.outbreak$chikv_negative))
# 0.2263427

# extract avg FOI for each city
mean(foi.m.df$betas) # 1.894449
mean(foi.u.df$betas) # 1.073027

# extract max FOI for each city and the week it occurred
foi.m.df[which.max(foi.m.df$betas),]
#     id_site   epiwk chikv_positive chikv_negative        proportion     S week.number  Ieff    betas
#642 Msambweni 2017-47             10             13 0.434782608695652 14422          14 6.219 3.956077
foi.u.df[which.max(foi.u.df$betas),]
#     id_site   epiwk chikv_positive chikv_negative        proportion     S week.number  Ieff    betas
# 874  Ukunda 2017-48             13              4 0.764705882352941 75337          15 3.152 2.565222

# turn average and max FOI into 'number infected per 10,000
# average
exp(1.894449) # 6, Msambweni
exp(1.073027) # 2, Ukunda
# max
exp(3.956077) # 52, Msambweni
exp(2.565222) # 13, Ukunda


# date ranges of outbreak 
f <- subset(foi.m.df, betas > 0) # 2017-43 to 2018-15
g <- subset(foi.u.df, betas > 0) # 2017-43 to 2018-17

# test statistical significance of differences b/w two cities
t.test <- data.frame("foi.m" = foi.m.df$betas, "foi.u" = foi.u.df$betas, "epiwks" = foi.u.df$epiwk) 

t.test(t.test$foi.m, 
       t.test$foi.u, 
       paired=TRUE, 
       conf.level=0.95)
# data:  t.test$foi.m and t.test$foi.u
# t = 5.2661, df = 39, p-value = 5.392e-06
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval: 0.4767445 1.0713773
# sample estimates: mean of the differences = 0.7740609 


# plot overlay of both cities FOI
ggplot() + geom_line(data = foi.u.df, aes(x = epiwk, y = betas, colour = "blue1", size = 2, group = 1)) + 
  geom_line(data = foi.m.df, aes(x = epiwk, y = betas, colour = "maroon3", size = 2, group = 2)) +
  labs(
    x = "Date",
    y = "FOI",
    colour = "City"
  ) +
  theme(axis.title = element_text(size = rel(3)),
        axis.text.x = element_text(size = rel(2.5), angle = 45, hjust = 1),
        axis.text.y = element_text(size = rel(2.5)),
        legend.title = element_text(face = "bold", size = 15),
        legend.title.align = 0.5,
        legend.text = element_text(size = 14),
        legend.position = "none",
        legend.justification = c("right", "bottom"),
        legend.box.just = "right",
        panel.background = element_rect("gray96"),
        panel.grid.major.x = element_line(colour = "gray88"),
        panel.grid.major.y = element_line(colour = "gray88")) +
  scale_colour_manual(
    values = alpha(c("slateblue", "maroon3")),
    labels = c("Ukunda", "Msambweni")) +
  scale_x_discrete(
    breaks = c("2017-40", "2017-45", "2017-50", "2018-03", "2018-08","2018-13", "2018-18", "2018-23"),
    labels = c("2017-40", "2017-45", "2017-50", "2018-03", "2018-08","2018-13", "2018-18", "2018-23"))
ggsave("cities_FOI.jpeg")




# Network analyses ------------------------------------------------------------------
### https://briatte.github.io/ggnet/

# load libraries
library(plyr)
#install.packages("GGally")
library(GGally)
# install.packages("network")
library(network)
# install.packages("sna")
library(sna)
library(ggplot2)

outbreak.aic <- subset(expanded_aic, id_site == "Msambweni" | id_site == "Ukunda" &
                         result_igg_chikv_stfd == 1|result_pcr_chikv_kenya==1|result_pcr_chikv_stfd==1|chikv_result_ufi==1)
outbreak.time.aic <- subset(outbreak.aic, date_start_infectious > "2017-10-15" & date_start_infectious < "2018-06-15")

# travelNet <- ddply(outbreak.aic, .(where_travel_aic),
#                   summarise,
#                   Msambweni = sum(id_site == "Msambweni"),
#                   Ukunda = sum(id_site == "Ukunda"))

# write.csv(travelNet, "travelNet.csv")
travelNet <- read.csv("travelNet.csv")

travelNet <- travelNet[2:(nrow(travelNet)-1),]
rownames(travelNet) <- travelNet[,1]

travelNet$where_travel_aic <- NULL

bip = network(travelNet,
              matrix.type = "bipartite",
              ignore.eval = FALSE,
              names.eval = "weights")

col = c("actor" = "cadetblue2", "event" = "gold")
set.edge.attribute(bip, "lty", ifelse(bip %e% "weights" > 5, 1, 1))
ggnet2(bip, mode = "kamadakawai", label = TRUE, label.color = "black",
       label.size = 8, color = "mode", node.size = 20, edge.label = "weights",
       edge.color = "gray60", edge.size = "weights", palette = col, edge.lty = "lty", 
       edge.label.size = 7, max_size = 9, legend.position = "none") 

#villageNet <- ddply(outbreak.aic, .(village_aic),
#                   summarise,
#                  Msambweni = sum(id_site == "Msambweni"),
#                   Ukunda = sum(id_site == "Ukunda"))
#write.csv(villageNet, "villageNet.csv")
villageNet <- read.csv("villageNet.csv")

villageNet <- villageNet[2:(nrow(villageNet)-1),]
rownames(villageNet) <- villageNet[,1]

villageNet$village_aic <- NULL

bip = network(villageNet,
              matrix.type = "bipartite",
              ignore.eval = FALSE,
              names.eval = "weights")

col = c("actor" = "cadetblue2", "event" = "gold")
set.edge.attribute(bip, "lty", ifelse(bip %e% "weights" > 10, 1, 5))
ggnet2(bip, mode = "kamadakawai", label = TRUE, label.color = "black",
       label.size = 8, color = "mode", node.size = 20, edge.label = "weights",
       edge.color = "gray60", palette = col, edge.lty = "lty", 
       edge.label.size = 7, max_size = 9, legend.position = "none") 



# Outbreak and Travel Correlation ------------------------------------------------------------------

# Result: 

# subset 
sumTravelers <- ddply(outbreak.time.aic, .(where_travel_aic, epiwk, Date),
                      summarise,
                      Msambweni = sum(id_site == "Msambweni"),
                      Ukunda = sum(id_site == "Ukunda"))
ggplot() + geom_col(data = sumTravelers, aes(x = epiwk, y = Msambweni, fill = "blue")) + 
  geom_col(data = sumTravelers, aes(x = epiwk, y = Ukunda, fill = "red")) +
  labs(
    x = "Week of Outbreak",
    y = "Number of Travelers",
    fill = "City"
  ) + 
  theme(axis.title = element_text(size = rel(1.5)),
        axis.text.x = element_text(size = rel(1), angle = 45, hjust = 1),
        axis.text.y = element_text(size = rel(1)),
        legend.title = element_text(face = "bold"),
        legend.title.align = 0.5,
        legend.text = element_text(size = 14),
        legend.position = "right",
        legend.justification = c("right", "bottom"),
        legend.box.just = "right",
        panel.background = element_rect("gray96"),
        panel.grid.major.x = element_line(colour = "gray88"),
        panel.grid.major.y = element_line(colour = "gray88")) +
  scale_fill_manual(
    values = alpha(c("red", "blue")),
    labels = c("Msambweni", "Ukunda")) +
  ggtitle("Number of CHIKV+ Travelers During OUtbreak")
