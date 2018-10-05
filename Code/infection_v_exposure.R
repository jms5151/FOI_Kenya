### Mailo Numazu, Stanford University, BSURPS 2018

## Data: Expanded Visits AIC (Acutely Ill COhort) Surveys 

## Purpose: determine periods of exposure and infection for DENV and CHIKV, and CHIKV during outbreak

# Set working directory
setwd("C:/Users/mailo/OneDrive/Desktop/Stanford 18-19/BSURPS/Working Directory")


# determine days in which AIC kids are exposed and infectious --------------------------
rm(list=ls()) #remove previous variable assignments

# load data (header = T just specifies that the data has a header row)
# expanded_aic <- read.csv("C:/Users/Jamie/Box Sync/FOI Kenya Project/aic_results_for_mailo_all_visits.csv", header = T, stringsAsFactors = F)
EV_aic <- read.csv("C:/Users/mailo/OneDrive/Desktop/Stanford 18-19/BSURPS/Working Directory/expanded_visits_aic.csv", header = T, stringsAsFactors = F)

# convert interview date to date class 
EV_aic$interview_date_aic <- as.Date(EV_aic$interview_date_aic, "%m/%d/%Y") #"%m/%d/%Y"

# all aic kids are febrile/have fever so if days with symptoms is 'NA', replace with zero since they had fever on day of visit 
EV_aic$date_symptom_onset[is.na(EV_aic$date_symptom_onset)] <- 0

# if kids had fever for more than 7 days treat as day 0
EV_aic$date_symptom_onset[EV_aic$date_symptom_onset>7] <- 0

# subset out any 'NA' dates
EV_aic <- subset(EV_aic, !is.na(interview_date_aic))

# exposure and infection start and end dates
EV_aic$date_start_infectious <- (EV_aic$interview_date_aic - EV_aic$date_symptom_onset)
EV_aic$date_end_infectious <- EV_aic$date_start_infectious + 4 # infectious for 5 days
EV_aic$date_start_exposed <- EV_aic$date_start_infectious - 7
EV_aic$date_end_exposed <- EV_aic$date_start_infectious - 1

# infectious period
mindate_infectious <- min(EV_aic$date_start_infectious)
maxdate_infectious <- max(EV_aic$date_end_infectious)

# exposed period
mindate_exposed <- min(EV_aic$date_start_exposed)
maxdate_exposed <- max(EV_aic$date_end_exposed)

# create date vectors
inf.dates <- seq.Date(from = mindate_infectious, to = maxdate_infectious, "days")
exp.dates <- seq.Date(from = mindate_exposed, to = maxdate_exposed, "days")

# turn sequential dates into data frames
inf.dates <- as.data.frame(inf.dates)
exp.dates <- as.data.frame(exp.dates)

# add seroconversion for igg at last visit (none yet)
EV_aic$hi_denv_stfd_igg <- 0
EV_aic$hi_chikv_stfd_igg <- 0

# create dataframe for infectious period for all kids
peopIDs <- unique(EV_aic$person_id)
visitNames <- c("a", "b", "c", "d", "e", "f", "g", "h", "i")

for(i in 1:length(peopIDs)){
  visits <- subset(EV_aic, person_id == peopIDs[i])
  visits[is.na(visits)] <- 0
  exposed <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(exposed) <- c("exp.dates", "denv", "chikv")
  infectious <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(infectious) <- c("inf.dates", "denv", "chikv")
  for (k in 1:nrow(visits)){
    vis <- substr(visits$redcap_event_name[k], 7,7)
    d.seroconv <- paste0(vis, visitNames[which(visitNames==vis)+1], "_denv_stfd_igg")
    visits$denv[k] <- ifelse(visits[k,d.seroconv]==1|visits$result_pcr_denv_kenya[k]==1|visits$result_pcr_denv_stfd[k]==1|visits$denv_result_ufi[k]==1, 1, 0)
    c.seroconv <- paste0(vis, visitNames[which(visitNames==vis)+1], "_chikv_stfd_igg")
    visits$chikv[k] <- ifelse(visits[k,c.seroconv]==1|visits$result_pcr_chikv_kenya[k]==1|visits$result_pcr_chikv_stfd[k]==1|visits$chikv_result_ufi[k]==1, 1, 0)
    # exposed
    tempExp <- as.data.frame(seq.Date(from=visits$date_start_exposed[k], to=visits$date_end_exposed[k], by="days"))
    colnames(tempExp) <- "exp.dates"
    tempExp$denv <- visits$denv[k]
    tempExp$chikv <- visits$chikv[k]
    exposed <- rbind(exposed, tempExp)
    # infectious
    tempInf <- as.data.frame(seq.Date(from=visits$date_start_infectious[k], to=visits$date_end_infectious[k], by="days"))
    colnames(tempInf) <- "inf.dates"
    tempInf$denv <- visits$denv[k]
    tempInf$chikv <- visits$chikv[k]
    infectious <- rbind(infectious, tempInf)
  }
  colnames(exposed)[2] <- paste0("DENV_", visits$id_site[k], "_", as.character(peopIDs[i]))
  colnames(exposed)[3] <- paste0("CHIKV_", visits$id_site[k], "_", as.character(peopIDs[i]))
  exp.dates <- merge(exp.dates, exposed, by = "exp.dates", all.x=T)
  colnames(infectious)[2] <- paste0("DENV_", visits$id_site[k], "_", as.character(peopIDs[i]))
  colnames(infectious)[3] <- paste0("CHIKV_", visits$id_site[k], "_", as.character(peopIDs[i]))
  inf.dates <- merge(inf.dates, infectious, by = "inf.dates", all.x=T)
}

write.csv(exp.dates, "C:/Users/mailo/OneDrive/Desktop/Stanford 18-19/BSURPS/Working Directory/exposed_period.csv", row.names = F)
write.csv(inf.dates, "C:/Users/mailo/OneDrive/Desktop/Stanford 18-19/BSURPS/Working Directory/infectious_period.csv", row.names = F)

# Concatenate outbreak data --------------------------------------------------------------------
# http://www.who.int/csr/don/27-february-2018-chikungunya-kenya/en/
# From mid-December 2017 through 3 February 2018, the Ministry of Health (MoH) 
# of Kenya reported 453 cases, including 32 laboratory-confirmed cases and 421 suspected 
# cases, of chikungunya from Mombasa County.

infectious <- read.csv("C:/Users/mailo/OneDrive/Desktop/Stanford 18-19/BSURPS/Working Directory/infectious_period.csv", head = T)

# format date
infectious$inf.dates <- as.Date(infectious$inf.dates, "%Y-%m-%d")

# format epiweek
infectious$epiwk <- strftime(infectious$inf.dates, format = "%Y-%V")

# subset chikungunya observations
chik.infectious <- infectious[,grepl("dates|epiwk|CHIKV", names(infectious))]

# subset coastal sites (Ukunda and Msambweni)
ukunda.infectious <- chik.infectious[, grepl("dates|epiwk|Ukunda", names(chik.infectious))]
msambweni.infectious <- chik.infectious[, grepl("dates|epiwk|Msambweni", names(chik.infectious))]

# sum by date
ukunda.infectious$chikv.neg <- rowSums(ukunda.infectious[,c(3:(ncol(ukunda.infectious)-1))]==0, na.rm=T)
ukunda.infectious$chikv.pos <- rowSums(ukunda.infectious[,c(3:(ncol(ukunda.infectious)-1))]==1, na.rm=T)
plot(ukunda.infectious$inf.dates, ukunda.infectious$chikv.pos, type='l')

msambweni.infectious$chikv.neg <- rowSums(msambweni.infectious[,c(3:(ncol(msambweni.infectious)-1))]==0, na.rm=T)
msambweni.infectious$chikv.pos <- rowSums(msambweni.infectious[,c(3:(ncol(msambweni.infectious)-1))]==1, na.rm=T)
plot(msambweni.infectious$inf.dates, msambweni.infectious$chikv.pos, type='l')

# subset by date
ukunda.outbreak <- subset(ukunda.infectious, inf.dates > "2017-10-15" & inf.dates < "2018-05-15")
msambweni.outbreak <- subset(msambweni.infectious, inf.dates > "2017-10-15" & inf.dates < "2018-05-15")

# aggregate by epiweek
ukunda.outbreak.wkly <- ddply(ukunda.outbreak, .(epiwk), summarise, chikv_positive = max(chikv.pos, na.rm=T))
barplot(ukunda.outbreak.wkly$chikv_positive)

msambweni.outbreak.wkly <- ddply(msambweni.outbreak, .(epiwk), summarise, chikv_positive = max(chikv.pos, na.rm=T))
barplot(msambweni.outbreak.wkly$chikv_positive)

# graph both ukunda and msambweni outbreaks
ggplot() + geom_col(data = ukunda.outbreak.wkly, aes(x = epiwk, y = chikv_positive, fill = "red")) + 
  geom_col(data = msambweni.outbreak.wkly, aes(x = epiwk, y = chikv_positive, fill = "blue")) +
  labs(
    x = "Weeks",
    y = "Number DENV+",
    fill = "City"
  ) +
  theme(plot.title = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.3)),
        axis.text.x = element_text(size = rel(1.2)),
        axis.text.y = element_text(size = rel(1.2)),
        panel.background = element_rect("gray96"),
        panel.grid.major.x = element_line(colour = "gray88"),
        panel.grid.major.y = element_line(colour = "gray88")) + 
  scale_fill_manual(
    values = alpha(c("blue", "red")),
    labels = c("Ukunda", "Msambweni")
  ) +
  ggtitle("Period of Exposure")
ggsave("kisumu_exp.jpeg")

msambweni.ids <- grep("CHIKV",x=names(msambweni.outbreak),value=T) 
msambweni.ids <- gsub("CHIKV_Msambweni_", "", msambweni.ids)
# 
ukunda.ids <- grep("CHIKV",x=names(ukunda.outbreak),value=T) 
ukunda.ids <- gsub("CHIKV_Ukunda_", "", ukunda.outbreak)
kid.ids <- c(msambweni.ids, ukunda.ids)
