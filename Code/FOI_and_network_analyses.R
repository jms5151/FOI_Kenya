### Mailo Numazu, Stanford University, BSURPS 2018

## Data: Expanded Visits AIC (Acutely Ill COhort) Surveys 

## Purpose: Find FOI for CHIKV outbreak in Ukunda and Msambweni & create network analysis of travel patterns for CHIKV+ individuals


# Concatenate outbreak data --------------------------------------------------------------------
# http://www.who.int/csr/don/27-february-2018-chikungunya-kenya/en/
# From mid-December 2017 through 3 February 2018, the Ministry of Health (MoH) 
# of Kenya reported 453 cases, including 32 laboratory-confirmed cases and 421 suspected 
# cases, of chikungunya from Mombasa County.
rm(list=ls()) #remove previous variable assignments

# Set working directory
setwd("C:/Users/mailo/OneDrive/Desktop/Stanford 18-19/BSURPS/Working Directory")

# expanded_aic <- read.csv("C:/Users/Jamie/Box Sync/FOI Kenya Project/aic_results_for_mailo_all_visits.csv", header = T, stringsAsFactors = F)
EV_aic <- read.csv("C:/Users/mailo/OneDrive/Desktop/Stanford 18-19/BSURPS/Working Directory/expanded_visits_aic.csv", header = T, stringsAsFactors = F)

# convert interview date to date class 
# expanded_aic$interview_date_aic <- as.Date(expanded_aic$interview_date_aic, "%Y-%m-%d") #"%m/%d/%Y"
EV_aic$interview_date_aic <- as.Date(EV_aic$interview_date_aic, "%m/%d/%Y") #"%m/%d/%Y"

# all aic kids are febrile so if days with symptoms is 'NA' replace with zero as they had fever on day of visit 
EV_aic$date_symptom_onset[is.na(EV_aic$date_symptom_onset)] <- 0

# if kids had fever for more than 7 days treat as day 7 -- changed from 0
EV_aic$date_symptom_onset[EV_aic$date_symptom_onset>7] <- 7

# subset out any 'NA' dates
EV_aic <- subset(EV_aic, !is.na(interview_date_aic)|interview_date_aic == "1900-01-01")

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

# make NAs for denv and chikv test results zeros -- change code to account for 
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

#x <- merge(epiweeks, expanded_aic[,c("epiwk", "Date")], by = "epiwk", all.x = T)

# Chikungunya subset for outbreak cities
msambweni.outbreak <- subset(epiweeks.C, id_site=="Msambweni")
barplot(msambweni.outbreak$chikv_positive)
ukunda.outbreak <- subset(epiweeks.C, id_site=="Ukunda")
barplot(ukunda.outbreak$chikv_positive)

# extract 2017 and 2018 dates for CHIKV outbreak
msambweni.outbreak <- subset(msambweni.outbreak, epiwk > "2017-33")
barplot(msambweni.outbreak$chikv_positive)
ukunda.outbreak <- subset(ukunda.outbreak, epiwk > "2017-33")
barplot(ukunda.outbreak$chikv_positive)

# Calculate FOI---------------------------------------------------------------------
# weighted values and model based on http://currents.plos.org/outbreaks/article/estimating-drivers-of-autochthonous-transmission-of-chikungunya-virus-in-its-invasion-of-the-americas/
weights <- c(0.011, 0.187, 0.432, 0.287, 0.083)
S.inits <- c(14444, 75357) # msambweni, ukunda

# make sure that dates are continuous(for missing weeks, find case numbers by averaging b/w previous and after week)
# install.packages("DataCombine")
library(DataCombine)
# Create new row
wk51m <- c("2017-51", "Msambweni", 2.5, 4.5, 0.415, "2017-12")
wk23m <- c("2018-23", "Msambweni", 0, 6.5, 0, "2018-05")
wk51u <- c("2017-51", "Ukunda", 2.75, 2.75, 0.401, "2017-12")
wk52u <- c("2017-52", "Ukunda", 6.25, 4.25, 0.544, "2017-12")

msambweni.outbreak <- unique(msambweni.outbreak)
ukunda.outbreak <- unique(ukunda.outbreak)

# Insert into appropriate row
msambweni.outbreak <- InsertRow(msambweni.outbreak, NewRow = wk51m, RowNum = 22)
msambweni.outbreak <- InsertRow(msambweni.outbreak, NewRow = wk23m, RowNum = 41)
ukunda.outbreak <- InsertRow(ukunda.outbreak, NewRow = wk51u, RowNum = 18)
ukunda.outbreak <- InsertRow(ukunda.outbreak, NewRow = wk52u, RowNum = 19)

msambweni.outbreak$chikv_positive <- as.numeric(msambweni.outbreak$chikv_positive)
ukunda.outbreak$chikv_positive <- as.numeric(ukunda.outbreak$chikv_positive)


# edit rows where chikv_negative almost equals chikv_positive
# replace values with averages of surrounding rows
ukunda.outbreak[23, 3] = 5.875
ukunda.outbreak[23, 4] = 5.375
ukunda.outbreak[23, 5] = 0.465

epidata <- list(msambweni.outbreak, ukunda.outbreak)

i=1
foi.m.df <- epidata[[i]]
foi.m.df$S <- NA
foi.m.df$S[1] <- S.inits[i]
foi.m.df$week.number <- c(1:nrow(foi.m.df))
foi.m.df$Ieff <- NA

# calculate susceptible population and effective infectious population
for(j in 2:nrow(foi.m.df)){
  foi.m.df$S[j] <- S.inits[i] - sum(foi.m.df$chikv_positive[1:(j-1)])
  if (j > 5){
    Ieff <- c(foi.m.df$chikv_positive[j:(j-4)])
    foi.m.df$Ieff[j] <- sum(Ieff*weights,na.rm=T)
  }
}

i=2
foi.u.df <- epidata[[i]]
foi.u.df$S <- NA
foi.u.df$S[1] <- S.inits[i]
foi.u.df$week.number <- c(1:nrow(foi.u.df))
foi.u.df$Ieff <- NA

# calculate susceptible population and effective infectious population
for(j in 2:nrow(foi.u.df)){
  foi.u.df$S[j] <- S.inits[i] - sum(foi.u.df$chikv_positive[1:(j-1)])
  if (j > 5){
    Ieff <- c(foi.u.df$chikv_positive[j:(j-4)])
    foi.u.df$Ieff[j] <- sum(Ieff*weights,na.rm=T)
  }
}

# subset data for model (week)
foi.m.df <- foi.m.df[6:nrow(foi.m.df),]
S <- foi.m.df$S
I <- foi.m.df$chikv_positive + 0.0001
Ieff <- foi.m.df$Ieff + 0.0001
N <- S.inits[i]
week.number <- foi.m.df$week.number

# run model
model<-glm(log(I)-log(S)+log(N)~-1+as.factor(week.number)+log(Ieff))
foi.m.df$betas <- NA
foi.m.df$betas<-coef(model)[-length(coef(model))]
foi.m.df$betas[foi.m.df$betas < 0] <- 0

# plot
plot.ts(foi.m.df$betas)
library(ggplot2)
ggplot(data = foi.m.df, aes(x = week.number, y = betas)) + geom_line() + 
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
ggsave("msambweni_FOI.jpeg")

# subset data for model (week)
foi.u.df <- foi.u.df[6:nrow(foi.u.df),]
S <- foi.u.df$S
I <- foi.u.df$chikv_positive + 0.0001
Ieff <- foi.u.df$Ieff + 0.0001
N <- S.inits[i]
week.number <- foi.u.df$week.number

# run model
model<-glm(log(I)-log(S)+log(N)~-1+as.factor(week.number)+log(Ieff))
foi.u.df$betas<-coef(model)[-length(coef(model))]
foi.u.df$betas[foi.u.df$betas < 0] <- 0

# plot
plot.ts(foi.u.df$betas)
ggplot(data = foi.u.df, aes(x = week.number, y = betas)) + geom_line() + 
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
ggsave("ukunda_FOI.jpeg")

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

outbreak.aic <- subset(EV_aic, id_site == "Msambweni" | id_site == "Ukunda" &
                         result_igg_chikv_stfd == 1|result_pcr_chikv_kenya==1|result_pcr_chikv_stfd==1|chikv_result_ufi==1)
outbreak.time.aic <- subset(outbreak.aic, date_start_infectious > "2017-10-15" & date_start_infectious < "2018-06-15")

travelNet <- ddply(outbreak.time.aic, .(where_travel_aic),
                   summarise,
                   Msambweni = sum(id_site == "Msambweni"),
                   Ukunda = sum(id_site == "Ukunda"))

write.csv(travelNet, "travelNet.csv")
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

### Result: traveling to areas near or in Mombasa is consistent throughout outbreak

# subset 
outbreakTravelers <- subset(outbreak.time.aic, where_travel_aic == "Mombasa"|
                              where_travel_aic == "Likoni"|
                              where_travel_aic == "Mwaembe"|
                              where_travel_aic == "Portreiz")
sumTravelers <- ddply(outbreakTravelers, .(where_travel_aic, epiwk, Date),
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
