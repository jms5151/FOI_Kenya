### Mailo Numazu, Stanford University, BSURPS 2018

## Data: Expanded Visits AIC (Acutely Ill COhort) Surveys 

# Similar to aic_data.R except with more variables and includes visits past 'b'
# Originally used expanded_aic that had more variables, but did not include visits past 'b'. The code for this data set is '#' out

## Purpose: (1) Summarize data
#           (2) Find DENV/CHIKV prevalence based on 3 tests
#           (3) Graph Seroconversion for both DENV and CHIKV
#           (4) Graph Age Seroprevalence for both DENV and CHIKV



# Set working directory
setwd("C:/Users/mailo/OneDrive/Desktop/Stanford 18-19/BSURPS/Working Directory")

#install.packages("ggplot2")
#install.packages("munsell")
#install.packages("plyr")

# load library
library('ggplot2')
library('plyr')

# load data (header = T just specifies that the data has a header row)
# expanded_aic <- read.csv("expanded_aic_results.csv", header = T)

# convert interview date to date class 
# expanded_aic$interview_date_aic <- as.Date(expanded_aic$interview_date_aic, "%m/%d/%Y")

# load expanded visits (EV) data
EV_aic <- read.csv("expanded_visits_aic.csv", header = T)

# convert interview date to date class 
EV_aic$interview_date_aic <- as.Date(EV_aic$interview_date_aic, "%m/%d/%Y")




# Purpose (1): Summarize Data

# Overall number of kids
# firstID_EA <- expanded_aic[!duplicated(expanded_aic$person_id),]
# expanded visits
firstID_EV <- EV_aic[!duplicated(EV_aic$person_id),] # eliminate duplicate IDs


# Survey frequency of each child 
#survfreq_EA <- data.frame(cbind(count(expanded_aic, vars = "person_id")))
#table(survfreq_EA$freq)
# expanded visits
survfreq_EV <- data.frame(cbind(count(EV_aic, vars = "person_id"))) # count how many times the IDs repeat to count the number of times kids came in to the clinic
table(survfreq_EV$freq)


# Number of kids in each age group
#age_EA <- data.frame(cbind(count(firstID_EA, vars = "age")))
# expanded visits
age_EV <- data.frame(cbind(count(firstID_EV, vars = "age"))) # count how many kids are 'x' years old


# Number of girls and boys
#gender_EA <- cbind(count(firstID_EA, vars = "gender_aic"))
# expanded visits
gender_EV <- cbind(count(firstID_EV, vars = "gender_aic")) # count how many kids are boys or girls


# Number of girls and boys in each age group
#agegender_EA <-  data.frame(firstID_EA$gender_aic,firstID_EA$age)
#genderperage_EA <- data.frame(cbind(count(agegender_EA, c("firstID_EA.gender_aic", "firstID_EA.age"))))
# expanded visits
agegender_EV <-  data.frame(firstID_EV$gender_aic,firstID_EV$age) # subset gender and age info of each kid
agegender.freq_EV <- data.frame(cbind(count(agegender_EV, c("firstID_EV.gender_aic", "firstID_EV.age")))) # count how many boys and girls are 'x' years old


# Number in each occupation
#occupation_EA <- cbind(count(firstID_EA, vars = "occupation_aic"))
#expanded visits
occupation_EV <- cbind(count(firstID_EV, vars = "occupation_aic")) # count how many kids have 'x' occupation


# summarize by serotype
#serotypes <- subset(expanded_aic, serotype_pcr_denv_kenya___1 == 1|
#                      serotype_pcr_denv_stfd___2 == 1|
#                      serotype_pcr_denv_stfd___3 == 1|
#                      serotype_pcr_denv_stfd___4 == 1)
# expanded visits
serotypes_ev <- subset(EV_aic, serotype_pcr_denv_kenya___1 == 1|
                         serotype_pcr_denv_stfd___2 == 1|
                         serotype_pcr_denv_stfd___3 == 1|
                         serotype_pcr_denv_stfd___4 == 1)





### Purpose (2): Find DENV/CHIKV prevalence based on 3 tests

# DENV Prevalence - Total Number of DENV+ kids (from any of the tests)/ Total Number of kids

# IgG
#DENVposIgg <- subset(expanded_aic, result_igg_denv_stfd == 1)
#DENVposIGG <- DENVposIgg[!duplicated(DENVposIgg$person_id),]
#131/6495
# = 0.02016936
# expanded visits
DENV.posIgg_EV <- subset(EV_aic, result_igg_denv_stfd == 1) # subset kids that are DENV+ based on IgG results
DENV.posIgg_EV <- DENV.posIgg_EV[!duplicated(DENV.posIgg_EV$person_id),] # get rid of duplicate IDs in case of kids that came in positive for multiple visits
181/6563
# = 0.02757885


# PCR Kenya
#DENVposPcrK <- subset(expanded_aic, result_pcr_denv_kenya == 1)
#120/6495
# = 0.01847575
# expanded visits
DENV.posPCR.K_EV <- subset(EV_aic, result_pcr_denv_kenya == 1) # subset kids that are DENV+ based on their PCR results done in Kenya
128/6563
# = 0.01950328


# Ufi
#DENVposUfi <- subset(expanded_aic, denv_result_ufi == 1)
#36/6495
# = 0.005542725
# expanded visits
DENV.posUFI_EV <- subset(EV_aic, denv_result_ufi == 1) # subset kids that are DENV+ kids based on their UFI results
38/6563
# = 0.005790035


# subset DENV+ kids based on positive results from any test
#DENVpos <- subset(expanded_aic, result_igg_denv_stfd == 1 | 
#                    result_pcr_denv_kenya == 1 | 
#                    result_pcr_denv_stfd == 1 | 
#                    denv_result_ufi == 1)
#317/6495
# = 0.04880677
# expanded visits
DENVpos_EV <- subset(EV_aic, result_igg_denv_stfd == 1 | 
                    result_pcr_denv_kenya == 1 | 
                    denv_result_ufi == 1)
415/6563
# = 0.06323328


# CHIKV Prevalence - Total Number of CHIKV+ kids (from any of the tests)/ Total Number of kids

# IgG
CHIKV.posIgg_EV <- subset(EV_aic, result_igg_chikv_stfd == 1)
CHIKV.posIgg_EV <- CHIKV.posIgg_EV[!duplicated(CHIKV.posIgg_EV$person_id),]
132/6563
# = 0.02011275


# PCR Kenya
CHIKV.posPCR.K_EV <- subset(EV_aic, result_pcr_chikv_kenya == 1)
265/6563
# = 0.04037788


# Ufi
CHIKV.posUfi_ev <- subset(EV_aic, chikv_result_ufi == 1)
42/6563
# = 0.006399512


# subset DENV+ kids based on positive results from any test
CHIKVpos_EV <- subset(EV_aic, result_igg_chikv_stfd == 1 | 
                       result_pcr_chikv_kenya == 1 | 
                       chikv_result_ufi == 1)
465/6563
# = 0.07085174






### Purpose (3): Graph Seroconversion for both DENV and CHIKV

# extract kids that seroconverted based on igg results confirmed at Stanford
EV_DENV_sero <- subset(EV_aic, ab_denv_stfd_igg==1| 
                            bc_denv_stfd_igg==1|
                            cd_denv_stfd_igg==1|
                            de_denv_stfd_igg==1|
                            ef_denv_stfd_igg==1|
                            fg_denv_stfd_igg==1|
                            gh_denv_stfd_igg==1)
EV_CHIKV_sero <- subset(EV_aic, ab_chikv_stfd_igg==1| 
                          bc_chikv_stfd_igg==1|
                          cd_chikv_stfd_igg==1|
                          de_chikv_stfd_igg==1|
                          ef_chikv_stfd_igg==1|
                          fg_chikv_stfd_igg==1|
                          gh_chikv_stfd_igg==1)

# plot visits of seroconversion
ggplot() + geom_point(data = EV_DENV_sero, aes(x = interview_date_aic, 
                                               y = person_id, 
                                               colour = redcap_event_name), size = 2.5) + 
  labs(
    x = "Date",
    y = "Person ID",
    colour = "Visit Type"
  ) +
  theme(plot.title = element_text(size = rel(1.5)), 
        axis.title = element_text(size = rel(1.3)),
        axis.text.x = element_text(size = rel(1.25), angle = 90, hjust = 1), 
        axis.text.y = element_text(size = rel(1.25)),
        legend.title = element_text(face = "bold"),
        legend.position = c(0.97, 0.1),
        legend.justification = c("right", "bottom"),
        legend.box.just = "right",
        panel.background = element_rect("gray96"),
        panel.grid.major.x = element_line(colour = "gray84"),
        panel.grid.major.y = element_line(colour = "gray88"),
        panel.grid.minor.x = element_line(colour = "gray91")) +
  scale_colour_manual(
    values = alpha(c("gray23", "indianred2", "chartreuse", "cyan", "darkgoldenrod1", "deeppink", "magenta", "orange")),
    breaks = c("visit_a_arm_1", 
               "visit_b_arm_1", 
               "visit_c_arm_1", 
               "visit_d_arm_1", 
               "visit_e_arm_1", 
               "visit_f_arm_1", 
               "visit_g_arm_1",
               "visit_h_arm_1"),
    labels = c("Initial Visit", "Visit B", "Visit C", "Visit D", "Visit E", "Visit F", "Visit G", "Visit H")
  ) +
  ggtitle("AIC Survey Frequency of Seroconverted Children")
ggsave("DENV_EV_seroconv.jpeg")

ggplot() + geom_point(data = EV_CHIKV_sero, aes(x = interview_date_aic, 
                                               y = person_id, 
                                               colour = redcap_event_name), size = 2.5) + 
  labs(
    x = "Date",
    y = "Person ID",
    colour = "Visit Type"
  ) +
  theme(plot.title = element_text(size = rel(1.5)), 
        axis.title = element_text(size = rel(1.3)),
        axis.text.x = element_text(size = rel(1.25), angle = 90, hjust = 1), 
        axis.text.y = element_text(size = rel(1.25)),
        legend.title = element_text(face = "bold"),
        legend.position = c(0.97, 0.1),
        legend.justification = c("right", "bottom"),
        legend.box.just = "right",
        panel.background = element_rect("gray96"),
        panel.grid.major.x = element_line(colour = "gray84"),
        panel.grid.major.y = element_line(colour = "gray88"),
        panel.grid.minor.x = element_line(colour = "gray91")) +
  scale_colour_manual(
    values = alpha(c("gray23", "indianred2", "chartreuse", "cyan", "darkgoldenrod1", "deeppink", "magenta", "orange")),
    breaks = c("visit_a_arm_1", 
               "visit_b_arm_1", 
               "visit_c_arm_1", 
               "visit_d_arm_1", 
               "visit_e_arm_1", 
               "visit_f_arm_1", 
               "visit_g_arm_1",
               "visit_h_arm_1"),
    labels = c("Initial Visit", "Visit B", "Visit C", "Visit D", "Visit E", "Visit F", "Visit G", "Visit H")
  ) +
  ggtitle("AIC Survey Frequency of Seroconverted Children")
ggsave("CHIKV_EV_seroconv.jpeg")




# Purpose (4): Graph Age Seroprevalence for both DENV and CHIKV

# Extract Age Frequency
#agefreq <- ddply(expanded_aic, .(age), summarise, 
#                 DENVposfreq = sum(result_igg_denv_stfd==1|
#                                     result_pcr_denv_kenya==1|
#                                     denv_result_ufi==1,na.rm = T), 
#                 DENVnegfreq = sum(result_igg_denv_stfd==0|
#                                     result_pcr_denv_kenya==0|
#                                     denv_result_ufi==0, na.rm=T))

#agepos <- agefreq[,c("age", "DENVposfreq")]
#agepos$res <- 1
#colnames(agepos)[2] <- "freq"
#ageneg <- agefreq[,c("age", "DENVnegfreq")]
#ageneg$res <- 0
#colnames(ageneg)[2] <- "freq"
#ageDENVpos <- rbind(agepos, ageneg)

# find the cumulative sum at each age
#agefreq$cumulative <- ((cumsum(agefreq$DENVposfreq))/6495)

# graph age seroprevalence
#ggplot() + geom_line() + geom_smooth(data = agefreq, aes(x = age, y = cumulative), se = FALSE) +
#  labs(
#    x = "Age",
#    y = "Seroprevalence"
#  ) +
#  theme(plot.title = element_text(size = rel(1.5)),
#        axis.title = element_text(size = rel(1.3)),
#        axis.text.x = element_text(size = rel(1.2)),
#        axis.text.y = element_text(size = rel(1.2)),
#        panel.background = element_rect("gray96"),
#        panel.grid.major.x = element_line(colour = "gray88"),
#        panel.grid.major.y = element_line(colour = "gray88")) + 
#  ggtitle("AIC Age Seroprevalence")
#ggsave("EA_age_seroprevalence.jpeg")

# expanded visits
agefreq_DENV <- ddply(EV_aic, .(age), summarise, 
                    DENVposfreq = sum(result_igg_denv_stfd==1|
                                        result_pcr_denv_kenya==1|
                                        denv_result_ufi==1,na.rm = T), 
                    DENVnegfreq = sum(result_igg_denv_stfd==0|
                                        result_pcr_denv_kenya==0|
                                        denv_result_ufi==0, na.rm=T))

agepos_DENV <- agefreq_DENV[,c("age", "DENVposfreq")]
agepos_DENV$res <- 1
colnames(agepos_DENV)[2] <- "freq"
ageneg_DENV <- agefreq_DENV[,c("age", "DENVnegfreq")]
ageneg_DENV$res <- 0
colnames(ageneg_DENV)[2] <- "freq"
ageDENVpos_DENV <- rbind(agepos_DENV, ageneg_DENV)

# find the cumulative sum at each age
agefreq_DENV$cumulative <- ((cumsum(agefreq_DENV$DENVposfreq))/6495)

# graph age seroprevalence
ggplot() + geom_line() + geom_smooth(data = agefreq_DENV, aes(x = age, y = cumulative), se = FALSE) +
  labs(
    x = "Age",
    y = "Seroprevalence"
  ) +
  theme(plot.title = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.3)),
        axis.text.x = element_text(size = rel(1.2)),
        axis.text.y = element_text(size = rel(1.2)),
        panel.background = element_rect("gray96"),
        panel.grid.major.x = element_line(colour = "gray88"),
        panel.grid.major.y = element_line(colour = "gray88")) + 
  ggtitle("DENV Age Seroprevalence")
ggsave("DENV_age_seroprevalence.jpeg")



# CHIKV age seroprevalence
agefreq_CHIKV <- ddply(EV_aic, .(age), summarise, 
                    CHIKVposfreq = sum(result_igg_chikv_stfd==1|
                                        result_pcr_chikv_kenya==1|
                                        result_pcr_chikv_stfd==1|
                                        chikv_result_ufi==1,na.rm = T), 
                    CHIKVnegfreq = sum(result_igg_chikv_stfd==0|
                                        result_pcr_chikv_kenya==0|
                                         result_pcr_chikv_stfd==0|
                                        chikv_result_ufi==0, na.rm=T))

agepos_CHIKV <- agefreq_CHIKV[,c("age", "CHIKVposfreq")]
agepos_CHIKV$res <- 1
colnames(agepos_CHIKV)[2] <- "freq"
ageneg_CHIKV <- agefreq_CHIKV[,c("age", "CHIKVnegfreq")]
ageneg_CHIKV$res <- 0
colnames(ageneg_CHIKV)[2] <- "freq"
ageCHIKV <- rbind(agepos_CHIKV, ageneg_CHIKV)

# find the cumulative sum at each age
agefreq_CHIKV$cumulative <- ((cumsum(agefreq_CHIKV$CHIKVposfreq))/6495)

# graph age seroprevalence
ggplot() + geom_line() + geom_smooth(data = agefreq_CHIKV, aes(x = age, y = cumulative, colour = "skyblue", size = 2), se = FALSE) +
  labs(
    x = "Age",
    y = "Seroprevalence"
  ) +
  theme(axis.title = element_text(size = rel(3.5)),
        axis.text.x = element_text(size = rel(3)),
        axis.text.y = element_text(size = rel(3)),
        legend.position = "none",
        panel.background = element_rect("gray96"),
        panel.grid.major.x = element_line(colour = "gray88"),
        panel.grid.major.y = element_line(colour = "gray88")) + 
  scale_colour_manual(
    values = alpha(c("skyblue")))
ggsave("CHIKV_age_seroprevalence.jpeg")



