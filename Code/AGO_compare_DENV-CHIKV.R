### Mailo Numazu, Stanford University, BSURPS 2018

## Data: Expanded Visits AIC (Acutely Ill COhort) Surveys 

## Purpose: to summarize data by Age, Gender, and Occupation
#           (1) Stacked bar graphs comparing DENV+ v. DENV- for age, gender, and occupation (better to use pcr/ufi)
#           (1a) Stacked bar graphs comparing CHIKV+ v. CHIKV- for age, gender, and occupation


# Set working directory
setwd("C:/Users/mailo/OneDrive/Desktop/Stanford 18-19/BSURPS/Working Directory")

#install.packages("ggplot2")
#install.packages("munsell")
#install.packages("plyr")

# load library
library('ggplot2')
library('plyr')


# load expanded visits (EV) data
EV_aic <- read.csv("expanded_visits_aic.csv", header = T)

# convert interview date to date class 
EV_aic$interview_date_aic <- as.Date(EV_aic$interview_date_aic, "%Y-%m-%d")


# Purpose (1): Stacked bar graphs comparing DENV+ v. DENV- for age, gender, and occupation (better to use pcr/ufi)

# Age ----------------------------------------------------------------------------------------------------------------------------------------------

agefreq_DENV <- ddply(expanded_visits_aic, .(age), summarise, 
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
ageDENV <- rbind(agepos_DENV, ageneg_DENV)

# Plot both DENV+ and DENV-
ggplot() + 
  geom_bar(data = ageDENV, stat = "identity", width = 0.95,
           aes(x = age, y = freq, fill = as.factor(res)),
           na.rm = TRUE
  ) +
  geom_text(data = ageDENV, aes(
    x = age, y = freq, 
    label = freq, colour = as.factor(res)), 
    check_overlap = FALSE,
    size = 3.5,
    vjust = -0.5
  ) +
  labs(
    x = "Age Groups",
    y = "Number of Kids",
    fill = "DENV Result",
    colour = "DENV Result"
  ) + 
  theme(plot.title = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.3)),
        axis.text = element_text(size = rel(1.1)),
        axis.ticks.x = element_blank(),
        legend.position = "right",
        panel.background = element_rect("gray96"),
        panel.grid = element_line("gray88")) +
  scale_x_continuous(breaks = seq(0,20,1)) +
  scale_fill_manual(
    values = alpha(c("bisque1", "skyblue3")),
    breaks = c("0", "1"),
    labels = c("0/Negative", "1/Positive")
  ) +
  scale_colour_manual(
    values = alpha(c("maroon", "navy")),
    breaks = c("0", "1"),
    labels = c("0/Negative", "1/Positive")
  ) +
  ggtitle("Amount of DENV+/- Kids in Each Age Group for AIC")
ggsave("ev_EA_DENV_res_age.jpeg")


# Plot only DENV+
ggplot() + 
  geom_bar(data = agepos_DENV, stat = "identity", width = 0.95,
           aes(x = age, y = freq, fill = as.factor(res)),
           na.rm = TRUE
  ) +
  geom_text(data = agepos_DENV, aes(
    x = age, y = freq, 
    label = freq), 
    check_overlap = TRUE,
    size = 4,
    nudge_y = 0.75
  ) +
  labs(
    x = "Age Groups",
    y = "Number of Kids",
    fill = NULL
  ) + 
  theme(plot.title = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.3)),
        axis.text = element_text(size = rel(1.1)),
        axis.ticks.x = element_blank(),
        legend.position = "none",
        panel.background = element_rect("gray96"),
        panel.grid = element_line("gray88")) +
  scale_x_continuous(breaks = seq(0,20,1)) +
  scale_fill_manual(
    values = alpha(c("skyblue3"))
  ) +
  ggtitle("Amount of DENV+ Kids in Each Age Group for AIC")
ggsave("ev_EA_DENV_pos_age.jpeg")

# plot proportions
# find total in each age group
agefreq_DENV$total <- (agefreq_DENV$DENVnegfreq + agefreq_DENV$DENVposfreq)
# extract proportion positive in each age group
agefreq_DENV$proportion <- (agefreq_DENV$DENVposfreq/ agefreq_DENV$total)
# round to 3 decimal places
agefreq_DENV$proportion <- round(agefreq_DENV$proportion, 3) 

ggplot() + 
  geom_bar(data = ageDENV, stat = "identity", width = 0.95,
           aes(x = age, y = freq, fill = as.factor(res)),
           na.rm = TRUE
  ) +
  coord_flip() +
  geom_text(data = agefreq_DENV, aes(
    x = age, y = proportion, 
    label = proportion), 
    check_overlap = TRUE,
    size = 4,
    nudge_y = 100
  ) +
  labs(
    x = "Age Groups",
    y = "Number of Kids",
    fill = "DENV Results"
  ) + 
  theme(plot.title = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.3)),
        axis.text = element_text(size = rel(1.1)),
        axis.ticks.x = element_blank(),
        legend.position = "right",
        panel.background = element_rect("gray96"),
        panel.grid = element_line("gray88")) +
  scale_x_continuous(breaks = seq(0,20,1)) +
  scale_fill_manual(
    values = alpha(c("bisque1", "skyblue3")),
    breaks = c("0", "1"),
    labels = c("0/Negative", "1/Positive")
  ) +
  ggtitle("Proportion of DENV+ Kids in Each Age Group for AIC")
ggsave("ev_EA_proportion_DENV_age.jpeg")


# Gender -------------------------------------------------------------------------------------------------------------------------------------------

gendfreq_DENV <- ddply(expanded_visits_aic, .(gender_aic), summarise, 
                     DENVposfreq = sum(result_igg_denv_stfd==1|
                                         result_pcr_denv_kenya==1|
                                         denv_result_ufi==1,na.rm = T),
                     DENVnegfreq = sum(result_igg_denv_stfd==0|
                                         result_pcr_denv_kenya==0|
                                         denv_result_ufi==0, na.rm=T))

gendpos_DENV <- gendfreq_DENV[,c("gender_aic", "DENVposfreq")]
gendpos_DENV$res <- 1
colnames(gendpos_DENV)[2] <- "freq"
gendneg_DENV <- gendfreq_DENV[,c("gender_aic", "DENVnegfreq")]
gendneg_DENV$res <- 0
colnames(gendneg_DENV)[2] <- "freq"
gendDENV <- rbind(gendpos_DENV, gendneg_DENV)

# Plot both DENV+ and DENV-
ggplot() + 
  geom_bar(data = gendDENV, stat = "identity", width = 0.75,
           aes(x = gender_aic, y = freq, fill = as.factor(res)),
           na.rm = TRUE
  ) +
  geom_text(data = gendDENV, aes(
    x = gender_aic, y = freq, 
    label = freq), 
    check_overlap = TRUE,
    size = 4,
    nudge_y = 70
  ) +
  labs(
    x = "Gender",
    y = "Number of Kids",
    fill = "DENV Result"
  ) + 
  theme(plot.title = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.3)),
        axis.text = element_text(size = rel(1.1)),
        axis.ticks.x = element_blank(),
        legend.position = "right",
        panel.background = element_rect("gray96"),
        panel.grid = element_line("gray88")) +
  scale_x_continuous(
    breaks = c(0, 1),
    labels = c("Male", "Female")
  ) +
  scale_fill_manual(
    values = alpha(c("bisque1", "skyblue3")),
    breaks = c("0", "1"),
    labels = c("0/Negative", "1/Positive")
  ) +
  ggtitle("Number of DENV+/- Kids Per Gender for AIC")
ggsave("ev_EA_DENV_posneg_gender.jpeg")


# Plot only DENV+
ggplot() + 
  geom_bar(data = gendpos_DENV, stat = "identity", width = 0.75,
           aes(x = gender_aic, y = freq, fill = as.factor(res)),
           na.rm = TRUE
  ) +
  geom_text(data = gendpos_DENV, aes(
    x = gender_aic, y = freq, 
    label = freq), 
    check_overlap = TRUE,
    size = 5,
    nudge_y = 2.5
  ) +
  labs(
    x = "Gender",
    y = "Number of Kids",
    fill = NULL
  ) + 
  theme(plot.title = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.3)),
        axis.text = element_text(size = rel(1.1)),
        axis.ticks.x = element_blank(),
        legend.position = "none",
        panel.background = element_rect("gray96"),
        panel.grid = element_line("gray88")) +
  scale_x_continuous(
    breaks = c(0, 1),
    labels = c("Male", "Female")
  ) +
  scale_fill_manual(
    values = alpha(c("skyblue3"))
  ) +
  ggtitle("Number of DENV+ Kids Per Gender for AIC")
ggsave("ev_EA_DENV_pos_gender.jpeg")

# plot proportions

# find total
gendfreq_DENV$total <- (gendfreq_DENV$DENVposfreq + gendfreq_DENV$DENVnegfreq)
# find proportion
gendfreq_DENV$proportion <- (gendfreq_DENV$DENVposfreq/ gendfreq_DENV$total)
# round to 3 decimal places
gendfreq_DENV$proportion <- round(gendfreq_DENV$proportion, 3) 

ggplot() + 
  geom_bar(data = gendDENV, stat = "identity", width = 0.95,
           aes(x = gender_aic, y = freq, fill = as.factor(res)),
           na.rm = TRUE
  ) +
  geom_text(data = gendfreq_DENV, aes(
    x = gender_aic, y = proportion, 
    label = proportion), 
    check_overlap = TRUE,
    size = 5,
    vjust = "top",
    hjust = "middle"
  ) +
  labs(
    x = "Gender",
    y = "Number of Kids",
    fill = "DENV Results"
  ) + 
  theme(plot.title = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.3)),
        axis.text = element_text(size = rel(1.1)),
        axis.ticks.x = element_blank(),
        legend.position = "right",
        panel.background = element_rect("gray96"),
        panel.grid = element_line("gray88")) +
  scale_x_continuous(breaks = seq(0,20,1)) +
  scale_fill_manual(
    values = alpha(c("bisque1", "skyblue3")),
    breaks = c("0", "1"),
    labels = c("0/Negative", "1/Positive")
  ) +
  ggtitle("Proportion of DENV+ to DENV- Kids Per Gender for AIC")
ggsave("ev_EA_proportion_DENV_gender.jpeg")


# Occupation ----------------------------------------------------------------------------------------------------------------------------------------

expanded_visits_aic$occupation_aic <- as.character(expanded_visits_aic$occupation_aic)

occfreq_DENV <- ddply(expanded_visits_aic, .(occupation_aic), summarise, 
                    DENVposfreq = sum(result_igg_denv_stfd==1|
                                        result_pcr_denv_kenya==1|
                                        denv_result_ufi==1,na.rm = T),
                    DENVnegfreq = sum(result_igg_denv_stfd==0|
                                        result_pcr_denv_kenya==0|
                                        denv_result_ufi==0, na.rm=T))

occpos_DENV <- occfreq_DENV[,c("occupation_aic", "DENVposfreq")]
occpos_DENV$res <- 1
colnames(occpos_DENV)[2] <- "freq"
occneg_DENV <- occfreq_DENV[,c("occupation_aic", "DENVnegfreq")]
occneg_DENV$res <- 0
colnames(occneg_DENV)[2] <- "freq"
occDENV <- rbind(occpos_DENV, occneg_DENV)
# exclude 99 and NA and 0 freq occupations
subocc_DENV <- subset(occDENV, freq > 0 & occupation_aic < 99)
suboccpos_DENV <- subset(occpos, freq > 0 & occupation_aic < 99)

# Plot both DENV+ and DENV-
ggplot() + 
  geom_bar(data = subocc_DENV, stat = "identity", width = 1,
           aes(x = occupation_aic, y = freq, fill = as.factor(res)),
           na.rm = TRUE
  ) +
  geom_text(data = subocc_DENV, aes(
    x = occupation_aic, y = freq, 
    label = freq), 
    check_overlap = TRUE,
    size = 4,
    nudge_y = 60
  ) +
  labs(
    x = "Occupation",
    y = "Number of Kids",
    fill = "DENV Result"
  ) + 
  theme(plot.title = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.3)),
        axis.text = element_text(size = rel(1)),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 70, hjust = 1),
        legend.position = "right",
        panel.background = element_rect("gray96"),
        panel.grid = element_line("gray88")) +
  scale_x_discrete(
    breaks = c(1,2,3,4,5,6,7,8,9,10),
    labels = c("Baby", 
               "Madrassa", 
               "Nursery School", 
               "Primary School", 
               "Secondary School", 
               "Other Student", 
               "Housewife",
               "Herder",
               "Business",
               "Other")
  ) +
  scale_fill_manual(
    values = alpha(c("bisque1", "skyblue3")),
    breaks = c("0", "1"),
    labels = c("0/Negative", "1/Positive")
  ) +
  ggtitle("Number of DENV+/- Kids Per Occupation for AIC")
ggsave("ev_EA_DENV_posneg_occupation.jpeg")


# Plot only DENV+
ggplot() + 
  geom_bar(data = suboccpos_DENV, stat = "identity", width = 0.75,
           aes(x = occupation_aic, y = freq, fill = as.factor(res)),
           na.rm = TRUE
  ) +
  geom_text(data = suboccpos_DENV, aes(
    x = occupation_aic, y = freq, 
    label = freq), 
    check_overlap = TRUE,
    size = 5,
    nudge_y = 2.5
  ) +
  labs(
    x = "Occupation",
    y = "Number of Kids",
    fill = NULL
  ) + 
  theme(plot.title = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.3)),
        axis.text = element_text(size = rel(1)),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 70, hjust = 1),
        legend.position = "none",
        panel.background = element_rect("gray96"),
        panel.grid = element_line("gray88")) +
  scale_x_discrete(
    breaks = c(1,2,3,4,5,6,7,8,9,10),
    labels = c("Baby", 
               "Madrassa", 
               "Nursery School", 
               "Primary School", 
               "Secondary School", 
               "Other Student", 
               "Housewife",
               "Herder",
               "Business",
               "Other")
  ) +
  scale_fill_manual(
    values = alpha(c("skyblue3"))
  ) +
  ggtitle("Number of DENV+ Kids Per Occupation for AIC")
ggsave("ev_EA_DENV_pos_occupation.jpeg")

# plot proportions

# exclude 99 and NA and 0 freq occupations
suboccfreq_DENV <- subset(occfreq_DENV, occupation_aic < 99)
# find total
suboccfreq_DENV$total <- (suboccfreq_DENV$DENVnegfreq + suboccfreq_DENV$DENVposfreq)
# find proportion
suboccfreq_DENV$proportion <- (suboccfreq_DENV$DENVposfreq/ suboccfreq_DENV$total)
# round to 3 decimal places
suboccfreq_DENV$proportion <- round(suboccfreq_DENV$proportion, 3) 

ggplot() + 
  geom_bar(data = subocc_DENV, stat = "identity", width = 0.95,
           aes(x = occupation_aic, y = freq, fill = as.factor(res)),
           na.rm = TRUE
  ) +
  geom_text(data = suboccfreq_DENV, aes(
    x = occupation_aic, y = proportion, 
    label = proportion), 
    check_overlap = TRUE,
    size = 5,
    vjust = "top",
    hjust = "middle"
  ) +
  labs(
    x = "Occupation",
    y = "Number of Kids",
    fill = "DENV Results"
  ) + 
  theme(plot.title = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.3)),
        axis.text = element_text(size = rel(1)),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 70, hjust = 1),
        legend.position = "none",
        panel.background = element_rect("gray96"),
        panel.grid = element_line("gray88")) +
  scale_x_discrete(
    breaks = c(1,2,3,4,5,6,7,8,9,10),
    labels = c("Baby", 
               "Madrassa", 
               "Nursery School", 
               "Primary School", 
               "Secondary School", 
               "Other Student", 
               "Housewife",
               "Herder",
               "Business",
               "Other")
  ) +
  scale_fill_manual(
    values = alpha(c("bisque1", "skyblue3")),
    breaks = c("0", "1"),
    labels = c("0/Negative", "1/Positive")
  ) +
  ggtitle("Proportion of DENV+ to DENV- Kids Per Occupation for AIC")
ggsave("ev_EA_proportion_DENV_occ.jpeg")






# Purpose (1a): Stacked bar graphs comparing CHIKV+ v. CHIKV- for age, gender, and occupation -----------------------

# Age -----------------------------------------------------------------------------------------------------------------------------------------------
agefreq_CHIKV <- ddply(expanded_visits_aic, .(age), summarise, 
                          CHIKVposfreq = sum(result_igg_chikv_stfd==1|
                                               result_pcr_chikv_kenya==1|
                                               chikv_result_ufi==1,na.rm = T), 
                          CHIKVnegfreq = sum(result_igg_chikv_stfd==0|
                                               result_pcr_chikv_kenya==0|
                                               chikv_result_ufi==0, na.rm=T))

agepos_CHIKV <- agefreq_CHIKV[,c("age", "CHIKVposfreq")]
agepos_CHIKV$res <- 1
colnames(agepos_CHIKV)[2] <- "freq"
ageneg_CHIKV <- agefreq_CHIKV[,c("age", "DENVnegfreq")]
ageneg_CHIKV$res <- 0
colnames(ageneg_CHIKV)[2] <- "freq"
ageCHIKV <- rbind(agepos_CHIKV, ageneg_CHIKV)

# Plot both CHIKV+ and CHIKV-
ggplot() + 
  geom_bar(data = ageCHIKV, stat = "identity", width = 0.95,
           aes(x = age, y = freq, fill = as.factor(res)),
           na.rm = TRUE
  ) +
  geom_text(data = ageCHIKV, aes(
    x = age, y = freq, 
    label = freq, colour = as.factor(res)), 
    check_overlap = FALSE,
    size = 3.5,
    vjust = -0.5
  ) +
  labs(
    x = "Age Groups",
    y = "Number of Kids",
    fill = "CHIKV Result",
    colour = "CHIKV Result"
  ) + 
  theme(plot.title = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.3)),
        axis.text = element_text(size = rel(1.1)),
        axis.ticks.x = element_blank(),
        legend.position = "right",
        panel.background = element_rect("gray96"),
        panel.grid = element_line("gray88")) +
  scale_x_continuous(breaks = seq(0,20,1)) +
  scale_fill_manual(
    values = alpha(c("bisque1", "skyblue3")),
    breaks = c("0", "1"),
    labels = c("0/Negative", "1/Positive")
  ) +
  scale_colour_manual(
    values = alpha(c("maroon", "navy")),
    breaks = c("0", "1"),
    labels = c("0/Negative", "1/Positive")
  ) +
  ggtitle("Amount of CHIKV+/- Kids in Each Age Group for AIC")
ggsave("ev_EA_CHIKV_res_age.jpeg")


# Plot only CHIKV+
ggplot() + 
  geom_bar(data = agepos_CHIKV, stat = "identity", width = 0.95,
           aes(x = age, y = freq, fill = as.factor(res)),
           na.rm = TRUE
  ) +
  geom_text(data = agepos_CHIKV, aes(
    x = age, y = freq, 
    label = freq), 
    check_overlap = TRUE,
    size = 4,
    nudge_y = 0.75
  ) +
  labs(
    x = "Age Groups",
    y = "Number of Kids",
    fill = NULL
  ) + 
  theme(plot.title = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.3)),
        axis.text = element_text(size = rel(1.1)),
        axis.ticks.x = element_blank(),
        legend.position = "none",
        panel.background = element_rect("gray96"),
        panel.grid = element_line("gray88")) +
  scale_x_continuous(breaks = seq(0,20,1)) +
  scale_fill_manual(
    values = alpha(c("skyblue3"))
  ) +
  ggtitle("Amount of CHIKV+ Kids in Each Age Group for AIC")
ggsave("ev_EA_CHIKV_pos_age.jpeg")

# plot proportions
# find total in each age group
agefreq_CHIKV$total <- (agefreq_CHIKV$CHIKVnegfreq + agefreq_CHIKV$CHIKVposfreq)
# extract proportion positive in each age group
agefreq_CHIKV$proportion <- (agefreq_CHIKV$CHIKVposfreq/ agefreq_CHIKV$total)
# round to 3 decimal places
agefreq_CHIKV$proportion <- round(agefreq_CHIKV$proportion, 3) 

ggplot() + 
  geom_bar(data = ageCHIKV, stat = "identity", width = 0.95,
           aes(x = age, y = freq, fill = as.factor(res)),
           na.rm = TRUE
  ) +
  coord_flip() +
  geom_text(data = agefreq_CHIKV, aes(
    x = age, y = proportion, 
    label = proportion), 
    check_overlap = TRUE,
    size = 4,
    nudge_y = 100
  ) +
  labs(
    x = "Age Groups",
    y = "Number of Kids",
    fill = "CHIKV Results"
  ) + 
  theme(plot.title = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.3)),
        axis.text = element_text(size = rel(1.1)),
        axis.ticks.x = element_blank(),
        legend.position = "right",
        panel.background = element_rect("gray96"),
        panel.grid = element_line("gray88")) +
  scale_x_continuous(breaks = seq(0,20,1)) +
  scale_fill_manual(
    values = alpha(c("bisque1", "skyblue3")),
    breaks = c("0", "1"),
    labels = c("0/Negative", "1/Positive")
  ) +
  ggtitle("Proportion of CHIKV+ Kids in Each Age Group for AIC")
ggsave("ev_EA_proportion_CHIKV_age.jpeg")


# Gender -----------------------------------------------------------------------------------------------------------------------------------------

gendfreq_CHIKV <- ddply(expanded_visits_aic, .(gender_aic), summarise, 
                        CHIKVposfreq = sum(result_igg_chikv_stfd==1|
                                             result_pcr_chikv_kenya==1|
                                             chikv_result_ufi==1,na.rm = T), 
                        CHIKVnegfreq = sum(result_igg_chikv_stfd==0|
                                             result_pcr_chikv_kenya==0|
                                             chikv_result_ufi==0, na.rm=T))

gendpos_CHIKV <- gendfreq_CHIKV[,c("gender_aic", "CHIKVposfreq")]
gendpos_CHIKV$res <- 1
colnames(gendpos_CHIKV)[2] <- "freq"
gendneg_CHIKV <- gendfreq_CHIKV[,c("gender_aic", "CHIKVnegfreq")]
gendneg_CHIKV$res <- 0
colnames(gendneg_CHIKV)[2] <- "freq"
gendCHIKV <- rbind(gendpos_CHIKV, gendneg_CHIKV)

# Plot both CHIKV+ and CHIKV-
ggplot() + 
  geom_bar(data = gendCHIKV, stat = "identity", width = 0.75,
           aes(x = gender_aic, y = freq, fill = as.factor(res)),
           na.rm = TRUE
  ) +
  geom_text(data = gendCHIKV, aes(
    x = gender_aic, y = freq, 
    label = freq), 
    check_overlap = TRUE,
    size = 4,
    nudge_y = 70
  ) +
  labs(
    x = "Gender",
    y = "Number of Kids",
    fill = "CHIKV Result"
  ) + 
  theme(plot.title = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.3)),
        axis.text = element_text(size = rel(1.1)),
        axis.ticks.x = element_blank(),
        legend.position = "right",
        panel.background = element_rect("gray96"),
        panel.grid = element_line("gray88")) +
  scale_x_continuous(
    breaks = c(0, 1),
    labels = c("Male", "Female")
  ) +
  scale_fill_manual(
    values = alpha(c("bisque1", "skyblue3")),
    breaks = c("0", "1"),
    labels = c("0/Negative", "1/Positive")
  ) +
  ggtitle("Number of CHIKV+/- Kids Per Gender for AIC")
ggsave("ev_EA_CHIKV_posneg_gender.jpeg")


# Plot only DENV+
ggplot() + 
  geom_bar(data = gendpos_CHIKV, stat = "identity", width = 0.75,
           aes(x = gender_aic, y = freq, fill = as.factor(res)),
           na.rm = TRUE
  ) +
  geom_text(data = gendpos_CHIKV, aes(
    x = gender_aic, y = freq, 
    label = freq), 
    check_overlap = TRUE,
    size = 5,
    nudge_y = 2.5
  ) +
  labs(
    x = "Gender",
    y = "Number of Kids",
    fill = NULL
  ) + 
  theme(plot.title = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.3)),
        axis.text = element_text(size = rel(1.1)),
        axis.ticks.x = element_blank(),
        legend.position = "none",
        panel.background = element_rect("gray96"),
        panel.grid = element_line("gray88")) +
  scale_x_continuous(
    breaks = c(0, 1),
    labels = c("Male", "Female")
  ) +
  scale_fill_manual(
    values = alpha(c("skyblue3"))
  ) +
  ggtitle("Number of CHIKV+ Kids Per Gender for AIC")
ggsave("ev_EA_CHIKV_pos_gender.jpeg")

# plot proportions

# find total
gendfreq_CHIKV$total <- (gendfreq_CHIKV$DENVposfreq + gendfreq_CHIKV$DENVnegfreq)
# find proportion
gendfreq_CHIKV$proportion <- (gendfreq_CHIKV$DENVposfreq/ gendfreq_CHIKV$total)
# round to 3 decimal places
gendfreq_CHIKV$proportion <- round(gendfreq_CHIKV$proportion, 3) 

ggplot() + 
  geom_bar(data = gendCHIKV, stat = "identity", width = 0.95,
           aes(x = gender_aic, y = freq, fill = as.factor(res)),
           na.rm = TRUE
  ) +
  geom_text(data = gendfreq_CHIKV, aes(
    x = gender_aic, y = proportion, 
    label = proportion), 
    check_overlap = TRUE,
    size = 5,
    vjust = "top",
    hjust = "middle"
  ) +
  labs(
    x = "Gender",
    y = "Number of Kids",
    fill = "DENV Results"
  ) + 
  theme(plot.title = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.3)),
        axis.text = element_text(size = rel(1.1)),
        axis.ticks.x = element_blank(),
        legend.position = "right",
        panel.background = element_rect("gray96"),
        panel.grid = element_line("gray88")) +
  scale_x_continuous(breaks = seq(0,20,1)) +
  scale_fill_manual(
    values = alpha(c("bisque1", "skyblue3")),
    breaks = c("0", "1"),
    labels = c("0/Negative", "1/Positive")
  ) +
  ggtitle("Proportion of CHIKV+ to CHIKV- Kids Per Gender for AIC")
ggsave("ev_EA_proportion_CHIKV_gender.jpeg")


# Occupation -------------------------------------------------------------------------------------------------------------------------------------

expanded_visits_aic$occupation_aic <- as.character(expanded_visits_aic$occupation_aic)

occfreq_CHIKV <- ddply(expanded_visits_aic, .(occupation_aic), summarise, 
                      CHIKVposfreq = sum(result_igg_chikv_stfd==1|
                                           result_pcr_chikv_kenya==1|
                                           chikv_result_ufi==1,na.rm = T), 
                      CHIKVnegfreq = sum(result_igg_chikv_stfd==0|
                                           result_pcr_chikv_kenya==0|
                                           chikv_result_ufi==0, na.rm=T))

occpos_CHIKV <- occfreq_CHIKV[,c("occupation_aic", "DENVposfreq")]
occpos_DENV$res <- 1
colnames(occpos_DENV)[2] <- "freq"
occneg_DENV <- occfreq_DENV[,c("occupation_aic", "DENVnegfreq")]
occneg_DENV$res <- 0
colnames(occneg_DENV)[2] <- "freq"
occDENV <- rbind(occpos_DENV, occneg_DENV)
# exclude 99 and NA and 0 freq occupations
subocc_DENV <- subset(occDENV, freq > 0 & occupation_aic < 99)
suboccpos_DENV <- subset(occpos, freq > 0 & occupation_aic < 99)

# Plot both DENV+ and DENV-
ggplot() + 
  geom_bar(data = subocc_DENV, stat = "identity", width = 1,
           aes(x = occupation_aic, y = freq, fill = as.factor(res)),
           na.rm = TRUE
  ) +
  geom_text(data = subocc_DENV, aes(
    x = occupation_aic, y = freq, 
    label = freq), 
    check_overlap = TRUE,
    size = 4,
    nudge_y = 60
  ) +
  labs(
    x = "Occupation",
    y = "Number of Kids",
    fill = "DENV Result"
  ) + 
  theme(plot.title = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.3)),
        axis.text = element_text(size = rel(1)),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 70, hjust = 1),
        legend.position = "right",
        panel.background = element_rect("gray96"),
        panel.grid = element_line("gray88")) +
  scale_x_discrete(
    breaks = c(1,2,3,4,5,6,7,8,9,10),
    labels = c("Baby", 
               "Madrassa", 
               "Nursery School", 
               "Primary School", 
               "Secondary School", 
               "Other Student", 
               "Housewife",
               "Herder",
               "Business",
               "Other")
  ) +
  scale_fill_manual(
    values = alpha(c("bisque1", "skyblue3")),
    breaks = c("0", "1"),
    labels = c("0/Negative", "1/Positive")
  ) +
  ggtitle("Number of DENV+/- Kids Per Occupation for AIC")
ggsave("ev_EA_DENV_posneg_occupation.jpeg")


# Plot only DENV+
ggplot() + 
  geom_bar(data = suboccpos_DENV, stat = "identity", width = 0.75,
           aes(x = occupation_aic, y = freq, fill = as.factor(res)),
           na.rm = TRUE
  ) +
  geom_text(data = suboccpos_DENV, aes(
    x = occupation_aic, y = freq, 
    label = freq), 
    check_overlap = TRUE,
    size = 5,
    nudge_y = 2.5
  ) +
  labs(
    x = "Occupation",
    y = "Number of Kids",
    fill = NULL
  ) + 
  theme(plot.title = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.3)),
        axis.text = element_text(size = rel(1)),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 70, hjust = 1),
        legend.position = "none",
        panel.background = element_rect("gray96"),
        panel.grid = element_line("gray88")) +
  scale_x_discrete(
    breaks = c(1,2,3,4,5,6,7,8,9,10),
    labels = c("Baby", 
               "Madrassa", 
               "Nursery School", 
               "Primary School", 
               "Secondary School", 
               "Other Student", 
               "Housewife",
               "Herder",
               "Business",
               "Other")
  ) +
  scale_fill_manual(
    values = alpha(c("skyblue3"))
  ) +
  ggtitle("Number of DENV+ Kids Per Occupation for AIC")
ggsave("ev_EA_DENV_pos_occupation.jpeg")

# plot proportions

# exclude 99 and NA and 0 freq occupations
suboccfreq_DENV <- subset(occfreq_DENV, occupation_aic < 99)
# find total
suboccfreq_DENV$total <- (suboccfreq_DENV$DENVnegfreq + suboccfreq_DENV$DENVposfreq)
# find proportion
suboccfreq_DENV$proportion <- (suboccfreq_DENV$DENVposfreq/ suboccfreq_DENV$total)
# round to 3 decimal places
suboccfreq_DENV$proportion <- round(suboccfreq_DENV$proportion, 3) 

ggplot() + 
  geom_bar(data = subocc_DENV, stat = "identity", width = 0.95,
           aes(x = occupation_aic, y = freq, fill = as.factor(res)),
           na.rm = TRUE
  ) +
  geom_text(data = suboccfreq_DENV, aes(
    x = occupation_aic, y = proportion, 
    label = proportion), 
    check_overlap = TRUE,
    size = 5,
    vjust = "top",
    hjust = "middle"
  ) +
  labs(
    x = "Occupation",
    y = "Number of Kids",
    fill = "DENV Results"
  ) + 
  theme(plot.title = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.3)),
        axis.text = element_text(size = rel(1)),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 70, hjust = 1),
        legend.position = "none",
        panel.background = element_rect("gray96"),
        panel.grid = element_line("gray88")) +
  scale_x_discrete(
    breaks = c(1,2,3,4,5,6,7,8,9,10),
    labels = c("Baby", 
               "Madrassa", 
               "Nursery School", 
               "Primary School", 
               "Secondary School", 
               "Other Student", 
               "Housewife",
               "Herder",
               "Business",
               "Other")
  ) +
  scale_fill_manual(
    values = alpha(c("bisque1", "skyblue3")),
    breaks = c("0", "1"),
    labels = c("0/Negative", "1/Positive")
  ) +
  ggtitle("Proportion of DENV+ to DENV- Kids Per Occupation for AIC")
ggsave("ev_EA_proportion_DENV_occ.jpeg")





