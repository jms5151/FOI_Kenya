### Mailo Numazu, Stanford University, BSURPS 2018

## Data: AIC (Acutely Ill COhort) Surveys

## Purpose: (1) Determine number of kids tested over time
#           (2) Determine survey frequency (how many kids had 'x' number of visits)
#           (3) Summarize AIC data based on age
#           (4) Age Seroprevalence

# Set working directory
setwd("C:/Users/mailo/OneDrive/Desktop/Stanford 18-19/BSURPS/Working Directory")

#install.packages("ggplot2")
#install.packages("munsell")
#install.packages("plyr")

# load library
library('ggplot2')
library('plyr')

# load data (header = T just specifies that the data has a header row)
aic <- read.csv("aic_igg_igm.csv", header = T)

# convert interview date to date class 
aic$int_date <- as.Date(aic$int_date, "%m/%d/%Y")



# Purpose (1): Determine number of kids in data set
firstIDaic <- aic[!duplicated(aic$person_id),] # eliminate duplicate IDs



# Purpose (2): Determine survey frequency (how many kids had 'x' number of visits)
dfaic <- data.frame(cbind(count(aic, vars = "person_id"))) # count how many times the IDs repeat to count the number of times kids came in to the clinic
table(dfaic$freq)



# Purpose (3): Summarize AIC data based on age

# determine how many kids are at each age
dfage <- data.frame(cbind(count(firstIDaic, vars = "age"))) # count how many kids are 'x' years old
aic_age <- subset(dfage, age >= 0) # get rid of NA

# determine age of DENV+ kids
aic.denv_pos <- subset(aic, result_igg_denv_stfd == 1) # subset kids that are DENV+
aic.denvpos_age <- data.frame(cbind(count(aic.denv_pos, vars = "age"))) # count how many DENV+ kids are 'x' years old

# determine age of DENV - kids
aic.denv_neg <- subset(aic, result_igg_denv_stfd == 0) # subset kids that are DENV-
aic.denvneg_age <- data.frame(cbind(count(aic.denv_neg, vars = "age"))) # count how many DENV- kids are 'x' years old

# bar plot of number of DENV+ kids in each age group
ggplot() + 
  geom_col(data = aic.denvpos_age, aes(x = age, y = freq), fill = "skyblue3") +
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
  scale_x_continuous(breaks = seq(1,17,1)) +
  ggtitle("Number of Dengue Positive Kids in Each Age Group")
ggsave("aic_DENVpos_age.jpeg")



# Purpose (4): Age Seroprevalence

# subset kids w/ DENV results and get rid of NA
aic.denv_res <- subset(aic, result_igg_denv_stfd <= 1) 

# convert results to character class to make bar chart easier to graph
aic.denv_res$result_igg_denv_stfd <- as.character(aic.denv_res$result_igg_denv_stfd)

# determine age of kids with DENV results
aic.denvres_age <- data.frame(cbind(count(aic.denv_res, vars = "age"))) # count kids that are 'x' years old

# count how many kids within each age group are + or -
age_res = c("age", "result_igg_denv_stfd") # create vector to count by when calculating frequency of age for kids w/ DENV
age_res_freq = count(aic.denv_res, age_res)

# plot number of kids in each age group that are DENV + or DENV - 
ggplot() + 
  geom_bar(data = age_res_freq, stat = "identity", width = 0.95,
           aes(x = age, y = freq, fill = result_igg_denv_stfd),
           na.rm = TRUE
           ) +
  geom_text(data = age_res_freq, aes(
    x = age, y = freq, 
    label = freq), 
    check_overlap = TRUE,
    size = 4,
    nudge_y = 18
    ) +
  labs(
    x = "Age Groups",
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
  scale_x_continuous(breaks = seq(0,19,1)) +
  scale_fill_manual(
    values = alpha(c("bisque1", "skyblue3")),
    breaks = c("0", "1"),
    labels = c("0/Negative", "1/Positive")
    ) +
  ggtitle("Amount of Dengue Positive and Negative Kids in Each Age Group for AIC")
ggsave("aic_DENV_res_age.jpeg")

                         
# plot frequency of DENV prevalence against age
ggplot() + 
  geom_col(data = aic.denvpos_age, aes(x = age, y = freq), fill = "skyblue3") +
  labs(
    x = "Age Groups",
    y = "Frequency",
    fill = NULL
  ) + 
  theme(plot.title = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.3)),
        axis.text = element_text(size = rel(1.1)),
        axis.ticks.x = element_blank(),
        legend.position = "none",
        panel.background = element_rect("gray96"),
        panel.grid = element_line("gray88")) +
  scale_x_continuous(breaks = seq(1,17,1)) +
  ggtitle("Frequency of Dengue Positive Kids in Each Age Group for AIC")
ggsave("aic_denv_freq_age.jpeg")

# plot DENV results against date
ggplot() +
  geom_point(data = aic.denv_res, aes(x = int_date, y = result_igg_denv_stfd)) +
  labs(
    x = "Date",
    y = "Dengue Results"
  ) +
  theme(plot.title = element_text(size = rel(1.3)),
        axis.title = element_text(size = rel(1.1)),
        axis.text.x = element_text(size = rel(1)),
        axis.text.y = element_text(size = rel(1)),
        panel.background = element_rect("gray96"),
        panel.grid.major.x = element_line(colour = "gray88"),
        panel.grid.major.y = element_line(colour = "gray88")) + 
  ggtitle("AIC Dengue Results Over Time")
ggsave("aic_denv_results_time.jpeg")
