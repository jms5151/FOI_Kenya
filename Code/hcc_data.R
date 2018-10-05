### Mailo Numazu, Stanford University, BSURPS 2018

## Data: HCC (Healthy Community Cohort) Surveys

# Useful b/c all patients are healthy and we are able to look at the patterns of when people become sick

## Purpose: (1) Determine survey frequency (how many kids had 'x' number of visits)
#           (2) Determine visit frequency of kids that seroconvert
#           (3) Plot IgG and IgM over time in kids that seroconvert
#           (4) Plot IgG and IgM levels between DENV- and DENV+

# Set working directory
setwd("C:/Users/mailo/OneDrive/Desktop/Stanford 18-19/BSURPS/Working Directory")

#install.packages("ggplot2")
#install.packages("munsell")
#install.packages("plyr")

# load library
library('ggplot2')
library('plyr')

# load data (header = T just specifies that the data has a header row)
hcc <- read.csv("hcc_igg_igm.csv", header = T)

# convert interview date to date class 
hcc$int_date <- as.Date(hcc$int_date, "%m/%d/%Y")



### Purpose (1): Determine survey frequency (how many kids had 'x' number of visits)

# Overall number of kids
firstID_hcc <- hcc[!duplicated(hcc$person_id),] # eliminate duplicate IDs

# Survey frequency of each child 
survfreq_hcc <- data.frame(cbind(count(hcc, vars = "person_id"))) # count how many times the IDs repeat to count the number of times kids came in to the clinic
table(survfreq_hcc$freq)




### Purpose (2): Determine visit frequency of kids that seroconvert

# subset individuals that were initially DENV- 
hcc_neg_init <- subset(hcc, redcap_event_name == "visit_a_arm_1" & result_igg_denv_stfd == 0)

# subset individuals that were DENV+ in later visits
hcc_denv_pos_conv <- subset(hcc, 
                        redcap_event_name %in% c(
  "visit_b_arm_1", 
  "visit_c_arm_1", 
  "visit_d_arm_1",
  "visit_e_arm_1", 
  "visit_f_arm_1") & 
    result_igg_denv_stfd == 1)

# find ID matches from hcc_neg_init in hcc_denv_pos_conv to find kids that became DENV+ after being initial DENV-
hcc_denv_sero_mat <- subset(hcc_neg_init, person_id %in% hcc_denv_pos_conv$person_id)

# extract seroconverted kids from main data set
hcc_denv_sero_freq <- subset(hcc, person_id %in% c("GC0045006", "GC0237005"))

# subset when kids seroconverted (since there were only 2 seroconversion cases, able to easily see seroconversion occurred in 2nd visit)
hcc_denv_sero_conv <- subset(hcc_denv_sero_freq, redcap_event_name == "visit_b_arm_1")

# graph survey frequency for seroconverted kids
ggplot() + geom_point(data = hcc_denv_sero_freq, aes(x = int_date, y = person_id), size = 2.5) + 
  geom_point(data = hcc_denv_sero_conv, aes(x = int_date, y = person_id, colour = redcap_event_name), size = 2.5) + 
  labs(
    x = "Date",
    y = "Person ID",
    colour = "Seroconversion"
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
    values = alpha(c("tomato")),
    breaks = c("visit_b_arm_1"),
    labels = c("2nd Visit")
    ) +
  ggtitle("HCC Survey Frequency of Seroconverted Children")
ggsave("hcc_seroconv_survfreq.jpeg")



### Purpose (3)

# graph changes in IgG antibodies in seroconverted kids
ggplot(data = hcc_denv_sero_freq, aes(x = int_date, y = value_igg_denv_stfd, colour = person_id)) +
  geom_point(na.rm = TRUE, size = 1.75) +
  geom_line(na.rm = TRUE, size = 1.25) + 
  labs(
    x = "Date",
    y = "Antibody Levels",
    colour = "Child ID"
  ) +
  theme(plot.title = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.3)),
        axis.text.x = element_text(size = rel(1.25), angle = 90, hjust = 1),
        axis.text.y = element_text(size = rel(1.25)),
        legend.title = element_text(face = "bold"),
        legend.position = c(0.95, 0.1),
        legend.justification = c("right", "bottom"),
        legend.box.just = "right",
        panel.background = element_rect("gray96"),
        panel.grid.major.x = element_line(colour = "gray84"),
        panel.grid.major.y = element_line(colour = "gray88"),
        panel.grid.minor.x = element_line(colour = "gray91")) + 
  ggtitle("HCC Antibody Levels Over Time in Seroconverted Children")
ggsave("hcc_seroconv_antibody.jpeg")



# Purpose (4)

# subset IgG levels in DENV- v. DENV+
hcc_igg_levels_neg <- subset(hcc_igg_igm, value_igg_denv_stfd > 0 & result_igg_denv_stfd == 0)
hcc_igg_levels_pos <- subset(hcc_igg_igm, value_igg_denv_stfd > 0 & result_igg_denv_stfd == 1)

# boxplot IgG levels in initial visit
ggplot() + 
  geom_boxplot(data = hcc_igg_levels_neg, aes(x = result_igg_denv_stfd, y = value_igg_denv_stfd, colour = "red1")) + 
  geom_boxplot(data = hcc_igg_levels_pos, aes(x = result_igg_denv_stfd, y = value_igg_denv_stfd, colour = "red1")) +
  labs(
    x = "Dengue Result",
    y = "Antibody Levels",
    colour = NULL
  ) +
  theme(plot.title = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.3)),
        axis.text.x = element_text(size = rel(1.25)),
        axis.text.y = element_text(size = rel(1.25)),
        legend.position = "none",
        panel.background = element_rect("gray96"),
        panel.grid.major.y = element_line(colour = "gray88")) + 
  scale_y_continuous(breaks = seq(0,2.5,0.5), limits = c(0,2.5)) + 
  scale_x_continuous(breaks = seq(0,1,1)) +
  ggtitle("IgG levels of Dengue Positive & Dengue Negative Children in HCC")
ggsave("hcc_boxplot_igg.jpeg")


