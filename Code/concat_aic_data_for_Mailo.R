rm(list=ls()) #remove previous variable assignments

# source redcap data -------------------------------------------------------
source("Codes/REDCap_extract_case_data.R")

# format cohort  
R01_lab_results$id_cohort<-substr(R01_lab_results$person_id, 2, 2) #F and M are AIC, 0 C and D are other
R01_lab_results <- within(R01_lab_results, id_cohort[R01_lab_results$id_cohort=="M"] <- "F")
# R01_lab_results <- subset(R01_lab_results, id_cohort == "F")

# Create a new variable by studyID for study site
R01_lab_results$id_city<-substr(R01_lab_results$person_id, 1, 1) #C is Chulaimbo, K is Kisumu, M is Msambweni, U is Ukunda, one 0 not sure, R is also Chulaimbo, G stands for Nganja (one of the subparts of Msambweni), L is for Mililani (part of Msambweni)
R01_lab_results$id_site<-NA
R01_lab_results <- within (R01_lab_results, id_site[R01_lab_results$id_city=="C" | R01_lab_results$id_city=="R"] <- "Chulaimbo")
R01_lab_results <- within (R01_lab_results, id_site[R01_lab_results$id_city=="K"] <- "Kisumu")
R01_lab_results <- within (R01_lab_results, id_site[R01_lab_results$id_city=="M" | R01_lab_results$id_city=="G" | R01_lab_results$id_city=="L"] <- "Msambweni")
R01_lab_results <- within (R01_lab_results, id_site[R01_lab_results$id_city=="U" ] <- "Ukunda")

# Remove the one studyID starting with a O
R01_lab_results <- R01_lab_results[which(R01_lab_results$id_city!="O"), ]

# interview dates ---------------------------------------------------------
R01_lab_results$int_date <- NA
R01_lab_results$int_date <- ifelse(!is.na(R01_lab_results$interview_date_aic)==TRUE, R01_lab_results$interview_date_aic, R01_lab_results$interview_date)
R01_lab_results$int_date <- ifelse(R01_lab_results$int_date == "1900-01-01", NA, R01_lab_results$int_date)
R01_lab_results$int_date <- as.Date(R01_lab_results$int_date, "%Y-%m-%d")

# age group ---------------------------------------------------------------
R01_lab_results$age = R01_lab_results$age_calc_rc  # your new merged column starts with age_calc_rc
R01_lab_results$age[!is.na(R01_lab_results$aic_calculated_age)] = R01_lab_results$aic_calculated_age[!is.na(R01_lab_results$aic_calculated_age)]  # merge with aic_calculated_age
R01_lab_results$age[!is.na(R01_lab_results$age_calc)] = R01_lab_results$age_calc[!is.na(R01_lab_results$age_calc)]  # merge with age_calc
R01_lab_results$age<-round(R01_lab_results$age)

R01_lab_results$age_group<-NA
R01_lab_results <- within(R01_lab_results, age_group[age<=2] <- "under 2")
R01_lab_results <- within(R01_lab_results, age_group[age>2 & age<=5] <- "2-5")
R01_lab_results <- within(R01_lab_results, age_group[age>5 & age<=10] <- "6-10")
R01_lab_results <- within(R01_lab_results, age_group[age>10 & age<=15] <- "11-15")
R01_lab_results <- within(R01_lab_results, age_group[age>15] <- "over 15")
R01_lab_results$age_group <- factor(R01_lab_results$age_group, levels = c("under 2", "2-5", "6-10", "11-15", "over 15"))

# reshape testing vars -----------------------------------------------------------------
tested<-R01_lab_results[, grepl("person_id|redcap_event|tested_", names(R01_lab_results))]
tested<-tested[, !grepl("date", names(tested))]
tested<-tested[which(tested$redcap_event_name=="patient_informatio_arm_1"),]
colnames(tested)<-sub("tested_*","", colnames(tested))    
colnames(tested)<-sub("*_2","", colnames(tested))    

#order
tested<-tested[,order(colnames(tested))]

nameVec <- names(tested)
v.names=c('chikv_kenya_igg', 'chikv_stfd_igg', 'denv_kenya_igg', 'denv_stfd_igg')
times = c("ab_", "bc_", "cd_", "de_", "ef_", "fg_", "gh_")    
varyingIndex <- which(grepl("ab_|bc_|cd_|de_|ef_|fg_|gh_", names(tested)))
tested_long<-reshape(tested, idvar = "person_id", varying = varyingIndex,  direction = "long", timevar = "visit", times=times, v.names=v.names)

tested_long <- within(tested_long, visit[visit=="ab_"] <- "visit_a_arm_1")
tested_long <- within(tested_long, visit[visit=="bc_"] <- "visit_b_arm_1")
tested_long <- within(tested_long, visit[visit=="cd_"] <- "visit_c_arm_1")
tested_long <- within(tested_long, visit[visit=="de_"] <- "visit_d_arm_1")
tested_long <- within(tested_long, visit[visit=="ef_"] <- "visit_e_arm_1")
tested_long <- within(tested_long, visit[visit=="fg_"] <- "visit_f_arm_1")
tested_long <- within(tested_long, visit[visit=="gh_"] <- "visit_g_arm_1")
tested_long$redcap_event_name<-tested_long$visit
names(tested_long)[names(tested_long) == 'denv_kenya_igg'] <- 'tested_denv_kenya_igg'
names(tested_long)[names(tested_long) == 'chikv_kenya_igg'] <- 'tested_chikv_kenya_igg'
names(tested_long)[names(tested_long) == 'denv_stfd_igg'] <- 'tested_denv_stfd_igg'
names(tested_long)[names(tested_long) == 'chikv_stfd_igg'] <- 'tested_chikv_stfd_igg'

# reshape sereoconverter vars -----------------------------------------------------------------
seroconverter<-R01_lab_results[, grepl("person_id|redcap_event|ab_|bc_|cd_|de_|ef_|fg_|gh_", names(R01_lab_results))]
seroconverter<-seroconverter[, !grepl("malaria|tested|freezer|rack|sample|fail|run", names(seroconverter))]
seroconverter<-seroconverter[which(seroconverter$redcap_event_name=="patient_informatio_arm_1"),]

#order
seroconverter<-seroconverter[,order(colnames(seroconverter))]

nameVec <- names(seroconverter)
v.names=c('chikv_kenya_igg', 'chikv_stfd_igg', 'denv_kenya_igg', 'denv_stfd_igg')
times = c("ab_", "bc_", "cd_", "de_", "ef_", "fg_", "gh_")    
varyingIndex <- which(grepl("ab_|bc_|cd_|de_|ef_|fg_|gh_", names(seroconverter)))
seroconverter_long<-reshape(seroconverter, idvar = "person_id", varying = varyingIndex, direction = "long", timevar = "visit", times=times, v.names=v.names)

seroconverter_long <- within(seroconverter_long, visit[visit=="ab_"] <- "visit_a_arm_1")
seroconverter_long <- within(seroconverter_long, visit[visit=="bc_"] <- "visit_b_arm_1")
seroconverter_long <- within(seroconverter_long, visit[visit=="cd_"] <- "visit_c_arm_1")
seroconverter_long <- within(seroconverter_long, visit[visit=="de_"] <- "visit_d_arm_1")
seroconverter_long <- within(seroconverter_long, visit[visit=="ef_"] <- "visit_e_arm_1")
seroconverter_long <- within(seroconverter_long, visit[visit=="fg_"] <- "visit_f_arm_1")
seroconverter_long <- within(seroconverter_long, visit[visit=="gh_"] <- "visit_g_arm_1")
seroconverter_long$redcap_event_name<-seroconverter_long$visit
names(seroconverter_long)[names(seroconverter_long) == 'denv_kenya_igg'] <- 'seroc_denv_kenya_igg'
names(seroconverter_long)[names(seroconverter_long) == 'chikv_kenya_igg'] <- 'seroc_chikv_kenya_igg'
names(seroconverter_long)[names(seroconverter_long) == 'denv_stfd_igg'] <- 'seroc_denv_stfd_igg'
names(seroconverter_long)[names(seroconverter_long) == 'chikv_stfd_igg'] <- 'seroc_chikv_stfd_igg'

# merging the created data sets back to main -----------------------------------------------------------------
R01_lab_results <- merge(seroconverter_long, R01_lab_results,  by=c("person_id", "redcap_event_name"), all = TRUE)  #merge symptoms to redcap data
R01_lab_results <- merge(tested_long, R01_lab_results,  by=c("person_id", "redcap_event_name"), all = TRUE)  #merge tested samples 

# PCR -----------------------------------------------------------------
# prnt
R01_lab_results$prnt_80_denv <- gsub("<10", "1", R01_lab_results$prnt_80_denv)
R01_lab_results$prnt_80_denv<-as.numeric(as.character(R01_lab_results$prnt_80_denv))

R01_lab_results$prnt_result_denv<-NA
R01_lab_results <- within(R01_lab_results, prnt_result_denv[R01_lab_results$prnt_80_denv <10] <- 0)
R01_lab_results <- within(R01_lab_results, prnt_result_denv[R01_lab_results$prnt_80_denv >=10] <- 1)

R01_lab_results$prnt_80_chikv <- gsub("<10", "1", R01_lab_results$prnt_80_chikv)
R01_lab_results$prnt_80_chikv<-as.numeric(as.character(R01_lab_results$prnt_80_chikv))

R01_lab_results$prnt_result_chikv<-NA
R01_lab_results <- within(R01_lab_results, prnt_result_chikv[R01_lab_results$prnt_80_chikv <10] <- 0)
R01_lab_results <- within(R01_lab_results, prnt_result_chikv[R01_lab_results$prnt_80_chikv >=10] <- 1)

# ufi2
R01_lab_results$ufi2_result_denv<-NA
R01_lab_results <- within(R01_lab_results, ufi2_result_denv[R01_lab_results$denv_ct_ufi2 >45] <- 0)
R01_lab_results <- within(R01_lab_results, ufi2_result_denv[R01_lab_results$denv_ct_ufi2 <=45] <- 1)

R01_lab_results$ufi2_result_chikv<-NA
R01_lab_results <- within(R01_lab_results, ufi2_result_chikv[R01_lab_results$chikv_ct_ufi2 >45] <- 0)
R01_lab_results <- within(R01_lab_results, ufi2_result_chikv[R01_lab_results$chikv_ct_ufi2 <=45] <- 1)

#stfd denv igg seroconverters or PCR positives as infected. 
R01_lab_results$infected_denv_stfd[R01_lab_results$tested_denv_stfd_igg ==1 |R01_lab_results$result_pcr_denv_kenya==0|R01_lab_results$result_pcr_denv_stfd==0|R01_lab_results$denv_result_ufi==0|R01_lab_results$prnt_result_denv==0|R01_lab_results$ufi2_result_denv==0]<-0
R01_lab_results$infected_denv_stfd[R01_lab_results$seroc_denv_stfd_igg==1|R01_lab_results$result_pcr_denv_kenya==1|R01_lab_results$result_pcr_denv_stfd==1|R01_lab_results$denv_result_ufi==1|R01_lab_results$prnt_result_denv==1|R01_lab_results$ufi2_result_denv==1]<-1
# table(R01_lab_results$infected_denv_stfd)

#stfd chikv igg seroconverters or PCR positives as infected. or PNRT +
R01_lab_results$infected_chikv_stfd[R01_lab_results$tested_chikv_stfd_igg ==1 |R01_lab_results$result_pcr_chikv_kenya==0|R01_lab_results$chikv_result_ufi==0|R01_lab_results$prnt_result_chikv==0|R01_lab_results$ufi2_result_chikv==0]<-0
R01_lab_results$infected_chikv_stfd[R01_lab_results$seroc_chikv_stfd_igg==1|R01_lab_results$result_pcr_chikv_kenya==1|R01_lab_results$chikv_result_ufi==1|R01_lab_results$prnt_result_chikv==1|R01_lab_results$ufi2_result_chikv==1]<-1
# table(R01_lab_results$infected_chikv_stfd)

aic.tsi.foi <- R01_lab_results[,c("person_id", "id_site", "redcap_event_name", "int_date"
                                  , "date_symptom_onset", "age", "gender_aic", "age_group"
                                  , "occupation_aic", "village_aic", "child_travel", "where_travel_aic"
                                  , "stay_overnight_aic", "infected_denv_stfd", "infected_chikv_stfd"
                                  )]

aic.tsi.foi <- aic.tsi.foi[!grepl("2|patient", aic.tsi.foi$redcap_event_name),]
write.csv(aic.tsi.foi, "C:/Users/Jamie/Box Sync/FOI Kenya Project/R01_hcc_aic.csv", row.names = F)




