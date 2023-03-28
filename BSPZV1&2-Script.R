
###########################################  BSPZV2 DATASET  #############################################
##########################################################################################################

#################################### Making Hematology and Demography Data ###############################

################## Hematology Dataset
library(readxl)
mydata_hem = read_excel("BSPZV2_HEMATOLOGY.xlsx")
head(mydata_hem)
str(mydata_hem)

#omitting unneeded columns in hematology dataset
names(mydata_hem)
mydata_hem = mydata_hem[,c(1,2,4,32,33,34,35,42,43,44,45,52:55)]
names(mydata_hem)
mydata_hem = mydata_hem[,-c(5,7,9,11,13,15)]


#Extract only entries for group 1 (i.e adults only of G1A and G1B)
mydata_hem1 = mydata_hem[grepl("G1", mydata_hem$PATID), ]
length(unique(mydata_hem1$PATID))

#Arrange by PATID and date 
mydata_hem1 = with(mydata_hem1, mydata_hem1[order(PATID, ANALYSIS_D),])

#Subsetting observations corresponding to screening only
mydata_hem_baseline = subset(mydata_hem1, mydata_hem1$VISNO=="SCREENING")
str(mydata_hem_baseline)

#Create a coumn showing the difference in time between screening and first vaccination and between screening and CHMI??

#check for duplicates in hematology and remove them 
sum(duplicated(mydata_hem_baseline$PATID))

################### Demography Dataset
mydata_dem = read_excel("BSPZV2_DEM.xlsx")
head(mydata_dem)
str(mydata_dem)

#omitting unneeded columns in demography dataset
names(mydata_dem)
mydata_dem = mydata_dem[, c(1,3,4,10)]

#Extracting adults only
mydata_dem_adults = mydata_dem[grepl("G1", mydata_dem$PATID), ]
length(unique(mydata_dem_adults$PATID))
sum(duplicated(mydata_dem_adults$PATID))
str(mydata_dem_adults)

#merge 'demography with adults only' to 'hematology baseline' (keep paticipants with missing data)
mydata_dem_hem = merge(mydata_dem_adults, mydata_hem_baseline, by = c("PATID"), all.x = TRUE)
length(unique(mydata_dem_hem$PATID))


############################ Importing and making CHMI1 datasets ##########################################
library(readxl)
chmi= read_excel("bspzv2_qpcr_data_chmi1&2_july2018_updated.xlsx")
str(chmi)
sum(is.na(chmi$collection_date))
which(is.na(chmi$collection_date))

#Omit unneeded coumns
names(chmi)
chmi = chmi[,c(1,2,4,6:7,8,9,11,12)]

##Extract rows for chmi1 ony
chmi1 = chmi[chmi$chmi_type== "CH1",]

#make coumn for PATID in chmi dataset
#combine character strings of "study" and "subject" coumns to get column for PATID
library(tidyr)
chmi1 = unite(chmi1, PATID, 2:3, sep = "",  remove = FALSE)
#convert PATIDs to capital letters
chmi1$PATID = toupper(chmi1$PATID)

#Chec for dupicates and omit more unneeded coumns
sum(duplicated(chmi1$PATID))
names(chmi1)
chmi1 = chmi1[,-c(1,4)]

#Order by PATID and collection_date
chmi1 = with(chmi1, chmi1[order(PATID, collection_date),])
length(unique(chmi1$PATID))


###################### Merging Hematology, Demography and PCR Data into one Dataframe ########################

mydata_BSPZV2 = merge(mydata_dem_hem, chmi1, by = c("PATID"), all.x = TRUE)
sum(duplicated(mydata_BSPZV2$PATID))

#removing rows that miss both hematology and pcr information
mydata_BSPZV2 = mydata_BSPZV2[!is.na(mydata_BSPZV2$qpcr_density_reviewed),]
length(unique(mydata_BSPZV2$PATID))
sum(is.na(mydata_BSPZV2))
which(is.na(mydata_BSPZV2))

#modify some column names
names(mydata_BSPZV2)
names(mydata_BSPZV2)[2] = "enrldate"
names(mydata_BSPZV2)[3] = "sex"
names(mydata_BSPZV2)[4] = "age"
names(mydata_BSPZV2)[5] = "visno"
names(mydata_BSPZV2)[6] = "dat_hem"
names(mydata_BSPZV2)[14] = "dat_par"



###########################################  BSPZV1 DATASET  #############################################
##########################################################################################################

#################################### Making Hematology and Demography Data ###############################

################# Demography Dataset
library(readxl)
mydata2_dem = read_excel("BSPZV1_DEM.xlsx")
head(mydata2_dem)
str(mydata2_dem)

#modify demography column names
names(mydata2_dem)
names(mydata2_dem)[1] = "screen_no"
names(mydata2_dem)[2] = "Randomization_no"
names(mydata2_dem)[4] = "group"

################## Hematology Dataset
mydata2_hem = read_excel("BSPZV1_HEMATOLOGY.xlsx")
head(mydata2_hem)
str(mydata2_hem)

#omitting unneeded columns in hematology dataset
names(mydata2_hem)
mydata2_hem = mydata2_hem[, c(1,4,20,21)]

#modify column names
names(mydata2_hem)
names(mydata2_hem)[1] = "dat_hem"
names(mydata2_hem)[2] = "screen_no"
names(mydata2_hem)[3] = "lym"
names(mydata2_hem)[4] = "mono"
length(unique(mydata2_hem$screen_no))

## Extract rows whose screening number ends with "-34"
mydata2_hem1 = mydata2_hem[grep("-34$", mydata2_hem$screen_no), ]
length(unique(mydata2_hem1$screen_no))
sum(duplicated(mydata2_hem1$screen_no))
str(mydata2_hem1)

## Sort rows by screen no and analysis date, then remove duplicates
mydata2_hem1 = with(mydata2_hem1, mydata2_hem1[order(screen_no, dat_hem),])

## Remove rows corresponding to enroment date, remain with those for screening date
mydata2_hem1 = mydata2_hem1[!duplicated(mydata2_hem1$screen_no),]
str(mydata2_hem1)


# Merge demography to hematology (keep the 5 participants of group 5 without hematology info)
mydata2_dem_hem = merge(mydata2_dem, mydata2_hem1, by = "screen_no", all = TRUE)

# Omit screening numbers missing demography info
mydata2_dem_hem = mydata2_dem_hem[!is.na(mydata2_dem_hem$Age),]
length(unique(mydata2_dem_hem$PATID))

#Create a column for M/L ratio
mydata2_dem_hem$ML = NULL
mydata2_dem_hem$ML = mydata2_dem_hem$mono/ mydata2_dem_hem$lym
names(mydata2_dem_hem)
names(mydata2_dem_hem)[2] = "randomization_nr"
sum(duplicated(mydata2_dem_hem$PATID))

######## Inspect dataset
hist(sqrt(mydata2_dem_hem$ML))
hist(log(mydata2_dem_hem$ML))


################################### Making parasitology Dataset ########################################
#PCR Dataset
bspzv1_par = read_excel("bspzv1_qpcr_data_aug2016.xlsx", sheet = 2)
str(bspzv1_par)
length(unique(bspzv1_par$randomization_nr))

#Omit unneeded coumns
names(bspzv1_par)
bspzv1_par = bspzv1_par[,c(4:7,9:15)]
bspzv1_par$study = NULL
bspzv1_par$study = "BSPZV1"
bspzv1_par = with(bspzv1_par, bspzv1_par[order(randomization_nr, visit_bagamoyo),])

# Extract rows for chmi1 ony
bspzv1_par_1 = bspzv1_par[bspzv1_par$chmi_nr == "CHMI1",]
length(unique(bspzv1_par_1$randomization_nr))

## Modify coumn names and deete unneeded coumns
names(bspzv1_par_1)
bspzv1_par_1 = bspzv1_par_1[,c(1,4:12)]


################### Merge demography, hematoogy and parasitoogy  ###########################
mydata_BSPZV1 = merge(mydata2_dem_hem, bspzv1_par_1, by = "randomization_nr", all = TRUE)

names(mydata_BSPZV1)
names(mydata_BSPZV1)[5] = "age"
names(mydata_BSPZV1)[6] = "sex"
names(mydata_BSPZV1)[10] = "ml"
names(mydata_BSPZV1)[11] = "dat_par"
names(mydata_BSPZV1)[12] = "visit_id"
names(mydata_BSPZV1)[14] = "prepatent_p"
names(mydata_BSPZV1)[16] = "qpcr_density"
names(mydata_BSPZV1)[18] = "treatment"
names(mydata_BSPZV1)
mydata_BSPZV1 = mydata_BSPZV1[,c(3,5:16,18,19)]

## Do necessary modifications on BSPZV2
names(mydata_BSPZV2)
names(mydata_BSPZV2)[7] = "lym"
names(mydata_BSPZV2)[8] = "mono"
names(mydata_BSPZV2)[14] = "visit_id"
names(mydata_BSPZV2)[16] = "chmi_nr"
names(mydata_BSPZV2)[17] = "prepatent_p"
names(mydata_BSPZV2)[18] = "qpcr_final"
names(mydata_BSPZV2)[19] = "qpcr_density"
mydata_BSPZV2 = mydata_BSPZV2[,c(1,4,3,6,7,8,15,14,16,17,18,19,13,15)]
mydata_BSPZV2$ml = NULL
mydata_BSPZV2$ml = mydata_BSPZV2$mono/ mydata_BSPZV2$lym
mydata_BSPZV2 = mydata_BSPZV2[,c(1:6,15,13,7:12)]
mydata_BSPZV2$qpcr_final2 = NULL
mydata_BSPZV2$qpcr_final2[mydata_BSPZV2$qpcr_density == 0] = 0
mydata_BSPZV2$qpcr_final2[mydata_BSPZV2$qpcr_density != 0] = 1
names(mydata_BSPZV2)
mydata_BSPZV2 = mydata_BSPZV2[,c(1:7,9:12,13,14,15,8)]
mydata_BSPZV2 = mydata_BSPZV2[,-c(12)]
names(mydata_BSPZV2)[13] = "qpcr_final"
sum(duplicated(mydata_BSPZV2))

## Get coumn for "treatment" for BSPZV2
bspzv2_treat_allocat = read_excel("bspzv2_chmi_data_tobias.xlsx")
names(bspzv2_treat_allocat)
bspzv2_treat_allocat = unite(bspzv2_treat_allocat, PATID, 2:3, sep = "",  remove = FALSE)
bspzv2_treat_allocat$PATID = toupper(bspzv2_treat_allocat$PATID)
bspzv2_treat_allocat = bspzv2_treat_allocat[bspzv2_treat_allocat$chmi == 1,]
names(bspzv2_treat_allocat)
bspzv2_treat_allocat = bspzv2_treat_allocat[,c(2,5)]

bspzv2_treat_allocat$treatment = NULL
bspzv2_treat_allocat$treatment[bspzv2_treat_allocat$product == "pfspz"] = "vaccine"
bspzv2_treat_allocat$treatment[bspzv2_treat_allocat$product == "placebo"] = "placebo"
sum(duplicated(bspzv2_treat_allocat$PATID))

mydata_BSPZV2 = merge(mydata_BSPZV2, bspzv2_treat_allocat, by = "PATID", all = TRUE)
names(mydata_BSPZV2)
mydata_BSPZV2 = mydata_BSPZV2[,c(1:11,13,12,15,14)]
mydata_BSPZV2 = mydata_BSPZV2[!is.na(mydata_BSPZV2$qpcr_density),]
sum(is.na(mydata_BSPZV2))
which(is.na(mydata_BSPZV2))
names(mydata_BSPZV2)
names(mydata_BSPZV2)[8] = "dat_par"
names(mydata_BSPZV2)[14] = "treatment"

names(mydata_BSPZV1)
names(mydata_BSPZV2)

#### Order the dataframes by PATID and dat_par and append
mydata_BSPZV1 = with(mydata_BSPZV1, mydata_BSPZV1[order(PATID, dat_par),])
length(unique(mydata_BSPZV1$PATID))
mydata_BSPZV2 = with(mydata_BSPZV2, mydata_BSPZV2[order(PATID, dat_par),])
length(unique(mydata_BSPZV2$PATID))

####################### Append BSPZV1 and BSPZV2 Dataframes
BSPZV_all = rbind(mydata_BSPZV1, mydata_BSPZV2)
## Chec character cass and modify to date cass when appicabe
str(BSPZV_all)
BSPZV_all$qpcr_density = as.numeric(BSPZV_all$qpcr_density)
BSPZV_all$prepatent_p = as.numeric(BSPZV_all$prepatent_p)
BSPZV_all$qpcr_final = as.integer(BSPZV_all$qpcr_final)
BSPZV_all$treatment = factor(BSPZV_all$treatment)
BSPZV_all$dat_par = as.Date(BSPZV_all$dat_par)
BSPZV_all$dat_hem = as.Date(BSPZV_all$dat_hem)
## Order by PATID, dat_par and visit_id
BSPZV_all = with(BSPZV_all, BSPZV_all[order(PATID, dat_par, visit_id),])

## Change the coding for PCR density where NA == 0 parasites
BSPZV_all$qpcr_density[BSPZV_all$qpcr_final == 0] = 0

###### Create columns for log ML, qpcr_density and prepatent_p
BSPZV_all$logml = NULL
BSPZV_all$logml = log(BSPZV_all$ml)
BSPZV_all$sqrtqpcr_density = NULL
BSPZV_all$sqrtqpcr_density = sqrt(BSPZV_all$qpcr_density)
BSPZV_all$logprepatent_p = NULL
BSPZV_all$logprepatent_p = log(BSPZV_all$prepatent_p)

names(BSPZV_all)
BSPZV_all = BSPZV_all[,c(1:7,16,15,8:11,18,12,13,17,14)]
str(BSPZV_all)

## Inspect dataset
sum(is.na(BSPZV_all$PATID))
sum(is.na(BSPZV_all$ml))
na_ml = BSPZV_all[is.na(BSPZV_all$ml),]
length(unique(na_ml$PATID))
sum(is.na(BSPZV_all$qpcr_density))
which(is.na(BSPZV_all$qpcr_density))


################################## Create dataset with first PCR positivity, or latest diagnosis date if no PCR positivity
library(dplyr)
## Filter the rows with qpcr_final == 1, it will return every row where (qpcr_final == 1)
BSPZV_all_1 = BSPZV_all %>% group_by(PATID) %>% filter(qpcr_final == 1)

## with the grouped pids, filter the row where the date is minimum (earliest date)
BSPZV_all_1b = BSPZV_all_1 %>% group_by(PATID) %>% filter(dat_par==min(dat_par)) %>% filter(visit_id ==min(visit_id))
sum(duplicated(BSPZV_all_1b$PATID))

## create a vector of unique PATID that have (qpcr_final == 1)
a = BSPZV_all_1$PATID 

### First group all the PATIDs, then filter the PATIDs that are not contained in the (a), i.e the unique pids of the first step
## then filter again the most recent date (max_date in our case)
BSPZV_all_1c = BSPZV_all %>% group_by(PATID) %>% filter(! PATID %in% a) %>% 
  filter(dat_par==max(dat_par)) %>% filter(visit_id ==min(visit_id))

##Combine the two datasets using the rbind() 
BSPZV_all_1d = rbind(BSPZV_all_1b, BSPZV_all_1c)

##Inspect the new dataset and remove dupicates
length(unique(BSPZV_all_1d$PATID))
sum(duplicated(BSPZV_all_1d$PATID))
BSPZV_all_1d = BSPZV_all_1d[!is.na(BSPZV_all_1d$ml),]
### Therefore, rtss_all_new1d is the dataset with first positive pasmodium diagnosis or ast negative 
#### pasmodium diagnosis between start of foowup and start of booster dose 

BSPZV_all_1d$treatment[BSPZV_all_1d$treatment=="pfspz"] = "vaccine"
BSPZV_all_1d$treatment = factor(BSPZV_all_1d$treatment)
str(BSPZV_all_1d)

write.csv(BSPZV_all_1d,"BSPZV_dataset.csv")

############### XXXXXXXXX Demographic information based on the cross sectional dataset BSPZV_a_1d XXXXXXXXXXXX ################

hist(BSPZV_all_1d$prepatent_p)
hist(BSPZV_all_1d$logprepatent_p)
hist(BSPZV_all_1d$logml)
hist(BSPZV_all_1d$qpcr_density)
hist(BSPZV_all_1d$logqpcr_density)
plot(BSPZV_all_1d$logml ~ BSPZV_all_1d$logqpcr_density)
plot(BSPZV_all_1d$logml, BSPZV_all_1d$logqpcr_density)
plot(BSPZV_all_1d$logml ~ BSPZV_all_1d$logprepatent_p)
plot(BSPZV_all_1d$logml, BSPZV_all_1d$logprepatent_p)

########### Number of vaccinees and controls
table(BSPZV_all_1d$treatment)
hist(log(BSPZV_all_1d$qpcr_density))

########### Sex
BSPZV_all_1d$sex = factor(BSPZV_all_1d$sex)
table(BSPZV_all_1d$sex)
table1a = table(BSPZV_all_1d$sex)
prop.table(table1a)

summary(BSPZV_all_1d$sex[BSPZV_all_1d$treatment=="vaccine"])
table1b = table(BSPZV_all_1d$sex[BSPZV_all_1d$treatment=="vaccine"])
prop.table(table1b)

summary(BSPZV_all_1d$sex[BSPZV_all_1d$treatment=="placebo"])
table1c = table(BSPZV_all_1d$sex[BSPZV_all_1d$treatment=="placebo"])
prop.table(table1c)

########### Age
summary(BSPZV_all_1d$age)
summary(BSPZV_all_1d$age[BSPZV_all_1d$treatment=="vaccine"])
summary(BSPZV_all_1d$age[BSPZV_all_1d$treatment=="placebo"])

########### Monocytes
summary(BSPZV_all_1d$mono[BSPZV_all_1d$treatment=="vaccine"])
summary(BSPZV_all_1d$mono[BSPZV_all_1d$treatment=="placebo"])

########## Lymphocytes
summary(BSPZV_all_1d$lym[BSPZV_all_1d$treatment=="vaccine"])
summary(BSPZV_all_1d$lym[BSPZV_all_1d$treatment=="placebo"])

########### ML Ratios
summary(BSPZV_all_1d$ml)
summary(BSPZV_all_1d$ml[BSPZV_all_1d$treatment=="vaccine"])
summary(BSPZV_all_1d$ml[BSPZV_all_1d$treatment=="placebo"])
hist(log(BSPZV_all_1d$ml))

########### qPCR Diagnosis
BSPZV_all_1d$qpcr_final = factor(BSPZV_all_1d$qpcr_final)
table(BSPZV_all_1d$qpcr_final)
table1d = table(BSPZV_all_1d$qpcr_final)
prop.table(table1d)

table(BSPZV_all_1d$qpcr_final[BSPZV_all_1d$treatment=="vaccine"])
table1e = table(BSPZV_all_1d$qpcr_final[BSPZV_all_1d$treatment=="vaccine"])
prop.table(table1e)

table(BSPZV_all_1d$qpcr_final[BSPZV_all_1d$treatment=="placebo"])
table1f = table(BSPZV_all_1d$qpcr_final[BSPZV_all_1d$treatment=="placebo"])
prop.table(table1f)

########## Foowup time
summary(BSPZV_all_1d$prepatent_p)
summary(BSPZV_all_1d$prepatent_p[BSPZV_all_1d$treatment=="vaccine"])
summary(BSPZV_all_1d$prepatent_p[BSPZV_all_1d$treatment=="placebo"])

########## Parasite density
summary(BSPZV_all_1d$qpcr_density)
summary(BSPZV_all_1d$qpcr_density[BSPZV_all_1d$treatment=="vaccine"])
summary(BSPZV_all_1d$qpcr_density[BSPZV_all_1d$treatment=="placebo"])

########## Parasite prepatent period
summary(BSPZV_all_1d$prepatent_p)
summary(BSPZV_all_1d$prepatent_p[BSPZV_all_1d$treatment=="vaccine"])
summary(BSPZV_all_1d$prepatent_p[BSPZV_all_1d$treatment=="placebo"])





################ XXXXXXXX Bivariate Analyses M vs. parasite density XXXXXXXX ###########
### Correlation of ML with parasite density at time of diagnosis
##Spearman rank
str(BSPZV_all_1d)
ml_density = cor.test(BSPZV_all_1d$logml, BSPZV_all_1d$sqrtqpcr_density, method = "spearman", exact = FALSE)
ml_density
plot(BSPZV_all_1d$logml ~ BSPZV_all_1d$sqrtqpcr_density)

############################# Cox Regression for correlation of ML with prepatent period

################### Unadjusted Cox Model  ###############
str(BSPZV_all_1d)
BSPZV_all_1d$sex = factor(BSPZV_all_1d$sex)

library(survival)
ml_corrl = coxph(formula = Surv(logprepatent_p, qpcr_final) ~ logml, data = BSPZV_all_1d)
summary(ml_corrl)  #output provides HR CIs
exp(confint(ml_corrl))  #Also HR CIs

hist(log(BSPZV_all_1d$age))
age_corrl = coxph(formula = Surv(logprepatent_p, qpcr_final) ~ log(age), data = BSPZV_all_1d)
summary(age_corrl)  #output provides HR CIs
exp(confint(age_corrl))  #Also HR CIs

crude_model = coxph(formula = Surv(logprepatent_p, qpcr_final) ~ treatment*logml, data = BSPZV_all_1d)
summary(crude_model)  #output provides HR CIs
exp(confint(crude_model))  #Also HR CIs

################## Model with ML only
crude_ml = coxph(formula = Surv(prepatent_p, qpcr_final) ~ logml, data = BSPZV_all_1d)
summary(crude_ml)  #output provides HR CIs
exp(confint(crude_ml))  #Also HR CIs

crude_ml2 = coxph(formula = Surv(logprepatent_p, qpcr_final) ~ logml, data = BSPZV_all_1d)
summary(crude_ml2)  #output provides HR CIs
exp(confint(crude_ml2))  #Also HR CIs

################## Adjusted Model 
adjusted_model = coxph(formula = Surv(prepatent_p, qpcr_final) ~ treatment + logml + age + study, data = BSPZV_all_1d)
summary(adjusted_model)  #output provides HR CIs
exp(confint(adjusted_model))  #Also HR CIs

################## Adjusted Model with Interaction
adjusted_interaction = coxph(formula = Surv(prepatent_p, qpcr_final) ~ treatment*logml + age + study, data = BSPZV_all_1d)
summary(adjusted_interaction)  #output provides HR CIs
exp(confint(adjusted_interaction))  #Also HR CIs




################################# PMR ##############################

#Count number of observations for each PATID
table(BSPZV_all$PATID)
    #Remove those with less than 6 observations and those never diagnosed with malaria over followup
library(dplyr)
BSPZV_pmr = BSPZV_all %>% group_by(PATID) %>% filter(n()>= 6)   #This code removes those with less than 6 observations
length(unique(BSPZV_all$PATID))
length(unique(BSPZV_pmr$PATID))

library(data.table)
setDT(BSPZV_pmr)     #convert dataframe to data.table in order to use the following command
BSPZV_pmr2 = BSPZV_pmr[,.SD[any(qpcr_density!=0)], by=PATID]    #This code removes those with only malaria negative throughout followup
length(unique(BSPZV_pmr2$PATID))

###Add column for qpcr_density for parasites per milliliter instead of parasites per microliter
BSPZV_pmr2$qpcr_density_milli = NULL
BSPZV_pmr2$qpcr_density_milli = (BSPZV_pmr2$qpcr_density * 1000)
###Replace zeroes in qpcr_density_milli with 10 parasites per milliliter
BSPZV_pmr2$qpcr_density_milli[BSPZV_pmr2$qpcr_density_milli<=0] = 10
table(BSPZV_pmr2$qpcr_density_milli>10 & BSPZV_pmr2$qpcr_density_milli<=20)
which(BSPZV_pmr2$qpcr_density_milli>10 & BSPZV_pmr2$qpcr_density_milli<=20)
###Add columns for log10 transformations of qpcr_density and prepatent_p
BSPZV_pmr2$log10qpcr_density_milli = NULL
BSPZV_pmr2$log10qpcr_density_milli = log10(BSPZV_pmr2$qpcr_density_milli)
BSPZV_pmr2$log10prepatent_p = NULL
BSPZV_pmr2$log10prepatent_p = log10(BSPZV_pmr2$prepatent_p)

##remove all NAs
sum(is.na(BSPZV_pmr2$log10qpcr_density_milli))
which(is.na(BSPZV_pmr2$log10qpcr_density_milli))
BSPZV_pmr2 = BSPZV_pmr2[-c(517, 533),]
sum(is.na(BSPZV_pmr2$log10prepatent_p))
which(is.na(BSPZV_pmr2$log10prepatent_p))
BSPZV_pmr2 = BSPZV_pmr2[!is.na(BSPZV_pmr2$log10prepatent_p),]


################################# run linear model on log10 transformed data to get parasite growth rates
library(reshape)

library(tidyverse)
library(dplyr)
library(broom)

linear_model = BSPZV_pmr2 %>% 
  group_by(PATID) %>% 
  do(tidy(lm(log10qpcr_density_milli ~ log10prepatent_p, .))) 
linear_model=linear_model%>% filter(term=='log10prepatent_p') %>%
  as.data.frame()

############ Merge BSPZV dataset with growth rate estimates from linear_model
BSPZV_pmr3 = merge(BSPZV_all_1d, linear_model, by = "PATID")
names(BSPZV_pmr3)
BSPZV_pmr3 = BSPZV_pmr3[,c(1:19,22)]
names(BSPZV_pmr3)
names(BSPZV_pmr3)[20] = "PMR"
BSPZV_pmr3 = BSPZV_pmr3[,c(1:18,20)]

str(BSPZV_pmr3)
as.numeric(BSPZV_pmr3$PMR)

hist(BSPZV_pmr3$PMR)
summary(BSPZV_pmr3$PMR[BSPZV_pmr3$treatment=="vaccine"])
summary(BSPZV_pmr3$PMR[BSPZV_pmr3$treatment=="placebo"])

############ Do a correlation test between PMR and logml
ml_pmr = cor.test(BSPZV_pmr3$PMR, BSPZV_pmr3$logml, method = "spearman", exact = FALSE)
ml_pmr

write.csv(BSPZV_all_1d, "BSPZV_dataset.csv")


#########################


























