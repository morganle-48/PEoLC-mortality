
library(tidyverse)
library(clipr)
library(lmtest)
library(lme4)
library(biglm)

database_extract<-read.csv("MPIFixedCause.csv")

database_extract <- database_extract |>
  mutate(cod = 
           case_when ( grepl('^A0', s_underlying_cod_icd10)  ~ 'Sudden Death',
                       grepl('^A39', s_underlying_cod_icd10) ~ 'Sudden Death',
                       grepl('^A4[01]', s_underlying_cod_icd10) ~ 'Sudden Death',
                       grepl('^B2[0-4]', s_underlying_cod_icd10) ~ 'Other Terminal Illness',
                       grepl('^D[0-3]', s_underlying_cod_icd10) ~ 'Cancer',
                       grepl('^D4[0-8]', s_underlying_cod_icd10) ~ 'Cancer',
                       grepl('^F0[13]', s_underlying_cod_icd10) ~ 'Frailty',
                       grepl('^G0[0-3]', s_underlying_cod_icd10) ~ 'Sudden Death',
                       grepl('^G30', s_underlying_cod_icd10) ~ 'Frailty',
                       grepl('^H[0-5]', s_underlying_cod_icd10) ~ 'Sudden Death',
                       grepl('^H[6-9]', s_underlying_cod_icd10) ~ 'Sudden Death',
                       grepl('^I2[12]', s_underlying_cod_icd10) ~ 'Sudden Death',
                       grepl('^I63', s_underlying_cod_icd10) ~ 'Sudden Death',
                       grepl('^I64', s_underlying_cod_icd10) ~ 'Sudden Death',
                       grepl('^I6[0-2]', s_underlying_cod_icd10) ~ 'Sudden Death',
                       grepl('^I71', s_underlying_cod_icd10) ~ 'Sudden Death',
                       grepl('^J1[2-8]', s_underlying_cod_icd10) ~ 'Sudden Death',
                       grepl('^K2[5-7]', s_underlying_cod_icd10) ~ 'Sudden Death',
                       grepl('^K4[0-6]', s_underlying_cod_icd10) ~ 'Sudden Death',
                       grepl('^K57', s_underlying_cod_icd10) ~ 'Organ Failure',
                       grepl('^K7[0-6]', s_underlying_cod_icd10) ~ 'Organ Failure',
                       grepl('^L', s_underlying_cod_icd10) ~ 'Organ Failure',
                       grepl('^O', s_underlying_cod_icd10) ~ 'Sudden Death',
                       grepl('^P', s_underlying_cod_icd10) ~ 'Sudden Death',
                       grepl('^Q[2-8]', s_underlying_cod_icd10) ~ 'Organ Failure',
                       grepl('^R54', s_underlying_cod_icd10) ~ 'Frailty',
                       grepl('^R', s_underlying_cod_icd10) ~ 'Sudden Death',
                       grepl('^R99', s_underlying_cod_icd10) ~ 'Sudden Death',
                       grepl('^U509', s_underlying_cod_icd10) ~ 'Sudden Death',
                       grepl('^W6[5-9]', s_underlying_cod_icd10) ~ 'Sudden Death',
                       grepl('^W7[0-4]', s_underlying_cod_icd10) ~ 'Sudden Death',
                       grepl('^W[01]', s_underlying_cod_icd10) ~ 'Sudden Death',
                       grepl('^X0', s_underlying_cod_icd10) ~ 'Sudden Death',
                       grepl('^X41', s_underlying_cod_icd10) ~ 'Sudden Death',
                       grepl('^X42', s_underlying_cod_icd10) ~ 'Sudden Death',
                       grepl('^X44', s_underlying_cod_icd10) ~ 'Sudden Death',
                       grepl('^X59', s_underlying_cod_icd10) ~ 'Sudden Death',
                       grepl('^X8[0-4]', s_underlying_cod_icd10) ~ 'Sudden Death',
                       grepl('^X8[5-9]', s_underlying_cod_icd10) ~ 'Sudden Death',
                       grepl('^X9', s_underlying_cod_icd10) ~ 'Sudden Death',
                       grepl('^X[67]', s_underlying_cod_icd10) ~ 'Sudden Death',
                       grepl('^Y0', s_underlying_cod_icd10) ~ 'Sudden Death',
                       grepl('^Y3[0-4]', s_underlying_cod_icd10) ~ 'Sudden Death',
                       grepl('^Y[12]', s_underlying_cod_icd10) ~ 'Sudden Death',
                       grepl('^A', s_underlying_cod_icd10) ~ 'Organ Failure',
                       grepl('^B', s_underlying_cod_icd10) ~ 'Organ Failure',
                       grepl('^C', s_underlying_cod_icd10) ~ 'Cancer',
                       grepl('^D[5-8]', s_underlying_cod_icd10) ~ 'Organ Failure',
                       grepl('^E', s_underlying_cod_icd10) ~ 'Other Terminal Illness',
                       grepl('^F', s_underlying_cod_icd10) ~ 'Other Terminal Illness',
                       grepl('^G', s_underlying_cod_icd10) ~ 'Other Terminal Illness',
                       grepl('^I', s_underlying_cod_icd10) ~ 'Organ Failure',
                       grepl('^J', s_underlying_cod_icd10) ~ 'Organ Failure',
                       grepl('^K', s_underlying_cod_icd10) ~ 'Other Terminal Illness',
                       grepl('^M', s_underlying_cod_icd10) ~ 'Frailty',
                       grepl('^N', s_underlying_cod_icd10) ~ 'Organ Failure',
                       grepl('^Q', s_underlying_cod_icd10) ~ 'Other Terminal Illness',
                       grepl('^V', s_underlying_cod_icd10) ~ 'Organ Failure',
                       grepl('^W', s_underlying_cod_icd10) ~ 'Organ Failure',
                       grepl('^X', s_underlying_cod_icd10) ~ 'Organ Failure',
                       grepl('^Y', s_underlying_cod_icd10) ~ 'Organ Failure',
                       grepl('^U0',s_underlying_cod_icd10) ~ 'Organ Failure',
                       TRUE ~ NA_character_
           ))

for(i in dim(database_extract)[1] ){
  if (database_extract$der_age_at_death[i] %in% 65:74 & runif(1,0,1) > 0.9 ){
    database_extract$cod[i]<-'Frailty'
  }
  if (database_extract$der_age_at_death[i] %in% 75:84 & runif(1,0,1) > 0.7 ){
    database_extract$cod[i]<-'Frailty'
  }
  if (database_extract$der_age_at_death[i] %in% 85:120 & runif(1,0,1) > 0.2 ){
    database_extract$cod[i]<-'Frailty'
  }
}

write_csv(database_extract, "MPIFixedCause.csv")


# Copied from SQL ---------------------------------------------------------

MPI <- database_extract %>%
  arrange(der_pseudo_nhs_number) %>%
  mutate(SUPatID = row_number())

MPI<- MPI |>
  mutate(LocationType = 
           case_when(  pod_code == 'H' ~ 'Home',
                       pod_code == 'E' ~ 'Elsewhere/Other',
  pod_nhs_establishment == 1 & pod_establishment_type %in% c('02', '04', '07', '10', '21', '2', '4', '7') ~ 'Care Home',
  pod_nhs_establishment == 1 & pod_establishment_type %in% c('01', '03', '18', '99', '1', '3') ~ 'Hospital',
  pod_nhs_establishment == 2 & pod_establishment_type %in% c('03', '04', '07', '10', '14', '20', '22', '32', '33', '99', '3', '4', '7') ~ 'Care Home',
  pod_nhs_establishment == 2 & pod_establishment_type %in% c('01', '18', '19', '1') ~ 'Hospital',
  pod_establishment_type == '83' ~ 'Hospice',
  pod_nhs_establishment == 1 & pod_establishment_type %in% c('5', '6', '8', '9', '11', '05', '06', '08', '09') ~ 'Elsewhere/Other',
  pod_nhs_establishment == 2 & (pod_establishment_type %in% c('5', '8', '9', '11', '12', '13', '15', '16', '17', '05', '08', '09') | 
                                  (pod_establishment_type >= '23' & pod_establishment_type <= '31') | 
                                  (pod_establishment_type >= '34' & pod_establishment_type <= '82')) ~ 'Elsewhere/Other',
  TRUE ~ 'Unknown'
))

MPI<- MPI |>
  mutate(ccg_responsible = 
           case_when( 
             der_nhais_ccg_of_registration != "" ~ der_nhais_ccg_of_registration,
             (der_nhais_ccg_of_registration == "" & ccg_of_registration_code != "") ~ ccg_of_registration_code,
             TRUE ~ 'Unknown'
           )
         )

## Duplication
## in data to this point there are 544 duplicate records

# removing duplicates
MPI<- MPI[ -which(duplicated(MPI$der_pseudo_nhs_number)==TRUE), ]

write_csv(MPI, "MPI.csv")
