library(shiny)
library(shinydashboard)
library(magrittr)
library(readr)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(reshape2)
library(epitools)
library(survival) 
library(stringr)
library(gridExtra)
library(grid)
library(extrafont)
library(cowplot)
source('Result/HelperFunction.R')


options(scipen = 999)

IR_all_orig <-  readr::read_csv('csv/aesi_ir_raw_27april.csv', col_types = readr::cols())

meta_result0 <- readr::read_csv('csv/meta_result0_27April.csv', col_types = readr::cols())

## subgroup ref 
subgroup_ref <- IR_all_orig %>% dplyr::select(subgroupCohortDefinitionId, subgroupName) %>% 
  dplyr::distinct() %>%
  dplyr::mutate(sex_group = dplyr::case_when(grepl('Male',subgroupName) ~ 'Male',
                             grepl('Female',subgroupName) ~'Female',
                             TRUE ~ 'NA'),
         age_lb = suppressWarnings(as.numeric(sub("\\D*(\\d+).*", "\\1",subgroupName))),
         age_ub = suppressWarnings(as.numeric(sub(".*\\b(\\d+).*", "\\1", subgroupName))),
         age_group = paste(age_lb, "-", age_ub)
  ) %>% dplyr::arrange(age_lb) %>%
  dplyr::group_by(subgroupCohortDefinitionId) %>% slice(1) %>% dplyr::ungroup()

subgroup_ref$age_group = ifelse(subgroup_ref$subgroupCohortDefinitionId %in% c(1,2),'NA',subgroup_ref$age_group  )
subgroup_ref$age_group = ifelse(subgroup_ref$subgroupCohortDefinitionId == 0,'All',subgroup_ref$age_group  )
subgroup_ref$age_group= factor(subgroup_ref$age_group, levels=c(   "0 - 5"   , "6 - 17" ,  "18 - 34" , "35 - 54",  "55 - 64" , "65 - 74", "75 - 84" , "85 - 114","All","NA"))

## outcome event ref 
outcome_ref <- IR_all_orig %>% dplyr::select(outcomeId, outcomeName,cleanWindow) %>% 
  distinct_all() %>% arrange(outcomeName) %>% 
  mutate(outcome_main_name = case_when( grepl( 'myocardial',tolower(outcomeName)) ~ 'Acute myocardial infarction',
                                        grepl( 'anaphylaxis',tolower(outcomeName)) ~ 'Anaphylaxis',
                                        grepl( 'appendicitis',tolower(outcomeName)) ~ 'Appendicitis',
                                        grepl( 'bells',tolower(outcomeName)) ~ 'Bells palsy',
                                        grepl( 'thrombosis',tolower(outcomeName)) ~ 'Deep vein thrombosis',
                                        grepl( 'intravascular',tolower(outcomeName)) ~ 'Disseminated intravascular coagulation',
                                        grepl( 'encephalomyelitis',tolower(outcomeName)) ~ 'Encephalomyelitis',
                                        grepl( 'seizures',tolower(outcomeName)) ~ 'Seizures',
                                        grepl( 'barre',tolower(outcomeName)) ~ 'Guillain-Barre syndrome',
                                        grepl( 'hemorrhagic stroke',tolower(outcomeName)) & grepl( 'non',tolower(outcomeName))==F  ~ 'Hemorrhagic stroke',
                                        grepl( 'thrombocytopenia',tolower(outcomeName)) ~ 'Immune thrombocytopenia',
                                        grepl( 'kawasaki',tolower(outcomeName)) ~ 'Kawasaki disease',
                                        grepl( 'myocarditis',tolower(outcomeName)) ~ 'Myocarditis pericarditis',
                                        grepl( 'narcolepsy',tolower(outcomeName)) ~ 'Narcolepsy',
                                        grepl( 'non-hemorrhagic',tolower(outcomeName)) ~ 'Non-hemorrhagic stroke',
                                        grepl( 'pulmonary',tolower(outcomeName)) ~ 'Pulmonary embolism',
                                        grepl( 'narcolepsy',tolower(outcomeName)) ~ 'Seizures',
                                        grepl( 'transverse',tolower(outcomeName)) ~ 'Transverse myelitis'
  )
  ) %>% mutate(outcome_group = case_when( outcome_main_name %in% c( "Acute myocardial infarction" , "Non-hemorrhagic stroke","Pulmonary embolism","Deep vein thrombosis","Hemorrhagic stroke",
                                                                    "Seizures" ) ~ "Common",   #Seizures  -COMMON
                                          outcome_main_name %in% c("Appendicitis" ,"Anaphylaxis", "Immune thrombocytopenia","Myocarditis pericarditis" ,
                                                                   "Kawasaki disease" ,"Bells palsy" ) ~ "Rare",  ## Kawasaki -RARE
                                          outcome_main_name %in% c("Disseminated intravascular coagulation", "Encephalomyelitis",
                                                                   "Guillain-Barre syndrome" ,"Transverse myelitis","Narcolepsy" ) ~  "Very rare" 
  )
  )%>% 
  mutate(outcome_include = if_else(outcomeId %in% c(312,303, 323,307, 332, 321, 301, 302, 308, 305, 324, 326, 314, 319, 310, 316,318),1,0),
         ip_require = if_else(grepl( 'ip',tolower(outcomeName)),1,0) ) 

outcome_ref$outcome_main_name <-  factor(outcome_ref$outcome_main_name,
                                         levels=c( "Non-hemorrhagic stroke","Acute myocardial infarction" ,"Deep vein thrombosis","Hemorrhagic stroke","Pulmonary embolism",
                                                   "Seizures", "Appendicitis"  ,"Bells palsy","Anaphylaxis", "Immune thrombocytopenia","Myocarditis pericarditis" ,
                                                   "Kawasaki disease"  ,"Disseminated intravascular coagulation", "Encephalomyelitis","Narcolepsy",
                                                   "Guillain-Barre syndrome" ,"Transverse myelitis" ))

## study design ref 
method_ref <- IR_all_orig %>%
  dplyr::select(
    targetCohortDefinitionId,
    targetName,
    timeAtRiskId,
    timeAtRiskStartOffset,
    timeAtRiskStartIndex,
    timeAtRiskEndOffset,
    timeAtRiskEndIndex
  ) %>%
  distinct_all()

## database ref
database_ref <-  IR_all_orig %>%  dplyr::select(databaseName) %>% distinct() %>%
  mutate(databaseNameU = toupper(databaseName),
         db_name = (
           case_when (
             grepl('AUSTRALIA', databaseNameU) ~ 'IQVIA_AUSTRALIA',
             grepl('FRANCE', databaseNameU) ~ 'IQVIA_FRANCE',
             grepl('GERMANY', databaseNameU) ~ 'IQVIA_GERMANY',
             grepl('JMDC', databaseNameU) ~ 'JMDC_JAPAN',
             grepl('OPTUM_EHR', databaseNameU) ~ 'OPTUM_EHR_US',
             grepl('OPTUM_EXTENDED', databaseNameU) ~ 'OPTUM_SES_US',
             grepl('CCAE', databaseNameU) ~ 'CCAE_US',
             grepl('MDCD', databaseNameU) ~ 'MDCD_US',
             grepl('MDCR', databaseNameU) ~ 'MDCR_US',
             grepl('CPRD', databaseNameU) ~ 'CPRD_GOLD_UK',
             grepl('CUMC', databaseNameU) ~ 'CUMC_US',
             grepl('SIDIAP', databaseNameU) ~ 'SIDIAP_H_SPAIN',
             grepl('INTEGRATED PRIMARY CARE INFORMATION', databaseNameU) ~ 'IPCI_NETHERLANDS'
           )
         ))

my_color <- c('#e6194b', '#3cb44b', '#ffe119', '#ed6276', 
              '#f58231', '#911eb4', '#46f0f0', '#f032e6',
              '#93f238',  '#4363d8', '#800000','#808000','#000075')

database_ref$db_color <- my_color 

IR_use <- IR_all_orig %>%
  dplyr::mutate(
    cell_ls5 = if_else(numOutcomes < 5 |
                         numPersonsWOutcome < 5, 1 , 0),
    #### flag small cell size #####
    IR_P_100000py = case_when(cell_ls5 == 0 ~ incidenceRateP100py *
                                1000) #### format to per 100,000 person-years ###
  ) %>%
  left_join(select(outcome_ref,-cleanWindow,-outcomeName), by = "outcomeId") %>%
  left_join(select(subgroup_ref, -age_lb,-age_ub,-subgroupName), by = 'subgroupCohortDefinitionId') %>%
  left_join(select(database_ref, -databaseNameU), by = 'databaseName') %>%
  mutate(IR_P_100000py = case_when(!(
    db_name == 'IPCI_NETHERLANDS' & ip_require == 1
  ) ~ IR_P_100000py)) 

IR_use_for_MA <- IR_use %>%
  filter(targetCohortDefinitionId == 104,  #1JAN2017-2019
         timeAtRiskId == 5,  #365d
         subgroupCohortDefinitionId > 20, #age/sex strata
         outcomeCohortDefinitionId %in% c(312,303, 323,307, 332, 321, 308, 305, 324, 326, 314, 319, 310, 316, 318), #primary phenotypes (removing Kawasaki and febrile seizures)
         numPersonsAtRisk > 0, # remove records for strata with no persons at risk
         !(outcomeCohortDefinitionId %in% c(312, 308, 305, 324, 310) & db_name %in% c('IPCI_NETHERLANDS', 'IQVIA_AUSTRALIA', 'IQVIA_FRANCE', 'IQVIA_GERMANY', 'CPRD_GOLD_UK')), # remove records for IP phenotypes from databases that dont capture IP
         !(outcomeCohortDefinitionId %in% c(321, 301, 314, 318, 319) & db_name %in% c('IPCI_NETHERLANDS')),  # remove records for ICPI for outcomes without ICPC codes
         !(subgroupCohortDefinitionId < 70 & db_name == 'MDCR_US'), # remove subgroups for MDCR < 65
         !(outcomeCohortDefinitionId %in% c(303) & db_name %in% c('JMDC_JAPAN')), # remove anaplyaxis for JMDC due to observed data quality issue
         !(outcomeCohortDefinitionId %in% c(321) & db_name %in% c('IQVIA_AUSTRALIA', 'IQVIA_FRANCE')), # remove DIC from australia and france since never observed in any strata
         !(outcomeCohortDefinitionId %in% c(318) & db_name %in% c('IQVIA_FRANCE')), # remove TM from france since never observed in any strata
         !(outcomeCohortDefinitionId %in% c(319) & db_name %in% c('OPTUM_EHR_US')) # remove narcolepsy from optumEHR since unexplainable increase over time in CohortDiangostics
  )


IR_use_for_MA$uncensoredOutcomes <- round(IR_use_for_MA$personYears*IR_use_for_MA$incidenceRateP100py/100)

#Meta result
meta_result <- meta_result0 %>% mutate(
  ir.rand=exp(random.te)*100000,
  ir.rand.l=exp(random.te.lower)*100000,
  ir.rand.u=exp(random.te.upper)*100000,
  ir.predict.lower=exp(lower.predict)*100000, 
  ir.predict.upper=exp(upper.predict)*100000
)

#add refs names
meta_result <-  meta_result %>% 
  left_join(select(method_ref,targetCohortDefinitionId, targetName,timeAtRiskId), 
            by=c('targetCohortDefinitionId','timeAtRiskId'))%>%
  left_join(rename(outcome_ref, outcomeCohortDefinitionId =outcomeId), by="outcomeCohortDefinitionId") %>%
  left_join(select(subgroup_ref,- age_lb, - age_ub),by='subgroupCohortDefinitionId')


tb2_a <-  meta_result %>% ungroup() %>%
  # filter(targetCohortDefinitionId == 104 &  timeAtRiskId == 5 & 
  #          outcome_include==1 & !subgroupCohortDefinitionId %in% c(0,1,2)) %>%
  mutate(IR.meta.ci= paste(round(ir.rand,2), " (", round(ir.rand.l,2), "-", round(ir.rand.u,2),")", sep='' ),
         IR.meta.ci.predict= paste(round(ir.rand,2), " (", round(ir.predict.lower,2), "-", round(ir.predict.upper,2),")", sep='' )
  )


theme_my <- function(base_size=12) {
  library(grid)
  #   library(ggthemes)
  #   (theme_foundation(base_size=base_size)
  theme(
    plot.title = ggplot2::element_text(
      face = "bold",
      size = rel(1.2),
      hjust = 0.5
    ),
    #      text = ggplot2::element_text(family= 'Arial'),
    panel.background = ggplot2::element_rect(colour = NA),
    plot.background = ggplot2::element_rect(colour = NA),
    #  panel.border = ggplot2::element_rect(colour = NA),
    axis.title = ggplot2::element_text(face = "bold", size = rel(1)),
    axis.title.y = ggplot2::element_text(
      angle = 90,
      vjust = 2,
      size = base_size
    ),
    axis.title.x = ggplot2::element_text(vjust = -0.2, size = base_size),
    axis.text = ggplot2::element_text(size = base_size),
    axis.line = ggplot2::element_line(colour = "black"),
    axis.ticks = ggplot2::element_line(),
    panel.grid.major = ggplot2::element_line(colour = "#f0f0f0"),
    panel.grid.minor = element_blank(),
    legend.key = ggplot2::element_rect(colour = NA),
    #  legend.position = "bottom",
    legend.direction = "horizontal",
    legend.key.size = unit(0.2, "cm"),
    # legend.margin = unit(0, "cm"),
    legend.title = ggplot2::element_text(face = "italic", size = base_size),
    plot.margin = unit(c(10, 5, 5, 5), "mm"),
    strip.background = ggplot2::element_rect(colour = "#f0f0f0", fill =
                                               "#f0f0f0"),
    strip.text = ggplot2::element_text(face = "bold", size = base_size),
    legend.text = ggplot2::element_text(size = base_size)
  )
  
} 


ir_for_plot <- IR_use_for_MA %>%
  #  filter (!subgroupCohortDefinitionId %in% c(0,1,2)) %>%
  left_join(
    select(
      tb2_a,
      outcomeCohortDefinitionId,
      subgroupCohortDefinitionId,
      ir.rand,
      ir.rand.l,
      ir.rand.u,
      ir.predict.lower,
      ir.predict.upper
    ),
    by = c('outcomeCohortDefinitionId', 'subgroupCohortDefinitionId')
  ) %>%
  mutate(sex_group_meta = paste(sex_group, ".meta", sep = ''))

db_color <-
  setNames(as.character(database_ref$db_color), database_ref$db_name)
db_color_sex <-
  c(db_color,
    setNames('#8b0000', 'Female'),
    setNames('#00008b', 'Male'))
db_shape_sex <-
  c(
    'Male' = 2,
    'Female' = 1,
    'Male.meta' = 17,
    'Female.meta' = 16
  )

database_ref$db_name
db_names <- as.character(database_ref$db_name)





# plotIRv3(outcomeCohortDefinitionId, "Common") + theme_my(base_size = 10) +
#   scale_y_continuous(
#     trans = 'log10',
#     limits = c(0.1, 10000),
#     breaks = c(0.1, 1, 10, 100, 1000, 10000)
#   ) + theme(legend.position = "none")


ageGroups <- ir_for_plot$age_group %>% 
  unique()
sexGroups <- ir_for_plot$sex_group %>% 
  unique()

