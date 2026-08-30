
######################################################################################
########## Script to calculate descriptive statistics and  figures of community surveys ##########
######################################################################################

######### Prepared for the paper "Social feasibility of community-led 
#########invasive species management in freshwater ecosystems bordering Kruger National Park, South Africa."

########## Authors: Berdeja, D., Bunn, D., South, J., Dumisani, K., and Hinch, S. 

#--------------------------- Packages ---------------------------#
#Install and load packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
#Read CSV file 

community_survey <- read.csv("Community Survey.csv")

#-----------SECTION A: Resondent Demographics and Household Context--------# 
#-----------Analysis of Variables---------------#


#1. Age 

#Descriptive Statistics of age 
mean(community_survey$age, na.rm = TRUE)

min(community_survey$age, na.rm = TRUE)

max(community_survey$age, na.rm = TRUE)

sd(community_survey$age, na.rm = TRUE)

#2. Gender

#Check for unique values 

unique(community_survey$gender)

#Group answers that are the same but inputted differently 
# ex. Male/Man or Female/Woman
#And remove blanks, put non-binary under prefer not to say 

community_survey$gender <- dplyr::case_when(
  community_survey$gender %in% c("Male", "Man", "Men") ~ "Male",
  community_survey$gender %in% c("Female", "Woman") ~ "Female",
  community_survey$gender %in% c("Woman/Prefer not to say", "Non-binary", "") ~ "Prefer not to say",
  TRUE ~ community_survey$gender
)

#Proportions of male/female/(prefer not to say)

gender_summary <- community_survey %>%
  filter(!is.na(gender)) %>%
  count(gender) %>%
  mutate(
    proportion = n / sum(n),
    percentage = round(proportion * 100, 1)
  )

gender_summary

#3. Ethnicity 

#check for unique entries 

unique(community_survey$ethnicity)

#group values that are the same 
#ex.Afican and Black African 

community_survey$ethnicity <- dplyr::case_when(
  community_survey$ethnicity %in% c("Black African", "African") ~ "Black African",
  community_survey$ethnicity == "" ~ NA_character_,
  TRUE ~ community_survey$ethnicity
)

#verify data cleaning is correct 

unique(community_survey$ethnicity)

table(community_survey$ethnicity, useNA = "ifany")

#calculate proportions of ethnicity 

ethnicity_summary <- community_survey %>%
  filter(!is.na(ethnicity)) %>%
  count(ethnicity) %>%
  mutate(
    proportion = n / sum(n),
    percentage = round(proportion * 100, 1)
  )

ethnicity_summary

#4. Education 

#check for unique values 

unique(community_survey$education)

#Group unique values together 
# Ex. Secondary/Some Secondary/Secondary incomplete 

community_survey$education <- dplyr::case_when(
  community_survey$education %in% c("None", "No formal", "No formal schooling", "No formal ") ~ "No formal education",
  
  community_survey$education == "Primary" ~ "Primary",
  
  community_survey$education %in% c(
    "Secondary",
    "Some secondary",
    "Some Secondary",
    "Secondary incomplete"
  ) ~ "Some secondary",
  
  community_survey$education == "Matric" ~ "Matric",
  
  community_survey$education %in% c(
    "Diploma",
    "Matric/Diploma",
    "Secondary, Matric, Diploma"
  ) ~ "Diploma",
  
  community_survey$education == "Degree" ~ "Degree",
  
  community_survey$education == "" ~ NA_character_,
  
  TRUE ~ community_survey$education
)

#Check results of recoding 

table(community_survey$education, useNA = "ifany")

#Check for proportions 

education_summary <- community_survey %>%
  filter(!is.na(education)) %>%
  count(education) %>%
  mutate(
    proportion = n / sum(n),
    percentage = round(proportion * 100, 1)
  )

education_summary

#5. Household size 

#with all values included

mean(community_survey$household_size, na.rm = TRUE) 

min(community_survey$household_size, na.rm = TRUE) 

max(community_survey$household_size, na.rm = TRUE) 

sd(community_survey$household_size, na.rm = TRUE)

#removing outlier 27?

mean(community_survey$household_size[community_survey$household_size != 27], na.rm = TRUE)

min(community_survey$household_size[community_survey$household_size != 27], na.rm = TRUE)

max(community_survey$household_size[community_survey$household_size != 27], na.rm = TRUE)

sd(community_survey$household_size[community_survey$household_size != 27], na.rm = TRUE)

#6. Employment Status 

#Check unique values 

unique(community_survey$employment_status)

#Recode the unique values into proper categories 
# ex. (Employed full-time, Full-time)

community_survey$employment_status <- dplyr::case_when(
  community_survey$employment_status %in% c(
    "Full-time",
    "Employed full-time",
    "Employed full-time ",
    "Employed"
  ) ~ "Employed full-time",
  
  community_survey$employment_status %in% c(
    "Employed part-time/seasonal",
    "Part-time/Seasonal"
  ) ~ "Employed part-time/seasonal",
  
  community_survey$employment_status %in% c(
    "Self-employed",
    "Self-employed/Unemployed"
  ) ~ "Self-employed",
  
  community_survey$employment_status %in% c(
    "Unemployed",
    "Hustle/Unemployed"
  ) ~ "Unemployed",
  
  community_survey$employment_status == "Retired" ~ "Retired",
  
  community_survey$employment_status == "Student" ~ "Student",
  
  community_survey$employment_status == "Other" ~ "Other",
  
  community_survey$employment_status == "" ~ NA_character_,
  
  TRUE ~ community_survey$employment_status
)

#check results 

table(community_survey$employment_status, useNA = "ifany")

#Check Proporitons 

employment_summary <- community_survey %>%
  filter(!is.na(employment_status)) %>%
  count(employment_status) %>%
  mutate(
    proportion = n / sum(n),
    percentage = round(proportion * 100, 1)
  )

employment_summary

#Additonal analysis, Age distribution of unemployment

#group by employemnt status 
community_survey %>%
  group_by(employment_status) %>%
  summarise(
    n = sum(!is.na(age)),
    mean_age = mean(age, na.rm = TRUE),
    median_age = median(age, na.rm = TRUE),
    min_age = min(age, na.rm = TRUE),
    max_age = max(age, na.rm = TRUE),
    sd_age = sd(age, na.rm = TRUE)
  )

#look at age of unemployed 

unemployed <- community_survey %>%
  filter(employment_status == "Unemployed")

summary(unemployed$age)
sd(unemployed$age, na.rm = TRUE)

#7. Occupation 

#Check for unique entries 

unique(community_survey$occupation)

#Group jobs into similar categories 
# ex. Builder, construction, Welder -> Construction and trades 
# also remove non-occupations ex. Student, Pensioner 

community_survey$occupation_group <- dplyr::case_when(
  community_survey$occupation %in% c(
    "Selling Tomatoes", "Hawker", "Sales",
    "Trader", "Selling Chips", "Hair salon and spaza shot"
  ) ~ "Sales/retail",
  
  community_survey$occupation %in% c(
    "Builder", "Construction", "Part-time Construction",
    "Supervisor/construction site", "Welder",
    "Brick Maker", "Tiler", "TLB"
  ) ~ "Construction & trades",
  
  community_survey$occupation == "Farming" ~ "Agriculture",
  
  community_survey$occupation %in% c(
    "Civil Servant", "Headman"
  ) ~ "Public sector",
  
  community_survey$occupation %in% c(
    "Car wash", "Pest control"
  ) ~ "Service industry",
  
  community_survey$occupation %in% c(
    "Transco-Transport tires"
  ) ~ "Transport & logistics",
  
  community_survey$occupation %in% c(
    "Self-employed", "hussler",
    "hustle(any piece-job)", "General Jobs"
  ) ~ "Casual/temporary work",
  
  TRUE ~ NA_character_
)

#Calculate proportions of occupations 
occupation_summary <- community_survey %>%
  filter(!is.na(occupation_group)) %>%
  count(occupation_group) %>%
  mutate(
    proportion = n / sum(n),
    percentage = round(100 * proportion, 1)
  )

occupation_summary

#8. Income 

#correct outlier 7500, use only weekly values if both available  

community_survey %>%
  select(income_zar_day, income_zar_week) %>%
  filter(
    (!is.na(income_zar_day) & income_zar_day > 0) |
      (!is.na(income_zar_week) & income_zar_week > 0)
  )

community_survey %>%
  filter(
    !is.na(income_zar_day) & income_zar_day > 0,
    !is.na(income_zar_week) & income_zar_week > 0
  ) %>%
  select(income_zar_day, income_zar_week)

community_survey <- community_survey %>%
  mutate(
    income_weekly = case_when(
      income_zar_day == 7500 ~ 1875,
      !is.na(income_zar_week) & income_zar_week > 0 ~ income_zar_week,
      !is.na(income_zar_day) & income_zar_day > 0 ~ income_zar_day * 5,
      TRUE ~ NA_real_
    )
  )

community_survey %>%
  filter(income_zar_day == 7500) %>%
  select(income_zar_day, income_zar_week, income_weekly)

community_survey %>%
  arrange(desc(income_weekly)) %>%
  select(income_zar_day, income_zar_week, income_weekly) %>%
  head(10)

income_summary <- community_survey %>%
  filter(!is.na(income_weekly), income_weekly > 0) %>%
  summarise(
    n = n(),
    mean = mean(income_weekly),
    median = median(income_weekly),
    min = min(income_weekly),
    max = max(income_weekly),
    sd = sd(income_weekly)
  )

income_summary

#------------SECTION B: Food Security and Protein Sources---------------------# 
#Analysis of Variables 

#9. Primary source of Protein 

#Check for unique answers 

unique(community_survey$primary_protein)

#Add vegetable to other and remove blanks 

community_survey$primary_protein <- dplyr::case_when(
  community_survey$primary_protein == "Vegetable" ~ "Other",
  community_survey$primary_protein == "" ~ NA_character_,
  TRUE ~ community_survey$primary_protein
)

#Double check response 

table(community_survey$primary_protein, useNA = "ifany")

#Primary Protein proportions 

protein_summary <- community_survey %>%
  filter(!is.na(primary_protein), !is.na(community), community != "") %>%
  count(community, primary_protein) %>%
  group_by(community) %>%
  mutate(
    proportion = n / sum(n),
    percentage = proportion * 100
  ) %>%
  ungroup()

protein_summary

#get total number of vaild respondents per community 
community_np <- protein_summary %>%
  group_by(community) %>%
  summarise(
    respondents = sum(n),
    .groups = "drop"
  )

#create community labels 

community_labels <- setNames(
  paste0(
    community_np$community,
    "\n(n = ", community_np$respondents, ")"
  ),
  community_np$community
)

community_np

#Create a stacked bar graph 
protein_colours <- c(
  "Beef" = "#B56A62",
  "Chicken" = "#C49A3A",
  "Eggs" = "#8C9A62",
  "Fish" = "#4F8577",
  "Goat" = "#5E8C96",
  "Lentils" = "#657486",
  "Other" = "#9A9188"
)
ggplot(
  protein_summary,
  aes(x = community, y = percentage, fill = primary_protein)
) +
  geom_col(color = "black", linewidth = 0.3) +
  scale_fill_manual(values = protein_colours)+
  scale_x_discrete(labels = community_labels)+
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    expand = c(0, 0)
  ) +
  labs(
    x = "Community",
    y = "Valid Respondents (%)",
    fill = "Primary Protein"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#10. Consume fish 

#Calculate counts and proportions 

consume_fish_summary <- community_survey %>%
  mutate(
    consume_fish = trimws(consume_fish),
    consume_fish = na_if(consume_fish, "")
  ) %>%
  filter(!is.na(consume_fish)) %>%
  count(community, consume_fish) %>%
  group_by(community) %>%
  mutate(
    proportion = n / sum(n),
    percentage = proportion * 100
  ) %>%
  ungroup()

consume_fish_summary

#No Reasons Stated 
#check for unique answers
unique(community_survey$consume_fish_no_reason)

#summarize answers 
community_survey %>%
  filter(consume_fish == "No") %>%
  mutate(
    consume_fish_no_reason = trimws(consume_fish_no_reason),
    consume_fish_no_reason = na_if(consume_fish_no_reason, "")
  ) %>%
  filter(!is.na(consume_fish_no_reason)) %>%
  count(consume_fish_no_reason, sort = TRUE)

#11. Consume Crayfish 
unique(community_survey$consume_crayfish)

#regroup no/don't know -> don't know 
community_survey$consume_crayfish <- case_when(
  community_survey$consume_crayfish == "Yes" ~ "Yes",
  
  community_survey$consume_crayfish == "No" ~ "No",
  
  community_survey$consume_crayfish %in% c(
    "Don't know what crayfish is",
    "Don't know what crayfish is ",
    "No/Don't know",
    "No/Don't know what crayfish is",
    "No/Don't know what crayfish is "
  ) ~ "Don't know what crayfish is",
  
  trimws(community_survey$consume_crayfish) == "" ~ NA_character_,
  
  TRUE ~ community_survey$consume_crayfish
)

#double check regrouping 

table(community_survey$consume_crayfish, useNA = "ifany")

#calculate proprtions 
consume_crayfish_summary <- community_survey %>%
  mutate(
    consume_crayfish = trimws(consume_crayfish),
    consume_crayfish = na_if(consume_crayfish, "")
  ) %>%
  filter(!is.na(consume_crayfish)) %>%
  count(community, consume_crayfish) %>%
  group_by(community) %>%
  mutate(
    proportion = n / sum(n),
    percentage = proportion * 100
  ) %>%
  ungroup()

consume_crayfish_summary

#12. Fish Frequency 

#check for unique responses, should only be 6 

unique(community_survey$fish_frequency)

#check proportions 

fish_frequency_community <- community_survey %>%
  mutate(
    community = trimws(community),
    fish_frequency = trimws(fish_frequency),
    fish_frequency = na_if(fish_frequency, ""),
    fish_frequency = factor(
      fish_frequency,
      levels = c(
        "Never",
        "Less than monthly",
        "Once-twice monthly",
        "1-2 times/week",
        "3-4 times/week",
        "Daily"
      )
    )
  ) %>%
  filter(
    !is.na(fish_frequency),
    !is.na(community),
    community != ""
  ) %>%
  count(community, fish_frequency) %>%
  group_by(community) %>%
  mutate(
    proportion = n / sum(n),
    percentage = proportion * 100
  ) %>%
  ungroup()

fish_frequency_community

#get total number of vaild respondants per community 
community_n <- fish_frequency_community %>%
  group_by(community) %>%
  summarise(
    respondents = sum(n),
    .groups = "drop"
  )

#create community labels 

community_labels <- setNames(
  paste0(
    community_n$community,
    "\n(n = ", community_n$respondents, ")"
  ),
  community_n$community
)

community_n

#Create a stacked graph 

frequency_colours <- c(
  "Never" = "#D9D9D9",
  "Less than monthly" = "#B8C6C1",
  "Once-twice monthly" = "#91AAA2",
  "1-2 times/week" = "#668B82",
  "3-4 times/week" = "#416B63",
  "Daily" = "#244A44"
)

ggplot(
  fish_frequency_community,
  aes(x = community, y = percentage, fill = fish_frequency)
) +
  geom_col(color = "black", linewidth = 0.3) +
  scale_fill_manual(values = frequency_colours)+
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    expand = c(0, 0)
  ) +
  labs(
    x = "Community",
    y = "Respondents (%)",
    fill = "Fish Consumption Frequency"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_text(face = "bold")
  )
ggplot(
  fish_frequency_community,
  aes(x = community, y = proportion, fill = fish_frequency)
) +
  scale_fill_manual(values = frequency_colours)+
  geom_col(color = "black", linewidth = 0.3) +
  scale_x_discrete(labels = community_labels)+
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2),
    expand = c(0, 0)
  ) +
  labs(
    x = "Community",
    y = "Proportion of Valid Responses",
    fill = "Fish Consumption Frequency"
  ) +
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_text(face = "bold")
  )
#13. Crayfish Frequency 
#check for unique responses, should only be 6 

unique(community_survey$crayfish_frequency)

#check proportions 

crayfish_frequency_community <- community_survey %>%
  mutate(
    community = trimws(community),
    crayfish_frequency = trimws(crayfish_frequency),
    crayfish_frequency = na_if(crayfish_frequency, ""),
    crayfish_frequency = factor(
      crayfish_frequency,
      levels = c(
        "Never",
        "Less than monthly",
        "Once-twice monthly",
        "1-2 times/week",
        "3-4 times/week",
        "Daily"
      )
    )
  ) %>%
  filter(
    !is.na(crayfish_frequency),
    !is.na(community),
    community != ""
  ) %>%
  count(community, crayfish_frequency) %>%
  group_by(community) %>%
  mutate(
    proportion = n / sum(n),
    percentage = proportion * 100
  ) %>%
  ungroup()

crayfish_frequency_community

#Create a stacked graph 

frequency_colours <- c(
  "Never" = "#D9D9D9",
  "Less than monthly" = "#B8C6C1",
  "Once-twice monthly" = "#91AAA2",
  "1-2 times/week" = "#668B82",
  "3-4 times/week" = "#416B63",
  "Daily" = "#244A44"
)

ggplot(
  crayfish_frequency_community,
  aes(x = community, y = percentage, fill = crayfish_frequency)
) +
  geom_col(color = "black", linewidth = 0.3) +
  scale_fill_manual(values = frequency_colours)+
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    expand = c(0, 0)
  ) +
  labs(
    x = "Community",
    y = "Respondents (%)",
    fill = "Crayfish Consumption Frequency"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_text(face = "bold")
  )
ggplot(
  crayfish_frequency_community,
  aes(x = community, y = proportion, fill = crayfish_frequency)
) +
  scale_fill_manual(values = frequency_colours)+
  geom_col(color = "black", linewidth = 0.3) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2),
    expand = c(0, 0)
  ) +
  labs(
    x = "Community",
    y = "Proportion of Valid Responses",
    fill = "Crayfish Consumption Frequency"
  ) +
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_text(face = "bold")
  )
#14. Factors affecting fish 

factor_columns <- c(
  "factor_not_enough_fish",
  "factor_water_pollution",
  "factor_drought",
  "factor_fishing_restrictions",
  "factor_lack_equipment",
  "factor_cost",
  "factor_safety",
  "factor_distance",
  "factor_health_concerns",
  "factor_other"
)

factor_summary <- community_survey %>%
  mutate(respondent_id = row_number()) %>%
  select(respondent_id, community, all_of(factor_columns)) %>%
  pivot_longer(
    cols = all_of(factor_columns),
    names_to = "factor",
    values_to = "selected"
  ) %>%
  mutate(
    community = trimws(community),
    
    selected = trimws(as.character(selected)),
    
    selected = case_when(
      selected %in% c("1", "Yes", "yes", "YES") ~ 1,
      selected %in% c("0", "No", "no", "NO") ~ 0,
      selected == "" ~ NA_real_,
      TRUE ~ suppressWarnings(as.numeric(selected))
    ),
    
    factor = case_when(
      factor == "factor_not_enough_fish" ~ "Not enough fish",
      factor == "factor_water_pollution" ~ "Water pollution",
      factor == "factor_drought" ~ "Drought",
      factor == "factor_fishing_restrictions" ~ "Fishing restrictions",
      factor == "factor_lack_equipment" ~ "Lack of equipment",
      factor == "factor_cost" ~ "Cost",
      factor == "factor_safety" ~ "Safety concerns",
      factor == "factor_distance" ~ "Distance",
      factor == "factor_health_concerns" ~ "Health concerns",
      factor == "factor_other" ~ "Other"
    )
  ) %>%
  filter(
    !is.na(community),
    community != "",
    !is.na(selected),
    selected %in% c(0, 1)
  ) %>%
  group_by(community, factor) %>%
  summarise(
    selected_n = sum(selected == 1),
    valid_n = n(),
    percentage = 100 * selected_n / valid_n,
    .groups = "drop"
  )

factor_summary

factor_order <- factor_summary %>%
  group_by(factor) %>%
  summarise(overall = mean(percentage)) %>%
  arrange(desc(overall)) %>%
  pull(factor)

factor_summary$factor <- factor(
  factor_summary$factor,
  levels = rev(factor_order)
)
# Total number of respondents who selected each factor
factor_n <- factor_summary %>%
  group_by(factor) %>%
  summarise(
    respondents = sum(selected_n),
    .groups = "drop"
  )

factor_n
factor_labels <- setNames(
  paste0(
    factor_n$factor,
    " (n = ", factor_n$respondents, ")"
  ),
  factor_n$factor
)
#Factor summary figure, groupe horizontal bar graph 
community_colours <- c(
  "Belfast"   = "#4E79A7",  # blue
  "Dumphries" = "#7AA974",  # green
  "Masetoni"  = "#C76D5B",  # muted red
  "Matsulu"   = "#8E77B5",  # purple
  "Selwana"   = "#D8A031",  # mustard
  "Sgagule"   = "#5AA6A5"   # teal
)

ggplot(
  factor_summary,
  aes(
    x = percentage,
    y = factor,
    fill = community
  )
) +
  scale_x_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    expand = c(0, 0)
  )+
  scale_y_discrete(labels = factor_labels) +
  scale_fill_manual(values = community_colours)+
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7,
    color = "black",
    linewidth = 0.2
  ) +
  labs(
    x = "Proportion of Valid Responses",
    y = "Factors Affecting Fish & Crayfish Consumption",
    fill = "Community"
  )+
  theme_classic() +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    panel.grid.major.x = element_line(color = "grey90"),
    panel.grid.minor = element_blank()
  )

#Group for all communities 

#Proportions 

factor_columns <- c(
  "factor_not_enough_fish",
  "factor_water_pollution",
  "factor_drought",
  "factor_fishing_restrictions",
  "factor_lack_equipment",
  "factor_cost",
  "factor_safety",
  "factor_distance",
  "factor_health_concerns",
  "factor_other"
)

factor_summary_all <- community_survey %>%
  select(all_of(factor_columns)) %>%
  pivot_longer(
    cols = all_of(factor_columns),
    names_to = "factor",
    values_to = "selected"
  ) %>%
  mutate(
    selected = trimws(as.character(selected)),
    
    selected = case_when(
      selected %in% c("1", "Yes", "yes", "YES") ~ 1,
      selected %in% c("0", "No", "no", "NO") ~ 0,
      selected == "" ~ NA_real_,
      TRUE ~ suppressWarnings(as.numeric(selected))
    ),
    
    factor = case_when(
      factor == "factor_not_enough_fish" ~ "Not enough fish",
      factor == "factor_water_pollution" ~ "Water pollution",
      factor == "factor_drought" ~ "Drought",
      factor == "factor_fishing_restrictions" ~ "Fishing restrictions",
      factor == "factor_lack_equipment" ~ "Lack of equipment",
      factor == "factor_cost" ~ "Cost",
      factor == "factor_safety" ~ "Safety concerns",
      factor == "factor_distance" ~ "Distance",
      factor == "factor_health_concerns" ~ "Health concerns",
      factor == "factor_other" ~ "Other"
    )
  ) %>%
  filter(
    !is.na(selected),
    selected %in% c(0, 1)
  ) %>%
  group_by(factor) %>%
  summarise(
    selected_n = sum(selected == 1),
    valid_n = n(),
    proportion = selected_n / valid_n,
    percentage = round(100 * proportion, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(percentage))

factor_summary_all

#Figure for factors affecting eating fish 
factor_summary_all$factor <- factor(
  factor_summary_all$factor,
  levels = rev(factor_summary_all$factor)
)

ggplot(
  factor_summary_all,
  aes(
    x = percentage,
    y = factor
  )
) +
  geom_col(
    width = 0.7,
    fill = "#4F8577",
    color = "black",
    linewidth = 0.2
  ) +
  scale_x_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    expand = c(0, 0)
  ) +
  labs(
    x = "Proportion of Valid Responses",
    y = "Factors Affecting Fish & Crayfish Consumption"
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    panel.grid.major.x = element_line(color = "grey90"),
    panel.grid.minor = element_blank()
  )

#15. Preferred Protein 

#Check for unique responses to remove incorrect ones 
pref_protein_columns <- c(
  "pref_protein_chicken",
  "pref_protein_beef",
  "pref_protein_goat",
  "pref_protein_pork",
  "pref_protein_fish",
  "pref_protein_crayfish",
  "pref_protein_eggs",
  "pref_protein_beans",
  "pref_protein_other"
)

pref_protein_summary <- community_survey %>%
  select(all_of(pref_protein_columns)) %>%
  pivot_longer(
    everything(),
    names_to = "protein",
    values_to = "selected"
  ) %>%
  mutate(
    protein = recode(
      protein,
      pref_protein_chicken = "Chicken",
      pref_protein_beef = "Beef",
      pref_protein_goat = "Goat",
      pref_protein_pork = "Pork",
      pref_protein_fish = "Fish",
      pref_protein_crayfish = "Crayfish",
      pref_protein_eggs = "Eggs",
      pref_protein_beans = "Beans",
      pref_protein_other = "Other"
    )
  ) %>%
  group_by(protein) %>%
  summarise(
    n = sum(selected == 1, na.rm = TRUE),
    total = sum(!is.na(selected)),
    proportion = n / total,
    percentage = round(100 * proportion, 1)
  )%>%
  arrange(desc(percentage))

pref_protein_summary

#Rows that have pref_protein_other checked off 

community_survey %>%
  filter(pref_protein_other == 1)
#16. Preferred Protein reasons 

#Check unique reasons 
sort(unique(protein_reason_data$reason))

#Categorize 

protein_reason_summary <- protein_reason_data %>%
  mutate(
    theme = case_when(
      
      grepl("cheap|affordable|inexpensive|cost|less expensive",
            reason, ignore.case = TRUE) ~ "Affordability/Cost",
      
      grepl("healthy|protein|nutrit|vitamin|energy|strength|body|low fat|benefit",
            reason, ignore.case = TRUE) ~ "Health/Nutrition",
      
      grepl("tasty|taste|delicious|love|favourite|favorite|nice|good|pleasant|palatable|smells",
            reason, ignore.case = TRUE) ~ "Taste/Preference",
      
      grepl("easy to buy|easy to find|available|raise|farm|river|home|find",
            reason, ignore.case = TRUE) ~ "Availability/Accessibility",
      
      grepl("grew up",
            reason, ignore.case = TRUE) ~ "Cultural/Familiarity",
      
      grepl("easy to cook|cook|doesn't take long",
            reason, ignore.case = TRUE) ~ "Convenience",
      
      TRUE ~ "Other"
    )
  ) %>%
  count(theme, sort = TRUE) %>%
  mutate(
    percent = round(100 * n / sum(n), 1)
  )

protein_reason_summary

#recode for answers to be able to contribute to 
#multiple themes 

protein_reason_summary <- protein_reason_data %>%
  mutate(
    reason = trimws(reason),
    
    affordability = grepl(
      "cheap|affordable|inexpensive|cost|less expensive|expensive",
      reason, ignore.case = TRUE
    ),
    
    health = grepl(
      "healthy|protein|nutrit|vitamin|energy|strength|body|low fat|benefit|sick|ill",
      reason, ignore.case = TRUE
    ),
    
    taste = grepl(
      "tasty|taste|delicious|love|favourite|favorite|nice|good|pleasant|palatable|smells|best",
      reason, ignore.case = TRUE
    ),
    
    availability = grepl(
      "easy to buy|easy to find|available|raise|farm|river|home|find|can be bought",
      reason, ignore.case = TRUE
    ),
    
    cultural = grepl(
      "grew up|common protein",
      reason, ignore.case = TRUE
    ),
    
    convenience = grepl(
      "easy to cook|cook|doesn't take long|easy/convenient",
      reason, ignore.case = TRUE
    ),
    
    other = !(
      affordability |
        health |
        taste |
        availability |
        cultural |
        convenience
    )
  ) %>%
  pivot_longer(
    cols = c(
      affordability,
      health,
      taste,
      availability,
      cultural,
      convenience,
      other
    ),
    names_to = "theme",
    values_to = "matched"
  ) %>%
  filter(matched) %>%
  mutate(
    theme = recode(
      theme,
      affordability = "Affordability/Cost",
      health = "Health/Nutrition",
      taste = "Taste/Preference",
      availability = "Availability/Accessibility",
      cultural = "Cultural/Familiarity",
      convenience = "Convenience",
      other = "Other"
    )
  ) %>%
  count(theme, sort = TRUE) %>%
  mutate(
    percentage = round(100 * n / nrow(protein_reason_data), 1)
  )

protein_reason_summary
#------------Section C: Fishing Participation Patterns------------------# 
#Analysis 

#17. Do you fish?

fishing_summary <- community_survey %>%
  mutate(
    do_you_fish = trimws(do_you_fish),
    do_you_fish = na_if(do_you_fish, "")
  ) %>%
  filter(!is.na(do_you_fish)) %>%
  count(community, do_you_fish) %>%
  group_by(community) %>%
  mutate(
    proportion = n / sum(n),
    percentage = proportion * 100
  ) %>%
  ungroup()

fishing_summary
fishing_summary_all <- community_survey %>%
  mutate(
    do_you_fish = trimws(do_you_fish),
    do_you_fish = na_if(do_you_fish, "")
  ) %>%
  filter(!is.na(do_you_fish)) %>%
  count( do_you_fish) %>%
  mutate(
    proportion = n / sum(n),
    percentage = proportion * 100
  ) %>%
  ungroup()

fishing_summary_all

#Create a figure 

community_n <- community_survey %>%
  mutate(
    community = trimws(community),
    do_you_fish = trimws(do_you_fish)
  ) %>%
  filter(
    !is.na(community),
    community != "",
    !is.na(do_you_fish),
    do_you_fish != ""
  ) %>%
  count(community, name = "N")

community_labels <- setNames(
  paste0(community_n$community, "\n(n = ", community_n$N, ")"),
  community_n$community
)

ggplot(
  fishing_summary,
  aes(
    x = community,
    y = percentage,
    fill = do_you_fish
  )
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7,
    color = "black"
  ) +
  scale_fill_manual(
    values = c(
      "No" = "#d3a13b",
      "Yes" = "#5277a3"
    ),
    breaks = c("No", "Yes")
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 25),
    labels = function(x) paste0(x, "%"),
    expand = c(0, 0)
  ) +
  scale_x_discrete(labels = community_labels) +
  labs(
    x = "Community",
    y = "Percentage of respondents",
    fill = "Do you fish?"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    axis.title = element_text(size = 13),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 11),
    legend.position = "right"
  )

#18. Primary Motivation for fishing 

#Check for unique responses

unique(community_survey$fishing_motivation)

#recode answers 

community_survey$fishing_motivation <- case_when(
  community_survey$fishing_motivation == "Subsistence" ~ "Subsistence",
  community_survey$fishing_motivation == "Sale" ~ "Sale",
  community_survey$fishing_motivation == "Recreation" ~ "Recreation",
  community_survey$fishing_motivation %in% c(
    "Cultural Practice",
    "Cultural practice"
  ) ~ "Cultural Practice",
  community_survey$fishing_motivation %in% c(
    "Other",
    "Kute"
  ) ~ "Other",
  trimws(community_survey$fishing_motivation) == "" ~ NA_character_,
  TRUE ~ community_survey$fishing_motivation
)

#Calculate Proportions 
motivation_summary <- community_survey %>%
  mutate(
    fishing_motivation = trimws(fishing_motivation),
    fishing_motivation = na_if(fishing_motivation, "")
  ) %>%
  filter(!is.na(fishing_motivation)) %>%
  count(fishing_motivation) %>%
  mutate(
    proportion = n / sum(n),
    percentage = proportion * 100
  ) %>%
  ungroup()

motivation_summary

#19. How often do you fish 

#check for unique answers 

unique(community_survey$fishing_frequency)

#recode answers 

community_survey$fishing_frequency <- case_when(
  community_survey$fishing_frequency == "Daily" ~ "Daily",
  community_survey$fishing_frequency == "Weekly" ~ "Weekly",
  community_survey$fishing_frequency == "Monthly" ~ "Monthly",
  community_survey$fishing_frequency %in% c("Seasonally", "Seasonally ") ~ "Seasonally",
  community_survey$fishing_frequency == "Rarely" ~ "Rarely",
  
  
  # Remove multiple responses
  community_survey$fishing_frequency %in% c(
    "Monthly/Seasonally",
    "Daily/Monthly",
    "Never"
  ) ~ NA_character_,
  
  # Remove blanks
  trimws(community_survey$fishing_frequency) == "" ~ NA_character_,
  
  TRUE ~ community_survey$fishing_frequency
)

#check answers 
table(community_survey$fishing_frequency, useNA = "ifany")

#Check proportions 

fishing_frequency_community <- community_survey %>%
  mutate(
    community = trimws(community),
    fishing_frequency = trimws(fishing_frequency),
    fishing_frequency = na_if(fishing_frequency, ""),
    fishing_frequency = factor(
      fishing_frequency,
      levels = c(
        "Rarely",
        "Seasonally",
        "Monthly",
        "Weekly",
        "Daily"
      )
    )
  ) %>%
  filter(
    !is.na(fishing_frequency),
    !is.na(community),
    community != ""
  ) %>%
  count(community, fishing_frequency) %>%
  group_by(community) %>%
  mutate(
    proportion = n / sum(n),
    percentage = proportion * 100
  ) %>%
  ungroup()

fishing_frequency_community
#Create figure with sequential colours 

frequency_colours <- c(
  "Rarely" = "#B8C6C1",
  "Seasonally" = "#91AAA2",
  "Monthly" = "#668B82",
  "Weekly" = "#416B63",
  "Daily" = "#244A44"
)

ggplot(
  fishing_frequency_community,
  aes(x = community, y = percentage, fill = fishing_frequency)
) +
  geom_col(color = "black", linewidth = 0.3) +
  scale_fill_manual(values = frequency_colours)+
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    expand = c(0, 0)
  ) +
  labs(
    x = "Community",
    y = "Respondents (%)",
    fill = "Fishing Frequency"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_text(face = "bold")
  )
ggplot(
  fishing_frequency_community,
  aes(x = community, y = proportion, fill = fishing_frequency)
) +
  scale_fill_manual(values = frequency_colours)+
  geom_col(color = "black", linewidth = 0.3) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2),
    expand = c(0, 0)
  ) +
  labs(
    x = "Community",
    y = "Proportion of Valid Responses",
    fill = "Fishing Frequency"
  ) +
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_text(face = "bold")
  )

#20. Where do you fish?
#calculate the amound of n respondetns who gave at least one locatoin 
# Number of respondents with a valid fishing-location response per community
community_n <- community_survey %>%
  mutate(
    across(
      all_of(location_columns),
      ~ suppressWarnings(as.numeric(as.character(.)))
    )
  ) %>%
  group_by(community) %>%
  summarise(
    respondents = sum(
      if_any(all_of(location_columns), ~ !is.na(.))
    ),
    .groups = "drop"
  )

community_n
total_location_respondents <- community_survey %>%
  filter(
    if_any(
      all_of(location_columns),
      ~ !is.na(.) & trimws(as.character(.)) != ""
    )
  ) %>%
  nrow()

total_location_respondents

community_survey %>%
  summarise(
    n_0 = sum(location_local_river == 0, na.rm = TRUE),
    n_1 = sum(location_local_river == 1, na.rm = TRUE),
    total = n_0 + n_1
  )
community_survey %>%
  summarise(
    n_0 = sum(location_other == 0, na.rm = TRUE),
    n_1 = sum(location_other == 1, na.rm = TRUE),
    total = n_0 + n_1
  )
community_survey %>%
  summarise(
    n_0 = sum(location_knp == 0, na.rm = TRUE),
    n_1 = sum(location_knp == 1, na.rm = TRUE),
    total = n_0 + n_1
  )
community_survey %>%
  summarise(
    n_0 = sum(location_varies == 0, na.rm = TRUE),
    n_1 = sum(location_varies == 1, na.rm = TRUE),
    total = n_0 + n_1
  )
#create labels for n communities 

community_labels <- setNames(
  paste0(community_n$community, "\n(n = ", community_n$respondents, ")"),
  community_n$community
)

#summary table 

location_columns <- c(
  "location_local_river",
  "location_other",
  "location_knp",
  "location_varies"
)

location_summary <- community_survey %>%
  select(community, all_of(location_columns)) %>%
  pivot_longer(
    cols = all_of(location_columns),
    names_to = "location",
    values_to = "selected"
  ) %>%
  mutate(
    selected = as.numeric(selected),
    location = recode(
      location,
      location_local_river = "Local river or dam",
      location_other = "Other",
      location_knp = "Kruger National Park",
      location_varies = "Varies"
    )
  ) %>%
  group_by(community, location) %>%
  summarise(
    n = sum(selected == 1, na.rm = TRUE),
    total = sum(!is.na(selected)),
    proportion = n / total,
    percentage = round(100 * proportion, 1),
    .groups = "drop"
  )

location_summary

#create a side by side bar graph 

location_colours <- c(
  "Local river or dam" = "#4F8577",
  "Kruger National Park" = "#7A6F9B",
  "Varies" = "#C49A3A",
  "Other" = "#9A9188"
)

ggplot(
  location_summary,
  aes(
    x = community,
    y = percentage,
    fill = location
  )
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7,
    color = "black"
  ) +
  scale_x_discrete(labels = community_labels)+
  scale_fill_manual(values = location_colours) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    labels = function(x) paste0(x, "%"),
    expand = c(0, 0)
  ) +
  labs(
    x = "Community",
    y = "Proportion of valid responses",
    fill = "Fishing location"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_text(face = "bold")
  )
#21. Gear Types

gear_columns <- c(
  "gear_rod_line",
  "gear_handline",
  "gear_gill_net",
  "gear_trap",
  "gear_spear",
  "gear_other"
)

gear_summary <- community_survey %>%
  select(all_of(gear_columns)) %>%
  pivot_longer(
    cols = all_of(gear_columns),
    names_to = "gear",
    values_to = "selected"
  ) %>%
  mutate(
    selected = as.numeric(selected),
    gear = recode(
      gear,
      gear_rod_line  = "Rod Line",
      gear_handline = "Hand Line",
      gear_gill_net = "Net",
      gear_trap = "Trap",
      gear_spear = "Spear",
      gear_other = "Other",
    )
  ) %>%
  group_by(gear) %>%
  summarise(
    n = sum(selected == 1, na.rm = TRUE),
    total = sum(selected %in% c(0, 1)),
    proportion = n / total,
    percentage = round(100 * proportion, 1),
    .groups = "drop"
  )

gear_summary

#22. Bait 
bait_columns <- c(
  "bait_worms",
  "bait_dough",
  "bait_dog_food",
  "bait_fish_pieces",
  "bait_insects",
  "bait_artificial_lures",
  "bait_other"
)

bait_summary <- community_survey %>%
  select(all_of(bait_columns)) %>%
  pivot_longer(
    cols = all_of(bait_columns),
    names_to = "bait",
    values_to = "selected"
  ) %>%
  mutate(
    selected = as.numeric(selected),
    bait = recode(
      bait,
      bait_worms = "Worms",
      bait_dough = "Dough",
      bait_dog_food = "Dog food",
      bait_fish_pieces = "Fish pieces",
      bait_insects = "Insects",
      bait_artificial_lures = "Artificial lure",
      bait_other = "Other"
    )
  ) %>%
  group_by(bait) %>%
  summarise(
    n = sum(selected == 1, na.rm = TRUE),
    total = sum(selected %in% c(0, 1)),
    proportion = n / total,
    percentage = round(100 * proportion, 1),
    .groups = "drop"
  )

bait_summary

#23. Capture use 

catch_columns <- c(
  "catch_eat_home",
  "catch_share",
  "catch_sell",
  "catch_bait",
  "catch_release",
  "catch_discard",
  "catch_other"
)

catch_summary <- community_survey %>%
  select(all_of(catch_columns)) %>%
  pivot_longer(
    cols = all_of(catch_columns),
    names_to = "catch",
    values_to = "selected"
  ) %>%
  mutate(
    selected = as.numeric(selected),
    catch = recode(
      catch,
      catch_eat_home = "Eat at home",
      catch_share = "Share",
      catch_sell = "Sell",
      catch_bait = "Use as bait",
      catch_release = "Release",
      catch_discard = "Discard",
      catch_other = "Other"
    )
  ) %>%
  group_by(catch) %>%
  summarise(
    n = sum(selected == 1, na.rm = TRUE),
    total = sum(selected %in% c(0, 1)),
    proportion = n / total,
    percentage = round(100 * proportion, 1),
    .groups = "drop"
  )

catch_summary

#24. If sold.. Price 
#Check for unique values in this column 

unique(community_survey$fish_price_zar_kg)

# Remove NA and 0 values
fish_price_clean <- community_survey %>%
  filter(!is.na(fish_price_zar_kg),
         fish_price_zar_kg > 0)

# Number of respondents who provided a price
n_responses <- nrow(fish_price_clean)

# Descriptive statistics
fish_price_summary <- fish_price_clean %>%
  summarise(
    respondents = n(),
    mean_price = mean(fish_price_zar_kg),
    median_price = median(fish_price_zar_kg),
    min_price = min(fish_price_zar_kg),
    max_price = max(fish_price_zar_kg),
    sd_price = sd(fish_price_zar_kg)
  )

fish_price_summary


#25. What fish have you captured 


species_columns <- c(
  "catch_ctenopharyngodon_idella",
  "catch_cyprinus_carpio",
  "catch_hypophthalmichthys_molitrix",
  "catch_lepomis_macrochirus",
  "catch_micropterus_dolomieu",
  "catch_micropterus_floridanus_salmoides",
  "catch_micropterus_punctulatus",
  "catch_oreochromis_niloticus",
  "catch_perca_fluviatilis",
  "catch_pterygoplichthys_disjunctivus",
  "catch_tinca_tinca",
  "catch_cherax_quadricarinatus"
)

#Proportions for a summary table 

species_summary <- community_survey %>%
  select(community, all_of(species_columns)) %>%
  pivot_longer(
    cols = all_of(species_columns),
    names_to = "species",
    values_to = "response"
  ) %>%
  mutate(
    response = trimws(as.character(response)),
    response = na_if(response, "")
  ) %>%
  filter(
    !is.na(response),
    response %in% c("Never", ">1", ">5", ">10", ">20")
  ) %>%
  mutate(
    response = factor(
      response,
      levels = c("Never", ">1", ">5", ">10", ">20")
    ),
    
    species = recode(
      species,
      catch_ctenopharyngodon_idella = "Ctenopharyngodon idella",
      catch_cyprinus_carpio = "Cyprinus carpio",
      catch_hypophthalmichthys_molitrix = "Hypophthalmichthys molitrix",
      catch_lepomis_macrochirus = "Lepomis macrochirus",
      catch_micropterus_dolomieu = "Micropterus dolomieu",
      catch_micropterus_floridanus_salmoides = "Micropterus floridanus salmoides",
      catch_micropterus_punctulatus = "Micropterus punctulatus",
      catch_oreochromis_niloticus = "Oreochromis niloticus",
      catch_perca_fluviatilis = "Perca fluviatilis",
      catch_pterygoplichthys_disjunctivus = "Pterygoplichthys disjunctivus",
      catch_tinca_tinca = "Tinca tinca",
      catch_cherax_quadricarinatus = "Cherax quadricarinatus"
    )
  ) %>%
  count(community, species, response) %>%
  group_by(community, species) %>%
  mutate(
    total = sum(n),
    proportion = n / total,
    percentage = round(proportion * 100, 1)
  ) %>%
  ungroup()

#Order species for figure 

# Set species order
species_order <- c(
  "Cherax quadricarinatus",
  "Oreochromis niloticus",
  "Lepomis macrochirus",
  "Hypophthalmichthys molitrix",
  "Micropterus floridanus salmoides",
  "Micropterus punctulatus",
  "Cyprinus carpio",
  "Ctenopharyngodon idella",
  "Micropterus dolomieu",
  "Perca fluviatilis",
  "Tinca tinca",
  "Pterygoplichthys disjunctivus"
)

species_summary <- species_summary %>%
  mutate(
    species = factor(
      species,
      levels = rev(species_order)
    ),
    response = factor(
      response,
      levels = c("Never", ">1", ">5", ">10", ">20")
    )
  )
#Create a facet wrapped horizontal bar graph with 
#italicized scientific names 

ggplot(
  species_summary,
  aes(
    x = proportion,
    y = species,
    fill = response
  )
) +
  
# All fish species
  geom_col(
    data = species_summary %>%
      filter(species != "Cherax quadricarinatus"),
    width = 0.75,
    color = "black",
    linewidth = 0.2
  ) +
  
# Crayfish only - thicker outline
  geom_col(
    data = species_summary %>%
      filter(species == "Cherax quadricarinatus"),
    width = 0.75,
    color = "black",
    linewidth = 0.8
  ) +
  
  facet_wrap(
    ~ community,
    ncol = 2
  ) +
  
  scale_fill_manual(
    values = c(
      "Never" = "white",
      ">1" = "#D9D9D9",
      ">5" = "#A6BDD7",
      ">10" = "#3690C0",
      ">20"   = "#034e7b"
    )
  ) +
  
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1),
    breaks = seq(0, 1, 0.25),
    expand = c(0, 0)
  ) +
  
# Scientific names italicized, with dagger denotation
  scale_y_discrete(
    labels = function(x) {
      labels <- paste0("italic('", x, "')")
      
      labels[x == "Cherax quadricarinatus"] <-
        "italic('Cherax quadricarinatus')~'\u2020'"
      
      parse(text = labels)
    }
  ) +
  
  labs(
    title = "Reported Catch Frequency\nby Species and Community",
    x = "Percentage of Respondents",
    y = NULL,
    fill = "Reported Catch", 
    caption ="\u2020 Crustacean"
  ) +
  
  theme_bw() +
  
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    ),
    
    strip.background = element_rect(
      fill = "grey85",
      color = "grey40"
    ),
    
    strip.text = element_text(
      face = "bold"
    ),
    
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    
# More space between left and right facets
    panel.spacing.x = unit(1, "cm"),
    panel.spacing.y = unit(0.3, "cm"),
    
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(face = "bold"),
    
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 9),
    
    plot.margin = margin(10, 15, 10, 15)
  ) +
  
  guides(
    fill = guide_legend(
      title.position = "left",
      title.hjust = 0.5,
      nrow = 1,
      override.aes = list(
        colour = "black",
        linewidth = 0.2
      )
    )
  )

#-----------Section D: Knowledge of Invasive Species and acceptability of Management---------# 
#Analysis 

#26. First time saw crayfish 

#Check for unique entries 

unique(community_survey$first_saw_crayfish)

#Propotions across all communities and add 

community_survey$first_saw_crayfish <- case_when(
  trimws(community_survey$first_saw_crayfish) == "" ~ NA_character_,
  community_survey$first_saw_crayfish %in% c("long", "Long time ago") ~ "Other",
  TRUE ~ community_survey$first_saw_crayfish
)

first_saw_summary <- community_survey %>%
  filter(!is.na(first_saw_crayfish)) %>%
  count(first_saw_crayfish) %>%
  mutate(
    proportion = n / sum(n),
    percentage = round(proportion * 100, 1)
  )

first_saw_summary

#Proportions by community 

first_saw_community <- community_survey %>%
  filter(!is.na(first_saw_crayfish)) %>%
  count(community, first_saw_crayfish) %>%
  group_by(community) %>%
  mutate(
    proportion = n / sum(n),
    percentage = round(proportion * 100, 1)
  ) %>%
  ungroup()

first_saw_community
#27. Where seen crayfish

#Check for unique responses 
unique(community_survey$where_saw_crayfish)

# Clean responses
community_survey$where_saw_crayfish <- case_when(
  trimws(community_survey$where_saw_crayfish) %in% c("", "Never", "Never ") ~ NA_character_,
  
  # Remove multiple responses
  grepl("/", community_survey$where_saw_crayfish) ~ NA_character_,
  
  # Standardize wording
  trimws(community_survey$where_saw_crayfish) == "Someone I know consumed it" ~
    "Someone I know consumed it",
  
  trimws(community_survey$where_saw_crayfish) == "In the wild" |
    trimws(community_survey$where_saw_crayfish) == "in the wild" ~
    "In the wild",
  
  trimws(community_survey$where_saw_crayfish) == "Local Market" ~
    "Local Market",
  
  trimws(community_survey$where_saw_crayfish) == "Aquarium" ~
    "Aquarium",
  
  trimws(community_survey$where_saw_crayfish) == "Someone had it as a pet" ~
    "Someone had it as a pet",
  
  # Everything else
  TRUE ~ "Other"
)

#Check for proportions 
where_saw_summary <- community_survey %>%
  mutate(
    where_saw_crayfish = factor(
      where_saw_crayfish,
      levels = c(
        "Local Market",
        "Someone I know consumed it",
        "In the wild",
        "Aquarium",
        "Someone had it as a pet",
        "Other"
      )
    )
  ) %>%
  filter(!is.na(where_saw_crayfish)) %>%
  count(community, where_saw_crayfish) %>%
  mutate(
    proportion = n / sum(n),
    percentage = round(100 * proportion, 1)
  )

where_saw_summary

#Create figure side by side 
where_saw_colours <- c(
  "Local Market" = "#4F8577",
  "Someone I know consumed it" = "#5E8C96",
  "In the wild" = "#C49A3A",
  "Aquarium" = "#8C9A62",
  "Someone had it as a pet" = "#657486",
  "Other" = "#9A9188"
)

ggplot(
  where_saw_summary,
  aes(
    x = community,
    y = percentage,
    fill = where_saw_crayfish
  )
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7,
    color = "black",
    linewidth = 0.2
  ) +
  scale_fill_manual(values = where_saw_colours) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    labels = function(x) paste0(x, "%"),
    expand = c(0, 0)
  ) +
  labs(
    x = "Community",
    y = "Proportion of valid responses",
    fill = "Where respondents first saw crayfish"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold")
  )

#28. Heard of term invasive species 
#Clean responses
community_survey$heard_invasive_species <- case_when(
  trimws(community_survey$heard_invasive_species) == "" ~ NA_character_,
  community_survey$heard_invasive_species == "0" ~ "No",
  TRUE ~ trimws(community_survey$heard_invasive_species)
)

#calculate proportions of yes/no

heard_summary <- community_survey %>%
  mutate(
    heard_invasive_species = trimws(heard_invasive_species),
    heard_invasive_species = na_if(heard_invasive_species, "")
  ) %>%
  filter(!is.na(heard_invasive_species)) %>%
  count(heard_invasive_species) %>%
  mutate(
    proportion = n / sum(n),
    percentage = round(100 * proportion, 1)
  )

heard_summary

#29. if yes -> meaning? 

#check for unique responses 

unique(community_survey$invasive_species_meaning)
#How many total responses \
# How many total valid responses
community_survey %>%
  summarise(
    total_rows = n(),
    total_responses = sum(
      !is.na(invasive_species_meaning) &
        trimws(invasive_species_meaning) != "" &
        trimws(invasive_species_meaning) != "N/A"
    ),
    missing_responses = sum(
      is.na(invasive_species_meaning) |
        trimws(invasive_species_meaning) %in% c("", "N/A")
    )
  )

#recode the unique answers to themes 

community_survey$invasive_species_meaning_theme <- case_when(
  
  # Danger / harmful
  trimws(community_survey$invasive_species_meaning) %in% c(
    "danger",
    "Danger",
    "Safety",
    "I am afraid",
    "Things that are at risk",
    "awareness of risks",
    "We are in danger",
    "we are in danger",
    "It is dangerous",
    "danger in our Community (handwritten English)",
    "danger in the Community (handwritten English)",
    "kotsi (danger, Sepedi — written at Q29 despite Q28 ambiguity)",
    "it will be dangerous",
    "danger to the Community",
    "Dangerous",
    "It's scary",
    "It means that it is dangerous",
    "Is dangerous",
    "dangerous",
    "It's bad",
    "Is too danger",
    "is dangerous to our community",
    "Alankabu is Dangerous",
    "It is not good"
  ) ~ "Danger/harmful",
  
  # Don't know
  trimws(community_survey$invasive_species_meaning) %in% c(
    "Don't know",
    "Don't Know",
    "I don't know",
    "No idea",
    "I have no idea",
    "None coz I did not know crayfish is an invasive",
    "None as I don't even know the term invasive",
    "I don't know what it means, all I know is that fish taste good!"
  ) ~ "Don't know",
  
  # Awareness
  trimws(community_survey$invasive_species_meaning) %in% c(
    "Yes, from TV",
    "Awareness",
    "So knowledgable",
    "Heard of alian plants not alian fish"
  ) ~ "Awareness",
  
  # Positive perception
  trimws(community_survey$invasive_species_meaning) %in% c(
    "it is important to have it for the nature",
    "It teaches me good things",
    "we can make money",
    "It does not pose a risk to our health"
  ) ~ "Positive Perception",
  
  # Ecological impacts
  trimws(community_survey$invasive_species_meaning) %in% c(
    "they eat other species",
    "they enter into the environment",
    "Not good for natural fish",
    "crayfish cause problems"
  ) ~ "Ecological impacts",
  
  # No response
  is.na(community_survey$invasive_species_meaning) |
    trimws(community_survey$invasive_species_meaning) %in% c("", "N/A") ~
    "No Responses",
  
  # Everything difficult to interpret
  TRUE ~ "Unclear"
)

#check which original responses ended up in each category 

community_survey %>%
  count(
    invasive_species_meaning_theme,
    invasive_species_meaning,
    sort = TRUE
  )

#Overall proportions 

meaning_summary <- community_survey %>%
  count(invasive_species_meaning_theme) %>%
  mutate(
    proportion = n / sum(n),
    percentage = round(proportion * 100, 1)
  ) %>%
  arrange(desc(percentage))

meaning_summary


#30. Invasive species problem?

#check for unique responses 
unique(community_survey$invasive_problem)

#calculate proportions 

invasive_problem_summary <- community_survey %>%
  mutate(
    invasive_problem = trimws(invasive_problem),
    invasive_problem = na_if(invasive_problem, "")
  ) %>%
  filter(!is.na(invasive_problem)) %>%
  count(invasive_problem) %>%
  mutate(
    proportion = n / sum(n),
    percentage = round(100 * proportion, 1)
  )

invasive_problem_summary
#31. Perceptions towards invasive species 

#Summarize and clean likert data 

likert_summary <- community_survey %>%
  pivot_longer(
    cols = starts_with("likert_"),
    names_to = "statement",
    values_to = "response"
  ) %>%
  mutate(
    response = trimws(response),
    response = na_if(response, ""),
    response = as.numeric(response)
  ) %>%
  filter(
    !is.na(response),
    response %in% 1:5
  ) %>%
  mutate(
    statement = recode(
      statement,
      likert_invasive_impact_native =
        "Invasive species negatively impact native species",
      likert_willing_target_invasive =
        "I am willing to target invasive species",
      likert_removing_affects_income =
        "Removing invasive species would affect my income",
      likert_participate_monitoring =
        "I would participate in monitoring invasive species",
      likert_learn_control =
        "I would like to learn how to control invasive species"
    ),
    
    statement = factor(
      statement,
      levels = rev(c(
        "Invasive species negatively impact native species",
        "I am willing to target invasive species",
        "Removing invasive species would affect my income",
        "I would participate in monitoring invasive species",
        "I would like to learn how to control invasive species"
      ))
    ),
    
    response = factor(
      response,
      levels = 1:5,
      labels = c(
        "Strongly disagree",
        "Disagree",
        "Neutral",
        "Agree",
        "Strongly agree"
      )
    )
  ) %>%
  count(
    community,
    statement,
    response,
    name = "count"
  ) %>%
  complete(
    community,
    statement,
    response,
    fill = list(count = 0)
  ) %>%
  group_by(community, statement) %>%
  mutate(
    total_answered = sum(count),
    percentage = count / total_answered
  ) %>%
  ungroup()

likert_summary

#Create a faceted likert graph 
#Faceted Graph 

likert_colours <- c(
  "Strongly disagree" = "#8C3B3B",
  "Disagree"          = "#C47A6A",
  "Neutral"           = "#D9D9D9",
  "Agree"             = "#78A58B",
  "Strongly agree"    = "#356B52"
)

likert_plot <- ggplot(
  likert_summary,
  aes(
    x = statement,
    y = percentage,
    fill = response
  )
) +
  geom_col(
    width = 0.75,
    colour = "white",
    linewidth = 0.25
  ) +
  coord_flip() +
  facet_wrap(
    ~community,
    ncol = 2
  ) +
  scale_fill_manual(
    values = likert_colours,
    drop = FALSE
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1),
    expand = expansion(mult = c(0, 0.03))
  ) +
  labs(
    x = NULL,
    y = "Respondents (%)",
    fill = NULL
  ) +
  theme_classic() +
  theme(
    strip.background = element_rect(
      fill = "grey92",
      colour = "black"
    ),
    strip.text = element_text(
      face = "bold",
      size = 11
    ),
    axis.text.y = element_text(size = 8),
    legend.position = "bottom",
    panel.spacing = unit(1, "lines"),
    panel.spacing.x = unit(1, "cm")
  ) +
  guides(
    fill = guide_legend(
      nrow = 1,
      byrow = TRUE
    )
  )

likert_plot

#Create another likert graph but with non-green/red colours to consider
likert_colours <- c(
  "Strongly disagree" = "#6A4C93",
  "Disagree"          = "#9C89B8",
  "Neutral"           = "#E6E6E6",
  "Agree"             = "#7AA6C2",
  "Strongly agree"    = "#2F6690"
)

likert_colours <- c(
  "Strongly disagree" = "#C65D20",
  "Disagree"          = "#F3A64A",
  "Neutral"           = "#F0F0F0",
  "Agree"             = "#5DA5DA",
  "Strongly agree"    = "#1B4F8A"
)

likert_colours <- c(
  "Strongly disagree" = "#D17C2F",
  "Disagree"          = "#F2BE7E",
  "Neutral"           = "#E8E8E8",
  "Agree"             = "#7DA7D9",
  "Strongly agree"    = "#2F6DAE"
)

likert_colours <- c(
  "Strongly disagree" = "#C98A4A",
  "Disagree"          = "#E9C69A",
  "Neutral"           = "#E6E6E6",
  "Agree"             = "#9FC5E8",
  "Strongly agree"    = "#4F81BD"
)
#32. Needs for monitoring participation 

#list columns 

need_columns <- c(
  "need_training",
  "need_equipment",
  "need_payment",
  "need_safety",
  "need_permission",
  "need_community_leadership",
  "need_other"
)

#remove respondents who selected more than three options 
need_data <- community_survey %>%
  mutate(
    total_selected = rowSums(
      select(., all_of(need_columns)),
      na.rm = TRUE
    )
  ) %>%
  filter(total_selected <= 3)

#summary across all communities 

need_summary <- need_data %>%
  select(all_of(need_columns)) %>%
  pivot_longer(
    everything(),
    names_to = "need",
    values_to = "selected"
  ) %>%
  mutate(
    selected = as.numeric(selected),
    need = recode(
      need,
      need_training = "Training",
      need_equipment = "Equipment",
      need_payment = "Payment",
      need_safety = "Safety",
      need_permission = "Permission",
      need_community_leadership = "Community leadership",
      need_other = "Other"
    )
  ) %>%
  group_by(need) %>%
  summarise(
    n = sum(selected == 1, na.rm = TRUE),
    total = sum(selected %in% c(0, 1)),
    proportion = n / total,
    percentage = round(100 * proportion, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(percentage))

need_summary

#Summary by community 
need_community <- need_data %>%
  select(community, all_of(need_columns)) %>%
  pivot_longer(
    cols = all_of(need_columns),
    names_to = "need",
    values_to = "selected"
  ) %>%
  mutate(
    selected = as.numeric(selected),
    need = recode(
      need,
      need_training = "Training",
      need_equipment = "Equipment",
      need_payment = "Payment",
      need_safety = "Safety",
      need_permission = "Permission",
      need_community_leadership = "Community leadership",
      need_other = "Other"
    )
  ) %>%
  group_by(community, need) %>%
  summarise(
    n = sum(selected == 1, na.rm = TRUE),
    total = sum(selected %in% c(0, 1)),
    proportion = n / total,
    percentage = round(100 * proportion, 1),
    .groups = "drop"
  )

need_community

#create a side by side horizontal figure 
need_order <- need_summary %>%
  arrange(desc(percentage)) %>%
  pull(need)

need_community$need <- factor(
  need_community$need,
  levels = rev(need_order)
)

community_colours <- c(
  "Belfast"   = "#4C78A8",
  "Dumphries" = "#7AA974",
  "Masetoni"  = "#D37261",
  "Matsulu"   = "#8F77B5",
  "Selwana"   = "#D8A031",
  "Sgagule"   = "#5DA5A4"
)

ggplot(
  need_community,
  aes(
    x = percentage,
    y = need,
    fill = community
  )
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7,
    color = "black",
    linewidth = 0.2
  ) +
  scale_fill_manual(values = community_colours) +
  scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    labels = function(x) paste0(x, "%"),
    expand = c(0, 0)
  ) +
  labs(
    x = "Proportion of valid responses",
    y = "Requirements for participation",
    fill = "Community"
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold")
  )
#33. Concerns for participation 

#select columns 

concern_columns <- c(
  "concern_wildlife",
  "concern_personal_safety",
  "concern_compensation",
  "concern_pollution",
  "concern_other"
)

#Summary across all communities 

concern_summary <- community_survey %>%
  select(all_of(concern_columns)) %>%
  pivot_longer(
    everything(),
    names_to = "concern",
    values_to = "selected"
  ) %>%
  mutate(
    selected = as.numeric(selected),
    concern = recode(
      concern,
      concern_wildlife = "Wildlife",
      concern_personal_safety = "Personal safety",
      concern_compensation = "Compensation",
      concern_pollution = "Pollution",
      concern_other = "Other"
    )
  ) %>%
  group_by(concern) %>%
  summarise(
    n = sum(selected == 1, na.rm = TRUE),
    total = sum(selected %in% c(0, 1)),
    proportion = n / total,
    percentage = round(100 * proportion, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(percentage))

concern_summary

#summary by community 

concern_community <- community_survey %>%
  select(community, all_of(concern_columns)) %>%
  pivot_longer(
    cols = all_of(concern_columns),
    names_to = "concern",
    values_to = "selected"
  ) %>%
  mutate(
    selected = as.numeric(selected),
    concern = recode(
      concern,
      concern_wildlife = "Wildlife",
      concern_personal_safety = "Personal safety",
      concern_compensation = "Compensation",
      concern_pollution = "Pollution",
      concern_other = "Other"
    )
  ) %>%
  group_by(community, concern) %>%
  summarise(
    n = sum(selected == 1, na.rm = TRUE),
    total = sum(selected %in% c(0, 1)),
    proportion = n / total,
    percentage = round(100 * proportion, 1),
    .groups = "drop"
  )
concern_community
#Order concerns by importance 

concern_order <- concern_summary %>%
  arrange(desc(percentage)) %>%
  pull(concern)

concern_community$concern <- factor(
  concern_community$concern,
  levels = rev(concern_order)
)

#Create grouped horizontal bar graph 

ggplot(
  concern_community,
  aes(
    x = percentage,
    y = concern,
    fill = community
  )
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7,
    color = "black",
    linewidth = 0.2
  ) +
  scale_fill_manual(values = community_colours) +
  scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    labels = function(x) paste0(x, "%"),
    expand = c(0, 0)
  ) +
  labs(
    x = "Proportion of valid responses",
    y = "Concerns regarding participation",
    fill = "Community"
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold")
  )