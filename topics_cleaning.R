# cleaning unique terms Excel sheet

### Required libraries ###
library(dplyr)
library(stringr)
library(openxlsx)

### Reading data ###
unique_terms <- read.xlsx("unique_terms.xlsx") %>% 
  select(-action)
# Chose to make a separate topics_data data frame to just have a database of topics
#and leave the original data the same

### Cleaning unique topics list ###
Unique_terms <- unique_terms %>% 
  mutate(topic = if_else(str_detect(topic, "(?i)bail"), "bail", topic),
         topic = if_else(str_detect(topic, "(?i)arrest"), "arrest", topic),
         topic = if_else(str_detect(topic, "(?i)body cam"), "body cameras", topic),
         topic = if_else(str_detect(topic, "\\bcertification\\b"), "certification", topic),
         topic = if_else(str_detect(topic, "(?i)civil liab"), "civil liability", topic),
         topic = if_else(str_detect(topic, "(?i)criminal liab"), "criminal liability", topic),
         topic = if_else(str_detect(topic, "(?i)data|(?i)data coll"), "data collection", topic),
         topic = if_else(str_detect(topic, "^de\\w*ification$"), "decertification", topic),
         topic = if_else(str_detect(topic, "biometric"), "biometric data", topic),
         topic = if_else(str_detect(topic, "^dis\\w*ity$"), "disability", topic),
         topic = if_else(str_detect(topic, "^disc\\w*ine$"), "discipline", topic),
         topic = if_else(str_detect(topic, "disciplinary|discipinary"), "disciplinary record disclosure", topic),
         topic = if_else(str_detect(topic, "duty to reprot"), "duty to report", topic),
         topic = if_else(str_detect(topic, "firearm|fire arms"), "firearms", topic),
         topic = if_else(str_detect(topic, "hiring"), "hiring", topic),
         topic = if_else(str_detect(topic, "indigenous"), "indigenous affairs", topic),
         topic = if_else(str_detect(topic, "prosecution of police|proseuction|prosecution"), "investigation/prosecution of police", topic),
         topic = if_else(str_detect(topic, "knock"), "no knock warrants", topic),
         topic = if_else(str_detect(topic, "oversight"), "oversight", topic),
         topic = if_else(str_detect(topic, "^p\\w*nel"), "personnel management", topic),
         topic = if_else(str_detect(topic, "ice in school"), "police in schools", topic),
         topic = if_else(str_detect(topic, "protocal"), "protocol", topic),
         topic = if_else(str_detect(topic, "immunity"), "qualified immunity", topic),
         topic = if_else(str_detect(topic, "profil"), "racial profiling", topic),
         topic = if_else(str_detect(topic, "sexual as"), "sexual assault", topic),
         topic = if_else(str_detect(topic, "offender"), "sex offenders", topic),
         topic = if_else(str_detect(topic, "standards"), "standards", topic),
         topic = if_else(str_detect(topic, "study com"), "study commission", topic),
         topic = if_else(str_detect(topic, "training"), "training", topic),
         topic = if_else(str_detect(topic, "use of"), "use of force", topic),
         topic = if_else(str_detect(topic, "search"), "search warrants", topic),
         topic = if_else(str_detect(topic, "youth prog"), "youth programs", topic)) %>% 
  # Consolidating topics
  na.omit() %>% 
  arrange(topic) %>% 
  # arrange in alphabetical order
  group_by(topic) %>% 
  mutate(count = sum(count)) %>% 
  distinct() %>% 
  ungroup()
  # updating/consolidating count column

### Cleaning policing_data typos?
policing_data$Topic <- 
  str_replace(policing_data$Topic, "\\bbody cam\\b", "body cameras") 
  str_replace(policing_data$Topic, "\\bliablity\\b", "liability")
  
write.csv(topics_data, "unique_terms", row.names = FALSE)
