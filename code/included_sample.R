library(tidyverse)

label_sample <- . %>% 
  mutate(Sample=ifelse(TRAVDAY %in% c("02", "03", "04", "05", "06") &
                         as.character(FLAG100)=="01" &
                         !is.na(UA) &
                         #HPMS=="Yes" &
                         #NTD=="Yes" &
                         DVMT <= quantile(.$DVMT, .99),
                       "Included", "Excluded"
  )) %>% 
  group_by(UACE) %>% 
  mutate(nhhs=sum(Sample=="Included"),
         Sample=ifelse(Sample=="Included" & nhhs < 100, "Excluded", Sample),
         Sample=factor(Sample, levels=c("Included", "Excluded"))) %>% 
  ungroup()

included_sample <- . %>%
  label_sample() %>% 
  dplyr::filter(Sample=="Included",
         #!is.na(FwyLaneMiP1k), 
         #!is.na(UZAVehOp),
         TRUE)
