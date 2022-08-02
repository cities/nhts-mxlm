filter_hh_df <- . %>% 
  mutate(#TranRevMiP1k = 1000 * Tranmilescap,
         #FwyLaneMiP1k = 1000 * Fwylnmicap,
         HPMS=ifelse(is.na(FwyLaneMiP1k), "No", "Yes"),
         NTD =ifelse(is.na(UZAVehOp),   "No", "Yes"),
         urban=case_when(
           LSADC==75 ~ "Urbanized Area",
           LSADC==76 ~ "Urban Cluster",
           TRUE     ~ "Non-Urban")) %>% 
  filter(TRAVDAY %in% c("02", "03", "04", "05", "06"), 
         as.character(FLAG100)=="01",
	 urban == "Urbanized Area",
         HPMS=="Yes",
         NTD=="Yes",
         DVMT <= quantile(.$DVMT, .99)
         ) %>% 
  group_by(UACE) %>% 
  mutate(nhhs=n()) %>% 
  ungroup() %>% 
  filter(nhhs >= 30)
