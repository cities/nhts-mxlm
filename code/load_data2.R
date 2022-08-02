cleanup <- TRUE
vars_ls <- ls()

if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
}
pacman::p_load(dplyr, foreign, readr, magrittr, stringr, readxl, tidyr)

recode <- dplyr::recode
select <- dplyr::select

poverty_df <-
  read_csv("data/poverty_guideline.csv", comment = "#")

load("data/NHTS/hh.rda")
load("data/NHTS/pp.rda")
load("data/NHTS/dd.rda")
load("data/NHTS/vv.rda")

pp.df <- pp

#data dictionary: http://nhts.ornl.gov/tables09/CodebookBrowser.aspx

hh.sub <- hh %>%
  mutate_if(is.factor, as.character) %>%
  left_join(poverty_df, by = "HHSIZE") %>%
  transmute(
    HOUSEID = HOUSEID,
    HTPPOPDN = as.integer(HTPPOPDN),
    #Urban = ifelse(URBRUR=="01", 1L, 0L),
    
    HHFAMINCVAL = case_when(
      HHFAMINC == '01' ~ (0 + 5000) / 2,
      HHFAMINC == '01' ~ (0 + 5000) / 2,
      HHFAMINC == '02' ~ (5001 + 10000) / 2,
      HHFAMINC == '03' ~ (10001 + 15000) / 2,
      HHFAMINC == '04' ~ (15001 + 20000) / 2,
      HHFAMINC == '05' ~ (20001 + 25000) / 2,
      HHFAMINC == '06' ~ (25001 + 30000) / 2,
      HHFAMINC == '07' ~ (30001 + 35000) / 2,
      HHFAMINC == '08' ~ (35001 + 40000) / 2,
      HHFAMINC == '09' ~ (40001 + 45000) / 2,
      HHFAMINC == '10' ~ (45001 + 50000) / 2,
      HHFAMINC == '11' ~ (50001 + 55000) / 2,
      HHFAMINC == '12' ~ (55001 + 60000) / 2,
      HHFAMINC == '13' ~ (60001 + 65000) / 2,
      HHFAMINC == '14' ~ (65001 + 70000) / 2,
      HHFAMINC == '15' ~ (70001 + 75000) / 2,
      HHFAMINC == '16' ~ (75001 + 80000) / 2,
      HHFAMINC == '17' ~ (80001 + 100000) / 2,
      HHFAMINC == '18' ~ (100001 + 150000) / 2,
      TRUE ~ as.numeric(NA),
    ),
    
    HHFAMINC = ifelse(HHFAMINC %in% c("-9", "-8", "-7"), NA, HHFAMINC),
    HHFAMINC10k = case_when(
      #HHFAMINC %in% c("-9", "-8", "-7") ~ NA,
      HHFAMINC %in% c("01", "02") ~ "<$10k",
      HHFAMINC %in% c("03", "04") ~ "$10-20k",
      HHFAMINC %in% c("05", "06") ~ "$20-30k",
      HHFAMINC %in% c("07", "08") ~ "$30-40k",
      HHFAMINC %in% c("09", "10") ~ "$40-50k",
      HHFAMINC %in% c("11", "12") ~ "$50-60k",
      HHFAMINC %in% c("13", "14") ~ "$60-70k",
      HHFAMINC %in% c("15", "16") ~ "$70-80k",
      HHFAMINC == "17" ~ "$80-100k",
      HHFAMINC == "18" ~ ">$100k",
      TRUE ~ HHFAMINC
    ),
    
    HHFAMINC20k = case_when(
      HHFAMINC10k == "<$10k" ~ "<$10k",
      HHFAMINC10k %in% c("$10-20k", "$20-30k") ~ "$10-30k",
      HHFAMINC10k %in% c("$30-40k", "$40-50k") ~ "$30-50k",
      HHFAMINC10k %in% c("$50-60k", "$60-70k") ~ "$50-70k",
      HHFAMINC10k %in% c("$70-80k", "$80-100k") ~ "$70-100k",
      HHFAMINC10k == ">$100k" ~ ">$100k",
      TRUE ~ HHFAMINC10k
    ),
    
    HHFAMINC20k = factor(
      HHFAMINC20k,
      levels = c("<$10k", "$10-30k", "$30-50k", "$50-70k", "$70-100k", ">$100k")
    ),
    
    #Whether households are below poverty line
    poverty = ifelse(HHFAMINCVAL - 2500 < POVERTY_GUIDELINE, 1, 0),
    
    #CENSUS_R = as.character(CENSUS_R),
    CENSUS_R = recode(
      CENSUS_R,
      "01" = "NE",
      "02" = "MW",
      "03" = "S",
      "04" = "W"
    ),
    CENSUS_D = as.character(CENSUS_D),
    CENSUS_D = recode(
      CENSUS_D,
      '01' = 'New England',
      '02' = 'Middle Atlantic',
      '03' = 'East North Central',
      '04' = 'West North Central',
      '05' = 'South Atlantic',
      '06' = 'East South Central',
      '07' = 'West South Central',
      '08' = 'Mountain',
      '09' = 'Pacific'
    ),
    
    #URBAN=as.character(URBAN),
    #URBAN=ifelse(URBAN=="01", "Urban Area", URBAN),
    #URBAN=ifelse(URBAN=="02", "Urban Cluster", URBAN),
    #URBAN=ifelse(URBAN %in% c("Urban Area", "Urban Cluster"), URBAN, "Non-Urban"),
    #Urban=ifelse(URBAN=="01", 1L, 0L),
    
    #LIF_CYC = as.character(
    LIF_CYC = case_when(
      LIF_CYC == "01" ~ "Single",
      LIF_CYC %in% c("02") ~ "Couple w/o children",
      LIF_CYC %in% c("03", "04", "05", "06", "07", "08") ~ "Parents w/ children",
      #LIF_CYC %in% c("03", "04") ~ "w/ children 0-5",
      #LIF_CYC %in% c("05", "06") ~ "w/ children 6-15",
      #LIF_CYC %in% c("07", "08") ~ "w/ children 16-21",
      LIF_CYC %in% c("09", "10") ~ "Empty Nester"
    ),
    
    DRVRCNT,
    HHSIZE,
    NUMADLT,
    WRKCOUNT,
    HHVEHCNT,
    TRAVDAY,
    TDAYDATE,
    WEEKDAY = (as.character(TRAVDAY) %in% c("02", "03", "04", "05", "06")),
    WEEKEND = (as.character(TRAVDAY) %in% c("01", "07")),
    HBHTNRNT = as.character(HBHTNRNT),
    HBHTNRNT = ifelse(HBHTNRNT == "-9", NA, HBHTNRNT),
    HBHTNRNT = as.numeric(HBHTNRNT),
    HBHUR,
    #HBPPOPDN=as.numeric(as.character(HBPPOPDN)),  #home bg renter%, urban/rural, pop density, residential unit density
    #HBRESDN=as.numeric(as.character(HBRESDN)),
    HTHTNRNT = as.character(HTHTNRNT),
    HTHTNRNT = ifelse(HTHTNRNT == "-9", NA, HTHTNRNT),
    HTHTNRNT = as.numeric(HTHTNRNT),
    #HTPPOPDN, HTRESDN=as.numeric(as.character(HTRESDN)), #home tract renter%, emp density, pop density, residential unit density
    #HTEEMPDN=as.numeric(as.character(HTEEMPDN)), HTEEMPDN=ifelse(HTEEMPDN==-9, NA, HTEEMPDN),
    MSACAT,
    MSASIZE,
    RAIL = ifelse(RAIL == "01", 1, 0),
    #MSA category, MSA size, and
    URBAN,
    URBANSIZE,
    URBRUR,
    WTHHFIN,
    #home address in urbanized area, size of urbanize area, urban/rural,
    FLAG100
  )# %>%
#  I() #dplyr::select(-URBAN) #drop the original URBAN variable to avoid confusion; take Urban variable from 2010 BG

hh_pp <- pp %>%
  mutate_if(is.factor, as.character) %>%
  group_by(HOUSEID) %>%
  summarize(
    Age65Plus = sum(R_AGE >= 65),
    Age0to14 = sum(R_AGE < 14),
    DrvAgePop = first(HHSIZE) - Age0to14
    #workers=sum(WORKER=="01"),  #=hh.WRKCOUNT
    #drivers=sum(DRIVER=="01")   #=hh.DRVRCNT
  )

hh.sub <- hh.sub %>% left_join(hh_pp, by = "HOUSEID")

vv %<>%
  mutate_if(is.factor, as.character) %>%
  filter(VEHCOMM == "02") %>%  ## exclude vehicles w/ commercial license
  mutate(
    ANNMILES = ifelse(ANNMILES < 0, NA, ANNMILES),
    BESTMILE = ifelse(BESTMILE < 0, NA, BESTMILE)
  )

# aggregate vehicle vmt to household
hh_vv <- vv %>%  group_by(HOUSEID) %>%
  dplyr::summarize(ANNMILES = sum(ANNMILES, na.rm = T),
                   BESTMILE = sum(BESTMILE, na.rm = T)) %>%
  ungroup()

hh.sub <- hh.sub %>% left_join(hh_vv)

#Is trip time correct? Yes/No
dd %>% group_by(TRIPTIME) %>% summarize(n = n()) %>% mutate('%' = 100 * n /
                                                              sum(n))

dd.sub1 <- dd %>%
  filter(as.character(TRIPTIME) == "01") %>% #only include those with correct trip time
  filter(TRPMILES >= 0, TRVL_MIN >= 0,  as.character(TRIPPURP) != '-9') %>%
  #dplyr::select(HOUSEID, TRIPTIME, TRPMILES, VMT_MILE, TRPTRANS, TRPTRNOS, TRVL_MIN, TRVLCMIN,
  #TRVLHR, TRVLMIN, WAIT_HR, WAIT_MIN, WHYTRP90, WHYTRPSP)
  dplyr::select(HOUSEID,
                TRPTRANS,
                TRPMILES,
                VMT_MILE,
                TRVL_MIN,
                TRVLCMIN,
                TRIPPURP,
                VEHID) %>%
  mutate(HOUSEID = as.character(HOUSEID))

dd.sub1 <- dd.sub1 %>%
  mutate(
    TRPTRANS = as.character(TRPTRANS),
    mode = case_when(
      TRPTRANS %in% c('01', '02', '03', '04', '05', '06', '07') ~ 'Auto',
      TRPTRANS %in% c('09', '10', '11', '12', '13', '14', '15', '16', '17', '18') ~ 'Transit',
      TRPTRANS %in% c('22') ~ 'Bike',
      TRPTRANS %in% c('23') ~ 'Walk',
      TRUE ~ 'Other'
    )
  )

trpmiles.cutoffs <- dd.sub1 %>%
  left_join(vv %>% dplyr::select(HOUSEID, VEHID, VEHCOMM)) %>%
  filter(is.na(VEHCOMM) | as.character(VEHCOMM) == "02") %>%
  group_by(mode) %>%
  summarize(cutoff = quantile(TRPMILES, probs = 0.99))

dd.sub2 <- dd.sub1 %>% right_join(trpmiles.cutoffs) %>%
  filter(TRPMILES <= cutoff) %>%
  dplyr::select(-cutoff)

hh.x.mode_dd <- dd.sub2 %>%
  filter(mode != "Other", TRPMILES >= 0, TRVL_MIN >= 0) %>%
  group_by(HOUSEID, mode) %>%
  summarize(
    td.miles = sum(TRPMILES, na.rm = T),
    tt.mins = sum(TRVL_MIN, na.rm = T),
    ntrips = n()
  ) %>%
  ungroup() %>%
  mutate(atd.miles = td.miles / ntrips)

#compute %td by mode
hh.x.mode_dd <- hh.x.mode_dd %>%
  #filter(td.miles > 0) %>% #not necessary as the minimum td.miles is larger than 0
  group_by(HOUSEID) %>%
  mutate(td.pct = td.miles / sum(td.miles),
         tt.pct = tt.mins / sum(tt.mins)) %>%
  ungroup()

# modes <- data.frame(mode=unique(hh.x.mode_dd$mode), .foo=1, stringsAsFactors=F)
#
# ## create a data frame hh x mode
# hh.x.mode <- hh.x.mode_dd %>%
#   dplyr::distinct(HOUSEID) %>%
#   mutate(.foo=1) %>%
#   inner_join(modes) %>%
#   dplyr::select(-c(.foo))
#
# hh.x.mode %<>% left_join(hh.x.mode_dd, by=c("HOUSEID", "mode")) %>%
#   mutate(td.miles=ifelse(is.na(td.miles), 0, td.miles),
#          td.pct=ifelse(is.na(td.pct), 0, td.pct),
#          tt.mins=ifelse(is.na(tt.mins), 0, tt.mins),
#          tt.pct=ifelse(is.na(tt.pct), 0, tt.pct),
#          ntrips=ifelse(is.na(ntrips), 0, ntrips)
#          )

# confusing renaming to get sane column names
hh_td.miles <-
  hh.x.mode_dd %>% dplyr::select(HOUSEID, value = td.miles, td.miles = mode) %>%
  tidyr::spread(
    key = td.miles,
    value = value,
    fill = 0.0,
    sep = "."
  )

hh_td.pct <-
  hh.x.mode_dd %>% dplyr::select(HOUSEID, value = td.pct, tdpct = mode) %>%
  tidyr::spread(
    key = tdpct,
    value = value,
    fill = 0.0,
    sep = "."
  )

hh_atd.miles <-
  hh.x.mode_dd %>% dplyr::select(HOUSEID, value = atd.miles, atd.miles =
                                   mode) %>%
  tidyr::spread(
    key = atd.miles,
    value = value,
    fill = 0.0,
    sep = "."
  )

hh_ntrips <-
  hh.x.mode_dd %>% dplyr::select(HOUSEID, value = ntrips, ntrips = mode) %>%
  tidyr::spread(
    key = ntrips,
    value = value,
    fill = 0.0,
    sep = "."
  )

hh.sub %<>% left_join(hh_td.miles, by = "HOUSEID")
hh.sub %<>% left_join(hh_td.pct, by = "HOUSEID")
hh.sub %<>% left_join(hh_atd.miles, by = "HOUSEID")
hh.sub %<>% left_join(hh_ntrips, by = "HOUSEID")

# get DVMT from dd
hh_dd <- dd.sub2 %>%
  mutate(VMT_MILE = ifelse(VMT_MILE < 0, 0, VMT_MILE)) %>%
  filter(mode != "Other") %>%
  group_by(HOUSEID) %>%
  summarize(
    DVMT = sum(VMT_MILE, na.rm = T),
    td.miles = sum(TRPMILES, na.rm = T),
    tt.mins = sum(TRVL_MIN, na.rm = T),
    ntrips = n()
  )

hh.sub <- hh.sub %>%
  left_join(hh_dd, by = "HOUSEID") %>%
  mutate(
    DVMT = ifelse(is.na(DVMT) | is.null(DVMT), 0, DVMT),
    td.miles = ifelse(is.na(td.miles) |
                        is.null(td.miles), 0, td.miles),
    tt.mins = ifelse(is.na(tt.mins) |
                       is.null(tt.mins), 0, tt.mins)
  )

## load block group location of households & look up in SLD
hhctbg <-
  read_csv("data/NHTS/CT-BG10v1/hhctbg.csv",
           col_types = cols(HOUSEID = 'c', HHBG10 = 'c'))
head(hhctbg)
hhctbg.geoid <- hhctbg %>%
  filter(HHBG10 != '-9') %>%
  mutate(GEOID10 = paste0(HHSTFIPS10, HHCNTYFP10, HHCT10, HHBG10)) %>%
  dplyr::select(HOUSEID, GEOID10)

sld_df <- read.dbf("data/SLD/SmartLocationDb.dbf", as.is = TRUE)
#data dictionary: https://www.epa.gov/sites/production/files/2014-03/documents/sld_userguide.pdf

## inconsistencies in SLD
##1. denominator for density variables (D1): AC_LAND vs AC_UNPR
### verification of the problem:
###a. in AL (SFIPS=01), AC_LAND is used in the place of AC_UNPR and D1_flag not set
sld_df %>% transmute(
  GEOID10,
  AC_LAND,
  AC_UNPR,
  COUNTHU10,
  HH,
  TOTPOP10,
  EMPTOT,
  D1A,
  D1B,
  D1C,
  D1D,
  D1_flag,
  AC_D1A = COUNTHU10 / D1A,
  AC_D1B = TOTPOP10 / D1B,
  AC_D1C = EMPTOT / D1C,
  AC_D1D = (EMPTOT + COUNTHU10) / D1D
) %>%
  #filter(AC_UNPR==0) %>%
  filter(round(AC_LAND, 0) != round(AC_UNPR, 0)) #%>%
#arrange(randomly(GEOID10)) %>%
#I()

#sld_df %>% mutate()

#rename confusing variable names
sld_df %<>% rename(BGWORKERS = WORKERS)

#code NA from -99999 (NA coding used by NHTS)
sld_df %<>% mutate_all(funs(ifelse(. == -99999, NA, .)))
# sld_df %<>% mutate(D4a=ifelse(D4a == -99999, NA, D4a),
#                 D4c=ifelse(D4c == -99999, NA, D4c),
#                 D4d=ifelse(D4d == -99999, NA, D4d),
#                 D5br=ifelse(D5br == -99999, NA, D5br),
#                 D5be=ifelse(D5be == -99999, NA, D5be),
#                 D5dri=ifelse(D5dri == -99999, NA, D5dri),
#                 D5ce=ifelse(D5ce == -99999, NA, D5ce),
#                 D5de=ifelse(D5de == -99999, NA, D5de),
#                 D5dei=ifelse(D5dei == -99999, NA, D5dei),
#                 D5dr=ifelse(D5dr == -99999, NA, D5dr),
#                 D5dri=ifelse(D5dri == -99999, NA, D5dri)
#   )

#select(COUNTHU10, TOTPOP10, HH, P_WRKAGE, AUTOOWN0, PCT_AO0, AUTOOWN1, PCT_AO1, AUTOOWN2P, PCT_AO2P, WORKERS, R_LOWWAGEW, R_MEDWAGEW, R_HIWAGEWK, R_PCTLOWWA, EMPTOT, E5_RET10, E5_OFF10, E5_IND10, E5_SVC10, E5_ENT10, E8_RET10, E8_OFF10, E8_IND10, E8_SVC10, E8_ENT10, E8_ED10, E8_HLTH10, E8_PUB10, E_FEDT10, E_FEDRET10, E_FEDOFF10, E_FEDIND10, E_FEDSVC10, E_FEDENT10, E_LOWWAGEW, E_MEDWAGEW, E_HIWAGEWK, E_PCTLOWWA, AC_TOT, AC_WATER, AC_LAND, AC_UNPR, D1A, D1B, D1C, D1C5_Ret10, D1C5_Off10, D1C5_Ind10, D1C5_Svc10, D1C5_Ent10, D1C8_Ret10, D1C8_Off10, D1C8_Ind10, D1C8_Svc10, D1C8_Ent10, D1C8_Ed10, D1C8_Hlth1, D1C8_Pub10, D1D, D1_flag, D2A_JPHH, D2B_E5MIX, D2B_E5MIXA, D2B_E8MIX, D2B_E8MIXA, D2A_EPHHM, D2C_TRPMX1, D2C_TRPMX2, D2C_TRIPEQ, D2R_JOBPOP, D2R_WRKEMP, D2A_WRKEMP, D2C_WREMIX, D3a, D3aao, D3amm, D3apo, D3b, D3bao, D3bmm3, D3bmm4, D3bpo3, D3bpo4, D4a, D4b025, D4b050, D4c, D4d, D5ar, D5ae, D5br, D5br_Flag, D5be, D5be_Flag, D5cr, D5cri, D5ce, D5cei, D5dr, D5dri, D5de, D5dei)

#stat.desc(sld_df)
# there are 55 tracts with upr area less than 5 ending up with very high density
# sld_df %>% select(TRPOPDEN, TOTPOP10, TRAREA, TRAC_LAND, TRAC_UNPR) %>% filter(TRAC_UNPR<5 & TRAC_UNPR>0) %>% print(n=55)
# or TODO::simply use AC_LAND for all?

sld_df %<>% group_by(SFIPS, CFIPS, TRFIPS) %>%
  mutate(
    TRAC_UNPR = sum(AC_UNPR),
    TRAC_LAND = sum(AC_LAND),
    TRShape_Area = sum(Shape_Area),
    TRAREA = ifelse(TRAC_UNPR <= 5, TRAC_LAND, TRAC_UNPR),
    TRAREA = ifelse(TRAREA == 0, TRShape_Area, TRAREA),
    TRPOP = sum(TOTPOP10),
    TRHU = sum(COUNTHU10),
    TREMP = sum(EMPTOT),
    TRACT = sum(EMPTOT + COUNTHU10),
    TRE5_ENT10 = sum(E5_ENT10),
    TRE5_IND10 = sum(E5_IND10),
    TRE5_OFF10 = sum(E5_OFF10),
    TRE5_RET10 = sum(E5_RET10),
    TRE5_SVC10 = sum(E5_SVC10),
    TRE8_ED10 = sum(E8_ED10),
    TRE8_ENT10 = sum(E8_ENT10),
    TRE8_HLTH10 = sum(E8_HLTH10),
    TRE8_IND10 = sum(E8_IND10),
    TRE8_OFF10 = sum(E8_OFF10),
    TRE8_PUB10 = sum(E8_PUB10),
    TRE8_RET10 = sum(E8_RET10),
    TRE8_SVC10 = sum(E8_SVC10)
  ) %>%
  ungroup()

hh.sld <- hhctbg.geoid %>% left_join(sld_df, by = "GEOID10")
hh.sld %<>% dplyr::select(
  HOUSEID,
  GEOID10,
  SFIPS,
  CBSA_POP,
  CBSA_EMP,
  CBSA_WRK,
  AC_TOT,
  AC_WATER,
  AC_LAND,
  AC_UNPR,
  COUNTHU10,
  TOTPOP10,
  HH,
  BGWORKERS,
  P_WRKAGE,
  R_LOWWAGEW,
  R_MEDWAGEW,
  R_HIWAGEWK,
  R_PCTLOWWA,
  AUTOOWN0,
  PCT_AO0,
  AUTOOWN1,
  PCT_AO1,
  AUTOOWN2P,
  PCT_AO2P,
  #D1A, D1B, D1C, D1D for tracts
  TRPOP,
  TRHU,
  TREMP,
  TRACT,
  TRAC_UNPR,
  TRAREA,
  #TRPOPDEN, TRHUDEN, TREMPDEN, TRACTDEN calculated in comp_dependencies
  D1A,
  D1B,
  D1C,
  D1C5_Ent10,
  D1C5_Ind10,
  D1C5_Off10,
  D1C5_Ret10,
  D1C5_Svc10,
  D1C8_Ed10,
  D1C8_Ent10,
  D1C8_Hlth1,
  D1C8_Ind10,
  D1C8_Off10,
  D1C8_Pub10,
  D1C8_Ret10,
  D1C8_Svc10,
  D1D,
  D1_flag,
  D2A_EPHHM,
  D2A_JPHH,
  D2A_WRKEMP,
  D2B_E5MIX,
  D2B_E5MIXA,
  D2B_E8MIX,
  D2B_E8MIXA,
  D2C_TRIPEQ,
  D2C_TRPMX1,
  D2C_TRPMX2,
  D2C_WREMIX,
  D2R_JOBPOP,
  D2R_WRKEMP,
  D3a,
  D3aao,
  D3amm,
  D3apo,
  D3b,
  D3bao,
  D3bmm3,
  D3bmm4,
  D3bpo3,
  D3bpo4,
  D4a,
  D4b025,
  D4b050,
  D4c,
  D4d,
  D5ae,
  D5ar,
  D5be,
  D5be_Flag,
  D5br,
  D5br_Flag,
  D5ce,
  D5cei,
  D5cr,
  D5cri,
  D5de,
  D5dei,
  D5dr,
  D5dri,
  E5_ENT10,
  E5_IND10,
  E5_OFF10,
  E5_RET10,
  E5_SVC10,
  EMPTOT
) %>%
  I() #filter(complete.cases(.))

hh.sub2 <- hh.sub %>% left_join(hh.sld, by = "HOUSEID") %>%
  filter(!is.na(GEOID10))

#rm(hh.sld)

##load place types
load("data/PlaceType.Rda")
placetype <- Outputs_df %>%
  #dplyr::select(-c(SFIPS, CBSA, CBSA_Name, HH, EMPTOT, TOTPOP10, E5_RET10, E5_SVC10, D1D, D2A_JPHH, D3amm, D3apo, D4a, D4c, D4d)) %>% #those are loaded from SLD
  transmute(
    GEOID10,
    AreaType = as.character(AreaType),
    # convert from factor to characters
    Diversity1,
    Diversity2,
    LocationType = as.character(LocationType),
    DevelopmentType = as.character(DevelopmentType),
    ACCESS = (2 * EMPTOT_2 * TOTPOP10_5) / (10000 * (EMPTOT_2 + TOTPOP10_5)),
    TOTPOP10_0.25,
    TOTPOP10_1,
    TOTPOP10_10,
    TOTPOP10_15,
    TOTPOP10_2,
    TOTPOP10_5,
    EMPTOT_0.25,
    EMPTOT_1,
    EMPTOT_10,
    EMPTOT_15,
    EMPTOT_2,
    EMPTOT_5
  )
#rm(Outputs_df)

hh.sub2 %<>% left_join(placetype, by = "GEOID10") #, suffix=c("", ".placetype"))
# verification of df join: sum(hh.sub3x$D1D.y != hh.sub3x$D1D.y)

hh.sub3 <- hh.sub2 #%>%  .[-131561,]

# set dataset to be used for later steps
hh_df <- hh.sub3
#rm(hh.sub, hh.sub2, hh.sub3);
#rm(hh_dd, hh_vv, hh_pp, hhctbg, hhctbg.geoid)

dd.sub3 <- dd.sub2 %>%
  left_join(hh_df, by = "HOUSEID") %>%
  filter(mode != "Other")

dd.df <- dd.sub3
#rm(dd.sub1, dd.sub2, dd.sub3)

#rm(vv)
#hh_df %<>% left_join(hh.vmt, by="HOUSEID")

### Urban Area level information ###
## load block-UA/UACE mapping
## http://www2.census.gov/geo/tiger/TIGER2010/TABBLOCK/2010/
bg_file <- "data/Census/tl_2013_us_bg.csv"
## concatenate from state block files
if (!file.exists(bg_file)) {
  for (sid in 1:78) {
    file_name <-
      paste0("data/Census/tl_2013_",
             str_pad(sid, 2, pad = "0"),
             "_tabblock.dbf")
    if (!file.exists(file_name))
      next
    blk <- read.dbf(file_name, as.is = TRUE)
    blk %<>% mutate(bg = substr(GEOID, 1, 12))
    bg <- blk %>%
      group_by(GEOID10 = bg) %>%
      summarize(UR10 = first(UR10),
                UACE10 = first(UACE10)) %>%
      ungroup() %>%
      transmute(GEOID10,
                Urban = ifelse(UR10 == "U", 1L, 0L),
                UACE = UACE10)
    write_csv(bg, bg_file, append = TRUE)
  }
}

bg2uace <-
  read_csv(bg_file, col_names = c("GEOID10", "Urban", "UACE"))

# eliminate duplicate GEOID10 in bg (28 rows)
# TODO: how did they get there?
bg2uace %<>% group_by(GEOID10) %>% filter(row_number() == 1)

### compute UA level variables
# sld_df.sub <- sld_df %>% select(GEOID10, AC_UNPR, AC_LAND, Shape_Area,
#                           TOTPOP10, COUNTHU10, COUNTHU10)
ua_df <- bg2uace %>% left_join(sld_df, by = "GEOID10") %>%
  group_by(UACE) %>%
  summarize(
    UZAAC_UNPR = sum(AC_UNPR),
    UZAAC_LAND = sum(AC_LAND),
    UZAShape_Area = sum(Shape_Area),
    UZAAREA = ifelse(UZAAC_UNPR <= 5, UZAAC_LAND, UZAAC_UNPR),
    UZAAREA = ifelse(UZAAREA == 0, UZAShape_Area, UZAAREA),
    UZAPOP = sum(TOTPOP10),
    UZAPOPDEN = UZAPOP / UZAAREA,
    UZAHUDEN = sum(COUNTHU10) / UZAAREA,
    UZAEMPDEN = sum(EMPTOT) / UZAAREA,
    UZAACTDEN = sum(EMPTOT + COUNTHU10) / UZAAREA
  ) %>%
  ungroup()

##load UMR data (freeway lane miles, transit miles, UZA population)
## UMR is a secondary data source, opted to load directly from HPMS and NTD instead
#umr <- read_csv("data/TTI/UMRdata.csv")

#umr2010 <- umr %>% filter(is.na(str_match(UZAsState, "Area Average"))) %>%
#  filter(!is.na(UZAsState)) %>%
#  filter(Year==2010)

#load UA names
#UA <- read_excel("data/Census/ua_list_ua.xls")
#UA <- read_csv("data/Census/ua_list_ua.csv")
## https://www.census.gov/geo/reference/ua/ualists_layout.html
ua_census <- read_fwf(
  "data/Census/ua_list_all.txt",
  col_positions = fwf_positions(
    start = c(1, 11, 76, 90, 104, 123, 137, 156, 170, 184),
    end =  c(5, 70, 84, 98, 117, 131, 150, 164, 178, 185),
    col_names = c(
      "UACE",
      "NAME",
      "POP",
      "HU",
      "AREALAND",
      "AREALANDSQMI",
      "AREAWATER",
      "AREAWATERSQMI",
      "POPDEN",
      "LSADC"
    )
  ),
  skip = 1
)

ua_census <- ua_census %>%
  rename(UA_NAME = NAME) %>%
  dplyr::select(UACE, UA_NAME, LSADC)

ua_df <- ua_df %>% left_join(ua_census, by = "UACE")

# UA1 <- UA %>% mutate(NAME1=str_replace_all(NAME, "--", "-"),
#                NAME1=str_replace_all(NAME1, ",", ""),
#                NAME1 = recode(NAME1,
#                  "Albany-Schenectady NY" = "Albany NY",
#                  "Allentown PA-NJ" = "Allentown-Bethlehem PA-NJ",
#                  "Boise City ID" = "Boise ID",
#                  "Urban Honolulu HI" = "Honolulu HI",
#                  "Indio-Cathedral City CA" = "Indio-Cathedral City-Palm Springs CA",
#                  "Las Vegas-Henderson NV" = "Las Vegas NV",
#                  "Los Angeles-Long Beach-Anaheim CA" = "Los Angeles-Long Beach-Santa Ana CA",
#                  "Louisville/Jefferson County KY-IN" = "Louisville KY-IN",
#                  "Minneapolis-St. Paul MN-WI" = "Minneapolis-St. Paul MN",
#                  "Poughkeepsie-Newburgh NY-NJ" = "Poughkeepsie-Newburgh NY",
#                  "Raleigh NC" = "Raleigh-Durham NC",
#                  "Salt Lake City-West Valley City UT" = "Salt Lake City UT",
#                  "Spokane WA" = "Spokane WA-ID"
#                 )
#                )
#
# umr2010 %<>% left_join(UA1 %>% dplyr::select(NAME1, UACE), by=c("UZAsState"="NAME1"))

#bg2uace %<>% left_join(umr2010 %>% dplyr::select(UACE, UZAPOP=POP, UZAFWLM=FWLM, UZAASLM=ASLM), by="UACE")
#define metro==1 if the UA of a bg is included in the UMR data (UMR only include data for 101 UZAs)
#bg2uace %<>% mutate(metro=ifelse(is.na(UZAPOP), "non_metro", "metro"))

#read transit service variables
#transit_VRM <- read_excel("data/NTD/TS2.2TimeSeriesSysWideOpexpSvc.xls", sheet="VRM", col_names=T)
transit <-
  read_excel(
    "data/Brian.Gregor/Transit Service Levels (Miles and Hours) - Comparison Regions.xlsx",
    sheet = "UZA Totals-20",
    col_names = T,
    skip = 2
  )

#fix ua names to be consistent with those in NTD
ua_census <-
  ua_census %>% mutate(
    UA_NAME2 = str_replace_all(UA_NAME, "--", "-"),
    UA_NAME2 = dplyr::recode(
      UA_NAME2,
      "Aberdeen-Bel Air South-Bel Air North, MD" =
        "Aberdeen-Havre de Grace-Bel Air, MD",
      "Albany-Schenectady, NY" = "Albany, NY",
      "Allentown, PA-NJ" = "Allentown-Bethlehem, PA-NJ",
      "Augusta-Richmond County, GA--SC"  = "Augusta-Richmond County, GA-SC",
      "Benton Harbor-St. Joseph-Fair Plain, MI" = "Benton Harbor-St. Joseph, MI",
      "Bonita Springs, FL" = "Bonita Springs-Naples, FL",
      #"Brooksville, FL"
      "Cumberland, MD-WV-PA" = "Cumberland, MD--WV",
      #"Danville, IL", ##??
      "Palm Coast-Daytona Beach-Port Orange, FL" = "Daytona Beach-Port Orange, FL",
      "El Centro-Calexico, CA" = "El Centro, CA",
      "Fayetteville-Springdale-Rogers, AR-MO" = "Fayetteville-Springdale, AR",
      "Fort Walton Beach-Navarre-Wright, FL" = "Fort Walton Beach, FL",
      #Galveston, TX,
      "Gulfport, MS" = "Gulfport-Biloxi, MS",
      "Urban Honolulu, HI" = "Honolulu, HI",
      "Indio-Cathedral City, CA" = "Indio-Cathedral City-Palm Springs, CA",
      "Kennewick-Pasco, WA" = "Kennewick-Richland, WA",
      "Kenosha, WI-IL" = "Kenosha, WI",
      "Las Vegas-Henderson, NV" = "Las Vegas, NV",
      "Leesburg-Eustis-Tavares, FL" = "Leesburg-Eustis, FL",
      "Los Angeles-Long Beach-Anaheim, CA" = "Los Angeles-Long Beach-Santa Ana, CA",
      "Louisville/Jefferson County, KY-IN" = "Louisville, KY-IN",
      "Minneapolis-St. Paul, MN-WI" = "Minneapolis-St. Paul, MN",
      "Monessen-California, PA" = "Monessen, PA",
      "Myrtle Beach-Socastee, SC-NC" = "Myrtle Beach, SC",
      "North Port-Port Charlotte, FL" = "North Port-Punta Gorda, FL",
      "Norwich-New London, CT-RI" = "Norwich-New London, CT",
      "Poughkeepsie-Newburgh, NY-NJ" = "Poughkeepsie-Newburgh, NY",
      "Raleigh-Durham, NC" = "Raleigh, NC",
      "Reno, NV-CA" = "Reno, NV",
      #"Sandusky, OH"
      "Salt Lake City-West Valley City, UT" = "Salt Lake City, UT",
      "Seaside-Monterey, CA" = "Seaside-Monterey-Marina, CA",
      "South Lyon-Howell, MI" = "South Lyon-Howell-Brighton, MI",
      "Spokane, WA" = "Spokane, WA-ID",
      #"St. Charles, MD"
      "Sebastian-Vero Beach South-Florida Ridge, FL" = "Vero Beach-Sebastian, FL",
      "Victorville-Hesperia, CA" = "Victorville-Hesperia-Apple Valley, CA",
      "Westminster-Eldersburg, MD" = "Westminster, MD"
    )
  )

transit.sub <- transit %>%
  dplyr::select(
    UZA,
    UZAVehOp = VehOp,
    UZAVehAv = VehAv,
    UZAAVRM = AVRM,
    UZAAVRH = AVRH,
    UZAULPT = ULPT,
    UZAPM = PM,
    UZAAVRHPC = AVRHPC
  ) %>%
  left_join(ua_census %>% dplyr::select(UA_NAME2, UACE),
            by = c("UZA" = "UA_NAME2")) %>%
  filter(!is.na(UACE))
#left_join(umr2010 %>% dplyr::select(UACE))  ## somehow there is no data for Boulder, CO (UACE 09298) in Transit DB

ua_df <- ua_df %>% left_join(transit.sub, by = "UACE")

# append data from HPMS (freeway lane miles in HM-72, 2008 data)
source("code/load_hpms.R")
hm72 <- hm72 %>%
  filter(!is.na(UACE)) %>%
  dplyr::select(UACE, UZAFWLM = freeway_lane_miles)

ua_df <- ua_df %>% left_join(hm72, by = "UACE")

# bg <- bg %>% left_join(hm72, by="UACE")
# bg %<>% left_join(transit.sub, by="UACE")
# bg %<>% left_join(ua, by="UACE")
# bg <- bg %>% left_join(UA %>% dplyr::select(UACE, UZA_name=NAME), by="UACE")
# # define metro==1 if a bg is in a UA with transit & freeway lane miles data
# bg %<>% mutate(metro=ifelse(is.na(UZAAVRM) | is.na(UZAFWLM), "non_metro", "metro"))

# rename variables to what Brian Gregor used
#bg %<>% mutate(Tranmilescap=UZAAVRM/UZAPOP,
#               Fwylnmicap=UZAFWLM/UZAPOP
#               #Tranmilescap=ifelse(is.na(Tranmilescap), 0, Tranmilescap),  # fill NAs in Tranmilecap (non-metro) with 0s
#               #Fwylnmicap=ifelse(is.na(Fwylnmicap), 0, Fwylnmicap)
#               )

hh_df %<>% left_join(bg2uace, by = "GEOID10") %>%
  left_join(ua_df, by = "UACE") %>%
  mutate(metro = ifelse(is.na(UZAAVRM) |
                          is.na(UZAFWLM), "non_metro", "metro"))

#rm(placetype, bg, umr, umr2010, transit, UA, UA1, UA2, transit.sub)

#hh_df %<>% mutate(
#  AADVMT=BESTMILE/365,
#  lnBESTMILE=log1p(BESTMILE) #,
#  # n = n(),
#  # tdpct.Auto=(tdpct.Auto * (n-1) + 0.5) / n,  #handle 0s & 1s
#  # tdpct.Transit=(tdpct.Transit * (n-1) + 0.5) / n,
#  # tdpct.Bike=(tdpct.Bike * (n-1) + 0.5) / n,
#  # tdpct.Walk=(tdpct.Walk * (n-1) + 0.5) / n,
#  #
#  # or.Transit_Auto=tdpct.Transit/tdpct.Auto,
#  # or.Bike_Auto=tdpct.Bike/tdpct.Auto,
#  # or.Walk_Auto=tdpct.Walk/tdpct.Auto
#)

#row.names(hh_df) <- hh_df$HOUSEID

to_keep_ls <- c("hh_df", "pp_df", "sld_df", "ua_df")
if (cleanup) {
  to_del_ls <- setdiff(ls(), vars_ls) %>% setdiff(to_keep_ls)
  rm(list=to_del_ls)
}