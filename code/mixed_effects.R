## This script runs after code/fixed_effects.R. Data are loaded therein.

library(tidyverse)
library(tictoc)
library(lme4)

source('code/comp_dependencies.R')
source('code/rename_variables.R')
source('code/included_sample.R')
source('code/est_models.R')

hh_df0 <- readRDS('output/intermediate/hh_df.rds') %>%
  compute_dependencies() %>%
  rename_variables() %>%
  included_sample()

hh_df1 <- hh_df0 %>%
  select_variables() %>%
  #dplyr::select(ZeroDVMT, DVMT, Drivers, HhSize, LifeCycle, Age0to14, Age65Plus, Workers, HHFAMINC20k, VehPerDriver, poverty,
  #              D1B, D2A_EPHHM, D3a, D3bpo4, D4c, D5ar1k, UA,
  #              hhwgt) %>%
  #mutate(UACE=fct_explicit_na(UACE, na_level = "(Missing)")) %>%
  drop_na() %>%
  droplevels()

hh_df1 %>% group_by(ZeroDVMT) %>% tally() %>% print()
hhwgt1 <- hh_df1 %>% pull(hhwgt)

hh_df2 <- hh_df1 %>% filter(DVMT >0)
hhwgt2 <- hh_df2 %>% pull(hhwgt)

## auto model selection for ZeroDVMT binary logit
## source("code/model_selection.R")

print("fitting base glmm for zero dvmt...")
tic()
glmx1 = glmer(ZeroDVMT ~ HHFAMINC20k + poverty + HhSize + Workers + ZeroVeh + VehPerDriver + 
                D1B + D2A_WRKEMP + D3a + D4b050 + D5ar1k + 
	              (1 | UA), 
	   data=hh_df1, family=binomial(link = "logit"), weights=hhwgt1, nAGQ=0L);
toc()
summary(glmx1) %>% print()

print("fitting full glmm for zero dvmt...")
tic()
glmx1i = glmer(ZeroDVMT ~ HHFAMINC20k + poverty + HhSize + Workers + ZeroVeh + VehPerDriver + 
                D1B + D2A_WRKEMP + D3a + D4b050 + D5ar1k + 
                (1 + D1B + D2A_WRKEMP + D3a + D4b050 + D5ar1k | UA), 
              data=hh_df1, family=binomial(link = "logit"), weights=hhwgt1, nAGQ=0L);
toc()
summary(glmx1i) %>% print()

print("fitting base glmm for dvmt...")
tic()
glmx2 = glmer(DVMT ~ HHFAMINC20k + poverty + HhSize + Workers + LifeCycle + VehPerDriver + 
                D1B + D2A_WRKEMP + D3a + D4b050 + D5ar1k + 
                (1 | UA), 
              data=hh_df2, family=gaussian(link = "log"), weights=hhwgt2, nAGQ=0L);
toc()
summary(glmx2) %>% print()

print("fitting full glmm for dvmt...")
tic()
glmx2i = glmer(DVMT ~ HHFAMINC20k + poverty + HhSize + Workers + LifeCycle + VehPerDriver + 
                D1B + D2A_WRKEMP + D3a + D4b050 + D5ar1k + 
                (1 + D1B + D2A_WRKEMP + D3a + D4b050 + D5ar1k | UA), 
              data=hh_df2, family=gaussian(link = "log"), weights=hhwgt2, nAGQ=0L);
toc()
summary(glmx2i) %>% print()

