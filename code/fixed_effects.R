
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

print("fitting glm for zero dvmt...")
tic()
glm1 = glm(ZeroDVMT ~ HHFAMINC20k + poverty + HhSize + Workers + ZeroVeh + VehPerDriver + 
             D1B + D2A_WRKEMP + D3a + D4b050 + D5ar1k + 
             UA, 
	   data=hh_df1, family=binomial(link = "logit"), weights=hhwgt1);
toc()

broom::tidy(glm1) %>% 
  filter(stringr::str_detect(term, "^UA", negate=TRUE)) %>% 
  arrange(term) %>% 
  print(n=30)
AIC(glm1)

if (DIAGNOSE <- FALSE) {
  ##warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
  glm1.resids <- broom::augment(glm1) %>%
    mutate(p = 1 / (1 + exp(-.fitted)),
           warning = (p > 1-eps) | (p < eps))
  
  arrange(glm1.resids, .fitted) %>%  
    #select(2:5, p, warning) %>% 
    slice(c(1,n()))
}

print("fitting glm for zero dvmt with UA interaction terms...")
tic()
glm1i = glm(ZeroDVMT ~ HHFAMINC20k + poverty + HhSize + Workers + ZeroVeh + VehPerDriver + 
             UA + UA:D1B + UA:D2A_WRKEMP + UA:D3a + UA:D4b050 + UA:D5ar1k,
           data=hh_df1, family=binomial(link = "logit"), weights=hhwgt1);
toc()
summary(glm1i)
AIC(glm1i)

print("fitting glm for dvmt...")
tic()
glm2 = glm(    DVMT ~ HHFAMINC20k + poverty + HhSize + Workers + VehPerDriver + LifeCycle + 
                 D1B + D2A_WRKEMP + D3a + D4b050 + D5ar1k + UA, 
	   data=hh_df2, family=gaussian(link = "log"), weights=hhwgt2);
toc()
#summary(glm2) %>% print()
broom::tidy(glm2) %>% 
  filter(stringr::str_detect(term, "^UA", negate=TRUE)) %>% 
  arrange(term) %>% 
  print(n=30)
AIC(glm2)

print("fitting glm for dvmt with UA interaction terms...")
tic()
glm2i = glm(DVMT ~ HHFAMINC20k + poverty + HhSize + Workers + VehPerDriver + LifeCycle +
              UA + UA:D1B + UA:D2A_WRKEMP + UA:D3a + UA:D4b050 + UA:D5ar1k,
            data=hh_df2, family=gaussian(link = "log"), weights=hhwgt2);
toc()
summary(glm2i)
AIC(glm2i)