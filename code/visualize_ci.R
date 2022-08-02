library(tidyverse)
library(broom)
library(stringr)
library(ggthemes)

# ref: https://www.quora.com/How-do-you-graph-a-confidence-interval-in-ggplot2-R

visualize_ci <- function(glm_obj, var_name="") {
  n_by_UA <- glm_obj$data %>% 
    group_by(UA) %>% 
    tally()
  
  fixed_coef <- tidy(glm_obj) %>% 
    filter(stringr::str_detect(term, "^UA")) %>% 
    separate(term, c("UA", "variable"), sep=":") %>% 
    mutate(variable=ifelse(is.na(variable), "Intercept", variable),
           UA=str_remove(UA, "^UA")) %>% 
    filter(std.error<100) 
  # exclude D4b050 coefficients with large std.error due to low/no variation
  # in the value of D4b050 (mostly/all 0)
  
  t.95 <- qt(0.975, df.residual(glm_obj) - 1)
  fixed_coef <- fixed_coef %>%  
    mutate(moe=std.error*t.95) %>% 
    left_join(n_by_UA, by="UA")
  
  ggplot(fixed_coef %>% filter(variable == var_name), 
         aes(x=fct_reorder(UA, estimate, .desc = TRUE), 
             y=estimate, group=1)) +
    geom_line() +
    geom_errorbar(width=.1, aes(ymin=estimate-moe, ymax=estimate+moe), colour="grey") + 
    labs(x="UZA", y= "coefficient estimate", title=var_name) + 
    coord_flip() +
    theme(axis.text.y = element_blank(), legend.position = "none")
    #theme_fivethirtyeight() #+
    #facet_wrap(~variable, ncol=2)
}

visualize_randeff <- function(glmx_obj, var_name="") {
  rand_coef <- ranef(glmx_obj)$UA %>% 
    rownames_to_column(var="UA") %>% 
    rename(Intercept=`(Intercept)`)
 
  ggplot(rand_coef, 
         aes(x=fct_reorder(UA, .data[[var_name]], .desc = TRUE), 
             y=.data[[var_name]], group=1)) +
    geom_line() +
    labs(x="UZA", y= "Random Effects Coefficient", title=var_name) + 
    coord_flip() +
    theme(axis.text.y = element_blank(), legend.position = "none")
  #theme_fivethirtyeight() #+
  #facet_wrap(~variable, ncol=2)
}

visualize_randelas <- function(glmx_obj, var_name="") {
  rand_coef <- ranef(glmx_obj)$UA %>% 
    rownames_to_column(var="UA") %>% 
    rename(Intercept=`(Intercept)`)

  ggplot(rand_coef, 
         aes(x=fct_reorder(UA, .data[[var_name]], .desc = TRUE), 
             y=.data[[var_name]], group=1)) +
    geom_line() +
    labs(x="UZA", y= "Random Effects Coefficient", title=var_name) + 
    coord_flip() +
    theme(axis.text.y = element_blank(), legend.position = "none")
  #theme_fivethirtyeight() #+
  #facet_wrap(~variable, ncol=2)
}
