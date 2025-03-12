
library(tidyverse)

df <- haven::read_sas("//Volumes//oecon-forloeb24//Råfiler//pnr_sample.sas7bdat")

df1 <- 
  df %>% 
  filter(!is.na(lonind) & !is.na(p_PERSAMLINKNETRENT_NY)) %>% 
  select(-c(nursery:ant_adiag, V_BMI:V_VAGT)) %>% 
  select(pnr, KOEN, ALDER, lonind, p_PERSAMLINKNETRENT_NY, everything())


load("//Volumes//oecon-forloeb24//Råfiler//Forlob_14032023.rda")


df.lagged <- 
  Forlob_18032022 %>% 
  mutate(ALDER = as.numeric(ALDER)) %>% 
  filter(ALDER >= 18) %>% 
  mutate(person_start = case_when(lag(pnr) == pnr ~ 0, TRUE ~ 1),
         person_slut = case_when(lead(pnr) == pnr ~ 0, TRUE ~ 1)) %>% 
  group_by(pnr) %>%
  mutate(begivenhed = if_else(lag(C_ANTBOERNF) == 0 & C_ANTBOERNF == 1 & person_start == 0, 1, 0),
         periode_slut = case_when(begivenhed == 1 | C_ANTBOERNF == 0 & person_slut == 1 ~ 1, TRUE ~ 0)) %>% 
  slice(1:which.max(periode_slut == 1)) %>% 
  mutate(start = 1:n(),
         slut = start + 1) %>% 
  ungroup() 
  
library(survival)

km.1 <- survfit(Surv(ALDER-17, begivenhed) ~ IE_TYPE, 
                type = "kaplan-meier", 
                data = df.lagged %>% filter(periode_slut == 1)
                )

plot(km.1)

fct_rev(factor(KOEN))


cox.1 <- coxph(Surv(start, slut, begivenhed) ~ IE_TYPE, 
                method = "efron", 
                data = df.lagged)

summary(cox.1)

cox.1.1 <- coxph(Surv(start, slut, begivenhed) ~ KOEN, 
                method = "efron", 
                data = df.lagged)

summary(cox.1.1)

cox.2 <- coxph(Surv(start, slut, begivenhed) ~ strata(IE_TYPE) + KOEN, 
                method = "efron", 
                data = df.lagged)

summary(cox.2)


             coef exp(coef) se(coef)      z Pr(>|z|)    
IE_TYPE2  0.51276   1.66989  0.15161  3.382  0.00072 ***
IE_TYPE3 -0.05044   0.95081  0.14986 -0.337  0.73644 

         coef exp(coef) se(coef)     z Pr(>|z|)    
KOEN2 0.46638   1.59422  0.03041 15.34   <2e-16 ***

             coef exp(coef) se(coef)      z Pr(>|z|)    
IE_TYPE2  0.53609   1.70931  0.15162  3.536 0.000407 ***
IE_TYPE3 -0.04906   0.95212  0.14986 -0.327 0.743380    
KOEN2     0.46723   1.59556  0.03041 15.364  < 2e-16 ***


