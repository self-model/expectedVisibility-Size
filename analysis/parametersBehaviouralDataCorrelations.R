# parameter behaviour correlations (sorry i just gotta check this)
occ_parameters <- read.table('../modelling/modelFitting/bestParameters/occlusion/best_parameters_from_E_occ_intersect_fit.csv', header=FALSE, sep=',') %>%
  mutate(alpha=V8**2,
         belalpha=V9**2,
         gamma = V5,
         diffalpha = belalpha-alpha)

occ_measures <- read.csv('../modelling/data/E_occ_intersect.csv') %>%
  dplyr::group_by(subj_id) %>%
  dplyr::summarise(present_RT_t = t.test(rt[present==1 & correct==1 & rt<7 & easy==1],
                                         rt[present==1 & correct==1 & rt<7 & easy==0])$statistic,
                   absent_RT_t = t.test(rt[present==-1 & correct==1 & rt<7 & easy==1],
                                        rt[present==-1 & correct==1 & rt<7 & easy==0])$statistic,
                   FA = mean(!correct[present==-1 & easy==1])-mean(!correct[present==-1 & easy==0]),
                   misses = mean(!correct[present==1 & easy==1])-mean(!correct[present==1 & easy==0]))

size_parameters <- read.table('../modelling/modelFitting/bestParameters/size/best_parameters_from_E_size_intersect_fit.csv', header=FALSE, sep=',') %>%
  mutate(alpha=V8**2,
         belalpha=V9**2,
         gamma = V5,
         diffalpha = belalpha-alpha) 

size_measures <- read.csv('../modelling/data/E_size_intersect.csv') %>%
  dplyr::group_by(subj_id) %>%
  dplyr::summarise(present_RT_t = t.test(rt[present==1 & correct==1 & rt<7 & easy==1],
                                  rt[present==1 & correct==1 & rt<7 & easy==0])$statistic,
            absent_RT_t = t.test(rt[present==-1 & correct==1 & rt<7 & easy==1],
                                 rt[present==-1 & correct==1 & rt<7 & easy==0])$statistic,
            FA = mean(!correct[present==-1 & easy==1])-mean(!correct[present==-1 & easy==0]),
            misses = mean(!correct[present==1 & easy==1])-mean(!correct[present==1 & easy==0]))

corr_data <- bind_cols(size_parameters, size_measures) %>%
  select(alpha,belalpha,misses,FA,present_RT_t, absent_RT_t)

matrix <- cor(corr_data, use = "complete.obs")

matrix_relevant <- matrix[c("misses", "present_RT_t", "FA", "absent_RT_t"),
                          c("alpha", "belalpha")]

library(corrplot)
ggcorrplot((matrix_relevant), 
           method = "square", 
           type = "full",     
           lab = TRUE, 
           lab_size = 3, 
           colors = c("red", "white"), 
           title = "Correlations between parameters and behavioural data")

library(Hmisc)
rcorr(as.matrix(corr_data), type = "pearson")

# occ
occ.corr_data <- bind_cols(occ_parameters, occ_measures) %>%
  select(alpha,belalpha,misses,FA,present_RT_t, absent_RT_t)

occ.matrix <- cor(occ.corr_data, use = "complete.obs")

occ.matrix_relevant <- occ.matrix[c("misses", "present_RT_t", "FA", "absent_RT_t"),
                                  c("alpha", "belalpha")]

library(corrplot)
ggcorrplot((occ.matrix_relevant), 
           method = "square", 
           type = "full",     
           lab = TRUE, 
           lab_size = 3, 
           colors = c("red", "white"), 
           title = "Correlations between parameters and behavioural data")

library(Hmisc)
rcorr(as.matrix(occ.corr_data), type = "pearson")
