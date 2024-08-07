# load 
library(dplyr)
library(tidyr)
library(ggplot2)

setwd('C:/Users/schip/Desktop/dissertation experiments/sizeNoOcclusion/experiments/ExpSize/data')
df_size <- read.csv('jatos_results_data_batch1.csv', sep = ",", header = TRUE)

# tidy 
raw_df_size <- df_size %>%
  dplyr::filter(trial_type=='noisyLetter'& (test_part=='test1' | test_part=='test2')) %>%
  dplyr::select(PROLIFIC_PID, RT, pixel_size_factor, present, correct, confidence, response, presence_key) %>%
  dplyr::rename(subj_id=PROLIFIC_PID,
                size=pixel_size_factor) %>%
  dplyr::mutate(
    RT=as.numeric(RT),
    confidence=as.numeric(confidence),
    present=as.numeric(present),
    response = response == presence_key,
    correct = ifelse(correct == 'true', TRUE, FALSE))

# exclusions
low_accuracy_size <- raw_df_size %>%
  dplyr::group_by(subj_id) %>%
  dplyr::summarise(
    accuracy = mean(correct)) %>%
  dplyr::filter(accuracy<0.5) %>%
  dplyr::pull(subj_id)

too_slow_size <- raw_df_size %>%
  dplyr::group_by(subj_id) %>%
  dplyr::summarise(
    third_quartile_RT = quantile(RT,0.75)) %>%
  dplyr::filter(third_quartile_RT>7000) %>%
  dplyr::pull(subj_id)

too_fast_size <- raw_df_size %>%
  dplyr::group_by(subj_id) %>%
  dplyr::summarise(
    first_quartile_RT = quantile(RT,0.25)) %>%
  dplyr::filter(first_quartile_RT<100) %>%
  dplyr::pull(subj_id)

to_exclude_size <- c(
  low_accuracy_size,
  too_slow_size,
  too_fast_size
) %>% unique()

task_df_size <- raw_df_size %>%
  filter(!(subj_id %in% to_exclude_size))

# DVs in presence-absence comparisons 
response_RT <- task_df_size %>%
    dplyr::filter(RT > 100 & RT < 7000 & correct == TRUE) %>%  
    dplyr::group_by(subj_id, response) %>%
    dplyr::summarise(RT = median(RT)) %>%
    tidyr::pivot_wider(names_from = response, values_from = RT) %>%
    dplyr::mutate(response_RT = `TRUE` - `FALSE`)
t.test(response_RT$response_RT)

response_confidence <- task_df_size %>%
    dplyr::filter(correct == TRUE) %>%
    dplyr::group_by(subj_id, response) %>%
    dplyr::summarise(confidence=mean(confidence)) %>%
    tidyr::pivot_wider(names_from = response, values_from = confidence) %>%
    dplyr::mutate(response_confidence = `TRUE` - `FALSE`)
t.test(response_confidence$response_confidence)

# DVs in target present responses 
RT_size_present <- task_df_size %>%
    dplyr::filter(RT > 100 & RT < 7000 & present == 1 & correct == TRUE) %>%
    dplyr::group_by(subj_id, size) %>%
    dplyr::summarise(RT = median(RT))%>%
    tidyr::pivot_wider(names_from = size, values_from = RT) %>%
    dplyr::mutate(RT_size_present = `5` - `3`)
t.test(RT_size_present$RT_size_present)

conf_size_present <- task_df_size %>%
    dplyr::filter(present == 1 & correct == TRUE) %>%
    dplyr::group_by(subj_id, size) %>%
    dplyr::summarise(confidence = mean(confidence)) %>%
    tidyr::pivot_wider(names_from = size, values_from = confidence) %>%
    dplyr::mutate(conf_size_present = `5`-`3`)

t.test(conf_size_present$conf_size_present)

# DVs in target absent responses 
RT_size_absent <- task_df_size %>%
    dplyr::filter(RT > 100 & RT < 7000 & present == 0 & correct == TRUE) %>%
    dplyr::group_by(subj_id, size) %>%
    dplyr::summarise(RT = median(RT))%>%
    tidyr::pivot_wider(names_from = size, values_from = RT) %>%
    dplyr::mutate(RT_size_absent = `5` - `3`)
t.test(RT_size_absent$RT_size_absent)

conf_size_absent <- task_df_size %>%
    dplyr::filter(present == 0 & correct == TRUE) %>%
    dplyr::group_by(subj_id, size) %>%
    dplyr::summarise(confidence = mean(confidence)) %>%
    tidyr::pivot_wider(names_from = size, values_from = confidence) %>%
    dplyr::mutate(conf_size_absent = `5`-`3`)
t.test(conf_size_absent$conf_size_absent)

# accuracy measures 
accuracy_size <- task_df_size %>%
  dplyr::group_by(subj_id, size) %>%
  dplyr::summarise(
    hit_rate =(sum(correct & present)+0.5)/(sum(present)+1),
    fa_rate = (sum(!correct & !present)+0.5)/(sum(!present)+1),
    dprime = qnorm(hit_rate)-qnorm(fa_rate),
    criterion = -0.5*(qnorm(hit_rate)+qnorm(fa_rate)))

dprime <- accuracy_size %>%
  dplyr::select(subj_id,size,dprime) %>%
  tidyr::pivot_wider(names_from=size, values_from=dprime) %>%
  dplyr::mutate(dprime_size=`5`-`3`)
t.test(dprime$dprime_size)

criterion <- accuracy_size %>%
  dplyr::select(subj_id,size,criterion) %>%
  tidyr::pivot_wider(names_from=size, values_from=criterion) %>%
  dplyr::mutate(criterion_size=`5`-`3`)
t.test(criterion$criterion_size)

# differences in target present vs absent responses 
RT_size_resp <- inner_join(
  RT_size_present,
  RT_size_absent,
  by = "subj_id") %>%
  mutate(RT_size_resp = RT_size_present - RT_size_absent)
t.test(RT_size_resp$RT_size_resp)

conf_size_resp <- inner_join(
  conf_size_present,
  conf_size_absent,
  by = "subj_id") %>%
  dplyr::mutate(conf_size_resp = conf_size_present - conf_size_absent)
t.test(conf_size_resp$conf_size_resp)

# plots 

## RT plot
RT_summary <- task_df_size %>%
  dplyr::filter(correct == TRUE, RT > 100 & RT < 7000) %>%
  dplyr::group_by(subj_id, size, response) %>%
  dplyr::summarise(
    RT = median(RT)
  ) %>%
  dplyr::group_by(size, response) %>%
  dplyr::summarise(
    mean_RT = mean(RT),  # mean of individual level median RTs 
    sd_RT = sd(RT),
    n = n(),
    se_RT = sd_RT / sqrt(n))

(RT_plot <- RT_summary %>%
  dplyr::mutate(response = factor(ifelse(response, 'present','absent'), levels=c('present','absent')),
                size = factor(ifelse(size==5, 'big','small'), levels=c('big','small'))) %>%
  ggplot(aes(x=size,
             y=mean_RT,
             color=response)) +
  geom_errorbar(aes(ymin=mean_RT-se_RT, ymax=mean_RT+se_RT),width=.1, linewidth=.5) +
  geom_line(aes(group = response)) +
  labs(x = "Size", 
       y = "RT (ms)", 
       color = "Target presence") +
  scale_color_manual(values=c("#377eb8", "#E21a1c"))+  
  theme_bw())

# conf plot
conf_summary <- task_df_size %>%
  dplyr::filter(correct==TRUE) %>%
  dplyr::group_by(subj_id, size, response) %>%
  dplyr::summarise(
    confidence=mean(confidence)) %>%
  dplyr::group_by(size,response) %>%
  dplyr::summarise(
    mean_conf = mean(confidence),
    sd_conf = sd(confidence),
    n = n(),
    se_conf = sd_conf/ sqrt(n))

(conf_plot <- conf_summary %>%
  dplyr::mutate(response = factor(ifelse(response, 'present','absent'), levels=c('present','absent')),
                size = factor(ifelse(size==5, 'big','small'), levels=c('big','small'))) %>%
  ggplot(aes(x=size,
             y=mean_conf,
             color=response)) +
  geom_errorbar(aes(ymin=mean_conf-se_conf, ymax=mean_conf+se_conf),width=.1, linewidth=.5) +
  geom_line(aes(group = response)) +
  labs(x = "Size", 
       y = "confidence", 
       color = "Target presence") +
  scale_color_manual(values=c("#377eb8", "#E21a1c"))+  
  theme_bw())

# response
response_descriptives <- task_df_size %>%
  group_by(subj_id,size, present) %>%
  dplyr::summarise(
    response_bias = mean(response==TRUE)) %>% # yes resp by present is basically just hit and fa 
  dplyr::group_by(size,present) %>%
  dplyr::summarise(
    mean_resp = mean(response_bias),
    sd_resp = sd(response_bias),
    n=n(),
    se_resp = sd_resp/ sqrt(n))

(response_plot <- response_descriptives %>%
  dplyr::mutate(present = factor(ifelse(present==1, 'present','absent'), levels=c('present','absent')),
                size = factor(ifelse(size==5, 'big','small'), levels=c('big','small'))) %>%
  ggplot(aes(x=size,
             y=mean_resp,
             color=present)) +
  geom_errorbar(aes(ymin=mean_resp-se_resp, ymax=mean_resp+se_resp),width=.1,linewidth=.5) +
  geom_line(aes(group=present)) + 
  labs(x="Size",
       y="Hit / False alarm (yes response)",
       color = "Target presence") +
  scale_color_manual(values=c("#377eb8", "#E21a1c"))+  
  theme_bw())

# indiv plots
(RT_plot_id <- task_df_size %>%
    dplyr::filter(RT > 100 & RT < 7000 & correct == TRUE) %>%
    dplyr::mutate(response = factor(ifelse(response, 'present','absent'), levels=c('present','absent')),
                  size = factor(ifelse(size==5, 'big','small'), levels=c('big','small'))) %>%
    dplyr::group_by(subj_id, size, response) %>%
    dplyr::summarise(RT = median(RT)) %>%
    ggplot(aes(x=size, y = RT, color = response, group = subj_id)) +
    geom_point() +
    facet_wrap(~response) +
    geom_line() + 
    labs(x = "Size", 
         y = "Median RT (ms)",
         color = "Target presence") +
    scale_color_manual(values = c("absent" = "#E21a1c", "present" = "#377eb8")) +
    theme_bw())

# trying to find the which_was_harder counts from df_size
response_counts <- df_size %>%
  dplyr::group_by(response) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::arrange(desc(count))

print(response_counts)
