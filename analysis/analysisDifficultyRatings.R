setwd('C:/Users/schip/Desktop/dissertation experiments/sizeNoOcclusion/experiments/ExpSize/data')
df_size <- read.csv('jatos_results_data_batch1.csv', sep = ",", header = TRUE)

group_info <- df_size %>%
  dplyr::filter(grepl("which_was_harder", response)) %>%
  dplyr::mutate(
    which_was_harder = str_extract(response, '(?<="which_was_harder":")(.*?)(?=")'),
    group = case_when(
      str_detect(which_was_harder, "smaller") ~ "smaller_harder",
      str_detect(which_was_harder, "bigger") ~ "bigger_harder",
      str_detect(which_was_harder, "no effect") ~ "no_effect",
      TRUE ~ NA_character_
    ))   %>%
  dplyr::select(PROLIFIC_PID, group) %>%
  dplyr::distinct()

df_size_raw <- df_size %>%
  dplyr::left_join(group_info, by = "PROLIFIC_PID") %>%
  dplyr::filter(trial_type=='noisyLetter'& (test_part=='test1' | test_part=='test2')) %>%
  dplyr::select(PROLIFIC_PID, RT, pixel_size_factor, present, correct, confidence, response, presence_key, group) %>%
  dplyr::rename(subj_id=PROLIFIC_PID,
                size=pixel_size_factor) %>%
  dplyr::mutate(
    RT=as.numeric(RT),
    confidence=as.numeric(confidence),
    present=as.numeric(present),
    response = response == presence_key,
    correct = ifelse(correct == 'true', TRUE, FALSE))

low_accuracy_size <- df_size_raw %>%
  dplyr::group_by(subj_id) %>%
  dplyr::summarise(
    accuracy = mean(correct)
  ) %>%
  dplyr::filter(accuracy < 0.5) %>%
  dplyr::pull(subj_id)

too_slow_size <- df_size_raw %>%
  dplyr::group_by(subj_id) %>%
  dplyr::summarise(
    third_quartile_RT = quantile(RT, 0.75)
  ) %>%
  dplyr::filter(third_quartile_RT > 7000) %>%
  dplyr::pull(subj_id)

too_fast_size <- df_size_raw %>%
  dplyr::group_by(subj_id) %>%
  dplyr::summarise(
    first_quartile_RT = quantile(RT, 0.25)
  ) %>%
  dplyr::filter(first_quartile_RT < 100) %>%
  dplyr::pull(subj_id)

to_exclude_size <- c(
  low_accuracy_size,
  too_slow_size,
  too_fast_size
) %>% unique()

task_df_size <- df_size_raw %>%
  dplyr::filter(!(subj_id %in% to_exclude_size))

RT_summary <- task_df_size %>%
  dplyr::filter(group == "smaller_harder" & correct == TRUE, RT > 100 & RT < 7000) %>%
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

RT_size_absent_big <- task_df_size %>%
  dplyr::filter(group == "bigger_harder" & RT > 100 & RT < 7000 & present == 0 & correct == TRUE) %>%
  dplyr::group_by(subj_id, size) %>%
  dplyr::summarise(RT = median(RT))%>%
  tidyr::pivot_wider(names_from = size, values_from = RT) %>%
  dplyr::mutate(RT_size_absent = `5` - `3`)
t.test(RT_size_absent_big$RT_size_absent)

RT_size_absent_small <- task_df_size %>%
  dplyr::filter(group == "smaller_harder" & RT > 100 & RT < 7000 & present == 0 & correct == TRUE) %>%
  dplyr::group_by(subj_id, size) %>%
  dplyr::summarise(RT = median(RT))%>%
  tidyr::pivot_wider(names_from = size, values_from = RT) %>%
  dplyr::mutate(RT_size_absent = `5` - `3`)

RT_size_absent_nodiff <- task_df_size %>%
  dplyr::filter(group == "no_effect" & RT > 100 & RT < 7000 & present == 0 & correct == TRUE) %>%
  dplyr::group_by(subj_id, size) %>%
  dplyr::summarise(RT = median(RT))%>%
  tidyr::pivot_wider(names_from = size, values_from = RT) %>%
  dplyr::mutate(RT_size_absent = `5` - `3`)

t.test(RT_size_absent_big$RT_size_absent, RT_size_absent_small$RT_size_absent) ## Mdiff = 103, p = .03
t.test(RT_size_absent_big$RT_size_absent, RT_size_absent_nodiff$RT_size_absent) ## p = .10
t.test(RT_size_absent_nodiff$RT_size_absent, RT_size_absent_small$RT_size_absent) ## p = .614

RT_size_present_big <- task_df_size %>%
  dplyr::filter(group == "bigger_harder" & RT > 100 & RT < 7000 & present == 1 & correct == TRUE) %>%
  dplyr::group_by(subj_id, size) %>%
  dplyr::summarise(RT = median(RT))%>%
  tidyr::pivot_wider(names_from = size, values_from = RT) %>%
  dplyr::mutate(RT_size_present = `5` - `3`)

RT_size_present_small <- task_df_size %>%
  dplyr::filter(group == "smaller_harder" & RT > 100 & RT < 7000 & present == 1 & correct == TRUE) %>%
  dplyr::group_by(subj_id, size) %>%
  dplyr::summarise(RT = median(RT))%>%
  tidyr::pivot_wider(names_from = size, values_from = RT) %>%
  dplyr::mutate(RT_size_present = `5` - `3`)

RT_size_present_nodiff <- task_df_size %>%
  dplyr::filter(group == "no_effect" & RT > 100 & RT < 7000 & present == 1 & correct == TRUE) %>%
  dplyr::group_by(subj_id, size) %>%
  dplyr::summarise(RT = median(RT))%>%
  tidyr::pivot_wider(names_from = size, values_from = RT) %>%
  dplyr::mutate(RT_size_present = `5` - `3`)

t.test(RT_size_present_big$RT_size_present, RT_size_present_small$RT_size_present) ## p = .249
t.test(RT_size_present_big$RT_size_present, RT_size_present_nodiff$RT_size_present) ## p = .449
t.test(RT_size_present_nodiff$RT_size_present, RT_size_present_small$RT_size_present) ## p = .509

# ---- confidence (but say it in a french accent) ----

conf_size_absent_small <- task_df_size %>%
  dplyr::filter(group == "smaller_harder" & present == 0 & correct == TRUE) %>%
  dplyr::group_by(subj_id, size) %>%
  dplyr::summarise(confidence = mean(confidence)) %>%
  tidyr::pivot_wider(names_from = size, values_from = confidence) %>%
  dplyr::mutate(conf_size_absent = `5`-`3`)
t.test(conf_size_absent_small$conf_size_absent)

conf_size_absent_big <- task_df_size %>%
  dplyr::filter(group == "bigger_harder" & present == 0 & correct == TRUE) %>%
  dplyr::group_by(subj_id, size) %>%
  dplyr::summarise(confidence = mean(confidence)) %>%
  tidyr::pivot_wider(names_from = size, values_from = confidence) %>%
  dplyr::mutate(conf_size_absent = `5`-`3`)
t.test(conf_size_absent_big$conf_size_absent)

conf_size_absent_nodiff <- task_df_size %>%
  dplyr::filter(group == "no_effect" & present == 0 & correct == TRUE) %>%
  dplyr::group_by(subj_id, size) %>%
  dplyr::summarise(confidence = mean(confidence)) %>%
  tidyr::pivot_wider(names_from = size, values_from = confidence) %>%
  dplyr::mutate(conf_size_absent = `5`-`3`)
t.test(conf_size_absent_nodiff$conf_size_absent)

t.test(conf_size_absent_small$conf_size_absent, conf_size_absent_big$conf_size_absent) # p = .185
t.test(conf_size_absent_small$conf_size_absent, conf_size_absent_nodiff$conf_size_absent) # p = .725
t.test(conf_size_absent_big$conf_size_absent, conf_size_absent_nodiff$conf_size_absent) # p = .091

conf_size_present_small <- task_df_size %>%
  dplyr::filter(group == "smaller_harder" & present == 1 & correct == TRUE) %>%
  dplyr::group_by(subj_id, size) %>%
  dplyr::summarise(confidence = mean(confidence)) %>%
  tidyr::pivot_wider(names_from = size, values_from = confidence) %>%
  dplyr::mutate(conf_size_present = `5`-`3`)

conf_size_present_big <- task_df_size %>%
  dplyr::filter(group == "bigger_harder" & present == 1 & correct == TRUE) %>%
  dplyr::group_by(subj_id, size) %>%
  dplyr::summarise(confidence = mean(confidence)) %>%
  tidyr::pivot_wider(names_from = size, values_from = confidence) %>%
  dplyr::mutate(conf_size_present = `5`-`3`)
t.test(conf_size_present_big$conf_size_present)

conf_size_present_nodiff <- task_df_size %>%
  dplyr::filter(group == "no_effect" & present == 1 & correct == TRUE) %>%
  dplyr::group_by(subj_id, size) %>%
  dplyr::summarise(confidence = mean(confidence)) %>%
  tidyr::pivot_wider(names_from = size, values_from = confidence) %>%
  dplyr::mutate(conf_size_present = `5`-`3`)


conf_summary <- task_df_size %>%
  dplyr::filter(group == "bigger_harder" & correct==TRUE) %>%
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
