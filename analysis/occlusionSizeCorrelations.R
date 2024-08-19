# effect of occlusion on absent RT
occ_RT_abs <- task_df_occ %>%
  dplyr::filter(RT > 100 & RT < 7000 & correct == TRUE) %>%
  dplyr::group_by(subj_id) %>%
  dplyr::summarise(
    occ_RT_abs = median(RT[present == 0 & hide_proportion == 0.10]) - median(RT[present == 0 & hide_proportion == 0.35]) # lets try keep this easy - hard
  )

# effect of size on absent RT
size_RT_abs <- task_df_size %>%
  dplyr::filter(RT > 100 & RT < 7000 & correct == TRUE) %>%
  dplyr::group_by(subj_id) %>%
  dplyr::summarise(
    size_RT_abs = median(RT[present == 0 & size == 5]) - median(RT[present == 0 & size ==3])
  )

# correlation
RT_abs <- occ_RT_abs %>%
  inner_join(size_RT_abs, by = "subj_id")

cor.test(RT_abs$occ_RT_abs, RT_abs$size_RT_abs, type = "pearson")

# present RT
occ_RT_pres <- task_df_occ %>%
  dplyr::filter(RT > 100 & RT < 7000 & correct == TRUE) %>%
  dplyr::group_by(subj_id) %>%
  dplyr::summarise(
    occ_RT_pres = median(RT[present == 1 & hide_proportion == 0.10]) - median(RT[present == 1 & hide_proportion == 0.35]) # lets try keep this easy - hard
  )

size_RT_pres <- task_df_size %>%
  dplyr::filter(RT > 100 & RT < 7000 & correct == TRUE) %>%
  dplyr::group_by(subj_id) %>%
  dplyr::summarise(
    size_RT_pres = median(RT[present == 1 & size == 5]) - median(RT[present == 1 & size ==3])
  )

RT_pres <- occ_RT_pres %>%
  inner_join(size_RT_pres, by = "subj_id")

cor.test(RT_pres$occ_RT_pres, RT_pres$size_RT_pres, type = "pearson")

# absent confidence
occ_conf_abs <- task_df_occ %>%
  dplyr::filter(correct == TRUE) %>%
  group_by(subj_id) %>%
  dplyr::summarise(
    occ_conf_abs = mean(confidence[present == 0 & hide_proportion == 0.10]) - mean(confidence[present == 0 & hide_proportion == 0.35])
  )

size_conf_abs <- task_df_size %>%
  dplyr::filter(correct == TRUE) %>%
  group_by(subj_id) %>%
  dplyr::summarise(
    size_conf_abs = mean(confidence[present == 0 & size == 5]) - mean(confidence[present == 0 & size == 3])
  )

conf_abs <- occ_conf_abs %>%
  inner_join(size_conf_abs, by = "subj_id")

cor.test(conf_abs$occ_conf_abs, conf_abs$size_conf_abs, type = "pearson")

# present confidence 
occ_conf_pres <- task_df_occ %>%
  dplyr::filter(correct == TRUE) %>%
  dplyr::group_by(subj_id) %>%
  dplyr::summarise(
    occ_conf_pres = mean(confidence[present == 1 & hide_proportion == 0.10]) - mean(confidence[present == 1 & hide_proportion == 0.35])
  )

size_conf_pres <- task_df_size %>%
  dplyr::filter(correct == TRUE) %>%
  dplyr::group_by(subj_id) %>%
  dplyr::summarise(
    size_conf_pres = mean(confidence[present == 1 & size == 5]) - mean(confidence[present == 1 & size == 3])
  )

conf_pres <- occ_conf_pres %>%
  inner_join(size_conf_pres, by = "subj_id")

cor.test(conf_pres$occ_conf_pres, conf_pres$size_conf_pres, type = "pearson")

# false alarms 
occ_fa <- task_df_occ %>%
  dplyr::group_by(hide_proportion,subj_id) %>%
  dplyr::summarise(fa_rate = (sum(!correct & !present)+0.5)/(sum(!present)+1)) %>%
  dplyr::group_by(subj_id) %>%
  dplyr::summarise(occ_fa = fa_rate[hide_proportion == 0.10] - fa_rate[hide_proportion == 0.35])

#  occ_fa2 <- task_df_occ %>%
#    dplyr::group_by(hide_proportion,subj_id) %>%
#    dplyr::summarise(fa_rate = (sum(!correct & !present)+0.5)/(sum(!present)+1)) %>%
#   tidyr::pivot_wider(names_from = hide_proportion, values_from = fa_rate) %>%
#    dplyr::mutate(occ_fa = `0.1` - `0.35`) %>%
#    dplyr::select(subj_id, occ_fa)

size_fa <- task_df_size %>%
  dplyr::group_by(size,subj_id) %>%
  dplyr::summarise(fa_rate = (sum(!correct & !present)+0.5)/(sum(!present)+1)) %>%
  dplyr::group_by(subj_id) %>%
  dplyr::summarise(size_fa = fa_rate[size == 5] - fa_rate[size == 3])

false_alarms <- occ_fa %>%
  inner_join(size_fa, by = "subj_id")

cor.test(false_alarms$occ_fa, false_alarms$size_fa, type = "pearson")

# hits
occ_hits <- task_df_occ %>%
  dplyr::group_by(hide_proportion,subj_id) %>%
  dplyr::summarise(hit_rate = (sum(correct & present)+0.5)/(sum(present)+1)) %>%
  dplyr::group_by(subj_id) %>%
  dplyr::summarise(occ_hits = hit_rate[hide_proportion == 0.10] - hit_rate[hide_proportion == 0.35])
  
size_hits <- task_df_size %>%
  dplyr::group_by(size,subj_id) %>%
  dplyr::summarise(hit_rate = (sum(correct & present)+0.5)/(sum(present)+1)) %>%
  dplyr::group_by(subj_id) %>%
  dplyr::summarise(size_hits = hit_rate[size == 5] - hit_rate[size == 3])

hits <- occ_hits %>%
  inner_join(size_hits, by = "subj_id")

cor.test(hits$occ_hits, hits$size_hits, type = "pearson") # no! a-ha
