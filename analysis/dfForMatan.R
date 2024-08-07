# occlusion effect on confidence in presence
conf_occ_presence <- task_df %>%
  dplyr::filter(present == 1 & correct == TRUE) %>%
  dplyr::group_by(subj_id, hide_proportion) %>%
  dplyr::summarise(confidence = mean(confidence)) %>%
  tidyr::pivot_wider(names_from = hide_proportion, values_from = confidence) %>%
  dplyr::mutate(conf_occ_presence = `0.1` - `0.35`) # this is swapped around from how i did it for the occlusion analysis; otherwise it's not easy - hard for both manipulations. 

# occlusion effect on confidence in absence
conf_occ_absence <- task_df %>%
  dplyr::filter(present == 0 & correct == TRUE) %>%
  dplyr::group_by(subj_id, hide_proportion) %>%
  dplyr::summarise(confidence = mean(confidence)) %>%
  tidyr::pivot_wider(names_from = hide_proportion, values_from = confidence) %>%
  dplyr::mutate(conf_occ_absence = `0.1` - `0.35`)

# occlusion response interaction on confidence 
conf_occ_resp <- inner_join(
  conf_occ_presence,
  conf_occ_absence,
  by = "subj_id") %>%
  mutate(conf_occ_resp = conf_occ_presence - conf_occ_absence)

# size effect on conf in presence
conf_size_present <- task_df_size %>%
  dplyr::filter(present == 1 & correct == TRUE) %>%
  dplyr::group_by(subj_id, size) %>%
  dplyr::summarise(confidence = mean(confidence)) %>%
  tidyr::pivot_wider(names_from = size, values_from = confidence) %>%
  dplyr::mutate(conf_size_present = `5`-`3`)

# size effect on conf in absence
conf_size_absent <- task_df_size %>%
  dplyr::filter(present == 0 & correct == TRUE) %>%
  dplyr::group_by(subj_id, size) %>%
  dplyr::summarise(confidence = mean(confidence)) %>%
  tidyr::pivot_wider(names_from = size, values_from = confidence) %>%
  dplyr::mutate(conf_size_absent = `5`-`3`)

# size response interaction on confidence
conf_size_resp <- inner_join(
  conf_size_present,
  conf_size_absent,
  by = "subj_id") %>%
  dplyr::mutate(conf_size_resp = conf_size_present - conf_size_absent)

# create big df 
manip_df <- inner_join(conf_occ_resp, conf_size_resp, by = "subj_id") %>%
  dplyr::select(subj_id, conf_occ_resp, conf_size_resp) %>%
  dplyr::rename(conf_occlusion = conf_occ_resp,
                conf_size = conf_size_resp) 

# save
write.csv(manip_df, "manipulations_df.csv", row.names = FALSE)

# size df
size_df <- task_df_size %>%
  dplyr::filter(correct == TRUE) %>%
  dplyr::select(subj_id, size, present, confidence) %>%
  dplyr::group_by(subj_id, size, present) %>%
  dplyr::summarise(confidence = mean(confidence))

write.csv(size_df, "size_df.csv", row.names = FALSE)

# occlusion df
occlusion_df <- task_df %>%
  dplyr::filter(correct == TRUE) %>%
  dplyr::select(subj_id, hide_proportion, present, confidence) %>%
  dplyr::group_by(subj_id, hide_proportion, present) %>%
  dplyr::summarise(confidence = mean(confidence))

write.csv(occlusion_df, "occlusion_df.csv", row.names = FALSE)
