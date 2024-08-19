# extract relevant measures from letter detection task 
size_RT <- task_df_size %>%
  dplyr::filter(RT > 100 & RT < 7000 & correct == TRUE) %>%
  dplyr::group_by(subj_id, present) %>%
  dplyr::summarise(
    size_RT = median(RT[size == 5], na.rm = TRUE) - median(RT[size == 3], na.rm = TRUE)
  ) %>%
  dplyr::ungroup() %>%  #this is acc important bc mean and sd need to be calc over entire dataset 
  dplyr::mutate(
    size_RT_z = (size_RT - mean(size_RT, na.rm = TRUE)) / sd(size_RT, na.rm = TRUE)
  )

size_confidence <- task_df_size %>%
  dplyr::filter(correct == TRUE) %>%
  dplyr::group_by(subj_id, present) %>%
  dplyr::summarise(
    size_confidence = mean(confidence[size == 5], na.rm = TRUE) - mean(confidence[size == 3], na.rm = TRUE)
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    size_confidence_z = (size_confidence - mean(size_confidence, na.rm = TRUE)) / sd(size_confidence, na.rm = TRUE)
  )

size_accuracy <- task_df_size %>%
  dplyr::group_by(subj_id, present, size) %>%
  dplyr::summarise(
    accuracy = sum(!correct)/n(),
    .groups = 'drop') %>%
  dplyr::group_by(subj_id, present) %>%
  dplyr::summarise(
    size_accuracy = accuracy[size == 5] - accuracy[size == 3]
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    size_accuracy_z = (size_accuracy - mean(size_accuracy, na.rm = TRUE)) / sd(size_accuracy, na.rm = TRUE)
  )

size_df <- size_RT %>%
  left_join(size_confidence, by = c("subj_id", "present")) %>%
  left_join(size_accuracy, by = c("subj_id", "present"))

# new variable for shapes
task_df_shapes <- task_df_shapes %>%
  dplyr::mutate(
    model_consistent = ifelse(inference_accepted, 1, -1),
    adj_confidence = model_consistent * confidence
  )

# extract relevant measures from shapes task
inference_confidence_new <- task_df_shapes %>%
  dplyr::filter(RT > 100) %>%
  dplyr::select(adj_confidence,inference_type,subj_id) %>%
  dplyr::group_by(subj_id) %>%
  dplyr::summarise(
    MT = mean(adj_confidence[inference_type == 'MT'], na.rm=TRUE) - mean(adj_confidence[inference_type == 'AC'], na.rm=TRUE),
    MP = mean(adj_confidence[inference_type == 'MP'], na.rm=TRUE) - mean(adj_confidence[inference_type == 'AC'], na.rm=TRUE),
    DA = mean(adj_confidence[inference_type == 'DA'], na.rm=TRUE) - mean(adj_confidence[inference_type == 'AC'], na.rm=TRUE)
  )

# bind together
analysis_df_size <- size_df %>%
  inner_join(inference_confidence_new, by = "subj_id")

# regression of occlusion effect on RT in absence as a function of confidence in inference types 
absent_df_size <- analysis_df_size %>% 
  dplyr::filter(present == 0) 

summary(RT_absent_model_size <- lm(size_RT_z ~ MT + MP + DA, data = absent_df_size, na.action=na.omit))

# regression of occlusion effect on RT in presence as a function of confidence in inference types 
present_df_size <- analysis_df_size %>% 
  dplyr::filter(present == 1) 

summary(RT_present_model_size <- lm(size_RT_z ~ MT + MP + DA, data = present_df_size, na.action=na.omit))

# confidence in absent
summary(conf_absent_model_size <- lm(size_confidence_z ~ MT + MP + DA, data = absent_df_size, na.action=na.omit))

# confidence in present
summary(conf_present_model_size <- lm(size_confidence_z ~ MT + MP + DA, data = present_df_size, na.action=na.omit))

# false alarm rate
summary(accuracy_absent_model_size <- lm(size_accuracy_z ~ MT + MP + DA, data = absent_df_size, na.action=na.omit))

# miss rate
summary(accuracy_present_model_size <- lm(size_accuracy_z ~ MT + MP + DA, data = present_df_size, na.action=na.omit))
