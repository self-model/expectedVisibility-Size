# too slow participants (to exclude)
occ_too_slow <- task_df %>%
  dplyr::group_by(subj_id) %>%
  dplyr::summarise(top_RT = quantile(RT, 0.95)) %>%
  dplyr::filter(top_RT > 7000) %>%
  dplyr::pull(subj_id) %>%
  unique()

size_too_slow <- task_df_size %>%
  dplyr::group_by(subj_id) %>%
  dplyr::summarise(top_RT = quantile(RT, 0.95)) %>%
  dplyr::filter(top_RT > 7000) %>%
  dplyr::pull(subj_id) %>%
  unique()

# pre-filter dfs 
E_occ_ddm <- task_df %>%
  dplyr::filter(!(subj_id %in% occ_too_slow))

E_size_ddm <- task_df_size %>%
  dplyr::filter(!(subj_id %in% size_too_slow))

# find remaining overlapping subj_ids (set this as levels so mapping consistent across two dfs)
overlapping_ids <- intersect(E_occ_ddm$subj_id, E_size_ddm$subj_id)

# create dfs 
E_occ_ddm <- E_occ_ddm %>%
  dplyr::filter(subj_id %in% overlapping_ids & RT > 100 & RT < 7000) %>%  # only overlapping ids 
  dplyr::mutate(rt = RT / 1000,
                easy = ifelse(hide_proportion < 0.2, 1, 0),
                subj_id = as.numeric(factor(subj_id, levels = overlapping_ids)),
                present = ifelse(present, 1, -1),
                correct = ifelse(correct, 1, 0)) %>%
  dplyr::select(subj_id, rt, correct, present, easy) %>%
  dplyr::arrange(subj_id)  # order subj_ids just for good measure 

E_size_ddm <- E_size_ddm %>%
  dplyr::filter(subj_id %in% overlapping_ids & RT > 100 & RT < 7000) %>%  
  dplyr::mutate(rt = RT / 1000,
                easy = ifelse(size == "5", 1, 0),
                subj_id = as.numeric(factor(subj_id, levels = overlapping_ids)),
                present = ifelse(present, 1, -1),
                correct = ifelse(correct, 1, 0)) %>%
  dplyr::select(subj_id, rt, correct, present, easy) %>%
  dplyr::arrange(subj_id)  

subj_id_mapping <- data.frame(
  subj_id = overlapping_ids,
  id_number = as.numeric(factor(overlapping_ids, levels = overlapping_ids))
) # save mapping 

E_occ_ddm %>% write.table(
  file = "../modelling/data/E_occ_new.csv",
  col.names = TRUE,
  row.names = FALSE,
  sep = ','
)

E_size_ddm %>% write.table(
  file = "../modelling/data/E_size_new.csv",
  col.names = TRUE,
  row.names = FALSE,
  sep = ','
)

