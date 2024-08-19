# get task_df_size with difficulty ratings from a script you very sniftily called "messing.R". please get your shit together.
# get overlapping ids from the exportForDDM_new.R script (don't ever call something new, if you got your shit together in the meantime this script will be called _intersect.R or something)
id_groups <- task_df_size %>%
  dplyr::filter(subj_id %in% overlapping_ids & RT > 100 & RT < 7000) %>%  
  dplyr::mutate(subj_id = as.numeric(factor(subj_id, levels = overlapping_ids))) %>% # i already know one day im gonna hate myself for this but basically i need to analyse something and turn it into this ddm format which it is not and for that we need to do this. ewaja.
  dplyr::select(subj_id, group) %>%
  dplyr::group_by(subj_id) %>%
  dplyr::slice(1) %>%
  dplyr::arrange(subj_id)   # i cant stress enough how important this is you will die otherwise (joking but the params only have row numbers which corresponds to the NUMBERS OF THE SUBJ_ID SO TO BIND YOU NEED THIS)

# parameters
params_per_group <- bind_cols(
  id_groups,
  params_df_intersect
)

params_per_group %>%
  filter(group == "smaller_harder") %>%
  pull(belalpha.size) %>%
  log() %>%
  t.test() # this is indeed very much non-significant, M = 0.03, p = .402

params_per_group %>%
  filter(group == "bigger_harder") %>%
  pull(belalpha.size) %>%
  log() %>%
  t.test() # M = 0.13, p = .07

params_per_group %>%
  filter(group == "no_effect") %>%
  pull(belalpha.size) %>%
  log() %>%
  t.test() # M = 0.10, p = .04

# i hate stats

# check diff
t.test(
  params_per_group$belalpha.size[params_per_group$group == "smaller_harder"],
  params_per_group$belalpha.size[params_per_group$group == "bigger_harder"]
)

t.test(
  params_per_group$belalpha.size[params_per_group$group == "smaller_harder"],
  params_per_group$belalpha.size[params_per_group$group == "no_effect"]
)

# or
summary(aov(belalpha.size ~ group, data = params_per_group))
