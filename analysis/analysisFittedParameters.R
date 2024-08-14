# ---- load and preprocess parameters ----

occ_parameters <- read.table('../modelling/modelFitting/bestParameters/occlusion/best_parameters_from_E_occ_fit.csv', header=FALSE, sep=',') %>%
  mutate(alpha=V8**2,
         belalpha=V9**2,
         gamma = V5,
         offsetalpha = belalpha-alpha,
         theta0 = V1/alpha, # V8
         theta1 = (V1+V2)/alpha, # V8
         beltheta0 = V3/belalpha, # V9
         beltheta1 = V3+V4/belalpha) # V9

size_parameters <- read.table('../modelling/modelFitting/bestParameters/size/best_parameters_from_E_size_fit.csv', header=FALSE, sep=',') %>%
  mutate(alpha=V8**2,
         belalpha=V9**2,
         gamma = V5,
         offsetalpha = belalpha-alpha,
         theta0 = V1/alpha, # V8
         theta1 = (V1+V2)/alpha, # V8
         beltheta0 = V3/belalpha, # V9
         beltheta1 = V3+V4/belalpha) # V9

# ---- analyse fitted parameters ----

# parameters df 
params_df <- occ_parameters %>%
  mutate(exp= 'occlusion',
         subj_id=1:n()) %>%
  rbind(size_parameters %>% 
          mutate(exp= 'size',
                 subj_id=1:n())) %>%
  group_by(exp) %>%
  mutate(diff = alpha - belalpha) 

# test whether alpha diff from 0 in size 
mean(log(params_df$alpha[params_df$exp == 'size']))
t.test(log(params_df$alpha[params_df$exp == 'size']), mu=0) 

# test whether believed alpha diff from 0 in size
mean(log(params_df$belalpha[params_df$exp == 'size']))
t.test(log(params_df$belalpha[params_df$exp == 'size']), mu=0) 

# test whether alpha diff from 0 in occlusion
mean(log(params_df$alpha[params_df$exp == 'occlusion']))
t.test(log(params_df$alpha[params_df$exp == 'occlusion']), mu=0) 

# test whether believed alpha diff from 0 in occlusion
mean(log(params_df$belalpha[params_df$exp == 'occlusion']))
t.test(log(params_df$belalpha[params_df$exp == 'occlusion']), mu=0) 

# ---- fix dfs/subj_ds ----

# redo these dfs
df_size <- read.csv('../modelling/modelFitting/fitSize/data/E_size.csv')
subj_ids_size <- unique(df_size$subj_id)
indices_size <- subj_ids_size[subj_ids_size <= nrow(size_parameters)]
size_params_filtered <- size_parameters[indices_size, ]

df_occ <- read.csv('../modelling/modelFitting/fitOcclusion/data/E_occ.csv')
subj_ids_occ <- unique(df_occ$subj_id)
indices_occ <- subj_ids_occ[subj_ids_occ <= nrow(occ_parameters)]
occ_params_filtered <- occ_parameters[indices_occ, ]

# idkkkk if this is right
size_params_filtered <- size_params_filtered %>%
  tibble::rownames_to_column(var = "row_number")  # row names went all weird so doing this but IDK 

occ_params_filtered <- occ_params_filtered %>%
  tibble::rownames_to_column(var = "row_number")  

params_df_filtered <- inner_join(
  size_params_filtered, 
  occ_params_filtered , 
  by = "row_number", # because you do want to do this right? because matlab ordered them by s? 
  suffix = c(".size", ".occ")) %>% 
  select(-row_number)

# difference betw alpha and believed alpha for occlusion vs size 
t.test(params_df_filtered$offsetalpha.occ, params_df_filtered$offsetalpha.size, paired = TRUE) # yes

# correlations betw parameters across experiments 
cor.test(params_df_filtered$alpha.occ, params_df_filtered$alpha.size, type = "pearson")
cor.test(params_df_filtered$belalpha.occ, params_df_filtered$belalpha.size, type = "pearson") # uhhhhhm 

# uh redo these or no?
t.test(log(params_df_filtered$alpha.size), mu=0)
t.test(log(params_df_filtered$belalpha.size), mu=0)
t.test(log(params_df_filtered$alpha.occ), mu=0)
t.test(log(params_df_filtered$belalpha.occ), mu=0)

# right so these results are wildly different 