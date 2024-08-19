# ---- load and preprocess parameters ----

occ_parameters <- read.table('../modelling/modelFitting/bestParameters/occlusion/best_parameters_from_E_occ_fit.csv', header=FALSE, sep=',') %>%
  mutate(alpha=V8**2,
         belalpha=V9**2,
         gamma = V5,
         diffalpha = belalpha-alpha,
         theta0 = V1/alpha, # V8
         theta1 = (V1+V2)/alpha, # V8
         beltheta0 = V3/belalpha, # V9
         beltheta1 = V3+V4/belalpha) # V9

size_parameters <- read.table('../modelling/modelFitting/bestParameters/size/best_parameters_from_E_size_fit.csv', header=FALSE, sep=',') %>%
  mutate(alpha=V8**2,
         belalpha=V9**2,
         gamma = V5,
         diffalpha = belalpha-alpha,
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
t.test(log(params_df$alpha[params_df$exp == 'size'])) 

# test whether believed alpha diff from 0 in size
mean(log(params_df$belalpha[params_df$exp == 'size']))
t.test(log(params_df$belalpha[params_df$exp == 'size'])) 

# test whether alpha diff from 0 in occlusion
mean(log(params_df$alpha[params_df$exp == 'occlusion']))
t.test(log(params_df$alpha[params_df$exp == 'occlusion'])) 

# test whether believed alpha diff from 0 in occlusion
mean(log(params_df$belalpha[params_df$exp == 'occlusion']))
t.test(log(params_df$belalpha[params_df$exp == 'occlusion'])) 

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
  tibble::rownames_to_column(var = "row_number")  # row numbers went all weird bc of the indexing so doing this but IDK ANYMORE

occ_params_filtered <- occ_params_filtered %>%
  tibble::rownames_to_column(var = "row_number")  

params_df_filtered <- inner_join(
  size_params_filtered, 
  occ_params_filtered , 
  by = "row_number", # because you do want to do this right? because matlab ordered them by s? 
  suffix = c(".size", ".occ")) %>% 
  select(-row_number)

# difference betw alpha and believed alpha for occlusion vs size 
t.test(params_df_filtered$diffalpha.occ, params_df_filtered$diffalpha.size, paired = TRUE) # yes

# correlations betw parameters across experiments 
cor.test(params_df_filtered$alpha.occ, params_df_filtered$alpha.size, type = "pearson")
cor.test(params_df_filtered$belalpha.occ, params_df_filtered$belalpha.size, type = "pearson") # uhhhhhm 

# uh redo these or no?
t.test(log(params_df_filtered$alpha.size)) # M = 0.18, p < .001
t.test(log(params_df_filtered$belalpha.size)) # M = 0.04, p = .266
t.test(log(params_df_filtered$alpha.occ)) # M = -0.26, p < .001
t.test(log(params_df_filtered$belalpha.occ)) # M = -0.13, p < .001

# ---- for intersected dfs ---- 

occ_parameters_intersect <- read.table('../modelling/modelFitting/bestParameters/occlusion/best_parameters_from_E_occ_intersect_fit.csv', header=FALSE, sep=',') %>%
  dplyr::mutate(alpha=V8**2,
         belalpha=V9**2,
         gamma = V5,
         diffalpha = belalpha-alpha,
         theta0 = V1/alpha, # V8
         theta1 = (V1+V2)/alpha, # V8
         beltheta0 = V3/belalpha, # V9
         beltheta1 = V3+V4/belalpha) # V9

size_parameters_intersect <- read.table('../modelling/modelFitting/bestParameters/size/best_parameters_from_E_size_intersect_fit.csv', header=FALSE, sep=',') %>%
  dplyr::mutate(alpha=V8**2,
         belalpha=V9**2,
         gamma = V5,
         diffalpha = belalpha-alpha,
         theta0 = V1/alpha, # V8
         theta1 = (V1+V2)/alpha, # V8
         beltheta0 = V3/belalpha, # V9
         beltheta1 = V3+V4/belalpha) # V9

params_df_intersect <- bind_cols(
  occ_parameters_intersect %>%
    dplyr::select(alpha, belalpha, gamma, diffalpha, theta0, theta1, beltheta0, beltheta1) %>%
    setNames(paste0(names(.), ".occ")),
  
  size_parameters_intersect %>%
    dplyr::select(alpha, belalpha, gamma, diffalpha, theta0, theta1, beltheta0, beltheta1) %>%
    setNames(paste0(names(.), ".size"))
)

# difference betw alpha and believed alpha for occlusion vs size 
t.test(params_df_intersect$diffalpha.occ, params_df_intersect$diffalpha.size, paired = TRUE) # Mdiff = 0.29, p < .001

# correlations betw parameters across experiments 
cor.test(params_df_intersect$alpha.occ, params_df_intersect$alpha.size, type = "pearson") # r = -0.06, p = .434
cor.test(params_df_intersect$belalpha.occ, params_df_intersect$belalpha.size, type = "pearson") # r = -0.01, p = .929 

# t-tests
t.test(log(params_df_intersect$alpha.size)) # M = 0.22, p < .001
t.test(log(params_df_intersect$belalpha.size)) # M = 0.08, p = .007
t.test(log(params_df_intersect$alpha.occ)) # M = -0.28, p < .001
t.test(log(params_df_intersect$belalpha.occ)) # M = -0.16, p < .001 
# these results are weird ... why are they different

# oh lord
t.test(params_df_intersect$alpha.size, params_df_intersect$belalpha.size, paired = TRUE) # Mdiff = 0.18, p < .001
t.test(params_df_intersect$alpha.occ, params_df_intersect$belalpha.occ, paired = TRUE) # Mdiff = -0.10, p < .001
