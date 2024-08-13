# load and preprocess parameters 

# fitted parameters
params_df <- occlusion_parameters %>%
  mutate(exp= 'occlusion',
         subj_id=1:n()) %>%
  rbind(size_parameters %>% 
          mutate(exp= 'size',
                 subj_id=1:n())) %>%
  group_by(exp) %>%
  mutate(diff = alpha - belalpha) # no z-scoring 

# report mean log alpha and mean log alpha prime for occlusion and size 

# correlations between parameters 

# test whether alpha diff from 0
t.test(log(params_df$alpha[params_df$exp == 'size']), mu=0) 

# test whether believed alpha diff from 0 
t.test(log(params_df$belalpha[params_df$exp == 'size']), mu=0) 

# difference betw alpha and believed alpha for occlusion vs size 
t.test(params_df$diff[params_df$exp == 'occlusion'], params_df$diff[params_df$exp == 'size'], paired = TRUE) # paired t-test
