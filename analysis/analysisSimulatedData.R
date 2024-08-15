# ---- load and preprocess ----

# simulated data 
occ.sim.df <- read.csv('../modelling/modelFitting/simulateDataFromParameters/simulated_data/E_occ.csv') %>%
  mutate(present=factor(present,levels=c(1,-1),labels=c('present','absent')),
         occluded_rows=factor(ifelse(easy==0,6,2)), 
         hide_proportion=ifelse(easy,0.1,0.35)); # why is this phrased differently from line above that's so confusing

size.sim.df <- read.csv('../modelling/modelFitting/simulateDataFromParameters/simulated_data/E_size.csv') %>%
  mutate(present=factor(present,levels=c(1,-1),labels=c('present','absent')),
         size=ifelse(easy,5,3));

# minimal human data
occ.minimal.df <- read.csv('../modelling/data/E_occ.csv') %>% # needed to create new E_occ because confidence 
  mutate(present=factor(present,levels=c(1,-1),labels=c('present','absent')),
         occluded_rows=factor(ifelse(easy==0,6,2)), 
         hide_proportion=ifelse(easy,0.1,0.35));

size.minimal.df <- read.csv('../modelling/data/E_size.csv') %>%
  mutate(present=factor(present,levels=c(1,-1),labels=c('present','absent')),
         size=ifelse(easy,5,3));

# ---- analyse and visualise: simulated occlusion data ----

## RT ## 
occ.sim.response_RT <- occ.sim.df %>%
  dplyr::filter(correct == 1) %>%
  dplyr::group_by(subj_id, present) %>%
  dplyr::summarise(rt = median(rt)) %>%    
  tidyr::pivot_wider(names_from = present, values_from = rt) %>%
  dplyr::mutate(response_RT = `present` - `absent`)
t.test(occ.sim.response_RT$response_RT) ## M = -0.21, p < .001

occ.sim.RT_pres <- occ.sim.df %>%
  dplyr::filter(present == 'present' & correct == 1) %>%
  dplyr::group_by(subj_id, hide_proportion) %>%
  dplyr::summarise(rt = median(rt)) %>%
  tidyr::pivot_wider(names_from = hide_proportion, values_from = rt) %>%
  dplyr::mutate(present_RT = `0.1` - `0.35`)
t.test(occ.sim.RT_pres$present_RT) ## -0.09, p < .001

occ.sim.RT_abs <- occ.sim.df %>%
  dplyr::filter(present == 'absent' & correct == 1) %>%
  dplyr::group_by(subj_id, hide_proportion) %>%
  dplyr::summarise(rt = median(rt)) %>%
  tidyr::pivot_wider(names_from = hide_proportion, values_from = rt) %>%
  dplyr::mutate(abs_RT = `0.1` - `0.35`)
t.test(occ.sim.RT_abs$abs_RT) ## 0.02, p < .138

(occ.sim.RT_plot <- occ.sim.df %>%
    dplyr::filter(correct == 1) %>% # rt?
    dplyr::group_by(subj_id, occluded_rows, present) %>%
    dplyr::summarise(rt = median(rt)) %>%    
    dplyr::group_by(occluded_rows, present) %>%
    dplyr::summarise(mean_rt = mean(rt)) %>% # mean of indiv median
    ggplot(aes(x=occluded_rows, y = mean_rt, color = present)) +
    geom_point() +
    geom_line(aes(group = present)) + 
    labs(x = "Rows occluded", 
         y = "RT (ms)", 
         color = "Target presence") +
    scale_color_manual(values = c("absent" = "#E21a1c", "present" = "#377eb8")) +
    theme_bw())

## confidence ## 

occ.sim.resp_conf <- occ.sim.df %>%
  dplyr::filter(correct == 1) %>%
  dplyr::group_by(subj_id, present) %>%
  dplyr::summarise(confidence=mean(confidence)) %>%
  tidyr::pivot_wider(names_from = present, values_from = confidence) %>%
  dplyr::mutate(resp_conf = `present` - `absent`)
t.test(occ.sim.resp_conf$resp_conf) ## 0.03, p < .001

occ.sim.conf_pres <- occ.sim.df %>%
  dplyr::filter(present == 'present' & correct == 1) %>%
  dplyr::group_by(subj_id, hide_proportion) %>%
  dplyr::summarise(confidence=mean(confidence)) %>%
  tidyr::pivot_wider(names_from = hide_proportion, values_from = confidence) %>%
  dplyr::mutate(conf_pres = `0.1` - `0.35`)
t.test(occ.sim.conf_pres$conf_pres) ## 0.02, p < .001

occ.sim.conf_abs <- occ.sim.df %>%
  dplyr::filter(present == 'absent' & correct == 1) %>%
  dplyr::group_by(subj_id, hide_proportion) %>%
  dplyr::summarise(confidence=mean(confidence)) %>%
  tidyr::pivot_wider(names_from = hide_proportion, values_from = confidence) %>%
  dplyr::mutate(conf_abs = `0.1` - `0.35`)
t.test(occ.sim.conf_abs$conf_abs) ## 0.02, p < .001

(occ.sim.conf_plot <- occ.sim.df %>%
    dplyr::filter(correct == 1) %>%
    dplyr::group_by(occluded_rows, present) %>%
    dplyr::summarise(confidence = mean(confidence)) %>%
    ggplot(aes(x=occluded_rows, y = confidence, color = present)) +
    geom_point() +
    geom_line(aes(group = present)) + 
    labs(x = "Rows occluded", 
         y = "Confidence", 
         color = "Target presence") +
    scale_color_manual(values = c("absent" = "#E21a1c", "present" = "#377eb8")) +
    theme_bw())

## errors ## 

(occ.sim.errors_plot <- occ.sim.df %>%
    group_by(subj_id,occluded_rows,present) %>%
    summarise(err = 1-mean(correct)) %>%
    group_by(occluded_rows,present) %>%
    summarise(se=se(err),
              mean_err = mean(err)) %>%
    ggplot(aes(x=occluded_rows, y=mean_err, color = present)) + 
    geom_line(aes(group=present)) + 
    labs(x="Rows occluded",
         y="Error",
         color = "Target presence") +
    scale_color_manual(values = c("absent" = "#E21a1c", "present" = "#377eb8")) +
    theme_bw())


# ---- analyse and visualise: simulated size data ----

size.sim_resp_RT <- size.sim.df %>%
  filter(correct == 1) %>%
  group_by(subj_id, present) %>%
  summarise(rt = median(rt)) %>%
  pivot_wider(names_from = present, values_from = rt) %>%
  mutate(resp_RT = `present` - `absent`)
t.test(size.sim_resp_RT$resp_RT) ## -0.24, p < .001

size.sim.RT_pres <- size.sim.df %>%
  filter(present == 'present' & correct == 1) %>%
  group_by(subj_id, size) %>%
  summarise(rt = median(rt)) %>%
  pivot_wider(names_from = size, values_from = rt) %>%
  mutate(pres_RT = `5` - `3`)
t.test(size.sim.RT_pres$pres_RT) ## 0.11, p < .001

size.sim.RT_abs <- size.sim.df %>%
  filter(present == 'absent' & correct == 1) %>%
  group_by(subj_id,size) %>%
  summarise(rt = median(rt)) %>%
  pivot_wider(names_from = size, values_from = rt) %>%
  mutate(abs_RT = `5` - `3`)
t.test(size.sim.RT_abs$abs_RT) ## -0.01, p = .178 

(size.sim.RT_plot <- size.sim.df %>%
    dplyr::filter(correct == 1) %>% # rt?
    dplyr::mutate(size = factor(ifelse(size==5, 'big','small'), levels=c('big','small'))) %>%
    dplyr::group_by(subj_id, size, present) %>%
    dplyr::summarise(rt = median(rt)) %>%    
    dplyr::group_by(size, present) %>%
    dplyr::summarise(mean_rt = mean(rt)) %>% # mean of indiv median
    ggplot(aes(x=size, y = mean_rt, color = present)) +
    geom_point() +
    geom_line(aes(group = present)) + 
    labs(x = "Size", 
         y = "RT (ms)", 
         color = "Target presence") +
    scale_color_manual(values = c("absent" = "#E21a1c", "present" = "#377eb8")) +
    theme_bw())

## confidence ##

size.sim.resp_conf <- size.sim.df %>%
  filter(correct == 1) %>%
  group_by(subj_id,present) %>%
  summarise(confidence = mean(confidence)) %>%
  pivot_wider(names_from = present, values_from = confidence) %>%
  mutate(resp_conf = `present` - `absent`)
t.test(size.sim.resp_conf$resp_conf) ## 0.02, p < .001

size.sim.conf_pres <- size.sim.df %>%
  filter(present == 'present' & correct ==1) %>%
  group_by(subj_id,size) %>%
  summarise(confidence = mean(confidence)) %>%
  pivot_wider(names_from=size, values_from=confidence) %>%
  mutate(conf_pres = `5`-`3`)
t.test(size.sim.conf_pres$conf_pres) ## 0.003, p = .342 

size.sim.conf_abs <- size.sim.df %>%
  filter(present == 'absent' & correct ==1) %>%
  group_by(subj_id,size) %>%
  summarise(confidence = mean(confidence)) %>%
  pivot_wider(names_from=size, values_from=confidence) %>%
  mutate(conf_abs = `5`-`3`)
t.test(size.sim.conf_abs$conf_abs) ## 0.003, p = .337

(size.sim.conf_plot <- size.sim.df %>%
    dplyr::filter(correct == 1) %>%
    dplyr::mutate(size = factor(ifelse(size==5, 'big','small'), levels=c('big','small'))) %>%
    dplyr::group_by(size, present) %>%
    dplyr::summarise(confidence = mean(confidence)) %>%
    ggplot(aes(x=size, y = confidence, color = present)) +
    geom_point() +
    geom_line(aes(group = present)) + 
    labs(x = "Size", 
         y = "Confidence", 
         color = "Target presence") +
    scale_color_manual(values = c("absent" = "#E21a1c", "present" = "#377eb8")) +
    theme_bw())

(size.sim.errors_plot <- size.sim.df %>%
    dplyr::mutate(size = factor(ifelse(size==5, 'big','small'), levels=c('big','small'))) %>%
    group_by(subj_id,size,present) %>%
    summarise(err = 1-mean(correct)) %>%
    group_by(size,present) %>%
    summarise(se=se(err),
              mean_err = mean(err)) %>%
    ggplot(aes(x=size, y=mean_err, color = present)) + 
    geom_line(aes(group=present)) + 
    labs(x="Size",
         y="Error",
         color = "Target presence") +
    scale_color_manual(values = c("absent" = "#E21a1c", "present" = "#377eb8")) +
    theme_bw())
