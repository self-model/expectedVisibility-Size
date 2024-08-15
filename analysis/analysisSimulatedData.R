# load and preprocess simulated data 
occ.sim.df <- read.csv('../modelling/modelFitting/simulateDataFromParameters/simulated_data/E_occ.csv') %>%
  mutate(present=factor(present,levels=c(1,-1),labels=c('present','absent')),
         occluded_rows=factor(ifelse(easy==0,6,2)), 
         hide_proportion=ifelse(easy,0.1,0.35)); # why is this phrased differently from line above that's so confusing

size.sim.df <- read.csv('../modelling/modelFitting/simulateDataFromParameters/simulated_data/E_size.csv') %>%
  mutate(present=factor(present,levels=c(1,-1),labels=c('present','absent')),
         size=ifelse(easy,5,3));

# load and preprocess minimal human data
occ.minimal.df <- read.csv('../modelling/data/E_occ.csv') %>% # needed to create new E_occ because confidence 
  mutate(present=factor(present,levels=c(1,-1),labels=c('present','absent')),
         occluded_rows=factor(ifelse(easy==0,6,2)), 
         hide_proportion=ifelse(easy,0.1,0.35));

size.minimal.df <- read.csv('../modelling/data/E_size.csv') %>%
  mutate(present=factor(present,levels=c(1,-1),labels=c('present','absent')),
         size=ifelse(easy,5,3));

# analyse and visualise: simulated occlusion data
occ.sim.response_RT <- occ.sim.df %>%
  dplyr::filter(correct == 1) %>%
  dplyr::group_by(subj_id, present) %>%
  dplyr::summarise(rt = median(rt)) %>%    
  tidyr::pivot_wider(names_from = present, values_from = rt) %>%
  dplyr::mutate(response_RT = `present` - `absent`)
t.test(response_RT$response_RT)

occ.sim.RT_pres <- occ.sim.df %>%
  dplyr::filter(present == 'present' & correct == 1) %>%
  

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


# analyse and visualise: simulated size data
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

# visualise human and simulated data 

