# ---- load and preprocess ----

# simulated data 
occ.sim.df <- read.csv('../modelling/modelFitting/simulateDataFromParameters/simulated_data/E_occ_intersect.csv') %>%
  mutate(present=factor(present,levels=c(1,-1),labels=c('present','absent')),
         occluded_rows=factor(ifelse(easy==0,6,2)), 
         hide_proportion=ifelse(easy,0.1,0.35),
         confidence=2*confidence-1); # why is this phrased differently from line above that's so confusing

size.sim.df <- read.csv('../modelling/modelFitting/simulateDataFromParameters/simulated_data/E_size_intersect.csv') %>%
  mutate(present=factor(present,levels=c(1,-1),labels=c('present','absent')),
         size=ifelse(easy,5,3),
         confidence=2*confidence-1);

# minimal human data
occ.minimal.df <- read.csv('../modelling/data/E_occ_intersect.csv') %>% # needed to create new E_occ because confidence 
  mutate(present=factor(present,levels=c(1,-1),labels=c('present','absent')),
         occluded_rows=factor(ifelse(easy==0,6,2)), 
         hide_proportion=ifelse(easy,0.1,0.35));

size.minimal.df <- read.csv('../modelling/data/E_size_intersect.csv') %>%
  mutate(present=factor(present,levels=c(1,-1),labels=c('present','absent')),
         size=ifelse(easy,5,3));

# set as num
occ.minimal.df$hide_proportion <- as.numeric(as.character(occ.minimal.df$hide_proportion))
occ.sim.df$hide_proportion <- as.numeric(as.character(occ.sim.df$hide_proportion))

size.minimal.df$size <- as.numeric(as.character(size.minimal.df$size))
size.sim.df$size <- as.numeric(as.character(size.sim.df$size))

detection_colors = c('#377eb8', '#e41a1c');
labels = c('Present','Absent')

# why does se not work anymore? here's your own
se <- function(x) {
  sd(x) / sqrt(length(x))
}

### panel A: errors ###
plot_errors_by_occlusion <- function(human_df, sim_df, occlusion_levels, file_name) {
  
  sim_errors <- sim_df %>%
    dplyr::group_by(subj_id,hide_proportion,present) %>%
    dplyr::summarise(err = 1-mean(correct))
  
  sim_errors_mean <- sim_errors %>%
    dplyr::group_by(hide_proportion,present) %>%
    dplyr::summarise(se=se(err),
              err=mean(err))
  
  human_errors <- human_df %>%
    dplyr::group_by(subj_id,hide_proportion,present) %>%
    dplyr::summarise(err = 1-mean(correct))
  
  human_errors_mean <- human_errors %>%
    dplyr::group_by(hide_proportion,present) %>%
    dplyr::summarise(se=se(err),
              err=mean(err))
  
  errors_mean <- merge(human_errors_mean,
                       sim_errors_mean,
                       by=c('hide_proportion','present'),
                       suffixes = c('.human','.sim'))
  
  errors_mean %>%
    ggplot(aes(x=hide_proportion,y=err.human,color=present,shape=present, fill=present))+
    scale_shape_manual(values=c(4,16))+
    geom_errorbar(aes(ymin=err.human-se.human,ymax=err.human+se.human),width=0.01)+
    scale_color_manual(values=detection_colors)+
    scale_fill_manual(values=detection_colors)+
    geom_rect(aes(xmin=hide_proportion-0.02,xmax=hide_proportion+0.02,ymin=err.sim-se.sim, ymax=err.sim+se.sim),alpha=0.3,color='NA')+
    geom_point(size=2)+
    geom_line()+
    theme_classic() +
    theme(legend.position='none') +
    scale_x_continuous(breaks=occlusion_levels,name='proportion occluded')+
    scale_y_continuous(name='error rate', limits = c(0,0.35)) 
  
  ggsave(paste('../docs/figures/Fig1/',file_name,'.png',sep=''), width=2.2,height=2.2, dpi=600)
}

plot_errors_by_occlusion(occ.minimal.df, occ.sim.df, c(0.10,0.35), 'occ_errors_intersect')

### panel B: RTs ###

plot_RT_by_occlusion <- function(human_df, sim_df, occlusion_levels, file_name) {
  
  sim_RT <- sim_df %>%
    dplyr::filter(correct==1)%>%
    dplyr::group_by(subj_id,hide_proportion,present) %>%
    dplyr::summarise(RT = median(rt))
  
  sim_RT_mean <- sim_RT %>%
    dplyr::group_by(hide_proportion,present) %>%
    dplyr::summarise(se=se(RT),
              RT=mean(RT))
  
  human_RT <- human_df %>%
    dplyr::filter(correct==1)%>%
    dplyr::group_by(subj_id,hide_proportion,present) %>%
    dplyr::summarise(RT = median(rt))
  
  human_RT_mean <- human_RT %>%
    dplyr::group_by(hide_proportion,present) %>%
    dplyr::summarise(se=se(RT),
              RT=mean(RT))
  
  RT_mean <- merge(human_RT_mean,
                   sim_RT_mean,
                   by=c('hide_proportion','present'),
                   suffixes = c('.human','.sim'))
  
  bar_scale <- occlusion_levels[2]-occlusion_levels[1];
  
  
  RT_mean %>%
    ggplot(aes(x=hide_proportion,y=RT.human,color=present,shape=present, fill=present))+
    scale_shape_manual(values=c(4,16))+
    geom_errorbar(aes(ymin=RT.human-se.human,ymax=RT.human+se.human),width=0.01)+
    scale_color_manual(values=detection_colors)+
    scale_fill_manual(values=detection_colors)+
    geom_rect(aes(xmin=hide_proportion-0.02,xmax=hide_proportion+0.02,ymin=RT.sim-se.sim, ymax=RT.sim+se.sim),alpha=0.3,color='NA')+
    geom_point(size=2)+
    geom_line()+
    theme_classic() +
    theme(legend.position='none') +
    scale_x_continuous(breaks=occlusion_levels,name='proportion occluded')+
    scale_y_continuous(name='RT (sec)', limits=c(1.5,2.5)) 
  
  ggsave(paste('../docs/figures/Fig1/',file_name,'.png',sep=''), width=2.2,height=2.2, dpi=600)
}

plot_RT_by_occlusion(occ.minimal.df, occ.sim.df, c(0.10,0.35), 'occ_RT_intersect')

### panel C: confidence ###

plot_confidence_by_occlusion <- function(human_df, sim_df, occlusion_levels, file_name) {
  
  sim_confidence <- sim_df %>%
    dplyr::filter(correct==1)%>%
    dplyr::group_by(subj_id,hide_proportion,present) %>%
    dplyr::summarise(confidence = mean(confidence))
  
  sim_confidence_mean <- sim_confidence %>%
    dplyr::group_by(hide_proportion,present) %>%
    dplyr::summarise(se=se(confidence),
              confidence=mean(confidence))
  
  human_confidence <- human_df %>%
    dplyr::filter(correct==1)%>%
    dplyr::group_by(subj_id,hide_proportion,present) %>%
    dplyr::summarise(confidence = mean(confidence))
  
  human_confidence_mean <- human_confidence %>%
    dplyr::group_by(hide_proportion,present) %>%
    dplyr::summarise(se=se(confidence),
              confidence=mean(confidence))
  
  confidence_mean <- merge(human_confidence_mean,
                           sim_confidence_mean,
                           by=c('hide_proportion','present'),
                           suffixes = c('.human','.sim'))
  
  
  confidence_mean %>%
    ggplot(aes(x=hide_proportion,y=confidence.human,color=present,shape=present, fill=present))+
    scale_shape_manual(values=c(4,16))+
    geom_errorbar(aes(ymin=confidence.human-se.human,ymax=confidence.human+se.human),width=0.01)+
    scale_color_manual(values=detection_colors)+
    scale_fill_manual(values=detection_colors)+
    geom_rect(aes(xmin=hide_proportion-0.02,xmax=hide_proportion+0.02,ymin=confidence.sim-se.sim, ymax=confidence.sim+se.sim),alpha=0.3,color='NA')+
    geom_point(size=2)+
    geom_line()+
    theme_classic() +
    theme(legend.position='none') +
    scale_x_continuous(breaks=occlusion_levels,name='proportion occluded')+
    scale_y_continuous(name='confidence', limits=c(0.6,0.9)) 
  
  ggsave(paste('../docs/figures/Fig1/',file_name,'.png',sep=''), width=2.2,height=2.2, dpi=600)
}

plot_confidence_by_occlusion(occ.minimal.df, occ.sim.df, c(0.1,0.35), 'occ_confidence_intersect')

# ---- DO SAME THING FOR SIZE ----

### panel A: errors ###
plot_errors_by_size <- function(human_df, sim_df, size_levels, file_name) {
  
  sim_errors <- sim_df %>%
    dplyr::group_by(subj_id,size,present) %>%
    dplyr::summarise(err = 1-mean(correct))
  
  sim_errors_mean <- sim_errors %>%
    dplyr::group_by(size,present) %>%
    dplyr::summarise(se=se(err),
              err=mean(err))
  
  human_errors <- human_df %>%
    dplyr::group_by(subj_id,size,present) %>%
    dplyr::summarise(err = 1-mean(correct))
  
  human_errors_mean <- human_errors %>%
    dplyr::group_by(size,present) %>%
    dplyr::summarise(se=se(err),
              err=mean(err))
  
  errors_mean <- merge(human_errors_mean,
                       sim_errors_mean,
                       by=c('size','present'),
                       suffixes = c('.human','.sim'))
  
  errors_mean %>%
    ggplot(aes(x=size,y=err.human,color=present,shape=present, fill=present))+
    scale_shape_manual(values=c(4,16))+
    geom_errorbar(aes(ymin=err.human-se.human,ymax=err.human+se.human),width=0.01)+
    scale_color_manual(values=detection_colors)+
    scale_fill_manual(values=detection_colors)+
    geom_rect(aes(xmin=size-0.2,xmax=size+0.2,ymin=err.sim-se.sim, ymax=err.sim+se.sim),alpha=0.3,color='NA')+
    geom_point(size=2)+
    geom_line()+
    theme_classic() +
    theme(legend.position='none') +
    scale_x_continuous(breaks=size_levels,name='size', trans = 'reverse')+ # reverse so it goes big left hard right (for sort of consistency ig)
    scale_y_continuous(name='error rate', limits = c(0,0.2)) 
  
  ggsave(paste('../docs/figures/Fig1/',file_name,'.png',sep=''), width=2.2,height=2.2, dpi=600)
}

plot_errors_by_size(size.minimal.df, size.sim.df, c(3,5), 'size_errors_intersect')

### panel B: RTs ###

plot_RT_by_size <- function(human_df, sim_df, size_levels, file_name) {
  
  sim_RT <- sim_df %>%
    dplyr::filter(correct==1)%>%
    dplyr::group_by(subj_id,size,present) %>%
    dplyr::summarise(RT = median(rt))
  
  sim_RT_mean <- sim_RT %>%
    dplyr::group_by(size,present) %>%
    dplyr::summarise(se=se(RT),
              RT=mean(RT))
  
  human_RT <- human_df %>%
    dplyr::filter(correct==1)%>%
    dplyr::group_by(subj_id,size,present) %>%
    dplyr::summarise(RT = median(rt))
  
  human_RT_mean <- human_RT %>%
    dplyr::group_by(size,present) %>%
    dplyr::summarise(se=se(RT),
              RT=mean(RT))
  
  RT_mean <- merge(human_RT_mean,
                   sim_RT_mean,
                   by=c('size','present'),
                   suffixes = c('.human','.sim'))
  
  bar_scale <- size_levels[2]-size_levels[1];
  
  
  RT_mean %>%
    ggplot(aes(x=size,y=RT.human,color=present,shape=present, fill=present))+
    scale_shape_manual(values=c(4,16))+
    geom_errorbar(aes(ymin=RT.human-se.human,ymax=RT.human+se.human),width=0.01)+
    scale_color_manual(values=detection_colors)+
    scale_fill_manual(values=detection_colors)+
    geom_rect(aes(xmin=size-0.2,xmax=size+0.2,ymin=RT.sim-se.sim, ymax=RT.sim+se.sim),alpha=0.3,color='NA')+
    geom_point(size=2)+
    geom_line()+
    theme_classic() +
    theme(legend.position='none') +
    scale_x_continuous(breaks=size_levels,name='size', trans = 'reverse')+
    scale_y_continuous(name='RT (sec)', limits=c(1.5,2.5)) 
  
 ggsave(paste('../docs/figures/Fig1/',file_name,'.png',sep=''), width=2.2,height=2.2, dpi=600)
}

plot_RT_by_size(size.minimal.df, size.sim.df, c(3,5), 'size_RT_intersect')

### panel C: confidence ###

plot_confidence_by_size <- function(human_df, sim_df, size_levels, file_name) {
  
  sim_confidence <- sim_df %>%
    dplyr::filter(correct==1)%>%
    dplyr::group_by(subj_id,size,present) %>%
    dplyr::summarise(confidence = mean(confidence))
  
  sim_confidence_mean <- sim_confidence %>%
    dplyr::group_by(size,present) %>%
    dplyr::summarise(se=se(confidence),
              confidence=mean(confidence))
  
  human_confidence <- human_df %>%
    dplyr::filter(correct==1)%>%
    dplyr::group_by(subj_id,size,present) %>%
    dplyr::summarise(confidence = mean(confidence))
  
  human_confidence_mean <- human_confidence %>%
    dplyr::group_by(size,present) %>%
    dplyr::summarise(se=se(confidence),
              confidence=mean(confidence))
  
  confidence_mean <- merge(human_confidence_mean,
                           sim_confidence_mean,
                           by=c('size','present'),
                           suffixes = c('.human','.sim'))
  
  
  confidence_mean %>%
    ggplot(aes(x=size,y=confidence.human,color=present,shape=present, fill=present))+
    scale_shape_manual(values=c(4,16))+
    geom_errorbar(aes(ymin=confidence.human-se.human,ymax=confidence.human+se.human),width=0.01)+
    scale_color_manual(values=detection_colors)+
    scale_fill_manual(values=detection_colors)+
    geom_rect(aes(xmin=size-0.2,xmax=size+0.2,ymin=confidence.sim-se.sim, ymax=confidence.sim+se.sim),alpha=0.3,color='NA')+
    geom_point(size=2)+
    geom_line()+
    theme_classic() +
    theme(legend.position='none') +
    scale_x_continuous(breaks=size_levels,name='size', trans = 'reverse')+
    scale_y_continuous(name='confidence', limits=c(0.6,0.9)) 
  
  ggsave(paste('../docs/figures/Fig1/',file_name,'.png',sep=''), width=2.2,height=2.2, dpi=600)
}

plot_confidence_by_size(size.minimal.df, size.sim.df, c(3,5), 'size_confidence_intersect')
