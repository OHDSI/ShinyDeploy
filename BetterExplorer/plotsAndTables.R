library(ggplot2)
library(dplyr)

# 04/12/2023: add BETTER-related plotting functions
plotType1Error <- function(d){
  if(nrow(d) == 0 || is.null(d)){
    return(NULL)
  }
  
  yinters = 0.05
  ybreaks = c(0,0.05, 0.1, 0.25, 0.5, 0.75,1.0)
  period_breaks = seq(from = min(d$period_id),
                      to = max(d$period_id),
                      by = 3)
  period_labels = as.integer(period_breaks)
  type1colors = c("#5BBCD6", "#046C9A", "#273046")
  
  capt = "" # no caption text for now
  
  pg = ggplot(d, 
             aes(x=period_id, y=y, color=approach, alpha = stage))+
    geom_line(size = 1.5) +
    geom_point(size=2)+
    geom_hline(yintercept = yinters, 
               color = 'gray60', 
               size = 1, linetype=2)+
    scale_y_continuous(limits = c(0,1),
                       breaks = ybreaks,
                       trans = 'sqrt'
    )+
    facet_grid(.~analysis,
               labeller = label_wrap_gen(width=28)) +
    scale_x_continuous(breaks = period_breaks, labels = period_labels)+
    labs(x='analysis period (months)', 
         y='Type 1 error rate', 
         caption = capt, 
         color='Type 1 error of:')+
    scale_color_manual(values = type1colors) +
    scale_alpha_continuous(range = c(0.2, 1), guide = 'none')+
    guides(color=guide_legend(nrow=1,byrow=TRUE))+
    theme_bw(base_size = 16)+
    theme(legend.position = 'bottom',
          plot.caption = element_text(hjust=0))
  
  return(pg)
}

plotPower <- function(d) {
  if(nrow(d) == 0 || is.null(d)){
    return(NULL)
  }
  
  period_breaks = seq(from = min(d$period_id),
                      to = max(d$period_id),
                      by = 3)
  period_labels = as.integer(period_breaks)
  
  colors = c("#5BBCD6", "#046C9A", "#273046")
  
  capt = "" # no caption text for now
  
  pg = ggplot(d, 
             aes(x=period_id, y=power, 
                 color = approach, 
                 alpha = stage))+
    geom_line(size = 1.5) +
    geom_point(size=2)+
    scale_y_continuous(limits = c(0,1))+
    scale_x_continuous(breaks = period_breaks, labels = period_labels)+
    labs(x='analysis period (months)', 
         y='power', 
         caption = capt, 
         color='Statistical power of:')+
    scale_color_manual(values = colors) +
    scale_alpha_continuous(range = c(0.2, 1), guide = 'none')+
    facet_grid(.~true_rr)+
    theme_bw(base_size = 16)+
    theme(legend.position = 'bottom',
          plot.caption = element_text(hjust=0)) # change to bottom legend...
  
  return(pg)
}

plotTTS <- function(d){
  if(nrow(d) == 0 || is.null(d)){
    return(NULL)
  }
  
  d = d %>% 
    rename('analysisId' = 'analysis_id') %>%
    left_join(analysis, by = c('method', 'analysisId')) %>%
    rename('analysis' = 'description')
  
  d$approach = factor(d$approach,
                      levels = c('MaxSPRT', "Adjusted Bayes"))
  
  colors = c("#046C9A", "#ABDDDE")
  
  sensitivity_level = d$sensitivity[1]
  
  pg = ggplot(d, 
              aes(y=as.factor(effect_size), 
                  x=time_to_sens, 
                  fill=approach)) +
    geom_bar(stat='identity', position = 'dodge')+
    geom_hline(yintercept = c(1.5,2.5), color='gray40')+
    scale_x_continuous(breaks = seq(from=0, to=12, by=3),
                       limits = c(0,12)) +
    labs(y='Effect Size', 
         x=sprintf('Time to %.0f%% sensitivity', 
                   sensitivity_level * 100),
         fill = '') +
    facet_grid(analysis~.,
               labeller = label_wrap_gen(width=25)) +
    theme_bw(base_size = 16)+
    theme(panel.grid.major.x = element_blank(),
          axis.ticks.x = element_blank(),
          strip.background = element_blank(),
          strip.text.y = element_text(angle = 0),
          legend.position = 'bottom')+
    guides(fill=guide_legend(nrow=1,byrow=TRUE)) +
    scale_fill_manual(values = colors,
                      breaks = c("Adjusted Bayes",'MaxSPRT'),
                      labels = c('Bayesian','MaxSPRT'))

  return(pg)
}


