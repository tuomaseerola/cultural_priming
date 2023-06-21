visualise <- function(data=NULL,titletext='Condition'){
  
  df <- data
  

  
  #### 2 Words ------------
  library(dplyr)

  S1 <- df %>%
    group_by(TargetValence, PrimeValence) %>%
    summarise(n=n(),m=mean(RT,na.rm = TRUE),sd=sd(RT,na.rm = TRUE),.groups = "drop") %>%
    mutate(se=sd/sqrt(n),LCI=m+qnorm(0.025)*se,UCI=m+qnorm(0.975)*se) 
  #S1
  
  pd <- position_dodge(.333) # move them .05 to the left and right
  #print(S1)
  g1 <- ggplot(S1,aes(x = TargetValence, y = m, color = PrimeValence, shape = PrimeValence, linetype = PrimeValence)) +
    geom_line(aes(group = PrimeValence),position = pd,show.legend = FALSE) +
    geom_point(aes(group = PrimeValence),position = pd,size=3,show.legend = TRUE)+
    #scale_color_brewer(type = 'seq',palette = 'Set1')+
    scale_linetype_manual(name="Prime Valence", values = c('solid','dashed'))+
    scale_shape_manual(name="Prime Valence", values = c(15,16))+
    scale_color_manual(name="Prime Valence", values = c('black','gray70'))+
    geom_errorbar(S1, mapping=aes(x=TargetValence, ymin=LCI, ymax=UCI), position = pd, width = 0.1, linewidth = 0.50, 
                  show.legend = FALSE, alpha = 1.0,linetype='solid')+
    ylab('Mean RT ± 95% CI')+
    xlab('Target Valence')+
    scale_y_continuous(limits = c(528,570))+
    ggtitle(titletext)+
    see::theme_modern()+
    theme(text=element_text(size=16,  family="Palatino"))+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(legend.justification=c(1,0), legend.position=c(1,0.8))
#    theme(legend.position="bottom")
    #theme_bw()
  #g7  
  
  
  S1 <- data %>%
    group_by(TargetValence, PrimeValence) %>%
    summarise(n=n(),m=mean(RT,na.rm = TRUE),sd=sd(RT,na.rm = TRUE),.groups = "drop") %>%
    mutate(se=sd/sqrt(n),LCI=m+qnorm(0.025)*se,UCI=m+qnorm(0.975)*se) 
  S1
  
  con<-1
  
  g3 <- ggplot(data,aes(y = RT, x = TargetValence,fill=PrimeValence, color = PrimeValence)) +
    stat_dotsinterval(position = "dodge",point_interval = mean_qi) +
    scale_color_grey(name="Prime", start = 0.0,end = 0.6) +
    scale_fill_grey(name="Prime") +
    xlab("Target")+
    ylab("Mean RT ± QI")+
    ggtitle(titletext)+
    scale_y_continuous(limits = c(360,780))+
    #stat_halfeye(position = "dodge") +
    see::theme_modern()+
    theme(text=element_text(size=16,  family="Palatino"))+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(legend.justification=c(1,0), legend.position=c(0.17,0.8),legend.key = element_rect(colour = "transparent", fill = "transparent"),legend.background=element_blank()) +
    guides(color=guide_legend(override.aes=list(fill=NA))) +
    annotate("segment", y = S1$m[1]-con, yend = S1$m[2]-con, x = 0.75, xend = 1.25,linetype='dashed')+
    annotate("segment", y = S1$m[3]-con, yend = S1$m[4]-con, x = 1.75, xend = 2.25,linetype='dashed')+
    annotate("text", y = S1$m[1]-con, x = 0.75-0.09, label=round(S1$m[1]),  family="Palatino",size=3)+
    annotate("text", y = S1$m[2]-con, x = 1.25+0.09, label=round(S1$m[2]),  family="Palatino",size=3)+
    annotate("text", y = S1$m[3]-con, x = 1.75-0.09, label=round(S1$m[3]),  family="Palatino",size=3)+
    annotate("text", y = S1$m[4]-con, x = 2.25+0.09, label=round(S1$m[4]),  family="Palatino",size=3)
  
  
  #### 9 GGHALEVS --------
  
  library(gghalves)
  
  pos_adj <- 1.2
  source('~/Documents/computational/R/theme_fs.R')
  custom_theme_size <- theme_fs(14)
  
#  head(df)
  g2 <- ggplot(df, aes(x = TargetValence, y = RT,fill = PrimeValence)) +
    geom_point(aes(x = TargetValence, y = RT,color=PrimeValence), show.legend = NA,
               #     position = position_dodge(width = pos_adj),
               size = 1.3,
               alpha = .3,
               position = position_jitterdodge(
                 seed = NA, dodge.width = pos_adj, jitter.width = .3, jitter.height = .3
               )
    )+
    ggdist::stat_halfeye(
      position = position_dodge(width = pos_adj),
      adjust = 1.0, 
      width = .6,
      color = 'black',
      .width = 0, 
      alpha=0.5,
      justification = -.3, 
      point_colour = NA) + 
    
    ## BOXPLOT
    # geom_boxplot(show.legend = NA,
    #              position = position_dodge(width = pos_adj),
    #              width = .25, 
    #              outlier.shape = NA
    # ) +

#    geom_point(position = position_dodge(width = pos_adj), stat = 'summary', fun = "mean",size=9)+
    stat_summary(aes(x = TargetValence, y = RT,color=PrimeValence),
                 fun = mean,
                 size = 3,
                 shape = 15,
                 position = position_dodge(width = pos_adj),
                 geom = "pointrange",
                 fun.max = function(x) mean(x) + sd(x) / sqrt(length(x)),
                 fun.min = function(x) mean(x) - sd(x) / sqrt(length(x))) +
        
    #geom_line(position = position_dodge(width = pos_adj))+
    #  facet_wrap(.~Interval,scales = "free_x",nrow = 4,ncol = 3)+
    #  scale_y_continuous(breaks = seq(1,7,by=3),limits = c(0.5,7.5))+
    scale_color_manual(values = c('purple3','green4'))+
    scale_fill_manual(values = c('purple3','green4'))+
    #  scale_fill_brewer(values = c('purple3','green4'))+
    #  coord_cartesian(xlim = c(1.2, NA), clip = "off")+
    #  ylab('Tension Rating')+
    theme_bw()+
    #  guides(color = guide_legend(override.aes = list(color = c('NA', 'NA'))))+
    theme(legend.position="bottom")+
    ggtitle(titletext)+
    custom_theme_size
  #g9  
  

    GRAPHS <- list(g1,g3)
  
  return <- GRAPHS
#  return <- list(g1,g2,g3,g4,g5,g5,g6,g7,g8,g9,g10)
}