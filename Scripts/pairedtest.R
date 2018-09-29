
concussed<- c('bU07','CP48','gN73','VO35','xL16')

hpp %>%
  group_by(subject,blocknum,amp.bins.15) %>%
  summarize(mean.gaze.velocity=mean(abs(peak.gaze.velocity))) %>%
  filter(as.numeric(blocknum)<=2,
         !is.na(amp.bins.15),
         subject != 'wR49') %>%
  mutate(concussed = subject %in% concussed)->
  s

s$concussed<- as.factor(s$concussed)
levels(s$concussed)<-c('Control','Concussed')


ggpaired(s,x='blocknum',y='mean.gaze.velocity',
         palette = 'npg',color='blocknum',line.color = "gray", line.size = 0.4)
         
         
 s %>%
   spread(key = blocknum,value=mean.gaze.velocity)->
   ss
 
ggpaired(ss,cond1='1',cond2='2',facet='concussed',
         palette = 'npg',line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired=TRUE)+
  xlab('Time Point')+
  ylab('Peak Gaze Shift Velocity')


s$blocknum<-as.factor(s$blocknum)



library(lme4)

hm %>%
  mutate(amp.bins.15 = cut(abs(target.amp),breaks=c(0,15,30,45,60,75,90))) %>%
  filter(as.numeric(blocknum)<=2,
         !is.na(amp.bins.15),
         subject != 'wR49') %>%
  mutate(concussed = subject %in% concussed)->
  hl

mod<- lmer(concussed ~ peak.gaze.velo)
