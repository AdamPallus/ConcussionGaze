library(ggpubr)
samplerate<- 304.7508/1000
hm %>%
  mutate(blocknum=as.factor(str_sub(block,7)),
         gaze.onset.ms=(gaze.onset-200)/samplerate,
         head.onset.ms=(head.onset-200)/samplerate,
         gaze.dur.ms=(gaze.dur)/samplerate,
         IEPr=IEP*sign(gaze.amp),
         gaze.gain=gaze.amp/target.amp,
         anti_saccade=(gaze.amp*target.amp)<0) %>%
  filter(missing.gs<15,
         gaze.onset.ms>150,
         gaze.onset.ms<500,
         abs(peak.gaze.velocity) > 200) %>%
  ungroup() %>%
  select(peak.gaze.velocity,
         gaze.onset.ms,
         head.onset.ms,
         gaze.dur.ms,
         gaze.gain,
         anti_saccade,
         peak.head.velocity,
         head.contribution,
         gaze.amp,
         gaze.dur,
         subject,
         block,
         blocknum,
         trialnum,
         target.amp,
         head.amp,
         IEPr,
         gaze.steps,
         total.gaze.amp,
         total.gaze.dur,
         total.head.contribution)->
  hpp

hpp <- mutate(hpp,amp.bins.20 = cut(abs(target.amp),breaks=c(0,20,40,60,80)))

hpp <- mutate(hpp,amp.bins.15 = cut(abs(target.amp),breaks=c(0,15,30,45,60,75,90)))

hpp <- mutate(hpp,amp.bins.10 = cut(abs(target.amp),breaks=c(0,10,20,30,40,50,60,70,80,90)))

hpp <- mutate(hpp,gaze.amp.bins.10 = cut(abs(gaze.amp),breaks=c(0,10,20,30,40,50,60,70,80,90)))

#remove NA bins and extra blocks
hpp %>%
  filter(!is.na(amp.bins.15),
         gaze.gain>0.5)->
  hpp

hpp %>%
  mutate(blocknum= as.factor(blocknum),
         block= as.factor(block),
         subject=as.factor(subject))->
  dataset

plot.vel<-ggboxplot(dataset, x = "blocknum", y='abs(peak.gaze.velocity)',
                    color = "blocknum", palette = "jco",
                    add = "jitter",
                    # facet.by = c("subject","amp.bins.15"), short.panel.labs = TRUE)
                    facet.by = c("subject","amp.bins.15"), short.panel.labs = TRUE) + 
  stat_compare_means(label = "p.signif")+
  xlab('Time point')+
  ylab('Gaze peak velocity (deg/s)')

plot.latency<-ggboxplot(dataset, x = "blocknum", y='gaze.onset.ms',
                        color = "blocknum", palette = "jco",
                        add = "jitter",
                        # facet.by = c("subject","amp.bins.15"), short.panel.labs = TRUE)
                        facet.by = c("subject","amp.bins.15"), short.panel.labs = TRUE) + 
  stat_compare_means(label = "p.signif")+
  xlab('Time point')+
  ylab('Gaze latency (ms)')

plot.dur<-ggboxplot(dataset, x = "blocknum", y='gaze.dur.ms',
                    color = "blocknum", palette = "jco",
                    add = "jitter",
                    # facet.by = c("subject","amp.bins.15"), short.panel.labs = TRUE)
                    facet.by = c("subject","amp.bins.15"), short.panel.labs = TRUE) + 
  stat_compare_means(label = "p.signif")+
  xlab('Time point')+
  ylab('Gaze duration (ms)')

plot.gain<-ggboxplot(dataset, x = "blocknum", y='gaze.gain',
                     color = "blocknum", palette = "jco",
                     add = "jitter",
                     # facet.by = c("subject","amp.bins.15"), short.panel.labs = TRUE)
                     facet.by = c("subject","amp.bins.15"), short.panel.labs = TRUE) + 
  stat_compare_means(label = "p.signif")+
  xlab('Time point')+
  ylab('Gaze gain')

plot.gain2<-ggboxplot(filter(dataset,gaze.gain>0.3), x = "blocknum", y='gaze.gain',
                     color = "blocknum", palette = "jco",
                     add = "jitter",
                     # facet.by = c("subject","amp.bins.15"), short.panel.labs = TRUE)
                     facet.by = c("subject","amp.bins.15"), short.panel.labs = TRUE) + 
  stat_compare_means(label = "p.signif")+
  xlab('Time point')+
  ylab('Gaze gain')

plot.steps<-ggboxplot(dataset, x = "blocknum", y='gaze.steps',
                      color = "blocknum", palette = "jco",
                      add = "jitter",
                      # facet.by = c("subject","amp.bins.15"), short.panel.labs = TRUE)
                      facet.by = c("subject","amp.bins.15"), short.panel.labs = TRUE) + 
  stat_compare_means(label = "p.signif")+
  xlab('Time point')+
  ylab('Gaze number of steps')

plot.hcont<-ggboxplot(dataset, x = "blocknum", y='abs(head.contribution)',
                      color = "blocknum", palette = "jco",
                      add = "jitter",
                      # facet.by = c("subject","amp.bins.15"), short.panel.labs = TRUE)
                      facet.by = c("subject","amp.bins.15"), short.panel.labs = TRUE) + 
  stat_compare_means(label = "p.signif")+
  xlab('Time point')+
  ylab('Head Contribution')

plot.hamp<-ggboxplot(dataset, x = "blocknum", y='abs(head.amp)',
                      color = "blocknum", palette = "jco",
                      add = "jitter",
                      # facet.by = c("subject","amp.bins.15"), short.panel.labs = TRUE)
                      facet.by = c("subject","amp.bins.15"), short.panel.labs = TRUE) + 
  stat_compare_means(label = "p.signif")+
  xlab('Time point')+
  ylab('Head Contribution')

plot.vel
plot.gain
plot.dur
plot.steps
plot.latency
plot.hcont
plot.hamp


ggplot(dataset,aes(target.amp,total.gaze.amp,color=blocknum)) +
  geom_point()+
  stat_smooth(method='lm')+
  geom_abline()

qplot(gaze.amp,total.gaze.amp,data=dataset)+geom_abline()
