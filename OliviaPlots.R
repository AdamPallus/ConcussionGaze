
#' #'
#' 1. Could you change the x-axes labels to "Time point" instead of "block"?
#' 
#' 
#' 2. Could you change the y-axes labels to:
#'   
#'   peak.gaze.velocity --> "Gaze peak velocity (deg/sec)"
#' 
#' gaze.onset.ms --> "Gaze latency (ms)"
#' 
#' gaze.duration.ms --> "Gaze duration (ms)"
#' 
#' gaze.gain ---> "Gaze gain"
#' 
#' gaze steps --> "Gaze number of steps" 
#' 
#' 
#' 3. A It would be great to have the "n" per amp range per time point (per block). 
#'                      
#'                
#' 4. Could you either have the box plots display the means or if it looks too crowded that way, give me the exact means for every amplitude at each time point (block)? 
#'                      
#'                      
#' 5. Could I get the exact p-values for all of the amplitudes at each time point (for significant and not significant)? 
#'                      
#'                      
#' The five measurements that I'm interested in for my thesis are 1) gaze peak velocity, 2) gaze onset (gaze latency),
#'  3) gaze duration, 4) gaze gain, and 5) gaze number of steps. 
#' 
#' #'

goodsubjects<-c('Cj21','Fq58','wR49','CP48','VO35','xL16')

getstats<- function(d,form){
  nblocks<-length(unique(d$blocknum))
  
  p.gv=-10
  p.latency= 10
  p.dur = -10
  p.gain = -10
  p.steps = -10
  if (nblocks == 2){
    p.gv=wilcox.test(abs(peak.gaze.velocity) ~ blocknum,data=d)$p.value
    p.latency= wilcox.test(gaze.onset.ms ~ blocknum,data=d)$p.value
    p.dur = wilcox.test(gaze.dur ~ blocknum,data=d)$p.value
    p.gain = wilcox.test(gaze.gain ~ blocknum,data=d)$p.value
    p.steps = wilcox.test(gaze.steps ~ blocknum,data=d)$p.value
  } 
  
  if (nblocks > 2){
    p.gv=kruskal.test(abs(peak.gaze.velocity) ~ blocknum,data=d)$p.value
    p.latency= kruskal.test(gaze.onset.ms ~ blocknum,data=d)$p.value
    p.dur = kruskal.test(gaze.dur.ms ~ blocknum,data=d)$p.value
    p.gain = kruskal.test(gaze.gain ~ blocknum,data=d)$p.value
    p.steps = kruskal.test(gaze.steps ~ blocknum,data=d)$p.value
  } 
  
  # return(list(p.gv,p.latency,p.dur,p.gain,p.steps))
  l<-list(p.gv,p.latency,p.dur,p.gain,p.steps)
  l[is.na(l)]<- 10
  l<- as.data.frame(l)
  names(l) <- c('p.gv','p.latency','p.dur','p.gain','p.steps')
  
  d %>%
<<<<<<< HEAD
    summarize(median_gv=median(abs(peak.gaze.velocity)),
              median_latency=median(gaze.onset.ms),
              median_dur=median(gaze.dur.ms),
              median_gain=median(gaze.gain),
              median_steps=median(gaze.steps),
              sd.gv=sd(abs(peak.gaze.velocity)),
              sd.latency=sd(gaze.onset.ms),
              sd.dur=sd(gaze.dur.ms),
              sd.gain=sd(gaze.gain),
              sd.steps=sd(gaze.steps),
              n=n()) %>%
=======
    summarize(#mean.gv=mean(abs(peak.gaze.velocity)),
      #mean.latency=mean(gaze.onset.ms),
      #mean.dur=mean(gaze.dur.ms),
      #mean.gain=mean(gaze.gain),
      #mean.steps=mean(gaze.steps),
      median_gv=median(abs(peak.gaze.velocity)),
      median_latency=median(gaze.onset.ms),
      median_dur=median(gaze.dur.ms),
      median_gain=median(gaze.gain),
      median_steps=median(gaze.steps),
      sd_gv=sd(abs(peak.gaze.velocity)),
      sd_latency=sd(gaze.onset.ms),
      sd_dur=sd(gaze.dur.ms),
      sd_gain=sd(gaze.gain),
      sd_steps=sd(gaze.steps),
      n=n()) %>%
>>>>>>> origin/master
    bind_cols(l)->
    l
              
  return(l)
}

dataset %>%
  filter(subject %in% goodsubjects) %>%
  group_by(subject,blocknum,amp.bins.15) %>%
  tally() ->
  dt

dt %>%
  spread(amp.bins.15,n)->
  dtable

dataset %>%
  group_by(subject,amp.bins.15) %>%
  do(getstats(.)) ->
  stats.table

stats.table %>%
  filter(subject %in% goodsubjects)->
  stats.table



write.csv(dtable,'Fall2016_n.csv')
write.csv(stats.table,'Fall2016_stats.csv')


<<<<<<< HEAD
#antisaccade
dataset %>%
  group_by(block,amp.bins.15,anti_saccade) %>%
  do(getstats(.)) %>%
  select(-starts_with("p."))->
  stats.table

=======
#ANTISACCADE FOR KAIA
dataset %>%
  # filter(anti_saccade) %>%
  group_by(subject,anti_saccade,amp.bins.15) %>%
  do(getstats(.)) ->
  stats.table
write.csv(stats.table,'AntisaccadeSummaryTable.csv')
>>>>>>> origin/master

dataset<- filter(dataset, subject %in% goodsubjects)


ggplot(dataset,aes(blocknum,abs(peak.gaze.velocity)))+
  geom_boxplot()+
  theme_minimal()+
  facet_wrap(~subject)



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

plot.steps<-ggboxplot(dataset, x = "blocknum", y='gaze.steps',
                     color = "blocknum", palette = "jco",
                     add = "jitter",
                     # facet.by = c("subject","amp.bins.15"), short.panel.labs = TRUE)
                     facet.by = c("subject","amp.bins.15"), short.panel.labs = TRUE) + 
  stat_compare_means(label = "p.signif")+
  xlab('Time point')+
  ylab('Gaze number of steps')

ggsave('velocity.png',plot.vel,width=10,height=8)
ggsave('latency.png',plot.latency,width=10,height=8)
ggsave('duration.png',plot.dur,width=10,height=8)
ggsave('gain.png',plot.gain,width=10,height=8)
ggsave('gain2.png',plot.gain+ylim(0,2),width=10,height=8)
ggsave('Steps.png',plot.steps,width=10,height=8)

ggsave('velocity.PDF',plot.vel,width=10,height=8)


#four separate plots for the controls 


saveOneSubject<-function(plt,filename,width=10,height=8,y='abs(peak.gaze.velocity)',
                         xlabel='Time point',ylabel='Peak gaze velocity (deg/s)'){
  cairo_pdf(filename,width=width,height=height)
  p<-ggboxplot(plt, x = "blocknum", y=y,
            color = "blocknum", palette = "jco")+
            # add = "jitter") + 
    stat_compare_means(label = "p.signif")+
    xlab(xlabel)+
    ylab(ylabel)
  print(p+theme(legend.position='none')+labs(caption=paste(plt$subject[1],plt$amp.bins.15[1],sep=' ')))
  dev.off()
}

saveSubjects<-function(plt,filename,width=10,height=8,y='abs(peak.gaze.velocity)',
                         xlabel='Time point',ylabel='Peak gaze velocity (deg/s)'){
  cairo_pdf(filename,width=width,height=height)
  p<-ggboxplot(plt, x = "blocknum", y=y,
               color = "blocknum", palette = "jco",
               facet.by = c("subject"), short.panel.labs = TRUE)+ 
    stat_compare_means(label = "p.signif")+
    xlab(xlabel)+
    ylab(ylabel)+
    facet_wrap(~subject,scales = 'free_x')
  print(p+theme(legend.position='none')+labs(caption=plt$amp.bins.15[1]))
  dev.off()
}

# toPlot<-dataset %>%
#   filter(subject=='Fq58',amp.bins.15=='(30,45]')

filenames<- c('Gv','HC','Glat','Gdur','Ggain','Gsteps')

yvars<- c('abs(peak.gaze.velocity)','abs(head.contribution)','gaze.onset.ms',
          'gaze.dur.ms','gaze.gain','gaze.steps')

ylabs<- c('Peak gaze velocity (deg/s)','Head contribution (deg)','Gaze latency (ms)',
          'Gaze duration (ms)','Gaze gain','Gaze steps')

subjects<-c('Cj21','CP48','VO35','xL16')

for (i in seq_along(subjects)){
  fn<- paste0(filenames,subjects[i],'.pdf')
  toPlot<-filter(dataset,
                 amp.bins.15=='(30,45]',
                 subject==subjects[i])
  for (j in seq_along(fn)){
    saveOneSubject(toPlot,fn[j],widt=5,height=5,
                   y=yvars[j],ylabel=ylabs[j])
  }
}

toPlot<- filter(dataset,
                amp.bins.15=='(30,45]',
                subject %in% subjects)
filenames<- paste0(c('GazeVelocity','HeadContribution',
                     'GazeLatency','GazeDuration','GazeGain','GazeSteps'),'.pdf')
for (j in seq_along(filenames)){
  saveSubjects(toPlot,filenames[j],widt=5,height=5,
                 y=yvars[j],ylabel=ylabs[j])
}
saveSubjects(toPlot,'xxx',width=5,height=5,
             y='abs(peak.gaze.velocity)')





####Multiple Comparisons----
dataset %>%
  filter(subject=='xL16',as.numeric(blocknum)>2,amp.bins.15=='(30,45]') %>%
ggboxplot(dataset, x = "blocknum", y='abs(peak.gaze.velocity)',
         color = "blocknum", palette = "npg",
         # facet.by = c("subject","amp.bins.15"), short.panel.labs = TRUE)
         facet.by = c("subject","amp.bins.15"), short.panel.labs = TRUE) + 
  stat_compare_means(label.y=700)+
  stat_compare_means(comparisons = list(c('5','6'),c('5','7'),c('6','7')))+
  # stat_compare_means(label = "p.signif")+
  xlab('Time point')+
  ylab('Gaze peak velocity (deg/s)')+
  theme(legend.position = 'none')

saveXL<-function(plt,filename,width=4,height=6,y='abs(peak.gaze.velocity)',
                 xlabel='Time point',ylabel='Peak gaze velocity (deg/s)'){
  cairo_pdf(filename,width=width,height=height)
  p<-dataset %>%
    filter(subject=='xL16',as.numeric(blocknum)>2,amp.bins.15=='(30,45]') %>%
    ggboxplot(dataset, x = "blocknum", y='abs(peak.gaze.velocity)',
              color = "blocknum", palette = "npg",
              # facet.by = c("subject","amp.bins.15"), short.panel.labs = TRUE)
              facet.by = c("subject","amp.bins.15"), short.panel.labs = TRUE) + 
    stat_compare_means(label.y=700)+
    stat_compare_means(comparisons = list(c('5','6'),c('5','7'),c('6','7')))+
    # stat_compare_means(label = "p.signif")+
    xlab('Time point')+
    ylab('Gaze peak velocity (deg/s)')+
    theme(legend.position = 'none')
  print(p)
  dev.off()
}







#dunntest:https://rcompanion.org/rcompanion/d_06.html
install.packages('FSA')
library(FSA)
dunnTest(abs(peak.gaze.velocity) ~ as.factor(blocknum),data=dataset)
dunnTest(gaze.onset.ms ~ as.factor(blocknum),data=dataset)
dunnTest(gaze.dur.ms ~ as.factor(blocknum),data=dataset)
dunnTest(gaze.gain ~ as.factor(blocknum),data=dataset)
dunnTest(gaze.steps ~ as.factor(blocknum),data=dataset)
?p.adjust


dataset30<- filter(dataset,amp.bins.15=='(30,45]')
dunnTest(abs(peak.gaze.velocity) ~ as.factor(blocknum),data=dataset30)
dunnTest(gaze.onset.ms ~ as.factor(blocknum),data=dataset30)
dunnTest(gaze.dur.ms ~ as.factor(blocknum),data=dataset30)
dunnTest(gaze.gain ~ as.factor(blocknum),data=dataset30)
dunnTest(gaze.steps ~ as.factor(blocknum),data=dataset30)


        