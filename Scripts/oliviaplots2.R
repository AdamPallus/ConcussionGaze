
filenames<- c('Gv','HC','Glat','Gdur','Ggain','Gsteps')

yvars<- c('abs(peak.gaze.velocity)','abs(head.contribution)','gaze.onset.ms',
          'gaze.dur.ms','gaze.gain','gaze.steps')

ylabs<- c('Peak gaze velocity (deg/s)','Head contribution (deg)','Gaze latency (ms)',
          'Gaze duration (ms)','Gaze gain','Gaze steps')

ylabspot<- c(700,50,650,650,3,3)

subjects<-c('xL16')

dataset %>%
  filter(subject %in% subjects,
         gaze.gain>0) %>%
  mutate(blocknum=as.numeric(blocknum)) %>%
  filter(blocknum>2) %>%
  mutate(blocknum=blocknum-4)->
  dolivia
         

for (i in seq_along(subjects)){
  fn<- paste0(filenames,subjects[i],'.pdf')
  toPlot<-filter(dolivia,
                 # amp.bins.15=='(30,45]',
                 subject==subjects[i])
  for (j in seq_along(fn)){
    saveXL(toPlot,fn[j],widt=15,height=5,
                   y=yvars[j],ylabel=ylabs[j],
           ylabspot=ylabspot[j])
  }
}

saveXL<-function(plt,filename,width=4,height=6,y='abs(peak.gaze.velocity)',
                 xlabel='Time point',ylabel='Peak gaze velocity (deg/s)',
                 ylabspot=700){
  cairo_pdf(filename,width=width,height=height)
  p<-plt %>%
    ggboxplot(x = "blocknum", y=y,
              color = "blocknum", palette = "jco",
              # facet.by = c("subject","amp.bins.15"), short.panel.labs = TRUE)
              facet.by = c("subject","amp.bins.15"), short.panel.labs = TRUE) + 
    stat_compare_means(label.y=ylabspot)+
    stat_compare_means(comparisons = list(c('1','2'),c('1','3'),c('2','3')))+
    # stat_compare_means(label = "p.signif")+
    xlab('Time point')+
    ylab(ylabel)+
    theme(legend.position = 'none')
  print(p)
  dev.off()
}
