#junk----
h %>% group_by(block) %>%
  mutate(Gv=parabolicdiff(G,7)*samplerate, #calculate velocity
         Hv=parabolicdiff(H,7)*samplerate,
         gazeshifts=markMovementsDouble(Gv,threshold1=100,threshold2=10),
         headmovement=markMovementsDouble(Hv,threshold1=10,threshold2=4))->
  tt

measureTrial<- function(tt){
  tt %>%
    mutate(
      firstshift=min(gazeshifts[counter>220&gazeshifts>0],na.rm=T), 
      firsthead=min(headmovement[counter>220&headmovement>0],na.rm=T),
      primarygazeshift=replace(gazeshifts,
                                gazeshifts!=firstshift|is.na(firstshift),
                                NA), #the first "eye saccade" we're calling the primary gaze shift
      primaryheadmovement=replace(headmovement,
                                  headmovement!=firsthead|is.na(firsthead),
                                  NA),
      counter=counter-200,
      tcounter=round(counter/samplerate))->
    hh
}

tt %>% 
  group_by(block,trialnum) %>%
  do(measureTrial(.))->
  tt


tt<- filter(h,block=='A-01',trialnum==12)


hh%>%
  summarize(
    gaze.onset=counter[!is.na(primarygazeshift)][1],
    gaze.offset=max(counter[!is.na(primarygazeshift)]),
    total.gaze.offset=max(counter[!is.na(gazeshifts)]),
    gaze.dur=gaze.offset-gaze.onset,
    gaze.dur.ms=gaze.dur/samplerate)->
  hhtest

#Measure Trials----
measureTrial<- function(tt, buffer=200){
  #This function will receive data from one trial and return a one-row data frame
  
  trial.length<- nrow(tt)
  
  #identify the id number of the first saccade that satisfies the criteria 
  firstshift=min(tt$gazeshifts[buffer:trial.length],na.rm=T)
  
  if (is.infinite(firstshift)){
    #Don't bother measuring this trial if there isn't a primary gaze shift
    message(paste0('No primary gaze shift in: ',hh$trialnum[1]))
    return(NULL)
  }
  
  #In this block, we will reject any saccades with a delay of more than 150ms since the last saccade
  #we will also reject saccades where the peak velocity is less than 200 ms as we assume they are corrective
  
  if (length(unique(tt$gazeshifts)) > 2){
    tt %>%
      filter(!is.na(gazeshifts)) %>%
      group_by(gazeshifts) %>%
      summarize(start.time=first(time),
                stop.time=last(time),
                peak.gaze.velocity=max(Gv)) %>%
      mutate(interval= start.time-lag(stop.time)) %>%
      filter(interval<47, peak.gaze.velocity>200) -> #reject saccades that are separated by too much time (47 samples ~ 150 ms)
      isi
    good.saccades<- c(firstshift, isi$gazeshifts)
    
    #delete them from the marking
    good.gazeshifts=replace(tt$gazeshifts,!tt$gazeshifts %in% good.saccades,NA)
  }else{
    good.saccades<- firstshift
    good.gazeshifts<- tt$gazeshifts
  }
  
  #measure gaze
  gaze.onset=first(tt$counter[good.gazeshifts==firstshift&!is.na(good.gazeshifts)])
  gaze.offset=last(tt$counter[good.gazeshifts==firstshift&!is.na(good.gazeshifts)])
  gaze.dur=gaze.offset-gaze.onset
  gaze.amp=tt$G[gaze.offset]-tt$G[gaze.onset]
  
  total.gaze.offset=max(tt$counter[!is.na(good.gazeshifts)])
  total.gaze.amp=tt$G[tt$counter==total.gaze.offset]-tt$G[tt$counter==gaze.onset]
  
  peak.gaze.velocity=maxabs(tt$Gv[good.gazeshifts==firstshift])
  IHP=tt$H[tt$counter==gaze.onset]
  IGP=tt$G[tt$counter==gaze.onset]
  IEP=IGP-IHP
  
  
  #measure head
  firsthead=min(tt$headmovement[buffer:trial.length],na.rm=T)
  #set up a flag to determine whether we will be measuring head movements
  head.onset<-NA
  head.offset<-NA
  peak.head.velocity<-NA
  head.amp<-NA
  if (!is.infinite(firsthead)){
    head.onset=first(tt$counter[tt$headmovement==firsthead&!is.na(tt$headmovement)])
    head.offset=last(tt$counter[tt$headmovement==firsthead&!is.na(tt$headmovement)])
    if (head.onset<total.gaze.offset){ #ignore head movements that start after the gaze shifts are done
    peak.head.velocity=maxabs(tt$Hv[tt$headmovement==firsthead])
    head.amp=tt$H[tt$counter==head.offset]-tt$H[tt$counter==head.onset]
    }
  }

  #measure target
  target.amp=hh$Targ[300]-hh$Targ[1]
  
  tt %>%
    ungroup() %>%
    summarize_at(c('block','trialnum'),funs(first))->
    d
  
  df<- data.frame(firstshift,
                  gaze.onset,
                  gaze.offset,
                  gaze.dur,
                  gaze.amp,
                  total.gaze.offset,
                  total.gaze.amp,
                  peak.gaze.velocity,
                  IHP,
                  IGP,
                  IEP,
                  firsthead,
                  head.onset,
                  head.offset,
                  peak.head.velocity,
                  head.amp,
                  target.amp)
  
  # message(paste0('Success: ',tt$trialnum[1],'\n'))
  d<- bind_cols(d,df)

}

h %>%
  filter(!is.na(trialnum)) %>%
  group_by(block,trialnum) %>%
  do(measureTrial(.))->
  hms

hm %>%
  group_by(block,trialnum) %>%
  summarize_all(funs(first))->
  hms
