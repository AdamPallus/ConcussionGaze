measureTrial<- function(tt){
  
  #the first block sets everything up and identifies the primary gaze shift and head movement
  tt %>%
    mutate(
      firstshift=min(gazeshifts[counter>220&gazeshifts>0],na.rm=T), 
      firsthead=min(headmovement[counter>220&headmovement>0],na.rm=T),
      primarygazeshifts=replace(gazeshifts,
                                gazeshifts!=firstshift|is.na(firstshift),
                                NA),
      primaryheadmovement=replace(headmovement,
                                  headmovement!=firsthead|is.na(firsthead),
                                  NA),
      # counter=counter-200,
      tcounter=round(counter/samplerate))->
    hh
  
  if (is.na(hh$firstshift[1])){
    message(hh$trialnum[1])
    return(NULL)
    
  }else{
    # message(hh$trialnum[1])
  }
  
  # #Deletes any trials where there is no primary gaze shift
  # hh %>% 
  #   group_by(block,trialnum) %>%
  #  summarize(gaze.dur=length(counter[!is.na(primarygazeshifts)])) %>%
  # ungroup()-> #%>%
  # # filter(gaze.dur>1) ->
  # hint
  
  #We need a block here to measure the intersaccadic interval and reject the late gaze shifts
  
  hh%>%
    summarize(
      gaze.onset=counter[!is.na(primarygazeshifts)][1],
      gaze.offset=max(counter[!is.na(primarygazeshifts)]),
      total.gaze.offset=max(counter[!is.na(gazeshifts)]),
      gaze.dur=gaze.offset-gaze.onset,
      gaze.amp=G[counter==gaze.offset]-G[counter==gaze.onset],
      total.gaze.amp=G[counter==total.gaze.offset]-G[counter==gaze.onset],
      IHP=H[counter==gaze.onset],
      IGP=G[counter==gaze.onset],
      IEP=IGP-IHP,
      IEPs=IEP*sign(gaze.amp), #positive number indicates eyes deviated in direction of saccade
      peak.gaze.velocity=maxabs(Gv[!is.na(gazeshifts)]),
      peak.head.velocity=maxabs(Hv[100:n()]),
      target.amp=Targ[300]-Targ[1],
      head.contribution=H[counter==gaze.offset]-IHP,
      eye.contribution=gaze.amp-head.contribution
    ) ->
    
    hp
  
  # hh <- left_join(hh,hp) #add mesurements back to original data file
}