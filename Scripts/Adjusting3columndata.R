#'Saturday morning proceedure for loading the 3-column format
#'We have only HV, E and Targ
#'We caclculate H using the AdjustCalibration function and no gain or offset
#'Then we run fixH which just fits a 3rd order polynomial and subtracts that
#'from the head signal to fix the drift
#'Then we go back to the analysis script and run it as normal


h %>%
  group_by(block) %>%
  do(AdjustCalibration(.,applyfilter = FALSE)) %>%
  do(fixH(.)) %>%
  mutate(G=H+E)->
  ht

qplot(time,E,data=ht,color=block,geom='line')




fixH<- function(t){
  mod<- lm(H~poly(time,3),data=t)
  t %>%
    mutate(H=H-predict(mod,newdata=t))->
    t
  return(t)
}
  

hcombined %>%
  group_by(block) %>%
  summarize(total=n(),
            anti=sum((gaze.amp*target.amp)<1),
            percent=anti/total*100)->
  hsum

ggplot(hsum)+
  geom_bar(aes(block,percent),stat='identity')
