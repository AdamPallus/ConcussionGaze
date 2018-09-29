


ggplotly(
  h %>%
    filter(block=='UG56ST2',trialnum==36) %>%
    ggplot()+
    geom_point(aes(counter,G),color='orange')+
    geom_point(aes(counter,Gnospline),color='darkgreen')+
    geom_line(aes(counter,Targ),color='gray50',size=2)+
    geom_line(aes(counter,H),color='darkblue',size=2)+
    ggtitle('Missing data interpolated')
)


badtrials<- filter(hm,abs(peak.gaze.velocity)>650)

manipulate(
  {
    h %>%
      filter(block==badtrials$block[chosenTrial],trialnum==badtrials$trialnum[chosenTrial]) %>%
      ggplot()+
      geom_point(aes(counter,Graw),color='darkgreen')+
      geom_line(aes(counter,G),color='orange')+
      geom_line(aes(counter,Targ),color='gray50',size=2)+
      geom_line(aes(counter,H),color='darkblue',size=2)+
      geom_line(aes(counter,Gv/10),color='hotpink')+
      ggtitle(badtrials$block[chosenTrial])
      
  },
  chosenTrial=slider(1,length(badtrials))
)
  