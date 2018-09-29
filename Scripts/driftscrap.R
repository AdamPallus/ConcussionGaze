
h<- left_join(h,hm)


library(manipulate)
library(plotly)

blocks<-unique(h$block)
ntrials<-length(unique(h$trialnum))

manipulate(h %>%
             filter(block==blocks[chosenBlock],trialnum==chosenTrial)%>%
             ggplot()+
             geom_line(aes(counter,G),color='darkgreen')+
             geom_line(aes(counter,Targ),alpha=0.5,size=2)+
             geom_line(aes(counter,G*sign(gazeshifts)),size=2,alpha=0.5,color='darkgreen'),
           
           chosenBlock=slider(1,length(blocks)),
           chosenTrial=slider(1,ntrials,step=1))


manipulate(h %>%
             filter(block==blocks[chosenBlock],trialnum==chosenTrial)%>%
             ggplot()+
             geom_line(aes(counter,Gv),color='darkgreen')+
             # geom_line(aes(counter,Targ),alpha=0.5,size=2)+
             geom_line(aes(counter,Gv*sign(gazeshifts)),size=2,alpha=0.5,color='darkgreen'),
           
           chosenBlock=slider(1,length(blocks)),
           chosenTrial=slider(1,ntrials,step=1))


manipulate({d<-h %>%
             filter(block==blocks[chosenBlock],trialnum==chosenTrial)
              multiplot(

             ggplot(d)+
             geom_line(aes(counter,G),color='darkgreen')+
             geom_line(aes(counter,Targ),alpha=0.5,size=2)+
             geom_line(aes(counter,G*sign(gazeshifts)),size=2,alpha=0.5,color='darkgreen')+
               geom_line(aes(counter,H),color='blue'),
             ggplot(d)+
               geom_line(aes(counter,Gv),color='darkgreen')+
               # geom_line(aes(counter,Targ),alpha=0.5,size=2)+
               geom_line(aes(counter,Gv*sign(gazeshifts)),size=2,alpha=0.5,color='darkgreen')+
               geom_line(aes(counter,Hv),color='blue')
             )},
           
           chosenBlock=slider(1,length(blocks)),
           chosenTrial=slider(1,ntrials,step=1))

htest %>%
  filter(!is.na(trialnum)) %>%
  group_by(task,subject,block,trialnum) %>%
  do(measureTrial(.))->
  hm

hplot<-filter(hm,missing.gs<5)

qplot(post.saccade.VOR,binwidth=.1,data=filter(hplot,abs(post.saccade.VOR)<5))+
  facet_wrap(~block,ncol=4)+
  geom_vline(xintercept = -1)
  

ggplot(hplot)+
  geom_point(aes(post.saccade.head,post.saccade.drift,color=block))

ggplot(filter(hplot,abs(post.saccade.head)>5),
       aes(abs(post.saccade.head),abs(post.saccade.drift),color=block))+
  geom_point()+
  stat_smooth(method='lm',se=FALSE)+
  geom_abline()


ggplot(filter(hplot,abs(post.saccade.head)>5),
       aes(post.saccade.head,post.saccade.drift,color=block))+
  geom_point()+
  stat_smooth(se=FALSE)

drifters<-filter(hplot,abs(post.saccade.drift)>10,abs(post.saccade.head)>10)
drifters<-select(ungroup(drifters),block,trialnum,post.saccade.head,post.saccade.drift,gaze.offset)

ggplot(drifters)+
      geom_point(aes(post.saccade.head,post.saccade.drift,color=block))+
  geom_abline()

ggplot(drifters,aes(abs(post.saccade.head),abs(post.saccade.drift),color=block))+
  geom_point()+
  geom_abline()+
  stat_smooth(method='lm',se=FALSE)

ggplot(hplot,aes(abs(post.saccade.head),abs(post.saccade.drift),color=block))+
  geom_point()+
  geom_abline()+
  stat_smooth(method='lm',se=FALSE)

ggplot(filter(hplot,abs(post.saccade.head)>10),aes(post.saccade.head,post.saccade.eye,color=block))+
  geom_point()+
  geom_abline(slope=-1)+
  stat_smooth(method='lm',se=FALSE)

ggplot(hplot,aes(post.saccade.eye/post.saccade.head,color=block))+
  geom_density()+
  xlim(-10,10)+
  geom_vline(xintercept = -1,color='red',size=2,alpha=0.5)



ggplot(filter(hplot,abs(post.saccade.VOR)<10),aes(post.saccade.head,post.saccade.VOR,color=block))+
  geom_point()+
  geom_hline(yintercept = -1)

# drifters<- filter(hplot,abs(post.saccade.VOR<10),abs(post.saccade.head)>15)

drifters %>%
  group_by(block) %>%
  tally()->
  driftcount

ggplot(driftcount,aes(block,n))+
  geom_bar(stat='identity')


d<- filter(drifters,block=='xL16ST6')
hpp<-filter(h,block=='xL16ST6')
trials<- unique(d$trialnum)

manipulate({dd<-hpp %>%
  filter(trialnum==trials[chosenTrial])
multiplot(
  
  ggplot(dd)+
    geom_line(aes(counter,G),color='darkgreen')+
    geom_line(aes(counter,Targ),alpha=0.5,size=2)+
    geom_line(aes(counter,G*sign(gazeshifts)),size=2,alpha=0.5,color='darkgreen')+
    geom_line(aes(counter,H),color='blue'),
  ggplot(dd)+
    geom_line(aes(counter,Gv),color='darkgreen')+
    # geom_line(aes(counter,Targ),alpha=0.5,size=2)+
    geom_line(aes(counter,Gv*sign(gazeshifts)),size=2,alpha=0.5,color='darkgreen')+
    geom_line(aes(counter,Hv),color='blue')
)},

chosenTrial=slider(1,length(trials),step=1))
driftersx<-select(drifters,block,trialnum,post.saccade.head,post.saccade.drift,gaze.offset)

hf<-filter(h,block=='xL16ST1',trialnum==2)

ggplot(hf)+
  geom_line(aes(counter,Gv),color='darkgreen')+
  # geom_line(aes(counter,Targ),alpha=0.5,size=2)+
  geom_line(aes(counter,Gv*sign(gazeshifts)),size=2,alpha=0.5,color='darkgreen')+
  geom_line(aes(counter,Hv),color='blue')+
  xlim(c(377,407))

