hh %>%
  group_by(sacnum) %>%
  summarize(start_time=first(time))->
  stimes

hh %>% group_by(sacnum) %>%
  mutate(reject=as.factor(any(is.na(G[200:500]))))->
  hh

levels(hh$reject)<- c('red','black')


manipulate(
  ggplot(filter(hh,sacnum==chosenMovement))+
    geom_line(aes(counter,H),color='blue')+
    # geom_line(aes(counter,Ef),color='red')+
    geom_line(aes(counter,G),color='darkgreen')+
    geom_line(aes(counter,T,color=reject),size=2,alpha=0.5)+
    scale_color_manual(values=c('black','red'))+
    geom_line(aes(counter,Hv/10),color='blue',linetype=2)+
    # geom_line(aes(counter,EV/10),color='red')+
    geom_line(aes(counter,Gv/10),color='darkgreen',linetype=2)+
    # geom_text(x=0,y=50,aes(label=round(sd(Gv),2)))+
    geom_point(aes(counter,sign(gazeshifts)*50))+
    geom_point(aes(counter,sign(headmovement)*25),color='blue')+
    annotate('text',x=-100,y=50,label='Target Position')+
    annotate('text',x=500,y=20,label='Head Position')+
    annotate('text',x=320,y=5,label='Head Velocity')+
    annotate('text',x=270,y=40,label='Gaze Velocity')+
    annotate('text',x=250,y=70,label='Gaze Position')+
    annotate('text',x=125,y=55,label='First Gaze Shift')+
    annotate('text',x=300,y=28,label='Head Movement')+
    geom_line(aes(counter,sign(gazeshifts)*Gv/10),size=2,color='darkgreen',alpha=0.5)+
    geom_line(aes(counter,sign(headmovement)*Hv/10),color='blue',size=2,alpha=0.5),
  chosenMovement=slider(1,length(unique(hh$sacnum)),step=1))

#interpolating----
source('removeBlinks.R')
func<-splinefun(x=hh$time,y=hh$G,method="fmm")
hh %>%
  group_by(block,sacnum) %>%
  do(applyspline(.,5))->
  hh2

applyspline<-function(hh,buffer=5){
  # message(paste0(nrow(hh),hh$sacnum[1]))
  #window function
  
  xout<- which(is.na(hh$G))
  # xout<-which(is.na(hh$G))
  if (length(xout)>0){
    interp<- removeBlinks(smooth(hh$G),buffer)
    # hh$interp<-splinefun(hh$time,interp,method="monoH.FC")(hh$time)
    hh$interp<-splinefun(hh$time,interp,method="natural")(hh$time)
    hh$interp2<-smooth.spline(hh$time,interp)
    hh
    # hh<-mutate(hh,interp=replace(G,xout,spline(time,G,xout=xout,method='fmm')$y))
  }else{
    hh$interp=hh$G
    # message('none missing')
   hh 
  }
}
  


ff <- filter(hh,sacnum==1,block=='A-01')
ff<-applyspline(ff,5)

ggplot(ff)+
  geom_line(aes(time,interp),color='orange')+
  geom_point(aes(time,G),color='darkgreen')


G<-ff$G

plot(2:length(G),diff(G))

plot(G)







manipulate(ggplot(filter(hh2,block=="A-02",sacnum==sac))+
             # geom_line(aes(time,interp),color='orange')+
             geom_point(aes(time,Gv/10),color='black')+
             geom_point(aes(time,G),color='darkgreen'),
           sac=slider(1,100))


hh<- mutate(hh,interp=replace(G,is.na(G),time[func(is.na(G))]))
 hh %>%
   filter(sacnum==1,block=="A-01") %>%
   ggplot()+
   geom_line(aes(time,G),color='green')+
   geom_line(aes(time,interp),color='yellow')
func



#plotting..----


manipulate({xx<- hh %>% filter(block=='A-01',sacnum==sac)
missing<- sum(is.na(xx$Graw2))
timeplot<- first(xx$time)
ggplot(xx)+  
  geom_point(aes(tcounter,G),color='orange')+
  geom_point(aes(tcounter,Graw),color='darkgreen')+
  # geom_point(aes(time,smooth(Graw),color='hotpink'))+

  # geom_point(aes(tcounter,Gv/10),color='hotpink')+
  annotate('text',x=100,y=10,label=paste0('missing samples: ',missing))
},
sac=slider(1,100))

hh %>%
  group_by(block,sacnum) %>%
  summarize(missing=sum(is.na(Graw2)))->
  m

qplot(missing,data=filter(m,missing>0,missing<200))+facet_wrap(~block)


hpp %>% 
  group_by(block,sacnum) %>% 
  summarize(missingGS=missingGS[1],missing=missing[1])-> 
  hpps

hh %>%
  group_by(block,sacnum) %>%
  do(m=plotTrial(.))->
  hhplots

plotTrial<- function(hh){
  # cat(hh$blinktrial[1])
  gp<- ggplot(hh)+
    geom_line(aes(tcounter,H),color='blue')+
    # geom_line(aes(tcounter,Ef),color='red')+
    geom_point(ae(tcounter,Graw2),color='darkgreen')+
    geom_point(aes(tcounter,G),color='orange')+
    geom_line(aes(tcounter,T),size=2,alpha=0.5)+
    geom_line(aes(tcounter,Hv/10),color='blue',linetype=2)+
    # geom_line(aes(tcounter,EV/10),color='red')+
    geom_line(aes(tcounter,Gv/10),color='darkgreen',linetype=2)+
    # geom_text(x=0,y=50,aes(label=round(sd(Gv),2)))+
    geom_point(aes(tcounter,sign(gazeshifts)*50),color='darkgreen')+
    geom_point(aes(tcounter,sign(headmovement)*25),color='blue')+
    geom_line(aes(tcounter,sign(primarygazeshifts)*Gv/10),size=2,color='darkgreen',alpha=0.5)+
    # geom_line(aes(tcounter,sign(gazeshifts)*Gv/10),size=2,color='yellow',alpha=0.5)+
    geom_line(aes(tcounter,sign(primaryheadmovement)*Hv/10),color='blue',size=2,alpha=0.5)+
    xlab('Time (ms)')
  
  # geom_line(aes(tcounter,sign(headmovement)*Hv/10),color='blue',size=2,alpha=0.5)+
  # blink<- hh$blinktrial[1]
  # if(blink | is.na(blink)){
  #   gp<-gp+geom_label(x=0,y=0,label='BLINK INTERRUPTED')
  # }
  gp
  
}
