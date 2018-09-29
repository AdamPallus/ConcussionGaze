#cluster


status<- numeric()
status[['bU07']]= 'concusssed'
status[['Cj21']]= 'control'
status[['CP48']]= 'concussed'
status[['Fq58']]= 'control'
status[['gN73']]= 'concussed'
status[['hS83']]= 'control'
status[['UG56']]= 'control'
status[['VO35']]= 'concussed'
status[['wR49']]= 'control'
status[['xL16']]= 'concussed'

concussed<- c('bU07','CP48','gN73','VO35','xL16')


hclust<-select(hm,subject,block,blocknum,gaze.onset,gaze.dur,gaze.amp,total.gaze.offset,total.gaze.amp,total.gaze.dur,
               peak.gaze.velocity,gaze.steps,IHP,IEP,IGP,IEPr,head.contribution,total.head.contribution,
               head.onset,peak.head.velocity,head.amp,target.amp)

hclust %>%
  mutate(post.concussion=subject %in% concussed & blocknum>1)->
  hclust

hclust<-hclust[complete.cases(hclust),]

km<- kmeans(select(hclust,-block,-blocknum,-subject),2)

hclust$cluster<-km$cluster

hclust %>%
  group_by(post.concussion,block,cluster) %>%
  tally() %>%
  group_by(block) %>%
  mutate(peak.cluster=cluster[n==max(n)],
         cluster.ratio=n[cluster==1]/n[cluster==2])->
  hs

ggplot(hs)+
  geom_point(aes(block,cluster.ratio,color=post.concussion))

qplot(cluster.ratio,post.concussion,data=hs)

htrain<- filter(hclust,!subject %in% c('UG56','VO35'),subject !='bU07' )
htest<- filter(hclust,subject %in% c('UG56','VO35'))

logmod<-glm(post.concussion~.,family=binomial(link='logit'),data=select(htrain,-block,-blocknum,-subject,-cluster))

anova(logmod, test="Chisq")

htest$prediction=predict(logmod,newdata=select(htest,-block,-blocknum,-subject,-cluster))

qplot(post.concussion,prediction,data=select(htest,-block,-blocknum,-subject,-cluster))

qplot(prediction,data=htest)

qplot(block,prediction,data=htest,geom='boxplot')

htrain$prediction=predict(logmod,newdata=select(htrain,-block,-blocknum,-subject,-cluster))

qplot(block,prediction,data=htrain,geom='boxplot',fill=post.concussion)

qplot(post.concussion,prediction,data=htrain,geom='boxplot')

qplot(prediction,data=htest,fill=post.concussion)

htest %>%
  group_by(block) %>%
  summarize(mpred= mean(prediction))->
  hts

ggplot(hts) +
  geom_bar(aes(block,mpred),stat='identity')

htrain %>%
  group_by(block,post.concussion) %>%
  summarize(mpred= mean(prediction))->
  hts

ggplot(hts) +
  geom_bar(aes(block,mpred,fill=post.concussion),stat='identity')
