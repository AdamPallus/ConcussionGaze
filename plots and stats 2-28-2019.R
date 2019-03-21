#get dataset from dashboardtest2.Rmd (or gaze.shinyapps.io/dashboardtest2)

dataset %>%
  filter(subject == 'CP48n') ->
  cp

cp %>%
  mutate(iep_centered = case_when(
    abs(IEPr) <= 10 ~ "centered",
    TRUE ~ "off-center"),
         iep_bin= case_when(
    IEPr > 0 & IEPr <=15 ~ '(0,15]',
    IEPr > 15 & IEPr <=30 ~ '(15,30]',
    IEPr < 0 & IEPr >= -15 ~ '[-15,0)',
    IEPr < -15 & IEPr >= -30 ~ '[-30,-15)'),
    gaze_dir = case_when(
      gaze.amp> 0 ~ 'RIGHTWARD',
      gaze.amp<0 ~ 'LEFTWARD'
    )
    ) ->
  cp
                            
  

ggplot(cp %>% filter(amp.bins.15 == '(30,45]', !is.na(iep_bin))) +
  geom_boxplot(aes(block, abs(peak.gaze.velocity)))+
  facet_wrap(~iep_bin)


#peak gaze velocity
p<- ggboxplot(cp %>% filter(amp.bins.15 == '(30,45]', !is.na(iep_bin)),
          x = "block", y = 'abs(peak.gaze.velocity)',
          fill = "blocknum", palette = "jco",
          facet.by = c("subject", "iep_bin"), short.panel.labs = TRUE)

p + stat_compare_means(label = "p.signif")

p<- ggboxplot(cp %>% filter(amp.bins.15 == '(30,45]'),
              x = "block", y = 'abs(peak.gaze.velocity)',
              fill = "blocknum", palette = "jco",
              facet.by = c("subject", "iep_centered"), short.panel.labs = TRUE)

p + stat_compare_means(label = "p.signif")


p<- ggboxplot(cp %>% filter(amp.bins.15 == '(45,60]', !is.na(iep_bin)),
              x = "block", y = 'abs(peak.gaze.velocity)',
              fill = "blocknum", palette = "jco",
              facet.by = c("subject", "iep_bin"), short.panel.labs = TRUE)

p + stat_compare_means(label = "p.signif")

p<- ggboxplot(cp %>% filter(amp.bins.15 == '(45,60]'),
              x = "block", y = 'abs(peak.gaze.velocity)',
              fill = "blocknum", palette = "jco",
              facet.by = c("subject", "iep_centered"), short.panel.labs = TRUE)

p + stat_compare_means(label = "p.signif")

#head contribution
p<- ggboxplot(cp %>% filter(amp.bins.15 == '(30,45]',!is.na(iep_bin)),
              x = "block", y = 'abs(head.contribution)',
              fill = "blocknum", palette = "jco",
              facet.by = c("subject","iep_bin"), short.panel.labs = TRUE)

p + stat_compare_means(label = "p.signif")

p<- ggboxplot(cp %>% filter(amp.bins.15 == '(30,45]'),
              x = "block", y = 'abs(head.contribution)',
              fill = "blocknum", palette = "jco",
              facet.by = c("subject", "iep_centered"), short.panel.labs = TRUE)

p + stat_compare_means(label = "p.signif")


p<- ggboxplot(cp %>% filter(amp.bins.15 == '(45,60]', !is.na(iep_bin)),
              x = "block", y = 'abs(head.contribution)',
              fill = "blocknum", palette = "jco",
              facet.by = c("subject", "iep_bin"), short.panel.labs = TRUE)

p + stat_compare_means(label = "p.signif")

p<- ggboxplot(cp %>% filter(amp.bins.15 == '(45,60]'),
              x = "block", y = 'abs(head.contribution)',
              fill = "blocknum", palette = "jco",
              facet.by = c("subject", "iep_centered"), short.panel.labs = TRUE)

p + stat_compare_means(label = "p.signif")


#left vs right
C. can you do the same thing for Gpv and Hcon across these amplitudes, 
but instead of IEP sorting, can you do LEFTWARD trials only 
(so we’re comparing similar amplitude trials but only to the left — disregard IEP for now).

p<- ggboxplot(cp %>% filter(amp.bins.15 == '(30,45]'),
              x = "block", y = 'abs(peak.gaze.velocity)',
              fill = "blocknum", palette = "jco",
              facet.by = c("subject", "gaze_dir"), short.panel.labs = TRUE)

p + stat_compare_means(label = "p.signif")

p<- ggboxplot(cp %>% filter(amp.bins.15 == '(45,60]'),
              x = "block", y = 'abs(peak.gaze.velocity)',
              fill = "blocknum", palette = "jco",
              facet.by = c("subject", "gaze_dir"), short.panel.labs = TRUE)

p + stat_compare_means(label = "p.signif")

p<- ggboxplot(cp %>% filter(amp.bins.15 == '(30,45]'),
              x = "block", y = 'abs(head.contribution)',
              fill = "blocknum", palette = "jco",
              facet.by = c("subject", "gaze_dir"), short.panel.labs = TRUE)

p + stat_compare_means(label = "p.signif")

p<- ggboxplot(cp %>% filter(amp.bins.15 == '(45,60]'),
              x = "block", y = 'abs(head.contribution)',
              fill = "blocknum", palette = "jco",
              facet.by = c("subject", "gaze_dir"), short.panel.labs = TRUE)

p + stat_compare_means(label = "p.signif")

# stats
cp %>% 
  filter(amp.bins.15 %in% c('(30,45]', '(45,60]')) %>%
  group_by(iep_bin, amp.bins.15) %>%
  summarize(n = n(),
            mean_HC = mean(abs(head.contribution)),
            sd_HC = sd(abs(head.contribution)),
            mean_Gpv = mean(abs(peak.gaze.velocity)),
            sd_Gpv = sd(abs(peak.gaze.velocity))) %>%
  write.csv('CP48n stats 2-28-2018.csv')

cp %>% 
  filter(amp.bins.15 %in% c('(30,45]', '(45,60]')) %>%
  group_by(gaze_dir, amp.bins.15) %>%
  summarize(n = n(),
            mean_HC = mean(abs(head.contribution)),
            sd_HC = sd(abs(head.contribution)),
            mean_Gpv = mean(abs(peak.gaze.velocity)),
            sd_Gpv = sd(abs(peak.gaze.velocity))) %>%
  write.csv('CP48n stats2 2-28-2018.csv')

