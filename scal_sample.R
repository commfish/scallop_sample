library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(reshape2)

# events database
events <- read.csv("./data/DATA_EVENTS_SCALLOP_DREDGE.csv")

# filter events dataframe, select the columns to use, calculate the actual tow used, define bed as a factor, rename the survey areas to match other databases
events %>%
   filter(USED_IN_ESTIMATE =="YES") %>%
   select(YEAR, EVENT_ID, SURVEY_AREA, BED, STATION_ID,  
          TOW_LENGTH_FIELD, TOW_LENGTH_DESIGNATED) %>%
   mutate(tow = ifelse (abs(TOW_LENGTH_FIELD - TOW_LENGTH_DESIGNATED) > 
                           TOW_LENGTH_FIELD * 0.10, TOW_LENGTH_FIELD, TOW_LENGTH_DESIGNATED), 
          BED = factor(str_trim(BED)),
          SURVEY_AREA = revalue(SURVEY_AREA, c('Kamishak Scallop Dredge' = 'Kamishak',
                                               'Kayak Scallop Dredge' = 'Kayak',
                                               'Kachemak Scallop' = 'Kachemak'))) -> events

#catch composition database
catchcomp <- read.csv("./data/DATA_CATCHCOMP_SCALLOP_DREDGE.csv")

# filter catchcomp data and summarize by event_id - broken into adults and pre-recruits in the database, join this with the events dataframe
catchcomp %>%
   filter(SPECIES_CODE ==850, CONDITION_CODE==1 ) %>%
   group_by(EVENT_ID) %>% 
   summarize(COUNT=sum(COUNT), SAMPLE_WT_KG= sum(SAMPLE_WT_KG)) %>% 
   right_join(events, by="EVENT_ID") -> tab


#survey stations dataframe
stations <- read.csv("./data/DATA_SCALLOP_SURVEY_STATIONS.csv")

#calculate total bed area 
stations %>% 
   group_by(SURVEY_AREA, BED) %>%
   summarize(area=sum(Area_Sq_NM)) -> area

# calculate standardized wt and catch values, summarize by area, bed and year, join this with total bed area
#dredge is 8' wide or 0.00131663 nautical miles
# nm2 to m2  = 3.43e+6
tab %>% 
   group_by(SURVEY_AREA, BED, YEAR) %>% 
   mutate(s.wt = SAMPLE_WT_KG/(tow*0.00131663), s.count = COUNT/(tow*0.00131663) ) %>% 
   select(-TOW_LENGTH_FIELD, -TOW_LENGTH_DESIGNATED) %>% 
   left_join(area) ->tab.a

#calculate sample squares to use as percentage of area with 50% being max
tab.a %>% 
   mutate(s2 = round(area*0.2), s3 = round(area*0.3), s4=round(area*0.4), s5=round(area*0.5)) -> tab.a


#seperate dataframes by bed
tab.a %>% 
   filter(SURVEY_AREA=='Kamishak', BED=="North") -> kn
tab.a %>% 
   filter(SURVEY_AREA=='Kamishak', BED=="South") -> ks
tab.a %>% 
   filter(SURVEY_AREA=='Kayak', BED=="East") -> ke
tab.a %>% 
   filter(SURVEY_AREA=='Kayak', BED=="West") -> kw

# function for sample a set number of squares in a bed, n=# of squares sampled
samp_it <- function(df,n) {
   df %>% 
      group_by(SURVEY_AREA, BED, YEAR) %>% 
      sample_n(n, replace = TRUE) %>% 
      summarize( P = mean(s.count)*mean(area), density = mean(s.count)/3.43e+6)
} 


###########  Kamishak North #######
#replicates looking at kamishak north bed at   
replicate(1000, kn, simplify = FALSE) %>%
   lapply(., samp_it, n=mean(kn$s2)) %>% 
   bind_rows %>%
   mutate(replicate = 1:n()) %>%
   group_by(SURVEY_AREA,BED,YEAR) %>% 
   summarize(Pbar = mean(P), Psd=sd(P)) -> kn2

replicate(1000, kn, simplify = FALSE) %>%
   lapply(., samp_it, n=mean(kn$s3)) %>% 
   bind_rows %>%
   mutate(replicate = 1:n()) %>%
   group_by(SURVEY_AREA,BED,YEAR) %>% 
   summarize(Pbar = mean(P), Psd=sd(P)) -> kn3
#added a rep for 35 % of grids sampled
replicate(1000, kn, simplify = FALSE) %>%
  lapply(., samp_it, n=(90*0.35)) %>% 
  bind_rows %>%
  mutate(replicate = 1:n()) %>%
  group_by(SURVEY_AREA,BED,YEAR) %>% 
  summarize(Pbar = mean(P), Psd=sd(P)) -> kn35

replicate(1000, kn, simplify = FALSE) %>%
   lapply(., samp_it, n=mean(kn$s4)) %>% 
   bind_rows %>%
   mutate(replicate = 1:n()) %>%
   group_by(SURVEY_AREA,BED,YEAR) %>% 
   summarize(Pbar = mean(P), Psd=sd(P)) -> kn4

replicate(1000, kn, simplify = FALSE) %>%
   lapply(., samp_it, n=mean(kn$s5)) %>% 
   bind_rows %>%
   mutate(replicate = 1:n()) %>%
   group_by(SURVEY_AREA,BED,YEAR) %>% 
   summarize(Pbar = mean(P), Psd=sd(P)) -> kn5



#K.palof 
#1-10-16
# Summarize the above replicates to look at the different between 'kn5' or sampling 50% of grids and sampling 20,30, 35 or 40 %

kn5 %>% 
  #mutate(Pbar52.dif = Pbar - kn2$Pbar, Psd52.dif = Psd -kn2$Psd ) %>% 
  mutate(Pbar52.dif.per = abs((Pbar - kn2$Pbar)/Pbar)*100, 
         Pbar53.dif.per = abs((Pbar - kn3$Pbar)/Pbar)*100, 
         Pbar535.dif.per = abs((Pbar - kn35$Pbar)/Pbar)*100, 
         Pbar54.dif.per = abs((Pbar - kn4$Pbar)/Pbar)*100,
         Psd52.dif.per = abs((Psd - kn2$Psd)/Psd)*100, 
         Psd53.dif.per = abs((Psd - kn3$Psd)/Psd)*100,
         Psd535.dif.per = abs((Psd - kn35$Psd)/Psd)*100, 
         Psd54.dif.per = abs((Psd - kn4$Psd)/Psd)*100) %>% 
  summarize (Psd52.avg.per = mean(Psd52.dif.per, na.rm = T), 
             Psd53.avg.per = mean(Psd53.dif.per, na.rm = T), 
             Psd535.avg.per = mean(Psd535.dif.per, na.rm = T), 
             Psd54.avg.per = mean(Psd54.dif.per, na.rm = T)) %>% 
   melt() %>% 
   mutate(sample_size = c(0.2,0.3,0.35,0.4))-> kn5sum.sd


###########  Kamishak South #######
#replicates looking at kamishak south bed at   
replicate(1000, ks, simplify = FALSE) %>%
  lapply(., samp_it, n=mean(ks$s2)) %>% 
  bind_rows %>%
  mutate(replicate = 1:n()) %>%
  group_by(SURVEY_AREA,BED,YEAR) %>% 
  summarize(Pbar = mean(P), Psd=sd(P)) -> ks2

replicate(1000, ks, simplify = FALSE) %>%
  lapply(., samp_it, n=mean(ks$s3)) %>% 
  bind_rows %>%
  mutate(replicate = 1:n()) %>%
  group_by(SURVEY_AREA,BED,YEAR) %>% 
  summarize(Pbar = mean(P), Psd=sd(P)) -> ks3
#added a rep for 35 % of grids sampled
replicate(1000, ks, simplify = FALSE) %>%
  lapply(., samp_it, n=(68*0.35)) %>% 
  bind_rows %>%
  mutate(replicate = 1:n()) %>%
  group_by(SURVEY_AREA,BED,YEAR) %>% 
  summarize(Pbar = mean(P), Psd=sd(P)) -> ks35

replicate(1000, ks, simplify = FALSE) %>%
  lapply(., samp_it, n=mean(ks$s4)) %>% 
  bind_rows %>%
  mutate(replicate = 1:n()) %>%
  group_by(SURVEY_AREA,BED,YEAR) %>% 
  summarize(Pbar = mean(P), Psd=sd(P)) -> ks4

replicate(1000, ks, simplify = FALSE) %>%
  lapply(., samp_it, n=mean(ks$s5)) %>% 
  bind_rows %>%
  mutate(replicate = 1:n()) %>%
  group_by(SURVEY_AREA,BED,YEAR) %>% 
  summarize(Pbar = mean(P), Psd=sd(P)) -> ks5


ks5 %>% 
   mutate(Pbar52.dif.per = abs((Pbar - ks2$Pbar)/Pbar)*100, 
          Pbar53.dif.per = abs((Pbar - ks3$Pbar)/Pbar)*100, 
          Pbar535.dif.per = abs((Pbar - ks35$Pbar)/Pbar)*100, 
          Pbar54.dif.per = abs((Pbar - ks4$Pbar)/Pbar)*100,
          Psd52.dif.per = abs((Psd - ks2$Psd)/Psd)*100, 
          Psd53.dif.per = abs((Psd - ks3$Psd)/Psd)*100,
          Psd535.dif.per = abs((Psd - ks35$Psd)/Psd)*100, 
          Psd54.dif.per = abs((Psd - ks4$Psd)/Psd)*100) %>% 
   summarize (Psd52.avg.per = mean(Psd52.dif.per, na.rm = T), 
              Psd53.avg.per = mean(Psd53.dif.per, na.rm = T), 
              Psd535.avg.per = mean(Psd535.dif.per, na.rm = T), 
              Psd54.avg.per = mean(Psd54.dif.per, na.rm = T)) %>% 
   melt() %>% 
   mutate(sample_size = c(0.2,0.3,0.35,0.4))-> ks5sum.sd


###########  Kayak East #######
#replicates looking at Kayak East bed at   
replicate(1000, ke, simplify = FALSE) %>%
  lapply(., samp_it, n=mean(ke$s2)) %>% 
  bind_rows %>%
  mutate(replicate = 1:n()) %>%
  group_by(SURVEY_AREA,BED,YEAR) %>% 
  summarize(Pbar = mean(P), Psd=sd(P)) -> ke2

replicate(1000, ke, simplify = FALSE) %>%
  lapply(., samp_it, n=mean(ke$s3)) %>% 
  bind_rows %>%
  mutate(replicate = 1:n()) %>%
  group_by(SURVEY_AREA,BED,YEAR) %>% 
  summarize(Pbar = mean(P), Psd=sd(P)) -> ke3
#added a rep for 35 % of grids sampled
replicate(1000, ke, simplify = FALSE) %>%
  lapply(., samp_it, n=(79*0.35)) %>% 
  bind_rows %>%
  mutate(replicate = 1:n()) %>%
  group_by(SURVEY_AREA,BED,YEAR) %>% 
  summarize(Pbar = mean(P), Psd=sd(P)) -> ke35

replicate(1000, ke, simplify = FALSE) %>%
  lapply(., samp_it, n=mean(ke$s4)) %>% 
  bind_rows %>%
  mutate(replicate = 1:n()) %>%
  group_by(SURVEY_AREA,BED,YEAR) %>% 
  summarize(Pbar = mean(P), Psd=sd(P)) -> ke4

replicate(1000, ke, simplify = FALSE) %>%
  lapply(., samp_it, n=mean(ke$s5)) %>% 
  bind_rows %>%
  mutate(replicate = 1:n()) %>%
  group_by(SURVEY_AREA,BED,YEAR) %>% 
  summarize(Pbar = mean(P), Psd=sd(P)) -> ke5


ke5 %>% 
   mutate(Pbar52.dif.per = abs((Pbar - ke2$Pbar)/Pbar)*100, 
          Pbar53.dif.per = abs((Pbar - ke3$Pbar)/Pbar)*100, 
          Pbar535.dif.per = abs((Pbar - ke35$Pbar)/Pbar)*100, 
          Pbar54.dif.per = abs((Pbar - ke4$Pbar)/Pbar)*100,
          Psd52.dif.per = abs((Psd - ke2$Psd)/Psd)*100, 
          Psd53.dif.per = abs((Psd - ke3$Psd)/Psd)*100,
          Psd535.dif.per = abs((Psd - ke35$Psd)/Psd)*100, 
          Psd54.dif.per = abs((Psd - ke4$Psd)/Psd)*100) %>% 
   summarize (Psd52.avg.per = mean(Psd52.dif.per, na.rm = T), 
              Psd53.avg.per = mean(Psd53.dif.per, na.rm = T), 
              Psd535.avg.per = mean(Psd535.dif.per, na.rm = T), 
              Psd54.avg.per = mean(Psd54.dif.per, na.rm = T)) %>% 
   melt() %>% 
   mutate(sample_size = c(0.2,0.3,0.35,0.4))-> ke5sum.sd


###########  Kayak West #######
#replicates looking at Kayak West bed at   
replicate(1000, kw, simplify = FALSE) %>%
  lapply(., samp_it, n=mean(kw$s2)) %>% 
  bind_rows %>%
  mutate(replicate = 1:n()) %>%
  group_by(SURVEY_AREA,BED,YEAR) %>% 
  summarize(Pbar = mean(P), Psd=sd(P)) -> kw2

replicate(1000, kw, simplify = FALSE) %>%
  lapply(., samp_it, n=mean(kw$s3)) %>% 
  bind_rows %>%
  mutate(replicate = 1:n()) %>%
  group_by(SURVEY_AREA,BED,YEAR) %>% 
  summarize(Pbar = mean(P), Psd=sd(P)) -> kw3
#added a rep for 35 % of grids sampled
replicate(1000, kw, simplify = FALSE) %>%
  lapply(., samp_it, n=(49*0.35)) %>% 
  bind_rows %>%
  mutate(replicate = 1:n()) %>%
  group_by(SURVEY_AREA,BED,YEAR) %>% 
  summarize(Pbar = mean(P), Psd=sd(P)) -> kw35

replicate(1000, kw, simplify = FALSE) %>%
  lapply(., samp_it, n=mean(kw$s4)) %>% 
  bind_rows %>%
  mutate(replicate = 1:n()) %>%
  group_by(SURVEY_AREA,BED,YEAR) %>% 
  summarize(Pbar = mean(P), Psd=sd(P)) -> kw4

replicate(1000, kw, simplify = FALSE) %>%
  lapply(., samp_it, n=mean(kw$s5)) %>% 
  bind_rows %>%
  mutate(replicate = 1:n()) %>%
  group_by(SURVEY_AREA,BED,YEAR) %>% 
  summarize(Pbar = mean(P), Psd=sd(P)) -> kw5


kw5 %>% 
   mutate(Pbar52.dif.per = abs((Pbar - kw2$Pbar)/Pbar)*100, 
          Pbar53.dif.per = abs((Pbar - kw3$Pbar)/Pbar)*100, 
          Pbar535.dif.per = abs((Pbar - kw35$Pbar)/Pbar)*100, 
          Pbar54.dif.per = abs((Pbar - kw4$Pbar)/Pbar)*100,
          Psd52.dif.per = abs((Psd - kw2$Psd)/Psd)*100, 
          Psd53.dif.per = abs((Psd - kw3$Psd)/Psd)*100,
          Psd535.dif.per = abs((Psd - kw35$Psd)/Psd)*100, 
          Psd54.dif.per = abs((Psd - kw4$Psd)/Psd)*100) %>% 
   summarize (Psd52.avg.per = mean(Psd52.dif.per, na.rm = T), 
              Psd53.avg.per = mean(Psd53.dif.per, na.rm = T), 
              Psd535.avg.per = mean(Psd535.dif.per, na.rm = T), 
              Psd54.avg.per = mean(Psd54.dif.per, na.rm = T)) %>% 
   melt() %>% 
   mutate(sample_size = c(0.2,0.3,0.35,0.4))-> kw5sum.sd

##################################################
### add dataframe together for plotting
sum_all <- rbind(kn5sum.sd, ks5sum.sd, ke5sum.sd, kw5sum.sd)

ggplot(sum_all, aes(sample_size, value, color=interaction(SURVEY_AREA, BED)))+geom_point()+geom_line()+
   ylim(0,65)+ylab('Percent difference in SD from 50% of survey area sampled')+xlab('Percent of sample area sampled')+theme_bw()+ scale_color_discrete(name="Survey area")
