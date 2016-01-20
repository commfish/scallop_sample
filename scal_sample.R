library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)

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



samp5



