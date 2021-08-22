library(dplyr)
library(xlsx)

'%ni%' <- Negate('%in%')

# Load Data
olympians <- read.csv('data/Olympic Athletes and Events.csv',stringsAsFactors = F)
nations <- read.csv('data/Olympic Nations and Region Look-Up.csv',stringsAsFactors = F)
regions <- read.csv('data/region_lookup.csv',stringsAsFactors = F)
event_groups <- read.csv('data/event_groups.csv',stringsAsFactors = F)
names(event_groups) <- c('event','type')
event_dist <- read.csv('data/event_distances.csv',stringsAsFactors = F)

#olympics_df <- filter(olympians, Medal == 'Gold')
olympics_df <- filter(olympics_df, Season == 'Summer')
olympics_df <- left_join(olympics_df,nations, by ='NOC')

olympics_df$event_gender <- ifelse(grepl('Mixed',olympics_df$Event),'Mixed',ifelse(grepl('Women',olympics_df$Event),'Womens','Mens'))
events_by_gender <- unique(olympics_df[,c('Year','Event','event_gender')]) %>% count(Year, event_gender, sort = TRUE)
write.csv(events_by_gender,'data/olympic_events_by_gender.csv',row.names = F)

