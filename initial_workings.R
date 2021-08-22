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

olympics_df <- filter(olympians, Medal == 'Gold')
olympics_df <- filter(olympics_df, Season == 'Summer')
olympics_df <- left_join(olympics_df,nations, by ='NOC')

# regions grouped
olympics_df$region <- ifelse(is.na(olympics_df$region),olympics_df$Team,olympics_df$region)
olympics_df <- inner_join(olympics_df,regions, by ='region')

# check for ties / mixed teams
multi_region_winners <- unique(olympics_df[,c('Year','Event','Sport','NOC')])
multi_region_winners <- multi_region_winners %>%
  group_by(Year,Event,Sport) %>%
  mutate(unique_types = n_distinct(NOC))

multi_region_winners <- filter(multi_region_winners,unique_types>1)
multi_region_winners_lookup <- unique(multi_region_winners[,c('Year','Event','Sport')])
multi_region_winners_lookup$mixed <- TRUE
olympics_df <- left_join(olympics_df,multi_region_winners_lookup, by = c('Year' = 'Year','Event' = 'Event','Sport' = 'Sport') ) 

#olympics_df$mixed <- ifelse(is.na(olympics_df$mixed),FALSE,TRUE)
olympics_df$group <- ifelse(is.na(olympics_df$mixed),olympics_df$group,"Individual Olympic Athletes")
olympics_df$group <- gsub("Individual Olympic Athletes",'Individual/Mixed Olympic Athletes',olympics_df$group)

# create event id
olympics_df$event_id <- paste0(olympics_df$Sport,' | ',olympics_df$Event,' | ',olympics_df$Year)

# name and nation
olympics_df$name_and_nation <- paste0(olympics_df$Name," (",olympics_df$region,")")
name_lookup <- unique(olympics_df[,c('Event','Year','name_and_nation')])
name_lookup <- name_lookup %>% 
  group_by(Event,Year) %>% 
  mutate(tooltip = paste0(name_and_nation, collapse = ", ")) 
name_lookup <- unique(name_lookup[,c('Event','Year','tooltip')])

# KPIs
nrow(olympics_df)/length(unique(olympics_df$Year))
nrow(olympians)/length(unique(olympics_df$Year))
olympics_df$event_gender <- ifelse(grepl('Mixed',olympics_df$Event),'Mixed',ifelse(grepl('Women',olympics_df$Event),'Womens','Mens'))
events_by_gender <- unique(olympics_df[,c('Year','Event','event_gender')]) %>% count(Year, event_gender, sort = TRUE)
write.csv(events_by_gender,'data/olympic_events_by_gender.csv',row.names = F)

events_by_group <- unique(olympics_df[,c('Year','Event','event_gender','group')]) %>% count(group,event_gender, sort = TRUE)
write.csv(events_by_group,'data/olympic_events_by_group.csv',row.names = F)


all_events <- unique(olympics_df$Event)
event_gender <- ifelse(grepl('Mixed',all_events),'Mixed',ifelse(grepl('Women',all_events),'Womens','Mens'))
event_lookup <- data.frame(event = all_events,gender = event_gender)
event_lookup <- inner_join(event_lookup,event_groups,by = 'event')
event_lookup <- inner_join(event_lookup,unique(olympics_df[,c('Event','Sport')]),by = c('event' = 'Event'))

event_lookup$event_no_gender <- gsub("Mixed|Women's|Men's","",event_lookup$event)
event_lookup$event_no_gender <- substr(event_lookup$event_no_gender,nchar(event_lookup$Sport)+1,nchar(event_lookup$event_no_gender))
event_lookup$event_no_gender <- trimws(event_lookup$event_no_gender)

event_lookup <- left_join(event_lookup,event_dist,by ='event_no_gender')
event_lookup$dist <- as.integer(event_lookup$dist)
event_lookup$event_dist_sort <- ifelse(is.na(event_lookup$dist),1000000,event_lookup$dist)

# Rejig Sport groupings
event_lookup$Sport <- gsub('Beach Volleyball','Volleyball',event_lookup$Sport)
athletics_events <- sort(unique(filter(event_lookup,Sport == 'Athletics')$event_no_gender))
multisport <- c("All-Around Championship","Decathlon","Heptathlon","Pentathlon","Modern Pentathlon",'Pentathlon (Ancient)','Olympic Distance')
event_lookup$Sport <- ifelse(event_lookup$event_no_gender %in% c(athletics_events[grepl('Jump',athletics_events)],'Pole Vault'),'Athletics (Jumping)',event_lookup$Sport)
event_lookup$Sport <- ifelse(event_lookup$event_no_gender %in% multisport,'Multi-Sport',event_lookup$Sport)
event_lookup$Sport <- ifelse(event_lookup$type == 'agility' & event_lookup$Sport == 'Athletics (Jumping)','Equestrianism',event_lookup$Sport)

ball_sports <- c('Baseball','Basketball','Cricket','Croquet','Football','Golf','Handball','Hockey','Lacrosse','Polo','Roque','Rugby','Rugby Sevens','Softball','Volleyball','Water Polo')
racquet_sports <- c('Basque Pelota','Badminton','Jeu De Paume','Racquets','Table Tennis','Tennis')
gymnastics <- c('Gymnastics','Rhythmic Gymnastics','Trampolining')

event_lookup$Sport <- ifelse(event_lookup$Sport %in% ball_sports,'Ball Sports',event_lookup$Sport)
event_lookup$Sport <- ifelse(event_lookup$Sport %in% racquet_sports,'Racquet Sports',event_lookup$Sport)
event_lookup$Sport <- ifelse(event_lookup$Sport %in% gymnastics,'Gymnastics',event_lookup$Sport)

# Sorting events
event_lookup <- event_lookup[with(event_lookup, order(type,Sport,event_dist_sort,event_no_gender,gender)), ]
event_lookup$sort <- 1:nrow(event_lookup)


# Building framework
all_years <- unique(olympics_df$Year)
frame_df <- merge(event_lookup,all_years,all=TRUE)
frame_df <- left_join(frame_df,unique(olympics_df[,c('Year','Event','group')]), by = c('y' = 'Year','event' = 'Event'))
frame_df$group <- ifelse(is.na(frame_df$group),'Not Held',frame_df$group)
frame_df <- frame_df[with(frame_df, order(sort,y)), ]
frame_df$sort <- 1:nrow(frame_df)

# build sunburst specific columns
frame_df$component <- paste0(frame_df$group,' | ',frame_df$Sport,' | ',frame_df$event,' | ',frame_df$y)
levels <- 1:length(all_years) 
level_lookup <- data.frame(y=sort(all_years),level=sort(levels),stringsAsFactors = F)
frame_df <- inner_join(frame_df,level_lookup, by = 'y')
frame_df$join <- 'Link'
frame_df$value <- 1
frame_df$parent <- ''
frame_df$lowest_level_value <- ''

sunburst_cols <- c('join','level','component','type','parent','lowest_level_value','value','sort')
sunburst_data <- frame_df[sunburst_cols]

# create paths and widths tabs
sunburst_widths <- data.frame(level = levels,distance = levels+3, width = 1,stringsAsFactors = F)
sunburst_paths <- data.frame(join = c('Link','Link'),path = c(1,362),stringsAsFactors = F)

# Write sunburst tabs to excel
write.xlsx(sunburst_data, file = 'Olympic Sunburst.xlsx', sheetName="Data", row.names=FALSE)
write.xlsx(sunburst_widths, file = 'Olympic Sunburst.xlsx', sheetName="Widths", append=TRUE, row.names=FALSE)
write.xlsx(sunburst_paths, file = 'Olympic Sunburst.xlsx', sheetName="Paths", append=TRUE, row.names=FALSE)

component_and_name <- inner_join(frame_df,name_lookup, by = c('event' = 'Event','y'='Year'))
component_and_name <- component_and_name[,c("component","tooltip")]
write.csv(component_and_name,'name_lookup.csv',row.names = F)