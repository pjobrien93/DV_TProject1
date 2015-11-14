require("jsonlite")
require("RCurl")
require("dplyr")
require("ggplot2")

# Change the USER and PASS below to be your UTEid
df <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from RESIDENCEHALLARREST2013"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_pjo293', PASS='orcl_pjo293', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

summary(df)
head(df)

KPI_Low_Max_value = 0.0018     
KPI_Medium_Max_value = 0.01

df <- df %>% group_by(INSTNM) %>% filter(LIQUOR12 != "null", DRUG12 != "null", WEAPON12 != "null", TOTAL != "null")

df$LIQUOR12 <- as.numeric(as.character(df$LIQUOR12))
df$DRUG12 <- as.numeric(as.character(df$DRUG12))
df$WEAPON12 <- as.numeric(as.character(df$WEAPON12))
df$TOTAL <- as.numeric(as.character(df$TOTAL))

df <- df %>% filter(SECTOR_DESC %in% c('Private nonprofit, 4-year or above','Private for-profit, 4-year or above','Public, 4-year or above')) %>%group_by(SECTOR_DESC, STATE) %>% mutate(crime_rate = (DRUG12 + WEAPON12 + LIQUOR12) / TOTAL) %>% mutate(kpi = ifelse(crime_rate <= KPI_Low_Max_value, '03 Low', ifelse(crime_rate <= KPI_Medium_Max_value, '02 Medium', '01 High'))) 

ggplot() + 
  coord_cartesian() + 
  scale_x_discrete() +
  scale_y_discrete() +
  labs(title='KPI of Crime Rate') +
  labs(x=paste("Type of University"), y=paste("State")) +
  layer(data=df, 
        mapping=aes(x=SECTOR_DESC, y=STATE, label=LIQUOR12), 
        stat="identity", 
        stat_params=list(), 
        geom="text",
        geom_params=list(colour="black", hjust=0, size=3), 
        position=position_identity()
  ) +
  layer(data=df, 
        mapping=aes(x=SECTOR_DESC, y=STATE, label=DRUG12), 
        stat="identity", 
        stat_params=list(), 
        geom="text",
        geom_params=list(colour="black", hjust=8, size=3), 
        position=position_identity()
  ) +
  layer(data=df, 
        mapping=aes(x=SECTOR_DESC, y=STATE, label=DRUG12), 
        stat="identity", 
        stat_params=list(), 
        geom="text",
        geom_params=list(colour="black", hjust=16, size=3),
        position=position_identity()
  ) +
layer(data=df, 
        mapping=aes(x=SECTOR_DESC, y=STATE, fill=kpi), 
        stat="identity", 
        stat_params=list(), 
        geom="tile",
        geom_params=list(alpha=0.50), 
        position=position_identity()
  )
