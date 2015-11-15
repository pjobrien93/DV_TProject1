require("jsonlite")
require("RCurl")
require("dplyr")
require("ggplot2")
require(dplyr)

# Change the USER and PASS below to be your UTEid
pov <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from STATEPOVERTYLEVELS"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_pjo293', PASS='orcl_pjo293', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

df <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from RESIDENCEHALLARREST2013"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_pjo293', PASS='orcl_pjo293', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

dplyr::inner_join(pov, df, by="STATE")


df$DRUG12 <- as.numeric(as.character(df$DRUG12))

df <- df %>% filter(SECTOR_DESC %in% c('Public, 2-year', 'Public, less-than 2-year','Public, 4-year or above')) %>% group_by(SECTOR_DESC, STATE)
ggplot() + 
  coord_cartesian() + 
  scale_x_discrete() +
  scale_y_discrete() +
  labs(title='Drug Offenses vs Percentage of Population Below Poverty') +
  labs(x=paste("Type of University"), y=paste("State")) +
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
  ) 

