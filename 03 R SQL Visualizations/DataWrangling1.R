require("jsonlite")
require("RCurl")
require("dplyr")
require("ggplot2")

# Change the USER and PASS below to be your UTEid
df <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from RESIDENCEHALLARREST2013"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_pjo293', PASS='orcl_pjo293', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

summary(df)
head(df)

df <- df %>% group_by(INSTNM) %>% filter(LIQUOR12 != 0, DRUG12 != 0, WEAPON12 !=0) %>% filter(LIQUOR12 != "null", DRUG12 != "null", WEAPON12 != "null")

ggplot() + 
  coord_cartesian() + 
  scale_x_discrete() +
  scale_y_discrete() +
  labs(title='###') +
  labs(x=paste("Number of Crime Incidents by Type"), y=paste("Institution Name")) +
  layer(data=df, 
        mapping=aes(x=LIQUOR12, y=INSTNM), 
        geom="bar"
  )
