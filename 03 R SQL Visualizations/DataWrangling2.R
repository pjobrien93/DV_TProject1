require("jsonlite")
require("RCurl")
require("dplyr")
require("ggplot2")

# Change the USER and PASS below to be your UTEid
df <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from RESIDENCEHALLARREST2013"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_pjo293', PASS='orcl_pjo293', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

df %>% mutate(percent_crime = (percent_men = MEN_TOTAL/TOTAL) %>% 
  ggplot() + 
  coord_cartesian() + 
  scale_x_continuous() +
  scale_y_continuous() +
  labs(title='Titanic') +
  labs(x="Percent Men", y=paste("Crime Rate")) +
  layer(data=df, 
        mapping=aes(x=as.numeric(as.character(LIQUOR12)), y=as.numeric(as.character(percent_crime))), 
        stat="identity", 
        stat_params=list(), 
        geom="point",
        geom_params=list()
  )
