require("jsonlite")
require("RCurl")
require("dplyr")
require("ggplot2")
require(dplyr)

# Change the USER and PASS below to be your UTEid
df <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select INSTNM , LIQUOR12, DRUG12, WEAPON12, MEN_TOTAL, TOTAL from RESIDENCEHALLARREST2013"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_pjo293', PASS='orcl_pjo293', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

summary(df)

df <- df %>% filter(LIQUOR12 != "null", DRUG12 != "null", WEAPON12 != "null", MEN_TOTAL != "null", TOTAL!="null"); View(df);

#df$MEN_TOTAL <- as.numeric(as.character(df$MEN_TOTAL))
df$LIQUOR12 <- as.numeric(as.character(df$LIQUOR12))
df$DRUG12 <- as.numeric(as.character(df$DRUG12))
df$WEAPON12 <- as.numeric(as.character(df$WEAPON12))
df$TOTAL <- as.numeric(as.character(df$TOTAL))
df$MEN_TOTAL <- as.numeric(as.character(df$MEN_TOTAL))
df$RATIO <- df$MEN_TOTAL / df$TOTAL
df$RATE <- (df$LIQUOR12 + df$DRUG12 + df$WEAPON12) / df$TOTAL

avg_crimeRate = mean(df$RATE)

View(df);
ggplot() + 
  coord_cartesian() + 
  scale_x_continuous() +
  scale_y_continuous() +
  labs(title='Titanic') +
  labs(x="Percent Men", y=paste("Crime Rate")) +
  layer(data=df, 
        mapping=aes(x=RATIO, y=RATE), 
        stat="identity", 
        stat_params=list(), 
        geom="point",
        geom_params=list()
        
  )+ 
  geom_hline(yintercept=avg_crimeRate)
