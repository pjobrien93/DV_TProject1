require("jsonlite")
require("RCurl")
require("dplyr")
require("ggplot2")
require(dplyr)
require(reshape2)

# Change the USER and PASS below to be your UTEid
df <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select INSTNM, LIQUOR12, DRUG12, WEAPON12 from RESIDENCEHALLARREST2013"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_pjo293', PASS='orcl_pjo293', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

summary(df)
head(df)

df <- df %>% group_by(INSTNM) %>% filter(LIQUOR12 != 0, DRUG12 != 0, WEAPON12 !=0) %>% filter(LIQUOR12 != "null", DRUG12 != "null", WEAPON12 != "null")

df$LIQUOR12 <- as.numeric(as.character(df$LIQUOR12))
df$DRUG12 <- as.numeric(as.character(df$DRUG12))
df$WEAPON12 <- as.numeric(as.character(df$WEAPON12))

df <- melt(df[,c('INSTNM','LIQUOR12','DRUG12', 'WEAPON12')],id.vars = "INSTNM")
df$value <- as.numeric(as.character(df$value))

ggplot(df, aes(INSTNM, value)) +   
  geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
  coord_flip()
