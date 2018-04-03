load(file="./2014flights.Rdata")
format(object.size(df),units='MiB')#get the size in memory

require(dplyr)
df %>% summarize(n())

df %>% group_by(DAY_OF_WEEK) %>% 
       summarize(total = n())


props = df %>% filter(CANCELLED == 0) %>% 
       na.omit %>%  mutate(delayInd = ifelse(DEP_DELAY > 0,1,0)) %>% 
       group_by(MONTH) %>% 
       summarize(total = n(),success = sum(delayInd),prop = mean(delayInd))
prop.test(x=props$success,n=props$total)
props$prop

DAY_OF_YEAR = df %>% mutate('DAY_OF_YEAR' = mapply(function(x,y){return(paste(c(x,y),collapse='_'))},
                                           as.character(df$MONTH),as.character(df$DAY_OF_MONTH))) %>% 
       group_by(DAY_OF_YEAR) %>% summarise(avgDepDelay = mean(DEP_DELAY,na.rm=TRUE)) %>%
       arrange(desc(avgDepDelay))

DAY_OF_YEAR[1:10,]

df %>% group_by(ORIGIN,DEST) %>% 
       summarize(medianDelay = median(DEP_DELAY,na.rm=TRUE)) %>%
       arrange(desc(medianDelay))
