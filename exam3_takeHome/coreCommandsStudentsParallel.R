require(nycflights13)
require(ggplot2)
require(dplyr) 
require(plyr) 

if(!require(foreach)){install.packages("foreach");require(foreach)}
operatingSystem = 'mac'
# The -1 is if using a GUI and hence needing system resources for other than processing
#   Usually, parallel processing is meant to be run in batch mode
#     (R CMD BATCH myRscript.r &)

if(operatingSystem == 'mac'){
  if(!require(doMC)){
    install.packages('doMC')
  }else{
    require(doMC)
  }
  nCores = detectCores(all.tests = FALSE, logical = TRUE) - 1
  registerDoMC(nCores)
}
if(operatingSystem == 'windows'){
  if(!require(doSMP)){
    install.packages('doSMP')
  }else{
    require(doSMP)
  }
  nCores = detectCores(all.tests = FALSE, logical = TRUE) - 1
  registerDoSMP(startWorkers(nCores))
}
x    = seq_len(150)
wait = function(i) Sys.sleep(0.1)
system.time(llply(x, wait))
system.time(llply(x, wait, .parallel = TRUE))

#using plyr:
startTime = proc.time()[3]
summarise(flights, delay = mean(dep_delay, na.rm = TRUE),.parallel=TRUE)
stopTime  = proc.time()[3]
cat('total time, parallel: ',stopTime - startTime)
startTime = proc.time()[3]
summarise(flights, delay = mean(dep_delay, na.rm = TRUE))
stopTime  = proc.time()[3]
cat('total time: ',stopTime - startTime)

#A very readable introduction: 
#   https://cran.r-project.org/web/packages/doMC/vignettes/gettingstartedMC.pdf