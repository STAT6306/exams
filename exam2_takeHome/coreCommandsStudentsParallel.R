require(nycflights13)
require(ggplot2)
require(dplyr) 
require(plyr) 

if(!require(foreach)){install.packages("foreach");require(foreach)}
operatingSystem = 'mac'
# The -1 in the "nCores" below is if using a GUI and hence needing system 
#   resources for other than processing.
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
  if(!require(foreach)){
    install.packages("foreach")
    library(foreach)
  }
  if(!require(doParallel)){
    install.packages("doParallel")
    library(doParallel)
  }
  nCores  = detectCores(all.tests = FALSE, logical = TRUE) - 1
  workers = makeCluster(nCores)  
  registerDoParallel(workers) 
}

x    = seq_len(10)
waitF = function(i) Sys.sleep(0.1)
system.time(llply(x, waitF))
system.time(llply(x, waitF, .parallel = TRUE))

#using plyr:
#Note there won't be a large difference as parallelism only helps on very large
# computations.  It can actually increase the computation times for smaller jobs
system.time(summarise(flights, delay = mean(dep_delay, na.rm = TRUE),.parallel=TRUE))
system.time(summarise(flights, delay = mean(dep_delay, na.rm = TRUE)))

#A very readable introduction: 
#   https://cran.r-project.org/web/packages/doMC/vignettes/gettingstartedMC.pdf