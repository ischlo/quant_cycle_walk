#### Controling python ####
library(reticulate)

Sys.which("python")
# Sys.setenv(RETICULATE_PYTHON="/opt/anaconda3/bin/python")
Sys.getenv()
py_config()

# create a new environment 
# conda_create("r-network")

# use an environment
use_condaenv("ox")
# use_condaenv("r-network")

py_module_available("osmnx")

# install packages
# py_install(packages =  c("pandas","networkx","geopandas","numpy")
#            ,envname = "ox"
#            )
# py_install(envname = "ox"
#            ,packages = "gc")


reticulate::repl_python()

#### clean environment 
gc()

