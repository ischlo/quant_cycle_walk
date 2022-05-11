#### Controling python ####
library(reticulate)

# which version of python is used
Sys.which("python")
# use an environment
use_condaenv(condaenv = "ox")

# alternatively set it:
# Sys.setenv(RETICULATE_PYTHON="/opt/anaconda3/envs/ox/bin/python")


Sys.getenv()
py_config()
py_discover_config()

# create a new environment 
# conda_create("r-network")

conda_list()

# use_condaenv("r-network")

py_module_available("pandas")

ox <- import("osmnx")

# install packages
py_install(packages =  c("pandas","networkx","geopandas","numpy")
           ,envname = "ox"
           )
py_install(envname = "ox"
           ,packages = "gc")


reticulate::repl_python()

brighton_custom_cycle <- py$brighton_custom_cycle
#### clean environment 
gc()

