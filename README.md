# RBSL_planning
Currently this just contains a set of functions to compute a closed-loop area from a set of bearings and distances, and to return a somewhat formatted csv file. 

To install, you need the `remotes` package. If you don't have it you can install this using `install.packages('remotes')`. Once you have that, you can install from this github repository using: `remotes::install_github('SimonCMills/RBSL_planning')`.

Once you have installed it, run the following: 
```{r}
# load the package's functionality into the R session
library(RBSLplanning) 

# get the list of filenames that you want to process
file_list <- list.files(path = '<directory path>', pattern = '.csv', 
                        full.names = TRUE)
process_files(file_list)
```
This will return a set out processed files to an `outputs` directory (which is created if it doesn't alread exist).
