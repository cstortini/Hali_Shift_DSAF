# I landed on: R 4.4.1, INLA 24.12.11, and TMB 1.9.15
#https://cran.r-project.org/bin/windows/base/old/4.4.1/
#also download rtools version
#https://cran.r-project.org/bin/windows/Rtools/rtools44/rtools.html

#update R
.libPaths()
install.packages("installr")
library(installr)
#updateR()

#see https://github.com/James-Thorson-NOAA/VAST for installing VAST and TMB
#Installing INLA was complicated and required a lot of trial and error..THe INLA website as well as 
#I needed to be in version 4.4.1, and install the appropriate version of Rtools,
R.version #check what you have
#install.packages("devtools")# you might need this
# List of required packages
required_packages <- c("Matrix", "methods", "stats", "graphics", "grDevices", "utils", "tools",
                       "sp", "rgdal", "rgeos", "raster", "splancs", "maptools", "mgcv", "ggplot2", "fields")

# Function to check and install missing packages
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

# Install missing packages
sapply(required_packages, install_if_missing)
#after all these dependencies are installed, restart r and run ...to be honest i do not remember which of the following worked...but one should 
#this will get you the latest version 
#install.packages("INLA", repos = c(getOption("repos"), INLA = "https://inla.r-inla-download.org/R/stable"), type = "source")
#install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
#install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/testing"), dep=TRUE)


#my version of INLA was built under 4.4.2
#Try updating R
#Try older version of INLA 23.09.09 manually download here: https://inla.r-inla-download.org/R/stable/bin/windows/contrib/4.3/
#install.packages("C:/Users/fergusonk/AppData/Local/Programs/R/R-4.2.2/INLA_23.09.09.zip", repos=NULL)#install from downloaded file


# Install the remotes package if you haven't already
install.packages("remotes")
library(remotes)
# Then install a specific version of TMB, available versions here: https://cran.r-project.org/src/contrib/Archive/TMB/
remotes::install_version("TMB", version = "1.9.16", repos = "http://cran.us.r-project.org")


# Install remotes package if not already installed
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

# Install INLA version 24.06.27
remotes::install_version("INLA", version = "24.06.27", repos = "https://inla.r-inla-download.org/R/stable")
