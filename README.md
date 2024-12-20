# EpiPlot

![](www/Epiplot_logo.png)

EpiPlot is a web application designed to format patient movement data in order to create an interactive movement tracking graph and build a network. A file with the samples taken for each patient (both positive and negative) can be added to visualize the samples on the graph and sort patients according to the genotype of their strain. The application has been created for the University Hospital of Lausanne and is in french.

## Install

R and RStudio are required to use the application locally. The installation procedure is described on <https://posit.co/download/rstudio-desktop/>. The Chrome or Edge browser should also be installed for report generation (webshot2 package).

##### For Linux

Some dependencies are required before installing the different R packages, especially if you are using a fresh system.

```         
sudo apt install cmake make gcc g++ zlib1g-dev libfreetype6-dev libfontconfig1-dev texlive
```

##### For windows

The Rtools tool must be installed and can be find at [https://cran.r-project.org/bin/windows/Rtools/]().

##### R libraries

The necessary libraries can be installed using the install_packages.R script.

```         
# Install libraries
Rscript install_packages.R
```

### Testing the application

A movement file and a sample file are available for testing the application in the Examples directory. Use these files as templates to format your own files.
