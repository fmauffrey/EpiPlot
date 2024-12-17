# EpiPlot

![](www/Epiplot_logo.png)

EpiPlot is a web application designed to format patient movement data in order to create an interactive movement tracking graph and build a network. A file with the samples taken for each patient (both positive and negative) can be added to visualize the samples on the graph and sort patients according to the genotype of their strain. The application has been created for the University Hospital of Lausanne and is in french.

R and RStudio are required to use the application locally. The installation procedure is described on <https://posit.co/download/rstudio-desktop/>.

The necessary libraries can be installed using the install_packages.R script.

```         
# Install libraries
Rscript install_packages.R
```

# Testing the application

A movement file and a sample file are available for testing the application in the Examples directory. Use these files as templates to format your own files.
