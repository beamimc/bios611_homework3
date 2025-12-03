FROM --platform=linux/amd64 rocker/verse:latest

RUN R -e "install.packages(c('cluster', 'purrr', 'dplyr', 'tidyr', 'ggplot2', 'plotly'), dependencies=TRUE)"