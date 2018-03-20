### Header for habitat selection
## Clear everything
rm(list = ls())
graphics.off()

## Libraries
library(tidyverse)
library(waveslim)
library(lattice)
library(vegan)
library(philentropy)

## Directories
path_landscapes <- "landscapes" %>% normalizePath()
path_single_gen <- "single_gen" %>% normalizePath()
path_movement <- "movement" %>% normalizePath()
path_analysis <- "analysis" %>% normalizePath()
path_multiple_gen <- "multiple_gen" %>% normalizePath()
path_community <- "community" %>% normalizePath()
path_figures <- "figures" %>% normalizePath()
path_ms <- "ms" %>% normalizePath()


## Sourcing
source('functions.R')
