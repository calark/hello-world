

#***************** Clear Enviroment, adjust settings and set working directory ******************/

# Clear objects from the environment
rm(list = ls())

# increase memory limit to help things to run (otherwise our laptops run out of memory!)
memory.limit()
memory.limit(50000)

# Disable Scientific numbers - otherwise these are really annoying!
options(scipen = 999)

# *UPDATE YEARLY* Set working directory (this is a reference point for describing file locations. For example,
# see how we retrieve the raw_data_plus file. We write "." to refer to the working directory. )
setwd("U:/RDIA/Waste and Resources Strategy/Indicator Trends and Targets/Indicator Framework/Pathway exploration/Input data")

#****************** Load packages **********************

# Load needed packages (note that these may need to be installed first)
require(combinat)
require(tidyverse)
require(janitor)
require(readxl)
require(openxlsx)
require(ggplot2)
require(rlang)
require(openxlsx)
require(stringr)
require(tm)
require(naniar)
require(data.table)

# *******************************************************************************
# Define functions
#********************************************************************************

#Import each sheet in a workbook and output as a list of dataframes
import_data <- function(filepath){ filepath %>% 
                                   excel_sheets() %>% 
                                   set_names() %>% 
                                   map(read_excel, path = filepath)}

# calc_lin_path once data has been split and before it is recombined
calc_lin_path_2 <- function(x){
  
# x <- df_split[["2018"]]  
  
  baseline_value <- x %>% 
    filter(., year == eval(baseline_year)) %>% 
    mutate(max = min(time_series, min(next_ambition, na.rm = TRUE), na.rm = TRUE)) %>% 
    .$max
  
  x <- x %>% 
    mutate(., years_since_baseline = year-baseline_year) %>% 
    mutate(., pathway = ((next_ambition - eval(baseline_value))/(next_ambition_year - baseline_year)) * years_since_baseline + eval(baseline_value)) %>% 
    mutate(., pathway = replace_na(pathway, eval(baseline_value))) %>% 
    mutate(., pathway = if_else(year<baseline_year, 0, pathway))
 

  
}

#Define function for imputing a linear pathway
calc_lin_path <- function(df) {
  
  #df <- if_data[["Sheet1"]]
  
  df_split <- df %>% 
              split(df$baseline_year)
  
map(df_split, calc_lin_path_2) %>% 
    bind_rows(.)
  
}


#***************** Import trend analysis data ***************

if_data <- import_data("./BMW example.xlsx")

#***************** Calculate linear pathway  ***************

if_data_inc_pathway <- map(if_data, calc_lin_path)

#***************** Export linear pathways ******************





