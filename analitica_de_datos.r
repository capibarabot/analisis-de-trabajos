library(dplyr)
library(stringr)
library(tidyverse)

#Cargar el dataset
datos <- read.csv("AI_Impact_on_Jobs_2030.csv")

# Modificamos el dataset transformando las categorías en números
datos_codificados <- datos %>%
  mutate(
    #Transformar Education_Level
    Education_Level = case_when(
      Education_Level == "High School" ~ 1,
      Education_Level == "Bachelor's"  ~ 2,
      Education_Level == "Master's"    ~ 3,
      Education_Level == "PhD"         ~ 4,
      TRUE ~ NA_real_ 
    ),
    
    #Transformar Risk_Category
    Risk_Category = case_when(
      Risk_Category == "Low"    ~ 1,
      Risk_Category == "Medium" ~ 2,
      Risk_Category == "High"   ~ 3,
      TRUE ~ NA_real_
    )
  )
view(datos_codificados)

