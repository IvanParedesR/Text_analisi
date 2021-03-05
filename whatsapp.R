library(rwhatsapp)
library(lubridate)
library(tidyverse)
library(tidytext)
library(kableExtra)
library(RColorBrewer)
library(knitr)
# LEEMOS EL CHAT A TRAVÉS DEL TXT EXPORTADO DESDE LA APP
#DATOS CORTOS

miChat <- rwa_read("C:/Users/iparedes/Documents/Pleno1.txt")

miChat$hora <- strftime(miChat$time, format="%H:%M:%S")


