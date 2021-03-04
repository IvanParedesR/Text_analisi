library(rwhatsapp)
library(lubridate)
library(tidyverse)
library(tidytext)
library(kableExtra)
library(RColorBrewer)
library(knitr)
# LEEMOS EL CHAT A TRAVÉS DEL TXT EXPORTADO DESDE LA APP

miChat <- rwa_read(“miChat_1.txt”)