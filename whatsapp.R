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

miChat$day <- as.Date(miChat$time)

# modificamos el data frame para separar los datos por sexenio
miChat$hora1 <- ifelse((miChat$hora >= "06:00:00" & miChat$hora <= "07:00:00"), 6,+
                        ifelse((miChat$hora >= "07:00:00" & miChat$hora <= "08:00:00"), 7,+
                                 ifelse((miChat$hora >= "08:00:00" & miChat$hora <= "09:00:00"), 8,+
                                          ifelse((miChat$hora >= "09:00:00" & miChat$hora <= "10:00:00"), 9,+
                                                   ifelse((miChat$hora >= "10:00:00" & miChat$hora <= "11:00:00"), 10,+
                                                            ifelse((miChat$hora >= "11:00:00" & miChat$hora <= "12:00:00"), 11,+
                                                                     ifelse((miChat$hora >= "12:00:00" & miChat$hora <= "12:59:59"), 12,+
                                                                              ifelse((miChat$hora >= "01:00:00" & miChat$hora <= "02:00:00"), 1,+
                                                                                       ifelse((miChat$hora >= "02:00:00" & miChat$hora <= "03:00:00"), 2,+
                                                                                                ifelse((miChat$hora >= "03:00:00" & miChat$hora <= "04:00:00"), 3,+
                                                                                                         ifelse((miChat$hora >= "04:00:00" & miChat$hora <= "05:00:00"), 4,+
                                                                                                                  ifelse((miChat$hora >= "05:00:00" & miChat$hora <= "06:00:00"), 5, 100))))))))))))





mutate( estacion = factor(miChat$hora1) ) %>% 
  filter(!is.na(author))

# PALETA DE COLORES
paleta.estaciones <- brewer.pal(12,"Set1")[c(9,10,11,12,7,5,1,3,4,2,6,8)]
# VERIFICANDO CUÁNTOS MENSAJES SE ENVIARON DURANTE EL PERIODO DE TIEMPO
miChat %>% 
  group_by(hora1) %>% 
  count(day) %>%
  ggplot(aes(x = day, y = n, fill=hora1)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=paleta.estaciones) +
  ylab("Número de mensajes") + xlab("Fecha") +
  ggtitle("Mensajes por día", "Frecuencia por estación del año") +
  theme_minimal() +
  theme( legend.title = element_blank(), 
         legend.position = "bottom")
