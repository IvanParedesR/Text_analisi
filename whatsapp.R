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





library("ggplot2"); theme_set(theme_minimal())
library("lubridate")
miChat %>%
  mutate(day = date(time)) %>%
  count(day) %>%
  ggplot(aes(x = day, y = n)) +
  geom_bar(stat = "identity") +
  ylab("") + xlab("") +
  ggtitle("Mensajes por día")


miChat %>%
  mutate(day = date(time)) %>%
  count(author) %>%
  ggplot(aes(x = reorder(author, n), y = n)) +
  geom_bar(stat = "identity") +
  ylab("") + xlab("") +
  coord_flip() +
  ggtitle("Number of messages")


library("tidyr")
miChat %>%
  unnest(emoji) %>%
  count(author, emoji, sort = TRUE) %>%
  group_by(author) %>%
  top_n(n = 6, n) %>%
  ggplot(aes(x = reorder(emoji, n), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  facet_wrap(~author, ncol = 2, scales = "free_y")  +
  ggtitle("Most often used emojis")

library("ggimage")
emoji_data <- rwhatsapp::emojis %>% # data built into package
  mutate(hex_runes1 = gsub("\\s[[:alnum:]]+", "", hex_runes)) %>% # ignore combined emojis
  mutate(emoji_url = paste0("https://abs.twimg.com/emoji/v2/72x72/", 
                            tolower(hex_runes1), ".png"))

miChat %>%
  unnest(emoji) %>%
  count(author, emoji, sort = TRUE) %>%
  group_by(author) %>%
  top_n(n = 6, n) %>%
  left_join(emoji_data, by = "emoji") %>% 
  ggplot(aes(x = reorder(emoji, n), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  geom_image(aes(y = n + 20, image = emoji_url)) +
  facet_wrap(~author, ncol = 2, scales = "free_y") +
  ggtitle("Most often used emojis") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())


library("tidytext")
miChat %>%
  unnest_tokens(input = text,
                output = word) %>%
  count(author, word, sort = TRUE) %>%
  group_by(author) %>%
  top_n(n = 6, n) %>%
  ggplot(aes(x = reorder_within(word, n, author), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  facet_wrap(~author, ncol = 2, scales = "free_y") +
  scale_x_reordered() +
  ggtitle("Most often used words")

library("stopwords")
to_remove <- c(stopwords(language = "es"),
               "que",
               "de",
               "en",
               "el",
               "la",
               "no",
               "android.s.wt")

miChat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(!word %in% to_remove) %>%
  count(author, word, sort = TRUE) %>%
  group_by(author) %>%
  top_n(n = 6, n) %>%
  ggplot(aes(x = reorder_within(word, n, author), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  facet_wrap(~author, ncol = 2, scales = "free_y") +
  scale_x_reordered() +
  ggtitle("Most often used words")