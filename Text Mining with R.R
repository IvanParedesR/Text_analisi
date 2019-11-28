
#Text Mining with R
A Tidy Approach
Julia Silge and David Robinson
2019-11-17
Welcome to Text Mining with R


### Prueba 1 con jpg
text <- tesseract::ocr("https://www.intentarlo.com/wp-content/uploads/2017/08/frases-de-superacion-personal.jpg")
cat(text)

### Prueba 2 con pdf
pngfile <- pdftools::pdf_convert('https://jeroen.github.io/images/ocrscan.pdf', dpi = 600)

text <- tesseract::ocr(pngfile)
cat(text)

pngfile <- pdftools::pdf_convert('D:/Users/iparedes/Documents/2019/Ponencias/DC-003-2018Anexo.pdf', language = es, pages = 1, dpi = 600)

text <- tesseract::ocr(pngfile)
cat(text)

pngfile <- pdftools::pdf_convert('D:/Users/iparedes/Documents/2019/Ponencias/DC-003-2018Anexo.pdf', dpi = 600)

text <- tesseract::ocr(pngfile)
cat(text)


?tesseract


pngfile1 <- pdftools::pdf_convert('D:/Users/iparedes/Documents/2019/Ponencias/EscritoInicial_CNT-116-2019.pdf', pages = 9, dpi = 600)

text1 <- tesseract::ocr(pngfile1)
cat(text1)

devtools::install_github("ropensci/magick")


text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")

text

library(dplyr)
text_df <- tibble(line = 1:4, text = text)
  
text_df
  
library(tidytext)
  
text_df %>%
    unnest_tokens(word, text)
  
library(janeaustenr)
library(dplyr)
library(stringr)

original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup()

original_books
