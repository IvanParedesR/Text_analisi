
# Text Mining with R
# A Tidy Approach
# Julia Silge and David Robinson
# 2019-11-17
# Welcome to Text Mining with R
# https://www.tidytextmining.com/

#Texto mining
  
text <- c("Because I could not stop for Death -",
            "He kindly stopped for me -",
            "The Carriage held but just Ourselves -",
            "and Immortality")  
text

texto2 <- c("Porque te tengo y no",
"porque te pienso",
"porque la noche está de ojos abiertos",
"porque la noche pasa y digo amor",
"porque has venido a recoger tu imagen",
"y eres mejor que todas tus imágene",
"porque eres linda desde el pie hasta el alma",
"porque eres buena desde el alma a mí")

texto2
library(dplyr)
text_df <- tibble(line = 1:4, text = text)
text_df
 
library(dplyr)
text_df2 <- tibble(line = 1:8, texto2 = texto2)
text_df2

library(tidytext)
text_df2 %>%
  unnest_tokens(word, texto2)



install.packages("zipfR") #The zipfR package performs Large-Number-of-Rare-Events (LNRE) 
#modeling of (linguistic) type frequency distributions (Baayen 2001) and provides 
#utilities to run various forms of lexical statistics analysis in R.
?`zipfR-package`

library(zipfR)

data(Dickens.spc)
data(BrownVer.spc)
View(Dickens.spc)

N(BrownVer.spc) # 166262
V(BrownVer.spc) # 10007
Vm(BrownVer.spc,1) # 3787
N(Dickens.spc) # 2817208
V(Dickens.spc) # 41116
Vm(Dickens.spc,1) # 14220

plot(log(BrownVer.spc$m),log(BrownVer.spc$Vm))

di.vgc <- vgc.interp(Dickens.spc,(1:100)*28170)
br.vgc <- vgc.interp(BrownVer.spc,(1:100)*1662)

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

