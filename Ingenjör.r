#'---
#'title: "Ingenjör"
#'author: "Mikael Lundqvist"
#'date: "9 maj 2019"
#'output: github_document
#'---

##rmarkdown::render("Ingenjör.r", encoding="UTF-8")

#+ r setup, include=FALSE
knitr::opts_chunk$set(echo = TRUE)
#+ 

#'Hjälpfunktioner

#+ r
library (tidyverse)
relative <- function (x){
  return (x / x[1])
}
tot_utv <- function (x){
  scales::percent ((tail(x, 1) / x[1]) - 1)
}
readfile <- function (file1){
  read_csv (file1, locale = readr::locale (encoding = "latin1")) %>%
    gather (starts_with("20"), key = "year", value = lön) %>%
    mutate (year2 = parse_number (year)) %>%
    mutate (heading = file1) %>%
    mutate (rellön = relative (lön)) %>%
    select (year, lön, rellön, year2, heading)
}
readfile2 <- function (file1){
  read_csv (file1, col_types = cols(), locale = readr::locale (encoding = "latin1")) %>%
    gather (starts_with("20"), key = "year", value = lön) %>%
    mutate (year2 = parse_number (year)) %>%
    mutate (heading = file1) %>%
    mutate (rellön = relative (lön))
}	

#'Genomsnittlig månadslön, lönespridning m.m., tjänstemän privat sektor (SLP) efter utbildningsinriktning SUN 2000 och kön. Ãr 2001 - 2017
#'Genomsnittlig månadslön (totallön), kronor
#'SUN2000, 4 naturvetenskap, teknik, data
#'kön, totalt

readfile2 ("AM0103I6.csv") %>%
  ggplot () +
    geom_line (mapping = aes(x = year2,y = lön, colour = `utbildningsinriktning SUN 2000`)) +
    theme (legend.position = "bottom") 

readfile2 ("AM0103I6.csv") %>% 
  group_by (`utbildningsinriktning SUN 2000`) %>% 
  mutate (growth = c (NA, diff(lön)) / lön) %>%
  ggplot () +  
    geom_line (mapping = aes(x = year2, y = growth, colour = `utbildningsinriktning SUN 2000`)) +
    theme (legend.position = "bottom")	

readfile2 ("AM0103I6_2.csv") %>% 
  group_by (`utbildningsinriktning SUN 2000`) %>% 
  summarise (tot = tot_utv (lön))

#'Anställda 16-64 år med arbetsplats i regionen (dagbef) efter region, Yrke (SSYK 2012), kön och år
#'Jönköpings län, yrke=311 Ingenjörer och tekniker, män och kvinnor
#'2014 bytte SCB till SSYK2012 från SSYK96 

readfile2 ("AM0208B6.csv") %>% 
  group_by (year2) %>% 
  summarise (total = sum (lön)) %>%
  ggplot ()+
    geom_line (mapping = aes (x = year2, y = total)) +
    theme (legend.position = "bottom")
  
readfile2 ("000000NL.csv") %>% 
  group_by (year2) %>% 
  summarise (total = sum (lön)) %>%
  ggplot ()+
    geom_line (mapping = aes (x = year2, y = total)) +
    theme (legend.position = "bottom")  

#'Genomsnittlig månadslön (totallön), tjänstemän privat sektor (SLP), kronor efter region, yrkesgrupp (SSYK), kön och år
#'yrkesgrupp (SSYK 2012) och kön. År 2000 - 2013
#'Genomsnittlig månadslön (totallön), tjänstemän privat sektor (SLP), kronor
#'214 Civilingenjörsyrken
#'män och kvinnor  
  
readfile2 ("AM0103H2.csv") %>% 
  ggplot ()+
    geom_line (mapping = aes(x = year2, y = lön, colour = region)) +
    theme (legend.position = "bottom")

#'Genomsnittlig månadslön, tjänstemän privat sektor (SLP) efter region, yrkesgrupp (SSYK 2012) och kön. År 2014 - 2017
#'Genomsnittlig månadslön (totallön), tjänstemän privat sektor (SLP), kronor
#'214 Civilingenjörsyrken
#'män och kvinnor

readfile2 ("0000002T.csv") %>% 
  ggplot ()+
    geom_line (mapping=aes(x = year2, y = lön, colour = region)) +
    theme (legend.position = "bottom")

#'TBC  
  
#+ 