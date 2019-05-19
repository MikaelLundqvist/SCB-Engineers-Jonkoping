#'---
#'title: "SCB Engineers Jönköping"
#'author: "Mikael Lundqvist"
#'date: "9 maj 2019"
#'output: github_document
#'---

##rmarkdown::render("Engineer.r", encoding="UTF-8")

#+ r setup, include=FALSE
knitr::opts_chunk$set(echo = TRUE)
#+ 

#'Introduction
#'Personal development in R, Statisics, Scientific Report, Markdown, and GitHub
#'with data from Statistics Sweden. I will extract statistics from Statistics 
#'Sweden regarding the labour market, salaries and other relevant data from 
#'engineers, primarily in Jönköping county. 

#'Help functions

#+ r
library (tidyverse)
relative_dev <- function (x){
  return (x / x[1])
}
tot_dev <- function (x){
  scales::percent ((tail(x, 1) / x[1]) - 1)
}
perc_women <- function(x){
  scales::percent (x[2] / x[1])
}
## Limited to statistics from 2000 or later
readfile <- function (file1){
  read_csv (file1, col_types = cols(), locale = readr::locale (encoding = "latin1"), na = c("..", "NA")) %>%
    drop_na() %>%
    gather (starts_with("20"), key = "year", value = salary) %>%
    mutate (year2 = parse_number (year)) %>%
    mutate (heading = file1) %>%
    mutate (relsalary = relative_dev (salary))
}	

#'Average monthly pay, pay dispersion etc., non-manual workers private sector (SLP) by educational orientation SUN 2000 and sex. Year 2001 - 2018
#'Average monthly pay (total pay), SEK
#'SUN2000, 4 natural sciences, mathematics and computing
#'sex, men and women

readfile ("AM0103I6.csv") %>%
  ggplot () +
    geom_line (mapping = aes(x = year2,y = salary, colour = `field of education SUN 2000`)) +
    theme (legend.position = "bottom") 

readfile ("AM0103I6.csv") %>% 
  group_by (`field of education SUN 2000`) %>% 
  mutate (growth = c (NA, diff(salary)) / salary) %>%
  ggplot () +  
    geom_line (mapping = aes(x = year2, y = growth, colour = `field of education SUN 2000`)) +
    theme (legend.position = "bottom")	

readfile ("AM0103I6_2.csv") %>% 
  group_by (`field of education SUN 2000`) %>% 
  summarise (tot = tot_dev (salary)) %>%
  arrange (desc (tot))

  
#'Employees 16-64 years by region of work, occupation (3-digit SSYK 2012) and sex. Year 2001 - 2017
#'Jönköping county, occupation=311 Physical and engineering science technicians, men and women  
#'In 2014 SCB changed from SSYK96 to SSYK2012
  

readfile ("AM0208B6.csv") %>% 
  group_by (year2) %>% 
  summarise (total = sum (salary)) %>%
  ggplot ()+
    geom_line (mapping = aes (x = year2, y = total)) +
    theme (legend.position = "bottom")
  
readfile ("000000NL.csv") %>% 
  group_by (year2) %>% 
  summarise (total = sum (salary)) %>%
  ggplot ()+
    geom_line (mapping = aes (x = year2, y = total)) +
    theme (legend.position = "bottom")  
	
	
#'Percent women in the countys
#'214 Engineering professionals


options(tibble.print_max = Inf) 	
readfile ("000000NL_3.csv") %>% 
  group_by (`region`, year) %>% 
  summarise (perc_women = perc_women (salary)) %>%	
  arrange (desc (perc_women))
  
#'Percent women in the countys
#'214 Engineering professionals
#'Joint-stock corporations not controlled by the government sector

readfile ("000000RM.csv") %>% 
  group_by (`region`, year) %>% 
  summarise (perc_women = perc_women (salary)) %>%	
  arrange (desc (perc_women))
  
  
#'Average monthly pay, non-manual workers private sector (SLP) by region, occupational group (SSYK) and sex. Year 2000 - 2013
#'Average monthly pay (total pay), non-manual workers private sector (SLP), SEK
#'214 Engineering professionals	
#'men and women	
  
readfile ("AM0103H2.csv") %>% 
  ggplot ()+
    geom_line (mapping = aes (x = year2, y = salary, colour = region)) +
    theme (legend.position = "bottom")

	
#'Average monthly pay, non-manual workers private sector (SLP) by region, occupational group (SSYK 2012) and sex. Year 2014 - 2018
#'Average monthly pay (total pay), non-manual workers private sector (SLP), SEK
#'214 Engineering professionals	
#'men and women
	
readfile ("0000002T.csv") %>% 
  ggplot ()+
    geom_line (mapping = aes (x = year2, y = salary, colour = region)) +
    theme (legend.position = "bottom")
	
#'Average monthly pay (total pay), non-manual workers private sector (SLP), SEK by occuptional (SSYK 2012), age, sex and year, Year 2000 - 2013	
#'age=total
#'sex=total
	
readfile ("AM0103E6.csv") %>% 
  group_by (`occupational group (SSYK)`) %>% 
  summarise (tot = tot_dev (salary)) %>%
  arrange (desc (tot))	
	
#'Average monthly pay (total pay), non-manual workers private sector (SLP), SEK by occuptional (SSYK 2012), age, sex and year, Year 2014 - 2018
#'age=total
#'sex=total
	
readfile ("00000031.csv") %>% 
  group_by (`occuptional  (SSYK 2012)`) %>% 
  summarise (tot = parse_number (tot_dev (salary))) %>%
  arrange (desc (tot))
  
  
	
#'TBC  
  
#'References
#'http://www.statistikdatabasen.scb.se/pxweb/en/  
  
#+ 