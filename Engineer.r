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
  
  
#'Theoretical study of salaries in groups with different age / salary structures.
#'Suppose there is two groups A and B that both have flat age distributions. 
#'Group B have a flat salary distribution in general, in group A the oldest 
#'employees earns twice as much as the youngest in general.  
  
A <- seq(30000, 60000, by=750)
B <- seq(30000, 30000, length=41)
year <- 2019
by <- (year - 25):(year - 65)  
tibble(by, A, B) %>% 
  gather(A, B, key = "group", value = "salary") %>%
  ggplot() +
  geom_bar(mapping = aes(x = by, y = salary, fill = group), stat = "identity", position = "dodge") +
  labs(
    title = "Salary structure of Group A and Group B"
  )

#'During the year both group A and B increase the sum of all salaries for 
#'respective group by two percent.
  
tibble(A_raise = sum(A) * 0.02, B_raise = sum(B) * 0.02) %>% 
  gather(A_raise, B_raise, key="group", value="raise") %>%
  ggplot() +
    geom_bar(mapping = aes(x=group, y=raise, fill = group), stat = "identity") +
	labs(
      title = "Salary raise by 2.0%"
    )  


#'Suppose that each groups increase is divided equally to the employees within 
#'respective group.
	
raise <- (A + sum(A) * 0.02 / length (A)) - A	
g <- tibble(by, A, raise) %>% 
  gather(A, raise, key = "group", value = "salary")
g$group <-  factor(g$group, levels = c("raise", "A"))
g %>%
  ggplot() +
  geom_bar(mapping = aes(x = by, y = salary, fill = group), stat = "identity") +
  labs(
    title = "Salary increase distribution over age Group A"
  )		
	
#'Suppose that each groups increase is divided equally to the employees within 
#'respective group.
	
raise <- (B + sum(B) * 0.02 / length (B)) - B	
g <- as_tibble(cbind(by, B, raise)) %>% 
  gather(B, raise, key = "group", value = "salary")
g$group <-  factor(g$group, levels = c("raise", "B"))
g %>%
  ggplot() +
  geom_bar(mapping = aes(x = by, y = salary, fill = group), stat = "identity") +
  labs(
    title = "Salary increase distribution over age Group B"
  )
  
#'The oldest employees retire and new adolescents enter the job market. Suppose 
#'that the starting salary for respective group is determined by the 
#'age / salary structure.  
  
by_year2 <- by + 1
B_year2 <- lag(B)
B_year2[1] <- B[1] * 1.02
raise_year2 <- lag(B + sum(B) * 0.02 / length (B) - B)
raise_year2[1] <- 0
t <- tibble(by_year2, B_year2, raise_year2) %>%
  gather(B_year2, raise_year2, key = "group", value = "salary")
t$group <- factor(t$group, levels=c("raise_year2", "B_year2"))
t %>%
  ggplot() +
  geom_bar(mapping = aes(x = by_year2, y = salary, fill = group), stat = "identity") +
  labs(
    title = "Salary distribution Group B after one year succession"
  )	  

#'The oldest employees retire and new adolescents enter the job market. Suppose 
#'that the starting salary for respective group is determined by the 
#'age / salary structure.    
  
by_year2 <- by + 1
raise_year2 <- lag(A + sum(A) * 0.02 / length (A) - A)
raise_year2[1] <- 0
A_year2 <- lag(A)
A_year2[1] <- A[1] + raise_year2[2] - (A[2] - A[1])
t <- tibble(by_year2, A_year2, raise_year2) %>%
  gather(A_year2, raise_year2, key = "group", value = "salary")
t$group <- factor(t$group, levels = c("raise_year2", "A_year2"))
t %>%
  ggplot() +
  geom_bar(mapping = aes(x = by_year2, y = salary, fill = group), stat = "identity") +
  labs(
    title = "Salary distribution Group A after one year succession"
  )	 

#'Before next years’ salary revision the sum of the salaries have increased by 
#'2.0 % for group B and only 0.31% for group A
  
tibble(A_raise_sum = sum(A) * 0.02 - A[length(A)] + A_year2[1], B_raise_sum = sum(B) * 0.02) %>% 
  gather(A_raise_sum, B_raise_sum, key = "group", value = "raise") %>%
  ggplot() +
    geom_bar(mapping = aes(x=group, y=raise, fill = group), stat = "identity") +
	labs(
      title = "Salary raise since last revision"
    )   
	
#'This animation shows how the salary development progresses for a longer 
#'period of time according to the prerequicites stated above.

##A <- seq(30000, 60000, by = 750)
##B <- seq(30000, 30000, length = 41)
##for (year in 2019:2059){
##  by <- (year - 25):(year - 65)  
##  tibble(by, A, B) %>% 
##    gather(A, B, key = "group", value = "salary") %>%
##    ggplot() +
##      geom_point(mapping = aes(x = by, y = salary, colour = group)) +
##      labs(
##        title = "Salary development different groups.",
##		subtitle = paste ("Year of revision", year)
##      ) +
##	  scale_x_continuous(name = "Year of birth", limits = c(1954, 2034)) +
##      scale_y_continuous(name = "Salary", limits = c(30000, 80000))		  
##  ggsave(paste (year, sep="", ".png"))
##  A <- A + sum (A) * 0.020 / length (A)
##  A <- c(A[1] - 750, A[1:40])
##  B <- B * 1.02
##}	
	
#' The animation was made with ImageMagick
##"c:\Program Files\ImageMagick-7.0.8-Q16\magick.exe" -delay 50 -loop 0 *.png animation.gif	
#'![](https://github.com/MikaelLundqvist/SCB-Engineers-Jonkoping/blob/master/animation.gif)	

	
#'TBC  
  
#'References
#'http://www.statistikdatabasen.scb.se/pxweb/en/  
#'https://imagemagick.org/
  
#+ 