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
library(gganimate)
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
  ggplot () +
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
  ggplot () +
    geom_line (mapping = aes (x = year2, y = salary, colour = region)) +
    theme (legend.position = "bottom")

	
#'Average monthly pay, non-manual workers private sector (SLP) by region, occupational group (SSYK 2012) and sex. Year 2014 - 2018
#'Average monthly pay (total pay), non-manual workers private sector (SLP), SEK
#'214 Engineering professionals	
#'men and women
	
readfile ("0000002T.csv") %>% 
  ggplot () +
    geom_line (mapping = aes (x = year2, y = salary, colour = region)) +
    theme (legend.position = "bottom")
	
#'Genomsnittlig grund- och månadslön samt kvinnors lön i procent av mäns lön
#'efter utbildningsgrupp SUN 2000 och kön. År 2004 - 2017	
#'Månadslön
#'Kön=Totalt
#'Only available at the Swedish SCB site

readfile ("AM0110D2.csv") %>% 
  ggplot () +
    geom_line (mapping = aes (x = year2, y = salary, colour = `utbildningsgrupp SUN 2000`)) +
    theme (legend.position = "bottom") + 	
	guides(col = guide_legend(title.position = "top", nrow = 5))
	
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
#'Approximaton with B-spline
	
readfile ("00000031.csv") %>% 
  group_by (`occuptional  (SSYK 2012)`) %>% 
  summarise (tot = parse_number (tot_dev (salary))) %>%
  arrange (desc (tot))
  
#'Average monthly pay (total pay), non-manual workers private sector (SLP), 
#'SEK by occuptional (SSYK), age, sex and year 2000-2013 
#'214 Engineering professionals 
#'sex=total  
##readfile("AM0103A9.csv") %>% rowwise() %>% mutate(age2 = unlist(lapply(strsplit(substr(age, 1, 5), "-"), strtoi))[1]) %>%
##rowwise() %>% mutate(age3 = unlist(lapply(strsplit(substr(age, 1, 5), "-"), strtoi))[2]) %>% 
## ggplot(mapping = aes(x = year2 - (age2 + age3) / 2, y = salary)) +
##   geom_point() + 
##   geom_smooth(method = lm, formula = y ~ splines::bs(x, 8), se = FALSE) +
##   transition_time(year2) +
##   labs(title = "Year: {frame_time}") +
##	scale_x_continuous(name = "Year of birth") +
##   scale_y_continuous(name = "Salary")
##anim_save("2000-2013.gif", width = 1000, height = 1000)	
#'![](https://github.com/MikaelLundqvist/SCB-Engineers-Jonkoping/blob/master/2000-2013.gif)	
  
#'Average monthly pay (total pay), non-manual workers private sector (SLP), 
#'SEK by occuptional (SSYK 2012), age, sex and year 2014-2018
#'214 Engineering professionals 
#'sex=total
##readfile("00000031_2.csv") %>% rowwise() %>% mutate(age2 = unlist(lapply(strsplit(substr(age, 1, 5), "-"), strtoi))[1]) %>%
##rowwise() %>% mutate(age3 = unlist(lapply(strsplit(substr(age, 1, 5), "-"), strtoi))[2]) %>% 
## ggplot(mapping = aes(x = year2 - (age2 + age3) / 2, y = salary)) +
##   geom_point() + 
##   geom_smooth(method = lm, formula = y ~ splines::bs(x, 8), se = FALSE) +
##   transition_time(year2) +
##   labs(title = "Year: {frame_time}") +
##	scale_x_continuous(name = "Year of birth") +
##   scale_y_continuous(name = "Salary")	
##anim_save("2014-2018.gif", width = 1000, height = 1000)  
#'![](https://github.com/MikaelLundqvist/SCB-Engineers-Jonkoping/blob/master/2014-2018.gif)	

#'Average monthly pay (total pay), non-manual workers private sector (SLP), 
#'SEK by occuptional (SSYK 2012), age, sex and year 2000-2013
#'214 Engineering professionals 
#'sex=total
#'Growth of salaries by age group
##csvfile <- readfile("AM0103A9.csv") %>% 
##  rowwise() %>% 
##  mutate(age2 = unlist(lapply(strsplit(substr(age, 1, 5), "-"), strtoi))[1]) %>%
##  rowwise() %>% mutate(age3 = unlist(lapply(strsplit(substr(age, 1, 5), "-"), strtoi))[2])	
##yearwise <- group_split(csvfile %>% group_by(year2))	
##ageGroupGrowth = data.frame()	
##for (i in 1:13){
##  temp <- cbind(year = 1999 + i, age = (yearwise[[i]]$age2 + yearwise[[i]]$age3) / 2, growth = yearwise[[i+1]]$relsalary / yearwise[[i]]$relsalary) - 1
##  ageGroupGrowth <- rbind(myvar2, temp)
##}	
##ageGroupGrowth[, 'age'] <- factor(ageGroupGrowth[, 'age'])		
##ageGroupGrowth %>%	
##  ggplot(mapping = aes(x = age, y = growth)) +
##  geom_bar(stat = "identity") +
##  transition_time(as.numeric(year)) +
##  labs(title = "Year: {frame_time}") +
##  scale_x_discrete(name = "Age group") +
##  scale_y_continuous(name = "Salary increase (%)")	
##anim_save("AgeGroupGrowth2000-2013.gif", width = 1000, height = 1000)  
#'![](https://github.com/MikaelLundqvist/SCB-Engineers-Jonkoping/blob/master/AgeGroupGrowth2000-2013.gif)	  

#'Average monthly pay (total pay), non-manual workers private sector (SLP), 
#'SEK by occuptional (SSYK 2012), age, sex and year 2000-2013
#'214 Engineering professionals 
#'sex=total
#'Individual salary increase by birthyear, estimated from B-spline approximation
##csvfile <- readfile("AM0103A9.csv") %>% 
##  rowwise() %>% 
##  mutate(age2 = unlist(lapply(strsplit(substr(age, 1, 5), "-"), strtoi))[1]) %>%
##  rowwise() %>% mutate(age3 = unlist(lapply(strsplit(substr(age, 1, 5), "-"), strtoi))[2])
##indGrowth = data.frame()
##for (i in 2000:2012){
##  yearfile <- filter(csvfile, year2 == i)
##  x = yearfile$year2 - (yearfile$age2 + yearfile$age3) / 2
##  model = lm(yearfile$salary ~ splines::bs(x, 8))
##  X1 = data.frame(x = unlist(map2( i - 62, i - 22, seq)))
##  Y1 = predict(model, X1) 
##  yearfile <- filter(csvfile, year2 == i + 1)
##  x = yearfile$year2 - (yearfile$age2 + yearfile$age3) / 2
##  model = lm(yearfile$salary ~ splines::bs(x, 8))
##  X2 = data.frame(x = unlist(map2( i + 1 - 62, i + 1 - 22, seq)))    
##  Y2 = predict(model, X2)
##  growth = Y2[1:40] / Y1[2:41]  
##  temp <- as_tibble(cbind(year = i+1, by=X2[1:40,1], growth=growth))
##  indGrowth <- rbind(indGrowth, temp)
##} 
##indGrowth %>%	
##  ggplot(mapping = aes(x = by, y = growth)) +
##  geom_line() +
## transition_time(year) +
##  labs(title = "Year: {frame_time}") +
##  scale_x_continuous(name = "Year of birth") +
##  scale_y_continuous(name = "Salary increase (%)") 
##anim_save("indGrowth2000-2013.gif", width = 1000, height = 1000)
#'![](https://github.com/MikaelLundqvist/SCB-Engineers-Jonkoping/blob/master/indGrowth2000-2013.gif)

#'Average monthly pay (total pay), non-manual workers private sector (SLP), 
#'SEK by occuptional (SSYK 2012), age, sex and year 2000-2013
#'214 Engineering professionals 
#'sex=total
#'Changes in the salary structure part by birthyear, salary structure part is 
#'defined as the derivative of the age / salary function, salary structure 
#'part defines how much the salaries needs to increase each year so that the
#'structure remains unchanged.
##csvfile <- readfile("AM0103A9.csv") %>% 
##  rowwise() %>% 
##  mutate(age2 = unlist(lapply(strsplit(substr(age, 1, 5), "-"), strtoi))[1]) %>%
##  rowwise() %>% mutate(age3 = unlist(lapply(strsplit(substr(age, 1, 5), "-"), strtoi))[2])  
##salaryStructure = data.frame()
##for (i in min(csvfile$year2):max(csvfile$year2)){
##  yearfile <- filter(csvfile, year2 == i)
##  x = yearfile$year2 - (yearfile$age2 + yearfile$age3) / 2
##  model = lm(yearfile$salary ~ splines::bs(x, 8))
##  X = data.frame(x = unlist(map2( i - 62, i - 22, seq, length = 100)))
##  Y = predict(model, X) 
##  dX = rowMeans(embed(X$x, 2))
##  dY = -diff(Y) / diff(X$x) / Y
##  temp <- as_tibble(cbind(year = i, dX = dX, dY = dY))
##  salaryStructure <- rbind(salaryStructure, temp)
##} 
##salaryStructure %>%	
##  ggplot(mapping = aes(x = dX, y = dY)) +
##  geom_line() +
##  transition_time(year) +
##  labs(title = "Year: {frame_time}") +
##  scale_x_continuous(name = "Year of birth") +
##  scale_y_continuous(name = "Structural increase (%)") 
##anim_save("salaryStructure2000-2013.gif", width = 1000, height = 1000)
#'![](https://github.com/MikaelLundqvist/SCB-Engineers-Jonkoping/blob/master/salaryStructure2000-2013.gif)
  
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
## by <- (year - 25):(year - 65)  
## tibble(by, A, B) %>% 
##   gather(A, B, key = "group", value = "salary") %>%
##   ggplot() +
##     geom_point(mapping = aes(x = by, y = salary, colour = group)) +
##     labs(
##       title = "Salary development different groups.",
##		subtitle = paste ("Year of revision", year)
##     ) +
##	  scale_x_continuous(name = "Year of birth", limits = c(1954, 2034)) +
##     scale_y_continuous(name = "Salary", limits = c(30000, 80000))		  
## ggsave(paste (year, sep="", ".png"))
## A <- A + sum (A) * 0.020 / length (A)
## A <- c(A[1] - 750, A[1:40])
## B <- B * 1.02
##}	
	
#' The animation was made with ImageMagick
##"c:\Program Files\ImageMagick-7.0.8-Q16\magick.exe" -delay 50 -loop 0 *.png animation.gif	
#'![](https://github.com/MikaelLundqvist/SCB-Engineers-Jonkoping/blob/master/animation.gif)	

	
#'TBC  
  
#'References
#'http://www.statistikdatabasen.scb.se/pxweb/en
#'http://www.statistikdatabasen.scb.se/pxweb/sv
#'https://imagemagick.org/
  
#+ 