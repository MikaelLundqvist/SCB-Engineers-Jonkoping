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
library(car)
relative_dev <- function (x){  
  return (x / x[1])  
}  
tot_dev <- function (x){  
  scales::percent ((tail(x, 1) / x[1]) - 1)  
}  
perc_women <- function(x){  
  scales::percent (x[2] / x[1])  
}   
readfile <- function (file1){  
  read_csv (file1, col_types = cols(), locale = readr::locale (encoding = "latin1"), na = c("..", "NA")) %>%  
    gather (starts_with("19"), starts_with("20"), key = "year", value = salary) %>%  
	drop_na() %>%  
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

readfile ("AM0103H2.csv") %>%   
  group_by (region) %>%
  mutate (growth = c (NA, diff(salary)) / salary) %>%  
  ggplot () +    
    geom_line (mapping = aes(x = year2, y = growth, colour = region)) +  
    theme (legend.position = "bottom")	  	
	  
#'Average monthly pay, non-manual workers private sector (SLP) by region, occupational group (SSYK 2012) and sex. Year 2014 - 2018  
#'Average monthly pay (total pay), non-manual workers private sector (SLP), SEK  
#'214 Engineering professionals	  
#'men and women  
	  
readfile ("0000002T.csv") %>%   
  ggplot () +  
    geom_line (mapping = aes (x = year2, y = salary, colour = region)) +  
    theme (legend.position = "bottom")  
	
readfile ("0000002T.csv") %>%   
  group_by (region) %>%
  mutate (growth = c (NA, diff(salary)) / salary) %>%  
  ggplot () +    
    geom_line (mapping = aes(x = year2, y = growth, colour = region)) +  
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
	
#'Befolkningen 2006-2018 fördelad efter utbildningsgrupp (SUN 2000) och kön. 25-64 år	
#'Only available at the Swedish SCB site  

readfile("tab8_tidsserie_2018.csv") %>%
  filter(`Utbildningsgrupp (SUN 2000)` == "Teknik och tillverkning") %>%
  filter(grepl("Civilingenjörsutbildning", `SUN 2000`)) %>%
  filter(grepl("Samtliga", year)) %>%
  ggplot () +  
    geom_line (mapping = aes (x = year2, y = salary, colour = `SUN 2000`)) +  
    theme (legend.position = "bottom") + 	  
	guides(col = guide_legend(title.position = "top", nrow = 5)) +
    scale_y_continuous(name = "Antal")    
	  
#'Average monthly pay (total pay), non-manual workers private sector (SLP), SEK by occuptional (SSYK 2012), age, sex and year, Year 2000 - 2013	  
#'age=total  
#'sex=total  
	  
readfile ("AM0103E6.csv") %>%   
  group_by (`occupational group (SSYK)`) %>%   
  summarise (tot = tot_dev (salary)) %>%  
  arrange (desc (tot))	  
  
#'Genomsnittlig månadslön, lön i fasta priser och lönespridning efter 
#'utbildningsnivå SUN 2000 och kön. År 1991 - 2015  
#'Genomsnittlig lön, kr
#'Only available at the Swedish SCB site

tb <- readfile("AM0112C1.csv") %>%
  filter(year2 > 1994) %>%
  group_by (`Utbildningsnivå SUN 2000`, kön) %>%   
  mutate (grouprelsal = relative_dev (salary))

tb %>%
  ggplot () +  
    geom_point (mapping = aes(x = year2,y = grouprelsal, colour = `Utbildningsnivå SUN 2000`, shape=kön))  
  
tb %>%
  ggplot () +  
    geom_point (mapping = aes(x = year2,y = log(grouprelsal), colour = `Utbildningsnivå SUN 2000`, shape=kön))
	
model <- lm (log(grouprelsal) ~ `Utbildningsnivå SUN 2000` + year2 + kön, data = tb)

tb <- bind_cols(tb, as_tibble(exp(predict(model, tb, interval = "confidence")))) 

tb %>%
  ggplot () +  
    geom_point (mapping = aes(x = year2,y = log(fit), colour = `Utbildningsnivå SUN 2000`, shape=kön))
  
model1 <- lm (log(grouprelsal) ~ `Utbildningsnivå SUN 2000` * year2 * kön, data = tb)  

tb <- bind_cols(tb, as_tibble(exp(predict(model1, tb, interval = "confidence"))))

tb %>%
  ggplot () +  
    geom_point (mapping = aes(x = year2,y = log(fit1), colour = `Utbildningsnivå SUN 2000`, shape=kön))
	
model2 <- lm (log(grouprelsal) ~ `Utbildningsnivå SUN 2000` * poly(year2, 2) * kön, data = tb)  

tb <- bind_cols(tb, as_tibble(exp(predict(model2, tb, interval = "confidence"))))

tb %>%
  ggplot () +  
    geom_point (mapping = aes(x = year2,y = log(fit2), colour = `Utbildningsnivå SUN 2000`, shape=kön))
	
tb %>%
  ggplot () +  
    geom_point (mapping = aes(x = year2,y = fit2, colour = `Utbildningsnivå SUN 2000`, shape=kön))	
	
tb <- tb %>% mutate(diffpolylin = fit2 - fit1)

tb %>%
  ggplot () +  
    geom_point (mapping = aes(x = year2,y = diffpolylin, colour = `Utbildningsnivå SUN 2000`, shape=kön))
	
summary (model2)

Anova(model2, type=2)

#'Average monthly pay, non-manual workers private sector (SLP) by occupational 
#'group (SSYK) age and sex. Year 2000 - 2013
#'Average monthly pay (total pay), non-manual workers private sector (SLP), SEK 
#'by occupational group (SSYK), age, sex and year

tb <- readfile("AM0103A9_1.csv") %>% 
  rowwise() %>% 
  mutate(age2 = unlist(lapply(strsplit(substr(age, 1, 5), "-"), strtoi))[1]) %>%  
  rowwise() %>% 
  mutate(age3 = unlist(lapply(strsplit(substr(age, 1, 5), "-"), strtoi))[2]) %>% 
  mutate(age4 = (age3 + age2) / 2) %>% 
  group_by (`occupational group (SSYK)`, age, sex) %>%   
  mutate (grouprelsal = relative_dev (salary))  
  
tb %>%
  ggplot () +  
    geom_point (mapping = aes(x = year2,y = log(salary), colour = age, shape=sex))  
	
model <- lm (log(salary) ~ year2 + sex + poly(age4, 3), data = tb)

tb <- bind_cols(tb, as_tibble(exp(predict(model, tb, interval = "confidence"))))

tb %>%
  ggplot () +  
    geom_point (mapping = aes(x = year2,y = log(fit), colour = age, shape=sex))
	
summary(model)	

Anova(model, type=2)

tb <- readfile("AM0103A9.csv") %>% 
  rowwise() %>% 
  mutate(age2 = unlist(lapply(strsplit(substr(age, 1, 5), "-"), strtoi))[1]) %>%  
  rowwise() %>% 
  mutate(age3 = unlist(lapply(strsplit(substr(age, 1, 5), "-"), strtoi))[2]) %>% 
  mutate(age4 = (age3 + age2) / 2) %>% 
  group_by (`occupational group (SSYK)`, age, sex) %>%   
  mutate (grouprelsal = relative_dev (salary))  
   	
model <- lm (log(salary) ~ `occupational group (SSYK)` + year2 + sex + poly(age4, 3), data = tb)
	
summary(model)	

Anova(model, type=2)
	  
#'Average monthly pay (total pay), non-manual workers private sector (SLP), SEK by occuptional (SSYK 2012),  , sex and year, Year 2014 - 2018  
#'age=total  
#'sex=total  
#'Approximaton with B-spline  
	      
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
  
#'Appendix  
#'All tables avaliable at Statistics Sweden, Labour market  
  
#'Labour market  
#'  Aggregate gross pay, payroll taxes and prel. tax statistics from employers monthly tax returns  
#'  Aggregate gross pay, payroll taxes and prel. tax statistics  
#'	Gross pay, payroll taxes and preliminary tax from employers monthly tax returns, by sector. Quarter 2001K1 - 2019K1 [2019-05-23]  
#'	Gross pay, payroll taxes and preliminary tax withheld by employers, by industry NACE Rev. 2 (aggr. level), for the business sector. Quarter 2008K1 - 2019K1 [2019-05-23]  
#'	Gross pay, payroll taxes and preliminary tax withheld by employers, by industry NACE Rev. 2, for the business sector. Quarter 2008K1 - 2019K1 [2019-05-23]  
#'  Old tables – not updated  
#'	Gross pay, payroll taxes and preliminary tax from employers monthly tax returns, by industry for the private sector. Quarter 2000K1 - 2008K4 [2009-03-02]  
#'	Gross pay, payroll taxes and preliminary tax from employers monthly tax returns, by sector (not updated). Quarter 2005K1 - 2015K1 [2015-05-22]  
#'	Gross pay, payroll taxes and preliminary tax withheld by employers, by industry Nace Rev. 2 (aggr. level), for the private sector (not updated). Quarter 2009K1 - 2015K1 [2015-05-22]  
#'	Gross pay, payroll taxes and preliminary tax withheld by employers, by industry NACE Rev. 2, for the private sector (not updated). Quarter 2013K1 - 2015K1 [2015-05-21]  
#'  Gross pay based on income of statements  
#'  Aggregate wages, prel. tax reported by employers and the amount of social benefit payments  
#'	Gross pay (SEK) and number of statements of income distributed by region of residence. Year 2010 - 2017 [2018-09-20]  
#'	Gross pay (SEK) and number of statements of income distrubuted by region of work and residence. Year 2005 - 2017 [2018-11-29]  
#'	Gross pay (SEK), preliminary tax and number of statements of income distributed by region. Year 1998 - 2017 [2018-11-29]  
#'  Aggregate wages, prel. tax reported by employers and the amount of social benefit payments by sex  
#'	Gross pay (SEK) and number of statements of income distributed by region of work and residence and sex. Year 2005 - 2017 [2018-11-29]  
#'  Job openings and unmet labour demand  
#'  Job openings and vacancies  
#'	Job openings and vacancies, Business by industry Nace Rev 2, Quarter 2015K2 - 2019K1 [2019-05-23]  
#'	Job openings and vacancies, Business by number of employees, Quarter 2015K2 - 2019K1 [2019-05-23]  
#'	Job openings and vacancies, Business by region and industry Nace Rev 2, Quarter 2015K2 - 2019K1 [2019-05-23]  
#'	Job openings and vacancies, Business by region NUTS2, Quarter 2015K2 - 2019K1 [2019-05-23]  
#'  Recruitment and vacancy rate  
#'	Recruitment and vacancy rate, Business by industry Nace rev 2, Quarter 2015K2 - 2019K1 [2019-05-23]  
#'	Recruitment and vacancy rate, Business by number of employees, Quarter 2015K2 - 2019K1 [2019-05-23]  
#'	Recruitment and vacancy rate, Business by region and industry Nace rev 2, Quarter 2015K2 - 2019K1 [2019-05-23]  
#'	Recruitment and vacancy rate, Business by region NUTS2, Quarter 2015K2 - 2019K1 [2019-05-23]  
#'  Recruitment time  
#'	Average recruitment time, in months, Business by Nace rev 2, Quarter 2015K2 - 2019K1 [2019-05-23]  
#'	Average recruitment time, in months, Business by region NUTS 2, Quarter 2015K2 - 2019K1 [2019-05-23]  
#'  Old tables, not updated  
#'	Average recruitment time, in months, private sector by Nace rev 1.1, Quarter 2003K3 - 2008K4 [2009-02-17]  
#'	Average recruitment time, in months, private sector by Nace rev 2, Quarter 2009K1 - 2015K1 [2015-05-21]  
#'	Average recruitment time, in months, private sector by region NUTS 2. Quarter 2003K3 - 2015K1 [2015-05-21]  
#'	Job openings and vacancies, private sector by industry Nace Rev 1.1, Quarter 2001K2 - 2008K4 [2009-02-17]  
#'	Job openings and vacancies, private sector by industry Nace Rev 2, Quarter 2009K1 - 2015K1 [2015-05-21]  
#'	Job openings and vacancies, private sector by number of employees, Quarter 2001K1 - 2015K1 [2015-05-21]  
#'	Job openings and vacancies, private sector by region and industry Nace Rev 2, Quarter 2009K1 - 2015K1 [2015-05-21]  
#'	Job openings and vacancies, private sector by region NUTS2 and industry Nace Rev 1.1, Quarter 2001K1 - 2008K4 [2009-02-17]  
#'	Job openings and vacancies, private sector by region NUTS2, Quarter 2001K1 - 2015K1 [2015-05-21]  
#'	Recruitment and vacancy rate, private sector by industry Nace rev 1.1, Quarter 2001K2 - 2008K4 [2009-02-17]  
#'	Recruitment and vacancy rate, private sector by industry Nace rev 2, Quarter 2009K1 - 2015K1 [2015-05-21]  
#'	Recruitment and vacancy rate, private sector by number of employees, Quarter 2002K2 - 2015K1 [2015-05-21]  
#'	Recruitment and vacancy rate, private sector by region and industry Nace rev 1.1, Quarter 2003K1 - 2008K4 [2009-02-17]  
#'	Recruitment and vacancy rate, private sector by region and industry Nace rev 2, Quarter 2009K1 - 2015K1 [2015-05-21]  
#'	Recruitment and vacancy rate, private sector by region NUTS2, Quarter 2002K2 - 2015K1 [2015-05-21]  
#'  Labour cost index  
#'	Labour Cost Index, manual workers, private sector (LCI man), NACE Rev. 2. Quarterly 2008K1 - 2018K4 [2019-03-13]  
#'	Labour Cost Index , non-manual workers, private sector (LCI non-man), NACE Rev. 2. Quarterly 2008K1 - 2018K4 [2019-03-13]  
#'	Labour Cost Index, manual workers, private sector (WAG man), NACE Rev. 2. Quarterly 2008k1 - 2018K4 [2019-03-13]  
#'	Labour Cost Index , non-manual workers, private sector (WAG non-man), NACE Rev. 2. Quarterly 2008K1 - 2018K4 [2019-03-13]  
#'  Labour cost index for wage-earners and salaried employees in the private sector (AKI)  
#'  Labour cost index for wage-earners and salaried employees in the private sector (AKI)  
#'	Labour cost index for salaried employees in private sector (AKI), NACE rev. 2. Month 2008M01 - 2019M03 [2019-05-29]  
#'	Labour cost index for wage-earners in private sector (AKI), NACE rev. 2. Month 2008M01 - 2019M03 [2019-05-29]  
#'  Index for direct wages and salaries  
#'	Index for direct salaries for salaried employees in private sector (LÖI), NACE rev. 2. Month 2008M01 - 2019M03 [2019-05-29]  
#'	Index for direct wages for wage-earners in private sector (LÖI), NACE rev. 2. Month 2008M01 - 2019M03 [2019-05-29]  
#'  Old tables, not updated  
#'	Index for direct salaries for salaried employees in private sector, NACE rev. 1.1 [2009-03-02]  
#'	Index for direct wages for wage-earners in mining, quarrying and manufacturing, NACE rev. 1.1 [2009-03-02]  
#'	Index for direct wages for wage-earners in private sector, NACE rev. 1.1 [2009-03-02]  
#'	Labour cost index for salaried employees in private sector, NACE rev. 1.1 [2009-03-02]  
#'	Labour cost index for wage-earners in mining, quarrying and manufacturing, NACE rev. 1.1 [2009-03-02]  
#'	Labour cost index for wage-earners in private sector, NACE rev. 1.1 [2009-03-02]  
#'  Economic indicators  
#'	Labour cost index (AKI), percentage change from corresponding month last year. Month 2009M01 - 2019M03 [2019-05-29]  
#'  Labour Force Surveys (LFS)  
#'  Population by labour status from 1970  
#'	Population aged 15-74 (LFS) by sex, age and labour status. Month 1970M01 - 2019M04 [2019-05-23]  
#'	Population aged 15-74 (LFS) by sex, age and labour status. Quarter 1970K1 - 2019K1 [2019-04-18]  
#'	Population aged 15-74 (LFS) by sex, age and labour status. Year 1970 - 2018 [2019-01-24]  
#'  Employed persons from 1970  
#'	Employed aged 15-74 (LFS), of which at work and of which absent from work the whole week by sex and age. Month 1970M01 - 2019M04 [2019-05-23]  
#'	Employed aged 15-74 (LFS), of which at work and of which absent from work the whole week by sex and age. Quarter 1970K1 - 2019K1 [2019-04-18]  
#'	Employed aged 15-74 (LFS), of which at work and of which absent from work the whole week by sex and age. Year 1970 - 2018 [2019-01-24]  
#'	Employed persons aged 15-74 (LFS) by degree of attachment to the labour market, sex and age. Month 1970M01 - 2019M04 [2019-05-23]  
#'	Employed persons aged 15-74 (LFS) by degree of attachment to the labour market, sex and age. Quarter 1970K1 - 2019K1 [2019-04-18]  
#'	Employed persons aged 15-74 (LFS) by degree of attachment to the labour market, sex and age. Year 1970 - 2018 [2019-01-24]  
#'	Employed persons aged 15-74 (LFS) by degree of attachment to the labour market, industrial classification NACE Rev. 2.0 and sex. Month 2009M01 - 2019M04 [2019-05-23]  
#'	Employed persons aged 15-74 (LFS) by degree of attachment to the labour market, industrial classification NACE Rev. 2.0 and sex. Quarter 2009K1 - 2019K1 [2019-04-18]  
#'	Employed persons aged 15-74 (LFS) by degree of attachment to the labour market, industrial classification NACE Rev. 2.0 and sex. Year 2009 - 2018 [2019-01-24]  
#'	Employed persons aged 15-74 (LFS) by attachment to the labour market, occupation SSYK 2012 and sex. Month 2015M01 - 2019M04 [2019-05-23]  
#'	Employed persons aged 15-74 (LFS) by attachment to the labour market, occupation SSYK 2012 and sex. Quarter 2015K1 - 2019K1 [2019-04-18]  
#'	Employed persons aged 15-74 (LFS) by attachment to the labour market, occupation SSYK 2012 and sex. Year 2015 - 2018 [2019-01-24]  
#'	Employed who study aged 15-74 (LFS) by full-time/part-time studies, sex and age. Month 2005M04 - 2019M04 [2019-05-23]  
#'	Employed who study aged 15-74 (LFS) by full-time/part-time studies, sex and age. Quarter 2005K2 - 2019K1 [2019-04-18]  
#'	Employed who study aged 15-74 (LFS) by full-time/part-time studies, sex and age. Year 2005 - 2018 [2019-01-24]  
#'	Employed who study aged 15-74 (LFS) - average number of hours worked/study hours per week and number of employed by sex and age. Month 2005M04 - 2019M04 [2019-05-23]  
#'	Employed who study aged 15-74 (LFS) - average number of hours worked/study hours per week and number of employed by sex and age. Quarter 2005K2 - 2019K1 [2019-04-18]  
#'	Employed who study aged 15-74 (LFS) - average number of hours worked/study hours per week and number of employed by sex and age. Year 2005 - 2018 [2019-01-24]  
#'	Employed aged 15-74 (LFS) by degree of attachment to the labour market, occupation SSYK 96 and sex. Month 2005M04 - 2015M12 [2016-01-28]  
#'	Employed aged 15-74 (LFS) by degree of attachment to the labour market, occupation SSYK 96 and sex. Quarter 2005K2 - 2015K4 [2016-01-28]  
#'	Employed aged 15-74 (LFS) by degree of attachment to the labour market, occupation SSYK 96 and sex. Year 2005 - 2015 [2016-01-28]  
#'	Employed persons aged 15-74 (LFS) by degree of attachment to the labour market, industrial classification NACE Rev. 1.1 and sex. Month 2005M04 - 2008M12 [2009-01-22]  
#'	Employed persons aged 15-74 (LFS) by degree of attachment to the labour market, industrial classification NACE Rev. 1.1 and sex. Quarter 2005K2 - 2008K4 [2009-01-22]  
#'	Employed persons aged 15-74 (LFS) by degree of attachment to the labour market, industrial classification NACE Rev. 1.1 and sex. Year 2005 - 2008 [2009-01-29]  
#'  Employees from 2005  
#'	Employees, total/permanent employees aged 15-74 (LFS) by sector, sex and age. Month 2005M04 - 2019M04 [2019-05-23]  
#'	Employees, total/permanent employees aged 15-74 (LFS) by sector, sex and age. Quarter 2005K2 - 2019K1 [2019-04-18]  
#'	Employees, total/permanent employees aged 15-74 (LFS) by sector, sex and age. Year 2005 - 2018 [2019-01-24]  
#'	Employees aged 15-74 (LFS) by type of employment, trade union membership and sex. Month 2005M04 - 2019M04 [2019-05-23]  
#'	Employees aged 15-74 (LFS) by type of employment, trade union membership and sex. Quarter 2005K2 - 2019K1 [2019-04-18]  
#'	Employees aged 15-74 (LFS) by type of employment, trade union membership and sex. Year 2005 - 2018 [2019-01-24]  
#'	Overtime in main occupation for employees aged 15-74 (persons and hours) during the reference week (LFS) by type of overtime, sex and age. Month 2005M04 - 2019M04 [2019-05-23]  
#'	Overtime in main occupation for employees aged 15-74 (persons and hours) during the reference week (LFS) by type of overtime, sex and age. Quarter 2005K2 - 2019K1 [2019-04-18]  
#'	Overtime in main occupation for employees aged 15-74 (persons and hours) during the reference week (LFS) by type of overtime, sex and age. Year 2005 - 2018 [2019-01-24]  
#'  Hours worked from 2005  
#'	Number of hours actually worked per week for persons aged 15-74 (LFS) by degree of attachment to the labour market, sector and sex. Month 2005M04 - 2019M04 [2019-05-23]  
#'	Number of hours actually worked per week for persons aged 15-74 (LFS) by degree of attachment to the labour market, by sector and sex. Quarter 2005K2 - 2019K1 [2019-04-18]  
#'	Number of hours actually worked per week for persons aged 15-74 (LFS) by degree of attachment to the labour market, by sector and sex. Year 2005 - 2018 [2019-01-24]  
#'	Average number of hours (actually) worked per week for employed persons aged 15-74 (LFS) by degree of attachment to the labour market, sex and age. Month 2005M04 - 2019M04 [2019-05-23]  
#'	Average number of hours (actually) worked per week for employed persons aged 15-74 (LFS) by degree of attachment to the labour market, sex and age. Quarter 2005K2 - 2019K1 [2019-04-18]  
#'	Average number of hours (actually) worked per week for employed persons aged 15-74 (LFS) by degree of attachment to the labour market, sex and age. Year 2005 - 2018 [2019-01-24]  
#'	Employed persons aged 15-74 (LFS) by working hours (by agreement) per week, sex and age. Month 2005M04 - 2019M04 [2019-05-23]  
#'	Employed persons aged 15-74 (LFS) by working hours (by agreement) per week, sex and age. Quarter 2005K2 - 2019K1 [2019-04-18]  
#'	Employed persons aged 15-74 (LFS) by working hours (by agreement) per week, sex and age. Year 2005 - 2018 [2019-01-24]  
#'	Average working hours (by agreement) per week for employed persons aged 15-74 (LFS) by sex and age. Month 2005M04 - 2019M04 [2019-05-23]  
#'	Average working hours (by agreement) per week for employed persons aged 15-74 (LFS) by sex and age. Quarter 2005K2 - 2019K1 [2019-04-18]  
#'	Average working hours (by agreement) per week for employed persons aged 15-74 (LFS) by sex and age. Year 2005 - 2018 [2019-01-24]  
#'  Hours worked by industrial classification (NACE Rev. 2) from 2009  
#'	Number of hours actually worked per week for persons aged 15-74 (LFS) by degree of attachment to the labour market, industrial classification NACE Rev. 2.0 and sex. Month 2009M01 - 2019M04 [2019-05-23]  
#'	Number of hours actually worked per week for persons aged 15-74 (LFS) by degree of attachment to the labour market, industrial classification NACE Rev. 2.0 and sex. Quarter 2009K1 - 2019K1 [2019-04-18]  
#'	Number of hours actually worked per week for persons aged 15-74 (LFS) by degree of attachment to the labour market, industrial classification NACE Rev. 2.0 and sex. Year 2009 - 2018 [2019-01-24]  
#'	Employed persons aged 15-74 (LFS) by number of working hours (by agreement) per week, industrial classification NACE Rev. 2.0 and sex. Month 2009M01 - 2019M04 [2019-05-23]  
#'	Employed persons aged 15-74 (LFS) by number of working hours (by agreement) per week, industrial classification NACE Rev. 2.0 and sex. Quarter 2009K1 - 2019K1 [2019-04-18]  
#'	Employed persons aged 15-74 (LFS) by number of working hours (by agreement) per week, industrial classification NACE Rev. 2.0 and sex. Year 2009 - 2018 [2019-01-24]  
#'	Average number of working hours (by agreement) per week for employed persons aged 15-74 (LFS) by industrial classification NACE Rev. 2.0 and sex. Month 2009M01 - 2019M04 [2019-05-23]  
#'	Average number of working hours (by agreement) per week for employed persons aged 15-74 (LFS) by industrial classification NACE Rev. 2.0 and sex. Quarter 2009K1 - 2019K1 [2019-04-18]  
#'	Average number of working hours (by agreement) per week for employed persons aged 15-74 (LFS) by industrial classification NACE Rev. 2.0 and sex. Year 2009 - 2018 [2019-01-24]  
#'  Hours worked by industrial classification (NACE Rev. 1.1) 2005-2008  
#'	Number of work hours (hours actually worked) per week for persons aged 15-74 (LFS) by degree of attachment to the labour market, industrial classification NACE Rev. 1.1 and sex. Month 2005M04 - 2008M12 [2009-01-22]  
#'	Number of work hours (hours actually worked) per week for persons aged 15-74 (LFS) by degree of attachment to the labour market, industrial classification NACE Rev. 1.1 and sex. Quarter 2005K2 - 2008K4 [2009-01-22]  
#'	Number of work hours (hours actually worked) per week for persons aged 15-74 (LFS) by degree of attachment to the labour market, industrial classification NACE Rev. 1.1 and sex. Year 2005 - 2008 [2009-01-29]  
#'	Employed persons aged 15-74 (LFS) by number of working hours (by agreement) per week, industrial classification NACE Rev. 1.1 and sex. Month 2005M04 - 2008M12 [2009-01-22]  
#'	Employed persons aged 15-74 (LFS) by number of working hours (by agreement) per week, industrial classification NACE Rev. 1.1 and sex.Quarter 2005K2 - 2008K4 [2010-06-08]  
#'	Employed persons aged 15-74 (LFS) by number of working hours (by agreement) per week, industrial classification NACE Rev. 1.1 and sex. Year 2005 - 2008 [2009-01-29]  
#'	Average number of working hours (by agreement) per week for employed persons aged 15-74 (LFS) by industrial classification NACE Rev. 1.1 and sex. Month 2005M04 - 2008M12 [2009-01-22]  
#'	Average number of working hours (by agreement) per week for employed persons aged 15-74 (LFS) by industrial classification NACE Rev. 1.1 and sex. Quarter 2005K2 - 2008K4 [2009-01-22]  
#'	Average number of working hours (by agreement) per week for employed persons aged 15-74 (LFS) by industrial classification NACE Rev. 1.1 and sex. Year 2005 - 2008 [2009-01-29]  
#'  Persons absent from work from 2005  
#'	Persons absent from work aged 15-74 (LFS)/hours of absence during the reference week by reason for absence, sex and age. Month 2005M04 - 2019M04 [2019-05-23]  
#'	Persons absent from work aged 15-74 (LFS)/hours of absence during the reference week by reason for absence, sex and age. Quarter 2005K2 - 2019K1 [2019-04-18]  
#'	Persons absent from work aged 15-74 (LFS)/hours of absence during the reference week by reason for absence, sex and age. Year 2005 - 2018 [2019-01-24]  
#'	Absent employed persons and employees aged 15-74 (number and portion) during the reference week (the whole week and part of the week) in main job (LFS) by sex and age. Month 2005M04 - 2019M04 [2019-05-23]  
#'	Absent employed persons and employees aged 15-74 (number and portion) during the reference week (the whole week and part of the week) in main job (LFS) by sex and age. Quarter 2005K2 - 2019K1 [2019-04-18]  
#'	Absent employed persons and employees aged 15-74 (number and portion) during the reference week (the whole week and part of the week) in main job (LFS) by sex and age. Year 2005 - 2018 [2019-01-24]  
#'	Absence from main job in the reference week (LFS) among employees aged 15-74 (persons and hours) by sex and age. Month 2005M04 - 2019M04 [2019-05-23]  
#'	Absence from main job in the reference week (LFS) among employees aged 15-74 (persons and hours) by sex and age. Quarter 2005K2 - 2019K1 [2019-04-18]  
#'	Absence from main job in the reference week (LFS) among employees aged 15-74 (persons and hours) by sex and age. Year 2005 - 2018 [2019-01-24]  
#'  Unemployed persons from 2005  
#'	Unemployed persons aged 15-74 (LFS) by duration of unemployment, sex and age. Month 2005M04 - 2019M04 [2019-05-23]  
#'	Unemployed persons aged 15-74 (LFS) by duration of unemployment, sex and age. Quarter 2005K2 - 2019K1 [2019-04-18]  
#'	Unemployed persons aged 15-74 (LFS) by duration of unemployment, sex and age. Year 2005 - 2018 [2019-01-24]  
#'	Unemployed persons and of which full-time students aged 15-74 (LFS) by sex and age. Month 2005M04 - 2019M04 [2019-05-23]  
#'	Unemployed persons and of which full-time students aged 15-74 (LFS) by sex and age. Quarter 2005K2 - 2019K1 [2019-04-18]  
#'	Unemployed persons and of which full-time students aged 15-74 (LFS) by sex and age. Year 2005 - 2018 [2019-01-24]  
#'	Average duration of unemployment and number of unemployed persons aged 15-74 (LFS) by sex and age. Month 2005M04 - 2019M04 [2019-05-23]  
#'	Average duration of unemployment and number of unemployed persons aged 15-74 (LFS) by sex and age. Quarter 2005K2 - 2019K1 [2019-04-18]  
#'	Average duration of unemployment and number of unemployed persons aged 15-74 (LFS) by sex and age. Year 2005 - 2018 [2019-01-24]  
#'  Persons not in the labour force from 2005  
#'	Not in the labour force, number of persons aged 15-74 (LFS) by reason, sex and age. Month 2005M04 - 2019M04 [2019-05-23]  
#'	Not in the labour force, number of persons aged 15-74 (LFS) by reason, sex and age. Quarter 2005K2 - 2019K1 [2019-04-18]  
#'	Not in the labour force, number of persons aged 15-74 (LFS) by reason, sex and age. Year 2005 - 2018 [2019-01-24]  
#'	Persons not in the labour force aged 15-74 (LFS) by wanting to work/being available for work, sex and age. Month 2005M04 - 2019M04 [2019-05-23]  
#'	Persons not in the labour force aged 15-74 (LFS) by wanting to work/being available for work, sex and age. Quarter 2005K2 - 2019K1 [2019-04-18]  
#'	Persons not in the labour force aged 15-74 (LFS) by wanting to work/being available for work, sex and age. Year 2005 - 2018 [2019-01-24]  
#'  Labour supply not utilized as from 2005  
#'	Underutilized labour supply, number of persons aged 15-74 and hours (LFS) for unemployed, not fully employed and persons potentially looking for a job by sex and age. Month 2005M04 - 2019M04 [2019-05-23]  
#'	Underutilized labour supply, number of persons aged 15-74 and hours (LFS) for unemployed, not fully employed and persons potentially looking for a job by sex and age. Quarter 2005K2 - 2019K1 [2019-04-18]  
#'	Underutilized labour supply, number of persons aged 15-74 and hours (LFS) for unemployed, not fully employed and persons potentially looking for a job by sex and age. Year 2005 - 2018 [2019-01-24]  
#'  Regional data from 2005  
#'	Population aged 15-74 (LFS) by region, labour status and sex. Quarter 2005K2 - 2019K1 [2019-04-18]  
#'	Population aged 15-74 (LFS) by region, labour status and sex. year 2005 - 2018 [2019-01-24]  
#'	Average working hours (by agreement) per week for employed persons aged 15-74 (LFS) by region and sex. Quarter 2005K2 - 2019K1 [2019-04-18]  
#'	Average working hours (by agreement) per week for employed persons aged 15-74 (LFS) by region and sex. Year 2005 - 2018 [2019-01-24]  
#'	Employed persons aged 15-74 (LFS) by region, degree of attachment to the labour market and sex. Quarter 2005K2 - 2019K1 [2019-04-18]  
#'	Employed persons aged 15-74 (LFS) by region, degree of attachment to the labour market and sex. Year 2005 - 2018 [2019-01-24]  
#'  Level and field of Education as from 2005  
#'	Unemployed persons aged 15-74 (LFS) by duration of unemployment, level of education and sex. Year 2005 - 2018 [2019-01-24]  
#'	Population aged 15-74 (LFS) by labour status, field of education (SUN 2000) and sex. Year 2005 - 2018 [2019-01-24]  
#'	Population aged 15-74 (LFS) by labour status, level of education and sex. Year 2005 - 2018 [2019-01-24]  
#'	Average number of working hours (by agreement) per week for employed persons aged 15-74 (LFS) by level of education and sex. Year 2005 - 2018 [2019-01-24]  
#'	Average duration of unemployment and number of unemployed persons aged 15-74 (LFS) by level of education and sex. Year 2005 - 2018 [2019-01-24]  
#'	Employed persons aged 15-74 (LFS) by degree of attachment to the labour market, level of education and sex. Year 2005 - 2018 [2019-01-24]  
#'  Civil status and children as from 2005  
#'	Population aged 15-74 (LFS) by labour force status, sex, marital status and whether there are children living at home or not. Month 2005M04 - 2019M04 [2019-05-23]  
#'	Population aged 15-74 (LFS) by labour force status, sex, marital status and whether there are children living at home or not. Quarter 2005K2 - 2019K1 [2019-04-18]  
#'	Population aged 15-74 (LFS) by labour force status, sex, marital status and whether there are children living at home or not. Year 2005 - 2018 [2019-01-24]  
#'	Average of actual hours worked for persons aged 15-74 (LFS) with children living at home by marital status, sex and age of the youngest child. Month 2005M04 - 2019M04 [2019-05-23]  
#'	Average of actual hours worked for persons aged 15-74 (LFS) with children living at home by marital status, sex and age of the youngest child. Quarter 2005K2 - 2019K1 [2019-04-18]  
#'	Average of actual hours worked for persons aged 15-74 (LFS) with children living at home by marital status, sex and age of the youngest child. Year 2005 - 2018 [2019-01-24]  
#'	Employed perons aged 15-74 (LFS) by average of actual hours worked, hours of absence and overtime hours per week and share of full-time workers (main job) and also by sex, marital status and whether there are children living at home or not. Month 2005M04 - 2019M04 [2019-05-23]  
#'	Employed perons aged 15-74 (LFS) by average of actual hours worked, hours of absence and overtime hours per week and share of full-time workers (main job) and also by sex, marital status and whether there are children living at home or not. Quarter 2005K2 - 2019K1 [2019-04-18]  
#'	Employed perons aged 15-74 (LFS) by average of actual hours worked, hours of absence and overtime hours per week and share of full-time workers (main job) and also by sex, marital status and whether there are children living at home or not. Year 2005 - 2018 [2019-01-24]  
#'	Employment rate for persons aged 15-74 (LFS) with children living at home by marital status, sex and age of the youngest child. Month 2005M04 - 2019M04 [2019-05-23]  
#'	Employment rate for persons aged 15-74 (LFS) with children living at home by marital status, sex and age of the youngest child. Quarter 2005K2 - 2019K1 [2019-04-18]  
#'	Employment rate for persons aged 15-74 (LFS) with children living at home by marital status, sex and age of the youngest child. Year 2005 - 2018 [2019-01-24]  
#'  Born in Sweden and Foreign-born persons as from 2005  
#'	Unemployed aged 15-74 (LFS) by duration of unemployment (27+ weeks) and by born in Sweden/foreign born, sex and age. Month 2005M04 - 2019M04 [2019-05-23]  
#'	Unemployed aged 15-74 (LFS) by duration of unemployment (27+ weeks) and by born in Sweden/foreign born, sex and age. Quarter 2005K2 - 2019K1 [2019-04-18]  
#'	Unemployed aged 15-74 (LFS) by duration of unemployment (27+ weeks) and by born in Sweden/foreign born, sex and age. Year 2005 - 2018 [2019-01-24]  
#'	Population aged 15-74 (LFS) by labour status, born in Sweden/foreign born, sex and age. Month 2005M04 - 2019M04 [2019-05-23]  
#'	Population aged 15-74 (LFS) by labour status, born in Sweden/foreign born, sex and age. Quarter 2005K2 - 2019K1 [2019-04-18]  
#'	Population aged 15-74 (LFS) by labour status, born in Sweden/foreign born, sex and age. Year 2005 - 2018 [2019-01-24]  
#'	Population aged 15-74 (LFS) by labour status, born in Sweden/foreign born, level of education and sex. Year 2005 - 2018 [2019-01-24]  
#'	Employed persons aged 15-74 (number and share) absent from main job (LFS) in the reference week (part of and the whole week) by born in Sweden/foreign born, sex and age. Month 2005M04 - 2019M04 [2019-05-23]  
#'	Employed persons aged 15-74 (number and share) absent from main job (LFS) in the reference week (part of and the whole week) by born in Sweden/foreign born, sex and age. Quarter 2005K2 - 2019K1 [2019-04-18]  
#'	Employed persons aged 15-74 (number and share) absent from main job (LFS) in the reference week (part of and the whole week) by born in Sweden/foreign born, sex and age. Year 2005 - 2018 [2019-01-24]  
#'	Average number of working hours (by agreement) per week for employed persons and number of employed persons aged 15-74 (LFS) by born in Sweden/foreign born, sex and age. Month 2005M04 - 2019M04 [2019-05-23]  
#'	Average number of working hours (by agreement) per week for employed persons and number of employed persons aged 15-74 (LFS) by born in Sweden/foreign born, sex and age. Quarter 2005K2 - 2019K1 [2019-04-18]  
#'	Average number of working hours (by agreement) per week for employed persons and number of employed persons aged 15-74 (LFS) by born in Sweden/foreign born, sex and age. Year 2005 - 2018 [2019-01-24]  
#'	Average duration of unemployment and number of unemployed persons aged 15-74 (LFS) and by born in Sweden/foreign born, sex and age. Month 2005M04 - 2019M04 [2019-05-23]  
#'	Average duration of unemployment and number of unemployed persons aged 15-74 (LFS) and by born in Sweden/foreign born, sex and age. Quarter 2005K2 - 2019K1 [2019-04-18]  
#'	Average duration of unemployment and number of unemployed persons aged 15-74 (LFS) and by born in Sweden/foreign born, sex and age. Year 2005 - 2018 [2019-01-24]  
#'	Labour supply not utilised, number of persons aged 15-74 and hours (LFS) for unemployed, underemployed and persons available to work but not seeking by born in Sweden/foreign born, sex and age. Month 2005M04 - 2019M04 [2019-05-23]  
#'	Labour supply not utilised, number of persons aged 15-74 and hours (LFS) for unemployed, underemployed and persons available to work but not seeking by born in Sweden/foreign born, sex and age. Quarter 2005K2 - 2019K1 [2019-04-18]  
#'	Labour supply not utilised, number of persons aged 15-74 and hours (LFS) for unemployed, underemployed and persons available to work but not seeking by born in Sweden/foreign born, sex and age. Year 2005 - 2018 [2019-01-24]  
#'	Employed perons aged 15-74 (LFS) by average of actual hours worked, hours of absence and overtime hours per week and share of full-time workers (main job) and also by born in Sweden/foreign born, sex and age. Month 2005M04 - 2019M04 [2019-05-23]  
#'	Employed perons aged 15-74 (LFS) by average of actual hours worked, hours of absence and overtime hours per week and share of full-time workers (main job) and also by born in Sweden/foreign born, sex and age. Quarter 2005K2 - 2019K1 [2019-04-18]  
#'	Employed perons aged 15-74 (LFS) by average of actual hours worked, hours of absence and overtime hours per week and share of full-time workers (main job) and also by born in Sweden/foreign born, sex and age. Year 2005 - 2018 [2019-01-24]  
#'	Employed persons aged 15-74 (LFS) by status in employment and by born in Sweden/foreign born, sex and age. Month 2005M04 - 2019M04 [2019-05-23]  
#'	Employed persons aged 15-74 (LFS) by status in employment and by born in Sweden/foreign born, sex and age. Quarter 2005K2 - 2019K1 [2019-04-18]  
#'	Employed persons aged 15-74 (LFS) by status in employment and by born in Sweden/foreign born, sex and age. Year 2005 - 2018 [2019-01-24]  
#'  Young people not in employment and not in any education and training (NEET) from 2007  
#'	Young people aged 15-34 (LFS) not in employment and not in any education and training (NEET) by sex and age. Quarter 2007K1 - 2019K1 [2019-04-18]  
#'	Young people aged 15-34 (LFS) not in employment and not in any education and training (NEET) by sex and age. Year 2007 - 2018 [2019-01-24]  
#'	Young people aged 15-24 (LFS) not in employment and not in any education and training (NEET) by sex and Swedish/foreign born. Quarter 2007K1 - 2019K1 [2019-04-18]  
#'	Young people aged 15-24 (LFS) not in employment and not in any education and training (NEET) by sex and Swedish/foreign born. Year 2007 - 2018 [2019-01-24]  
#'	Young people aged 15-24 (LFS) not in employment and not in any education and training (NEET) by sex and previous experience of work, Quarter 2007K1 - 2019K1 [2019-04-18]  
#'	Young people aged 15-24 (LFS) not in employment and not in any education and training (NEET) by sex and previous experience of work, Year 2007 - 2018 [2019-01-24]  
#'	Young people aged 15-24 (LFS) not in employment and not in any education and training (NEET) by sex and labour status. Quarter 2007K1 - 2019K1 [2019-04-18]  
#'	Young people aged 15-24 (LFS) not in employment and not in any education and training (NEET) by sex and labour status. Year 2007 - 2018 [2019-01-24]  
#'  Economic indicators  
#'	Labour Force Surveys (LFS), population aged 15-74, by economic indicator. Non seasonally adjusted estimates. Month 2013M01 - 2019M04 [2019-05-23]  
#'	Labour Force Surveys (LFS), population aged 15-74, by economic indicator. Smoothed and seasonally adjusted estimates. Month 2013M01 - 2019M04 [2019-05-23]  
#'	Labour Force Surveys (LFS), population aged 15-74, by economic indicator. Seasonally adjusted estimates. Month 2013M01 - 2019M04 [2019-05-23]  
#'  Labour statistics based on administrative sources  
#'  Population 16+ years, 2004-  
#'	Population 16+ years (RAMS) by region, employment, age and sex. Year 2004 - 2017 [2018-11-29]  
#'	Population 16+ yeras (RAMS) at national level, employment, country of citizenship, age and sex. Year 2004 - 2017 [2018-11-29]  
#'	Population 16+ years (RAMS) at national level, employment, country of birth, latest year of immigration, age and sex. Year 2004 - 2017 [2018-11-29]  
#'  Population 16+ years, 1993-2003  
#'	Population 16+ years (RAMS) by region, employment, age and sex. 1993-2003 [2005-03-21]  
#'	Population 16+ years (RAMS) at national level, employment, country of citizenship, age and sex. 1993-2003 [2005-03-21]  
#'	Population 16+ years (RAMS) at national level, employment, country of birth, latest year of immigration, age and sex. 1993-2003 [2011-03-30]  
#'  Gainfully employed commuters 16- years of age, 1993-2003 / 2004-  
#'	Gainfully employed commuters 16+ years , leaving or coming into the municipality, by municipality and sex 2004 - 2017 [2018-11-29]  
#'	Gainfully employed commuters, 16+ years , leaving or coming into the municipality, by municipality and sex. Year 1993-2003 [2007-03-26]  
#'	Gainfully employed commuters 16+ years , leaving or coming into the county, by county and sex 2004 - 2017 [2018-11-29]  
#'	Gainfully employed commuters, 16+ years , leaving or coming into the county, by county and sex. Year 1993-2003 1993 - 2003 [2007-03-26]  
#'	Gainfully employed commuters by municipality 16+ years by municipality of residence, municipality of work and sex. Year 2004 - 2017 [2018-11-29]  
#'	Gainfully employed commuters by municipality 16+ years by municipality of residence, municipality of work and sex. Year 1993-2003 [2012-02-06]  
#'	Gainfully employed commuters by county 16+ years by county of residence, county of work and sex. Year 2004 - 2017 [2018-11-29]  
#'	Gainfully employed commuters by county 16+ years by county of residence, county of work and sex. Year 1993-2003 [2012-02-06]  
#'  Gainfully employed 16+ years by region of residence, 2004-  
#'	Gainfully employed 16+ years by region of residence (RAMS), by region, industry SNI2007 and sex. Year 2008 - 2017 [2018-11-30]  
#'	Gainfully employed 16+ years by region of residence (RAMS), by region, industry SNI2002 and sex. Year 2004 - 2007 [2008-12-16]  
#'	Gainfully employed 16+ years by region of residence (RAMS), by region, sector, age and sex. Year 2004 - 2017 [2018-11-29]  
#'	Gainfully employed 16+ years by region of residence (RAMS), at national level, industry SNI2007, country of birth and sex. Year 2008 - 2017 [2018-11-29]  
#'	Gainfully employed 16+ years by region of residence (RAMS), at national level, industry SNI2002, country of birth and sex. Year 2004 - 2007 [2013-03-04]  
#'	Gainfully employed 16+ years by region of residence (RAMS), at national level, industry SNI2007, country of citizenship and sex. Year 2008 - 2017 [2018-11-29]  
#'	Gainfully employed 16+ years by region of residence (RAMS), at national level, industry SNI2002, country of citizenship and sex. Year 2004 - 2007 [2013-03-04]  
#'	Gainfully employed 16+ years by region of residence (RAMS), at national level, industry SNI2007, status in employment, age and sex. Year 2008 - 2017 [2018-11-30]  
#'	Gainfully employed 16+ years by region of residence (RAMS), at national level, industry SNI2002, status in employment, age and sex. Year 2004 - 2007 [2013-03-04]  
#'	Gainful employment rate 20-64 years by region, born in Sweden and foreign born and sex. Year 2004 - 2017 [2018-11-29]  
#'  Gainfully employed 16+ years by region of residence, 1993-2003  
#'	Gainfully employed 16+ years by region of residence (RAMS), by region, industry SNI92 and sex. 1993-2003 [2005-03-21]  
#'	Gainfully employed 16+ years by region of residence (RAMS), by region, sector, age and sex. 1993-2003 [2005-03-21]  
#'	Gainfully employed 16+ years by region of residence (RAMS), at national level, industry SNI92, country of birth and sex. 1993-2003 [2005-03-21]  
#'	Gainfully employed 16+ years by region of residence (RAMS), at national level, industry SNI92, country of citizenship and sex. 1993-2003 [2005-03-21]  
#'	Gainfully employed 16+ years by region of residence (RAMS), at national level, industry SNI92, status in employment and sex. 1993-2003 [2006-02-08]  
#'	Gainful employment rate 20-64 years by region, born in Sweden and foreign born and sex. Year 1993 - 2003 [2016-02-10]  
#'  Gainfully employed 16+ years by region of work, 2004-  
#'	Gainfully employed 16+ years by region of work (RAMS), by region, industry SNI 2007 and sex. Year 2008 - 2017 [2018-11-29]  
#'	Gainfully employed 16+ years by region of work (RAMS), by region, industry SNI 2002 and sex. Year 2004 - 2007 [2008-12-16]  
#'	Gainfully employed 16+ years by region of work (RAMS), by region, sector, age and sex. Year 2004 - 2017 [2018-11-29]  
#'	Gainfully employed 16+ years by region of work (RAMS), at national level, industry SNI 2007, level of education, size class of the local unit and sex. Year 2008 - 2017 [2018-11-29]  
#'	Gainfully employed 16+ years by region of work (RAMS), at national level, industry SNI 2002, level of education, size class of the local unit and sex. Year 2004 - 2007 [2013-03-04]  
#'	Gainfully employed 16+ years by region of work (RAMS), at national level, industry SNI 2007, status in employment, age and sex. Year 2008 - 2017 [2018-11-29]  
#'	Gainfully employed 16+ years by region of work (RAMS), at national level, industry SNI 2002, status in employment, age and sex. Year 2004 - 2007 [2013-03-04]  
#'	Gainfully employed 16+ years by region of work (RAMS), at national level, industry SNI 2007, status in employment, level of education and sex. Year 2008 - 2017 [2018-11-29]  
#'	Gainfully employed 16+ years by region of work (RAMS), at national level, industry SNI 2002 , status in employment, level of education and sex. Year 2004 - 2007 [2013-03-04]  
#'  Gainfully employed 16+ years by region of work, 1993-2003  
#'	Gainfully employed 16+ years by region of work (RAMS), by region, industry SNI92 and sex. 1993-2003 [2005-03-21]  
#'	Gainfully employed 16+ years by region of work (RAMS), by region, sector, age and sex. 1993-2003 [2005-03-21]  
#'	Gainfully employed 16+ years by region of work (RAMS), at national level, industry SNI92, level of education, size class of the local unit and sex. 1993-2003 [2017-02-10]  
#'	Gainfully employed 16+ years by region of work (RAMS), at national level, industry SNI92 (43 study domains), status in employment, age and sex. Year 1993 - 2003 [2005-03-21]  
#'	Gainfully employed 16+ years by region of work (RAMS), at national level, industry SNI92 ,status in employment, level of education and sex. 1993-2003 [2005-03-21]  
#'  Labour Statistics Based on Administrative Sources 1985-1993  
#'	Population 16+ years (ÅRSYS) by region, employment, age and sex. Year 1985 - 1993 [1997-01-01]  
#'  Salary structures, whole economy  
#'  Salaries by occupation (SSYK, 4-digit level)  
#'	Average basic salary, monthly salary and women´s salary as a percentage of men´s salary by sector, occupation (SSYK 2012), sex and educational level (SUN) . Year 2014 - 2017 [2018-06-19]  
#'	Average basic salary, monthly salary and women´s salary as a percentage of men´s salary by sector, occupation (SSYK), sex and educational level (SUN). Year 2005 - 2013 [2014-06-17]  
#'	Average basic salary, monthly salary and women´s salary as a percentage of men´s salary by region, sector, occupation (SSYK 2012) and sex . Year 2014 - 2017 [2018-06-19]  
#'	Average basic salary, monthly salary and women´s salary as a percentage of men´s salary by region, sector, occupation (SSYK) and sex. Year 2005 - 2013 [2014-06-17]  
#'	Average basic salary, monthly salary and women´s salary as a percentage of men´s salary by sector, occupation (SSYK 2012) , sex and age . Year 2014 - 2017 [2018-06-19]  
#'	Average basic salary, monthly salary and women´s salary as a percentage of men´s salary by sector, occupation (SSYK) , sex and age. Year 2005 - 2013 [2014-06-17]  
#'	Average salary and salary dispersion by sector, occupation (SSYK 2012) and sex . Year 2014 - 2017 [2018-06-19]  
#'	Average salary and salary dispersion by sector, occupation (SSYK) and sex. Year 2005 - 2013 [2014-06-17]  
#'	Women´s salary as a percentage of men´s salary, standard weighted and unweighted, by occupation (SSYK 2012) . Year 2014 - 2017 [2018-06-19]  
#'	Women´s salary as a percentage of men´s salary, standard weighted and unweighted, by occupation (SSYK). Year 2005 - 2013 [2014-06-17]  
#'  Salaries by occupational group (SSYK, 3-digit level)  
#'	Average basic salary, monthly salary and women´s salary as a percentage of men´s salary by sector, occupational group (SSYK 2012), sex and educational level (SUN). Year 2014 - 2017 [2018-06-19]  
#'	Average basic salary, monthly salary and women´s salary as a percentage of men´s salary by sector, occupational group (SSYK), sex and educational level (SUN). Year 2003 - 2013 [2014-06-17]  
#'	Average basic salary, monthly salary and women´s salary as a percentage of men´s salary by region, sector, occupational group (SSYK 2012) and sex . Year 2014 - 2017 [2018-06-19]  
#'	Average basic salary, monthly salary and women´s salary as a percentage of men´s salary by region, sector, occupational group (SSYK) and sex. Year 2003 - 2013 [2014-06-17]  
#'	Average basic salary, monthly salary and women´s salary as a percentage of men´s salary by sector, occupational group (SSYK 2012) , sex and age. Year 2014 - 2017 [2018-06-19]  
#'	Average basic salary, monthly salary and women´s salary as a percentage of men´s salary by sector, occupational group (SSYK) , sex and age. Year 2004 - 2013 [2014-06-17]  
#'	Average salary and salary dispersion by sector, occupational group (SSYK 2012) and sex. Year 2014 - 2017 [2018-06-19]  
#'	Average salary and salary dispersion by sector, occupational group (SSYK) and sex. Year 2004 - 2013 [2014-06-17]  
#'	Women´s salary as a percentage of men´s salary, standard weighted and unweighted, by occupational group (SSYK 2012). Year 2014 - 2017 [2018-06-19]  
#'	Women´s salary as a percentage of men´s salary, standard weighted and unweighted, by occupational group (SSYK). Year 2004 - 2013 [2014-06-17]  
#'  Salaries by educational group and industry  
#'	Average basic salary, monthly salary and women´s salary as a percentage of men´s salary by SNI 2007 and sex . Year 2014 - 2017 [2018-06-21]  
#'  Short term business statistics on sick pay  
#'  Number of sickdays and sickperiods  
#'	Number of sick days and sick periods distributed by sex and industrial classification SNI2007 (production-and service industry). Quarter 2009K1 - 2018K4 [2019-03-08]  
#'	Number of sickperiods and sickdays per employee distributed by sex and sector. Quaterly 2015K2 - 2018K4 [2019-03-08]  
#'  Older tables, not updated  
#'	Number of sickperiods and sickdays per employee distributed by sex and sector (old Institutional Sectors). Quaterly 2006K1 - 2015K1 [2015-06-02]  
#'	Number of sickdays and sickperiods distributed by sex and industrial classification SNI2002 (production-and service industry). Quaterly 2006K1 - 2009K1 [2009-06-02]  
#'  Short-term employment  
#'  Employed population  
#'	Number of employees by region and sector. Quarter 2015K2 - 2019K1 [2019-05-23]  
#'	Number of employees, business sector, by industry SNI2007. Quarter 2015K2 - 2019K1 [2019-05-23]  
#'  Sick Leave  
#'	Proportion of absent employees due to sick leave by branch of industry in business sector. Quarter 2015K2 - 2019K1 [2019-05-23]  
#'	Proportion of absent employees due to sick leave by sector and sex. Quarter 2015K2 - 2019K1 [2019-05-23]  
#'  Old tables, not updated  
#'	Number of employees, private sector, index 2000Q1=100, by industry SNI2007. Quarter 2000K1 - 2015K1 [2015-05-21]  
#'	Number of employees, private sector by industry. Quarter 1993K1 - 2008K4 [2009-02-17]  
#'  Short-term statistics, salaries in the county councils  
#'	Average monthly salary in the county councils (KLL). Month 1999M01 - 2019M03 [2019-05-29]  
#'  Short-term statistics, salaries in the municipalities  
#'	Average monthly salary in the municipalities (KLK). Month 1999M01 - 2019M03 [2019-05-29]  
#'  Short-term statistics, wages and salaries in the private sector  
#'  Manual workers: Hourly earnings by industry  
#'	Average hourly earnings of manual workers in the private sector (KLP) by industrial classification SNI2007 and overtime pay. Month 2008M01 - 2019M03 [2019-05-29]  
#'  Non-manual workers: Monthly salary by industry  
#'	Average monthly salary of non-manual workers in the private sector (KLP) by industrial classification SNI2007 and variable supplements. Month 2008M01 - 2019M03 [2019-05-29]  
#'  Older tables, not updated  
#'	Average hourly earnings of manual workers in the private sector (KLP) by industrial classification SNI2002 and overtime pay (no update). Month 1996M01 - 2008M12 [2010-03-01]  
#'	Average monthly salary of non-manual workers in the private sector (KLP) by industrial classification SNI2002 and variable supplements (no update). Month 1996M01 - 2008M12 [2010-03-01]  
#'  Short-term statistics; salaries in the governmental sector  
#'  Salaries and employees  
#'	Average monthly salary for monthly paid and number of employees in the governmental sector (KLS) by authority, sex and full-/ part-time. Month 2010M01 - 2019M03 [2019-05-29]  
#'	Average monthly salary for monthly paid and number of employees in the governmental sector (KLS) by industry sector (SNI2007), sex and full-/part-time. Month 2010M01 - 2019M03 [2019-05-29]  
#'  Older tables, not updated  
#'	Average monthly salary and number of employees in the governmental sector (KLS) by authority, sex and full-/part-time. Month 2000M01 - 2011M12 [2013-03-01]  
#'	Average monthly salary and number of employees in the governmental sector (KLS) by industry sector (SNI2007), sex and full-/part-time. Month 2009M01 - 2011M12 [2013-03-01]  
#'	Average monthly salary and number of employees in the governmental sector (KLS) by industry sector (SNI92), sex and full-/part-time. Month 2000M01 - 2008M12 [2010-03-01]  
#'  The Swedish Occupational Register  
#'  The population 16+ years  
#'	The population 16+ years by region, employment status, age and sex. Year 2001 - 2017 [2019-03-07]  
#'  Employed population 16-64 years at national level (SSYK3)  
#'	Employees 16-64 years at national level by occupation (3-digit SSYK 2012), industry SNI2007 (aggr. level), age and sex. Year 2014 - 2017 [2019-03-07]  
#'	Employees 16-64 years at national level by occupation (3-digit SSYK 2012, sector, age and sex. Year 2014 - 2017 [2019-03-07]  
#'	Employees 16-64 years at national level by occupation (3-digit SSYK 2012), level of education SUN 2000, age and sex. Year 2014 - 2017 [2019-03-07]  
#'	Employees 16-64 years at national level by occupation (3-digit SSYK 2012), orientation of education (SUN 2000), age and sex. Year 2014 - 2017 [2019-03-07]  
#'	Employees 16-64 years at national level by occupation (3-digit SSYK 2012), region of birth and sex. Year 2014 - 2017 [2019-03-07]  
#'	Employees 16-64 years at national level by occupation (3-digit SSYK 2012), size class of the local unit, industry SNI2007 (aggr. level), and sex. Year 2014 - 2017 [2019-03-07]  
#'	Employees 16-64 years at national level by occupation (3-digit SSYK 96), industry SNI 2002 (aggr. level), age and sex. Year 2001 - 2010 [2012-03-07]  
#'	Employees 16-64 years at national level by occupation (3-digit SSYK 96), industry SNI2007 (aggr. level), age and sex. Year 2008 - 2013 [2015-02-12]  
#'	Employees 16-64 years at national level by occupation (3-digit SSYK 96), sector, age and sex. Year 2001 - 2013 [2015-02-12]  
#'	Employees 16-64 years at national level by occupation (3-digit SSYK 96), level of education SUN 2000, age and sex. Year 2001 - 2013 [2015-02-12]  
#'	Employees 16-64 years at national level by occupation (3-digit SSYK 96), orientation of education (SUN 2000), age and sex. Year 2001 - 2013 [2015-02-12]  
#'	Employees 16-64 years at national level by occupation (3-digit SSYK 96), size class of the local unit, industry SNI 2002 (aggr. level), and sex. Year 2007 - 2010 [2012-03-07]  
#'	Employees 16-64 years at national level by occupation (3-digit SSYK 96), size class of the local unit, industry SNI2007 (aggr. level), and sex. Year 2008 - 2013 [2015-02-12]  
#'  Employed population 16-64 years at national level (SSYK4)  
#'	Employees 16-64 years at national level by occupation (4-digit SSYK 2012), sector and sex. Year 2014 - 2017 [2019-03-07]  
#'	Employees 16-64 years at national level by occupation (4-digit SSYK 2012), level of education SUN 2000, age and sex. Year 2014 - 2017 [2019-03-07]  
#'	Employees 16-64 years at national level by occupation (4-digit SSYK 2012), orientation of education (SUN 2000), age and sex. Year 2014 - 2017 [2019-03-07]  
#'	Employees 16-64 years at national level by occupation (4-digit SSYK 2012), region of birth and sex. Year 2014 - 2017 [2019-03-07]  
#'	Employees 16-64 years at national level by occupation (4-digit SSYK 2012), industry SNI2007 (aggr. level), age and sex. Year 2014 - 2017 [2019-03-07]  
#'	Employees 16-64 years at national level by occupation (4-digit SSYK 96), sector and sex. Year 2005 - 2013 [2015-02-12]  
#'	Employees 16-64 years at national level by occupation (4-digit SSYK 96), level of education SUN 2000, age and sex. Year 2005 - 2013 [2015-02-12]  
#'	Employees 16-64 years at national level by occupation (4-digit SSYK 96), orientation of education (SUN 2000), age and sex. Year 2005 - 2013 [2015-02-12]  
#'	Employees 16-64 years at national level by occupation (4-digit SSYK 96), industry SNI 2002 (aggr. level), age and sex. Year 2005 - 2010 [2012-03-07]  
#'	Employees 16-64 years at national level by occupation (4-digit SSYK 96), industry SNI2007 (aggr. level), age and sex. Year 2008 - 2013 [2015-02-12]  
#'  Employed population 16-64 years by region of residence (SSYK3)  
#'	Employees 16-64 years by region of residence, occupation (3-digit SSYK 2012), age and sex. Year 2014 - 2017 [2019-03-07]  
#'	Employees 16-64 years by region of residence, occupation (3-digit SSYK 96), age and sex. Year 2001 - 2013 [2015-02-12]  
#'  Employed population 16-64 years by region of work (SSYK3)  
#'	Employees 16-64 years by region of work, occupation (3-digit SSYK 2012), industry SNI2007 (aggr. level) and sex. Year 2014 - 2017 [2019-03-07]  
#'	Employees 16-64 years by region of work, occupation (3-digit SSYK 2012), age and sex. Year 2014 - 2017 [2019-03-07]  
#'	Employees 16-64 years by region of work, occupation (3-digit SSYK 2012) and sex. Year 2014 - 2017 [2019-03-07]  
#'	Employees 16-64 years by region of work, occupation (3-digit SSYK 2012), sector and sex. Year 2014 - 2017 [2019-03-07]  
#'	Employees 16-64 years by region of work, occupation (3-digit SSYK 96), industry SNI 2002 (aggr. level) and sex. Year 2001 - 2010 [2012-03-07]  
#'	Employees 16-64 years by region of work, occupation (3-digit SSYK 96), industry SNI2007 (aggr. level) and sex. Year 2008 - 2013 [2015-02-12]  
#'	Employees 16-64 years by region of work, occupation (3-digit SSYK 96), age and sex. Year 2001 - 2013 [2015-02-12]  
#'	Employees 16-64 years by region of work, occupation (3-digit SSYK 96) and sex. Year 2001 - 2013 [2015-02-12]  
#'	Employees 16-64 years by region of work, occupation (3-digit SSYK 96), sector and sex. Year 2001 - 2013 [2015-02-12]  
#'  Employed population 16-64 years by region of work (SSYK4)  
#'	Employees 16-64 years by region of work, occupation (4-digit SSYK 2012) and sex. Year 2014 - 2017 [2019-03-07]  
#'  Self employed 16+ years  
#'	Self employed by region of work (NUTS2), occupation (3-digit SSYK 2012), age and sex. Year 2014 - 2017 [2019-03-07]  
#'	Self employed by national level, occupation (4-digit SSYK 2012), region of birth and sex. Year 2014 - 2017 [2019-03-07]  
#'  Wage and salary structures and employment in county councils  
#'  Salaries  
#'	Average monthly salary in the county councils by activity, agreed rate of employment and sex. Year 2000 - 2018 [2019-05-17]  
#'	Average monthly salary in th county councils by activity, extent of employment and sex. Year 2000 - 2018 [2019-05-17]  
#'	Average monthly salary and salary dispersion in SEK in the county councils by occupation and sex. Year 2014 - 2018 [2019-05-17]  
#'	Average monthly salary and basic salary in the county councils by occupation, age and sex. Year 2014 - 2018 [2019-05-17]  
#'	Average monthly salary and basic salary in the county councils by region (NUTS1, 2008), occupation and sex . Year 2014 - 2018 [2019-05-17]  
#'  Time serie: Number of employees in the public sector  
#'	Employees in the governmental-, municipal-, county council sector and the Church of Sweden by sector and sex. Year 1995 - 2018 [2019-05-17]  
#'  Old tables, not updated  
#'	Average monthly salary in the county councils by county council, occupation and sex. Year 2000 - 2008 [2009-05-26]  
#'	Average monthly salary and salary dispersion in SEK in the county councils by occupation and sex. Year 2000 - 2013 [2014-05-21]  
#'	Average monthly salary and basic salary in the county councils by region (NUTS1, 2008), occupation and sex. Year 2009 - 2013 [2014-05-21]  
#'	Average monthly salary and basic salary in the county councils by occupation, age and sex. Year 2000 - 2013 [2014-05-21]  
#'	Average monthly salary and basic salary in the county councils by region (NUTS2), occupation and sex. Year 2000 - 2008 [2009-05-26]  
#'	Average monthly salary in the county councils by occupation and sex. Year 1999 [2000-10-20]  
#'	Employees in the county councils by monthly salary interval in SEK, sex and age. Year 2000 - 2010 [2011-05-24]  
#'	Employees in the county councils by montly salar interval, occupation and sex. Year 2000 - 2010 [2011-05-24]  
#'	Average basic salary in the county councils by occupation, sex and age. Year 1999 [2009-05-26]  
#'	Employees in the county council-, municipal- , governmental, - other public sectors and the Church of Sweden by sector and sex. Year 1992 - 2013 [2014-05-21]  
#'	Employees, new appointments and resignations/retirements in the county councils by agreed rate of employment, age and sex. Year 2000 - 2013 [2014-05-21]  
#'	Employees in the county councils by activity, agreed rate of employment and sex. Year 2000 - 2013 [2014-05-21]  
#'	Employees in the county councils by activity, extent of employment and sex. Year 2000 - 2013 [2014-05-21]  
#'	Employees in the county councils by county council, activity and sex. Year 2000 - 2013 [2014-05-21]  
#'	Employees in the county councils by county council, extent of employment and sex. Year 2000 - 2013 [2014-05-21]  
#'  Wage and salary structures and employment in the central government sector  
#'  Salaries  
#'	Average monthly salary, salary dispersion etc. in the central government sector by field of education, level of education and sex. Year 2001 - 2018 [2019-05-17]  
#'	Average monthly salary in the central government sector by county, occupation and sex. Year 2014 - 2018 [2019-05-17]  
#'	Average monthly salary, salary dispersion etc. in the central government sector by occupation and sex. Year 2014 - 2018 [2019-05-17]  
#'	Average monthly salary and basic salary in the central government sector by occupation, age and sex. Year 2014 - 2018 [2019-05-17]  
#'	Average monthly salary and basic salary in the central government sector by region (NUTS2), occupation and sex. Year 2014 - 2018 [2019-05-17]  
#'  Time serie: Number of employees in the public sector  
#'	Employees in the governmental-, municipal-, county council sector and the Church of Sweden by sector and sex. Year 1995 - 2018 [2019-05-17]  
#'  Old tables, not updated  
#'	Number of employees, new appointments and resignations/retirements in the central government sector by extent of employment, age and sex. Year 2000 - 2013 [2014-05-21]  
#'	Average monthly salary in the central government sector by county, occupation and sex. Old table, not updated. Year 2000 - 2003 [2004-05-13]  
#'	Average monthly salary, salary dispersion etc. in the central government sector by occupation and sex. Old table, not updated. Year 2000 - 2003 [2004-05-13]  
#'	Average monthly salary and basic salary in the central government sector by occupation, age and sex. Old table, not updated. Year 2000 - 2003 [2004-05-13]  
#'	Average monthly salary and basic salary in the central government sector by region (NUTS2), occupation and sex. Old table, not updated. Year 2000 - 2003 [2004-05-13]  
#'	Employees in the central government sector by region, extent of employment and sex. Year 2000 - 2013 [2014-05-21]  
#'	Employees in the central government sector by monthly salary interval in SEK, sex and age. Year 2000 - 2011 [2012-05-22]  
#'	Average monthly salary in the central government sector by county, occupation and sex. Year 2004 - 2013 [2014-05-21]  
#'	Employees in the public sector (governmental, county council-, municipal-, other public sectors and the Church of Sweden) by sector and sex. Year 1992 - 2013 [2014-05-21]  
#'	Average monthly salary, salary dispersion etc. in the central government sector by occupation and sex. Year 2004 - 2013 [2014-05-21]  
#'	Average monthly salary and basic salary in the central government sector by occupation, age and sex. Year 2004 - 2013 [2014-05-21]  
#'	Average monthly salary and basic salary in the central government sector by region (NUTS2), occupation and sex. Year 2004 - 2013 [2014-05-21]  
#'  Wage and salary structures and employment in the primary municipalities  
#'  Salaries  
#'	Average monthly salary in the primary municipalities by municipality and sex. Year 2007 - 2018 [2019-05-17]  
#'	Average monthly salary, salary dispersion etc. in the primary municipalities by field of education, level of education and sex. Year 2001 - 2018 [2019-05-17]  
#'	Average monthly salary in the primary municipalities by activity, extent of employment and sex. Year 2000 - 2018 [2019-05-17]  
#'	Average monthly salary in the primary municipalities by activity, agreed rate of employment and sex. Year 2012 - 2018 [2019-05-17]  
#'	Average monthly salary in the primary municipalities by county, occupation and sex. Year 2014 - 2018 [2019-05-17]  
#'	Average monthly salary, salary dispersion etc. in the primary municipalities by occupation and sex. Year 2014 - 2018 [2019-05-17]  
#'	Average monthly salary and basic salary in the primary municipalities by occupation, age and sex. Year 2014 - 2018 [2019-05-17]  
#'	Average monthly salary and basic salary in the primary municipalities by region (NUTS2), occupation and sex. Year 2014 - 2018 [2019-05-17]  
#'  Time serie: Number of employees in the public sector  
#'	Employees in the governmental-, municipal-, county council sector and the Church of Sweden by sector and sex. Year 1995 - 2018 [2019-05-17]  
#'  Old tables, not updated  
#'	Average monthly salary in the primary municipalities by county, occupation and sex. Year 2000 - 2013 [2014-05-21]  
#'	Average monthly salary and basic salary in the primary municipalities by region (NUTS2), occupation and sex. Year 2000 - 2013 [2014-05-21]  
#'	Average monthly salary and basic salary in the primary municipalities by occupation, age and sex. Year 2000 - 2013 [2014-05-21]  
#'	Average monthly salary, salary dispersion etc. in the primary municipalities by occupation and sex. Year 2000 - 2013 [2014-05-21]  
#'	Average monthly salary in the primary municipalities by occupation and sex. Year 1999 [2000-10-20]  
#'	Average monthly salary in the primary municipalities by activity, type of appointment and sex. Year 2000 - 2011 [2012-05-22]  
#'	Employees in the governmental-, municipal-, county council-, other public sector and the Church of Sweden by sector and sex. Year 1992 - 2013 [2014-05-21]  
#'	Employees in the primary municipalities by activity, agreed rate of employment and sex. Year 2012 - 2013 [2014-05-21]  
#'	Employees in the primary municipalities by activity, extent of employment and sex. Year 2000 - 2013 [2014-05-21]  
#'	Employees in the primary municipalities by activity, type of appointment and sex. Year 2000 - 2011 [2012-05-22]  
#'	Employees in the primary municipalities by monthly salary interval, occupation (major group) and sex. Year 2000 - 2010 [2011-05-24]  
#'	Employees in the primary municipalities by monthly salary interval, sex and age. Year 2000 - 2010 [2011-05-24]  
#'	Employees in the primary municipalities by region, activity and sex. Year 2000 - 2013 [2014-05-21]  
#'	Employees in the primary municipalities by region, extent of employment and sex. Year 2000 - 2003 [2004-05-14]  
#'	Number of employees, new appointments and resignations/retirements in the primary municipalities by extent of employment, age and sex. Year 2000 - 2013 [2014-05-21]  
#'  Wage and salary structures in the private sector  
#'  Manual workers: Hourly wages  
#'	Average hourly pay, manual workers private sector (SLP) by region, occupational group (SSYK) and sex. Year 2014 - 2018 [2019-05-17]  
#'	Average hourly pay, manual workers private sector (SLP) by, occupational group (SSYK) age and sex. Year 2014 - 2018 [2019-05-17]  
#'	Average hourly pay, manual workers private sector (SLP) by region, occupation (4-digit SSYK) and sex. Year 2014 - 2018 [2019-05-17]  
#'	Average hourly pay, manual workers private sector (SLP) by occupation (4-digit SSYK) and sex. Year 2014 - 2018 [2019-05-17]  
#'	Average hourly pay, manual workers private sector (SLP) by pay element by industry SNI 2007, type of working time and sex. Year 2008 - 2018 [2019-05-17]  
#'	Average hourly pay and confidence interval, manual workers private sector (SLP) by region, occupational group (SSYK) and sex. Year 2008 - 2018 [2019-05-17]  
#'  Manual workers: Wages dispersion  
#'	Average hourly pay, pay dispersion etc., manual workers private sector (SLP) by educational orientation SUN 2000 and sex. Year 2001 - 2018 [2019-05-17]  
#'	Average hourly pay, pay dispersion etc., manual workers private sector (SLP) by industry SNI 2007, educational level (SUN96) and sex. Year 2008 - 2018 [2019-05-17]  
#'	Average hourly pay, pay dispersion etc., manual workers private sector (SLP) by region, occupational group (SSYK2012) and sex. Year 2014 - 2018 [2019-05-17]  
#'	Average hourly pay, pay dispersion etc., Average hourly pay, manual workers private sector (SLP) by, occupational group (SSYK 201 ) and sex. Year 2014 - 2018 [2019-05-17]  
#'  Manual workers: Other time series  
#'	Average hourly pay and real pay development in the mining and manufacturing industries, manual workers private sector (SLP) by sex. Year 1952 - 2013 [2014-05-21]  
#'  Manual workers: Old tables, not updated  
#'	Average hourly pay, manual workers private sector (SLP) by region, occupational group (SSYK) and sex. Year 2000 - 2013 [2014-05-21]  
#'	Average hourly pay, pay dispersion etc., Average hourly pay, manual workers private sector (SLP) by, occupational group (SSYK) and sex. Year 2000 - 2013 [2014-05-21]  
#'	Average hourly pay, manual workers private sector (SLP) by, occupational group (SSYK) age and sex. Year 2000 - 2013 [2014-05-21]  
#'	Average hourly pay, pay dispersion etc., manual workers private sector (SLP) by region, occupational group (SSYK) and sex. Year 2007 - 2013 [2014-05-21]  
#'	Average hourly pay, manual workers private sector (SLP) by region, occupation (4-digit SSYK) and sex. Year 2007 - 2013 [2014-05-21]  
#'	Average hourly pay, manual workers private sector (SLP) by occupation (4-digit SSYK) and sex. Year 2007 - 2013 [2014-05-21]  
#'  Non-manual workers: Monthly salary  
#'	Average monthly pay, non-manual workers private sector (SLP) by industry SNI 2007, type of employment form, occupational area (SSYK 2012) and sex. Year 2014 - 2018 [2019-05-17]  
#'	Average monthly pay, non-manual workers private sector (SLP) by region, occupation (4-digit SSYK 2012 ) and sex. Year 2014 - 2018 [2019-05-17]  
#'	Average monthly pay, non-manual workers private sector (SLP) by region, occupational group (SSYK 2012) and sex. Year 2014 - 2018 [2019-05-17]  
#'	Average monthly pay, non-manual workers private sector (SLP) by occupation (4-digit SSYK 2012), age and sex. Year 2014 - 2018 [2019-05-17]  
#'	Average monthly pay, non-manual workers private sector (SLP) by occupational group (SSYK 2012) age and sex. Year 2014 - 2018 [2019-05-17]  
#'  Non-manual workers: Salary dispersion  
#'	Average monthly pay, pay dispersion etc., non-manual workers private sector (SLP) by educational orientation SUN 2000 and sex. Year 2001 - 2018 [2019-05-17]  
#'	Average monthly pay, pay dispersion etc., non-manual workers private sector (SLP) by industry SNI 2007, educational level (SUN96) and sex. Year 2008 - 2018 [2019-05-17]  
#'	Average monthly pay, pay dispersion etc., non-manual workers private sector (SLP) by occupation (4-digit SSYK 2012) and sex. Year 2014 - 2018 [2019-05-17]  
#'	Average monthly pay, pay dispersion etc., non-manual workers private sector (SLP) by occupational group (SSYK 2012) and sex. Year 2014 - 2018 [2019-05-17]  
#'  Non-manual workers: Other time series  
#'	Average monthly pay, (total pay) and real pay development in the mining and manufacturing industries, full-time employees, non-manual workers private sector (SLP) by sex. Year 1947 - 2013 [2014-05-21]  
#'  Non-manual workers: Old tables, not updated  
#'	Average monthly pay, non-manual workers private sector (SLP) by region, occupational group (SSYK) and sex. Year 2000 - 2013 [2014-05-21]  
#'	Average monthly pay, pay dispersion etc., non-manual workers private sector (SLP) by occupational group (SSYK) and sex. Year 2000 - 2013 [2014-05-21]  
#'	Average monthly pay, non-manual workers private sector (SLP) by occupational group (SSYK) age and sex. Year 2000 - 2013 [2014-05-21]  
#'	Average monthly pay, pay dispersion etc., non-manual workers private sector (SLP) by occupation (4-digit SSYK) and sex. Year 2007 - 2013 [2014-05-21]  
#'	Average monthly pay, non-manual workers private sector (SLP) by region, occupation (4-digit SSYK) and sex. Year 2007 - 2013 [2014-05-21]  
#'	Average monthly pay, non-manual workers private sector (SLP) by occupation (4-digit SSYK), age and sex. Year 2007 - 2013 [2014-05-21]  
#'	Average monthly pay, non-manual workers private sector (SLP) by industry SNI 2007, type of employment form, occupational area (SSYK) and sex. Year 2008 - 2013 [2014-05-21]  
    
#'References  
#'http://www.statistikdatabasen.scb.se/pxweb/en  
#'http://www.statistikdatabasen.scb.se/pxweb/sv  
#'https://imagemagick.org/  
    
#+   