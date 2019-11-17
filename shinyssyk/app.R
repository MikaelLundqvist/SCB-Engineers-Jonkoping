#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button in the RStudio IDE or running the command runApp("app.R").
#

library(shiny)
library (tidyverse) 
library (broom) 
library (gganimate)  
library (car)
library (swemaps) # devtools::install_github('reinholdsson/swemaps')
library (ggpubr)
library (Cubist)
library(PerformanceAnalytics)
library(factoextra)
library(clValid)

nuts <- read_csv ("nuts.csv", col_types = cols(), locale = readr::locale (encoding = "UTF-8"), na = c("..", "NA"))

options(tibble.print_max = Inf)

nuts %>%
  knitr::kable(
  booktabs = TRUE,
  caption = 'NUTS (Nomenclature des Unités Territoriales Statistiques) is the EU\'s hierarchical regional division.') 

map_ln_n <- map_ln %>%
  mutate(lnkod_n = as.numeric(lnkod)) 
  
relative_dev <- function (x){  
  return (x / x[1])  
}  

tot_dev <- function (x){  
  scales::percent ((tail(x, 1) / x[1]) - 1)  
}  

perc_women <- function(x){  
  ifelse (length(x) == 2, scales::percent (x[2] / (x[1] + x[2])), NA)
}  

perc_sal <- function(x){  
  ifelse (length(x) == 2, scales::percent (x[2] / x[1]), NA)
}  

readfile <- function (file1){  
  read_csv (file1, col_types = cols(), locale = readr::locale (encoding = "latin1"), na = c("..", "NA")) %>%  
    gather (starts_with("19"), starts_with("20"), key = "year", value = salary) %>%  
	drop_na() %>%  
    mutate (year2 = parse_number (year)) %>%  
    mutate (heading = file1) %>%  
    mutate (relsalary = relative_dev (salary))  
}

tb <- readfile("000000D2_1.csv") %>%
  rowwise() %>%
  mutate(age2 = unlist(lapply(strsplit(substr(age, 1, 5), "-"), strtoi))[1]) %>% 
  rowwise() %>%
  mutate(age3 = unlist(lapply(strsplit(substr(age, 1, 5), "-"), strtoi))[2]) %>%
  mutate(age4 = (age3 + age2) / 2)
 
tb2 <- readfile("000000CD_13.csv")
 
summary_table = 0
anova_table = 0
tb2 <- tb2 %>%
  group_by (`region`, year, `occuptional  (SSYK 2012)`) %>%
  mutate (perc_women = as.numeric (sub ("%", "", perc_women (salary)))) %>%
  group_by(region, year) %>%
  mutate(sum_pop = sum(salary)) 
 
tb4 <- readfile("000000CD_13.csv")
summary_table = 0
anova_table = 0
tb4 <- tb4 %>%
  group_by (`region`, year, `occuptional  (SSYK 2012)`) %>%
  mutate (perc_women = as.numeric (sub ("%", "", perc_women (salary)))) %>%
  group_by(region, year) %>%
  mutate(sum_pop = sum(salary)) 
tb4 <- readfile("000000CG_3.csv") %>%
  inner_join(tb4, by = c("region", "year", "sex", "occuptional  (SSYK 2012)"))   
 
tb6 <- readfile("000000CD_13.csv")
summary_table = 0
anova_table = 0
tb6 <- tb6 %>%
  group_by (`region`, year, `occuptional  (SSYK 2012)`) %>%
  mutate (perc_women = as.numeric (sub ("%", "", perc_women (salary)))) %>%
  group_by(region, year) %>%
  mutate(sum_pop = sum(salary))
tb6 <- readfile("000000CG_3.csv") %>%
  inner_join(tb6, by = c("region", "year", "sex", "occuptional  (SSYK 2012)"))   
  
edu <- readfile("000000CV_5.csv")
edu$`level of education` <- as.factor(edu$`level of education`)
edu$`level of education` <- factor(edu$`level of education`, levels(edu$`level of education`)[c(2, 3, 4, 6, 7, 5, 1)])

age <- readfile("000000NK_2.csv") 

sector <- readfile("000000CD.csv")




# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("SCB Engineers Jonkoping"),
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          selectInput(
            "select",
            label = h3("SSYK"),
            choices = list(SSYK = tb$`occuptional  (SSYK 2012)`)
          ),
		  p("For more information please visit:"),
		  a(href = "https://mikaellundqvist.github.io/SCB-Engineers-Jonkoping/", "https://mikaellundqvist.github.io/SCB-Engineers-Jonkoping/")
        ),
        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("distPlot5"),  
          plotOutput("distPlot"),
          tableOutput('table1'),
          tableOutput('table2'),
          plotOutput("distPlot6"),    
          plotOutput("distPlot2"),
          tableOutput('table3'),
          tableOutput('table4'), 
          plotOutput("distPlot7"),    
          plotOutput("distPlot3"),
          tableOutput('table5'),
          tableOutput('table6'),  
          plotOutput("distPlot8"),	 
          plotOutput("distPlot4"),    
          tableOutput('table7'),
          tableOutput('table8'),
          plotOutput("distPlot9"),	 
          plotOutput("distPlot10"),
          plotOutput("distPlot11"),
          plotOutput("distPlot12"),
          plotOutput("distPlot13"),
	      p("For more information please visit:"),
		  a(href = "https://mikaellundqvist.github.io/SCB-Engineers-Jonkoping/", "https://mikaellundqvist.github.io/SCB-Engineers-Jonkoping/")			  
        )
  
  
    )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
tb <- readfile("000000D2_1.csv") %>%
  rowwise() %>%
  mutate(age2 = unlist(lapply(strsplit(substr(age, 1, 5), "-"), strtoi))[1]) %>% 
  rowwise() %>%
  mutate(age3 = unlist(lapply(strsplit(substr(age, 1, 5), "-"), strtoi))[2]) %>%
  mutate(age4 = (age3 + age2) / 2)
 
    output$distPlot5 <- renderPlot({
      tb1 <- tb %>% filter(`occuptional  (SSYK 2012)` == input$select)
   tb1 %>%
        ggplot(aes(x = age4, y = salary, colour = sex, size = year2)) +
        geom_point() +
        theme(legend.position="bottom") +
        labs(
          x = "Age",
          y = "Salary"
        ) +
        facet_grid(. ~ sex)
    }) 
    output$distPlot <- renderPlot({
      tb1 <- tb %>% filter(`occuptional  (SSYK 2012)` == input$select)
   tb1 %>%
        ggscatter(x = "age4", y = "salary",
        add = "reg.line", conf.int = TRUE,
        cor.coef = TRUE, cor.method = "pearson") +
        labs(
          x = "Age",
          y = "Salary"
        ) +
        facet_grid(. ~ sex)
    })
 
    output$table1 <- renderTable({
      tb1 <- tb %>% filter(`occuptional  (SSYK 2012)` == input$select)
   if (dim(tb1)[1] > 50){
     model <- lm (log(salary) ~ year2 + sex + poly(age4, 3), data = tb1)
     summary(model) %>% tidy()
      } else {
     "Not enough data"
   }
    }) 
 
    output$table2 <- renderTable({
      tb1 <- tb %>% filter(`occuptional  (SSYK 2012)` == input$select)
   if (dim(tb1)[1] > 50){
     model <- lm (log(salary) ~ year2 + sex + poly(age4, 3), data = tb1)
  Anova(model, type=2) %>% tidy()
      } else {
     "Not enough data"
   }
    })     
    output$distPlot6 <- renderPlot({
      tb3 <- tb2 %>% filter(`occuptional  (SSYK 2012)` == input$select)
   tb3 %>%
        ggplot(aes(x = sum_pop, y = perc_women, colour = region, size = year2)) +
        geom_point() +
        theme(legend.position="bottom") +
        labs(
          x = "Population",
          y = "Per cent women"
        )
    }) 
 
    output$distPlot2 <- renderPlot({
      tb3 <- tb2 %>% filter(`occuptional  (SSYK 2012)` == input$select)
   tb3 %>%
        ggscatter(x = "sum_pop", y = "perc_women",
        add = "reg.line", conf.int = TRUE,
        cor.coef = TRUE, cor.method = "pearson") +
        labs(
          x = "Population",
          y = "Per cent women"
        )
    }) 

    output$table3 <- renderTable({
      tb3 <- tb2 %>% filter(`occuptional  (SSYK 2012)` == input$select)
   if (dim(tb3)[1] > 50){
     model <- lm (log(perc_women) ~ sum_pop + year2 + sex, data = tb3)
     summary(model) %>% tidy()
      } else {
     "Not enough data"
   }
    }) 
 
    output$table4 <- renderTable({
      tb3 <- tb2 %>% filter(`occuptional  (SSYK 2012)` == input$select)
   if (dim(tb3)[1] > 50){
     model <- lm (log(perc_women) ~ sum_pop + year2 + sex, data = tb3)
  Anova(model, type=2) %>% tidy()
      } else {
     "Not enough data"
   }
    })  
 
    output$distPlot7 <- renderPlot({
      tb5 <- tb4 %>% filter(`occuptional  (SSYK 2012)` == input$select)
   tb5 %>%
        ggplot(aes(x = salary.x, y = perc_women, colour = region, size = year2.x)) +
        geom_point() +
        theme(legend.position="bottom") +
        labs(
          x = "Salary",
          y = "Per cent women"
        ) +
        facet_grid(. ~ sex)
    })  
 
    output$distPlot3 <- renderPlot({
      tb5 <- tb4 %>% filter(`occuptional  (SSYK 2012)` == input$select)
   tb5 %>%
        ggscatter(x = "salary.x", y = "perc_women",
        add = "reg.line", conf.int = TRUE,
        cor.coef = TRUE, cor.method = "pearson") +
        labs(
          x = "Salary",
          y = "Per cent women"
        ) +
        facet_grid(. ~ sex)
    })
    output$table5 <- renderTable({
      tb5 <- tb4 %>% filter(`occuptional  (SSYK 2012)` == input$select)
   if (dim(tb5)[1] > 50){
  model <- lm(perc_women ~ log(salary.x) + year2.x + sex, data = tb5)
     summary(model) %>% tidy()
      } else {
     "Not enough data"
   }
    }) 
 
    output$table6 <- renderTable({
      tb5 <- tb4 %>% filter(`occuptional  (SSYK 2012)` == input$select)
   if (dim(tb5)[1] > 50){
  model <- lm(perc_women ~ log(salary.x) + year2.x + sex, data = tb5)
  Anova(model, type=2) %>% tidy()
      } else {
     "Not enough data"
   }
    }) 
 

    output$distPlot8 <- renderPlot({
      tb7 <- tb6 %>% filter(`occuptional  (SSYK 2012)` == input$select)
   tb7 %>%
        ggplot(aes(x = sum_pop, y = salary.x, colour = region, size = year2.x)) +
        geom_point() +
        theme(legend.position="bottom") + 
        labs(
          x = "Population",
          y = "Salary"
        )  +
        facet_grid(. ~ sex)
    })   
 
    output$distPlot4 <- renderPlot({
      tb7 <- tb6 %>% filter(`occuptional  (SSYK 2012)` == input$select)
   tb7 %>%
        ggscatter(x = "sum_pop", y = "salary.x",
        add = "reg.line", conf.int = TRUE,
        cor.coef = TRUE, cor.method = "pearson") +
        labs(
          x = "Population",
          y = "Salary"
        )  +
        facet_grid(. ~ sex)
    })  
	

    output$table7 <- renderTable({
      tb7 <- tb6 %>% filter(`occuptional  (SSYK 2012)` == input$select)
   if (dim(tb7)[1] > 50){
    model <- lm(log(salary.x) ~ sum_pop + year2.x + sex, data = tb7)  
     summary(model) %>% tidy()
      } else {
     "Not enough data"
   }
    }) 
 
    output$table8 <- renderTable({
      tb7 <- tb6 %>% filter(`occuptional  (SSYK 2012)` == input$select)
   if (dim(tb7)[1] > 50){
    model <- lm(log(salary.x) ~ sum_pop + year2.x + sex, data = tb7)  
  Anova(model, type=2) %>% tidy()
      } else {
     "Not enough data"
   } 
    }) 	
	
    output$distPlot9 <- renderPlot({
      tb3 <- tb2 %>% filter(`occuptional  (SSYK 2012)` == input$select) %>% filter(year2 == 2017)
	  nuts <- nuts %>% mutate(NUTS2_code = substr(NUTS2, 1, 4))
	  tb3 <- tb3 %>% mutate(region_code = substr(region, 1, 4))
   tb3 %>%
     left_join(nuts, by = c("region_code" = "NUTS2_code")) %>%
     right_join(map_ln_n, by = c("Länskod" = "lnkod_n")) %>%
     ggplot() +
     geom_polygon(mapping = aes(x = ggplot_long, y = ggplot_lat, group = lnkod, fill = perc_women)) +
       coord_equal() 
    }) 	
	
    output$distPlot10 <- renderPlot({
      tb5 <- tb4 %>% filter(`occuptional  (SSYK 2012)` == input$select) %>% filter(year2.x == 2017)
	  nuts <- nuts %>% mutate(NUTS2_code = substr(NUTS2, 1, 4))
	  tb5 <- tb5 %>% mutate(region_code = substr(region, 1, 4))	  
   tb5 %>%
     left_join(nuts, by = c("region_code" = "NUTS2_code")) %>%
     right_join(map_ln_n, by = c("Länskod" = "lnkod_n")) %>%
     ggplot(mapping = aes(x = ggplot_long, y = ggplot_lat, group = lnkod, fill = salary.x)) +
     geom_polygon() +
       coord_equal() +
	   facet_grid(. ~ sex)
    }) 
	
    output$distPlot11 <- renderPlot({
      edu %>%
        filter(year2 == 2017) %>%  filter(`occuptional  (SSYK 2012)` == input$select) %>%
        ggplot(aes(x = `occuptional  (SSYK 2012)`, y = salary, fill = `level of education`)) +
        geom_col(position = "fill") +
        theme(legend.position="bottom") +
        facet_grid(. ~ sex) 
    }) 	
	
    output$distPlot12 <- renderPlot({
      age %>% filter(year2 == 2017) %>% filter(`occupation (SSYK 2012)` == input$select) %>%
        rowwise() %>% mutate(age2 = unlist(lapply(strsplit(substr(age, 1, 5), "-"), strtoi))[1]) %>% 
        rowwise() %>% mutate(age3 = unlist(lapply(strsplit(substr(age, 1, 5), "-"), strtoi))[2]) %>%
        ggplot() +
          geom_line(aes(x = (age2 + age3) / 2, y = salary, color = region)) +
            theme(legend.position="bottom") +
            facet_grid(. ~ sex)
    }) 	

    output$distPlot13 <- renderPlot({
	  sector %>%
        filter(year2 == 2017) %>% filter(`occuptional  (SSYK 2012)` == input$select) %>%
        mutate(lnkod_n = substr(region, 1,4)) %>%
        ggplot(aes(x = lnkod_n, y = salary, fill = sector)) +
          geom_col(position = "fill") +
          theme(legend.position="bottom") +
          facet_grid(. ~ sex)
    }) 			
	
}

# Run the application
shinyApp(ui = ui, server = server)