
library(ggplot2)
library(dplyr)
library(shiny)

dt_KPI <- read.csv("./Data/lesson2_KPI.csv") %>% filter_all(all_vars(!is.na(.)))

server <- function(input, output){
  output$plot <- renderPlot({
    ggplot(data = dt_KPI, mapping = aes_string(x = "Premium", y = "Expenses", colour = input$select)) +
      geom_point() +
      geom_smooth()
  })
}


###########################################################################################################

# Why categories behave differently? 
##### kategoria - Business:
# cestovne poistenie (Travel) je velmi rizikove preto nadobuda vysoke hodnoty Expenses (aj Premium)
# najstrmsi sklon ma krivka pre poistenie domacnosti (Housing), co moze byt sposobene tym, ze toto poistenie je jedno z najkomplexnejších druhov poistení
##### kategoria - Region:
# z daneho grafu by sme mohli vyvodit ze oblast Alandia je pravdepodobnejsie najvacsia (najrizikovejsia), kedze ma najvyssie Expenses 
##### kategoria - Unit:
# vidime, ze Unit7 pravdepodobne obsahuje najrizikovejsie spolocnosti, kedze ma najvyssie Expenses
