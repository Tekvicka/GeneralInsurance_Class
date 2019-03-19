library(dplyr)
dt_KPI <- read.csv("./Data/lesson2_KPI.csv") %>% filter_all(all_vars(!is.na(.)))

ui <- fluidPage(
  titlePanel("Scatter Plot with colour"),

  sidebarLayout(      
    sidebarPanel(
      selectInput("select", "Colouring Var",
                  choices=colnames(dt_KPI)[1:5]),
      hr()
  ),
  mainPanel(
    plotOutput(outputId = "plot")  
  )
)
)
