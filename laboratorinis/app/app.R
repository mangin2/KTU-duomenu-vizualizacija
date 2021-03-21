library(shiny)
library(shinydashboard)
library(tidyverse)
library(scales)

ui <- dashboardPage(skin = "black",
                    dashboardHeader(title = "Sodros duomenys"),
                    
                    dashboardSidebar( 
                      
                      selectizeInput(inputId = "imones_pavadinimas", label = "Imones pavadinimas", choices = NULL), #selected = NULL),
                      
                      sidebarMenu(
                        menuItem("Pagr. informacija", icon = icon("chart-line"), tabName = "Pagrindine_informacija"),
                        menuItem("Lentele", icon = icon("list-ul"), tabName = "Lentele")
                      )
                      
                    ),
                    
                    dashboardBody(
                      
                      tabItems(
                        tabItem(tabName = "Pagrindine_informacija", h2("Pagrindine informacija"),
                                fluidRow(
                                  valueBoxOutput("mokesciai", width = 6),
                                  valueBoxOutput("kodas", width = 6),
                                  box(title = "Apdraustu darbuotoju skaicius", width = 12, solidHeader = TRUE, collapsible = TRUE, plotOutput("plot", height = 350)),
                                  box(title = "Atlyginimo dinamika", width = 12, solidHeader = TRUE, collapsible = TRUE, plotOutput("plot1", height = 350))
                                )
                        ),
                        
                        tabItem(tabName = "Lentele", h2("Lentele"),
                                box(title = "Visi duomenys", width = 12, solidHeader = TRUE, collapsible = TRUE, tableOutput("table"))
                        )
                      )
                
                    )
)

server <- function(input, output, session) {
  
  #Duomenu nuskaitymas
  duomenys <- read_csv("https://raw.githubusercontent.com/mangin2/KTU-duomenu-vizualizacija/main/laboratorinis/data/lab_sodra.csv")
  #Duomenu fitravimas pagal veiklos koda ir menesio stulpelio sukurimas
  duomenys <- duomenys %>% filter(ecoActCode == 682000) %>%  mutate("month1" = as.numeric(substr(month, 5, 6)))
  
  updateSelectizeInput(session, "imones_pavadinimas", choices = duomenys$name, server = TRUE)
  
  #Duomeys atrenkami pagal imones pavadinima, ismetamas menesio stulpelis, pervadinami kiti stulpeliai
  output$table <- renderTable(
    duomenys %>% filter(name == input$imones_pavadinimas) %>% select(-"month1") %>% rename(Kodas = code, Pavadinimas = name, Savivaldybe = municipality, Men = month, EkoVeikPav = ecoActName, EkoVeikKod = ecoActCode, Atlyginimas = avgWage, Apdr = numInsured, Mok = tax), digits = 0
  )
  
  #Apdraustuju skaiciaus kitimo vaizdavimas
  output$plot <- renderPlot(
    duomenys %>% filter(name == input$imones_pavadinimas) %>%
      ggplot(aes(x = month1, y = numInsured))+
      geom_col(fill = alpha("grey82", 0.4))+
      geom_text(aes(label = numInsured), vjust = -0.5)+
      scale_x_continuous(breaks = 1:12)+
      scale_y_continuous(breaks = pretty_breaks())+
      labs(x = 'Menesiai', y = 'Apdraustuju skaicius')+
      theme_classic()
  )
  
  #Atlyginimu kitimo vaizdavimas
  output$plot1 <- renderPlot(
    duomenys %>% filter(name == input$imones_pavadinimas) %>%
      ggplot(aes(x = month1, y = avgWage))+
      geom_line(color = "mediumpurple2", size = 1.1)+
      geom_text(aes(label = avgWage), vjust = -0.5)+
      scale_x_continuous(breaks = 1:12)+
      scale_y_continuous(breaks = pretty_breaks())+
      labs(x = 'Menesiai', y = 'Atlyginimas')+
      theme_classic()
    
  )
  
  #Lenteles, rodancios sumoketus mokescius, kurimas
  output$mokesciai <- renderValueBox({
      a <- duomenys %>%
      filter(name == input$imones_pavadinimas) %>%
      summarise(suma = sum(tax, na.rm = TRUE)) %>%
      round() %>%
      prettyNum(big.mark = ",")
      b <- "Info nera"
      valueBox(
        value = ifelse(a == 0, b, paste(a, "eur")),
        subtitle = "Sumoketi mokesciai",
        color = "black"
      )
  })
  
  #Lenteles, rodancios koda, kurimas
  output$kodas <- renderValueBox({
      duomenys %>%
      filter(name == input$imones_pavadinimas) %>%
      select(code) %>% unique() %>% 
      valueBox(
        subtitle = "Kodas",
        color = "teal"
      )
  })
  
}

shinyApp(ui, server)
