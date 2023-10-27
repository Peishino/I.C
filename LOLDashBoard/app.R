library(shiny)
library(shinydashboard)
library(ggplot2)

ui <- dashboardPage(skin = "black",
                    dashboardHeader(title = "League of legends worlds"),
                    dashboardSidebar(
                      selectInput("Player", "Selecione o jogador", choices =  NULL),
                      selectInput("Season", "Filtrar por temporada", choices = NULL),
                      plotOutput("WinRate")
                    ),
                    dashboardBody(
                      fluidRow(
                        box(width = 12, status = "warning", solidHeader = TRUE,
                            title = "Desempenho do jogador por partida",
                            infoBoxOutput("Team", width = 3),
                            infoBoxOutput("KDA", width = 3),
                            infoBoxOutput("CreepScore", width = 3),
                            infoBoxOutput("Damage", width = 3)
                        ),
                        column(9,
                               box(width = 12, height = 211, status = "primary", solidHeader = T,
                                   plotOutput("KDA_Season", height = 190)
                               ),
                               box(width = 12, height = 211, status = "success", solidHeader = T,
                                   plotOutput("Wr_Season", height = 190)
                               )
                        ),
                        column(3,
                               box(width = 12,height = 444,
                                   plotOutput("cs_min"),
                                   plotOutput("ks")
                                   ),
                              
                        )
                      )
                    )
)

server <- function(input, output, session) {
  
  # lendo a base de dados reativa
  PlayersData <- reactive({
    df <- Player
    df <- df[df$event == "Main",]
    return(df)
  })
  
  # criando filtro jogador
  observe({
    updateSelectInput(session, "Player", choices = unique(PlayersData()$player))
  })
  
  # seleção de filtros
  observeEvent(input$Player, {
    base <- PlayersData()
    base <- base %>% filter(player == input$Player)
    updateSelectInput(session, "Season", choices = unique(base$season))
    base <- base %>% filter(season == input$Season)
  })
  
  # base filtrada
  Players_Inputs <- reactive({
    base <- PlayersData()
    base <- base %>% filter(player == input$Player)
    base <- base %>% filter(season == input$Season)
    return(base)
  })
  
  # info boxes
  output$Team <- renderInfoBox({
    infoBox(
      fill = TRUE,
      title = "Time",
      value = Players_Inputs()$team,
      icon = icon("headset"),
      color = "yellow"
    )
  })
  
  output$KDA <- renderInfoBox({
    infoBox(
      fill = TRUE,
      title = "Média de KDA",
      value = mean(Players_Inputs()$kill_death_assist_ratio),
      icon = icon("hand-fist"),
      color = "yellow"
    )
  })
  
  output$CreepScore <- renderInfoBox({
    infoBox(
      fill = TRUE,
      title = "Média de Minions",
      value = mean(Players_Inputs()$creep_score),
      icon = icon("chess-pawn"),
      color = "yellow",
      
    )
  })
  
  output$Damage <- renderInfoBox({
    infoBox(
      fill = TRUE,
      title = "Média de Dano",
      value = mean(Players_Inputs()$damage),
      icon = icon("wand-sparkles"),
      color = "yellow"
    )
  })
  
  
  output$KDA_Season <- renderPlot({
    df <- PlayersData()
    df_filtered <- df %>% filter(player == input$Player)
    
    df_filtered$season <- factor(df_filtered$season, levels = unique(df_filtered$season))
    
    ggplot(df_filtered, aes(x = season, y = kill_death_assist_ratio)) +
      geom_bar(stat = "identity", fill = "lightblue", size = 1) +
      labs(title = "Média de KDA por Temporada", x = "Temporada", y = "KDA Média") +
      theme_minimal() 
  })
  
  output$Wr_Season <- renderPlot({
    df <- PlayersData()
    df_filtered <- df %>% filter(player == input$Player)
    
    ggplot(df_filtered, aes(x = season)) +
      geom_area(aes(y = wins, fill = "Win"), position = "identity", alpha = 0.5) +
      geom_area(aes(y = loses, fill = "Lose"), position = "identity", alpha = 0.5) +
      geom_line(aes(y = wins, color = "Win"), size = 1.5) +
      geom_line(aes(y = loses, color = "Lose"), size = 1.5) +
      scale_fill_manual(values = c("Win" = "green", "Lose" = "red")) +
      scale_color_manual(values = c("Win" = "green", "Lose" = "red")) +
      labs(title = "Vitórias e Derrotas por Temporada", x = "Temporada", y = "Quantidade") +
      theme_minimal()
  })
  
  
  
}

shinyApp(ui, server)