library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "League of legends worlds"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Jogadores", tabName = "dashboard1", icon = icon("chart-line")),
      menuItem("Personagens", tabName = "dashboard2", icon = icon("chart-bar")),
      menuItem("Partidas", tabName = "dashboard3", icon = icon("chart-area"))
    )
  ),
  dashboardBody(
    tabItems(
      # Conteúdo para a primeira aba 
      tabItem(
        tabName = "dashboard1",
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
                     selectInput("Player", "Selecione o jogador", choices =  NULL),
                     selectInput("Season", "Filtrar por temporada", choices = NULL)
                 )
          )
        )
      ),
      # Conteúdo para a segunda aba 
      tabItem(
        tabName = "dashboard2",
        fluidRow(
          box(width = 12, height = 2,
              selectInput("Season", "Filtrar por temporada", choices = "Todas"),
              selectInput("Lane", "Filtrar por rota", choices = "Todas")
          ),
          column(6,
                 box(width = 12, status = "primary", solidHeader = T,
                     plotOutput("GamesContests")
                     ),
                 box(width = 12, status = "primary", solidHeader = T,
                     plotOutput("WinLose")
                 )
          column(6,
                 box(),
                 box())
          
        )
      ),
      # Conteúdo para a terceira aba 
      tabItem(
        tabName = "dashboard3",
        fluidRow(
          box(width = 12, status = "warning", solidHeader = TRUE,
              title = "Conteúdo do Dashboard 3"
          )
        )
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
      title = "Média de Minions por minuto",
      value = mean(Players_Inputs()$cs.min),
      icon = icon("chess-pawn"),
      color = "yellow",
      
    )
  })
  60*21.91
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
    
    ggplot(df_filtered, aes(x = factor(season), group = 1)) +
      geom_point(aes(y = wins, color = "Win"), size = 3) +
      geom_point(aes(y = loses, color = "Lose"), size = 3) +
      geom_line(aes(y = wins, color = "Win"), size = 1.5) +
      geom_line(aes(y = loses, color = "Lose"), size = 1.5) +
      scale_color_manual(values = c("Win" = "green", "Lose" = "red")) +
      labs(title = "Vitórias e Derrotas por Temporada", x = "Temporada", y = "Quantidade") +
      theme_minimal() +
      scale_x_discrete(breaks = unique(df_filtered$season))
  })
  
  
  
}

shinyApp(ui, server)