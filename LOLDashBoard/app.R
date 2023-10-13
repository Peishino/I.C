library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)

calculatePlayerAverages <- function(player_data) {
  kills_avg <- mean(player_data$kills)
  deaths_avg <- mean(player_data$deaths)
  assists_avg <- mean(player_data$assists)
  creep_score_avg <- mean(player_data$creep_score)
  damage_avg <- mean(player_data$damage)
  kill_participation_avg <- mean(player_data$kill_participation)
  
  return(c(kills_avg, deaths_avg, assists_avg, creep_score_avg, damage_avg, kill_participation_avg))
}
ui <- fluidPage(
    theme = bslib::bs_theme(bootswatch = "darkly"),

    div(
      class = "container",
      h1("League of Legends Worlds", class = "text-center")
    ),

    fluidPage(
        fluidRow(
          column(6,
                 plotOutput("Teams_plot")
          ),
          column(6,
                 selectInput("Player_filter","Jogador",choices = NULL,multiple = TRUE),
                 plotOutput("Comparative_plot")
          )
        ),
        fluidRow(
          column(6,
                 fluidRow(
                   column(6,
                          selectInput("Season_filter","Temporada",choices = NULL)
                          ),
                   column(6,
                          selectInput("Team_filter","Time",choices = NULL)
                          )
                   ),
                 dataTableOutput("Table_team")
          ),
          column(6,
                 box(title = "Estatísticas do jogador",width = 12,
                     fluidRow(
                       column(4,
                              valueBoxOutput("Kill")),
                       column(4,
                              valueBoxOutput("Death")
                              ),
                       column(4,
                              valueBoxOutput("Assistance")
                       )
                     ),
                    fluidRow(
                      column(4,
                             valueBoxOutput("Creep_score")
                             ),
                      column(4,
                             valueBoxOutput("Damage")
                      ),
                      column(4,
                             valueBoxOutput("Kill_participation")
                      )
                     )
                 )
                
          )
            
        )
    )
)


server <- function(input, output, session) {
  PlayersData <- reactive({
    df <- Player
    return(df)
  })
  WinnersData <- Winners[Winners$event == "Main",]
  WinnersData <- WinnersData[1:20,]
  WinnersData$winner <- reorder(WinnersData$winner, -WinnersData$games)
  colnames(WinnersData) <- c("Times","event","games")
  purple_to_black_palette <- colorRampPalette(c("orange", "yellow"))
  
  observe({
    updateSelectInput(session, "Season_filter", choices = unique(PlayersData()$season))
  })
  
  observe({
    seasonfilter <- input$Season_filter
    if (!is.null(seasonfilter)) {
      teams <- unique(PlayersData()$team[PlayersData()$season == seasonfilter])
      updateSelectInput(session, "Team_filter", choices = teams)
    }
  })
  
  
  output$Teams_plot <- renderPlot({
    ggplot(WinnersData, aes(x = Times, y = games, fill = Times)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = purple_to_black_palette(length(unique(WinnersData$Times)))) +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#222222"),
        panel.background = element_rect(fill = "#222222"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 16, color = "white"),
        legend.title = element_text(size = 16, color = "white"),
        axis.title.y = element_text(size = 20, color = "white"),
        axis.text.y = element_text(size = 16, color = "white"),
        axis.text.x = element_blank(),
        axis.title.x = element_blank()
      )
  })
  
  
  filtered_data <- reactive({
    season_filter <- input$Season_filter
    team_filter <- input$Team_filter
    
    filtered_players <- PlayersData()
    
    if (!is.null(season_filter)) {
      filtered_players <- filtered_players %>% filter(season == season_filter)
    }
    
    if (!is.null(team_filter)) {
      filtered_players <- filtered_players %>% filter(team %in% team_filter)
    }
    
    return(filtered_players)
  })
  

  
  output$Table_team <- renderDataTable({
    data <- filtered_data()
    colunas <- c('season', 'team', 'player', 'games_played', 'win_rate')
    
    datatable(
      data[, colunas, drop = FALSE],
    )
  })
  
  observe ({
    updateSelectInput(session, "Player_filter", choices = unique(PlayersData()$player))
  })
  
  
  output$Comparative_plot <- renderPlot({
    player_filter <- input$Player_filter
    if (is.null(player_filter)) {
      return(NULL)
    }
    jogadores <- PlayersData()
    jogadores <- jogadores[jogadores$event == "Main",]
    jogadores <- jogadores[jogadores$player %in% player_filter, ]
    # Criar o gráfico de linhas com pontos
    ggplot(jogadores, aes(x = season, y = win_rate, group = player, color = player)) +
      geom_line() +
      geom_point() +
      labs(title = "Win Rate por Temporada", x = "Temporada", y = "Win Rate") +
      scale_x_continuous(breaks = 1:12, labels = 1:12)
  })
  
  output$Kill <- renderValueBox({
  player_filter <- input$Player_filter
  if (is.null(player_filter) || length(player_filter) == 0) {
    return(valueBox("N/A", "Média de Kills", icon = icon("times")))
  }

  jogadores <- PlayersData()
  jogadores <- jogadores[jogadores$event == "Main" & jogadores$player %in% player_filter, ]
  
  if (nrow(jogadores) > 0) {
    avg_kills <- mean(jogadores$kills)
    return(valueBox(round(avg_kills, 2), "Média de Kills"))
  } else {
    return(valueBox("N/A", "Média de Kills", icon = icon("times")))
  }
})

# Código para a média de Deaths
output$Death <- renderValueBox({
  player_filter <- input$Player_filter
  if (is.null(player_filter) || length(player_filter) == 0) {
    return(valueBox("N/A", "Média de Deaths", icon = icon("times")))
  }

  jogadores <- PlayersData()
  jogadores <- jogadores[jogadores$event == "Main" & jogadores$player %in% player_filter, ]
  
  if (nrow(jogadores) > 0) {
    avg_deaths <- mean(jogadores$deaths)
    return(valueBox(round(avg_deaths, 2), "Média de Deaths"))
  } else {
    return(valueBox("N/A", "Média de Deaths", icon = icon("times")))
  }
})

# Código para a média de Assists
output$Assistance <- renderValueBox({
  player_filter <- input$Player_filter
  if (is.null(player_filter) || length(player_filter) == 0) {
    return(valueBox("N/A", "Média de Assists", icon = icon("times")))
  }

  jogadores <- PlayersData()
  jogadores <- jogadores[jogadores$event == "Main" & jogadores$player %in% player_filter, ]
  
  if (nrow(jogadores) > 0) {
    avg_assists <- mean(jogadores$assists)
    return(valueBox(round(avg_assists, 2), "Média de Assists"))
  } else {
    return(valueBox("N/A", "Média de Assists", icon = icon("times")))
  }
})

# Código para a média de Creep Score
output$Creep_score <- renderValueBox({
  player_filter <- input$Player_filter
  if (is.null(player_filter) || length(player_filter) == 0) {
    return(valueBox("N/A", "Média de Creep Score", icon = icon("times")))
  }

  jogadores <- PlayersData()
  jogadores <- jogadores[jogadores$event == "Main" & jogadores$player %in% player_filter, ]
  
  if (nrow(jogadores) > 0) {
    avg_creep_score <- mean(jogadores$creep_score)
    return(valueBox(round(avg_creep_score, 2), "Média de Creep Score"))
  } else {
    return(valueBox("N/A", "Média de Creep Score", icon = icon("times")))
  }
})

# Código para a média de Damage
output$Damage <- renderValueBox({
  player_filter <- input$Player_filter
  if (is.null(player_filter) || length(player_filter) == 0) {
    return(valueBox("N/A", "Média de Damage", icon = icon("times")))
  }

  jogadores <- PlayersData()
  jogadores <- jogadores[jogadores$event == "Main" & jogadores$player %in% player_filter, ]
  
  if (nrow(jogadores) > 0) {
    avg_damage <- mean(jogadores$damage)
    return(valueBox(round(avg_damage, 2), "Média de Damage"))
  } else {
    return(valueBox("N/A", "Média de Damage", icon = icon("times")))
  }
})

# Código para a média de Kill Participation
output$Kill_participation <- renderValueBox({
  player_filter <- input$Player_filter
  if (is.null(player_filter) || length(player_filter) == 0) {
    return(valueBox("N/A", "Média de Kill Participation", icon = icon("times")))
  }

  jogadores <- PlayersData()
  jogadores <- jogadores[jogadores$event == "Main" & jogadores$player %in% player_filter, ]
  
  if (nrow(jogadores) > 0) {
    avg_kill_participation <- mean(jogadores$kill_participation)
    return(valueBox(round(avg_kill_participation, 2), "Média de Kill Participation"))
  } else {
    return(valueBox("N/A", "Média de Kill Participation", icon = icon("times")))
  }
})
}

shinyApp(ui = ui, server = server)
