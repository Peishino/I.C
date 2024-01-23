library(shiny)
library(shinydashboard)
library(plotly)
library(tidyverse)

load("Matchs.RData")
load("Champions.RData")
load("Player.RData")

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
          box(width = 12, status = "warning",solidHeader = T,
              infoBoxOutput("Team", width = 3),
              infoBoxOutput("KDA", width = 3),
              infoBoxOutput("CreepScore", width = 3),
              infoBoxOutput("Damage", width = 3)
          ),
          column(7,
                 box(width = 12, status = "warning", solidHeader = T,
                     plotlyOutput("Wr_Season")
                 ),
                 box(width = 12, status = "warning",solidHeader = T,
                     plotlyOutput("gauge")
                 )
          ),
          column(5,
                 box(width = 12, status = "warning",solidHeader = T,
                     selectInput("Player", "Selecione o jogador", choices =  NULL),
                     selectInput("Season", "Filtrar por temporada", choices = NULL)
                 ),
                 box(width = 12, status = "warning", solidHeader = T,
                     plotlyOutput("KDA_Season", height = 642)
                 )
                 
          )
        )
      ),
      # Conteúdo para a segunda aba 
      tabItem(
        tabName = "dashboard2",
        fluidRow(
          box(width = 12,status = "primary",solidHeader = T,
              column(6,
                     selectInput("SeasonChamp", "Filtrar por temporada", choices = NULL)
                     ),
              column(6,
                     selectInput("Lane", "Filtrar por rota", choices = NULL)
                     )
          ),
          column(6,
                 box(width = 12, status = "primary", solidHeader = T,
                     plotlyOutput("GamesContests")
                     ),
                 box(width = 12, status = "primary", solidHeader = T,
                     plotlyOutput("WinLose")
                     )
                 ),
          column(6,
                 box(width = 12, status = "primary", solidHeader = T,
                     plotlyOutput("PickBan")
                     ),
                 box(width = 12, status = "primary", solidHeader = T,
                     plotlyOutput("PlayedByNumber")
                     )
                 )
          
        )
      ),
      # Conteúdo para a terceira aba 
      tabItem(
        tabName = "dashboard3",
        fluidRow(
          box(width = 12,status = "danger" ,solidHeader = T,
              column(6,
                     selectInput("SeasonMatch", "Filtrar por temporada", choices = NULL)
              ),
              column(6,
                     selectInput("Combinacao", "Filtrar por combinação de rotas", choices = c("ADC & SUP","MID & JG","TOP & JG","MID & TOP","ADC, SUP & JG","ADC, SUP & MID","MID, TOP & JG", "Composição"))
              )
          ),
          box(width = 12, status = "danger", solidHeader = T,
              plotlyOutput("WinnerWinrate")
          ),
          column(6,
                 box(width = 12, status = "danger", solidHeader = T,
                     plotlyOutput("RedBlue")
                 )
                 
          ),
          column(6,
                 box(width = 12, status = "danger", solidHeader = T,
                     plotlyOutput("Combinacoes")
                 )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # lendo as bases de dados reativas
  {
    PlayersData <- reactive({
      df <- Player
      df <- df[df$event == "Main", ]
      return(df)
    })
    
    ChampionsData <- reactive({
      df <- Champions
      df <- df[df$event == "Main", ]
      return(df)
    })
    
    MatchsData <- reactive({
      df<- Matchs
      df<- df[df$event == "Main",]
      return(df)
    })
    
  }
  
  
  # inputs Players
  {
    observe({
      updateSelectInput(session, "Player", choices = unique(PlayersData()$player),selected = "Faker")
    })
    
    # seleção de filtros
    observeEvent(input$Player, {
      base <- PlayersData()
      base <- base %>% filter(player == input$Player)
      updateSelectInput(session, "Season", choices = unique(base$season))
      base <- base %>% filter(season == input$Season)
    })
    
    # base Players filtrada
    Players_Inputs <- reactive({
      base <- PlayersData()
      base <- base %>% filter(player == input$Player)
      base <- base %>% filter(season == input$Season)
      return(base)
    })
  }
  
  # inputs Champions
  {
    Champions_filtered <- reactive({
      base <- ChampionsData()
      if (input$SeasonChamp == "Todas" & input$Lane == "Todas") {
        df <- base
      } else {
        if (input$SeasonChamp == "Todas") {
          df <- base %>% filter(lane == input$Lane)
        } else {
          if (input$Lane == "Todas") {
            df <- base %>% filter(season == input$SeasonChamp)
          } else {
            df <- base %>% filter(lane == input$Lane & season == input$SeasonChamp)
            
          }
        }
      }
      champs <- df %>%
        group_by(champion) %>%
        summarise(
          games_contest = sum(games_contests),
          pick = sum(played_games),
          ban = sum(banned_games),
          wins = sum(win),
          loses = sum(lose),
          numberofp = sum(played_by_number_of_players)
        )%>% arrange(-games_contest)
      return(champs)
    })
      
    observe({
      choices <- c("Todas", unique(ChampionsData()$season))
      updateSelectInput(session, "SeasonChamp", choices = choices, selected = if (input$SeasonChamp %in% choices) input$SeasonChamp else "Todas")
    })
    
    observe({
      choices <- c("Todas", unique(ChampionsData()$lane))
      updateSelectInput(session, "Lane", choices = choices, selected = if (input$Lane %in% choices) input$Lane else "Todas")
    })
  }
  
  # inputs Matchs
  {
    Matchs_filtered <- reactive({
      base <- MatchsData()
      df <- if (input$SeasonMatch == "Todas") base else base %>% filter(season == input$SeasonMatch)
      
      df <- df %>%
        mutate(
          combinacoesAzul = case_when(
            input$Combinacao == "ADC & SUP" ~ paste(pick_4_blue_team, pick_5_blue_team, sep = " & "),
            input$Combinacao == "MID & JG" ~ paste(pick_3_blue_team, pick_2_blue_team, sep = " & "),
            input$Combinacao == "TOP & JG" ~ paste(pick_1_blue_team, pick_2_blue_team, sep = " & "),
            input$Combinacao == "MID & TOP" ~ paste(pick_3_blue_team, pick_2_blue_team, sep = " & "),
            input$Combinacao == "ADC, SUP & JG" ~ paste(pick_4_blue_team, pick_5_blue_team, pick_2_blue_team, sep = " & "),
            input$Combinacao == "ADC, SUP & MID" ~ paste(pick_4_blue_team, pick_5_blue_team, pick_3_blue_team, sep = " & "),
            input$Combinacao == "MID, TOP & JG" ~ paste(pick_3_blue_team, pick_1_blue_team, pick_2_blue_team, sep = " & "),
            input$Combinacao == "Composição" ~ paste(pick_4_blue_team, pick_5_blue_team, pick_3_blue_team, pick_2_blue_team, pick_1_blue_team, sep = " & ")
          ),
          combinacoesVermelho = case_when(
            input$Combinacao == "ADC & SUP" ~ paste(pick_4_red_team, pick_5_red_team, sep = " & "),
            input$Combinacao == "MID & JG" ~ paste(pick_3_red_team, pick_2_red_team, sep = " & "),
            input$Combinacao == "TOP & JG" ~ paste(pick_1_red_team, pick_2_red_team, sep = " & "),
            input$Combinacao == "MID & TOP" ~ paste(pick_3_red_team, pick_2_red_team, sep = " & "),
            input$Combinacao == "ADC, SUP & JG" ~ paste(pick_4_red_team, pick_5_red_team, pick_2_red_team, sep = " & "),
            input$Combinacao == "ADC, SUP & MID" ~ paste(pick_4_red_team, pick_5_red_team, pick_3_red_team, sep = " & "),
            input$Combinacao == "MID, TOP & JG" ~ paste(pick_3_red_team, pick_1_red_team, pick_2_red_team, sep = " & "),
            input$Combinacao == "Composição" ~ paste(pick_4_red_team, pick_5_red_team, pick_3_red_team, pick_2_red_team, pick_1_red_team, sep = " & ")
          )
        )
      contador <- c(df$combinacoesAzul, df$combinacoesVermelho)
      contadordf <- as.data.frame(contador)
      
      agrupa <- contadordf %>%
        group_by(contador) %>%
        summarise(
          numvez = n()
        ) %>%
        arrange(desc(numvez))
      
      top5composicoes <- head(agrupa, 5)
  
      return(top5composicoes)
    })
    
    Matchs_RedBlue <- reactive({
      base <- MatchsData()
      if (input$SeasonMatch == "Todas"){
        df <- base
      }else {
        df <- base %>% filter(season == input$SeasonMatch)
      }
      df$blue_winner <- ifelse(df$blue_team == df$winner,1,0)
      df$red_winner <- ifelse(df$red_team == df$winner,1,0)
      
      somawinner <- df %>%
        group_by(winner) %>%
        summarise(
          blueWins = sum(blue_winner),
          redWins = sum(red_winner),
          partidas = sum(blue_winner + red_winner)
        ) %>%
        arrange(desc(partidas)) %>%
        head(10)
      
      return(somawinner)
    })
    
    observe({
      choices <- c("Todas", unique(MatchsData()$season))
      updateSelectInput(session, "SeasonMatch", choices = choices, selected = if (input$SeasonMatch %in% choices) input$SeasonMatch else "Todas")
    })
    
    Matchs_Gauge <- reactive({
      df <- MatchsData()
      seasonput <- ifelse(input$SeasonMatch == "Todas",12,as.numeric(input$SeasonMatch))
      lista_win <- list("Fnatic","Taipei_Assassins","T1","Samsung_White","T1","T1","Samsung_Galaxy","Invictus_Gaming","FunPlus_Phoenix","Dplus_KIA","EDward_Gaming","DRX")
      equipe_vencedora <- lista_win[[seasonput]]
      df_filtered <- filter(df, season == seasonput & (blue_team == equipe_vencedora | red_team == equipe_vencedora))
      return(list(df_filtered = df_filtered, equipe_vencedora = equipe_vencedora))
    })
  }
  
  # info boxes
  {
    output$Team <- renderInfoBox({
      infoBox(
        title = "Time",
        value = Players_Inputs()$team,
        icon = icon("headset"),
        color = "yellow"
      )
    })
    
    output$KDA <- renderInfoBox({
      infoBox(
        title = "Média de KDA",
        value = mean(Players_Inputs()$kill_death_assist_ratio),
        icon = icon("hand-fist"),
        color = "yellow"
      )
    })
    
    output$CreepScore <- renderInfoBox({
      infoBox(
        title = "Média de Minions por minuto",
        value = mean(Players_Inputs()$cs.min),
        icon = icon("chess-pawn"),
        color = "yellow"
      )
    })
    output$Damage <- renderInfoBox({
      infoBox(
        title = "Média de Dano",
        value = mean(Players_Inputs()$damage),
        icon = icon("wand-sparkles"),
        color = "yellow"
      )
    })
  }
  
  # gráficos
  {
    output$KDA_Season <- renderPlotly({
      df <- PlayersData()
      df_filtered <- df %>% filter(player == input$Player)
      
      plot_ly(df_filtered, x = ~kill_death_assist_ratio, y = ~season, type = 'bar',orientation = 'h', marker = list(color = 'orange')) %>%
        layout(title = "Média de KDA por Temporada", xaxis = list(title = "KDA Média"), yaxis = list(title = "Temporada"),
               sliders = list(list(currentvalue = list(prefix = "Temporada:"), steps = list())))
    })
    
    output$Wr_Season <- renderPlotly({
      df <- PlayersData()
      df_filtered <- df %>% filter(player == input$Player)
      
      plot_ly(df_filtered, x = ~factor(season), y = ~wins, type = 'scatter', mode = 'lines+markers', name = 'Win', marker = list(color = 'green')) %>%
        add_trace(y = ~loses, name = 'Lose', marker = list(color = 'red')) %>%
        layout(title = "Vitórias e Derrotas por Temporada", xaxis = list(title = "Temporada"), yaxis = list(title = "Quantidade"))
    })
    
    output$gauge <- renderPlotly({
      df <- PlayersData()
      df_filtered <- df %>% filter(player == input$Player)
      valor <- sum(df_filtered$wins)/sum(df_filtered$games_played)*100
      
      plot_ly(
      type = "indicator",
      title = "Winrate do jogador",
      mode = "gauge+number",
      value = valor,
      gauge = list(
        axis = list(range = list(0, 100)),
        bar = list(color = "#ff9933"),
        steps = list(
          list(range = c(0, 25), color = "#ffd9b3"),
          list(range = c(25, 50), color = "#ffcc99"),
          list(range = c(50, 75), color = "#ffbf80"),
          list(range = c(75, 100), color = "#ffb366")
        )
      )
     )
    })
    
    output$GamesContests <- renderPlotly({
      df <- Champions_filtered()
      top10_champions <- head(df, 10)
      plot_ly(top10_champions, x = ~champion, y = ~games_contest, type = 'bar', marker = list(color = "#3399ff"))%>%
        layout(title = "Top 10 personagens mais disputados", xaxis = list(title = "Personagem"), yaxis = list(title = "Partidas"))
    })
    
    output$PickBan <- renderPlotly({
      df <- Champions_filtered()
      top10_champions <- head(df, 10)
      plot_ly(top10_champions, x = ~champion, y = ~pick, type = 'scatter', mode = 'markers', name = 'Jogados', marker = list(color = '#80bfff'))%>%
        add_trace(y = ~ban, name = 'Banidos', marker = list(color = '#004080'))%>%
        layout(title = "Quantidade de jogos banidos e jogados", xaxis = list(title = "Personagem"), yaxis = list(title = "Quantidade"))
    })
    
    output$WinLose <- renderPlotly({
      df <- Champions_filtered()
      top10_champions <- head(df, 10)
      plot_ly(top10_champions, x = ~champion, y = ~wins, type = 'bar', name = 'Vitórias', marker = list(color = '#1a53ff')) %>%
        add_trace(y = ~loses, name = 'Derrotas', marker = list(color = '#668cff')) %>%
        layout(title = "Número de vitórias e derrotas do personagem",
               xaxis = list(title = "Personagem"),
               yaxis = list(title = "Número de jogos"),
               barmode = 'stack'
        )
    })
    
    output$PlayedByNumber <- renderPlotly({
      df <- Champions_filtered()
      top10_champions <- head(df, 10)
      
      plot_ly(top10_champions, x = ~champion, y = ~numberofp, type = 'bar', name = 'Number of Players', marker = list(color = '#00264d')) %>%
        layout(title = "Número de jogadores que utilizaram o personagem",
               xaxis = list(title = "Personagem"),
               yaxis = list(title = "Número de jogadores"),
               barmode = 'group'
        )
    })
    
    output$Combinacoes <- renderPlotly({
      df <- Matchs_filtered()
      top5composicao <- df
      plot_ly(top5composicao, x = ~contador, y = ~numvez, type = 'bar', marker = list(color = "#BD0000"))%>%
        layout(title = "Top 5 composições", xaxis = list(title = "Composições"), yaxis = list(title = "Partidas"))
    })
    
    output$RedBlue <- renderPlotly({
      df <- Matchs_RedBlue()
      plot_ly(df, x = ~winner, y = ~blueWins, type = 'scatter', mode = 'markers', name = 'Time Azul', marker = list(color = '#214BA0'))%>%
        add_trace(y = ~redWins, name = 'Time Vermelho', marker = list(color = '#A02121'))%>%
        layout(title = "Quantidade de jogos ganhados em cada lado", xaxis = list(title = "Times"), yaxis = list(title = "Partidas"))
    })
    
    output$WinnerWinrate <- renderPlotly({
      result <- Matchs_Gauge()
      df <- result$df_filtered
      equipe_vencedora <- result$equipe_vencedora
      
      valor <- sum(df$winner == equipe_vencedora) / nrow(df) * 100
      
      plot_ly(type = "indicator",
        title = paste("Winrate do time ganhador da season", equipe_vencedora),
        mode = "gauge+number",
        value = valor,
        gauge = list(
          axis = list(range = list(0, 100)),
          bar = list(color = "#F78080"),
          steps = list(
            list(range = c(0, 25), color = "#B21111"),
            list(range = c(25, 50), color = "#C13232"),
            list(range = c(50, 75), color = "#D45050"),
            list(range = c(75, 100), color = "#E36262")
          )
        )
      )
    })
  }
}

shinyApp(ui, server)