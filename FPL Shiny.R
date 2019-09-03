# R MODEL SHINY
library(shinydashboard)
library(DT)
library(plotly)
library(tidyverse)
library(shinyWidgets)
# library(highcharter)
library(tidyverse)

data_0 <- read_csv("data_aggregated/data_final/final_dataset_5h_5f.csv")


CustomTheme <- theme_bw()+
  theme(
    legend.text = element_text(colour="grey47"),
    legend.title = element_text(colour="grey47"),
    panel.background = element_rect("white"),
    plot.background = element_rect("white"),
    panel.border = element_rect("grey",fill=NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.title = element_text(hjust=0.5,colour="grey47"),
    axis.title = element_text(colour="grey47"),
    axis.text = element_text(colour="grey47"),
    strip.background = element_rect(colour="grey70",fill="grey98"),
    strip.text = element_text(colour="grey47")
  )

# UI Logic ----------------------------------------------------------------

# Sidebar layout ----
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Summary Analysis", tabName="tab1"),
    menuItem("Player Analysis", tabName="tab2")
  )
)

# ..Selection Rows ====
selection <-fluidRow(
  box(
    column(3,uiOutput("s_team")),
    column(3,uiOutput("s_element")),
    column(3,uiOutput("s_variable")),
    column(3,uiOutput("s_minutes")),
    status="primary",width=12)
)

selection_ngames <- fluidRow(
  box(column(3,uiOutput("s_games")),
      column(3,uiOutput("s_highlight_team")),
      column(3,uiOutput("s_xvariable_ngames")),
      column(3,uiOutput("s_yvariable_ngames")),
      status="primary",width=12)
)

player_selection <- fluidRow(
  box(column(6,uiOutput("s_player_team")),
      column(6,uiOutput("s_player_name")),
      status="primary",width=12)
)

selection_scatter <- fluidRow(
  box(column(12,uiOutput("s_gameno")),
      status="primary",width=12)
)

# ..Plot Rows ====
plot <- fluidRow(
  box(
    plotOutput(outputId = "plot"),width=12,status="primary"
  )
)

plot_ngames <- fluidRow(
  box(
    plotOutput(outputId = "plot_ngames"),
    width=12,status="primary"
  )
)

plot_scatter_plotly <- fluidRow(
  box(
    plotlyOutput("plot_scatter_plotly"),
    width=12,status="primary"
  )
)

plot_player_cumustats <- fluidRow(
  box(
    plotOutput(outputId = "plot_player_cumustats"),width=12,status="primary"
  )
)


# Main panel for displaying outputs ----
body <- dashboardBody(
  tabItems(
    tabItem(tabName ="tab1",
            h2("Teams Comparison"),
            selection,
            plot,
            selection_ngames,
            plot_ngames,
            selection_scatter,
            plot_scatter_plotly
    ),
    tabItem(tabName="tab2",
            h2("Player Analysis"),
            player_selection,
            plot_player_cumustats)
  )
)

ui <- dashboardPage(
  dashboardHeader(title="Teams Comparison"),
  sidebar,
  body
)

# Server Logic ------------------------------------------------------------

server <- function(input,output,session) {


# Controls ----------------------------------------------------------------

  
  # ..Graph 1 controls ====
  output$s_team <- renderUI ({
    pickerInput(inputId = "s_team",label = "Team:",
                choices = c(unique(data_0$team)),multiple=TRUE,
                options = list(
                  `actions-box` = TRUE, 
                  size = 10,
                  `selected-text-format` = "count > 3"
                ),selected = c(unique(data_0$team))
                )
  })
  
  output$s_element <- renderUI ({
    pickerInput(inputId = "s_element",label = "Position:",
                choices = c(unique(data_0$element_type)),
                multiple=TRUE,
                options = list(
                  `actions-box` = TRUE, 
                  size = 10
                ),selected=c(unique(data_0$element_type)))
  })
  
  output$s_variable <- renderUI ({
    selectInput(inputId = "s_variable",label = "Variable:",
                choices = c(colnames(select(data_0,-player_id,-web_name,-team,-element_type,-game_no))),
                selected="total_points")
  })
  
  output$s_minutes <- renderUI ({
    checkboxInput(inputId = "s_minutes",label = "Must have played?",
                value=TRUE)
  })
  
  #..Graph 2 controls ====
  output$s_games <- renderUI ({
    sliderInput(inputId = "s_games",label = "Number of prior games to include:",
                min=1,max=5,value=3,step=1)
  })
  
  output$s_highlight_team <- renderUI ({
    selectInput(inputId = "s_highlight_team",
                label = "Team to highlight:",
                choices = c(unique(data_0$team)),
                selected="Spurs")
  })
  
  output$s_xvariable_ngames <- renderUI ({
    pickerInput(inputId = "s_xvariable_ngames",
                label = "X Variable:",
                choices = c(colnames(select(data_0,-player_id,-web_name,-team,-element_type,-game_no))),
                selected="n_goals_scored"
                # choicesOpt = list(
                #   disabled = c(colnames(select(data_0,-player_id,-web_name,-team,-opponent,-element_type,-game_no,-was_home))) %in% c(input$s_yvariable_ngames)
                 )
  })
  
  output$s_yvariable_ngames <- renderUI ({
    pickerInput(inputId = "s_yvariable_ngames",
                label = "Y Variable:",
                choices = c(colnames(select(data_0,-player_id,-web_name,-team,-element_type,-game_no))),
                selected = "n_total_points"
                # choicesOpt = list(
                #   disabled = c(colnames(select(data_0,-player_id,-web_name,-team,-opponent,-element_type,-game_no,-was_home))) %in% c(input$s_xvariable_ngames)
                   )
  })
  

  #.. Scatter controls ====
  output$s_gameno <- renderUI({
    sliderInput(inputId = "s_gameno",
                label = "Game:",
                min = min(data_0$game_no),
                max = max(data_0$game_no),
                value = max(data_0$game_no)-1)
    
  })
  
  
  #..Player controls====
  
  output$s_player_team <- renderUI ({
    selectInput(inputId = "s_player_team",label = "Player team:",
                choices = unique(select(data_0,team)),
                selected="Spurs")
  })
  
  output$s_player_name <- renderUI ({
    selectInput(inputId = "s_player_name",label = "Player name:",
                choices = unique(select(filter(data_0,team==input$s_player_team),web_name)),
                selected="Kane")
  })
  

# Reactive Datasets -------------------------------------------------------

# ..Reactive: Current game data (for graph 1) ====
  r_dataset <- reactive({

    dataset_new <- data_0 %>%
      {if (input$s_minutes == TRUE) filter(.,n_minutes > 0)  else filter(.,n_minutes >= 0)} %>%
      filter(.,element_type %in% input$s_element) %>%
      filter(.,team %in% input$s_team) %>%
      gather(-player_id,-web_name,-team,-element_type,-game_no,key = "variable",value="value") %>%
      filter(variable == input$s_variable)
    dataset_new$value <- as.numeric(dataset_new$value)
    
    return(dataset_new)
  })
  
# ..Reactive: ngames data ====
  r_dataset_ngames <- reactive({

    dataset_new <- data_0

    dataset_new <- dataset_new %>%
      {if (input$s_minutes == TRUE) filter(.,n_minutes > 0)  else filter(.,n_minutes >= 0)} %>%
      filter(.,element_type %in% input$s_element) %>%
      filter(.,team %in% input$s_team) %>%
      select(team,web_name,input$s_xvariable_ngames,input$s_yvariable_ngames)
    #dataset_new$value <- as.numeric(dataset_new$value)
    
    return(dataset_new)
  })
  
  r_dataset_scatter <- reactive({
    dataset <- data_0 %>%
      filter(n_minutes > 0) %>%
      filter(game_no == input$s_gameno)
    
    return(dataset)
  })
  
# ..Reactive: Player information ====
  r_dataset_player <- reactive({
    dataset <- data_0 %>%
      filter(team==input$s_player_team) %>%
      filter(web_name==input$s_player_name)
    
    return(dataset)
  })


# Chart Creation Functions ------------------------------------------------

# Team Overview Charts -----------------------------------------------------------

  # Function to create cluster chart
  f_create_chart_cluster <- function(){
    
    dataset_r <- r_dataset()
    
    ggplot(dataset_r,aes(team,value,colour=team))+
      geom_point(position="jitter",alpha=0.6,size=0.7) + 
      geom_boxplot(aes(alpha=0.5),outlier.shape=NA)+
      CustomTheme+
      theme(legend.position = "none")+
      ggtitle("Points per game")+
      ylab(input$s_variable)

  }
  
  output$plot <- renderPlot({
    f_create_chart_cluster()
  })


  # Function to create ngames chart
  f_create_chart_ngames <- function(){
    
    dataset_r <- r_dataset_ngames()
    
    xvar = input$s_xvariable_ngames
    yvar = input$s_yvariable_ngames
    
    if(xvar==yvar){
      yvar = xvar
    }
    
    highlighted <- input$s_highlight_team
    
    print(dataset_r)
    ggplot(dataset_r,aes_string(xvar,yvar,colour="team"))+
      geom_point(data=subset(dataset_r,team != highlighted),position="jitter",colour="grey") + 
      geom_point(data=subset(dataset_r,team == highlighted),position="jitter") + 
      geom_smooth(aes(colour=NA))+
      CustomTheme+
      theme(legend.position = "none")+
      ggtitle("Points vs variable")+
      xlab(input$s_xvariable_ngames)+
      ylab(input$s_yvariable_ngames)
    
  }
  
  output$plot_ngames <- renderPlot({
    f_create_chart_ngames()
  })
  
  # Function to create scatter chart
  f_create_chart_scatter_plotly <- function(){
    
    dataset_r <- r_dataset_scatter()
    
    plot_ly(data = dataset_r, x = ~n_creativity, y= ~n_influence, text= ~paste(web_name))
    
  }
  
  output$plot_scatter_plotly <- renderPlotly({
    f_create_chart_scatter_plotly()
  })
  

# Player Charts -----------------------------------------------------------


  # Function to create cumulative stats graph
  f_create_chart_player_cumustats <- function(){

    dataset <- r_dataset_player()
    
    ggplot(dataset,aes(game_no,n_goals_scored,colour=element_type))+
      geom_point()+
      geom_line()+
      CustomTheme
  }
  
  output$plot_player_cumustats <- renderPlot({
    f_create_chart_player_cumustats()
  })
  

  
  
}

shinyApp(ui=ui,server=server)
