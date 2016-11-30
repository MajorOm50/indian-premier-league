library("shiny")
library("shinyjs")
library("plotly")

library(ggplot2)
library(reshape2)
library(RColorBrewer)

matches = read.csv("matches.csv", stringsAsFactors = TRUE)
deliveries = read.csv("deliveries.csv", stringsAsFactors = TRUE)

df = merge(matches, deliveries, by.x = "id", by.y = "match_id")
df$season = as.factor(df$season)

batsman_list = as.list(levels(df$batsman))
names(batsman_list) = levels(df$batsman)

functions_list = c("Runs by Seasons", "Dismissals by Seasons", "Strike Rate Plots", "Performance against Teams" , "Favorite Bowlers", "Least Favorite Bowlers", "Favorite Non-Strikers", "Favorite Venue")


ui <- fluidPage(
  
  useShinyjs(),
  
  titlePanel("IPL Batsmen Analytics"),
  
  sidebarPanel(
  selectInput("batsman", "Batsman", choices = batsman_list),
  selectInput("function_name", "Function", choices = functions_list),
  actionButton("button", "Analyze!"),
  br(),br(),
  tags$div(class="header", checked=NA,
           tags$a(href="https://shubh24.github.io", "Check out my blog!")
  )
  ),
  
  mainPanel(
    h3("What is this?"),
    div("My attempt at building an interactive analytics platform for IPL -- Data from all seasons thus far, for all batsmen!"),
    br(),
    div("You can analyze runs scored, dismissals, strike rates, favourite bowlers, non-strikers or venues for any batsman.. Go ahead, have fun!"),
    h3("Thanks to Kaggle!"),
    tags$div(class="header", checked=NA,
             tags$p("An awesome dataset available on Kaggle Open Datasets."),
             tags$a(href="https://www.kaggle.com/manasgarg/ipl", "Show them sove love!")
    )
  ),

  column(10, plotlyOutput("plot")),
  #plotlyOutput("share_plot"),

  column(10, dataTableOutput("list"))
  
)

server <- function(input, output) {
  
  runs = function(vk)({
    vk_season = aggregate(batsman_runs ~ season, data = vk, FUN = sum)
    
    return (renderPlotly(
      ggplot(vk_season, aes(x=season, y = batsman_runs))+
        labs(x = "Season", y = "Runs Scored")+
        ggtitle("Runs by Seasons")+
        geom_bar(stat="identity", fill="black")
    ))
    
  })
  
  dismissal = function(vk)({
    print (str(vk))
    vk_dismissal_long = melt(vk)
    
    return (renderPlotly({
      ggplot(vk_dismissal_long, aes(x = season, y = ..count.. , fill = dismissal_kind)) +
        geom_bar(stat="count") +
        ggtitle("Dismissals by Seasons") +
        labs(x = "Season", y = "Dismissal Kind")+
        scale_fill_brewer(palette = "Set2")
    }))
  })
  
  strike_rate = function(vk)({
    
    vk_over_runs = aggregate(batsman_runs ~ over, data = vk, FUN = sum)
    vk_overs_faced = as.data.frame(table(vk$over))
    colnames(vk_overs_faced) = c("over", "vk_freq")
    
    vk_over_runs = merge(vk_over_runs, vk_overs_faced)
    vk_over_runs$vk_strike_rate = (vk_over_runs$batsman_runs/vk_over_runs$vk_freq)*100
    vk_over_runs$over = as.factor(vk_over_runs$over)
    
    vk_over_runs$batsman_runs = NULL
    
    vk_over_runs_long = melt(vk_over_runs)
    
    return (renderPlotly({
      ggplot(vk_over_runs_long, aes(over, value, group = variable, col = variable)) +
        geom_point() + geom_smooth()+
        ggtitle("Strike Rate by Over and Frequency of Overs Played") +
        labs(x = "Over", y = "Strike Rate/Frequency")
    }))
  })
  
  team_list = function(vk)({
    
    vk$opposition = vk$team1
    vk$opposition[vk$opposition == "Royal Challengers Bangalore"] = vk$team2[vk$opposition == "Royal Challengers Bangalore"]
    vk_fav_team = aggregate(batsman_runs ~ opposition, data = vk, FUN = sum)
    # vk_fav_team[with(vk_fav_team, order(-batsman_runs)),]
    
    vk_balls_faced = as.data.frame(table(vk$opposition))
    colnames(vk_balls_faced) = c("opposition", "balls_faced")
    
    vk_fav_team = merge(vk_fav_team, vk_balls_faced)
    
    vk_fav_team$average_runs_per_ball = vk_fav_team$batsman_runs/vk_fav_team$balls_faced
    
    return (
      renderDataTable({
        vk_fav_team[with(vk_fav_team, order(-average_runs_per_ball)),c("opposition", "average_runs_per_ball")]
      })
    )
    
  })    
  
  fav_bowler = function(vk){
    vk_fav_bowler = aggregate(batsman_runs ~ bowler, data = vk, FUN = sum)
    
    vk_balls_faced_bowler = as.data.frame(table(vk$bowler))
    colnames(vk_balls_faced_bowler) = c("bowler", "balls_faced")
    vk_fav_bowler = merge(vk_fav_bowler, vk_balls_faced_bowler)
    
    vk_fav_bowler = subset(vk_fav_bowler, vk_fav_bowler$balls_faced >= 20) #Consider bowlers with more than 20 deliveries
    
    vk_fav_bowler$average_against_bowler = vk_fav_bowler$batsman_runs/vk_fav_bowler$balls_faced
    
    return (renderDataTable({
      head(vk_fav_bowler[with(vk_fav_bowler, order(-average_against_bowler)),c("bowler", "average_against_bowler")])
    }))
  }
  
  least_fav_bowler = function(vk){
    vk_fav_bowler = aggregate(batsman_runs ~ bowler, data = vk, FUN = sum)
    
    vk_balls_faced_bowler = as.data.frame(table(vk$bowler))
    colnames(vk_balls_faced_bowler) = c("bowler", "balls_faced")
    vk_fav_bowler = merge(vk_fav_bowler, vk_balls_faced_bowler)
    
    vk_fav_bowler = subset(vk_fav_bowler, vk_fav_bowler$balls_faced >= 20) #Consider bowlers with more than 20 deliveries
    
    vk_fav_bowler$average_against_bowler = vk_fav_bowler$batsman_runs/vk_fav_bowler$balls_faced
    
    return (renderDataTable({
      tail(vk_fav_bowler[with(vk_fav_bowler, order(-average_against_bowler)),c("bowler", "average_against_bowler")])
    }))
  }
  
  fav_non_striker = function(vk){
    vk_fav_non_striker = aggregate(batsman_runs ~ non_striker, data = vk, FUN = sum)
    
    return (renderDataTable({
      head(vk_fav_non_striker[with(vk_fav_non_striker, order(-batsman_runs)),])
    }))
  }
  
  fav_venue = function(vk){
    vk_fav_venue = aggregate(batsman_runs ~ venue, data = vk, FUN = sum)
    
    return (renderDataTable({
      head(vk_fav_venue[with(vk_fav_venue, order(-batsman_runs)),])
    }))
  }
  
  observeEvent((input$button), {
    
    vk = subset(df, df$batsman == input$batsman)
    
    if (input$function_name == "Dismissals by Seasons" | input$function_name == "Strike Rate Plots" | input$function_name == "Runs by Seasons"){
      hide("list")
      show("plot")
    } 
    
    if (input$function_name == "Dismissals by Seasons"){
      output$plot =  dismissal(subset(vk, vk$player_dismissed == input$batsman)[,c("season", "dismissal_kind")])
    }
    else if (input$function_name == "Strike Rate Plots"){
      output$plot = strike_rate(vk)
    }
    else if (input$function_name == "Runs by Seasons"){
      output$plot = runs(vk)
    }
    
    else({
      hide("plot")
      show("list")
      output$list = switch(input$function_name,
                          "Performance against Teams" = team_list(vk),
                          "Favorite Bowlers" = fav_bowler(vk),
                          "Least Favorite Bowlers" = least_fav_bowler(vk),
                          "Favorite Non-Strikers" = fav_non_striker(vk), 
                          "Favorite Venue" = fav_venue(vk)
      )
    })
 
#     vk_share = as.data.frame(table(vk$total_runs))
#   
#     output$share_plot = renderPlotly({
#       ggplot(vk_share, aes(x = factor(1), y = vk_share$Freq, fill = factor(vk_share$Var1))) +
#         geom_bar(stat = "identity")+
#         coord_polar(theta = "y")+
#         ggtitle("Kohli's Run Share")+
#         labs(x="", y="")+
#         scale_fill_discrete(guide_legend(title = "Run Color"))
#     })

  })  
  
}


shinyApp(ui = ui, server = server)
