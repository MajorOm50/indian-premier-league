library(shiny)
library("plotly")

matches = read.csv("../matches.csv", stringsAsFactors = TRUE)
deliveries = read.csv("../deliveries.csv", stringsAsFactors = TRUE)

df = merge(matches, deliveries, by.x = "id", by.y = "match_id")
df$season = as.factor(df$season)

vk = subset(df, df$batsman == "V Kohli")
msd = subset(df, df$batsman == "MS Dhoni")

vk_season = aggregate(batsman_runs ~ season, data = vk, FUN = sum)
colnames(vk_season) = c("season", "runs_kohli")
msd_season = aggregate(batsman_runs ~ season, data = msd, FUN = sum)
colnames(msd_season) = c("season", "runs_dhoni")
vk_msd_season = merge(vk_season, msd_season)

library(ggplot2)
library(reshape2)
library(RColorBrewer)

vk_msd_season_long = melt(vk_msd_season) #Transforms the data frame to one, with dhoni/kohli as factors

vk_dismissal = subset(vk, vk$player_dismissed == "V Kohli")[,c("season", "dismissal_kind")]
vk_dismissal_long = melt(vk_dismissal)

msd_dismissal = subset(msd, msd$player_dismissed == "MS Dhoni")[,c("season", "dismissal_kind")]
msd_dismissal_long = melt(msd_dismissal)

# ggplot(msd_dismissal_long, aes(x = season, y = ..count.. , fill = dismissal_kind)) + 
#   geom_bar(stat="count") + 
#   ggtitle("Dhoni -- Dismissals by Seasons") +
#   labs(x = "Season", y = "Dismissal Kind")+
#   scale_fill_brewer(palette = "Set2")

ui <- fluidPage(
  tabPanel(
    "Plot",
    fluidRow(
  plotlyOutput("plot1"),
  plotlyOutput("plot2"),
  verbatimTextOutput("event")
)))

server <- function(input, output) {
  
  # renderPlotly() also understands ggplot2 objects!
  output$plot1 <- renderPlotly({
    ggplot(vk_msd_season_long, aes(x = season, y = value, fill = variable)) + 
      geom_bar(stat="identity", position = "dodge") + #dodge -- place bars side-to-side
      scale_fill_manual(values = c("red","yellow")) + #scale_fill_manual for barplots
      ggtitle("Kohli vs Dhoni -- Runs by Seasons") +
      labs(x = "Season", y = "Runs")
    
  })
  output$plot2 = renderPlotly({
    ggplot(vk_dismissal_long, aes(x = season, y = ..count.. , fill = dismissal_kind)) +
      geom_bar(stat="count") + 
      ggtitle("Kohli -- Dismissals by Seasons") +
      labs(x = "Season", y = "Dismissal Kind")+
      scale_fill_brewer(palette = "Set2")
    
  })
  
  
  output$event <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d)) "Hover on a point!" else d
  })
}

shinyApp(ui = ui, server = server)
