library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(dplyr)
library(scales)
library(DT)
library(fmsb)

#pickvalues <- read.csv("pickvalues.csv")
data <- read.csv("draftdata.csv", fileEncoding="UTF-8-BOM")
data$PositionRank <- substring(data$PositionRank, 5)
data$Finish <- substring(data$Finish, 5)

teams <- sort(unique(data$Team))

avs <- data %>% group_by(Year, Team) %>% summarise(avg.value = sum(Draft.Value))
avs <- avs %>% group_by(Year) %>% summarise(avg.value = mean(avg.value))
data <- left_join(data, avs, by='Year')

#matchups <- read.csv("matchup_data.csv")
seasons <- read.csv("seasoninfo.csv")

#matchups <- left_join(matchups, seasons, by='Year')
#matchups$regular <- ifelse(matchups$Week <= matchups$Weeks, 1, 0)

wk <- ceiling(diff(c(strptime("07.09.2021", format = "%d.%m.%Y"),Sys.time()))/7)
wk <- as.integer(wk)

year <- 2021

teamcolors = c(
  "BISCH" = "darkorange3",
  "BONO" = "chartreuse3",
  "COLLIN" = "hotpink",
  "CAP" = "sienna4",
  "CRAIG" = "purple4",
  "DUKE" = "green3",
  "ERIC" = "goldenrod2",
  "FIELER" = "darkred",
  "GREG" = "tomato",
  "HAMM" = "#0E5E69",
  "HOFFY" = "darkorange",
  "JACKROSS" = "mediumpurple1",
  "JAMES" = "navy",
  "JETT" = "skyblue",
  "KERN" = "royalblue3",
  "KEVIN" = "mediumorchid2",
  "KLENK" = "yellow4",
  "KYLE" = "dodgerblue",
  "LOTH" = "white",
  "MEADE" = "pink3",
  "MITCH" = "orangered2",
  "MURPH" = "red",
  "PETERS" = "gold3",
  "RYAN" = "black",
  "SOUP" = "olivedrab",
  "TEPE" = "gray25",
  "TIERNAN" = "burlywood3")


# R-calculated data (download from FPros?)
# multi-season capabilities?
# draft boards/summaries?

library(reticulate)

#reticulate::virtualenv_create("python35_env", python = "python3")
#reticulate::virtualenv_install("python35_env", packages = c("pandas", "espn_api"))
#reticulate::use_virtualenv("python35_env", required = TRUE)

pdf <- read.csv('espn_dat_live.csv')

pdf <- pdf %>% group_by(Year, Week, Team, Position) %>% mutate(posrank = paste(Position, 
                                                                               dense_rank(-Points), sep=''))
pdf <- pdf %>% group_by(Year, Week, Team) %>% mutate(Optimal = max(Points[posrank %in% c('RB3', 'WR3')]) + 
                                                       sum(Points[posrank %in% c('QB1', 'RB1', 'RB2',
                                                                                 'WR1', 'WR2', 'TE1',
                                                                                 'D/ST1', 'K1')]))

opt <- pdf %>% group_by(Year, Week, Team) %>% summarise(Optimal = mean(Optimal))

mdf <- read.csv('espn_mat_live.csv')
mdf <- left_join(mdf, opt, by=c('Year','Week','Team'))
mdf <- left_join(mdf, seasons, by='Year')
mdf$regular <- ifelse(mdf$Week <= mdf$Weeks, 1, 0)
#reticulate::py_run_file('update_data.py')

#df2 <- py$mdf

#print(tail(df2))

mdf <- mdf %>% group_by(Year, Week) %>% mutate(OVRW = min_rank(Score))#, PPG = mean(Score), cons = PPG - sd(Score))
mdf$win <- (mdf$Score > mdf$Opp_Score)
mdf$loss <- (mdf$Score < mdf$Opp_Score)
mdf$draw <- (mdf$Score == mdf$Opp_Score)


ui <- navbarPage(title = "PFL Database",
                 navbarMenu("In-Season Tools",
                            tabPanel('Rankings',
                                     tags$h2('Standings'), tags$br(),   
                                     tableOutput('standings'), tags$br(),tags$br(),
                                     tags$h2('Power Rankings'), tags$br(),
                                     tableOutput('power'), tags$br(),tags$br(),
                                     tags$h2('Season-to-Date Manager Rankings'),tags$br(),
                                     tableOutput('manager')
                            ),
                            tabPanel('Weekly Report',
                                     selectizeInput(
                                       'rp_yr',
                                       'Year: ',
                                       c(2021, 2020,2019),
                                       selected=1
                                     ),
                                     selectizeInput(
                                       'rp_wk',
                                       'Week: ',
                                       c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17),
                                       selected=wk-1
                                     ),
                                     plotlyOutput('pts_wk'),tags$br(),
                                     textOutput('rp_hi'),
                                     textOutput('rp_lo'),
                                     textOutput('rp_av'),
                                     textOutput('rp_top'),
                                     textOutput('rp_nqb'),tags$br(),
                                     tags$h3('Power Rankings'),
                                     tableOutput('rp_power'), tags$br(),
                                     plotlyOutput('rp_prg'), tags$br(),
                                     tags$h3('Season-to-Date Manager Rankings'),
                                     tableOutput('rp_manager'),tags$br(),
                                     tags$h3("Next Week's Matchups"),
                                     htmlOutput('nextweek')
                            ),
                            tabPanel('Positional Charts',
                                     selectizeInput(
                                       'pos_yr',
                                       'Year: ',
                                       c(2021, 2020,2019),
                                       selected=1
                                     ),
                                     selectizeInput(
                                       'pos_wk',
                                       'Week: ',
                                       c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17),
                                       selected=wk-1
                                     ),
                                     plotlyOutput('rp_qb'),tags$br(),
                                     plotlyOutput('rp_rb'),tags$br(),
                                     plotlyOutput('rp_wr'),tags$br(),
                                     plotlyOutput('rp_te'),tags$br(),
                                     plotlyOutput('rp_dst'),tags$br(),
                                     plotlyOutput('rp_k')
                            )
                            
                 ),
                 navbarMenu("Draft",
                            tabPanel("Interactive Graphs",
                                     tags$div(
                                       "The starting value of each pick in a draft is determined using ", 
                                       tags$a(href="https://www.theringer.com/nfl/2018/8/20/17758898/fantasy-football-draft-pick-value-chart",
                                              "this modified version"),
                                       "of Jimmy Johnson's original draft value chart.", tags$br(), tags$br(), 
                                       "That value is then replaced with the value corresponding to the spot where the player would
                            have been drafted if players were drafted in order of their final ranking (positions staying
                            the same). For example, if a player (say, James Robinson) finishes as the RB4, then the pick
                            where he was taken returned the value of the spot where the 4th RB was taken. ", tags$br(),tags$br(),
                                       "Returned Value is the total value of the players drafted regardless of when they were picked. 
                            Added Value is the difference between the Returned Value and the initial value of the draft
                            pick. ", tags$br(),tags$br()
                                     ),
                                     sidebarPanel(
                                       radioButtons(
                                         'metric',
                                         "Metric: ",
                                         selected = 1,
                                         choiceNames = c("Returned Value (Cumulative)", "Added Value (Cumulative)",
                                                         "Returned Value (Each)", "Added Value (Each)"),
                                         choiceValues = c(1,2,3,4)
                                       ),
                                       selectizeInput(
                                         'year',
                                         'Year: ',
                                         c(2020, 2019, 2018, 2017, 2016, 2015, 2014, 2013, 2012, 2011, 2010, 2009),
                                         selected = 2020
                                       )
                                     ),
                                     mainPanel(
                                       plotlyOutput("plot"),
                                       tags$br(),
                                       dataTableOutput('datatable')
                                     )
                            ),
                            tabPanel("Superlatives",
                                     sidebarPanel(
                                       radioButtons(
                                         'bestwhat',
                                         'Best: ',
                                         selected = 1,
                                         choiceNames = c("Draft","Pick", "Drafter"),
                                         choiceValues = c(1,2,3)
                                       ),
                                       sliderInput(
                                         'range',
                                         'Timespan:',
                                         2009,
                                         2020,
                                         value=c(2016,2020),
                                         step=1,
                                         sep=""
                                       ),
                                       numericInput(
                                         'linecount',
                                         'Show Top: ',
                                         value = 20
                                       ),
                                       selectizeInput(
                                         'team',
                                         'Pick a Team: ',
                                         c('All', 'Active', teams),
                                         selected = 1
                                       )
                                     ),
                                     mainPanel(
                                       plotOutput('draftbox'),
                                       tableOutput('besttable')
                                     )
                            )
                 ),
                 navbarMenu("Matchups",
                            tabPanel("vs Opponent",
                                     sidebarPanel(
                                       selectizeInput(
                                         'oppteam',
                                         'Pick a Team: ',
                                         c('All',teams),
                                         selected = 1
                                       ),
                                       checkboxInput(
                                         'opp_post',
                                         label='Include Postseaon'
                                       ),
                                       checkboxInput(
                                         'active',
                                         label='Only Active Opponents',
                                         value = TRUE
                                       ),
                                       
                                       sliderInput(
                                         'opp_range',
                                         'Timespan:',
                                         2009,
                                         2021,
                                         value=c(2009,2021),
                                         step=1,
                                         sep=""
                                       )
                                     ),
                                     mainPanel(
                                       plotlyOutput('heat'),
                                       tableOutput('opptable')
                                     )
                            ),
                            tabPanel("Leaders",
                                     sidebarPanel(
                                       radioButtons(
                                         'matchtype',
                                         'Metric: ',
                                         selected = 1,
                                         choiceNames = c('Wins', 'Win Pct', 'Season Points', 'Highest Scoring Weeks',
                                                         'Lowest Scoring Weeks'),
                                         choiceValues = c(1,2,3,4,5)
                                       ),
                                       checkboxInput(
                                         'match_post',
                                         label='Include Postseaon'
                                       ),
                                       selectizeInput(
                                         'matchteam',
                                         'Pick a Team: ',
                                         c('Active', 'All', teams),
                                         selected = 1
                                       ),
                                       numericInput(
                                         'matchcount',
                                         'Show Top: ',
                                         value = 30
                                       ),
                                       sliderInput(
                                         'match_range',
                                         'Timespan:',
                                         2009,
                                         2021,
                                         value=c(2017,2021),
                                         step=1,
                                         sep=""
                                       )
                                     ),
                                     mainPanel(
                                       tableOutput('matchtable')
                                     )
                            )
                 )
                 
)


server <- function(input, output, session) {
  output$plot <- renderPlotly({
    chartdata <- subset(data, data$Year == input$year)
    p <- ggplot(chartdata)
    if (input$metric == 1) {
      p <- p + geom_line(aes(x=Round, y=Value.addup, color=Team), size = 1.5)
      p <- p + geom_point(aes(x=Round,y=Value.addup, 
                              text=paste(Team,"\n",Year, " Pick ",Pick,"\n",Player,
                                         "\nPicked: ",PositionRank, "\nFinish: ",Finish,sep=""),
                              color=Team), size=0.5)
      p <- p + labs(y="Returned Value", color="")
    } else if (input$metric == 2) {
      p <- p + geom_line(aes(x=Round, y=VAT, color=Team), size = 1.5)
      p <- p + geom_point(aes(x=Round,y=VAT, 
                              text=paste(Team,"\n",Year, " Pick ",Pick,"\n",Player,
                                         "\nPicked: ",PositionRank, "\nFinish: ",Finish,sep=""),
                              color=Team), size=0.5)
      p <- p + labs(y="Added Value", color="")
    } else if (input$metric == 3) {
      p <- p + geom_line(aes(x=Round, y=Draft.Value, color=Team), size = 1.5)
      p <- p + geom_point(aes(x=Round,y=Draft.Value, 
                              text=paste(Team,"\n",Year, " Pick ",Pick,"\n",Player,
                                         "\nPicked: ",PositionRank, "\nFinish: ",Finish,sep=""),
                              color=Team), size=0.5)
      p <- p + labs(y="Returned Value", color="")
    } else {
      p <- p + geom_line(aes(x=Round, y=VA, color=Team), size = 1.5)
      p <- p + geom_point(aes(x=Round,y=VA, 
                              text=paste(Team,"\n",Year, " Pick ",Pick,"\n",Player,
                                         "\nPicked: ",PositionRank, "\nFinish: ",Finish,sep=""),
                              color=Team), size=0.5)
      p <- p + labs(y="Added Value", color="")
    }
    p <- p + ggtitle(sprintf("%s Draft Value by Round",input$year))
    p <- p + scale_x_continuous(n.breaks=14)
    p <- p + theme(panel.grid.minor.x = element_blank(), panel.background = element_rect(fill="gray50"),
                   plot.background = element_rect(fill="gray80"), 
                   legend.key = element_rect(colour="transparent", fill = "transparent"),
                   legend.background = element_rect(fill="transparent"))
    
    p <- p + scale_color_manual(values = teamcolors)
    p <- ggplotly(p, tooltip='text')
    p
  })
  output$datatable <- renderDT({
    tabledata <- subset(data, data$Year == input$year)
    tabledata <- subset(tabledata, select = -c(Value.addup, VAT, Year, TeamID, DraftRange, avg.value))
  }, filter = "top", rownames=FALSE)
  
  output$draftbox <- renderPlot({
    boxdata <- data %>% group_by(Team, Year) %>%
      summarise(position = min(Pick), Value = sum(Draft.Value), Added = sum(VA), 
                avg = mean(avg.value), ValPlus = Value/avg*100)
    
    boxdata <- subset(boxdata, boxdata$Team %in% c('HAMM','CRAIG','BISCH',
                                                   'CAP','COLLIN','RYAN',
                                                   'MURPH','FIELER','ERIC',
                                                   'JAMES','LOTH','DUKE'))
    
    boxdata <- subset(boxdata, boxdata$Year >= input$range[1] & boxdata$Year <= input$range[2])
    
    p <- ggplot(boxdata, aes(x = Team, y = ValPlus))
    #p <- p + geom_violin(trim = FALSE)
    p <- p + geom_dotplot(aes(fill=Team),binaxis = "y", stackdir = "center")
    p <- p + scale_fill_manual(values = teamcolors)
    p <- p + labs(y="Draft+", color="")
    p <- p + theme(legend.position = "none")
    
    
    p
  })
  
  output$besttable <- renderTable({
    if (input$bestwhat == 1){
      bestdata <- data %>% group_by(Team, Year) %>%
        summarise(position = min(Pick), Value = sum(Draft.Value), Added = sum(VA), 
                  avg = mean(avg.value), ValPlus = Value/avg*100)
      bestdata <- bestdata[order(-bestdata$ValPlus),]
      bestdata <- subset(bestdata, select=-avg)
      bestdata <- subset(bestdata, bestdata$Year >= input$range[1] & bestdata$Year <= input$range[2])
      bestdata <- bestdata %>% rename("Draft+" = "ValPlus")
    } else if (input$bestwhat == 2) {
      bestdata <- subset(data, select = -c(Value.addup, VAT, TeamID, DraftRange, Rank, FinRank, avg.value))
      bestdata <- bestdata[order(-bestdata$VA),]
      bestdata <- subset(bestdata, bestdata$Year >= input$range[1] & bestdata$Year <= input$range[2])
    } else {
      bestdata <- data
      bestdata <- subset(bestdata, bestdata$Year >= input$range[1] & bestdata$Year <= input$range[2])
      bestdata <- bestdata %>% group_by(Team, Year) %>%
        summarise(position = min(Pick), Value = sum(Draft.Value), Added = sum(VA), 
                  avg = mean(avg.value), ValPlus = Value/avg*100)
      bestdata <- bestdata %>% group_by(Team) %>%
        summarise(Drafts = n(), Value = sum(Value), Added = sum(Added), 
                  ValPlus = mean(ValPlus))
      bestdata <- bestdata[order(-bestdata$ValPlus),]
      bestdata <- bestdata %>% rename("Draft+" = "ValPlus")
      
    }
    
    if (input$team == 'Active'){
      bestdata <- subset(bestdata, bestdata$Team %in% c('HAMM','CRAIG','BISCH',
                                                        'CAP','COLLIN','RYAN',
                                                        'MURPH','FIELER','ERIC',
                                                        'JAMES','LOTH','DUKE'))
    } else if (input$team != "All") {
      bestdata <- subset(bestdata, bestdata$Team == input$team)
    }
    row.names(bestdata) <- NULL
    head(bestdata, input$linecount)
  }, rownames=TRUE)
  
  output$opptable <- renderTable({
    if (input$oppteam == 'All') {
      oppdata <- subset(mdf, mdf$Team %in% c('HAMM','CRAIG','BISCH',
                                             'CAP','COLLIN','RYAN',
                                             'MURPH','FIELER','ERIC',
                                             'JAMES','LOTH','DUKE'))
    } else {
      oppdata <- subset(mdf, mdf$Team == input$oppteam)
    }
    
    if (input$opp_post) {
      
    } else {
      oppdata <- subset(oppdata, oppdata$regular == 1)
    }
    if (input$active) {
      oppdata <- subset(oppdata, oppdata$Opponent %in% c('HAMM','CRAIG','BISCH',
                                                         'CAP','COLLIN','RYAN',
                                                         'MURPH','FIELER','ERIC',
                                                         'JAMES','LOTH','DUKE'))
    } else {
      
    }
    oppdata <- subset(oppdata, oppdata$Year >= input$opp_range[1] & oppdata$Year <= input$opp_range[2])
    oppdata <- oppdata[!(oppdata$Year == year & oppdata$Week >= wk),]
    oppdata <- oppdata %>% group_by(Team, Opponent) %>% 
      summarize(G=sum(win)+sum(loss)+sum(draw), W=sum(win), L=sum(loss), D=sum(draw), Wpct=round((W+0.5*D)/G,3), Pts=sum(Score),
                PA = sum(Opp_Score))
    oppdata <- oppdata[order(-oppdata$G),]
    oppdata <- oppdata[order(-oppdata$Wpct),]
    oppdata$Wpct <- format(oppdata$Wpct, nsmall=3)
    oppdata
  }, rownames=FALSE)
  
  output$heat <- renderPlotly({
    heatdata <- subset(mdf, mdf$Team %in% c('HAMM','CRAIG','BISCH',
                                            'CAP','COLLIN','RYAN',
                                            'MURPH','FIELER','ERIC',
                                            'JAMES','LOTH','DUKE'))
    heatdata <- subset(heatdata, heatdata$Opponent %in% c('HAMM','CRAIG','BISCH',
                                                          'CAP','COLLIN','RYAN',
                                                          'MURPH','FIELER','ERIC',
                                                          'JAMES','LOTH','DUKE'))
    heatdata <- subset(heatdata, heatdata$Year >= input$opp_range[1] & heatdata$Year <= input$opp_range[2])
    heatdata <- heatdata[!(heatdata$Year == year & heatdata$Week >= wk),]
    if (input$opp_post) {
      
    } else {
      heatdata <- subset(heatdata, heatdata$regular == 1)
    }
    heatdata <- heatdata %>% group_by(Team, Opponent) %>% 
      summarize(Wpct = round((sum(win)+0.5*sum(draw))/(sum(win)+sum(loss)+sum(draw)),3))
    
    p <- ggplot(data=heatdata, mapping = aes(x=Team, y=Opponent, fill=Wpct))
    p <- p + geom_tile()
    p <- p + scale_fill_gradientn(colors = hcl.colors(20, "RdYlGn"))
  })
  
  output$matchtable <- renderTable({
    
    matchdata <- subset(mdf, mdf$Year >= input$match_range[1] & mdf$Year <= input$match_range[2])
    
    matchdata <- matchdata[!(matchdata$Year == year & matchdata$Week >= wk),]
    
    if (input$matchteam == 'Active'){
      matchdata <- subset(matchdata, matchdata$Team %in% c('HAMM','CRAIG','BISCH',
                                                           'CAP','COLLIN','RYAN',
                                                           'MURPH','FIELER','ERIC',
                                                           'JAMES','LOTH','DUKE'))
    } else if (input$matchteam != "All") {
      matchdata <- subset(matchdata, matchdata$Team == input$matchteam)
    }
    
    if (input$match_post) {
      
    } else {
      matchdata <- subset(matchdata, matchdata$regular == 1)
    }
    
    if (input$matchtype == 1){
      matchdata <- matchdata %>% group_by(Team) %>% 
        summarize(W = sum(win), G = sum(win) + sum(loss) + sum(draw))
      matchdata <- matchdata[order(-matchdata$W),]
      head(matchdata, input$matchcount)
    } else if (input$matchtype == 2) {
      matchdata <- matchdata %>% group_by(Team) %>%
        summarize(G=sum(win)+sum(loss)+sum(draw), W=sum(win), L=sum(loss), D=sum(draw), Wpct=round((W+0.5*D)/G,3))
      matchdata <- matchdata[order(-matchdata$Wpct),]
      matchdata$Wpct <- format(matchdata$Wpct, nsmall=3)
      head(matchdata, input$matchcount)
    } else if (input$matchtype == 3) {
      matchdata <- matchdata %>% group_by(Team, Year) %>%
        summarise(Pts = sum(Score), G = sum(win)+sum(loss)+sum(draw), W = sum(win), PPG = round(Pts/G,2))
      matchdata <- matchdata[order(-matchdata$PPG),]
      head(matchdata, input$matchcount)
    } else if (input$matchtype == 4) {
      matchdata <- select(matchdata,c('Team','Year','Week','Opponent','Score','Result'))
      matchdata <- matchdata[order(-matchdata$Score),]
      row.names(matchdata) <- NULL
      head(matchdata, input$matchcount)
    } else if (input$matchtype == 5) {
      matchdata <- select(matchdata,c('Team','Year','Week','Opponent','Score','Result'))
      matchdata <- matchdata[order(matchdata$Score),]
      row.names(matchdata) <- NULL
      head(matchdata, input$matchcount)
    }
    
  }, rownames = TRUE)
  
  output$pts_wk <- renderPlotly({
    
    wdf <- subset(mdf, mdf$Year == input$rp_yr)
    
    p <- ggplot(wdf)
    p <- p + geom_line(aes(x=Week, y=Score, color=Team), size=1)
    p <- p + geom_point(aes(x=Week, y=Score, color=Team,
                            text=paste(Team,"\n",Score,"\n",Result," vs ",Opponent,sep="")))
    p <- p + theme(panel.grid.minor.x = element_blank(), panel.background = element_rect(fill="gray50"),
                   plot.background = element_rect(fill="gray80"), 
                   legend.key = element_rect(colour="transparent", fill = "transparent"),
                   legend.background = element_rect(fill="transparent"))
    p <- p + scale_color_manual(values = teamcolors)
    if (as.integer(input$rp_wk) >= 5){
      p <- p + scale_x_continuous(n.breaks=5,limits=c(as.integer(input$rp_wk) - 4, as.integer(input$rp_wk)))
    } else if (as.integer(input$rp_wk) >= 2) {
      p <- p + scale_x_continuous(n.breaks=as.integer(input$rp_wk),limits=c(1, as.integer(input$rp_wk)))
    } else {
      p <- p + scale_x_continuous(limits = c(1,1))
    }
    p <- ggplotly(p, tooltip='text')
    
    p
  })
  
  output$rp_hi <- renderText({
    wdf <- subset(mdf, mdf$Year == input$rp_yr & mdf$Week == input$rp_wk)
    wdf <- wdf[order(-wdf$Score),]
    top <- head(wdf,1)
    sprintf('Highest Scorer: %s, %s points',top$Team, top$Score)
  })
  
  output$rp_lo <- renderText({
    wdf <- subset(mdf, mdf$Year == input$rp_yr & mdf$Week == input$rp_wk)
    wdf <- wdf[order(wdf$Score),]
    bot <- head(wdf,1)
    sprintf('Lowest Scorer: %s, %s points',bot$Team, bot$Score)
  })
  
  output$rp_av <- renderText({
    wdf <- subset(mdf, mdf$Year == input$rp_yr & mdf$Week == input$rp_wk)
    avg <- round(mean(wdf$Score),2)
    sprintf('Average Score: %s points',avg)
  })
  
  output$rp_top <- renderText({
    wdf <- subset(pdf, pdf$Year == input$rp_yr & pdf$Week == input$rp_wk & pdf$Slot != 'BE')
    wdf <- wdf[order(-wdf$Points),]
    top <- head(wdf,1)
    sprintf('Top Scorer: %s (%s), %s points',top$Player, top$Team, top$Points)
  })
  
  output$rp_nqb <- renderText({
    wdf <- subset(pdf, pdf$Year == input$rp_yr & pdf$Week == input$rp_wk & pdf$Slot != 'BE')
    wdf <- wdf[order(-wdf$Points),]
    top <- head(wdf,1)
    if (top$Position == 'QB'){
      wdf <- subset(wdf, wdf$Position != 'QB')
      wdf <- wdf[order(-wdf$Points),]
      top <- head(wdf,1)
      sprintf('Top Scoring non-QB: %s (%s), %s points',top$Player, top$Team, top$Points)
    } else {
      sprintf('')
    }
    
  })
  
  output$rp_qb <- renderPlotly({
    wdf <- subset(pdf, pdf$Year == input$pos_yr & pdf$Slot == 'QB' & as.integer(pdf$Week) <= as.integer(input$pos_wk))
    #wdf$QB.Points <- as.numeric(unlist(tapply(wdf$Points, wdf$Team, cumsum)))
    wdf = wdf %>% group_by(Team) %>% arrange(Week) %>% mutate(QB.Points = cumsum(Points))
    p <- ggplot(wdf)
    p <- p + geom_line(aes(x=Week, y=QB.Points, color=Team), size=1)
    p <- p + geom_point(aes(x=Week, y=QB.Points, color=Team,
                            text=paste(Team,"\n",Player, "\n",Points,"\n",QB.Points," Total",sep="")))
    p <- p + theme(panel.grid.minor.x = element_blank(), panel.background = element_rect(fill="gray50"),
                   plot.background = element_rect(fill="gray80"), 
                   legend.key = element_rect(colour="transparent", fill = "transparent"),
                   legend.background = element_rect(fill="transparent"))
    p <- p + scale_color_manual(values = teamcolors)
    if (as.integer(input$pos_wk) >= 2) {
      p <- p + scale_x_continuous(n.breaks=as.integer(input$pos_wk),limits=c(1, as.integer(input$pos_wk)))
    } else {
      p <- p + scale_x_continuous(limits = c(1,1))
    }
    p <- p + ggtitle('Cumulative QB Points')
    p <- ggplotly(p, tooltip='text')
    
    p
  })
  output$rp_rb <- renderPlotly({
    wdf <- subset(pdf, pdf$Year == input$pos_yr & pdf$Position == 'RB' & 
                    as.integer(pdf$Week) <= as.integer(input$pos_wk) & pdf$Slot != 'BE' & pdf$Slot != 'IR')
    
    #wdf$QB.Points <- as.numeric(unlist(tapply(wdf$Points, wdf$Team, cumsum)))
    #wdf <- wdf %>% group_by(Team, Week) %>% mutate(Players = paste())
    
    wdf <- wdf %>% group_by(Team, Week) %>% summarize(RBP = sum(Points), n = n(), 
                                                      players = paste(Player, collapse = ', '),
                                                      scores = paste(Points, collapse = ', '))
    
    wdf <- wdf %>% group_by(Team) %>% arrange(Week) %>% mutate(RB.Points = cumsum(RBP), RBs = cumsum(n))
    
    p <- ggplot(wdf)
    p <- p + geom_line(aes(x=Week, y=RB.Points, color=Team), size=1)
    p <- p + geom_point(aes(x=Week, y=RB.Points, color=Team,
                            text=paste(Team,"\n",players, "\n",scores, "\n",RBP," Week ",input$pos_wk,"\n",
                                       RB.Points," Total\n",round(RB.Points/RBs,2)," per RB",sep="")))
    p <- p + theme(panel.grid.minor.x = element_blank(), panel.background = element_rect(fill="gray50"),
                   plot.background = element_rect(fill="gray80"), 
                   legend.key = element_rect(colour="transparent", fill = "transparent"),
                   legend.background = element_rect(fill="transparent"))
    p <- p + scale_color_manual(values = teamcolors)
    if (as.integer(input$pos_wk) >= 2) {
      p <- p + scale_x_continuous(n.breaks=as.integer(input$pos_wk),limits=c(1, as.integer(input$pos_wk)))
    } else {
      p <- p + scale_x_continuous(limits = c(1,1))
    }
    p <- p + ggtitle('Cumulative RB Points')
    p <- ggplotly(p, tooltip='text')
    
    p
  })
  output$rp_wr <- renderPlotly({
    wdf <- subset(pdf, pdf$Year == input$pos_yr & pdf$Position == 'WR' & 
                    as.integer(pdf$Week) <= as.integer(input$pos_wk) & pdf$Slot != 'BE' & pdf$Slot != 'IR')
    
    #wdf$QB.Points <- as.numeric(unlist(tapply(wdf$Points, wdf$Team, cumsum)))
    wdf <- wdf %>% group_by(Team, Week) %>% summarize(WRP = sum(Points), n = n(),
                                                      players = paste(Player, collapse = ', '),
                                                      scores = paste(Points, collapse = ', '))
    
    wdf <- wdf %>% group_by(Team) %>% arrange(Week) %>% mutate(WR.Points = cumsum(WRP), WRs = cumsum(n))
    
    p <- ggplot(wdf)
    p <- p + geom_line(aes(x=Week, y=WR.Points, color=Team), size=1)
    p <- p + geom_point(aes(x=Week, y=WR.Points, color=Team,
                            text=paste(Team,"\n",players, "\n",scores, "\n",WRP, " Week ",input$pos_wk, "\n",
                                       WR.Points," Total\n",round(WR.Points/WRs,2)," per WR",sep="")))
    p <- p + theme(panel.grid.minor.x = element_blank(), panel.background = element_rect(fill="gray50"),
                   plot.background = element_rect(fill="gray80"), 
                   legend.key = element_rect(colour="transparent", fill = "transparent"),
                   legend.background = element_rect(fill="transparent"))
    p <- p + scale_color_manual(values = teamcolors)
    if (as.integer(input$pos_wk) >= 2) {
      p <- p + scale_x_continuous(n.breaks=as.integer(input$pos_wk),limits=c(1, as.integer(input$pos_wk)))
    } else {
      p <- p + scale_x_continuous(limits = c(1,1))
    }
    p <- p + ggtitle('Cumulative WR Points')
    p <- ggplotly(p, tooltip='text')
    
    p
  })
  output$rp_te <- renderPlotly({
    wdf <- subset(pdf, pdf$Year == input$pos_yr & pdf$Position == 'TE' & 
                    as.integer(pdf$Week) <= as.integer(input$pos_wk) & pdf$Slot != 'BE' & pdf$Slot != 'IR')
    
    #wdf$QB.Points <- as.numeric(unlist(tapply(wdf$Points, wdf$Team, cumsum)))
    wdf <- wdf %>% group_by(Team, Week) %>% summarize(TE.Points = sum(Points), n = n(),
                                                      players = paste(Player, collapse = ', '))
    wdf <- wdf %>% group_by(Team) %>% arrange(Week) %>% mutate(TE.Points = cumsum(TE.Points), TEs = cumsum(n))
    p <- ggplot(wdf)
    p <- p + geom_line(aes(x=Week, y=TE.Points, color=Team), size=1)
    p <- p + geom_point(aes(x=Week, y=TE.Points, color=Team,
                            text=paste(Team,"\n",players, "\n",TE.Points," Total\n",round(TE.Points/TEs,2)," per TE",sep="")))
    p <- p + theme(panel.grid.minor.x = element_blank(), panel.background = element_rect(fill="gray50"),
                   plot.background = element_rect(fill="gray80"), 
                   legend.key = element_rect(colour="transparent", fill = "transparent"),
                   legend.background = element_rect(fill="transparent"))
    p <- p + scale_color_manual(values = teamcolors)
    if (as.integer(input$pos_wk) >= 2) {
      p <- p + scale_x_continuous(n.breaks=as.integer(input$pos_wk),limits=c(1, as.integer(input$pos_wk)))
    } else {
      p <- p + scale_x_continuous(limits = c(1,1))
    }
    p <- p + ggtitle('Cumulative TE Points')
    p <- ggplotly(p, tooltip='text')
    
    p
  })
  
  output$rp_k <- renderPlotly({
    wdf <- subset(pdf, pdf$Year == input$pos_yr & pdf$Slot == 'K' & as.integer(pdf$Week) <= as.integer(input$pos_wk))
    #wdf$QB.Points <- as.numeric(unlist(tapply(wdf$Points, wdf$Team, cumsum)))
    wdf = wdf %>% group_by(Team) %>% arrange(Week) %>% mutate(K.Points = cumsum(Points))
    p <- ggplot(wdf)
    p <- p + geom_line(aes(x=Week, y=K.Points, color=Team), size=1)
    p <- p + geom_point(aes(x=Week, y=K.Points, color=Team,
                            text=paste(Team,"\n",Player, "\n",Points,"\n",K.Points," Total",sep="")))
    p <- p + theme(panel.grid.minor.x = element_blank(), panel.background = element_rect(fill="gray50"),
                   plot.background = element_rect(fill="gray80"), 
                   legend.key = element_rect(colour="transparent", fill = "transparent"),
                   legend.background = element_rect(fill="transparent"))
    p <- p + scale_color_manual(values = teamcolors)
    if (as.integer(input$pos_wk) >= 2) {
      p <- p + scale_x_continuous(n.breaks=as.integer(input$pos_wk),limits=c(1, as.integer(input$pos_wk)))
    } else {
      p <- p + scale_x_continuous(limits = c(1,1))
    }
    p <- p + ggtitle('Cumulative K Points')
    p <- ggplotly(p, tooltip='text')
    
    p
  })
  
  output$rp_dst <- renderPlotly({
    wdf <- subset(pdf, pdf$Year == input$pos_yr & pdf$Slot == 'D/ST' & as.integer(pdf$Week) <= as.integer(input$pos_wk))
    #wdf$QB.Points <- as.numeric(unlist(tapply(wdf$Points, wdf$Team, cumsum)))
    wdf = wdf %>% group_by(Team) %>% arrange(Week) %>% mutate(DST.Points = cumsum(Points))
    p <- ggplot(wdf)
    p <- p + geom_line(aes(x=Week, y=DST.Points, color=Team), size=1)
    p <- p + geom_point(aes(x=Week, y=DST.Points, color=Team,
                            text=paste(Team,"\n",Player, "\n",Points,"\n",DST.Points," Total",sep="")))
    p <- p + theme(panel.grid.minor.x = element_blank(), panel.background = element_rect(fill="gray50"),
                   plot.background = element_rect(fill="gray80"), 
                   legend.key = element_rect(colour="transparent", fill = "transparent"),
                   legend.background = element_rect(fill="transparent"))
    p <- p + scale_color_manual(values = teamcolors)
    if (as.integer(input$pos_wk) >= 2) {
      p <- p + scale_x_continuous(n.breaks=as.integer(input$pos_wk),limits=c(1, as.integer(input$pos_wk)))
    } else {
      p <- p + scale_x_continuous(limits = c(1,1))
    }
    p <- p + ggtitle('Cumulative D/ST Points')
    p <- ggplotly(p, tooltip='text')
    
    p
  })
  
  output$standings <- renderTable({
    sdf <- mdf %>% group_by(Year, Team) %>% arrange(Week) %>% mutate(W = cumsum(win), L = cumsum(loss), 
                                                                     Pts = cumsum(Score), PA = cumsum(Opp_Score))
    if (wk == 1) {
      sdf <- subset(sdf, sdf$Year == year & sdf$Week == wk)
    } else {
      sdf <- subset(sdf, sdf$Year == year & sdf$ Week == wk-1)
    }
    sdf$Wpct <- sdf$W/(sdf$W + sdf$L)
    sdf <- subset(sdf, select = c(Team, TeamName, W, L, Wpct, Pts, PA))
    sdf <- sdf[order(-sdf$Pts),]
    sdf[order(-sdf$W),]
  }, rownames = TRUE)
  
  output$power <- renderTable({ # Power Rankings (Rankings Page)
    sdf <- mdf[!(mdf$Year == year & mdf$Week >= wk),]
    
    sdf <- sdf %>% group_by(Year, Team) %>% arrange(Week) %>% mutate(W = cumsum(win), L = cumsum(loss), 
                                                                     Pts = cumsum(Score), PA = cumsum(Opp_Score),
                                                                     PPG = mean(Score), cons = PPG-sd(Score),
                                                                     OVW = cumsum(OVRW))
    if (wk == 1) {
      sdf <- subset(sdf, sdf$Year == year & sdf$Week == wk)
    } else {
      sdf <- subset(sdf, sdf$Year == year & sdf$ Week == wk-1)
    }
    
    sdf$W.Rank <- min_rank(-sdf$W)
    sdf$Pts.Rank <- min_rank(-sdf$PPG)
    sdf$OVW.Rank <- min_rank(-sdf$OVW)
    sdf$Cons.Rank <- min_rank(-sdf$cons)
    
    if (wk <= 2) {
      sdf$avg <- (sdf$W.Rank*3 + sdf$Pts.Rank + sdf$OVW.Rank)/5
    } else {
      sdf$avg <- (sdf$W.Rank*3 + sdf$Pts.Rank + sdf$OVW.Rank + sdf$Cons.Rank)/6
    }
    
    sdf$Power.Rank <- min_rank(sdf$avg)
    
    sdf <- subset(sdf, select = c(Power.Rank, Team, TeamName, avg, W.Rank, Pts.Rank, OVW.Rank, Cons.Rank))
    sdf[order(sdf$Power.Rank),]
    
  }, rownames=FALSE)
  
  output$rp_power <- renderTable({ # Power Rankings (Report Page)
    sdf <- mdf[!(as.integer(mdf$Year) == year & as.integer(mdf$Week) >= wk),]
    
    sdf <- sdf %>% group_by(Year, Team) %>% arrange(Week) %>% mutate(W = cumsum(win), L = cumsum(loss), 
                                                                     Pts = cumsum(Score), PA = cumsum(Opp_Score),
                                                                     PPG = mean(Score), cons = PPG-sd(Score),
                                                                     OVW = cumsum(OVRW))

      sdf <- subset(sdf, sdf$Year == input$rp_yr & as.integer(sdf$Week) == as.integer(input$rp_wk))

    sdf$W.Rank <- min_rank(-sdf$W)
    sdf$Pts.Rank <- min_rank(-sdf$PPG)
    sdf$OVW.Rank <- min_rank(-sdf$OVW)
    sdf$Cons.Rank <- min_rank(-sdf$cons)
    
    if (as.integer(input$rp_wk) < 2) {
      sdf$avg <- (sdf$W.Rank*3 + sdf$Pts.Rank + sdf$OVW.Rank)/5
    } else {
      sdf$avg <- (sdf$W.Rank*3 + sdf$Pts.Rank + sdf$OVW.Rank + sdf$Cons.Rank)/6
    }
    
    sdf$Power.Rank <- min_rank(sdf$avg)
    
    sdf <- subset(sdf, select = c(Power.Rank, Team, TeamName, avg, W.Rank, Pts.Rank, OVW.Rank, Cons.Rank))
    sdf[order(sdf$Power.Rank),]
    
  }, rownames=FALSE)
  
  output$rp_manager <- renderTable({ # Manager Rankings (Report Page)
    sdf <- mdf[!(as.integer(mdf$Year) == year & as.integer(mdf$Week) >= wk),]
    
    if (input$rp_wk == 1) {
      sdf <- subset(sdf, sdf$Year == input$rp_yr & as.integer(sdf$Week) == as.integer(input$rp_wk))
    } else {
      sdf <- subset(sdf, sdf$Year == input$rp_yr & as.integer(sdf$Week) <= as.integer(input$rp_wk))
    }
    
    sdf <- sdf %>% group_by(Team) %>% summarise(Points = sum(Score), Possible = sum(Optimal), 
                                                      Rate = round(Points/Possible,3))
    
    sdf <- sdf[order(-sdf$Rate),]
    
    sdf$Rate <- label_percent()(sdf$Rate)
    sdf
  }, rownames = TRUE)
  
  output$manager <- renderTable({ # Manager Rankings (Rankings Page)
    sdf <- mdf[!(as.integer(mdf$Year) == year & as.integer(mdf$Week) >= wk),]
    

    if (wk == 1) {
      sdf <- subset(sdf, sdf$Year == year & sdf$Week == wk)
    } else {
      sdf <- subset(sdf, sdf$Year == year & sdf$Week <= wk-1)
    }

    
    sdf <- sdf %>% group_by(Team) %>% summarise(Points = sum(Score), Possible = sum(Optimal), 
                                                Rate = round(Points/Possible,3))
    
    sdf <- sdf[order(-sdf$Rate),]
    
    sdf$Rate <- label_percent()(sdf$Rate)
    sdf
  }, rownames = TRUE)
  
  output$rp_prg <- renderPlotly({ # Power Rankings Graph
    sdf <- mdf[!(as.integer(mdf$Year) == year & as.integer(mdf$Week) >= wk),]
    
    sdf <- sdf %>% group_by(Year, Team) %>% arrange(Week) %>% mutate(W = cumsum(win), L = cumsum(loss), 
                                                                     Pts = cumsum(Score), PA = cumsum(Opp_Score),
                                                                     PPG = Pts/Week, 
                                                                     cons = PPG-sd(Score[Week <= as.integer(input$rp_wk)]),
                                                                     OVW = cumsum(OVRW))
    sdf <- sdf %>% group_by(Year, Week) %>% mutate(W.Rank = min_rank(-W),
                                                   Pts.Rank = min_rank(-PPG),
                                                   OVW.Rank = min_rank(-OVW),
                                                   Cons.Rank = min_rank(-cons))
    
    sdf$avg <- ifelse(sdf$Week < 2, (sdf$W.Rank*3 + sdf$Pts.Rank + sdf$OVW.Rank)/5,
                      (sdf$W.Rank*3 + sdf$Pts.Rank + sdf$OVW.Rank + sdf$Cons.Rank)/6)
    
    sdf <- sdf %>% group_by(Year, Week) %>% mutate(Power.Rank = min_rank(avg))
    
    if (input$rp_wk == 1) {
      sdf <- subset(sdf, sdf$Year == input$rp_yr & as.integer(sdf$Week) <= as.integer(input$rp_wk))
    } else {
      sdf <- subset(sdf, sdf$Year == input$rp_yr & as.integer(sdf$Week) <= as.integer(input$rp_wk))
    }

    p <- ggplot(sdf)
    
    p <- p + geom_line(aes(x=Week, y=Power.Rank, color=Team), size=1)
    p <- p + geom_point(aes(x=Week, y=Power.Rank, color=Team))
    p <- p + theme(panel.grid.minor.x = element_blank(), panel.background = element_rect(fill="gray50"),
                   plot.background = element_rect(fill="gray80"), 
                   legend.key = element_rect(colour="transparent", fill = "transparent"),
                   legend.background = element_rect(fill="transparent"))
    p <- p + scale_color_manual(values = teamcolors)

    p <- p + scale_y_continuous(breaks = seq(from = 12.0, to = 0.0, by = -1), trans = 'reverse')
    if (as.integer(input$rp_wk) >= 2) {
      p <- p + scale_x_continuous(n.breaks=as.integer(input$rp_wk),limits=c(1, as.integer(input$rp_wk)))
    } else {
      p <- p + scale_x_continuous(limits = c(1,1))
    }

    p <- p + ggtitle('Power Rankings')
    p
    
    
  })
  
  output$nextweek <- renderUI({
    mat <- subset(mdf, mdf$Year == input$rp_yr & mdf$Week == as.integer(input$rp_wk) + 1)
    mhis <- mdf[!(as.integer(mdf$Year) == input$rp_yr & as.integer(mdf$Week) >= input$rp_wk),]
    mt <- c()
    matxt <- ""
    for (i in 1:nrow(mat)) {
      mt <- c(mt,mat$Opponent[i])
      if (mat$Team[i] %in% mt) {
        
      } else {
        vs <- sprintf("<b> %s vs %s </b>",mat$Team[i],mat$Opponent[i])
        
        mh <- subset(mhis, mhis$Team == mat$Team[i] & mhis$Opponent == mat$Opponent[i])
        histxt <- sprintf("Matchup History: %s %s Wins, %s %s Wins",
                          mat$Team[i],sum(mh$win),mat$Opponent[i],sum(mh$loss))
        
        matxt <- paste(matxt,'<br/>',vs,'<br/>',histxt,'<br/>')
      }
    }
    #HTML(paste(matxt[1],matxt[2],matxt[3],matxt[4],matxt[5],matxt[6], sep="<br/>"))
    HTML(matxt)
    
  })
}

shinyApp(ui = ui, server = server)


