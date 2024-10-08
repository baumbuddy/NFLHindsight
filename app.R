# libraries ####
library(DT)
library(ggplot2)
library(cowplot)
library(shiny)
library(plyr)
library(summaryBox)
library(bslib)
library(shinydashboard)
library("imager")
library("png")
library(grid)
library(ggridges)

# data ####
data_nflhindsight <- readRDS("data/data_nflhindsight.rds")

# define metrics and units ####
metrics <- c("height","wt","arm","hand","X40combine","bench","X10ydsplit","vertical","broad_jump")
names(metrics) <- c("inches", "lbs", "inches", 
                    "inches","seconds", "225 repetitions", 
                    "seconds","inches","inches")

# set colors and themes ####
colors_hindsigt <- c("#0099ff","#ff0099","#ff3399")
theme_hindsight <- function(){ 
  font <- "Georgia"   #assign font family up front
  
  theme_minimal() +    #replace elements we want to change
    
    theme(
      
      #grid elements
      panel.grid.major = element_blank(),    #strip major gridlines
      panel.grid.minor = element_blank(),    #strip minor gridlines
      panel.grid.major.y = element_line(size = 0.8, linetype = "dashed"),
      axis.ticks = element_line(),          #strip axis ticks
      
      #axis lines
      axis.line = element_line(),
      
      #since theme_minimal() already strips axis lines, 
      #we don't need to do that again
      
      #text elements
      plot.title = element_text(             #title
        family = font,            #set font family
        size = 14,                #set font size
        face = 'plain',            #bold typeface
        hjust = 0,                #left align
        vjust = 2),               #raise slightly
      
      plot.subtitle = element_text(          #subtitle
        family = font,            #font family
        size = 14),               #font size
      
      plot.caption = element_text(           #caption
        family = font,            #font family
        size = 12,                 #font size
        hjust = 1),               #right align
      
      axis.title = element_text(             #axis titles
        family = font,            #font family
        size = 12),               #font size
      
      axis.text = element_text(              #axis text
        family = font,            #axis famuly
        size = 12),                #font size
      
      axis.text.x = element_text(            #margin for axis text
        margin=margin(5, b = 10))
      
      #since the legend often requires manual tweaking 
      #based on plot content, don't define it here
    )
}

# Shiny: app ####
{
  # UI ####
  ui = dashboardPage(
    dashboardHeader(title = "NFL Hindsight"),
    # Nav ####
    dashboardSidebar(width = 180,  
                     sidebarMenu(
                       menuItem("INFO", tabName = "INFO", icon = icon("dashboard")),
                       menuItem("DATA", tabName = "DATA", icon = icon("dashboard")),
                       menuItem("DRAFT RESULTS", tabName = "DRAFT_RESULTS", icon = icon("dashboard")),
                       menuItem("PCA", tabName = "PCA", icon = icon("dashboard")),
                       menuItem("COMPARISONS", tabName = "COMPARISONS", icon = icon("dashboard")),
                       menuItem("COMPARE YOURSELF", tabName = "COMPARE_YOURSELF", icon = icon("dashboard"))
                     )),
    # BODY ####
    dashboardBody(
      tabItems(
        # INFO ####
        tabItem( 
          tabName = "INFO",
          fluidRow(
            box(
              title = "NFL",
              width = 4,
              plotOutput("info_nfllogo"),
              "NFL (National Football League) is the professional American football league in the United States. It is a league of 32 teams who each play 17 regular season games in hopes of reaching the knock-out phase called \"playoff\". The final two teams face off in the coveted final game: the SuperBowl."
            ),
            box(
              title = "NFL COMBINE",
              width = 4,
              plotOutput("info_nflcombine"),
              "The NFL Combine is a yearly event in which player prospects, hoping to get drafted into the NFL, display their athleticism to bolster their \"Draft\" stock."
            ),
            box(
              title = "NFL DRAFT",
              width = 4,
              plotOutput("info_nfldraft"),
              "The NFL Draft is an event that takes place in the offseason every year. NFL teams take turns selecting players in a turn-order determined by the results of the previous season, with the worst performing teams selecting first."
            )
          )
        )
        # DATA ####
        ,
        tabItem( 
          tabName = "DATA",
          fluidRow(
            box(
              width = 2,
              checkboxGroupInput(
                "data_checkbox.class",
                label = "Draft class",
                choices = c(
                  "2013" = 2013,
                  "2014" = 2014,
                  "2015" = 2015,
                  "2016" = 2016
                ),
                selected = list(2013, 2014, 2015, 2016)
              ),
              checkboxGroupInput(
                "data_checkbox.position",
                label = "Position(s)",
                choices = c(
                  "QB" = "QB",
                  "RB" = "RB",
                  "WR" = "WR",
                  "TE" = "WR",
                  "OL" = "OL",
                  "DT" = "DT",
                  "DE" = "DE",
                  "DB" = "DB"
                ),
                selected = list("QB", "RB", "WR", "TE", "OL", "DT", "DE", "DB")
              ),
              "Download dataset",
              br(),
              # Button
              downloadButton("data_downloadbutton", "Download")
            ),
            box(width = 10,
                DT::DTOutput("data_table")),
            box(
              width = 3, collapsible = TRUE, collapsed = TRUE,
              title = "Definitions: Draft class",
              solidHeader = TRUE,
              background = "blue",
              "A \"draft class\" is one year's class of draft prospects.",
              br(),
              ""
            ),
            box(
              width = 3,collapsible = TRUE, collapsed = TRUE,
              title = "Definitions: Positions",
              solidHeader = TRUE,
              background = "blue",
              "American football has a large varierty of positions who themselves branch off into further subpositions based off position subtypes. Here, I have elected to categorize players into 9 broad groupings:",
              br(),
              "QB = Quarterback, RB = Running Back, WR = Wide Receiver, TE = Tight End, OL = Offensive Lineman, DT = Defensive Tackle, DE = Defensive End, LB = Linebacker, DB = Defensive Back"
            ),
            box(
              width = 3,collapsible = TRUE, collapsed = TRUE,
              title = "Definitions: Combine disciplines & abbreviations",
              solidHeader = TRUE,
              background = "blue",
              "The NFL Combine includes a host of disciplines of which I have included nine: arm length (arm), 225 lbs bench press (bench), broad jump, hand size (hand), height, vertical jump (vertical), weight (wt), 10-yard split (10yd-split or X10ydsplit) and 40-yard dash (40yd-dash or X40combine).",
            ),
            box(
              width = 3,collapsible = TRUE, collapsed = TRUE,
              title = "Definitions: Measurement units",
              solidHeader = TRUE,
              background = "blue",
              "NFL football is an United States football league, and all measurements included within this work are logged with the very practical imperial units.",
              br(),
              "Arm length (inches), 225 bench press (lbs), broad jump (inches), hand size (inches), height (inches), vertical jump (inches), weight (lbs), 10-yard split (s) and 40-yard dash (s)."
            )
          ))
        # DRAFT RESULTS ####
        ,
        tabItem(
          tabName = "DRAFT_RESULTS",
          fluidRow(
            box(
              width = 3,
              numericInput(
                inputId = "threshold.rating_draft",
                label = "Madden-rating threshold",
                value = 81
              ),
              checkboxGroupInput(
                inputId = "checkbox.class_draft",
                label = "Draft class",
                choices = c(
                  "2013" = 2013,
                  "2014" = 2014,
                  "2015" = 2015,
                  "2016" = 2016
                ),
                selected = list(2013, 2014, 2015, 2016)
              ),
              checkboxGroupInput(
                inputId = "draft_checkbox.position",
                label = "Position(s)",
                choices = c(
                  "QB" = "QB",
                  "RB" = "RB",
                  "WR" = "WR",
                  "TE" = "WR",
                  "OL" = "OL",
                  "DT" = "DT",
                  "DE" = "DE",
                  "DB" = "DB"
                ),
                selected = list("QB")
              )
            ),
            box(
              width = 4,
              plotOutput("draftresults_barplot1")),
            box(
              width = 4,
              plotOutput("draftresults_barplot2")),
            box(
              width = 3,collapsible = TRUE, collapsed = TRUE,
              title = "Definitions: Madden-rating threshold",
              solidHeader = TRUE,
              background = "blue",
              "The Madden-rating threshold is the minimum Madden-rating that must be achieved by a player in year 4 or 5 of their career, in order for that player to be considered apart from the rest in these analyses and visualizations.",
            )
          ))
        # PCA ####
        ,
        tabItem(
          tabName = "PCA",
          fluidRow(
            box(
              width = 2,
              numericInput(
                inputId = "threshold.rating_PCA",
                label = "Madden-rating threshold",
                value = 81
              ),
              checkboxGroupInput(
                inputId = "pca_checkbox.position",
                label = "Position(s)",
                choices = c(
                  "QB" = "QB",
                  "RB" = "RB",
                  "WR" = "WR",
                  "TE" = "WR",
                  "OL" = "OL",
                  "DT" = "DT",
                  "DE" = "DE",
                  "DB" = "DB"
                ),
                selected = list("RB")),
              sliderInput(
                "rangePC1",
                "PC1 range:",
                min = -7.0,
                max = 7.0,
                value = c(-1.0, 1.0),
                step = 0.1
              ),
              sliderInput(
                "rangePC2",
                "PC2 range:",
                min = -7.0,
                max = 7.0,
                value = c(-7.0, 7.0),
                step = 0.1
              ),
              sliderInput(
                "rangePC3",
                "PC3 range:",
                min = -7.0,
                max = 7.0,
                value = c(-7.0, 7.0),
                step = 0.1
              ),
              checkboxGroupInput(
                inputId = "pca_checkbox.metrics",
                label = "NFL Combine metrics",
                choices = c(
                  "Arm length" = "arm",
                  "Bench press" = "bench",
                  "Broad jump" = "broad_jump",
                  "Hand size" = "hand",
                  "Height" = "height",
                  "Vertical jump" = "vertical",
                  "Weight" = "wt",
                  "10-yard split" = "X10ydsplit",
                  "40-yard dash" = "X40combine"
                ),
                selected = list("arm","bench","broad_jump","hand","height","vertical","wt","X10ydsplit","X40combine")
              )
            ),
            box(
              width = 10,
              plotOutput("plotPCA", height = 600)
            )
          ))
        # COMPARISONS ####
        ,
        tabItem(
          tabName = "COMPARISONS",
          fluidRow(
            box(
              width = 2,
              numericInput(
                inputId = "comparisons_threshold.rating",
                label = "Madden-rating threshold",
                value = 81
              ),
              checkboxGroupInput(
                inputId = "comparisons_checkbox.position",
                label = "Position(s)",
                choices = c(
                  "RB" = "RB",
                  "WR" = "WR",
                  "TE" = "WR",
                  "OL" = "OL",
                  "DT" = "DT",
                  "DE" = "DE",
                  "DB" = "DB"
                ),
                selected = list("RB")
              ),
              checkboxGroupInput(
                inputId = "comparisons_checkbox.draftp",
                label = "Round(s) drafted",
                choices = c(
                  "1" = 1,
                  "2" = 2,
                  "3" = 3,
                  "4" = 4,
                  "5" = 5,
                  "6" = 6,
                  "7" = 7
                ),
                selected = list(1,2,3,4,5,6,7)
              )
            ),
            box(
              width = 10,
              plotOutput("comparisons_distplot", height = 500)
            )))
        # COMPARE YOURSELF ####
        ,
        tabItem(
          tabName = "COMPARE_YOURSELF",
          fluidRow(
            box(
              width = 2,
              selectInput(
                inputId = "compare.yourself.metric",
                label = "What to compare?",
                choices = c(
                  "arm length" = "arm",
                  "broad jump" = "broad_jump",
                  "hand size" = "hand",
                  "height" = "height",
                  "225 lbs bench press" = "bench",
                  "10-yard split" = "X10ydsplit",
                  "40-yard dash" = "X40combine"
                ),
                selected = "arm length"
              ),
              numericInput(
                label = "Input measurement (cm/seconds/reps)",
                inputId = "compare.yourself.metric.value",
                value = 5, step = 0.01
              )
              
            ),
            box(
              width = 10,
              plotOutput("plotCOMPARE", height = 400)
            )
          ))
        
        # END
      )
    )
  )
  
  
  # server ####
  server = function(input, output) {
    # INFO ####
    output$info_nfllogo <- renderPlot({
      #load image
      nfl_logo <- png::readPNG("data/nfllogo.PNG")
      #raster
      g <- grid::rasterGrob(nfl_logo, interpolate=TRUE)
      # plot image
      p <- ggplot() + theme_void() +
        annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
      print(p)
    })
    output$info_nflcombine <- renderPlot({
      # load image
      nfl_logo <- png::readPNG("data/nflcombine.PNG")
      #raster
      g <- grid::rasterGrob(nfl_logo, interpolate=TRUE)
      # plot image
      ggplot() + theme_void() +
        annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
    })
    output$info_nfldraft <- renderPlot({
      # load image
      image <- png::readPNG("data/nfldraft2.PNG")
      # raster
      g <- grid::rasterGrob(image, interpolate=TRUE)
      # plot image
      ggplot() + theme_void() +
        annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
    })
    
    
    # DATA ####
    newData <- reactive({
      data_nflhindsight <- subset(data_nflhindsight, class %in% input$data_checkbox.class & position %in% input$data_checkbox.position)
    })
    
    rv <- reactiveValues(
      options = list(stateSave = TRUE,
                     stateDuration = -1,
                     order = list())
    )
    
    output$data_table <- renderDT(
      newData(),
      options = list(
        pageLength = 10, 
        info = TRUE, 
        lengthMenu = list(c(10,-1), c("10","All")),
        scrollX = TRUE)
    )
    
    observeEvent(input$newData_state$order, {
      if (!identical(rv$options$order, input$newData_state$order)) {
        rv$options$order <- input$newData_state$order
      }
    }
    )
    
    output$data_downloadbutton <- downloadHandler(
      filename = function() {
        paste(input$data_dataset, ".csv", sep = "")
      },
      content = function(file) {
        write.csv(newData(), file, row.names = FALSE)
      }
    )
    
    # DRAFT RESULTS ####
    draft.results <- reactive({
      # define variables
      data_nflhindsight$max_rating <- NA
      data_nflhindsight$success <- FALSE
      
      # convert madden ratings to numeric
      data_nflhindsight[,c("madden_4","madden_5")] <- sapply(data_nflhindsight[,c("madden_4","madden_5")], as.numeric)
      
      # find max rating in years 4 and 5
      for (i in 1:nrow(data_nflhindsight)) {
        data_nflhindsight$max_rating[i] <- max(data_nflhindsight[i,c("madden_4","madden_5")])
      }
      
      data_nflhindsight$success[data_nflhindsight$max_rating >= input$threshold.rating_draft] <- TRUE
      
      # isolate positions of interest
      df.all <- data_nflhindsight
      df.select <- data_nflhindsight[data_nflhindsight$class %in% input$checkbox.class_draft & data_nflhindsight$position %in% input$draft_checkbox.position, ]
      
      # create df aggregated by round: all
      draft.aggregate_all <- data.frame(
        aggregate(df.all$success, by = list(df.all$round), sum)[,1],
        aggregate(df.all$success, by = list(df.all$round), sum)[,2],
        aggregate(df.all$success, by = list(df.all$round), length)[,2]
      )
      colnames(draft.aggregate_all) <- c("round","success","total")
      # create df aggregated by round: select
      draft.aggregate_select <- data.frame(
        aggregate(df.select$success, by = list(df.select$round), sum)[,1],
        aggregate(df.select$success, by = list(df.select$round), sum)[,2],
        aggregate(df.select$success, by = list(df.select$round), length)[,2]
      )
      colnames(draft.aggregate_select) <- c("round","success","total")
      
      # define prop variable
      draft.aggregate_all$prop <- draft.aggregate_all$success / draft.aggregate_all$total
      draft.aggregate_select$prop <- draft.aggregate_select$success / draft.aggregate_select$total
      
      # create visual
      plot.all <- ggplot(draft.aggregate_all, aes(x = round, y = prop)) + 
        geom_bar(stat = "identity", fill = colors_hindsigt[1]) +
        xlab("Draft Round") + 
        ylab("Prop. players \u2265Threshold") +
        ggtitle("All draft classes | All positions") + 
        scale_x_continuous(breaks = 1:7) +
        geom_text(aes(label=success), position=position_dodge(width=0.9), vjust=-0.25)
      plot.select <- ggplot(draft.aggregate_select, aes(x = round, y = prop)) + 
        geom_bar(stat = "identity", fill = "orange") +
        xlab("Draft Round") + 
        ylab("Prop. players \u2265Threshold") +
        ggtitle(paste(paste0(input$checkbox.class_draft, collapse = ", "),"|",paste0(input$draft_checkbox.position, collapse = ",")))+ 
        scale_x_continuous(breaks = 1:7) +
        geom_text(aes(label=success), position=position_dodge(width=0.9), vjust=-0.25)
      plot_grid(plot.all, plot.select)
      return(list("plot.all" = plot.all, "plot.select" = plot.select))
    })
    output$draftresults_barplot1 <- renderPlot({
      draft.results()$plot.all +
        theme_hindsight() +
        ylim(0,1)
      
    })
    output$draftresults_barplot2 <- renderPlot({
      draft.results()$plot.select +
        theme_hindsight() + 
        ylim(0,1)
    })
    
    
    # PCA ####
    output$plotPCA <- renderPlot({
      # define variables
      data_nflhindsight$max_rating <- NA
      data_nflhindsight$success <- FALSE
      
      # convert madden ratings to numeric
      data_nflhindsight[,c("madden_4","madden_5")] <- sapply(data_nflhindsight[,c("madden_4","madden_5")], as.numeric)
      
      # find max rating in years 4 and 5
      for (i in 1:nrow(data_nflhindsight)) {
        data_nflhindsight$max_rating[i] <- max(data_nflhindsight[i,c("madden_4","madden_5")])
      }
      
      data_nflhindsight$success[data_nflhindsight$max_rating >= input$threshold.rating_PCA] <- TRUE
      
      # <--- prepare data for PCA ####
      df.na.omit <- data_nflhindsight[, c("success", "position", input$pca_checkbox.metrics)]
      df.na.omit[, -(1:2)] <- sapply(df.na.omit[,-(1:2)], as.numeric)
      df.na.omit <- as.data.frame(df.na.omit)
      df.na.omit <- na.omit(df.na.omit)
      df.na.omit.pos <- df.na.omit[df.na.omit$position %in% input$pca_checkbox.position,]
      
      # <--- perform PCA ####
      PCA <- prcomp(df.na.omit.pos[,-(1:2)], scale. = TRUE, center = TRUE)
      PCA$x <- as.data.frame(PCA$x)
      
      PCA$selection <- data.frame(
        "player.index" = as.numeric(rownames(df.na.omit.pos)),
        "PC1" = rep(NA, nrow(PCA$x)),
        "PC2" = rep(NA, nrow(PCA$x)),
        "PC3" = rep(NA, nrow(PCA$x)),
        "select" = "yes"
      )
      PCA$selection$PC1[PCA$x[, c("PC1")] >= input$rangePC1[1] & PCA$x[, c("PC1")] <= input$rangePC1[2]] <- PCA$x[PCA$x[, c("PC1")] >= input$rangePC1[1] & PCA$x[, c("PC1")] <= input$rangePC1[2], c("PC1")]
      PCA$selection$PC2[PCA$x[, c("PC2")] >= input$rangePC2[1] & PCA$x[, c("PC2")] <= input$rangePC2[2]] <- PCA$x[PCA$x[, c("PC2")] >= input$rangePC2[1] & PCA$x[, c("PC2")] <= input$rangePC2[2], c("PC2")]
      PCA$selection$PC3[PCA$x[, c("PC3")] >= input$rangePC3[1] & PCA$x[, c("PC3")] <= input$rangePC3[2]] <- PCA$x[PCA$x[, c("PC3")] >= input$rangePC3[1] & PCA$x[, c("PC3")] <= input$rangePC3[2], c("PC3")]
      
      # scree plot data
      scree_plot_data = PCA$sdev^2 / sum(PCA$sdev^2)
      
      
      df.hull <- na.omit(PCA$selection)
      
      # PCA 1 & 2 hull
      find_hull_12 <- function(df.hull) df.hull[chull(df.hull$PC1, df.hull$PC2), ]
      hulls_12 <- ddply(df.hull, "select", find_hull_12)
      
      # PCA 1 & 3 hull
      find_hull_13 <- function(df.hull) df.hull[chull(df.hull$PC1, df.hull$PC3), ]
      hulls_13 <- ddply(df.hull, "select", find_hull_13)
      
      PCA_12 <- ggplot() +
        geom_point(data = PCA$x, aes(
          x = PC1,
          y = PC2,
          color = df.na.omit.pos$success
        ), size = 3, alpha = 0.8) +
        geom_polygon(
          aes(x = hulls_12$PC1, y = hulls_12$PC2),
          data = hulls_12,
          alpha = 0.3,
          fill = colors_hindsigt[1]
        ) +
        scale_color_manual(values = c("grey60",colors_hindsigt[3]), labels = c("<Threshold","\u2265Threshold")) +
        ggtitle(paste("PCA Components 1 & 2", "|", paste(input$pca_checkbox.position, collapse = ", "))) +
        theme_hindsight() +
        theme(legend.position = c(0,0), legend.justification = c(0,0), legend.title = element_blank()) +
        xlab(paste("PC1 ", "(", round(scree_plot_data[1]*100, 0), "%)",sep = "")) +
        ylab(paste("PC2 ", "(", round(scree_plot_data[2]*100, 0), "%)",sep = ""))
      
      PCA_13 <- ggplot() +
        geom_point(data = PCA$x, aes(
          x = PC1,
          y = PC3,
          color = df.na.omit.pos$success
        ), size = 3, alpha = 0.8) +
        geom_polygon(
          aes(x = hulls_13$PC1, y = hulls_13$PC3),
          data = hulls_13,
          alpha = 0.3,
          fill = colors_hindsigt[1]
        ) +
        scale_color_manual(values = c("grey60",colors_hindsigt[3]), labels = c("<Threshold","\u2265Threshold")) +
        ggtitle(paste("PCA Components 1 & 3", "|", paste(input$pca_checkbox.position, collapse = ", "))) +
        theme_hindsight() +
        theme(legend.position = c(0,0), legend.justification = c(0,0), legend.title = element_blank()) +
        xlab(paste("PC1 ", "(", round(scree_plot_data[1]*100, 0), "%)",sep = "")) +
        ylab(paste("PC3 ", "(", round(scree_plot_data[3]*100, 0), "%)",sep = ""))
      
      
      # summary stats
      # position mean metrics (not counting selected players)
      position.mean_w.o_selection <- df.na.omit.pos[!(rownames(df.na.omit.pos) %in% df.hull$player.index), c("success", input$pca_checkbox.metrics)]
      norm <- colMeans(position.mean_w.o_selection[-1])
      other.starters <- sum(position.mean_w.o_selection[, "success"])
      other.total <- nrow(position.mean_w.o_selection)
      other.prop <- other.starters / other.total
      
      # selection mean metrics
      selection.mean <- df.na.omit.pos[rownames(df.na.omit.pos) %in% df.hull$player.index, c("success",input$pca_checkbox.metrics)]
      select.starters <- sum(selection.mean[, "success"])
      select.total <- nrow(selection.mean)
      select.prop <- select.starters / select.total
      
      # others mean metrics
      others.mean <- df.na.omit.pos[!(rownames(df.na.omit.pos) %in% df.hull$player.index), c("success",input$pca_checkbox.metrics)]
      others.starters <- sum(selection.mean[, "success"])
      others.total <- nrow(selection.mean)
      others.prop <- select.starters / select.total
      
      
      # normalize values of selected
      selection.mean[, -1] <- mapply("/", selection.mean[, -1], as.vector(norm))
      selection.mean_summarized <- data.frame(
        "metric" = names(selection.mean[-1]), 
        "mean" = sapply(selection.mean[, -1], mean),
        "sd" = sapply(selection.mean[, -1], sd))
      selection.mean$group <- c("Highlighted area")
      
      others.mean[, -1] <- mapply("/", others.mean[, -1], as.vector(norm))
      others.mean_summarized <- data.frame(
        "metric" = names(others.mean[-1]), 
        "mean" = sapply(others.mean[, -1], mean),
        "sd" = sapply(others.mean[, -1], sd))
      others.mean$group <- c("Outside")
      
      PCA_boxplot.predata <- rbind.data.frame(others.mean, selection.mean)
      PCA_boxplot.data <- stack(PCA_boxplot.predata[, -c(1, ncol(PCA_boxplot.predata))])
      PCA_boxplot.data$group <- PCA_boxplot.predata$group
      PCA_boxplot.data$success <- PCA_boxplot.predata$success
      
      comp <- ggplot(PCA_boxplot.data, aes(y = ind, x = values, fill = factor(group))) +
        geom_density_ridges(alpha = 0.4) +
        theme_hindsight() +
        theme(legend.position = "top", legend.title = element_blank(), axis.title.y = element_blank(),axis.line.y = element_blank()) +
        xlab("Measurement (norm. to Outside-group average)") +
        scale_fill_manual(values = c(colors_hindsigt[1], "grey60")) +
        scale_y_discrete() +
        ggtitle(paste("Highlighted area: ",select.starters," of ",select.total," (",round(select.prop*100,0), "%)", " | ", "Outside: ",other.starters," of ",other.total," (",round(other.prop*100,0), "%)", sep = ""))
      
      # <--- prepare plots for PCA ####
      # scree plot
      df.scree_plot_data <- as.data.frame(scree_plot_data)
      scree_plot <- ggplot(df.scree_plot_data, aes(x = 1:nrow(df.scree_plot_data), y = scree_plot_data)) + 
        geom_bar(stat = "identity") +
        ggtitle(paste("Scree-plot", "|", paste(input$pca_checkbox.position, collapse = ", "))) +
        theme_hindsight() +
        scale_x_continuous(breaks = 1:9) +
        xlab("PCA Component") + 
        ylab("Prop. variance explained")
      
      plot_grid(scree_plot, PCA_12, PCA_13, comp, nrow = 2)
    })
    
    # comparisons ####
    output$comparisons_distplot <- renderPlot({
      
      # define max_rating & success variables
      data_nflhindsight$max_rating <- NA
      data_nflhindsight$success <- FALSE
      
      # convert madden ratings to numeric
      data_nflhindsight[,c("madden_4","madden_5")] <- sapply(data_nflhindsight[,c("madden_4","madden_5")], as.numeric)
      
      # find max rating in years 4 and 5
      for (i in 1:nrow(data_nflhindsight)) {
        data_nflhindsight$max_rating[i] <- max(data_nflhindsight[i,c("madden_4","madden_5")])
      }
      
      #  make true all success observations where max rating surpass rating threshold
      data_nflhindsight$success[data_nflhindsight$max_rating >= input$comparisons_threshold.rating] <- TRUE
      
      # prepare data
      plot.list <- lapply(metrics, function(x) {
        # load data and isolate non-NA players
        df.na.omit <- data_nflhindsight[, c("success", "position", x, "round")]
        df.na.omit[, -(1:2)] <- sapply(df.na.omit[,-(1:2)], as.numeric)
        df.na.omit <- as.data.frame(df.na.omit)
        df.na.omit <- na.omit(df.na.omit)
        
        # derive all position-specific comparisons
        df.metric.comparisons <- cbind.data.frame(df.na.omit$position, df.na.omit$success, df.na.omit[, x], df.na.omit$round)
        colnames(df.metric.comparisons) <- c("position","success", x, "round")
        df.position.input.comparisons <- df.metric.comparisons[df.metric.comparisons$position %in% input$comparisons_checkbox.position & df.metric.comparisons$round %in% input$comparisons_checkbox.draftp,]
        
        # test differences
        test <- t.test(df.position.input.comparisons[, x][df.position.input.comparisons$success == TRUE],
                       df.position.input.comparisons[, x][df.position.input.comparisons$success == FALSE],var.equal = FALSE
        )
        pvalue <- p.adjust(p = test$p.value, method = "bonferroni", n = 9)
        symbol.frame <- data.frame("alpha" = c(0.05, 0.01, 0.001, 0.0001),
                                   "symbol" = c("ns", "*", "**", "***"))
        
        symbol.score <- pvalue < symbol.frame$alpha
        symbol.score[1] <- TRUE
        symbol.index <- max(which(symbol.score))
        symbol <- symbol.frame$symbol[symbol.index]
        theme_comparisons <- list(theme_hindsight(), 
                                  theme(legend.position = "none"),
                                  scale_fill_manual(values = c("grey60",colors_hindsigt[3])),
                                  ylab("Gaussian kernal density"))
        p <- ggplot(df.position.input.comparisons) + 
          geom_density(aes(x = df.position.input.comparisons[, 3], fill = success), alpha = 0.4) +
          ggtitle(paste(x,"| ",paste(input$comparisons_checkbox.position, collapse = ", "))) +
          theme_comparisons +
          annotate("text", label = paste("sig.: ", symbol), x = -Inf, y = Inf, vjust = 1.15, hjust = -0.05) +
          xlab(names(metrics[metrics == x])) +
          theme(axis.text.x = element_text(margin = margin(r = 0)))
        return(p)
      })
      comparisons_plots <- plot_grid(plotlist =plot.list, align = "hv")
      
      
      comparisons_legend <- get_legend(
        plot.list[[1]] + 
          theme(legend.position = "bottom", legend.title = element_blank()) +
          scale_fill_manual(values = c("grey60",colors_hindsigt[3]), labels = c("<Threshold","\u2265Threshold"))
      )
      
      plot_grid(
        comparisons_plots,
        comparisons_legend,
        ncol = 1,
        align = "hv",
        rel_heights = c(0.92,0.08)
      )
    })
    # COMPARE YOURSELF ####
    output$plotCOMPARE <- renderPlot({
      ## comparison function
      pctl <- function(vector, value){
        
        out = sum(value >= vector)/ length(vector)
        
        return(out)
        
      }
      
      # prepare data
      
      # load data and isolate non-NA players
      df.na.omit <- data_nflhindsight[, c("position", input$compare.yourself.metric)]
      df.na.omit[, -(1)] <- sapply(df.na.omit[,-(1)], as.numeric)
      df.na.omit <- as.data.frame(df.na.omit)
      df.na.omit <- na.omit(df.na.omit)
      
      # 
      df.barplot <- data.frame(
        "position" = rep(NA, 9),
        "rank" = rep(NA, 9)
      )
      
      if(input$compare.yourself.metric %in% c("height","wt","arm","hand","vertical","broad_jump")) {
        value <- input$compare.yourself.metric.value * 0.393701 
      } else {
        value <- input$compare.yourself.metric.value
      }
      
      df.barplot[1,] <- c("QB", pctl(df.na.omit[df.na.omit$position == "QB", input$compare.yourself.metric],value)*100)
      df.barplot[2,] <- c("RB", pctl(df.na.omit[df.na.omit$position == "RB", input$compare.yourself.metric],value)*100)
      df.barplot[3,] <- c("WR", pctl(df.na.omit[df.na.omit$position == "WR", input$compare.yourself.metric],value)*100)
      df.barplot[4,] <- c("TE", pctl(df.na.omit[df.na.omit$position == "TE", input$compare.yourself.metric],value)*100)
      df.barplot[5,] <- c("OL", pctl(df.na.omit[df.na.omit$position == "OL", input$compare.yourself.metric],value)*100)
      df.barplot[6,] <- c("DT", pctl(df.na.omit[df.na.omit$position == "DT", input$compare.yourself.metric],value)*100)
      df.barplot[7,] <- c("DE", pctl(df.na.omit[df.na.omit$position == "DE", input$compare.yourself.metric],value)*100)
      df.barplot[8,] <- c("LB", pctl(df.na.omit[df.na.omit$position == "LB", input$compare.yourself.metric],value)*100)
      df.barplot[9,] <- c("DB", pctl(df.na.omit[df.na.omit$position == "DB", input$compare.yourself.metric],value)*100)
      
      df.barplot[, "rank"] <- sapply(df.barplot[, "rank"], as.numeric)
      df.barplot$position <- factor(df.barplot$position, levels = df.barplot$position)
      
      p <- ggplot(df.barplot) + 
        geom_bar(aes(x = position, y = rank), stat = "identity", fill = "#0066ff", width = 0.5) +
        theme_hindsight() +
        ylab("Percentile rank") +
        xlab("Position") +
        geom_text(aes(x = position, y = rank, label = paste(round(rank, 0), "th", sep = "")), vjust = -1.5, color = "black") +
        ggtitle("How you rank among NFL players:") +
        scale_y_continuous(breaks = seq(0,100,20), limits = c(0, 110))
      print(p)
    })
  }
  
  # app view ####  
  shinyApp(ui = ui, server = server)
}
