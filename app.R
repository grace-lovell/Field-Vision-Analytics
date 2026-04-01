library(tidyverse)
library(shiny)
library(DT)
library(plotly)

ui <- fluidPage(
  # -------------------------
  # CONNECTION TO CSS
  # -------------------------  
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Barlow+Condensed:wght@400;600;700;800;900&family=Barlow:wght@300;400;500;600&display=swap"),
    tags$link(rel = "stylesheet", href = "styles.css?v=2")
  ),
  
  uiOutput("pageContent")
)

server <- function(input, output, session) {
  
  showDashboard <- reactiveVal(FALSE)
  clearCount <- reactiveVal(0)
  filesCleared <- reactiveVal(FALSE)
  
  fileCount <- reactive({
    clearCount()
    if (filesCleared()) return(0)
    sum(
      !is.null(input$passing_file),
      !is.null(input$rushing_receiving_file),
      !is.null(input$defense_file),
      !is.null(input$special_file),
      !is.null(input$roster_file),
      !is.null(input$schedule_file)
    )
  })
  
  output$uploadCount <- renderText({
    fileCount()
  })
  
  output$counterText <- renderText({
    if (fileCount() == 0) {
      "No files uploaded yet"
    } else {
      paste(fileCount(), "file(s) uploaded")
    }
  })
  
  observeEvent(input$launch_dashboard, {
    if (fileCount() == 0) {
      showNotification("Please upload a file before continuing.", type = "error")
    } else {
      showDashboard(TRUE)
    }
  })
  
  observeEvent(input$clearAllBtn, {
    if (fileCount() == 0) {
      showNotification("There are no files to clear.", type = "warning")
    } else {
      clearCount(clearCount() + 1)
      filesCleared(TRUE)
      showNotification("All files have been cleared.", type = "warning")
    }
  })
  
  observeEvent(input$passing_file, { filesCleared(FALSE) })
  observeEvent(input$rushing_receiving_file, { filesCleared(FALSE) })
  observeEvent(input$defense_file, { filesCleared(FALSE) })
  observeEvent(input$special_file, { filesCleared(FALSE) })
  observeEvent(input$roster_file, { filesCleared(FALSE) })
  observeEvent(input$schedule_file, { filesCleared(FALSE) })
  
  observeEvent(input$help_content, {
    showModal(modalDialog(
      title = NULL,
      tags$h4("User Manual & CSV Templates", 
              style = "color: var(--team-primary); font-family: 'Barlow Condensed', sans-serif; font-weight: 700;"),
      tags$p("Instructions on how to upload data and use the visualizations:"),
      tags$ul(
        tags$li(tags$a("Download User Manual", href = "Field Vision Analytics - User Manual.pdf", download = NA)),
        tags$li(tags$a("Download Passing Template", href = "Field Vision Analytics Templates - Passing.csv", download = NA)),
        tags$li(tags$a("Download Rushing/Receiving Template", href = "Field Vision Analytics Templates - Rushing & Receiving.csv", download = NA)),
        tags$li(tags$a("Download Defense Template", href = "Field Vision Analytics Templates - Defense.csv", download = NA)),
        tags$li(tags$a("Download Special Teams Template", href = "Field Vision Analytics Templates - Special Teams.csv", download = NA)),
        tags$li(tags$a("Download Roster Template", href = "Field Vision Analytics Templates - Roster.csv", download = NA)),
        tags$li(tags$a("Download Schedule Template", href = "Field Vision Analytics Templates - Schedule.csv", download = NA))
      ),
      easyClose = TRUE,
      footer = modalButton("Close"),
      class = "help-modal"
    ))
  })
  
  output$uploadGrid <- renderUI({
    clearCount()
    
    # ------------------------
    # UPLOAD FILES GRID
    # ------------------------
    tags$div(class = "upload-grid",
      # ------------------------
      # OFFENSE - PASSING UPLOAD
      # ------------------------
      tags$div(class = "upload-zone offense",
        tags$div(class = "zone-header",
          tags$div(class = "zone-icon", "🏈"),
          tags$div(class = "zone-meta",
            tags$div(class = "zone-title", "Passing"),
            tags$div(class = "zone-desc", "Passing Yards, Touchdowns, and Interceptions")
          )
        ),
        tags$div(class = "drop-area",
          fileInput("passing_file", NULL, accept = ".csv",
                    buttonLabel = "📂  Choose File", placeholder = NULL),
          textOutput("passing_filename")
        )
      ),
      # ------------------------
      # OFFENSE - RUSHING & RECEIVING UPLOAD
      # ------------------------
      tags$div(class = "upload-zone offense",
        tags$div(class = "zone-header",
          tags$div(class = "zone-icon", "🏃"),
          tags$div(class = "zone-meta",
            tags$div(class = "zone-title", "Rushing & Receiving"),
            tags$div(class = "zone-desc", "Rushing & Receiving Yards, Touchdowns, and Fumbles")
          )
        ),
        tags$div(class = "drop-area",
          fileInput("rushing_receiving_file", NULL, accept = ".csv",
                    buttonLabel = "📂  Choose File", placeholder = NULL),
          textOutput("rushing_receiving_filename")
        )
      ),
      # ------------------------
      # DEFENSE UPLOAD
      # ------------------------
      tags$div(class = "upload-zone defense",
        tags$div(class = "zone-header",
          tags$div(class = "zone-icon", "🛡️"),
          tags$div(class = "zone-meta",
            tags$div(class = "zone-title", "Defensive Stats"),
            tags$div(class = "zone-desc", "Tackles, Sacks, Interceptions, and Forced Fumbles")
          )
        ),
        tags$div(class = "drop-area",
          fileInput("defense_file", NULL, accept = ".csv",
                    buttonLabel = "📂  Choose File", placeholder = NULL),
          textOutput("defense_filename")
        )
      ),
      # ------------------------
      # SPECIAL TEAMS UPLOAD
      # ------------------------
      tags$div(class = "upload-zone special",
        tags$div(class = "zone-header",
          tags$div(class = "zone-icon", "🦵️"),
          tags$div(class = "zone-meta",
            tags$div(class = "zone-title", "Special Teams"),
            tags$div(class = "zone-desc", "Field Goal Percentage, Returns, and Yardage")
          )
        ),
        tags$div(class = "drop-area",
          fileInput("special_file", NULL, accept = ".csv",
                    buttonLabel = "📂  Choose File", placeholder = NULL),
          textOutput("special_filename")
        )
      ),
      # ------------------------
      # ROSTER UPLOAD
      # ------------------------
      tags$div(class = "upload-zone roster",
        tags$div(class = "zone-header",
          tags$div(class = "zone-icon", "📋️"),
          tags$div(class = "zone-meta",
            tags$div(class = "zone-title", "Roster"),
            tags$div(class = "zone-desc", "Player Stats")
          )
        ),
        tags$div(class = "drop-area",
          fileInput("roster_file", NULL, accept = ".csv",
                    buttonLabel = "📂  Choose File", placeholder = NULL),
          textOutput("roster_filename")
        )
      ),
      # ------------------------
      # SCHEDULE UPLOAD
      # ------------------------
      tags$div(class = "upload-zone schedule",
        tags$div(class = "zone-header",
          tags$div(class = "zone-icon", "🗓️️"),
          tags$div(class = "zone-meta",
            tags$div(class = "zone-title", "Schedule"),
            tags$div(class = "zone-desc", "Game Schedule")
          )
        ),
        tags$div(class = "drop-area",
          fileInput("schedule_file", NULL, accept = ".csv",
                    buttonLabel = "📂  Choose File", placeholder = NULL),
          textOutput("schedule_filename")
        )
      )
    )
  })
  
  observeEvent(input$back_to_upload, {
    showDashboard(FALSE)
  })
  
  output$passing_filename <- renderText({
    if (filesCleared()) return(NULL)
    req(input$passing_file)
    paste("✓", input$passing_file$name)
  })
  
  output$rushing_receiving_filename <- renderText({
    if (filesCleared()) return(NULL)
    req(input$rushing_receiving_file)
    paste("✓", input$rushing_receiving_file$name)
  })
  
  output$defense_filename <- renderText({
    if (filesCleared()) return(NULL)
    req(input$defense_file)
    paste("✓", input$defense_file$name)
  })
  
  output$special_filename <- renderText({
    if (filesCleared()) return(NULL)
    req(input$special_file)
    paste("✓", input$special_file$name)
  })
  
  output$roster_filename <- renderText({
    if (filesCleared()) return(NULL)
    req(input$roster_file)
    paste("✓", input$roster_file$name)
  })
  
  output$schedule_filename <- renderText({
    if (filesCleared()) return(NULL)
    req(input$schedule_file)
    paste("✓", input$schedule_file$name)
  })
  
  # -------------------------
  # OVERVIEW VISUALS
  # -------------------------
  visualUnit <- reactiveVal("Points For vs. Points Against")
  observeEvent(input$visual_next, {
    current <- visualUnit()
    visualUnit(switch(current, 
      "Points For vs. Points Against" = "Score Margin",
      "Score Margin" = "Offensive Balance Chart",
      "Offensive Balance Chart" = "Defensive Balance Chart",
      "Defensive Balance Chart" = "Points For vs. Points Against"
    ))
  })
  
  observeEvent(input$visual_prev, {
    current <- visualUnit()
    visualUnit(switch(current,
      "Points For vs. Points Against" = "Defensive Balance Chart",
      "Score Margin" = "Points For vs. Points Against",
      "Offensive Balance Chart" = "Score Margin",
      "Defensive Balance Chart" = "Offensive Balance Chart"
    ))
  })
  
  output$visuals <- renderUI({
    unit <- visualUnit()
    tags$div(
      tags$div(class = "leaders-header",
               actionButton("visual_prev", "←", class = "btn btn-ghost"),
               tags$div(class = "leaders-unit-title", unit),
               actionButton("visual_next", "→", class = "btn btn-ghost")
      ),
      plotlyOutput("active_visual", height = "1000px")
    )
  })
  
  output$active_visual <- renderPlotly({
    unit <- visualUnit()
    
    theme_fva <- function() {
      theme_minimal() +
        theme(
          plot.background   = element_rect(fill = "transparent", color = NA),
          panel.background  = element_rect(fill = "transparent", color = NA),
          panel.grid.major  = element_line(color = alpha("#f0f4ff", 0.08)),
          panel.grid.minor  = element_blank(),
          axis.text         = element_text(color = "#8a96b0", size = 10),
          axis.title        = element_text(color = "#8a96b0", size = 11),
          legend.text       = element_text(color = "#8a96b0"),
          legend.background = element_rect(fill = "transparent", color = NA),
          legend.position   = "top"
        )
    }
    
    plot <- if (unit == "Points For vs. Points Against") {
      req(input$schedule_file)
      df <- read.csv(input$schedule_file$datapath)
      ggplot(df, aes(x = Points.For, y = Points.Against, label = Opponent)) +
        geom_point(color = "#1e90ff", size = 3) +
        geom_text(nudge_x = 1.5, size = 6, color = "#f0f4ff", fontface = "bold") +
        labs(x = "Points For", y = "Points Against") +
        theme_fva()
      
    } else if (unit == "Score Margin") {
      req(input$schedule_file)
      df <- read.csv(input$schedule_file$datapath)
      df <- df %>%
        filter(!is.na(Points.For) & !is.na(Points.Against)) %>%
        mutate(
          Margin = Points.For - Points.Against,
          Result_Color = ifelse(Margin >= 0, "Win", "Loss"),
          label = ifelse(Margin >= 0, paste0("+", Margin), Margin),
          label_y = Margin / 2
        )
      
      ggplot(df, aes(x = Week, y = Margin, fill = Result_Color)) +
        geom_col() +
        geom_hline(yintercept = 0, linewidth = 0.5) +
        geom_text(
          aes(y = label_y, label = label),  # Use label_y to center
          size = 6,
          color = "#1a3a5c",
          fontface = "bold"
        ) +
        scale_fill_manual(values = c("Win" = "lightgreen", "Loss" = "#c0392b")) +
        scale_x_continuous(breaks = df$Week) +
        labs(x = "Week", y = "Point Margin", fill = NULL) +
        theme_fva()
      
    } else if (unit == "Offensive Balance Chart") {
      req(input$rushing_receiving_file)
      df <- read.csv(input$rushing_receiving_file$datapath)
      
      total_rush <- sum(df$Rushing.Yards, na.rm = TRUE)
      total_receive <- sum(df$Reception.Yards, na.rm = TRUE)
      
      total_df <- data.frame(
        Category = c("Rushing", "Receiving"),
        Yards = c(total_rush, total_receive)
      )
      
      ggplot(total_df, aes(x = "Offense", y = Yards, fill = Category)) +
        geom_col(width = 0.5) +
        geom_text(aes(label = paste0(Category, "\n", format(Yards, big.mark = ","), " yds")),
                  position = position_stack(vjust = 0.5),
                  color = "#1a3a5c", size = 6, fontface = "bold") +
        scale_fill_manual(values = c(
          "Rushing" = "#e8a020",
          "Receiving" = "#1e90ff"
        )) +
        coord_flip() +
        labs(x = NULL, y = "Yards", fill = NULL) +
        theme_fva() +
        theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

    } else if (unit == "Defensive Balance Chart") {
      req(input$defense_file)
      df <- read.csv(input$defense_file$datapath)
      
      df <- df %>%
        mutate(Group = case_when(
          Pos %in% c("DL", "DT", "DE", "DG", "NT", "MG", "LDT", "RDT", "LE", "RE", "LDE", "RDE") ~ "D-Line",
          Pos %in% c("LB", "OLB", "ILB", "MLB", "LLB", "RLB", "WILL", "MIKE", "SAM", "LOLB", "LILB", "ROLB", "RILB", "SLB", "WLB", "RUSH") ~ "Linebackers",
          Pos %in% c("DB", "CB", "S", "SS", "FS", "LCB", "RCB", "RS", "LDH", "RDH") ~ "Defensive Backs",
          TRUE ~ "Other"
        ))
      
      grouped_df <- df %>%
        group_by(Group) %>%
        summarise(
          Tackles = sum(Combined.Tackles, na.rm = TRUE),
          Sacks = sum(Sacks, na.rm = TRUE),
          Interceptions = sum(Interceptions, na.rm = TRUE),
          ForcedFumbles = sum(Forced.Fumbles, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        pivot_longer(cols = c(Tackles, Sacks, Interceptions, ForcedFumbles),
                     names_to = "Stat",
                     values_to = "Value")
      
      ggplot(grouped_df, aes(x = Group, y = Value, fill = Stat)) +
        geom_col(position = "stack", width = 0.6) +
        geom_text(aes(label = ifelse(Value >= 10, Value, "")),
                  position = position_stack(vjust = 0.5),
                  color = "#1a3a5c", size = 6, fontface = "bold") +
        scale_fill_manual(values = c(
          "Tackles" = "#e8a020",
          "Sacks" = "#1e90ff",
          "Interceptions" = "#c0392b",
          "ForcedFumbles" = "lightgreen"
        )) +
        labs(x = NULL, y = "Count", fill = NULL) +
        theme_fva()
    }
    ggplotly(plot, tooltip = "all") %>%
      layout(
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        font = list(color = "#8a96b0"),
        legend = list(font = list(color = "#8a96b0"))
      )
  })
  
  # -------------------------
  # OVERVIEW LEADERS
  # -------------------------
  leaderUnit <- reactiveVal("Offense")
  observeEvent(input$leader_next, {
    current <- leaderUnit()
    leaderUnit(switch(current,
      "Offense" = "Defense",
      "Defense" = "Special Teams",
      "Special Teams" = "Offense"
    ))
  })
  
  observeEvent(input$leader_prev, {
    current <- leaderUnit()
    leaderUnit(switch(current,
      "Offense" = "Special Teams",
      "Defense" = "Offense",
      "Special Teams" = "Defense"
    ))
  })
  
  output$team_leaders <- renderUI({
    unit <- leaderUnit()

    if (unit == "Offense") {
      req(input$passing_file, input$rushing_receiving_file)
      pass <- read.csv(input$passing_file$datapath)
      rush <- read.csv(input$rushing_receiving_file$datapath)
      top_passer <- pass[which.max(pass$Yards), ]
      top_rusher <- rush[which.max(rush$Rushing.Yards), ]
      top_receiver <- rush[which.max(rush$Reception.Yards), ]
      leaders <- list(
        list(label = "Passing Yards", player = top_passer$Player, value = top_passer$Yards),
        list(label = "Rushing Yards", player = top_rusher$Player, value = top_rusher$Rushing.Yards),
        list(label = "Receiving Yards", player = top_receiver$Player, value = top_receiver$Reception.Yards)
      )
    } else if (unit == "Defense") {
      req(input$defense_file)
      def <- read.csv(input$defense_file$datapath)
      max_tackles <- max(def$Combined.Tackles, na.rm = TRUE)
      top_tacklers <- def[def$Combined.Tackles == max_tackles, ]
      top_tackler <- if (nrow(top_tacklers) > 1) top_tacklers[which.max(top_tacklers$Forced.Fumbles), ] else top_tacklers
      max_sacks <- max(def$Sacks, na.rm = TRUE)
      top_sackers <- def[def$Sacks == max_sacks, ]
      top_sacker <- if (nrow(top_sackers) > 1) top_sackers[which.max(top_sackers$Forced.Fumbles), ] else top_sackers
      max_ints <- max(def$Interceptions, na.rm = TRUE)
      top_intercepters <- def[def$Interceptions == max_ints, ]
      top_int <- if (nrow(top_intercepters) > 1) top_intercepters[which.max(top_intercepters$Combined.Tackles), ] else top_intercepters
      # top_tackler <- def[which.max(def$Combined.Tackles), ]
      # top_sacker <- def[which.max(def$Sacks), ]
      # top_int <- def[which.max(def$Interceptions), ]
      leaders <- list(
        list(label = "Tackles", player = top_tackler$Player, value = top_tackler$Combined.Tackles),
        list(label = "Sacks", player = top_sacker$Player, value = top_sacker$Sacks),
        list(label = "Interceptions", player = top_int$Player, value = top_int$Interceptions)
      )
    } else {
      req(input$special_file)
      special <- read.csv(input$special_file$datapath)
      kickers <- special[special$Pos %in% c("K"), ]
      punters <- special[special$Pos %in% c("P"), ]
      returners <- special[special$Pos %in% c("KR", "PR", "RET", "WR", "RB"), ]
      top_kicker <- if (nrow(kickers) > 0) kickers[which.max(kickers$Field.Goals.Made), ] else special[which.max(special$Field.Goals.Made), ]
      top_punter <- if (nrow(punters) > 0) punters[which.max(punters$Punt.Yardage), ] else special[which.max(special$Punt.Yardage), ]
      top_kreturner <- if(nrow(returners) > 0) returners[which.max(returners$Kickoff.Return.Yardage), ] else special[which.max(special$Kickoff.Return.Yardage), ]
      top_preturner <- if(nrow(returners) > 0) returners[which.max(returners$Punt.Return.Yardage), ] else special[which.max(special$Punt.Return.Yardage), ]
      leaders <- list(
        list(label = "Kicker", player = top_kicker$Player, value = top_kicker$Field.Goals.Made),
        list(label = "Punter", player = top_punter$Player, value = top_punter$Punt.Yardage),
        list(label = "Kick Returner", player = top_kreturner$Player, value = top_kreturner$Kickoff.Return.Yardage),
        list(label = "Punt Returner", player = top_preturner$Player, value = top_preturner$Punt.Return.Yardage)
      )
    }

    tags$div(
      tags$div(class = "leaders-header",
        actionButton("leader_prev", "←", class = "btn btn-ghost"),
        tags$div(class = "leaders-unit-title", unit),
        actionButton("leader_next", "→", class = "btn btn-ghost")
      ),
      tags$div(
        lapply(leaders, function(l) {
          tags$div(class = "leader-row",
            tags$div(
              tags$div(class = "leader-label", l$label),
              tags$div(class = "leader-name",  as.character(l$player))
            ),
            tags$div(class = "leader-value",
              format(as.numeric(l$value), big.mark = ",")
            )
          )
        })
      )
    )
  })
  
  # -------------------------
  # OVERVIEW STAT ROW
  # -------------------------
  output$record <- renderUI({
    req(input$schedule_file)
    df <- read.csv(input$schedule_file$datapath)
    wins <- sum(df$Result == "Win", na.rm = TRUE)
    losses <- sum(df$Result == "Loss", na.rm = TRUE)
    ties <- sum(df$Result == "Tie", na.rm = TRUE)
    record <- if (ties > 0) {
                paste0(wins, "-", losses, "-", ties)
              } else {
                paste0(wins, "-", losses)
              }
    tags$span(record)
  })
  
  output$pts_per_game <- renderUI({
    req(input$schedule_file)
    df <- read.csv(input$schedule_file$datapath)
    avg <- round(mean(df$Points.For, na.rm = TRUE), 1)
    tags$span(paste(format(avg, nsmall = 1)))
  })
  
  output$pts_diff <- renderUI({
    req(input$schedule_file)
    df <- read.csv(input$schedule_file$datapath)
    points_for <- sum(df$Points.For, na.rm = TRUE)
    points_against <- sum(df$Points.Against, na.rm = TRUE)
    diff <- points_for - points_against
    sign <- if (diff >= 0) "+" else ""
    tags$span(paste0(sign, diff))
  })
  
  output$turnover_margin <- renderUI({
    req(input$passing_file)
    req(input$rushing_receiving_file)
    req(input$defense_file)
    pass <- read.csv(input$passing_file$datapath)
    rush <- read.csv(input$rushing_receiving_file$datapath)
    def <- read.csv(input$defense_file$datapath)
    ints_thrown <- sum(pass$Interceptions, na.rm = TRUE)
    fumbles_lost <- sum(rush$Fumbles, na.rm = TRUE)
    ints_caught <- sum(def$Interceptions, na.rm = TRUE)
    fumbles_recovered <- sum(def$Fumbles.Recovered, na.rm = TRUE)
    turnovers <- ints_thrown + fumbles_lost
    takeaways <- ints_caught + fumbles_recovered
    margin <- takeaways - turnovers
    sign <- if (margin >= 0) "+" else ""
    tags$span(paste0(sign, margin))
  })
  
  output$sack_diff <- renderUI({
    req(input$passing_file)
    req(input$defense_file)
    pass <- read.csv(input$passing_file$datapath)
    def <- read.csv(input$defense_file$datapath)
    off_sacks <- sum(pass$Sacks, na.rm = TRUE)
    def_sacks <- sum(def$Sacks, na.rm = TRUE)
    diff <- def_sacks - off_sacks
    sign <- if (diff >= 0) "+" else ""
    tags$span(paste0(sign, diff))
  })
  
  # -------------------------
  # OFFENSE STAT ROW
  # -------------------------
  output$total_pass_yards <- renderUI({
    req(input$passing_file)
    df <- read.csv(input$passing_file$datapath)
    total <- sum(df$Yards, na.rm = TRUE)
    tags$span(format(total, big.mark = ","))
  })
  
  output$total_rush_yards <- renderUI({
    req(input$rushing_receiving_file)
    df <- read.csv(input$rushing_receiving_file$datapath)
    total <- sum(df$Rushing.Yards, na.rm = TRUE)
    tags$span(format(total, big.mark = ","))
  })
  
  output$total_receive_yards <- renderUI({
    req(input$rushing_receiving_file)
    df <- read.csv(input$rushing_receiving_file$datapath)
    total <- sum(df$Reception.Yards, na.rm = TRUE)
    tags$span(format(total, big.mark = ","))
  })
  
  output$total_pass_tds <- renderUI({
    req(input$passing_file)
    df <- read.csv(input$passing_file$datapath)
    total <- sum(df$Touchdowns, na.rm = TRUE)
    tags$span(format(total, big.mark = ","))
  })
  
  output$total_rush_tds <- renderUI({
    req(input$rushing_receiving_file)
    df <- read.csv(input$rushing_receiving_file$datapath)
    total <- sum(df$Rushing.Touchdowns, na.rm = TRUE)
    tags$span(format(total, big.mark = ","))
  })
  
  output$total_receive_tds <- renderUI({
    req(input$rushing_receiving_file)
    df <- read.csv(input$rushing_receiving_file$datapath)
    total <- sum(df$Receiving.Touchdowns, na.rm = TRUE)
    tags$span(format(total, big.mark = ","))
  })
  
  # -------------------------
  # DEFENSE STAT ROW
  # -------------------------
  output$total_sacks <- renderUI({
    req(input$defense_file)
    df <- read.csv(input$defense_file$datapath)
    total <- sum(df$Sacks, na.rm = TRUE)
    tags$span(format(total, big.mark = ","))
  })
  
  output$total_tfls <- renderUI({
    req(input$defense_file)
    df <- read.csv(input$defense_file$datapath)
    total <- sum(df$Tackles.For.Loss, na.rm = TRUE)
    tags$span(format(total, big.mark = ","))
  })
  
  output$total_qb_hits <- renderUI({
    req(input$defense_file)
    df <- read.csv(input$defense_file$datapath)
    total <- sum(df$QB.Hits, na.rm = TRUE)
    tags$span(format(total, big.mark = ","))
  })
  
  output$total_ints <- renderUI({
    req(input$defense_file)
    df <- read.csv(input$defense_file$datapath)
    total <- sum(df$Interceptions, na.rm = TRUE)
    tags$span(format(total, big.mark = ","))
  })
  
  output$total_forced_fumbles <- renderUI({
    req(input$defense_file)
    df <- read.csv(input$defense_file$datapath)
    total <- sum(df$Forced.Fumbles, na.rm = TRUE)
    tags$span(format(total, big.mark = ","))
  })
  
  # -------------------------
  # SPECIAL TEAMS STAT ROW
  # -------------------------
  
  output$avg_kick_return <- renderUI({
    req(input$special_file)
    df <- read.csv(input$special_file$datapath)
    total_returns <- sum(df$Kickoff.Returns, na.rm = TRUE)
    total_yards <- sum(df$Kickoff.Return.Yardage, na.rm = TRUE)
    avg <- round(total_yards / total_returns, 1)
    tags$span(paste(format(avg, nsmall = 1), "Yds"))
  })
  
  output$avg_punt_return <- renderUI({
    req(input$special_file)
    df <- read.csv(input$special_file$datapath)
    total_returns <- sum(df$Punt.Returns, na.rm = TRUE)
    total_yards <- sum(df$Punt.Return.Yardage, na.rm = TRUE)
    avg <- round(total_yards / total_returns, 1)
    tags$span(paste(format(avg, nsmall = 1), "Yds"))
  })
  
  output$max_tb_perc <- renderUI({
    req(input$special_file)
    df <- read.csv(input$special_file$datapath)
    max <- max(df$Touchback.Perc, na.rm = TRUE)
    tags$span(paste(format(max, nsmall = 2), "%"))
  })
  
  # -------------------------
  # ROSTER STAT ROW
  # -------------------------
  output$avg_age <- renderUI({
    req(input$roster_file)
    df <- read.csv(input$roster_file$datapath)
    avg <- round(mean(df$Age, na.rm = TRUE), 1)
    tags$span(paste(format(avg, nsmall = 1)))
  })
  
  output$avg_years <- renderUI({
    req(input$roster_file)
    df <- read.csv(input$roster_file$datapath)
    df <- df %>%
      mutate(Years = as.numeric(ifelse(Years == "Rook", 0, Years)))
    avg <- round(mean(df$Years, na.rm = TRUE), 1)
    tags$span(as.character(avg))
  })
  
  # -------------------------
  # PASSING TABLE
  # -------------------------
  output$passing_table <- renderDT({
    req(input$passing_file)
    df <- read.csv(input$passing_file$datapath)
    
    if(!is.null(input$search_passing) && input$search_passing != "") {
      mask <- apply(df, 1, function(row) {
        any(str_detect(as.character(row), regex(input$search_passing, ignore_case = TRUE)))
      })
      df <- df[mask, ]
    }
    
    if (!is.null(input$filter_pos_passing) && !"All" %in% input$filter_pos_passing && length(input$filter_pos_passing) > 0) {
      df <- df[df$Pos %in% input$filter_pos_passing, ]
    }
    
    df <- cbind("Row #" = 1:nrow(df), df)
    
    df
  }, options = list(paging = FALSE, scrollX = TRUE, scrollY = "800px"), rownames = FALSE)
  
  # -------------------------
  # RUSHING & RECEIVING TABLE
  # -------------------------
  output$rush_receive_table <- renderDT({
    req(input$rushing_receiving_file)
    df <- read.csv(input$rushing_receiving_file$datapath)
    
    if(!is.null(input$search_rush_receive) && input$search_rush_receive != "") {
      mask <- apply(df, 1, function(row) {
        any(str_detect(as.character(row), regex(input$search_rush_receive, ignore_case = TRUE)))
      })
      df <- df[mask, ]
    }
    
    if (!is.null(input$filter_pos_rush_receive) && !"All" %in% input$filter_pos_rush_receive && length(input$filter_pos_rush_receive) > 0) {
      df <- df[df$Pos %in% input$filter_pos_rush_receive, ]
    }
    
    rush_cols <- c("Player", "Age", "Pos", "Games.Played", "Games.Started", 
                   "Rush.Attempts", "Rushing.Yards", "Rushing.Touchdowns", "Rushing.FirstDowns", "Rushing.SuccessRate")
    receive_cols <- c("Player", "Age", "Pos", "Games.Played", "Games.Started",
                      "Targets", "Receptions", "Reception.Yards", "Receiving.Touchdowns", "Receiving.FirstDowns", "Receiving.SuccessRate")
    if (is.null(input$filter_stat_offense) || length(input$filter_stat_offense) == 0) {
      selected_cols <- names(df)
    } else {
      selected_cols <- c("Player", "Age", "Pos", "Games.Played", "Games.Started")
      if ("Rushing" %in% input$filter_stat_offense) {
        selected_cols <- union(selected_cols, rush_cols)
      }
      if ("Receiving" %in% input$filter_stat_offense) {
        selected_cols <- union(selected_cols, receive_cols)
      }
      selected_cols <- selected_cols[selected_cols %in% names(df)]
    }
    df <- df[, selected_cols, drop = FALSE]
    
    df <- cbind("Row #" = 1:nrow(df), df)
    
    df
  }, options = list(paging = FALSE, scrollX = TRUE, scrollY = "800px"), rownames = FALSE)
  
  # -------------------------
  # DEFENSE TABLE
  # -------------------------
  output$defense_table <- renderDT({
    req(input$defense_file)
    df <- read.csv(input$defense_file$datapath)
    
    if(!is.null(input$search_defense) && input$search_defense != "") {
      mask <- apply(df, 1, function(row) {
        any(str_detect(as.character(row), regex(input$search_defense, ignore_case = TRUE)))
      })
      df <- df[mask, ]
    }
    
    if (!is.null(input$filter_pos_defense) && !"All" %in% input$filter_pos_defense && length(input$filter_pos_defense) > 0) {
      df <- df[df$Pos %in% input$filter_pos_defense, ]
    }
    
    if (!is.null(input$filter_pos_group_defense) && !"All" %in% input$filter_pos_group_defense && length(input$filter_pos_group_defense) > 0) {
      get_pos <- list(
        "D-Line" = c("DL", "DT", "DE", "DG", "NT", "MG", "LDT", "RDT", "LE", "RE", "LDE", "RDE"),
        "Linebackers" = c("LB", "OLB", "ILB", "MLB", "LLB", "RLB", "WILL", "MIKE", "SAM", "LOLB", "LILB", "ROLB", "RILB", "SLB", "WLB", "RUSH"),
        "Defensive Backs" = c("DB", "CB", "S", "SS", "FS", "LCB", "RCB", "RS", "LDH", "RDH"),
        "Other" = c("QB", "RB", "HB", "TB", "FB", "LH", "RH", "BB", "B", "WB", "WR", "FL", "SE", "E", "TE", "LT", "LOT", "T", "LG", "G", "C", "RG", "RT", "ROT", "LS", "K", "P", "PR", "KR", "RET")
      )
      allowed_pos <- unlist(get_pos[input$filter_pos_group_defense])
      df <- df[df$Pos %in% allowed_pos, ]
    }
    
    interception_cols <- c("Player", "Age", "Pos", "Games.Played", "Games.Started", 
                      "Interceptions", "IntYards", "Interception.Touchdowns", "Passes.Defended")
    fumble_cols <- c("Player", "Age", "Pos", "Games.Played", "Games.Started",
                      "Forced.Fumbles", "Fumbles", "Fumbles.Recoverd", "Fumble.Yards", "Fumbles.Resulted.TD")
    tackle_cols <- c("Player", "Age", "Pos", "Games.Played", "Games.Started",
                      "Sacks", "Combined.Tackles", "Solo.Tackles", "Assisted.Tackles", "Tackles.For.Loss", "QB.Hits", "Safeties")
    if (is.null(input$filter_stat_defense) || length(input$filter_stat_defense) == 0) {
      selected_cols <- names(df)
    } else {
      selected_cols <- c("Player", "Age", "Pos", "Games.Played", "Games.Started")
      if ("Interceptions" %in% input$filter_stat_defense) {
        selected_cols <- union(selected_cols, interception_cols)
      }
      if ("Fumbles" %in% input$filter_stat_defense) {
        selected_cols <- union(selected_cols, fumble_cols)
      }
      if ("Tackles" %in% input$filter_stat_defense) {
        selected_cols <- union(selected_cols, tackle_cols)
      }
      selected_cols <- selected_cols[selected_cols %in% names(df)]
    }
    df <- df[, selected_cols, drop = FALSE]
    
    df <- cbind("Row #" = 1:nrow(df), df)
    
    df
  }, options = list(paging = FALSE, scrollX = TRUE, scrollY = "800px"), rownames = FALSE)

  # -------------------------
  # SPECIAL TEAMS TABLE
  # -------------------------  
  output$special_table <- renderDT({
    req(input$special_file)
    df <- read.csv(input$special_file$datapath)
    
    if(!is.null(input$search_special) && input$search_special != "") {
      mask <- apply(df, 1, function(row) {
        any(str_detect(as.character(row), regex(input$search_special, ignore_case = TRUE)))
      })
      df <- df[mask, ]
    }
    
    if (!is.null(input$filter_pos_special) && !"All" %in% input$filter_pos_special && length(input$filter_pos_special) > 0) {
      df <- df[df$Pos %in% input$filter_pos_special, ]
    }
    
    kicking_cols <- c("Player", "Age", "Pos", "Games.Played", "Games.Started", 
                      "Field.Goals.Att", "Field.Goals.Made", "Field.Goal.Perc", 
                      "Extra.Points.Att", "Extra.Points.Made", "Extra.Point.Perc",
                      "Kickoffs", "Kickoff.Yards", "Touchbacks", "Touchback.Perc")
    punting_cols <- c("Player", "Age", "Pos", "Games.Played", "Games.Started",
                      "Punts", "Punt.Yardage")
    returning_cols <- c("Player", "Age", "Pos", "Games.Played", "Games.Started",
                        "Punt.Returns", "Punt.Return.Yardage", "Punt.Return.for.TD",
                        "Kickoff.Returns", "Kickoff.Return.Yardage", "Kickoff.Return.for.TD")
    if (is.null(input$filter_stat_special) || length(input$filter_stat_special) == 0) {
      selected_cols <- names(df)
    } else {
      selected_cols <- c("Player", "Age", "Pos", "Games.Played", "Games.Started")
      if ("Kicking" %in% input$filter_stat_special) {
        selected_cols <- union(selected_cols, kicking_cols)
      }
      if ("Punting" %in% input$filter_stat_special) {
        selected_cols <- union(selected_cols, punting_cols)
      }
      if ("Returning" %in% input$filter_stat_special) {
        selected_cols <- union(selected_cols, returning_cols)
      }
      selected_cols <- selected_cols[selected_cols %in% names(df)]
    }
    df <- df[, selected_cols, drop = FALSE]
    
    df <- cbind("Row #" = 1:nrow(df), df)
    
    df
  }, options = list(paging = FALSE, scrollX = TRUE, scrollY = "800px"), rownames = FALSE)
  
  # -------------------------
  # ROSTER TABLE
  # -------------------------
  output$roster_table <- renderDT({
    req(input$roster_file)
    df <- read.csv(input$roster_file$datapath)
    
    if(!is.null(input$search_roster) && input$search_roster != "") {
      mask <- apply(df, 1, function(row) {
        any(str_detect(as.character(row), regex(input$search_roster, ignore_case = TRUE)))
      })
      df <- df[mask, ]
    }
    
    if (!is.null(input$filter_age)) {
      df <- df[df$Age >= input$filter_age[1] &
                 df$Age <= input$filter_age[2], ]
    }
    
    if (!is.null(input$filter_pos_roster) && !"All" %in% input$filter_pos_roster && length(input$filter_pos_roster) > 0) {
      df <- df[df$Pos %in% input$filter_pos_roster, ]
    }
    
    df <- cbind("Row #" = 1:nrow(df), df)
    
    df
  }, options = list(paging = FALSE, scrollX = TRUE, scrollY = "800px"), rownames = FALSE)
  
  # -------------------------
  # SCHEDULE TABLE
  # -------------------------
  output$schedule_table <- renderDT({
    req(input$schedule_file)
    df <- read.csv(input$schedule_file$datapath)
    
    if (!is.null(input$search_schedule) && input$search_schedule != "") {
      mask <- apply(df, 1, function(row) {
        any(str_detect(as.character(row), regex(input$search_schedule, ignore_case = TRUE)))
      })
      df <- df[mask, ]
    }
    
    if (!is.null(input$filter_week) && !"All" %in% input$filter_week && length(input$filter_week) > 0) {
      week_nums <- as.numeric(gsub("Week ", "", input$filter_week))
      df <- df[df$Week %in% week_nums, ]
    }
    
    if (!is.null(input$filter_result) && input$filter_result != "All") {
      df <- df[df$Result == input$filter_result, ]
    }
    
    if (!is.null(input$filter_location) && input$filter_location != "All") {
      df <- df[df$Location == input$filter_location, ]
    }
    
    if (!is.null(input$filter_points_for)) {
      df <- df[df$Points.For >= input$filter_points_for[1] &
                 df$Points.For <= input$filter_points_for[2], ]
    }
    
    if (!is.null(input$filter_points_against)) {
      df <- df[df$Points.Against >= input$filter_points_against[1] &
                 df$Points.Against <= input$filter_points_against[2], ]
    }
    
    df
  }, options = list(paging = FALSE, scrollX = TRUE, scrollY = "800px"), rownames = FALSE)
  
  output$pageContent <- renderUI({
    
    if (!showDashboard()) {
    # -------------------------
    # LANDING / UPLOAD SCREEN
    # -------------------------
      fluidPage(
        tags$div(class = "bg-glow"),
        # -------------------------
        # HEADER
        # -------------------------
        tags$header(class = "dash-header",
          tags$div(class = "logo-area",
            tags$div(class = "team-badge", "FVA"),
            tags$div(class = "dash-title", "Field Vision ",
              tags$span("Analytics")
            )
          ),
          tags$nav(class = "header-nav",
            actionButton("launch_dashboard", "Launch Dashboard", 
              class = "nav-btn primary")     
          ),
          tags$div(class = "upload-header",
            actionButton("help_content", "New Here?", 
              class = "nav-btn ghost")
          ),
          tags$div(class = "season-badge", paste(format(Sys.Date(), "%Y")), "Season")
        ),
        
        tags$div(class = "page-wrap",
          # ------------------------
          # HERO BANNER
          # ------------------------
          tags$section(class = "hero",
            tags$div(class = "hero-eyebrow", "Data Upload Center"),
            tags$h1(
              tags$span(class = "line-dim", "Load Your"), 
              tags$br(),
              tags$span(class = "line-accent", "Season Data")
            ),
            tags$p(class = "hero-sub",
                  "Upload your team's spreadsheets below to power the full coaching 
                  dashboard. All data sheets are optional - upload what you have 
                  to get started."
            )
          ),
          
          # ------------------------
          # DIVIDER
          # ------------------------
          tags$div(class = "divider",
            tags$div(class = "divider-line left"),
            tags$div(class = "divider-label", "Select Files to Upload"),
            tags$div(class = "divider-line")
          ),
          # ------------------------
          # UPLOAD GRID
          # ------------------------
          uiOutput("uploadGrid"),
          # ------------------------
          # ACTION ROW 
          # ------------------------
          tags$div(class = "action-row",
            tags$div(class = "action-info",
              tags$div(class = "upload-counter",
                textOutput("uploadCount")
              ),
              tags$div(class = "counter-label",
                textOutput("counterText")
              )
            ),
            tags$div(class = "action-btns",
              actionButton("clearAllBtn", "Clear All", class = "btn btn-ghost"),
              actionButton("launch_dashboard", "Launch Dashboard", class = "btn btn-primary")
            )
          )
        )
      )
    } else {
      # -------------------------
      # DASHBOARD SCREEN
      # -------------------------
      fluidPage(
        # -------------------------
        # HEADER
        # -------------------------
        tags$header(class = "dash-header",
          tags$div(class = "logo-area",
            tags$div(class = "team-badge", "FVA"),
            tags$div(class = "dash-title", "Field Vision ",
              tags$span("Analytics")
            )
          ),
          tags$nav(class = "header-nav",
            actionButton("back_to_upload", "← Upload Data", 
              class = "nav-btn ghost")
          ),
          tags$div(class = "upload-header",
            actionButton("help_content", "New Here?", 
              class = "nav-btn ghost")
          ),
          tags$div(class = "season-badge", paste(format(Sys.Date(), "%Y")), "Season")
        ),
        
        # -------------------------
        # CONTENT SCREENS
        # -------------------------
        tags$main(class = "dash-body",
          tabsetPanel(id = "mainTabs",
             
            # -------------------------
            # OVERVIEW TAB
            # -------------------------       
            tabPanel("📊 Overview", value = "overview",
              tags$div(class = "section-label", "Your Season At A Glance"),
              tags$div(class = "stat-row",
                tags$div(class = "stat-card",
                  tags$div(class = "stat-label", "Record"),
                  tags$div(class = "stat-value", uiOutput("record"))
                ),
                tags$div(class = "stat-card",
                  tags$div(class = "stat-label", "Average Points Per Game"),
                  tags$div(class = "stat-value", uiOutput("pts_per_game"))
                ),
                tags$div(class = "stat-card",
                  tags$div(class = "stat-label", "Points Differential"),
                  tags$div(class = "stat-value", uiOutput("pts_diff"))
                ),
                tags$div(class = "stat-card",
                  tags$div(class = "stat-label", "Turnover Margin"),
                  tags$div(class = "stat-value", uiOutput("turnover_margin"))
                ),
                tags$div(class = "stat-card",
                  tags$div(class = "stat-label", "Sack Differential"),
                  tags$div(class = "stat-value", uiOutput("sack_diff"))
                )
              ),    
              tags$div(class = "section-label", "Team Leaders"),
              tags$div(class = "panel-card",
                tags$div(class = "panel-header",
                  tags$div(class = "panel-title",
                    tags$div(class = "dot"), "Leaders"
                  )
                ),
                uiOutput("team_leaders")
              ),
              tags$div(class = "section-label", "Team Visuals"),
              tags$div(class = "panel-card",
                tags$div(class = "panel-header",
                  tags$div(class = "panel-title",
                    tags$div(class = "dot"), "Visualizations"
                  )
                ),
                uiOutput("visuals")
              )
            ),
            
            # -------------------------
            # OFFENSE TAB
            # -------------------------
            tabPanel("🏈 Offense", value = "offense",
              tags$div(class = "section-label", "Offensive Stats"),
              tags$div(class = "stat-row",
                tags$div(class = "stat-card",
                  tags$div(class = "stat-label", "Total Passing Yards"),
                  tags$div(class = "stat-value", uiOutput("total_pass_yards"))
                ),
                tags$div(class = "stat-card",
                  tags$div(class = "stat-label", "Total Rushing Yards"),
                  tags$div(class = "stat-value", uiOutput("total_rush_yards"))
                ),
                tags$div(class = "stat-card",
                  tags$div(class = "stat-label", "Total Receiving Yards"),
                  tags$div(class = "stat-value", uiOutput("total_receive_yards"))
                ),
                tags$div(class = "stat-card",
                  tags$div(class = "stat-label", "Total Passing Touchdowns"),
                  tags$div(class = "stat-value", uiOutput("total_pass_tds"))
                ),
                tags$div(class = "stat-card",
                  tags$div(class = "stat-label", "Total Rushing Touchdowns"),
                  tags$div(class = "stat-value", uiOutput("total_rush_tds"))
                ),
                tags$div(class = "stat-card",
                  tags$div(class = "stat-label", "Total Receiving Touchdowns"),
                  tags$div(class = "stat-value", uiOutput("total_receive_tds"))
                )
              ),
              tags$div(class = "section-label", "Passing Data"),
              tags$div(class = "panel-card",
                tags$div(class = "panel-header",
                  tags$div(class = "panel-title", tags$span(class = "dot"), "Filters")
                ),
                tags$div(class = "filter-grid",
                  tags$div(
                    tags$div(class = "section-label", "Search:"),
                    textInput("search_passing", NULL, placeholder = "Name..."),
                  ),
                  tags$div(
                    tags$div(class = "section-label", "Position:"),
                    selectizeInput("filter_pos_passing", NULL, choices = c("QB", "RB", "HB", "TB", "FB", 
                                                                            "LH", "RH", "BB", "B", "WB", 
                                                                            "WR", "FL", "SE", "E", "TE",
                                                                            "LE", "LT", "LOT", "T", "LG",
                                                                            "G", "C", "RG", "RT", "ROT", 
                                                                            "RE", "DL", "LDE", "DE", "LDT",
                                                                            "DT", "NT", "MG", "DG", "RDT",
                                                                            "RDE", "LOLB", "RUSH", "OLB",
                                                                            "LLB", "LILB", "WILL", "ILB", 
                                                                            "SLB", "MLB", "MIKE", "WLB", 
                                                                            "RILB", "RLB", "ROLB", "SAM",
                                                                            "LB", "LCB", "CB", "RCB", "SS", 
                                                                            "FS", "LDH", "RDH", "LS", "S",
                                                                            "RS", "DB", "K", "P", "PR", "KR", "RET"),
                                                                            NULL, multiple = TRUE),
                  )
                )
              ),
              tags$div(class = "panel-card",
                tags$div(class = "panel-header",
                  tags$div(class = "panel-title", tags$span(class = "dot"), "Passing"),
                  tags$div(class = "panel-badge", paste(format(Sys.Date(), "%Y")), "Season")
                ),
                tags$div(class = "data-table-wrap", DTOutput("passing_table"))
              ),
              tags$div(class = "section-label", "Rushing & Receiving Data"),
              tags$div(class = "panel-card",
                tags$div(class = "panel-header",
                  tags$div(class = "panel-title", tags$span(class = "dot"), "Filters")
                ),
                tags$div(class = "filter-grid",
                  tags$div(
                    tags$div(class = "section-label", "Search:"),
                    textInput("search_rush_receive", NULL, placeholder = "Name..."),
                  ),
                  tags$div(
                    tags$div(class = "section-label", "Position:"),
                    selectizeInput("filter_pos_rush_receive", NULL, choices = c("QB", "RB", "HB", "TB", "FB", 
                                                                                "LH", "RH", "BB", "B", "WB", 
                                                                                "WR", "FL", "SE", "E", "TE",
                                                                                "LE", "LT", "LOT", "T", "LG",
                                                                                "G", "C", "RG", "RT", "ROT", 
                                                                                "RE", "DL", "LDE", "DE", "LDT",
                                                                                "DT", "NT", "MG", "DG", "RDT",
                                                                                "RDE", "LOLB", "RUSH", "OLB",
                                                                                "LLB", "LILB", "WILL", "ILB", 
                                                                                "SLB", "MLB", "MIKE", "WLB", 
                                                                                "RILB", "RLB", "ROLB", "SAM",
                                                                                "LB", "LCB", "CB", "RCB", "SS", 
                                                                                "FS", "LDH", "RDH", "LS", "S",
                                                                                "RS", "DB", "K", "P", "PR", "KR", "RET"),
                                                                                NULL, multiple = TRUE),
                    ),
                  tags$div(
                    tags$div(class = "section-label", "Stat Type:"),
                    selectizeInput("filter_stat_offense", NULL, choices = c("Rushing", "Receiving"), NULL, multiple = TRUE)
                  )
                )
              ),
              tags$div(class = "panel-card",
                tags$div(class = "panel-header",
                  tags$div(class = "panel-title", tags$span(class = "dot"), "Rushing & Receiving"),
                  tags$div(class = "panel-badge", paste(format(Sys.Date(), "%Y")), "Season")
                ),
                tags$div(class = "data-table-wrap", DTOutput("rush_receive_table"))
              ),
            ),
            # -------------------------
            # DEFENSE TAB
            # -------------------------
            tabPanel("🛡 Defense", value = "defense",
              tags$div(class = "section-label", "Defensive Stats"),
              tags$div(class = "stat-row",
                tags$div(class = "stat-card",
                  tags$div(class = "stat-label", "Total Sacks"),
                  tags$div(class = "stat-value", uiOutput("total_sacks"))
                ),
                tags$div(class = "stat-card",
                  tags$div(class = "stat-label", "Total Tackles for a Loss"),
                  tags$div(class = "stat-value", uiOutput("total_tfls"))
                ),
                tags$div(class = "stat-card",
                  tags$div(class = "stat-label", "Total Quarterback Hits"),
                  tags$div(class = "stat-value", uiOutput("total_qb_hits"))
                ),
                tags$div(class = "stat-card",
                  tags$div(class = "stat-label", "Total Interceptions"),
                  tags$div(class = "stat-value", uiOutput("total_ints"))
                ),
                tags$div(class = "stat-card",
                  tags$div(class = "stat-label", "Total Forced Fumbles"),
                  tags$div(class = "stat-value", uiOutput("total_forced_fumbles"))
                )
              ),
              tags$div(class = "panel-card",
                tags$div(class = "panel-header",
                  tags$div(class = "panel-title", tags$span(class = "dot"), "Filters")
                ),
                tags$div(class = "filter-grid",
                  tags$div(
                    tags$div(class = "section-label", "Search:"),
                    textInput("search_defense", NULL, placeholder = "Name...")
                  ),
                  tags$div(
                    tags$div(class = "section-label", "Position:"),
                    selectizeInput("filter_pos_defense", NULL, choices = c("QB", "RB", "HB", "TB", "FB", 
                                                                          "LH", "RH", "BB", "B", "WB", 
                                                                          "WR", "FL", "SE", "E", "TE",
                                                                          "LE", "LT", "LOT", "T", "LG",
                                                                          "G", "C", "RG", "RT", "ROT", 
                                                                          "RE", "DL", "LDE", "DE", "LDT",
                                                                          "DT", "NT", "MG", "DG", "RDT",
                                                                          "RDE", "LOLB", "RUSH", "OLB",
                                                                          "LLB", "LILB", "WILL", "ILB", 
                                                                          "SLB", "MLB", "MIKE", "WLB", 
                                                                          "RILB", "RLB", "ROLB", "SAM",
                                                                          "LB", "LCB", "CB", "RCB", "SS", 
                                                                          "FS", "LDH", "RDH", "LS", "S",
                                                                          "RS", "DB", "K", "P", "PR", "KR", "RET"),
                                                                          NULL, multiple = TRUE)
                  ),
                  tags$div(
                    tags$div(class = "section-label", "Position Group:"),
                    selectizeInput("filter_pos_group_defense", NULL, choices = c("D-Line", "Linebackers", "Defensive Backs", "Other"), NULL, multiple = TRUE)
                  ),
                  tags$div(
                    tags$div(class = "section-label", "Stat Type:"),
                    selectizeInput("filter_stat_defense", NULL, choices = c("Interceptions", "Fumbles", "Tackles"), NULL, multiple = TRUE)
                  )
                )  
              ),
              tags$div(class = "panel-card",
                tags$div(class = "panel-header",
                  tags$div(class = "panel-title", tags$span(class = "dot"), "Defense"),
                  tags$div(class = "panel-badge", paste(format(Sys.Date(), "%Y")), "Season")
                ),
                tags$div(class = "data-table-wrap", DTOutput("defense_table"))
              )
            ),
            
            # -------------------------
            # SPECIAL TEAMS TAB
            # -------------------------
            tabPanel("🦵 Special Teams", value = "special",
              tags$div(class = "section-label", "Special Teams Stats"),
              tags$div(class = "stat-row",
                tags$div(class = "stat-card",
                  tags$div(class = "stat-label", "Average Kick Return"),
                  tags$div(class = "stat-value", uiOutput("avg_kick_return"))
                ),
                tags$div(class = "stat-card",
                  tags$div(class = "stat-label", "Average Punt Return"),
                  tags$div(class = "stat-value", uiOutput("avg_punt_return"))
                ),
                tags$div(class = "stat-card",
                  tags$div(class = "stat-label", "Touchback Percentage"),
                  tags$div(class = "stat-value", uiOutput("max_tb_perc"))
                )
              ),
              tags$div(class = "panel-card",
                tags$div(class = "panel-header",
                  tags$div(class = "panel-title", tags$span(class = "dot"), "Filters")
                ),
                tags$div(class = "filter-grid",
                  tags$div(
                    tags$div(class = "section-label", "Search:"),
                    textInput("search_special", NULL, placeholder = "Name...")
                  ),
                  tags$div(
                    tags$div(class = "section-label", "Position:"),
                    selectizeInput("filter_pos_special", NULL, choices = c("K", "P", "PR", "KR", "RET",
                                                                  "QB", "RB", "HB", "TB", "FB", 
                                                                  "LH", "RH", "BB", "B", "WB", 
                                                                  "WR", "FL", "SE", "E", "TE",
                                                                  "LE", "LT", "LOT", "T", "LG",
                                                                  "G", "C", "RG", "RT", "ROT", 
                                                                  "RE", "DL", "LDE", "DE", "LDT",
                                                                  "DT", "NT", "MG", "DG", "RDT",
                                                                  "RDE", "LOLB", "RUSH", "OLB",
                                                                  "LLB", "LILB", "WILL", "ILB", 
                                                                  "SLB", "MLB", "MIKE", "WLB", 
                                                                  "RILB", "RLB", "ROLB", "SAM",
                                                                  "LB", "LCB", "CB", "RCB", "SS", 
                                                                  "FS", "LDH", "RDH", "LS", "S",
                                                                  "RS", "DB")
                                                                  , NULL, multiple = TRUE)
                  ),
                  tags$div(
                    tags$div(class = "section-label", "Stat Type:"),
                    selectizeInput("filter_stat_special", NULL, choices = c("Kicking", "Punting", "Returning"), NULL, multiple = TRUE)
                  )
                )         
              ),
              tags$div(class = "panel-card",
                tags$div(class = "panel-header",
                  tags$div(class = "panel-title", tags$span(class = "dot"), "Special Teams"),
                  tags$div(class = "panel-badge", paste(format(Sys.Date(), "%Y")), "Season")
                ),
                tags$div(class = "data-table-wrap", DTOutput("special_table"))
              )       
            ),
            
            # -------------------------
            # ROSTER TAB
            # -------------------------
            tabPanel("📋 Roster", value = "roster",
              tags$div(class = "section-label", "Roster Stats"),
              tags$div(class = "stat-row",
                tags$div(class = "stat-card",
                  tags$div(class = "stat-label", "Average Age"),
                  tags$div(class = "stat-value", uiOutput("avg_age"))
                ),
                tags$div(class = "stat-card",
                  tags$div(class = "stat-label", "Average Years Experience"),
                  tags$div(class = "stat-value", uiOutput("avg_years"))
                )
              ),
              tags$div(class = "section-label", "Active Roster"),
              tags$div(class = "panel-card",
                tags$div(class = "panel-header",
                  tags$div(class = "panel-title", tags$span(class = "dot"), "Filters")
                ),
                tags$div(class = "filter-grid",
                  tags$div(
                    tags$div(class = "section-label", "Search:"),
                    textInput("search_roster", NULL, placeholder = "Number, name...")
                  ),
                  tags$div(
                    tags$div(class = "section-label", "Age:"),
                    sliderInput("filter_age", NULL, min = 18, max = 50, value = c(18, 50), step = 1)
                  ),
                  tags$div(
                    tags$div(class = "section-label", "Position:"),
                    selectizeInput("filter_pos_roster", NULL, choices = c("QB", "RB", "HB", "TB", "FB", 
                                                                   "LH", "RH", "BB", "B", "WB", 
                                                                   "WR", "FL", "SE", "E", "TE",
                                                                   "LE", "LT", "LOT", "T", "LG",
                                                                   "G", "C", "RG", "RT", "ROT", 
                                                                   "RE", "DL", "LDE", "DE", "LDT",
                                                                   "DT", "NT", "MG", "DG", "RDT",
                                                                   "RDE", "LOLB", "RUSH", "OLB",
                                                                   "LLB", "LILB", "WILL", "ILB", 
                                                                   "SLB", "MLB", "MIKE", "WLB", 
                                                                   "RILB", "RLB", "ROLB", "SAM",
                                                                   "LB", "LCB", "CB", "RCB", "SS", 
                                                                   "FS", "LDH", "RDH", "LS", "S",
                                                                   "RS", "DB", "K", "P", "PR", "KR", "RET")
                                                                   , NULL, multiple = TRUE)
                  )
                )         
              ),
              tags$div(class = "panel-card",
                tags$div(class = "panel-header",
                  tags$div(class = "panel-title", tags$span(class = "dot"), "Roster"),
                  tags$div(class = "panel-badge", paste(format(Sys.Date(), "%Y")), "Season")
                ),
                tags$div(class = "data-table-wrap", DTOutput("roster_table"))
              )
            ),
            
            # -------------------------
            # SCHEDULE TAB
            # -------------------------
            tabPanel("🗓 Schedule", value = "schedule", 
              tags$div(class = "section-label", "This Year's Schedule"),
              tags$div(class = "panel-card",
                tags$div(class = "panel-header",
                  tags$div(class = "panel-title", tags$span(class = "dot"), "Filters")
                ),
                tags$div(class = "filter-grid",
                  tags$div(
                    tags$div(class = "section-label", "Search:"),
                    textInput("search_schedule", NULL, placeholder = "Opponent, week...")
                  ),
                  tags$div(
                    tags$div(class = "section-label", "Week:"),
                    selectizeInput("filter_week", NULL, choices = c(paste("Week", 1:18)), NULL, multiple = TRUE)
                  ),
                  tags$div(
                    tags$div(class = "section-label", "Result:"),
                    selectInput("filter_result", NULL, choices = c("All", "Win", "Loss", "Tie"), selected = "All")
                  ),
                  tags$div(
                    tags$div(class = "section-label", "Location:"),
                    selectInput("filter_location", NULL, choices = c("All", "Home", "Away"), selected = "All")
                  ),
                  tags$div(
                    tags$div(class = "section-label", "Points For:"),
                    sliderInput("filter_points_for", NULL, min = 0, max = 60, value = c(0, 60), step = 1)
                  ),
                  tags$div(
                    tags$div(class = "section-label", "Points Against:"),
                    sliderInput("filter_points_against", NULL, min = 0, max = 60, value = c(0, 60), step = 1)
                  )
                )
              ),
              tags$div(class = "panel-card",
                tags$div(class = "panel-header",
                  tags$div(class = "panel-title", tags$span(class = "dot"), "Season Schedule"),
                  tags$div(class = "panel-badge", paste(format(Sys.Date(), "%Y")), "Season")
                ),
                tags$div(class = "data-table-wrap", DTOutput("schedule_table"))
              )
            )
          )
        )
      )
    }
  })
}

shinyApp(ui = ui, server = server)