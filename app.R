library(tidyverse)
library(shiny)
library(shinyjs)

ui <- fluidPage(
  # -------------------------
  # CONNECTION TO CSS
  # -------------------------  
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Barlow+Condensed:wght@400;600;700;800;900&family=Barlow:wght@300;400;500;600&display=swap"),
    tags$link(rel = "stylesheet", href = "styles.css")
  ),
  
  uiOutput("pageContent")
)

server <- function(input, output, session) {
  
  # This controls which page shows
  showDashboard <- reactiveVal(FALSE)
  
  # File counter
  fileCount <- reactive({
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
  
  
  # When button clicked → show dashboard
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
      reset("passing_file")
      reset("rushing_receiving_file")
      reset("defense_file")
      reset("special_file")
      reset("roster_file")
      reset("schedule_file")
      showNotification("All files have been cleared.", type = "warning")
    }
  })
  
  observeEvent(input$back_to_upload, {
    showDashboard(FALSE)
  })
  
  output$passing_filename <- renderText({
    req(input$passing_file)
    paste("✓", input$passing_file$name)
  })
  
  output$rushing_receiving_filename <- renderText({
    req(input$rushing_receiving_file)
    paste("✓", input$rushing_receiving_file$name)
  })
  
  output$defense_filename <- renderText({
    req(input$defense_file)
    paste("✓", input$defense_file$name)
  })
  
  output$special_filename <- renderText({
    req(input$special_file)
    paste("✓", input$special_file$name)
  })
  
  output$roster_filename <- renderText({
    req(input$roster_file)
    paste("✓", input$roster_file$name)
  })
  
  output$schedule_filename <- renderText({
    req(input$schedule_file)
    paste("✓", input$schedule_file$name)
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
  output$passing_table <- renderTable({
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
    
    df
  })
  
  # -------------------------
  # RUSHING & RECEIVING TABLE
  # -------------------------
  output$rush_receive_table <- renderTable({
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
    
    df
  })
  
  # -------------------------
  # DEFENSE TABLE
  # -------------------------
  output$defense_table <- renderTable({
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
    
    df
  })

  # -------------------------
  # SPECIAL TEAMS TABLE
  # -------------------------  
  output$special_table <- renderTable({
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
    
    df
  })
  
  
  # -------------------------
  # ROSTER TABLE
  # -------------------------
  output$roster_table <- renderTable({
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
    
    df
  })
  
  # -------------------------
  # SCHEDULE TABLE
  # -------------------------
  output$schedule_table <- renderTable({
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
  })
  
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
                  tags$div(class = "zone-desc", "Passing, TDs, play efficiency")
                )
              ),
              tags$div(class = "drop-area",
                fileInput("passing_file", NULL, accept = c(".csv", ".xlsx", ".xls"),
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
                  tags$div(class = "zone-desc", "Rushing, recieving yards, TDs, play efficiency")
                )
              ),
              tags$div(class = "drop-area",
                fileInput("rushing_receiving_file", NULL, accept = c(".csv", ".xlsx", ".xls"),
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
                  tags$div(class = "zone-desc", "Tackles, sacks, INTS, pressure rate")
                )
              ),
              tags$div(class = "drop-area",
                fileInput("defense_file", NULL, accept = c(".csv", ".xlsx", ".xls"),
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
                  tags$div(class = "zone-desc", "FG %, returns, coverage, net yardage")
                )
              ),
              tags$div(class = "drop-area",
                fileInput("special_file", NULL, accept = c(".csv", ".xlsx", ".xls"),
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
                  tags$div(class = "zone-desc", "Player stats")
                )
              ),
              tags$div(class = "drop-area",
                fileInput("roster_file", NULL, accept = c(".csv", ".xlsx", ".xls"),
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
          ),
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
                   actionButton("back_to_upload", "← Upload Data", class = "nav-btn ghost")
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
                tags$div(class = "data-table-wrap", tableOutput("passing_table"))
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
                tags$div(class = "data-table-wrap", tableOutput("rush_receive_table"))
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
                  tags$div(class = "panel-title", tags$span(class = "dot"), "Filters"),
                    tags$div(class = "section-label", "Search:"),
                    textInput("search_defense", NULL, placeholder = "Name..."),
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
                                                                          NULL, multiple = TRUE),
                    tags$div(class = "section-label", "Position Group:"),
                    selectizeInput("filter_pos_group_defense", NULL, choices = c("D-Line", "Linebackers", "Defensive Backs", "Other"), NULL, multiple = TRUE),
                    tags$div(class = "section-label", "Stat Type:"),
                    selectizeInput("filter_stat_defense", NULL, choices = c("Interceptions", "Fumbles", "Tackles"), NULL, multiple = TRUE)
                )  
              ),
              tags$div(class = "panel-card",
                tags$div(class = "panel-header",
                  tags$div(class = "panel-title", tags$span(class = "dot"), "Defense"),
                  tags$div(class = "panel-badge", paste(format(Sys.Date(), "%Y")), "Season")
                ),
                tags$div(class = "data-table-wrap", tableOutput("defense_table"))
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
                  tags$div(class = "panel-title", tags$span(class = "dot"), "Filters"),
                    tags$div(class = "section-label", "Search:"),
                    textInput("search_special", NULL, placeholder = "Name..."),
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
                                                                  , NULL, multiple = TRUE),
                  tags$div(class = "section-label", "Stat Type:"),
                  selectizeInput("filter_stat_special", NULL, choices = c("Kicking", "Punting", "Returning"), NULL, multiple = TRUE)
                )         
              ),
              tags$div(class = "panel-card",
                tags$div(class = "panel-header",
                  tags$div(class = "panel-title", tags$span(class = "dot"), "Special Teams"),
                  tags$div(class = "panel-badge", paste(format(Sys.Date(), "%Y")), "Season")
                ),
                tags$div(class = "data-table-wrap", tableOutput("special_table"))
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
                  tags$div(class = "panel-title", tags$span(class = "dot"), "Filters"),
                    tags$div(class = "section-label", "Search:"),
                    textInput("search_roster", NULL, placeholder = "Number, name..."),
                    tags$div(class = "section-label", "Age:"),
                    sliderInput("filter_age", NULL, min = 18, max = 50, value = c(18, 50), step = 1),
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
                                                                   , NULL, multiple = TRUE),
                )         
              ),
              tags$div(class = "panel-card",
                tags$div(class = "panel-header",
                  tags$div(class = "panel-title", tags$span(class = "dot"), "Roster"),
                  tags$div(class = "panel-badge", paste(format(Sys.Date(), "%Y")), "Season")
                ),
                tags$div(class = "data-table-wrap", tableOutput("roster_table"))
              )
            ),
            
            # -------------------------
            # SCHEDULE TAB
            # -------------------------
            tabPanel("🗓 Schedule", value = "schedule", 
              tags$div(class = "section-label", "This Year's Schedule"),
              tags$div(class = "panel-card",
                tags$div(class = "panel-header",
                  tags$div(class = "panel-title", tags$span(class = "dot"), "Filters"),
                    tags$div(class = "section-label", "Search:"),
                    textInput("search_schedule", NULL, placeholder = "Opponent, week..."),
                    tags$div(class = "section-label", "Week:"),
                    selectizeInput("filter_week", NULL, choices = c(paste("Week", 1:18)), NULL, multiple = TRUE),
                    tags$div(class = "section-label", "Result:"),
                    selectInput("filter_result", NULL, choices = c("All", "Win", "Loss", "Tie"), selected = "All"),
                    tags$div(class = "section-label", "Location:"),
                    selectInput("filter_location", NULL, choices = c("All", "Home", "Away"), selected = "All"),
                    tags$div(class = "section-label", "Points For:"),
                    sliderInput("filter_points_for", NULL, min = 0, max = 60, value = c(0, 60), step = 1),
                    tags$div(class = "section-label", "Points Against:"),
                    sliderInput("filter_points_against", NULL, min = 0, max = 60, value = c(0, 60), step = 1)
                )
              ),
              tags$div(class = "panel-card",
                tags$div(class = "panel-header",
                  tags$div(class = "panel-title", tags$span(class = "dot"), "Season Schedule"),
                  tags$div(class = "panel-badge", paste(format(Sys.Date(), "%Y")), "Season")
                ),
                tags$div(class = "data-table-wrap", tableOutput("schedule_table"))
              )
            )
          )
        )
      )
    }
  })
}

shinyApp(ui = ui, server = server)