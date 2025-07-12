server <- function(input, output, session) { 
  
  # Reactive value to control the show/hide of the UI for matching below project selection
  startUpTrigger <- reactiveValues()
  startUpTrigger$trigger <- 0
  observeEvent(startUpTrigger$trigger,{ 
    DF_MATCH <<- data.frame()
    DF_REF <<- data.frame()
    strings_to_match <<- c()
    match_mat_lev <<- matrix()
    match_mat_lcs <<- matrix()
    shinyjs::hide(id = "body_to_hide_at_launch")
  })
  
  # Create a new project
  observeEvent(input$mm_add_new_project, {
    showModal(
      modalDialog(
        div(
          textInput("mm_new_project_name_IN", "Project name", placeholder = "Enter a name for the project", width = "100%"),
          div(class = "spacer_div", style = "height:10px;"),
          fileInput("mm_new_project_match_list_IN", "Upload a list of strings to be matched",
                    multiple = FALSE,
                    accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv",
                               ".txt"),
                    width = "100%"),
          # div(class = "spacer_div", style = "height:10px;"),
          prettyRadioButtons("mm_new_project_ref_table_choice_IN", "Matching reference catalog",
                             choiceNames = c("Upload new table",
                                             "Select from existing"),
                             choiceValues = c("upload",
                                              "existing")),
          uiOutput("mm_new_project_ref_table_UI"),
          selectizeInput("mm_new_project_ref_column_select_IN", 
                         "Select the reference column", 
                         choices = c(), 
                         width = "100%")  ,
          selectizeInput("mm_new_project_code_column_select_IN", 
                         "Select the code column", 
                         choices = c(), 
                         width = "100%"),
          hr(style = "border: 1px dashed #bec5cb;"),
          div(style = "width:100%;text-align:center;",
              actionButton("mm_create_project_IN", "Create new project")    
          )
        ),
        title = "Create a new project",
        easyClose = TRUE,
        footer = NULL
      )  
    )
  })
  output$mm_new_project_ref_table_UI <- renderUI({
    chs <- input$mm_new_project_ref_table_choice_IN
    
    if (chs == "upload"){
      ui <- fluidRow(
        column(11, offset = 1,
               fileInput("mm_new_project_ref_table_upload_IN", "Upload a reference catalog",
                         multiple = FALSE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv",
                                    ".txt"),
                         width = "100%")   
        )
      )
      
      
    }
    else if (chs == "existing"){
      chs <- c()
      if (nrow(MATCHER_PROJECTS)>0){
        chs <- MATCHER_PROJECTS$NAME
      }
      
      ui <- fluidRow(
        column(11, offset = 1,
               selectizeInput("mm_new_project_ref_table_select_IN", 
                              "Select a reference catalog from project:", choices = chs, width = "100%")   
        )
      )
      
    }
    return(ui)
  })
  
  # Create project
  observeEvent(input$mm_new_project_ref_table_upload_IN,{
    proj_ref_table_upload <- input$mm_new_project_ref_table_upload_IN
    
    table_names <- c("")
    if(!is.null(proj_ref_table_upload)){
      DF_REF_UPLOADED <- read.csv2(proj_ref_table_upload$datapath)
      if (is.data.frame(DF_REF_UPLOADED)){
        if (ncol(DF_REF_UPLOADED) > 1){
          table_names <- names(DF_REF_UPLOADED)
        }
      }
    }
    updateSelectizeInput(session, "mm_new_project_ref_column_select_IN", choices = table_names)
    updateSelectizeInput(session, "mm_new_project_code_column_select_IN", choices = table_names)
  })
  observeEvent(input$mm_new_project_ref_table_select_IN, {
    proj_ref_table_select <- input$mm_new_project_ref_table_select_IN
    
    table_names <- c("")
    if (proj_ref_table_select %in% MATCHER_PROJECTS$NAME){
      establishSqliteDatabaseConnection()
      DF_REF_SELECTED <- dbGetQuery(dbConn, paste0("SELECT * FROM DF_REF_", MATCHER_PROJECTS$ID[match(proj_ref_table_select, MATCHER_PROJECTS$NAME)]))
      closeSqliteDatabaseConnection()
      if (is.data.frame(DF_REF_SELECTED)){
        table_names <- names(DF_REF_SELECTED)
      }
    }
    updateSelectizeInput(session, "mm_new_project_ref_column_select_IN", choices = table_names)
    updateSelectizeInput(session, "mm_new_project_code_column_select_IN", choices = table_names)
  })
  observeEvent(input$mm_create_project_IN, {
    
    proj_name <- input$mm_new_project_name_IN
    proj_match_list <- input$mm_new_project_match_list_IN
    proj_ref_table_choice <- input$mm_new_project_ref_table_choice_IN
    proj_ref_table_upload <- input$mm_new_project_ref_table_upload_IN
    proj_ref_table_select <- input$mm_new_project_ref_table_select_IN
    proj_ref_column <- input$mm_new_project_ref_column_select_IN
    proj_code_column <- input$mm_new_project_code_column_select_IN
    
    # Test passed binary. If one test failes it turns "FALSE" and project is not created
    test_passed <- TRUE
    
    # Test: Project already exists
    if (proj_name %in% MATCHER_PROJECTS$NAME){
      test_passed <- FALSE
      fail_message <- "Project name already exists."
    }
    if (trimws(proj_name) == ""){
      test_passed <- FALSE
      fail_message <- "Project name is empty."
    }
    
    # Test: list of strings is properly formatted
    if (!is.null(proj_match_list)){
      proj_match_list <- readLines(proj_match_list$datapath)
      if (!is.character(proj_match_list)){
        test_passed <- FALSE
        fail_message <- "Match list is not formatted correctly."
      }
      else {
        if ("" %in% proj_match_list){
          proj_match_list <- proj_match_list[-match("", proj_match_list)]
        }
        proj_match_list <- trimws(proj_match_list)
        proj_match_list <- unique(proj_match_list)
      }
    }
    else {
      test_passed <- FALSE
      fail_message <- "No match list was uploaded."
    }
    
    # Test: if reference catalog is properly formatted or exists
    if (proj_ref_table_choice == "upload"){
      if(!is.null(proj_ref_table_upload)){
        DF_REF_NEW <- read.csv2(proj_ref_table_upload$datapath)
        if (!is.data.frame(DF_REF_NEW)){
          test_passed <- FALSE
          fail_message <- "Ref table is not formatted correctly."
        }
      }
      else {
        test_passed <- FALSE
        fail_message <- "No ref table was uploaded."
      }
    }
    else if (proj_ref_table_choice == "existing"){
      if (proj_ref_table_select %in% MATCHER_PROJECTS$NAME){
        establishSqliteDatabaseConnection()
        DF_REF_NEW <- dbGetQuery(dbConn, paste0("SELECT * FROM DF_REF_", MATCHER_PROJECTS$ID[match(proj_ref_table_select, MATCHER_PROJECTS$NAME)]))
        closeSqliteDatabaseConnection()
        if (!is.data.frame(DF_REF_NEW)){
          test_passed <- FALSE
          fail_message <- "Ref table is not formatted correctly."
        }
      }
      else {
        test_passed <- FALSE
        fail_message <- "Ref table does not exist"
      }
    }
    
    
    # Check if tests are passed
    if (test_passed == TRUE){
      # Create project
      
      # Get tables
      establishSqliteDatabaseConnection()
      MATCHER_PROJECTS <<- dbGetQuery(dbConn, "SELECT * FROM MATCHER_PROJECTS")
      MATCHER_REF_COLUMN <<- dbGetQuery(dbConn, "SELECT * FROM MATCHER_REF_COLUMN")
      MATCHER_CODE_COLUMN <<- dbGetQuery(dbConn, "SELECT * FROM MATCHER_CODE_COLUMN")
      closeSqliteDatabaseConnection()
      
      # Generate id for new project
      if (nrow(MATCHER_PROJECTS) == 0){
        id <- 1
      }
      else {
        id <- max(MATCHER_PROJECTS$ID) + 1 
      }
      
      # Add data to tables
      MATCHER_PROJECTS <<- rbind(MATCHER_PROJECTS, data.frame(
        ID = id,
        NAME = proj_name,
        CREATIONDATE = as.character(Sys.time())
      ))
      MATCHER_REF_COLUMN <<- rbind(MATCHER_REF_COLUMN, data.frame(
        PROJECT_ID = id,
        COLUMN_NAME = proj_ref_column
      ))
      MATCHER_CODE_COLUMN <<- rbind(MATCHER_CODE_COLUMN, data.frame(
        PROJECT_ID = id,
        COLUMN_NAME = proj_code_column
      ))
      
      
      # Create DF_MATCH
      progressSweetAlert(session = session,
                         id = "DF_MATCH_initialize",
                         value = 0, #total = 100,
                         display_pct = TRUE,
                         title = "Creating project",
                         commas = TRUE, unit_mark = "%")
      message("Building DF_MATCH")
      DF_MATCH <- data.frame(
        STRINGS_TO_MATCH = proj_match_list,
        REFERENCE = NA,
        LV_DIST = NA,
        LCS_LEN = NA,
        CODE = NA,
        MATCH_QUALITY = NA,
        COMMENT = NA
      )
      for (i in 1:nrow(DF_MATCH)){
        updateProgressBar(session = session,
                          id = "DF_MATCH_initialize",
                          value = round((i/nrow(DF_MATCH)), digits = 2)*100, #total = 100,
                          title = "Creating project")
        match_index <- which(DF_REF_NEW[,proj_ref_column] == DF_MATCH$STRINGS_TO_MATCH[i])
        if (length(match_index) > 0){
          DF_MATCH$REFERENCE[i]     <- DF_REF_NEW[,proj_ref_column][match_index[1]]
          DF_MATCH$LV_DIST[i]       <- 0
          DF_MATCH$LCS_LEN[i]       <- nchar(DF_REF_NEW[,proj_ref_column][match_index[1]])
          DF_MATCH$CODE[i]          <- paste0(DF_REF_NEW[,proj_code_column][match_index], collapse = "+")
          DF_MATCH$MATCH_QUALITY[i] <- 1
        }
      }
      DF_MATCH <- DF_MATCH[order(DF_MATCH$REFERENCE),]
      closeSweetAlert(session = session)
      
      
      # Write changes to db
      message("Writing to database")
      establishSqliteDatabaseConnection()
      dbWriteTable(dbConn, name = "MATCHER_PROJECTS", value = MATCHER_PROJECTS,
                   overwrite = TRUE)
      dbWriteTable(dbConn, name = "MATCHER_REF_COLUMN", value = MATCHER_REF_COLUMN,
                   overwrite = TRUE)
      dbWriteTable(dbConn, name = "MATCHER_CODE_COLUMN", value = MATCHER_CODE_COLUMN,
                   overwrite = TRUE)
      dbWriteTable(dbConn, name = paste0("DF_REF_", id), value = DF_REF_NEW,
                   overwrite = TRUE)
      dbWriteTable(dbConn, name = paste0("DF_MATCH_", id), value = DF_MATCH,
                   overwrite = TRUE)
      closeSqliteDatabaseConnection()
      
      
      # Update project selectizeInput 
      updateSelectizeInput(session, "mm_select_project_IN", choices = MATCHER_PROJECTS$NAME)
      updateSelectizeInput(session, "mm_export_select_project_IN", choices = MATCHER_PROJECTS$NAME)
      updateSelectizeInput(session, "mm_new_project_ref_table_select_IN", choices = paste0("DF_REF_", MATCHER_PROJECTS$ID))
      
      
      # Update trigger to show div
      removeModal()
      shinyalert("Project created!", "Project was successfully created.", type = "success",
                 closeOnClickOutside = TRUE)
    }
    else {
      shinyalert("Project not created!", fail_message, type = "error")
    }
  })
  
  # When selecting a new project update all the data/figures
  loadProject <- reactiveValues()
  loadProject$trigger <- 0
  observeEvent(input$mm_load_project_IN, {
    
    # Show div
    if (nrow(MATCHER_PROJECTS) == 0){
      shinyalert("No projects found!", "Create a project and then load again.", type = "error")
    }
    else {
      proj_name <- input$mm_select_project_IN
      
      # Fetch project data and generate matching matrices
      progressSweetAlert(session = session,
                         id = "fetch_project",
                         value = 25, 
                         display_pct = TRUE,
                         title = "Initializing project",
                         commas = TRUE, unit_mark = "%")
      
      id <<- MATCHER_PROJECTS$ID[match(proj_name, MATCHER_PROJECTS$NAME)]
      establishSqliteDatabaseConnection()
      DF_MATCH <<- dbGetQuery(dbConn, paste0("SELECT * FROM DF_MATCH_", id))
      DF_REF   <<- dbGetQuery(dbConn, paste0("SELECT * FROM DF_REF_", id))
      ref_col  <<- dbGetQuery(dbConn, paste0("SELECT COLUMN_NAME FROM MATCHER_REF_COLUMN WHERE PROJECT_ID = ", id))$COLUMN_NAME
      code_col <<- dbGetQuery(dbConn, paste0("SELECT COLUMN_NAME FROM MATCHER_CODE_COLUMN WHERE PROJECT_ID = ", id))$COLUMN_NAME
      strings_to_match <<- DF_MATCH$STRINGS_TO_MATCH
      establishSqliteDatabaseConnection()
      
      
      # Predicting time calculations will take
      secs <- round(predict(matrix_computation_time_fit, newdata = data.frame(N = nrow(DF_REF)*nrow(DF_MATCH))))
      
      unit <- "seconds"
      if (secs > 60){
        secs <- floor(secs/60)
        unit <- "mins"
      }
      
      # Building matching matrices
      # Levensthein 
      message("Building match_mat_lev")
      updateProgressBar(session = session,
                        id = "fetch_project",
                        value = 50, 
                        title = paste0("Initializing project (~ ",secs, " ", unit,")"))
      match_mat_lev <<- stringdist::stringdistmatrix(DF_MATCH$STRINGS_TO_MATCH, DF_REF[,ref_col])
      message("Building match_mat_lcs")
      updateProgressBar(session = session,
                        id = "fetch_project",
                        value = 75, 
                        title = paste0("Initializing project (~ ",secs, " ", unit,")"))
      match_mat_lcs <<- stringdist::stringdistmatrix(DF_MATCH$STRINGS_TO_MATCH, DF_REF[,ref_col], method = "lcs")
      rownames(match_mat_lev) <<- DF_MATCH$STRINGS_TO_MATCH
      colnames(match_mat_lev) <<- DF_REF[,ref_col]
      rownames(match_mat_lcs) <<- DF_MATCH$STRINGS_TO_MATCH
      colnames(match_mat_lcs) <<- DF_REF[,ref_col]
      updateProgressBar(session = session,
                        id = "fetch_project",
                        value = 100, 
                        title = paste0("Initializing project (~ ",secs, " ", unit,")"))
      closeSweetAlert(session = session)
      shinyjs::show(id = "body_to_hide_at_launch")
      loadProject$trigger <- loadProject$trigger +1
      
      # Setup current index
      mm_index_adjust <<- 0
      eval_btns$trigger <- 0
      leftClick$trigger <- 0
      rightClick$trigger <- 0
    }
  })
  
  leftClick <- reactiveValues()
  rightClick <- reactiveValues()
  leftClick$trigger <- 0
  rightClick$trigger <- 0
  observeEvent(input$mm_left_click,{
    leftClick$trigger <- leftClick$trigger + 1
  })
  observeEvent(input$mm_right_click,{
    rightClick$trigger <- rightClick$trigger + 1
  })
  
  mm_index_adjust <- 0
  mmProgressBarController <- reactive({
    
    # Trigger
    loadProject$trigger
    
    #Inputs 
    n_min_match <- input$mm_n_min_match
    left_click  <- leftClick$trigger
    right_click <- rightClick$trigger
    eval_btns$trigger
    total <- length(strings_to_match)
    match_type <- input$mm_type_of_match_IN
    
    # Check if the selected project is valid
    if (total>0){
      
      #Calculate position
      val <- min(which(is.na(DF_MATCH$REFERENCE))) + right_click - left_click
      if ((val+mm_index_adjust) < 1){
        mm_index_adjust <<- mm_index_adjust + 1
      }
      if ((val+mm_index_adjust) > total){
        mm_index_adjust <<- mm_index_adjust - 1
      }
      val <- val + mm_index_adjust
      
      #Calculate matching candidates
      if (match_type == "LV"){
        match_cand <- match_mat_lev[val,]
        index_ordered <- order(match_cand, decreasing = FALSE)
        index_ordered_min <- index_ordered[1:n_min_match]
        match_cand_min <- match_cand[index_ordered_min]
        other_min <- which(match_cand == match_cand_min[n_min_match])
        if (sum(other_min %in% index_ordered_min) <= length(other_min)){
          index_ordered_min <- unique(c(index_ordered_min, other_min))
          match_cand_min <- match_cand[index_ordered_min]
        }  
      } 
      else if (match_type == "LCS"){
        match_cand <- match_mat_lcs[val,]
        index_ordered <- order(match_cand, decreasing = FALSE)
        index_ordered_min <- index_ordered[1:n_min_match]
        match_cand_min <- match_cand[index_ordered_min]
        other_min <- which(match_cand == match_cand_min[n_min_match])
        if (sum(other_min %in% index_ordered_min) <= length(other_min)){
          index_ordered_min <- unique(c(index_ordered_min, other_min))
          match_cand_min <- match_cand[index_ordered_min]
        }
      }
      else if (match_type == "ALL_DF_REF"){
        match_cand_min <- c(DF_REF[,ref_col])
        index_ordered_min <- c(1:nrow(DF_REF))
      }
      
      if (length(index_ordered_min) > 0){
        match_cand_min_lv     <- match_mat_lev[val, index_ordered_min]
        match_cand_min_lcs    <- match_mat_lcs[val, index_ordered_min]
      }
      else {
        match_cand_min_lv <- c()
        match_cand_min_lcs <- c()
      }
    }
    else {
      val <- NA
      index_ordered_min <- NA
      match_cand_min_lv <- NA
      match_cand_min_lcs <- NA
    }
    
    res <- list()
    res[[1]] <- val[1]
    res[[2]] <- total
    res[[3]] <- index_ordered_min
    res[[4]] <- match_cand_min_lv
    res[[5]] <- match_cand_min_lcs
    names(res) <- c("val", "total",
                    "index_ordered_min",
                    "match_cand_min_lv",
                    "match_cand_min_lcs")
    return(res)
  })
  mmProgressBarPresenter <- reactive({
    
    #Input
    res <- mmProgressBarController()
    val_col <- value_col
    bck_col <- background_light
    
    #Plot
    if (is.na(res$val)){
      p <- ggplot()+theme_void()+ 
        theme(plot.background = element_rect(fill = "#353c42", colour = "#353c42"))+ 
        theme(panel.background = element_rect(fill = "#353c42", colour = "#353c42"))
    }
    else {
      p <- progressBarPlot(total = res$total, 
                           val = res$val, 
                           val_col, 
                           bck_col)
    }
    return(p)
  })
  output$MM_PROGRESS_BAR <- renderPlot({
    p <- mmProgressBarPresenter()
    p
  })
  output$mm_progressbar_numbers_UI <- renderUI({
    
    #Inputs
    res <- mmProgressBarController()
    val_col <- value_col
    bck_col <- input$bckgnd_col
    
    if (is.na(res$val)){
      ui <- div()
    }
    else {
      ui <- div(style = "font-size:17px;font-weight:bold;text-align:center;",
                span(res$val, style = paste0("color:",val_col,";")),
                span(" / ", style = paste0("color:",text_bright,";")),
                span(res$total, style = paste0("color:",bck_col,";"))
      )  
    }
    return(ui)
  })
 
  #Textoutput detailing the current string
  output$mm_current_string_to_match_TEXT <- renderText({
    val <- mmProgressBarController()$val
    
    if (is.na(val)){
      txt <- ""
    }
    else {
      txt <- strings_to_match[val]
    }
    return(txt)
  })
  mmMatchingCandidatesTableDfProvider <- reactive({
    res <- mmProgressBarController()
    
    if (is.na(res$val)){
      df <- data.frame()
    }
    else {
      df <- data.frame(
        REFERENCE = DF_REF[,ref_col][res$index_ordered_min],
        CODE = DF_REF[,code_col][res$index_ordered_min],
        LV_DIST = res$match_cand_min_lv,
        LCS_LEN = res$match_cand_min_lcs,
        INDEX = res$index_ordered_min
      )
    }
    return(df)
  })
  output$MM_MATCHING_CANDIDATES_TABLE <- DT::renderDataTable({
    
    #Inputs
    match_type <- isolate(input$mm_type_of_match_IN)
    df <- mmMatchingCandidatesTableDfProvider()
    
    if (nrow(df)>0 && ncol(df)>0){
      pl <- 100
      if (match_type == "ALL_DF_REF"){
        pl <- nrow(DF_REF)
      }
      
      df <- df[, -match("INDEX", names(df))]
      dt <- dataTableDefault(df,
                             height = 480,
                             pageLength = pl,
                             dom = "fti",
                             collapse_height = TRUE,
                             selection = "multiple")
    }
    else {
      dt <- dataTableDefault(df,
                             height = 480)
    }
    
    
    
    
    return(dt)
  })
  
  eval_btns <- reactiveValues()
  eval_btns$trigger <- 0
  observeEvent(input$mm_save_perfect, {
    
    #Inputs
    val <- mmProgressBarController()$val
    rows <- input$MM_MATCHING_CANDIDATES_TABLE_rows_selected
    df <- mmMatchingCandidatesTableDfProvider()
    
    if (length(rows)>0){
      #Edit the match table in R session
      DF_MATCH$REFERENCE[val]     <<- paste0(df[rows, "DESCRIPTION"], collapse = "+")
      DF_MATCH$LV_DIST[val]       <<- mean(df[rows, "LV_DIST"])
      DF_MATCH$LCS_LEN[val]       <<- mean(df[rows, "LCS_LEN"])
      DF_MATCH$CODE[val]      <<- paste0(df[rows, "CODE"], collapse = "+")
      DF_MATCH$MATCH_QUALITY[val] <<- 1
      DF_MATCH$COMMENT[val]       <<- isolate(input$mm_matching_comment_IN)
      
      #Write changes to database
      establishSqliteDatabaseConnection()
      dbWriteTable(dbConn, name = paste0("DF_MATCH_", id), value = DF_MATCH, overwrite = TRUE)
      closeSqliteDatabaseConnection()
      
      #Update the trigger
      eval_btns$trigger <- eval_btns$trigger + 1
      
      updateTextInput(session, "mm_matching_comment_IN", value = "")
      
    }
    else {
      showNotification("An entry must be selected in the table before matching.")
    }
  })
  observeEvent(input$mm_save_good, {
    
    #Inputs
    val <- mmProgressBarController()$val
    rows <- input$MM_MATCHING_CANDIDATES_TABLE_rows_selected
    df <- mmMatchingCandidatesTableDfProvider()
    
    if (length(rows)>0){
      #Edit the match table in R session
      DF_MATCH$REFERENCE[val]     <<- paste0(df[rows, "DESCRIPTION"], collapse = "+")
      DF_MATCH$LV_DIST[val]       <<- mean(df[rows, "LV_DIST"])
      DF_MATCH$LCS_LEN[val]       <<- mean(df[rows, "LCS_LEN"])
      DF_MATCH$CODE[val]      <<- paste0(df[rows, "CODE"], collapse = "+")
      DF_MATCH$MATCH_QUALITY[val] <<- 2
      DF_MATCH$COMMENT[val]       <<- isolate(input$mm_matching_comment_IN)
      
      #Write changes to database
      establishSqliteDatabaseConnection()
      dbWriteTable(dbConn, name = paste0("DF_MATCH_", id), value = DF_MATCH, overwrite = TRUE)
      closeSqliteDatabaseConnection()
      
      #Update the trigger
      eval_btns$trigger <- eval_btns$trigger + 1
      
      updateTextInput(session, "mm_matching_comment_IN", value = "")
    }
    else {
      showNotification("An entry must be selected in the table before matching.")
    }
  })
  observeEvent(input$mm_save_decent, {
    
    #Inputs
    val <- mmProgressBarController()$val
    rows <- input$MM_MATCHING_CANDIDATES_TABLE_rows_selected
    df <- mmMatchingCandidatesTableDfProvider()
    
    if (length(rows)>0){
      #Edit the match table in R session
      DF_MATCH$REFERENCE[val]     <<- paste0(df[rows, "DESCRIPTION"], collapse = "+")
      DF_MATCH$LV_DIST[val]       <<- mean(df[rows, "LV_DIST"])
      DF_MATCH$LCS_LEN[val]       <<- mean(df[rows, "LCS_LEN"])
      DF_MATCH$CODE[val]      <<- paste0(df[rows, "CODE"], collapse = "+")
      DF_MATCH$MATCH_QUALITY[val] <<- 3
      DF_MATCH$COMMENT[val]       <<- isolate(input$mm_matching_comment_IN)
      
      #Write changes to database
      establishSqliteDatabaseConnection()
      dbWriteTable(dbConn, name = paste0("DF_MATCH_", id), value = DF_MATCH, overwrite = TRUE)
      closeSqliteDatabaseConnection()
      
      #Update the trigger
      eval_btns$trigger <- eval_btns$trigger + 1
      
      updateTextInput(session, "mm_matching_comment_IN", value = "")
    }
    else {
      showNotification("An entry must be selected in the table before matching.")
    }
  })
  observeEvent(input$mm_save_bad, {
    
    #Inputs
    val <- mmProgressBarController()$val
    rows <- input$MM_MATCHING_CANDIDATES_TABLE_rows_selected
    df <- mmMatchingCandidatesTableDfProvider()
    
    if (length(rows)>0){
      #Edit the match table in R session
      DF_MATCH$REFERENCE[val]     <<- paste0(df[rows, "DESCRIPTION"], collapse = "+")
      DF_MATCH$LV_DIST[val]       <<- mean(df[rows, "LV_DIST"])
      DF_MATCH$LCS_LEN[val]       <<- mean(df[rows, "LCS_LEN"])
      DF_MATCH$CODE[val]      <<- paste0(df[rows, "CODE"], collapse = "+")
      DF_MATCH$MATCH_QUALITY[val] <<- 4
      DF_MATCH$COMMENT[val]       <<- isolate(input$mm_matching_comment_IN)
      
      #Write changes to database
      establishSqliteDatabaseConnection()
      dbWriteTable(dbConn, name = paste0("DF_MATCH_", id), value = DF_MATCH, overwrite = TRUE)
      closeSqliteDatabaseConnection()
      
      #Update the trigger
      eval_btns$trigger <- eval_btns$trigger + 1
      
      updateTextInput(session, "mm_matching_comment_IN", value = "")
    }
    else {
      showNotification("An entry must be selected in the table before matching.")
    }
  })
  observeEvent(input$mm_save_not_matchable, {
    
    #Inputs
    val <- mmProgressBarController()$val
    rows <- input$MM_MATCHING_CANDIDATES_TABLE_rows_selected
    df <- mmMatchingCandidatesTableDfProvider()
    
    if (length(rows)>0){
      #Edit the match table in R session
      DF_MATCH$REFERENCE[val]     <<- paste0(df[rows, "DESCRIPTION"], collapse = "+")
      DF_MATCH$LV_DIST[val]       <<- mean(df[rows, "LV_DIST"])
      DF_MATCH$LCS_LEN[val]       <<- mean(df[rows, "LCS_LEN"])
      DF_MATCH$CODE[val]          <<- paste0(df[rows, "CODE"], collapse = "+")
      DF_MATCH$MATCH_QUALITY[val] <<- 5
      DF_MATCH$COMMENT[val]       <<- isolate(input$mm_matching_comment_IN)
      
      #Write changes to database
      establishSqliteDatabaseConnection()
      dbWriteTable(dbConn, name = paste0("DF_MATCH_", id), value = DF_MATCH, overwrite = TRUE)
      closeSqliteDatabaseConnection()
      
      #Update the trigger
      eval_btns$trigger <- eval_btns$trigger + 1
      
      updateTextInput(session, "mm_matching_comment_IN", value = "")
    }
    else {
      showNotification("An entry must be selected in the table before matching.")
    }
  })
  
  # Dashboard
  output$DASHBOARD_MATCH_QUALITY_DISTRIBUTION <- renderPlotly({
    
    #Trigger inputs
    eval_btns$trigger
    loadProject$trigger
    
    #Plot
    if (nrow(DF_MATCH) == 0){
      fig <- mmEmptyPlotter(title = "No data \n available")
    }
    else {
      df <- as.data.frame(table(DF_MATCH$MATCH_QUALITY))
      df$Var1 <- paste0("qual_",df$Var1)
      df$Var1 <- gsub("qual_1", "Perfect", df$Var1, fixed = T)
      df$Var1 <- gsub("qual_2", "Good", df$Var1, fixed = T)
      df$Var1 <- gsub("qual_3", "Decent", df$Var1, fixed = T)
      df$Var1 <- gsub("qual_4", "Bad", df$Var1, fixed = T)
      df$Var1 <- gsub("qual_5", "No match", df$Var1, fixed = T)
      df$Var1 <- factor(df$Var1, levels = df$Var1)
      
      fig <- plot_ly(
        y = df$Var1,
        x = df$Freq,
        type = "bar",
        orientation = 'h',
        marker = list(color = hexToRbgaStringConverter(value_col),
                      line = list(color = hexToRbgStringConverter(value_col),
                                  width = 1.5))
      ) %>% layout(
        plot_bgcolor=hexToRbgStringConverter("#272c30"),
        paper_bgcolor=hexToRbgStringConverter("#272c30"),
        font = list(color = '#bec5cb'),
        yaxis = list(zerolinecolor = hexToRbgStringConverter("#272c30"))
      )%>% config(displayModeBar = FALSE)  
    }
    fig
  })
  
  # Export
  mmExportProjectTableProvider <- reactive({
    proj_name <- input$mm_export_select_project_IN
    id <- MATCHER_PROJECTS$ID[match(proj_name, MATCHER_PROJECTS$NAME)]
    establishSqliteDatabaseConnection()
    DF_MATCH_EXPORT <- dbGetQuery(dbConn, paste0("SELECT * FROM DF_MATCH_",id))
    closeSqliteDatabaseConnection()
    return(DF_MATCH_EXPORT)
  })
  output$mm_export_project_IN <- downloadHandler(
      filename = function() {
        paste('EXPORT_',input$mm_export_select_project_IN,"_", Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(mmExportProjectTableProvider(), con, row.names = FALSE)
      }
    )
  
  
}