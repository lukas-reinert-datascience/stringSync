#Medication matcher
#Author: Lukas Reinert
library(shiny)
library(shinyjs)
library(shinyalert)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(ggplot2)
library(DT)
library(RSQLite)
library(plotly)
library(stringdist)

ui <- dashboardPage(
  dashboardHeader(
  ),
  dashboardSidebar(
    HTML('<ul class="sidebar-menu">
  <li>
    <a href="#shiny-tab-matcher" data-toggle="tab" data-value="matcher">
      <i class="fas fa-computer" role="presentation" aria-label="computer icon"></i>
      <span style="padding-left:10px;">Matching tool</span>
    </a>
  </li>
  <li>
    <a href="#shiny-tab-settings" data-toggle="tab" data-value="settings">
      <i class="fas fa-gear" role="presentation" aria-label="gear icon"></i>
      <span style="padding-left:10px;">Settings</span>
    </a>
  </li>
  <li>
    <a href="#shiny-tab-dashboard" data-toggle="tab" data-value="dashboard">
      <i class="fas fa-gauge" role="presentation" aria-label="gauge icon"></i>
      <span style="padding-left:10px;">Dashboard</span>
    </a>
  </li>
  <li>
    <a href="#shiny-tab-export" data-toggle="tab" data-value="export">
      <i class="fas fa-download" role="presentation" aria-label="download icon"></i>
      <span style="padding-left:10px;">Export</span>
    </a>
  </li>
</ul>')
  ),
  dashboardBody(
    useShinyjs(),
    includeCSS("www/manual_matcher.css"),
    tabItems(
      tabItem(tabName = "matcher",
              div(class = "spacer_div"),
              fluidRow(
                column(8, offset = 2,
                       selectizeInput("mm_select_project_IN", "Select a project", choices = MATCHER_PROJECTS$NAME, width = "100%"),
                       div(style = "width:100%;text-align:center;",
                           actionButton("mm_load_project_IN", "Load project")    
                       ),
                       div(class = "spacer_div"),
                ),
                column(1,
                       div(class = "spacer_div", style = "height:25px;"),
                       actionButton("mm_add_new_project", "+")
                )
              ),
              div(class = "spacer_div"),
              div(id = "body_to_hide_at_launch", 
                  fluidRow(
                    
                    column(1, offset = 4, style = "text-align:right;",
                           actionButton("mm_left_click", "<<")     
                    ),
                    column(2,
                           plotOutput("MM_PROGRESS_BAR", height = "35px", width = "100%")
                    ),
                    column(1,
                           actionButton("mm_right_click", ">>")
                    )
                  ),
                  fluidRow(
                    column(4, offset = 4, style = "text-align:right;",
                           uiOutput("mm_progressbar_numbers_UI")
                    )
                  ),
                  div(class = "spacer_div", style = "height:15px;"),
                  fluidRow(
                    column(8, offset = 2,
                           div(class = "box_div", style = "min-height:50px;font-size: 17px;",
                               textOutput("mm_current_string_to_match_TEXT")
                           ),
                           div(class = "spacer_div", style = "height:20px;"),
                           fluidRow(
                             column(10, offset = 1,
                                    prettyRadioButtons("mm_type_of_match_IN", label = "Type of match",
                                                       choiceNames = c("Levensthein (LV)", "Longest common substring (LCS)",
                                                                       "Entire reference catalog"),
                                                       choiceValues = c("LV", "LCS", "ALL_DF_REF"),
                                                       inline = TRUE)
                             )
                           ),
                           div(class = "box_div", style = "height:500px;background-color:transparent;",
                               DT::dataTableOutput("MM_MATCHING_CANDIDATES_TABLE", width = "100%"),
                           ),
                           div(class = "spacer_div", style = "height:15px;"),
                           textInput("mm_matching_comment_IN", label = NULL, placeholder = "Enter a matching comment...", width = "100%"),
                           fluidRow(
                             column(2, offset = 1, style = "text-align:center;",
                                    actionButton("mm_save_perfect", "Perfect")
                             ),
                             column(2, style = "text-align:center;",
                                    actionButton("mm_save_good", "Good")
                             ),
                             column(2, style = "text-align:center;",
                                    actionButton("mm_save_decent", "Decent")
                             ),
                             column(2, style = "text-align:center;",
                                    actionButton("mm_save_bad", "Bad")
                             ),
                             column(2, style = "text-align:center;",
                                    actionButton("mm_save_not_matchable", "No match")
                             )
                           )      
                    )
                  ) 
              )
              
      ),
      tabItem(tabName = "settings", 
              sliderInput("mm_n_min_match", label = "Set the number of presented matches",
                          min = 1, max = 20, value = 10)
      ),
      tabItem(tabName = "dashboard",
              div(class = "spacer_div"),
              fluidRow(
                column(6, offset = 3,
                       div(class = "header_div",
                           tags$b("Match quality distribution")
                       ),
                       div(class = "box_div", height = "500px",
                           plotlyOutput("DASHBOARD_MATCH_QUALITY_DISTRIBUTION", width = "100%", height = "480px")
                       )   
                )
              )
      ),
      tabItem(tabName = "export", 
              div(class = "spacer_div"),
              fluidRow(
                column(8, offset = 2,
                       selectizeInput("mm_export_select_project_IN", "Select a project", choices = MATCHER_PROJECTS$NAME, width = "100%"),
                       div(style = "width:100%;text-align:center;",
                           downloadButton("mm_export_project_IN", label = "Download")
                       )
                )
              )
      )
    )
  ),
  controlbar = NULL,
  footer = NULL,
  skin = "midnight",
  title = "StringSync"
)