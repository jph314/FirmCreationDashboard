# UI ----
ui <- fluidPage(
  use_googlefont("Fira Sans"),
  dashboardPage(title="UK COVID-19 Firm Creation Dashboard",
                dashboardHeader(
                  # Header
                  title= "Dashboard",
                  tags$li(a(href = 'https://www.ukfirmcreation.com/',
                             img(src = 'logo.png',
                                 title = "Project website", height = "30px"),
                             style = "padding-top:10px; padding-bottom:10px;", target = "_blank"),
                           class = "dropdown"),
                  
                  menuItemOutput("info"),
                  menuItemOutput("authors")
                ), # End of header
                dashboardSidebar(
                  #Sidebar
                  sidebarMenu(
                    id = "tabs",
                    menuItem("Home", tabName = "home", icon = icon("home")),
                    menuItem("Aggregate Analysis", tabName = "AggStats", icon = icon("bullseye")),
                    #     menuItem("Regional Analysis", icon = icon("flag"), tabName = "country"),
                    menuItem("Sectoral Analysis", icon = icon("industry"), tabName = "industries"),
                    menuItem("Regional comparison", icon = icon("earth-europa"), tabName = "regions")
                    
                    #     menuItem("Raw Data", icon = icon("database"), tabName="rawdata")
                  ),
                  ### date select
                  dateRangeInput(inputId = "dateAgg",
                                 label = "Choose date range:",
                                 start = as.Date("2020-01-01"),
                                 end = as.Date(max(registerPC$date)),
                                 min = as.Date("2020-01-01"),
                                 max = as.Date(max(registerPC$date))+30,
                                 startview = "month", weekstart = 0,
                                 language = "en", separator = " to ", width = "100%"),
                  pickerInput(
                    inputId = "pickCountry",
                    label = "Choose region:",
                    choices = c("United Kingdom" = "UK",
                                "England" = "Eng",
                                "London" = "Lon",
                                "England excl. London" = "Eng2",
                                "Scotland" = "Sco",
                                "Wales" = "Wal",
                                "Northern Ireland" = "NI"),
                    selected = "UK",
                    multiple = FALSE
                  )
                ), #End of sidebar
                dashboardBody(
                  #UKFirmCreationTheme,
                  tags$head(tags$style(HTML('
                                /* body */
                                .content-wrapper, .right-side {
                                background-color: #ffffff ;
                                overflow: hidden;
                                }

                                .myClass {
                                font-size: 10px;
                                line-height: 50px;
                                text-align: left;
                                font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
                                padding: 0 0px;
                                overflow: hidden;
                                color: white;
                                }
                                            
                                .small-box{
                                height: 120px;
                                }'
                  ))),
                  tags$script(HTML('
      $(document).ready(function() {
        $("header").find("nav").append(\'<span class="myClass"> <strong>Data last update:</strong> 14 September, 2021 </span>\');
      })'
                  )),

                    tabItems(
                      tabItem(tabName="home",
                              withMathJax(includeMarkdown("www/home.md"))
                      ),
                      ## AGGREGATE STATS ====
                      tabItem(tabName="AggStats",
                              h2("Statistics of companies in the UK"), #width=12,
                              offset=0, style='padding:3px;',
                              fluidRow(
                                     column(width=3, style='padding:2px;', height = 'auto',
                                            valueBoxOutput("vboxTotal", width=NULL)),
                                     column(width=3, style='padding:2px;', height = 'auto',
                                            valueBoxOutput("vboxVaccine", width=NULL)),
                                     column(width=3, style='padding:2px;', height = 'auto',
                                            valueBoxOutput("vboxLD", width=NULL)),
                                     column(width=3, style='padding:2px;', height = 'auto',
                                            valueBoxOutput("vboxOpen", width=NULL))),
                              fluidRow(
                                     box(width = NULL, align="center", height = 'auto',
                                         status = "primary", solidHeader = FALSE,
                                         plotlyOutput("rollingAvg", height='300px') %>% withSpinner(color ="#4C566A")
                                     )
                              ),
                              fluidRow(
                                     box(width = NULL, align="center", height = 'auto',
                                         status = "primary", solidHeader = FALSE,
                                         plotlyOutput("UKmap", height='600px') %>% withSpinner(color ="#4C566A")
                                     )
                              ),
                              fluidRow(
                                     box(width = NULL, align="center", height = 'auto',
                                         status = "primary", solidHeader = FALSE,
                                         plotlyOutput("donut", height='400px') %>% withSpinner(color ="#4C566A")
                                     ), 
                                     box(width = NULL, align="center", height = 'auto',
                                         status = "primary", solidHeader = FALSE,
                                         plotlyOutput("treemap", height='500px') %>% withSpinner(color ="#4C566A")
                                     )
                              )
                              
                      ), #AggStats
                      ## INDUSTRIES COMPARISON ====
                      tabItem(tabName = "industries",
                              h2("Industry comparison"),
                              offset=0, style='padding:3px;',
                              fluidRow(
                                # Select industry groups
                                pickerInput(
                                  inputId = "groupPicker",
                                  label = "Choose industry group(s):",
                                  choices = sort(unique(registerPC$Group)),
                                  choicesOpt = list(
                                    subtext = unique(registerPC$Group.name)[order(unique(registerPC$Group))]
                                  ),
                                  selected = 11,
                                  multiple = TRUE,
                                  options = list(
                                    `live-search`= TRUE,
                                    size = 7
                                    #             `action-box` = TRUE
                                  )
                                )
                              ),
                              fluidRow(
                                box(width = NULL, align="center", height = 'auto',
                                    status = "primary", solidHeader = FALSE,
                                    dataTableOutput("groups") %>% withSpinner(color ="#4C566A")
                                ),
                                box(width = NULL, align="center", height = 'auto',
                                    status = "primary", solidHeader = FALSE,
                                    plotlyOutput("groupsPlot") %>% withSpinner(color ="#4C566A")
                                ),
                                box(width = NULL, align="center", height = 'auto',
                                    status = "primary", solidHeader = FALSE,
                                    plotlyOutput("sectorsPlot") %>% withSpinner(color ="#4C566A")
                                )
                              )
                      ), #Industries
                      tabItem(tabName = "regions",
                              h2("Regional comparison by Sector/Group"),
                              offset=0, style='padding:3px;',
                              fluidRow(
                                # Select industry group
                                pickerInput(
                                  inputId = "groupPicker2",
                                  label = "Choose industry group:",
                                  choices = sort(unique(registerPC$Group)),
                                  choicesOpt = list(
                                    subtext = unique(registerPC$Group.name)[order(unique(registerPC$Group))]
                                  ),
                                  selected = 11,
                                  multiple = FALSE,
                                  options = list(
                                    `live-search`= TRUE,
                                    size = 7
                                    #             `action-box` = TRUE
                                  )
                                )
                              ),
                              fluidRow(
                                box(width = NULL, align="center", height = 'auto',
                                    status = "primary", solidHeader = FALSE,
                                    plotlyOutput("groupsRegion") %>% withSpinner(color ="#4C566A")
                                ),
                                box(width = NULL, align="center", height = 'auto',
                                    status = "primary", solidHeader = FALSE,
                                    plotlyOutput("sectorsRegion") %>% withSpinner(color ="#4C566A")
                                )
                              )
                      ) #Regions
                    ) #Tabs

                ) #Body
  ) #Page
) #UI