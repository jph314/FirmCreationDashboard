# UI ----
ui <- fluidPage(
  use_googlefont("Fira Sans"),
  dashboardPage(
    title = "UK COVID-19 Firm Creation Dashboard",
    dashboardHeader(
      # Header
      title = shinyDashboardLogoDIY(
        boldText = "Dashboard",
        mainText = "App",
        textSize = 16,
        badgeText = "beta",
        badgeTextColor = "white",
        badgeTextSize = 2,
        badgeBackColor = "#5e81ac",
        badgeBorderRadius = 3
      ),
      tags$li(a(
        href = "https://www.ukfirmcreation.com/",
        img(
          src = "logo.png",
          title = "Project website", height = "30px"
        ),
        style = "padding-top:10px; padding-bottom:10px;", target = "_blank"
      ),
      class = "dropdown"
      ),
      menuItemOutput("info"),
      menuItemOutput("authors")
    ), # End of header
    dashboardSidebar(
      # Sidebar
      ## to use font-awesome icons 6, I add the following line
      ## when needed in the app, I use `icon(name-of-icon)`
      tags$style("@import url(https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.1.1/css/all.min.css);"),
      sidebarMenu(
        id = "tabs",
        menuItem("Home", tabName = "home", icon = icon("home")),
        menuItem("Incorporations",
          icon = icon("shop"),
          menuSubItem("Aggregate Analysis", tabName = "AggStats", icon = icon("bullseye")),
          menuSubItem("Sectoral Analysis", icon = icon("industry"), tabName = "industries"),
          menuSubItem("Regional Analysis", icon = icon("map-marker-alt"), tabName = "regions")
        ),
        menuItem("Dissolutions",
          icon = icon("shop-slash"), tabName = "dissolutions",
          menuSubItem("Aggregate Analysis", tabName = "AggStatsDis", icon = icon("bullseye")),
          menuSubItem("Sectoral Analysis", icon = icon("industry"), tabName = "industriesDis"),
          menuSubItem("Regional Analysis", icon = icon("map-marker-alt"), tabName = "regionsDis")
        ),
        menuItem("Get custom data", icon = icon("database"), tabName = "customData")
      ),
      ### data selection: latest register vs. archive
      radioButtons(
        inputId = "pickData",
        label = "Choose data:",
        choices = c(
          "Latest register",
          "Archive"
        ),
        #  selected = "Latest register",
        inline = TRUE
      ), # End of pickData

      ### date select
      dateRangeInput(
        inputId = "dateAgg",
        label = "Choose date range:",
        start = startDate,
        end = endDate,
        min = startDate,
        max = endDate + 30,
        startview = "month", weekstart = 0,
        language = "en", separator = " to ", width = "100%"
      ),
      pickerInput(
        inputId = "pickCountry",
        label = "Choose region:",
        choices = c(
          "United Kingdom",
          "England",
          "London",
          "England excl. London",
          "Scotland",
          "Wales",
          "Northern Ireland"
        ),
        selected = "United Kingdom",
        multiple = FALSE
      )
    ), # End of sidebar
    dashboardBody(
      # UKFirmCreationTheme,
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
                                }'))),
      tags$script(HTML('
      $(document).ready(function() {
        $("header").find("nav").append(\'<span class="myClass"> <strong>Data last update:</strong> May 1, 2022 </span>\');
      })')),
      tabItems(
        tabItem(
          tabName = "home",
          withMathJax(includeMarkdown("www/home.md"))
        ),
        ## AGGREGATE STATS ====
        tabItem(
          tabName = "AggStats",
          uiOutput("countryText"), # width=12,
          offset = 0, style = "padding:3px;",
          fluidRow(
            column(
              width = 6, style = "padding:2px;", height = "auto",
              valueBoxOutput("vboxTotal", width = NULL)
            ),
            column(
              width = 6, style = "padding:2px;", height = "auto",
              valueBoxOutput("vboxActive", width = NULL)
            )#,
            # column(
            #   width = 3, style = "padding:2px;", height = "auto",
            #   valueBoxOutput("vboxLD", width = NULL)
            # ),
            # column(
            #   width = 3, style = "padding:2px;", height = "auto",
            #   valueBoxOutput("vboxOpen", width = NULL)
            # )
          ),
          fluidRow(
            box(
              width = NULL, align = "center", height = "auto",
              status = "primary", solidHeader = FALSE,
              plotlyOutput("rollingAvg", height = "300px") %>% withSpinner(color = "#4C566A"),
              downloadButton("dailyRegDownload", "Download data as .csv")
            )
          ),
          fluidRow(
            box(
              width = NULL, align = "center", height = "auto",
              status = "primary", solidHeader = FALSE,
              plotlyOutput("UKmap", height = "600px") %>% withSpinner(color = "#4C566A"),
              downloadButton("NUTS2Download", "Download data")
            )
          ),
          fluidRow(
            box(
              width = NULL, align = "center", height = "auto",
              status = "primary", solidHeader = FALSE,
              plotlyOutput("treemap", height = "500px") %>% withSpinner(color = "#4C566A"),
              downloadButton("DivisionDownload", "Download data as .csv")
            )
          )
        ), # AggStats
        ## INDUSTRIES COMPARISON ====
        tabItem(
          tabName = "industries",
          h2("Incorporations: Industry comparison"),
          offset = 0, style = "padding:3px;",
          fluidRow(
            # Select industry groups
            pickerInput(
              inputId = "groupPicker",
              label = "Choose industry group(s):",
              choices = sort(unique(register1$Group)),
              choicesOpt = list(
                subtext = unique(register1$Group.name)[order(unique(register1$Group))]
              ),
              selected = 11,
              multiple = TRUE,
              options = list(
                `live-search` = TRUE,
                size = 7
                #             `action-box` = TRUE
              )
            )
          ),
          fluidRow(
            box(
              width = NULL, align = "center", height = "auto",
              status = "primary", solidHeader = FALSE,
              DT::dataTableOutput("groups") %>% withSpinner(color = "#4C566A")
            ),
            box(
              width = NULL, align = "center", height = "auto",
              status = "primary", solidHeader = FALSE,
              plotlyOutput("groupsPlot") %>% withSpinner(color = "#4C566A"),
              downloadButton("groupsDownload", "Download data as .csv")
            ),
            box(
              width = NULL, align = "center", height = "auto",
              status = "primary", solidHeader = FALSE,
              plotlyOutput("sectorsPlot") %>% withSpinner(color = "#4C566A"),
              downloadButton("sectorsDownload", "Download data as .csv")
            )
          )
        ), # Industries
        tabItem(
          tabName = "regions",
          h2("Incorporations: Regional comparison"),
          offset = 0, style = "padding:3px;",
          fluidRow(
            # Select local authority district
            pickerInput(
              inputId = "ladPicker",
              label = "Choose Local Authority District:",
              choices = sort(unique(register1$District)),
              # choicesOpt = list(
              #   subtext = unique(registerPC$Group.name)[order(unique(registerPC$Group))]
              # ),
              selected = "Canterbury",
              multiple = TRUE,
              options = list(
                `live-search` = TRUE,
                size = 7
                #             `action-box` = TRUE
              )
            )
          ),
          fluidRow(
            box(
              width = NULL, align = "center", height = "auto",
              status = "primary", solidHeader = FALSE,
              DT::dataTableOutput("districts") %>% withSpinner(color = "#4C566A")
            ),
            box(
              width = NULL, align = "center", height = "auto",
              status = "primary", solidHeader = FALSE,
              plotlyOutput("districtsPlot") %>% withSpinner(color = "#4C566A"),
              downloadButton("districtsDownload", "Download data as .csv")
            )
          )
        ), # Regions

        # Dissolutions ----
        tabItem(
          tabName = "AggStatsDis",
          uiOutput("countryTextDis"), # width=12,
          offset = 0, style = "padding:3px;",
          fluidRow(
            column(
              width = 3, style = "padding:2px;", height = "auto",
              valueBoxOutput("vboxTotalDis", width = NULL)
            ),
            column(
              width = 3, style = "padding:2px;", height = "auto",
              valueBoxOutput("vboxVaccineDis", width = NULL)
            ),
            column(
              width = 3, style = "padding:2px;", height = "auto",
              valueBoxOutput("vboxLDDis", width = NULL)
            ),
            column(
              width = 3, style = "padding:2px;", height = "auto",
              valueBoxOutput("vboxOpenDis", width = NULL)
            )
          ),
          fluidRow(
            box(
              width = NULL, align = "center", height = "auto",
              status = "primary", solidHeader = FALSE,
              plotlyOutput("rollingAvgDis", height = "300px") %>% withSpinner(color = "#4C566A"),
              downloadButton("dailyRegDisDownload", "Download data as .csv")
            )
          ),
          fluidRow(
            box(
              width = NULL, align = "center", height = "auto",
              status = "primary", solidHeader = FALSE,
              plotlyOutput("UKmapDis", height = "600px") %>% withSpinner(color = "#4C566A"),
              downloadButton("NUTS2DisDownload", "Download data")
            )
          ),
          fluidRow(
            box(
              width = NULL, align = "center", height = "auto",
              status = "primary", solidHeader = FALSE,
              plotlyOutput("donutDis", height = "400px") %>% withSpinner(color = "#4C566A"),
              downloadButton("SectorDownDis", "Download data as .csv")
            ),
            box(
              width = NULL, align = "center", height = "auto",
              status = "primary", solidHeader = FALSE,
              plotlyOutput("treemapDis", height = "500px") %>% withSpinner(color = "#4C566A"),
              downloadButton("DivisionDisDownload", "Download data as .csv")
            )
          )
        ), # Aggregate dissolutions
        ## INDUSTRIES COMPARISON dissolutions ====
        tabItem(
          tabName = "industriesDis",
          h2("Dissolutions: Industry comparison"),
          offset = 0, style = "padding:3px;",
          fluidRow(
            # Select industry groups
            pickerInput(
              inputId = "groupPickerDis",
              label = "Choose industry group(s):",
              choices = sort(unique(dissolutions$Group)),
              choicesOpt = list(
                subtext = unique(dissolutions$Group.name)[order(unique(dissolutions$Group))]
              ),
              selected = 15,
              multiple = TRUE,
              options = list(
                `live-search` = TRUE,
                size = 7
                #             `action-box` = TRUE
              )
            )
          ),
          fluidRow(
            box(
              width = NULL, align = "center", height = "auto",
              status = "primary", solidHeader = FALSE,
              dataTableOutput("groupsDis") %>% withSpinner(color = "#4C566A")
            ),
            box(
              width = NULL, align = "center", height = "auto",
              status = "primary", solidHeader = FALSE,
              plotlyOutput("groupsPlotDis") %>% withSpinner(color = "#4C566A"),
              downloadButton("groupsDownloadDis", "Download data as .csv")
            ),
            box(
              width = NULL, align = "center", height = "auto",
              status = "primary", solidHeader = FALSE,
              plotlyOutput("sectorsPlotDis") %>% withSpinner(color = "#4C566A"),
              downloadButton("sectorsDownloadDis", "Download data as .csv")
            )
          )
        ), # Industries dissolutions

        ### Region Dissolutions
        tabItem(
          tabName = "regionsDis",
          h2("Dissolutions: Regional comparison by Sector/Group"),
          offset = 0, style = "padding:3px;",
          fluidRow(
            # Select industry group
            pickerInput(
              inputId = "groupPicker2Dis",
              label = "Choose industry group:",
              choices = sort(unique(dissolutions$Group)),
              choicesOpt = list(
                subtext = unique(dissolutions$Group.name)[order(unique(dissolutions$Group))]
              ),
              selected = 11,
              multiple = FALSE,
              options = list(
                `live-search` = TRUE,
                size = 7
                #             `action-box` = TRUE
              )
            )
          ),
          fluidRow(
            box(
              width = NULL, align = "center", height = "auto",
              status = "primary", solidHeader = FALSE,
              plotlyOutput("groupsRegionDis") %>% withSpinner(color = "#4C566A"),
              downloadButton("regionGroupDownloadDis", "Download data as .csv")
            ),
            box(
              width = NULL, align = "center", height = "auto",
              status = "primary", solidHeader = FALSE,
              plotlyOutput("sectorsRegionDis") %>% withSpinner(color = "#4C566A"),
              downloadButton("regionSectorDownloadDis", "Download data as .csv")
            )
          )
        ), # Regions dissolutions

        ## Get custom data ----
        tabItem(
          tabName = "customData",
          h2("Get custom incorporation data"),
          # h3("Select the date range, postcode(s) and SIC sectors"),
          offset = 0, style = "padding:3px;",
          fluidRow(
            column(
              width = 5, style = "padding:2px;", height = "auto",
              # Select your postcode
              pickerInput(
                inputId = "pickPostcode",
                label = "Choose postcode district:",
                choices = sort(unique(register1$District)),
                # choicesOpt = list(
                #  subtext = unique(registerPC$Group.name)[order(unique(registerPC$Group))]
                # ),
                selected = "AB10",
                multiple = TRUE,
                options = list(
                  `actions-box` = TRUE, # build buttons for collective selection
                  `live-search` = TRUE,
                  size = 7
                )
              ), # picker postcode
            ),
            column(
              width = 5, style = "padding:2px;", height = "auto",
              # select 4-digit SIC code
              pickerInput(
                inputId = "pickSIC",
                label = "Choose SIC Class:",
                choices = sort(unique(register1$Class)),
                choicesOpt = list(
                  subtext = unique(register1$Class.name)[order(unique(register1$Class))]
                ),
                selected = 150,
                multiple = TRUE,
                options = list(
                  `actions-box` = TRUE, # build buttons for collective selection
                  `live-search` = TRUE,
                  size = 7
                  #             `action-box` = TRUE
                )
              ) # picker SIC code
            ) # box
          ),
          fluidRow(
            column(
              12,
              dataTableOutput("customdata") %>% withSpinner(color = "#4C566A"),
              downloadButton("customDownload", "Download data as .csv")
            )
          )
        ) # custom data
      ) # Tabs
    ) # Body
  ) # Page
) # UI
