#---
# Shiny App for UK COVID-19 Firm Creation
# Link to project's website:
# Yannis Galanakis; <i.galanakis@kent.ac.uk>
# February 2021
#---

# Libraries
library(tidyverse)
library(naniar)
library(haven)
library(survey)
library(rAmCharts)
library(data.table)
library(lubridate)
library(ggalt)
library(nord)
library(cowplot)
library(animation)
library(patchwork)
library(sp)
library(scales)
library(raster)
library(rgeos)
library(mapproj)
library(rgdal)
library(maptools)
library(emojifont)
library(paletteer)
library(plotly)
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(shinydashboard)
library(DT)
library(pipeR)
library(shinyFeedback)
library(shinycssloaders)
library(dashboardthemes)
library(fresh)
library(zoo)
library(treemap)
library(highcharter)
library(RColorBrewer)
library(rjson)
library(geojsonio)
library(leaflet)
library(htmlwidgets)
library(leaflet.extras)
library(shinyBS)
library(waiter)

convert <- fread("data/convert.csv", sep = ",")
registerSecReg <- fread("data/registerSectorsRegionsUK.csv", sep = ",")
registerSecReg$IncorporationDate <- as.Date(registerSecReg$IncorporationDate, "%d/%m/%Y")
registerSecReg <- registerSecReg %>% rename(date = IncorporationDate)
registerSecReg <- registerSecReg[order(Group)]
pc2nuts <- fread("data/UKpc2NUTS.csv", sep = ",")
pc2nuts <- pc2nuts %>% rename(postcode = postcodeUnit)
regionsUK <- fread("data/regionsUK.csv", sep = ",")
regionsUK <- regionsUK %>% rename(postcodeArea = PostcodeArea)

# Begin Exclude Linting
# Theme: UK COVID-19 Firm Creation ------------------------------------------------------------------------------
#' @title theme_UKFirmCreationTheme
#' @description UK COVID-19 Firm Creation theme for a shinydashboard application
#'
#' @return Object produced by shinyDashboardThemeDIY
#' @seealso \code{\link{shinyDashboardThemeDIY}}
#' @export
# Custom Theme ----
UKFirmCreationTheme <- shinyDashboardThemeDIY(
  ### general
  appFontFamily = "Fira Sans"
  ,appFontColor = "#2D2D2D"
  ,primaryFontColor = "#FFFFFF"
  ,infoFontColor = "#FFFFFF"
  ,successFontColor = "#FFFFFF"
  ,warningFontColor = "#FFFFFF"
  ,dangerFontColor = "#FFFFFF"
  ,bodyBackColor = "#FFFFFF"

  ### header
  ,logoBackColor = "#4B556A"

  ,headerButtonBackColor = "#4C566A"
  ,headerButtonIconColor = "#FFFFFF"
  ,headerButtonBackColorHover = "#E0DCDC"
  ,headerButtonIconColorHover = "#6E6E6E"

  ,headerBackColor = "#4C566A"
  ,headerBoxShadowColor = ""
  ,headerBoxShadowSize = "0px 0px 0px"

  ### sidebar
  ,sidebarBackColor = "#FFFFFF"
  ,sidebarPadding = "0"

  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = "0"
  ,sidebarMenuBorderRadius = 0

  ,sidebarShadowRadius = ""
  ,sidebarShadowColor = "0px 0px 0px"

  ,sidebarUserTextColor = "#737373"

  ,sidebarSearchBackColor = "#F0F0F0"
  ,sidebarSearchIconColor = "#646464"
  ,sidebarSearchBorderColor = "#DCDCDC"

  ,sidebarTabTextColor = "#646464"
  ,sidebarTabTextSize = "14"
  ,sidebarTabBorderStyle = "none"
  ,sidebarTabBorderColor = "none"
  ,sidebarTabBorderWidth = "0"

  ,sidebarTabBackColorSelected = "#E6E6E6"
  ,sidebarTabTextColorSelected = "#000000"
  ,sidebarTabRadiusSelected = "0px"

  ,sidebarTabBackColorHover = "#F5F5F5"
  ,sidebarTabTextColorHover = "#000000"
  ,sidebarTabBorderStyleHover = "none solid none none"
  ,sidebarTabBorderColorHover = "#C8C8C8"
  ,sidebarTabBorderWidthHover = "4"
  ,sidebarTabRadiusHover = "0px"

  ### boxes
  ,boxBackColor = "#FFFFFF"
  ,boxBorderRadius = "5"
  ,boxShadowSize = "none"
  ,boxShadowColor = ""
  ,boxTitleSize = "18"
  ,boxDefaultColor = "#FFFFFF"
  ,boxPrimaryColor = "#4C566A"
  ,boxInfoColor = "#B4B4B4"
  ,boxSuccessColor = "#A3BE8C"
  ,boxWarningColor = "#D08770"
  ,boxDangerColor = "#BF616A"

  ,tabBoxTabColor = "#F8F8F8"
  ,tabBoxTabTextSize = "14"
  ,tabBoxTabTextColor = "#646464"
  ,tabBoxTabTextColorSelected = "#2D2D2D"
  ,tabBoxBackColor = "#F8F8F8"
  ,tabBoxHighlightColor = "#C8C8C8"
  ,tabBoxBorderRadius = "5"

  ### inputs
  ,buttonBackColor = "#D8DEE9"
  ,buttonTextColor = "#2D2D2D"
  ,buttonBorderColor = "#D8DEE9"
  ,buttonBorderRadius = "5"

  ,buttonBackColorHover = "#BEBEBE"
  ,buttonTextColorHover = "#000000"
  ,buttonBorderColorHover = "#969696"

  ,textboxBackColor = "#FFFFFF"
  ,textboxBorderColor = "#767676"
  ,textboxBorderRadius = "5"
  ,textboxBackColorSelect = "#ECEFF4"
  ,textboxBorderColorSelect = "#6C6C6C"

  ### tables
  ,tableBackColor = "#FCFCFC"
  ,tableBorderColor = "#F5F5F5"
  ,tableBorderTopSize = "1"
  ,tableBorderRowSize = "1"
)
# End Exclude Linting


# Define UI ----
ui <- fluidPage(
    use_googlefont("Fira Sans"),

    dashboardPage( title="UK COVID-19 Firm Creation Dashboard",
        ### ui header
        dashboardHeader(

            ### changing logo
            title = shinyDashboardLogoDIY(

              boldText = "Dashboard"
              ,mainText = "App"
              ,textSize = 16
              ,badgeText = "beta"
              ,badgeTextColor = "white"
              ,badgeTextSize = 2
              ,badgeBackColor = "#5e81ac"
              ,badgeBorderRadius = 3
            ),

            tags$li(a(href = 'https://www.ukfirmcreation.com/',
                      img(src = 'logo.png',
                          title = "Project website", height = "30px"),
                      style = "padding-top:10px; padding-bottom:10px;", target = "_blank"),
                    class = "dropdown"),

            menuItemOutput("info"),
            menuItemOutput("authors")

        ), # End of header

    dashboardSidebar(
        sidebarMenu(
            # Setting id makes input$tabs give the tabName of currently-selected tab
            id = "tabs",
            menuItem("Home", tabName = "home", icon = icon("home")),
            menuItem("Aggregate Analysis", tabName = "AggStats", icon = icon("bullseye")),
       #     menuItem("Regional Analysis", icon = icon("flag"), tabName = "country"),
            menuItem("Sectoral Analysis", icon = icon("industry"), tabName = "industry",
                     menuSubItem("Group(s) per Country", tabName = "industriesCountry"),
                     menuSubItem("Group by Country", tabName = "industryCountries")
                     )
       #     menuItem("Raw Data", icon = icon("database"), tabName="rawdata")
        )
    ), # End of Sidebar

    # body
    dashboardBody(
        ### changing theme
      UKFirmCreationTheme,

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

                                '))),
      tags$script(HTML('
      $(document).ready(function() {
        $("header").find("nav").append(\'<span class="myClass"> <strong>Data last update:</strong> August 3, 2021 </span>\');
      })
     ')),
      # tags$a(href='https://uk-covid19-firm-creation.netlify.app/',
      #        tags$img(src="logo.png", title="Project Website",height='45', width='45')
      # ),

      use_garcon(),
      use_waiter(),
      waiter_show_on_load(
        tags$img(
          src="logo.png",
          height=200,
          id = "myLogo" # set id
        ),
      ),

        # AGGREGATE STATS ----
        mainPanel(
         tabItems(
           tabItem(tabName="home",
                   withMathJax(includeMarkdown("www/home.md"))
           ),

            tabItem(tabName = "AggStats",
                    h2("Statistics of companies in the UK"), width = 12,
                    offset = 0, style='padding:0px;',
            #       fluidRow(
                      radioGroupButtons(
                        inputId = "pickCountry",
                        label = "Choose country:",
                        choices = c("United Kingdom" =  "UK",
                                    "England"        =  "Eng",
                                    "Northern Ireland"  = "NI",
                                    "Scotland"    = "Scotland",
                                    "Wales"   = "Wales"),
                        selected = "UK",
                        justified = TRUE
                      ),
                    column(width = 3,offset = 0, style='padding:0px;',
                           # box(width=NULL,
                           #     fluidRow(
                           #         column(width= 12,
                                          ### date slider
                                          dateRangeInput(inputId = "dateAgg",
                                                       label   = "Choose date range:",
                                                       start = as.Date("2021-01-01"),
                                                       end = as.Date(max(registerSecReg$date)),
                                                       min = as.Date("2020-01-01"),
                                                       max = as.Date(max(registerSecReg$date)+30),
                                                       startview = "month", weekstart = 0,
                                                       language = "en", separator = " to ", width = "100%") # dateAgg
                        #    ) #column
                        # )#fluidRow
                        #    ) #box
                    ),
                    column(width = 9,
                        column(width = 3,
                               valueBoxOutput("UKvbox", width = NULL)
                        ),#column
                      column(width = 3,
                             valueBoxOutput("regLockI", width = NULL)
                    ),
                      column(width =3,
                             valueBoxOutput("regRecentLock", width= NULL)
                             ),
                      column(width = 3,
                             valueBoxOutput("regRecentEaseLock", width=NULL)
                             )

                    ),

                  #  ), #column
                    # fluidRow(
                    #   column(width=3,
                    #          ),
                    #
                    # column(width = 9,
                    #        box(width = NULL, align="center", height = 'auto',
                    #            status = "primary", solidHeader = FALSE,
                    #            withSpinner(DTOutput("sumStats"),
                    #                        type = 4, color = "#4c566a", size = 2),
                    #            inlineCSS(list("table" = "font-size: 12px")),
                    #            p(""),
                    #        )
                    # )
                    # ),
                    #fluidRow(
                      column(width = 6,
                             box(width = NULL, align="center", height = 'auto',
                                 status = "primary", solidHeader = FALSE,
                                 plotlyOutput("histogramTotal", height='250px') %>% withSpinner(color ="#4C566A"),

                                 p(""),
                             )
                             ),
                      column(width = 6,
                             box(width = NULL, align="center", height = 'auto',
                                 status = "primary", solidHeader = FALSE,


                                 prettySwitch(
                                   inputId = "lockdown",
                                   label = "Show national lockdown periods"
                                 ),

                                 p(""),
                                 plotlyOutput("rollAv", height = '250px'),
                                 p("")

                      )
                    ),

                    #       ),# histogram box
                   # fluidRow(
                      column(width = 6,
                             box(width = NULL, align = 'center', height = 'auto',
                                 status = 'primary', solidHeader = F,
                                 amChartsOutput(outputId = "sectorsChartFull", width = "100%", height = '700px')

                             )),
                      column(width = 6,
                             box(width = NULL, align = 'center', height = 'auto',
                                 status = 'primary', solidHeader = F,
                                 leafletOutput("UKmap", width = "100%", height = '700px')

                             )),
                  #  ),

                 #   fluidRow(
                      column(width = 12,
                             box(width = NULL, align = 'center', height = 'auto',
                                 status = 'primary', solidHeader = F,
                                 plotlyOutput(outputId = "treemap", width = "100%", height = "650px")

                             ))
                  #  )




                    ),#tabItemAggStats
            tabItem(tabName = "industriesCountry",
                    h2("Statistics by industry"),width = 12, height = NULL,
                    h3("Multiple industries per country"),
                    fluidRow(
                      column(width = 3,
                             box(width=NULL,
                                 fluidRow(
                                   column(width= 12,
                      dateRangeInput(inputId = "dateIndustry",
                                     label   = "Choose date range:",
                                     start = as.Date("2021-01-01"),
                                     end = as.Date(max(registerSecReg$date)),
                                     min = as.Date("2019-01-01"),
                                     max = as.Date(max(registerSecReg$date)+30),
                                     startview = "month", weekstart = 0,
                                            language = "en", separator = " to ", width = "100%")
                                )
                                 )
                             ), #box

                      # choose country
                             box(width=NULL,
                                 fluidRow(
                                   column(width = 12,
                                          pickerInput(
                                            inputId = "CountryPicker",
                                            label = "Choose country:",
                                            choices = c("United Kingdom" =  "UK",
                                                        "London"        =  "LDN",
                                                        "England (exc. London)" = "Eng",
                                                        "Northern Ireland"  = "NI",
                                                        "Scotland"    = "Scotland",
                                                        "Wales"   = "Wales"),
                                            selected = "UK",
                                            multiple = F
                                          ))
                                 )

                             ), #box country

                         box(width = NULL,
                                 fluidRow(
                                   column(width = 12,
                                          div(
                                            div(
                                              # edit1
                                              style="width:80%; display:inline-block; vertical-align: middle;",
                                          #class
                                          pickerInput(
                                            inputId = "groupPicker",
                                            label = "Choose industry group(s):",
                                            choices = unique(registerSecReg$Group),
                                            choicesOpt = list(
                                               subtext = unique(paste0(registerSecReg$Group.name))
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
                                 div(
                                   # edit2
                                   style="display:inline-block; vertical-align: middle;",
                                   bsButton("e", label = "", icon = icon("info"),
                                            style = "info"),
                                   bsPopover(id = "e", title = "Industry group(s)",
                                             content = paste0("We use 3-digit SIC levels (ONS Groups) and group them into broader sectors (1-digit; ONS Sections). <br> You may select <strong>multiple</strong> groups."),
                                             placement = "right",
                                             trigger = "click",
                                             options = list(container = "body"))
                                 ))))
                             ), # box group
                        box(width = NULL,
                            fluidRow(
                              column(width = 12,
                                # 7days average
                               radioGroupButtons(
                                  inputId = "graphPref",
                                  label = "Chart Preferences:",
                                  choices = c("Daily" = "daily",
                                              "7-day rolling average" = "7day"),
                                  individual = TRUE,
                                  checkIcon = list(
                                    yes = tags$i(class = "fa fa-circle",
                                                 style = "color: steelblue"),
                                    no = tags$i(class = "fa fa-circle-o",
                                                style = "color: steelblue"))
                                )
                              )
                            )
                            )
                             ),# initial column=3
                      column(width = 9,
                             box(width = NULL,
                                 status = "primary", solidHeader = FALSE,
                                 p("The figures depend on the following industry classification:"),
                                 dataTableOutput("group1"),
                                 withSpinner(plotlyOutput(outputId ="classPlot", width = "100%"),
                                             type = 5, color = "#4c566a", size = 2),
                                 p("The following figure aggregates all the 3-digit Groups that belong in the same Section."),
                                 withSpinner(plotlyOutput(outputId ="sectorPlot", width = "100%"),
                                             type = 5, color = "#4c566a", size = 2)
                             )
                             ) # main column with graphs
                    )# initial fluidRow

            ),#tabItemindustriesCountry
            tabItem(tabName = "industryCountries",
                    h2("Statistics by industry"), width = 12,
                    h3("Industry by country"),
                    fluidRow(
                      column(width = 3,
                             box(width=NULL,
                                 fluidRow(
                                   column(width= 12,
                                          dateRangeInput(inputId = "dateIndustry2",
                                                         label   = "Choose date range:",
                                                         start = as.Date("2021-01-01"),
                                                         end = as.Date(max(registerSecReg$date)),
                                                         min = as.Date("2019-01-01"),
                                                         max = as.Date(max(registerSecReg$date)+30),
                                                         startview = "month", weekstart = 0,
                                                         language = "en", separator = " to ", width = "100%")
                                   )
                                 )
                             ), #box

                             box(width = NULL,
                                 fluidRow(
                                   column(width = 12,
                                          div(
                                            div(
                                              # edit1
                                              style="width:80%; display:inline-block; vertical-align: middle;",
                                              #class
                                              pickerInput(
                                                inputId = "groupPicker2",
                                                label = "Choose industry group:",
                                                choices = unique(registerSecReg$Group),
                                                choicesOpt = list(
                                                  subtext = unique(paste0(registerSecReg$Group.name))
                                                ),
                                                selected = 11,
                                                multiple = F,
                                                options = list(
                                                  `live-search`= TRUE,
                                                  size = 7
                                                  #             `action-box` = TRUE
                                                )
                                              )
                                            ),
                                            div(
                                              # edit2
                                              style="display:inline-block; vertical-align: middle;",
                                              bsButton("e", label = "", icon = icon("info"),
                                                       style = "info"),
                                              bsPopover(id = "e", title = "Industry group(s)",
                                                        content = paste0("We use 3-digit SIC levels (ONS Groups) and group them into broader sectors (1-digit; ONS Sections)."),
                                                        placement = "right",
                                                        trigger = "click",
                                                        options = list(container = "body"))
                                            ))))
                             ), # box group
                             box(width = NULL,
                                 fluidRow(
                                   column(width = 12,
                                          # 7days average
                                          radioGroupButtons(
                                            inputId = "graphPref2",
                                            label = "Chart Preferences:",
                                            choices = c("Daily" = "daily",
                                                        "7-day rolling average" = "7day"),
                                            individual = TRUE,
                                            checkIcon = list(
                                              yes = tags$i(class = "fa fa-circle",
                                                           style = "color: steelblue"),
                                              no = tags$i(class = "fa fa-circle-o",
                                                          style = "color: steelblue"))
                                          )
                                   )
                                 )
                             )
                      ),# initial column=3
                      column(width = 9,
                             box(width = NULL,
                                 status = "primary", solidHeader = FALSE,
                                 p("The figures depend on the following industry classification:"),
                                 dataTableOutput("group2"),
                                 withSpinner(plotlyOutput(outputId ="classPlot2", width = "100%"),
                                             type = 5, color = "#4c566a", size = 2),
                                 p("The following figure aggregates all the 3-digit Groups that belong in the same Section by country."),
                                 withSpinner(plotlyOutput(outputId ="sectorPlot2", width = "100%"),
                                             type = 5, color = "#4c566a", size = 2)
                             )
                      ) # main column with graphs
                    )# initial fluidRow

            )#tabItemindustriesCountry
            )



        ) # End of body
    )#dashboardbody
 )#dashboardpage
)#ui


# Define server ----
server <- function(input, output) {



  g <- Garcon$new("myLogo", filter = "opacity",
                  direction = "bt")

  for(i in 1:10){
    Sys.sleep(runif(1))
    g$set(i * 10)
  }

  waiter_hide()

  start_ldI <- "2020-03-23"
  end_ldI <- "2020-07-04"
  start_ldII <- "2020-11-05"
  end_ldII <- "2020-12-02"
  start_tier4 <- "2020-12-20"
  start_ldIII <- "2021-01-05"
  end_ldIII <- "2021-04-12"


  duringldIII <- subset(registerSecReg, date>=start_ldIII &
                          date < end_ldIII)
  duringldIII <- duringldIII %>% group_by(date) %>% count()
  med_incorp_during_ldIII <- median(duringldIII$n)

    duringldIIIENG <- subset(registerSecReg, date>=start_ldIII
                             & date < end_ldIII & Country == "England")
    duringldIIIENG <- duringldIIIENG %>% group_by(date) %>% count()
    med_incorp_during_ldIIIENG <- median(duringldIIIENG$n)

    #NI
    duringldIIINI <- subset(registerSecReg, date>=start_ldIII
                            & date < end_ldIII & Country == "Northern Ireland")
    duringldIIINI <- duringldIIINI %>% group_by(date) %>% count()
    med_incorp_during_ldIIINI <- median(duringldIIINI$n)

    # Scotland
    duringldIIISC <- subset(registerSecReg, date>=start_ldIII
                            & date < end_ldIII & Country == "Scotland")
    duringldIIISC <- duringldIIISC %>% group_by(date) %>% count()
    med_incorp_during_ldIIISC<- median(duringldIIISC$n)

    # WALES
    duringldIIIW <- subset(registerSecReg, date>=start_ldIII
                           & date < end_ldIII & Country == "Wales")
    duringldIIIW <- duringldIIIW %>% group_by(date) %>% count()
    med_incorp_during_ldIIIW <- median(duringldIIIW$n)

    # info
    output$info <-renderMenu({
        dropdownMenu(type = "messages",
                           icon = icon("info"), badgeStatus = "primary",
                     headerText = tags$b("Project ID"),
                     messageItem(
                         from = "University of Kent",
                         message = "School of Economics",
                         icon = icon("institution"),
                     ),
                     messageItem(
                         from = "ESRC-funded project",
                         message = "2020-2022",
                         icon = icon("pound-sign"),
                     ),
                     messageItem(
                         from = "Contact",
                         message = "Yannis Galanakis",
                         href = "mailto:i.galanakis@kent.ac.uk",
                         icon = icon("envelope"),
                     )
                     )
    })# End info

    #authors
    output$authors<-renderMenu({
        dropdownMenu(type = "notifications", icon = icon("users"), badgeStatus = "primary",
                     headerText = tags$b("Team"),
                     notificationItem(icon = icon("user"), status = "info",
                                      "Alfred Duncan",
                                      href = 'https://www.alfredduncan.co.uk/'),
                     notificationItem(icon = icon("user"), status = "info",
                                      "Yannis Galanakis", href = 'https://ygalanak.github.io/'),
                     notificationItem(icon = icon("user"), status = "info",
                                      "Miguel León-Ledesma", href = 'https://sites.google.com/site/miguelleonledesmaspernonalsite/Home'),
                     notificationItem(icon = icon("user"), status = "info",
                                      "Anthony Savagar", href= 'https://www.asavagar.com/'
                     ))
    }) # End authors

    Tfirmsfilt <- reactive({
      registerSecReg %>%
        filter(date >= input$dateAgg[1] &
                                   date <= input$dateAgg[2])
    })


    output$UKvbox <- renderValueBox({
      if (input$pickCountry == 'UK') {

      Tfirms <- Tfirmsfilt() %>%
        group_by(date) %>%
        count()

      valueBox(
        value = prettyNum(sum(Tfirms$n),big.mark=",",scientific=FALSE),
        subtitle = paste0("New registrations between ", input$dateAgg[1], " and ", input$dateAgg[2]),
        color = "aqua",
        icon = icon("building")
      )
      } # COUNTRY IS UK
      else if (input$pickCountry == 'Eng'){
        Tfirmsfilt <- Tfirmsfilt() %>% filter(Country == "England")

        Tfirms <- Tfirmsfilt %>%
          group_by(date) %>%
          count()

        valueBox(
          value = prettyNum(sum(Tfirms$n),big.mark=",",scientific=FALSE),
          subtitle = paste0("New registrations between ", input$dateAgg[1], " and ", input$dateAgg[2]),
          color = "aqua",
          icon = icon("building")
        )
      } # country is England
      else if (input$pickCountry == 'NI'){
        Tfirmsfilt <- Tfirmsfilt() %>% filter(Country == "Northern Ireland")

        Tfirms <- Tfirmsfilt %>%
          group_by(date) %>%
          count()

        valueBox(
          value = prettyNum(sum(Tfirms$n),big.mark=",",scientific=FALSE),
          subtitle = paste0("New registrations between ", input$dateAgg[1], " and ", input$dateAgg[2]),
          color = "aqua",
          icon = icon("building")
        )
      }# country is NI
      else if (input$pickCountry == 'Scotland'){
        Tfirmsfilt <- Tfirmsfilt() %>% filter(Country == "Scotland")
        Tfirms <- Tfirmsfilt %>%
          group_by(date) %>%
          count()

        valueBox(
          value = prettyNum(sum(Tfirms$n),big.mark=",",scientific=FALSE),
          subtitle = paste0("New registrations between ", input$dateAgg[1], " and ", input$dateAgg[2]),
          color = "aqua",
          icon = icon("building")
        )
      } # COUNTRY IS SCOTLAND
      else if (input$pickCountry == 'Wales'){
        Tfirmsfilt <- Tfirmsfilt() %>% filter(Country == "Wales")

        Tfirms <- Tfirmsfilt %>%
          group_by(date) %>%
          count()

        valueBox(
          value = prettyNum(sum(Tfirms$n),big.mark=",",scientific=FALSE),
          subtitle = paste0("New registrations between ", input$dateAgg[1], " and ", input$dateAgg[2]),
          color = "aqua",
          icon = icon("building")
        )
      } #COUNTRY IS WALES
    }) #total registration in selected period
    output$regLockI <- renderValueBox({
      if (input$pickCountry == 'UK') {
      TflckIfilt <- registerSecReg[registerSecReg$date >= "2020-12-08" &
                                        registerSecReg$date <= input$dateAgg[2]]
      TflckI <- TflckIfilt %>%
        group_by(date) %>%
        count()

      valueBox(
        value = prettyNum(sum(TflckI$n),big.mark=",",scientific=FALSE),
        subtitle = paste("New registrations between first vaccine and", input$dateAgg[2]),
        color = "light-blue",
        icon = icon("syringe")
      )
      } # country is UK
      else if (input$pickCountry == 'Eng') {
        TflckIfilt <- registerSecReg[registerSecReg$date >= "2020-12-08" &
                                           registerSecReg$date <= input$dateAgg[2] &
                                           registerSecReg$Country == "England" ]
        TflckI <- TflckIfilt %>%
          group_by(date) %>%
          count()

        valueBox(
          value = prettyNum(sum(TflckI$n),big.mark=",",scientific=FALSE),
          subtitle = paste("New registrations between first vaccine and", input$dateAgg[2]),
          color = "light-blue",
          icon = icon("syringe")
        )
      } # country is England
      else if (input$pickCountry == 'NI') {
        TflckIfilt <- registerSecReg[registerSecReg$date >= "2020-12-08" &
                                             registerSecReg$date <= input$dateAgg[2] &
                                             registerSecReg$Country == "Northern Ireland" ]
        TflckI <- TflckIfilt %>%
          group_by(date) %>%
          count()

        valueBox(
          value = prettyNum(sum(TflckI$n),big.mark=",",scientific=FALSE),
          subtitle = paste("New registrations between first vaccine and", input$dateAgg[2]),
          color = "light-blue",
          icon = icon("syringe")
        )
      } # country is NI
      else if (input$pickCountry == 'Scotland') {
        TflckIfilt <- registerSecReg[registerSecReg$date >= "2020-12-08" &
                                             registerSecReg$date <= input$dateAgg[2] &
                                             registerSecReg$Country == "Scotland" ]
        TflckI <- TflckIfilt %>%
          group_by(date) %>%
          count()

        valueBox(
          value = prettyNum(sum(TflckI$n),big.mark=",",scientific=FALSE),
          subtitle = paste("New registrations between first vaccine and", input$dateAgg[2]),
          color = "light-blue",
          icon = icon("syringe")
        )
      } # country is Scotland
      else if (input$pickCountry == 'Wales') {
        TflckIfilt <- registerSecReg[registerSecReg$date >= "2020-12-08" &
                                             registerSecReg$date <= input$dateAgg[2] &
                                             registerSecReg$Country == "Wales" ]
        TflckI <- TflckIfilt %>%
          group_by(date) %>%
          count()

        valueBox(
          value = prettyNum(sum(TflckI$n),big.mark=",",scientific=FALSE),
          subtitle = paste("New registrations between first vaccine and", input$dateAgg[2]),
          color = "light-blue",
          icon = icon("syringe")
        )
      } # country is Wales
    })#registrations since first vaccine
    output$regRecentLock <- renderValueBox({
      if (input$pickCountry == 'UK'){
      TfrecentlckIfilt <- registerSecReg[registerSecReg$date >= "2021-01-05" &
                                                 registerSecReg$date <= "2021-04-11" ]
      TrflckI <- TfrecentlckIfilt %>%
        group_by(date) %>%
        count()

      valueBox(
        value = prettyNum(sum(TrflckI$n),big.mark=",",scientific=FALSE),
        subtitle = paste("New registrations during the most recent lockdown"),
        color = "yellow",
        icon = icon("store-alt-slash")
      )
      } # country is UK
      else if (input$pickCountry == 'Eng'){
        TfrecentlckIfilt <- registerSecReg[registerSecReg$date >= "2021-01-05" &
                                                 registerSecReg$date <= "2021-04-11" &
                                                 registerSecReg$Country == "England" ]
        TrflckI <- TfrecentlckIfilt %>%
          group_by(date) %>%
          count()

        valueBox(
          value = prettyNum(sum(TrflckI$n),big.mark=",",scientific=FALSE),
          subtitle = paste("New registrations during the most recent lockdown"),
          color = "yellow",
          icon = icon("store-alt-slash")
        )
      } # country is England
      else if (input$pickCountry == 'NI'){
        TfrecentlckIfilt <- registerSecReg[registerSecReg$date >= "2021-01-05" &
                                                   registerSecReg$date <= "2021-04-11" &
                                                   registerSecReg$Country == "Northern Ireland" ]
        TrflckI <- TfrecentlckIfilt %>%
          group_by(date) %>%
          count()

        valueBox(
          value = prettyNum(sum(TrflckI$n),big.mark=",",scientific=FALSE),
          subtitle = paste("New registrations during the most recent lockdown"),
          color = "yellow",
          icon = icon("store-alt-slash")
        )
      } # country is NI
      else if (input$pickCountry == 'Scotland'){
        TfrecentlckIfilt <- registerSecReg[registerSecReg$date >= "2021-01-05" &
                                                   registerSecReg$date <= "2021-04-11" &
                                                   registerSecReg$Country == "Scotland" ]
        TrflckI <- TfrecentlckIfilt %>%
          group_by(date) %>%
          count()

        valueBox(
          value = prettyNum(sum(TrflckI$n),big.mark=",",scientific=FALSE),
          subtitle = paste("New registrations during the most recent lockdown"),
          color = "yellow",
          icon = icon("store-alt-slash")
        )
      } # country is Scotland
      else if (input$pickCountry == 'Wales'){
        TfrecentlckIfilt <- registerSecReg[registerSecReg$date >= "2021-01-05" &
                                                   registerSecReg$date <= "2021-04-11" &
                                                   registerSecReg$Country == "Wales" ]
        TrflckI <- TfrecentlckIfilt %>%
          group_by(date) %>%
          count()

        valueBox(
          value = prettyNum(sum(TrflckI$n),big.mark=",",scientific=FALSE),
          subtitle = paste("New registrations during the most recent lockdown"),
          color = "yellow",
          icon = icon("store-alt-slash")
        )
      } # country is Wales
    })#registrations since the most recent Lockdown
    output$regRecentEaseLock <- renderValueBox({
      if (input$pickCountry == 'UK'){
        TfrecentlckIfilt <- registerSecReg[registerSecReg$date >= "2021-04-12" &
                                                   registerSecReg$date <= input$dateAgg[2] ]
        TrflckI <- TfrecentlckIfilt %>%
          group_by(date) %>%
          count()

        valueBox(
          value = prettyNum(sum(TrflckI$n),big.mark=",",scientific=FALSE),
          subtitle = paste("New registrations since easing the most recent lockdown and", input$dateAgg[2]),
          color = "green",
          icon = icon("store-alt")
        )
      } # country is UK
      else if (input$pickCountry == 'Eng'){
        TfrecentlckIfilt <- registerSecReg[registerSecReg$date >= "2021-04-12" &
                                                   registerSecReg$date <= input$dateAgg[2] &
                                                   registerSecReg$Country == "England" ]
        TrflckI <- TfrecentlckIfilt %>%
          group_by(date) %>%
          count()

        valueBox(
          value = prettyNum(sum(TrflckI$n),big.mark=",",scientific=FALSE),
          subtitle = paste("New registrations since easing the most recent lockdown and", input$dateAgg[2]),
          color = "green",
          icon = icon("store-alt")
        )
      } # country is England
      else if (input$pickCountry == 'NI'){
        TfrecentlckIfilt <- registerSecReg[registerSecReg$date >= "2021-04-12" &
                                                   registerSecReg$date <= input$dateAgg[2] &
                                                   registerSecReg$Country == "Northern Ireland" ]
        TrflckI <- TfrecentlckIfilt %>%
          group_by(date) %>%
          count()

        valueBox(
          value = prettyNum(sum(TrflckI$n),big.mark=",",scientific=FALSE),
          subtitle = paste("New registrations since easing the most recent lockdown and", input$dateAgg[2]),
          color = "green",
          icon = icon("store-alt")
        )
      } # country is NI
      else if (input$pickCountry == 'Scotland'){
        TfrecentlckIfilt <- registerSecReg[registerSecReg$date >= "2021-04-12" &
                                                   registerSecReg$date <= input$dateAgg[2] &
                                                   registerSecReg$Country == "Scotland" ]
        TrflckI <- TfrecentlckIfilt %>%
          group_by(date) %>%
          count()

        valueBox(
          value = prettyNum(sum(TrflckI$n),big.mark=",",scientific=FALSE),
          subtitle = paste("New registrations since easing the most recent lockdown and", input$dateAgg[2]),
          color = "green",
          icon = icon("store-alt")
        )
      } # country is Scotland
      else if (input$pickCountry == 'Wales'){
        TfrecentlckIfilt <- registerSecReg[registerSecReg$date >= "2021-04-12" &
                                                   registerSecReg$date <= input$dateAgg[2] &
                                                   registerSecReg$Country == "Wales" ]
        TrflckI <- TfrecentlckIfilt %>%
          group_by(date) %>%
          count()

        valueBox(
          value = prettyNum(sum(TrflckI$n),big.mark=",",scientific=FALSE),
          subtitle = paste("New registrations since easing the most recent lockdown and", input$dateAgg[2]),
          color = "green",
          icon = icon("store-alt")
        )
      } # country is England
    }) # registrations since non-essentials shops open again most recently

    # output$sumStats <- renderDT({
    #   if (input$pickCountry == 'UK') {
    #
    # Tf <- Tfirmsfilt() %>%
    #   group_by(date) %>%
    #   count()
    # #uk
    # beforeldI <- subset(registerSecReg,
    #                     date >= "2020-01-01"
    #                     & date < start_ldI)
    # beforeldI <- beforeldI %>% group_by(date) %>% count()
    #
    # duringldI <- subset(registerSecReg,
    #                     date >= start_ldI
    #                     & date < end_ldI)
    # duringldI <- duringldI %>% group_by(date) %>% count()
    #
    # postldI<- subset(registerSecReg,
    #                  date >= end_ldI
    #                  & date < start_ldII)
    # postldI<- postldI %>% group_by(date) %>% count()
    #
    # duringldII <- subset(registerSecReg,
    #                      date >= start_ldII
    #                      & date < end_ldII)
    # duringldII <- duringldII %>% group_by(date) %>% count()
    #
    # afterldII <- subset(registerSecReg,
    #                     date >= end_ldII
    #                     & date < start_ldIII)
    # afterldII <- afterldII %>% group_by(date) %>% count()
    #
    #
    # afterldIII <- subset(registerSecReg, date >= end_ldIII)
    # afterldIII <- afterldIII %>% group_by(date) %>% count()
    #
    #
    # tf<- tibble::tibble("Period" = "of selection",!!!summary(Tf$n), sd=sd(Tf$n),) %>%
    #      add_row(tibble::tibble("Period" = "Before lockdown I",!!!summary(beforeldI$n), sd=sd(beforeldI$n),)) %>%
    #      add_row(tibble::tibble("Period" = "During lockdown I", !!!summary(duringldI$n), sd=sd(duringldI$n),)) %>%
    #   add_row(tibble::tibble("Period" = "After lockdown I", !!!summary(postldI$n), sd=sd(postldI$n),)) %>%
    #   add_row(tibble::tibble("Period" = "During lockdown II", !!!summary(duringldII$n), sd=sd(duringldII$n),)) %>%
    #   add_row(tibble::tibble("Period" = "After lockdown II", !!!summary(afterldII$n), sd=sd(afterldII$n),)) %>%
    #   add_row(tibble::tibble("Period" = "During lockdown III", !!!summary(duringldIII$n), sd=sd(duringldIII$n),)) %>%
    #   add_row(tibble::tibble("Period" = "After lockdown III", !!!summary(afterldIII$n), sd=sd(afterldIII$n),))
    #
    #
    # datatable(tf, extensions = 'Buttons',
    #           options = list(
    #             dom = 'Bfrtip',
    #             buttons = list(
    #               list(extend = "copy", text = '<span class="glyphicon glyphicon-copy" ></span> Copy'),
    #               list(extend = "csv", text = '<span class="glyphicon glyphicon-download-alt"></span> csv', title = "Summary Statistics"),
    #               list(extend = "print", text = '<span class="glyphicon glyphicon-print"></span> print')
    #           ),
    #         scrollX = TRUE)
    #         ) %>%
    #   formatRound(columns=c(3:6,8),digits=2) %>%
    #   formatRound(columns=c(2,7), digits=0)
    #   } # Country is UK
    #   else if (input$pickCountry == 'Eng') {
    #     Tffilt <- Tfirmsfilt() %>% filter(Country == "England")
    #
    #     Tf <- Tffilt %>%
    #       group_by(date) %>%
    #       count()
    #
    #     beforeldIENG <- subset(registerSecReg,
    #                            date >= "2020-01-01"
    #                            & date < start_ldI &
    #                              Country == "England")
    #     beforeldIENG <- beforeldIENG %>% group_by(date) %>% count()
    #
    #     duringldIENG <- subset(registerSecReg,
    #                            date >= start_ldI
    #                            & date < end_ldI & Country == "England")
    #     duringldIENG <- duringldIENG %>% group_by(date) %>% count()
    #
    #     postldIENG<- subset(registerSecReg,
    #                         date >= end_ldI
    #                         & date < start_ldII & Country == "England")
    #     postldIENG<- postldIENG %>% group_by(date) %>% count()
    #     med_incorp_post_ldIENG <- median(postldIENG$n)
    #
    #     duringldIIENG <- subset(registerSecReg,
    #                             date >= start_ldII
    #                             & date < end_ldII & Country == "England")
    #     duringldIIENG <- duringldIIENG %>% group_by(date) %>% count()
    #
    #     afterldIIENG <- subset(registerSecReg,
    #                            date >= end_ldII
    #                            & date < start_ldIII & Country == "England")
    #     afterldIIENG <- afterldIIENG %>% group_by(date) %>% count()
    #
    #     afterldIIIENG <- subset(registerSecReg,
    #                             date >= end_ldIII
    #                             & Country == "England")
    #     afterldIIIENG <- afterldIIIENG %>% group_by(date) %>% count()
    #
    #
    #     tf<- tibble::tibble("Period" = "of selection",!!!summary(Tf$n), sd=sd(Tf$n),) %>%
    #       add_row(tibble::tibble("Period" = "Before lockdown I",!!!summary(beforeldIENG$n), sd=sd(beforeldIENG$n),)) %>%
    #       add_row(tibble::tibble("Period" = "During lockdown I", !!!summary(duringldIENG$n), sd=sd(duringldIENG$n),)) %>%
    #       add_row(tibble::tibble("Period" = "After lockdown I", !!!summary(postldIENG$n), sd=sd(postldIENG$n),)) %>%
    #       add_row(tibble::tibble("Period" = "During lockdown II", !!!summary(duringldIIENG$n), sd=sd(duringldIIENG$n),)) %>%
    #       add_row(tibble::tibble("Period" = "After lockdown II", !!!summary(afterldIIENG$n), sd=sd(afterldIIENG$n),)) %>%
    #       add_row(tibble::tibble("Period" = "During lockdown III", !!!summary(duringldIIIENG$n), sd=sd(duringldIIIENG$n),)) %>%
    #       add_row(tibble::tibble("Period" = "After lockdown III", !!!summary(afterldIIIENG$n), sd=sd(afterldIIIENG$n),))
    #
    #     datatable(tf, extensions = 'Buttons',
    #               options = list(
    #                 dom = 'Bfrtip',
    #                 buttons = list(
    #                   list(extend = "copy", text = '<span class="glyphicon glyphicon-copy" ></span> Copy'),
    #                   list(extend = "csv", text = '<span class="glyphicon glyphicon-download-alt"></span> csv', title = "Summary Statistics_England"),
    #                   list(extend = "print", text = '<span class="glyphicon glyphicon-print"></span> print')
    #                 ),
    #                 scrollX = TRUE)
    #     ) %>%
    #       formatRound(columns=c(3:6,8),digits=2) %>%
    #       formatRound(columns=c(2,7), digits=0)
    #   } # Country is England
    #   else if (input$pickCountry == 'NI') {
    #     Tffilt <- Tfirmsfilt() %>% filter(Country == "Northern Ireland")
    #     Tf <- Tffilt %>%
    #       group_by(date) %>%
    #       count()
    #     beforeldINI <- subset(registerSecReg,
    #                           date >= "2020-01-01"
    #                           & date < start_ldI &
    #                             Country == "Northern Ireland")
    #     beforeldINI <- beforeldINI %>% group_by(date) %>% count()
    #
    #     duringldINI <- subset(registerSecReg,
    #                           date >= start_ldI
    #                           & date < end_ldI & Country == "Northern Ireland")
    #     duringldINI <- duringldINI %>% group_by(date) %>% count()
    #
    #     postldINI<- subset(registerSecReg,
    #                        date >= end_ldI
    #                        & date < start_ldII & Country == "Northern Ireland")
    #     postldINI<- postldINI %>% group_by(date) %>% count()
    #
    #     duringldIINI <- subset(registerSecReg,
    #                            date >= start_ldII
    #                            & date < end_ldII & Country == "Northern Ireland")
    #     duringldIINI <- duringldIINI %>% group_by(date) %>% count()
    #
    #     afterldIINI <- subset(registerSecReg,
    #                           date >= end_ldII
    #                           & date < start_ldIII & Country == "Northern Ireland")
    #     afterldIINI <- afterldIINI %>% group_by(date) %>% count()
    #
    #     afterldIIINI <- subset(registerSecReg,
    #                           date >= end_ldIII & Country == "Northern Ireland")
    #     afterldIIINI <- afterldIIINI %>% group_by(date) %>% count()
    #
    #
    #     tf<- tibble::tibble("Period" = "of selection",!!!summary(Tf$n), sd=sd(Tf$n),) %>%
    #       add_row(tibble::tibble("Period" = "Before lockdown I",!!!summary(beforeldINI$n), sd=sd(beforeldINI$n),)) %>%
    #       add_row(tibble::tibble("Period" = "During lockdown I", !!!summary(duringldINI$n), sd=sd(duringldINI$n),)) %>%
    #       add_row(tibble::tibble("Period" = "After lockdown I", !!!summary(postldINI$n), sd=sd(postldINI$n),)) %>%
    #       add_row(tibble::tibble("Period" = "During lockdown II", !!!summary(duringldIINI$n), sd=sd(duringldIINI$n),)) %>%
    #       add_row(tibble::tibble("Period" = "After lockdown II", !!!summary(afterldIINI$n), sd=sd(afterldIINI$n),)) %>%
    #       add_row(tibble::tibble("Period" = "During lockdown III", !!!summary(duringldIIINI$n), sd=sd(duringldIIINI$n),)) %>%
    #       add_row(tibble::tibble("Period" = "After lockdown III", !!!summary(afterldIIINI$n), sd=sd(afterldIIINI$n),))
    #
    #     datatable(tf, extensions = 'Buttons',
    #               options = list(
    #                 dom = 'Bfrtip',
    #                 buttons = list(
    #                   list(extend = "copy", text = '<span class="glyphicon glyphicon-copy" ></span> Copy'),
    #                   list(extend = "csv", text = '<span class="glyphicon glyphicon-download-alt"></span> csv', title = "Summary Statistics_NI"),
    #                   list(extend = "print", text = '<span class="glyphicon glyphicon-print"></span> print')
    #                 ),
    #                 scrollX = TRUE)
    #     ) %>%
    #       formatRound(columns=c(3:6,8),digits=2) %>%
    #       formatRound(columns=c(2,7), digits=0)
    #   } # Country is NI
    #   else if (input$pickCountry == 'Scotland') {
    #     Tffilt <- Tfirmsfilt() %>% filter(Country == "Scotland")
    #     Tf <- Tffilt %>%
    #       group_by(date) %>%
    #       count()
    #
    #     beforeldISC <- subset(registerSecReg,
    #                           date >= "2020-01-01"
    #                           & date < start_ldI &
    #                             Country == "Scotland")
    #     beforeldISC <- beforeldISC %>% group_by(date) %>% count()
    #
    #     duringldISC <- subset(registerSecReg,
    #                           date >= start_ldI
    #                           & date < end_ldI & Country == "Scotland")
    #     duringldISC <- duringldISC %>% group_by(date) %>% count()
    #
    #     postldISC<- subset(registerSecReg,
    #                        date >= end_ldI
    #                        & date < start_ldII & Country == "Scotland")
    #     postldISC<- postldISC %>% group_by(date) %>% count()
    #     med_incorp_post_ldISC <- median(postldISC$n)
    #
    #     duringldIISC <- subset(registerSecReg,
    #                            date >= start_ldII
    #                            & date < end_ldII & Country == "Scotland")
    #     duringldIISC <- duringldIISC %>% group_by(date) %>% count()
    #
    #     afterldIISC <- subset(registerSecReg,
    #                           date >= end_ldII
    #                           & date < start_ldIII & Country == "Scotland")
    #     afterldIISC <- afterldIISC %>% group_by(date) %>% count()
    #
    #     afterldIIISC <- subset(registerSecReg,
    #                           date >= end_ldIII
    #                           & Country == "Scotland")
    #     afterldIIISC <- afterldIIISC %>% group_by(date) %>% count()
    #
    #
    #     tf<- tibble::tibble("Period" = "of selection",!!!summary(Tf$n), sd=sd(Tf$n),) %>%
    #       add_row(tibble::tibble("Period" = "Before lockdown I",!!!summary(beforeldISC$n), sd=sd(beforeldISC$n),)) %>%
    #       add_row(tibble::tibble("Period" = "During lockdown I", !!!summary(duringldISC$n), sd=sd(duringldISC$n),)) %>%
    #       add_row(tibble::tibble("Period" = "After lockdown I", !!!summary(postldISC$n), sd=sd(postldISC$n),)) %>%
    #       add_row(tibble::tibble("Period" = "During lockdown II", !!!summary(duringldIISC$n), sd=sd(duringldIISC$n),)) %>%
    #       add_row(tibble::tibble("Period" = "After lockdown II", !!!summary(afterldIISC$n), sd=sd(afterldIISC$n),)) %>%
    #       add_row(tibble::tibble("Period" = "During lockdown III", !!!summary(duringldIIISC$n), sd=sd(duringldIIISC$n),)) %>%
    #       add_row(tibble::tibble("Period" = "After lockdown III", !!!summary(afterldIIISC$n), sd=sd(afterldIIISC$n),))
    #
    #     datatable(tf, extensions = 'Buttons',
    #               options = list(
    #                 dom = 'Bfrtip',
    #                 buttons = list(
    #                   list(extend = "copy", text = '<span class="glyphicon glyphicon-copy" ></span> Copy'),
    #                   list(extend = "csv", text = '<span class="glyphicon glyphicon-download-alt"></span> csv', title = "Summary Statistics_Scotland"),
    #                   list(extend = "print", text = '<span class="glyphicon glyphicon-print"></span> print')
    #                 ),
    #                 scrollX = TRUE)
    #     ) %>%
    #       formatRound(columns=c(3:6,8),digits=2) %>%
    #       formatRound(columns=c(2,7), digits=0)
    #   } # Country is Scotland
    #   else if (input$pickCountry == 'Wales') {
    #     Tffilt <- Tfirmsfilt() %>% filter(Country == "Wales")
    #     Tf <- Tffilt %>%
    #       group_by(date) %>%
    #       count()
    #     beforeldIW <- subset(registerSecReg,
    #                          date >= "2020-01-01"
    #                          & date < start_ldI &
    #                            Country == "Wales")
    #     beforeldIW <- beforeldIW %>% group_by(date) %>% count()
    #
    #     duringldIW <- subset(registerSecReg,
    #                          date >= start_ldI
    #                          & date < end_ldI & Country == "Wales")
    #     duringldIW <- duringldIW %>% group_by(date) %>% count()
    #
    #     postldIW<- subset(registerSecReg,
    #                       date >= end_ldI
    #                       & date < start_ldII & Country == "Wales")
    #     postldIW<- postldIW %>% group_by(date) %>% count()
    #
    #     duringldIIW <- subset(registerSecReg,
    #                           date >= start_ldII
    #                           & date < end_ldII & Country == "Wales")
    #     duringldIIW <- duringldIIW %>% group_by(date) %>% count()
    #
    #     afterldIIW <- subset(registerSecReg,
    #                          date >= end_ldII
    #                          & date < start_ldIII & Country == "Wales")
    #     afterldIIW <- afterldIIW %>% group_by(date) %>% count()
    #
    #     afterldIIIW <- subset(registerSecReg,
    #                          date >= end_ldIII & Country == "Wales")
    #     afterldIIIW <- afterldIIIW %>% group_by(date) %>% count()
    #
    #
    #     tf<- tibble::tibble("Period" = "of selection",!!!summary(Tf$n), sd=sd(Tf$n),) %>%
    #       add_row(tibble::tibble("Period" = "Before lockdown I",!!!summary(beforeldIW$n), sd=sd(beforeldIW$n),)) %>%
    #       add_row(tibble::tibble("Period" = "During lockdown I", !!!summary(duringldIW$n), sd=sd(duringldIW$n),)) %>%
    #       add_row(tibble::tibble("Period" = "After lockdown I", !!!summary(postldIW$n), sd=sd(postldIW$n),)) %>%
    #       add_row(tibble::tibble("Period" = "During lockdown II", !!!summary(duringldIIW$n), sd=sd(duringldIIW$n),)) %>%
    #       add_row(tibble::tibble("Period" = "After lockdown II", !!!summary(afterldIIW$n), sd=sd(afterldIIW$n),)) %>%
    #       add_row(tibble::tibble("Period" = "During lockdown III", !!!summary(duringldIIIW$n), sd=sd(duringldIIIW$n),)) %>%
    #       add_row(tibble::tibble("Period" = "After lockdown III", !!!summary(afterldIIIW$n), sd=sd(afterldIIIW$n),))
    #
    #     datatable(tf, extensions = 'Buttons',
    #               options = list(
    #                 dom = 'Bfrtip',
    #                 buttons = list(
    #                   list(extend = "copy", text = '<span class="glyphicon glyphicon-copy" ></span> Copy'),
    #                   list(extend = "csv", text = '<span class="glyphicon glyphicon-download-alt"></span> csv', title = "Summary Statistics_Wales"),
    #                   list(extend = "print", text = '<span class="glyphicon glyphicon-print"></span> print')
    #                 ),
    #                 scrollX = TRUE)
    #     ) %>%
    #       formatRound(columns=c(3:6,8),digits=2) %>%
    #       formatRound(columns=c(2,7), digits=0)
    #   } # Country is Wales
    # })#Summary Stats
    output$histogramTotal <- renderPlotly({
      if (input$pickCountry == 'UK') {

      Tfirms <- Tfirmsfilt() %>%
        group_by(date) %>%
        count()

      med_incorp <- median(Tfirms$n)
      plot_ly(alpha = 0.5, y=Tfirms$n, x=Tfirms$date, type = "bar", name = "registrations", showlegend= F) %>%
       layout(xaxis=list(type='auto')) %>%
        add_segments(x = min(Tfirms$date), xend= max(Tfirms$date), y=med_incorp, yend=med_incorp, name = "of selected period", showlegend=T) %>%
       add_segments(x = min(Tfirms$date), xend= max(Tfirms$date), y=med_incorp_during_ldIII, yend=med_incorp_during_ldIII, name = "during lockdownIII", showlegend=T) %>%
        layout(title = paste0('Daily company registrations',
                              '<br>',
                              '<sup>',
                              'between ', input$dateAgg[1], " and ", input$dateAgg[2],
                              '</sup>'),
               yaxis = list(title = "Number of registrations", showgrid = F),
               legend=list(title=list(text='<b> Median </b>')))
      } # COUNTRY IS UK
      else if (input$pickCountry == 'Eng') {
        Tfirmsfilt <- Tfirmsfilt() %>% filter(Country == "England" )
        Tfirms <- Tfirmsfilt %>%
          group_by(date) %>%
          count()

        med_incorp <- median(Tfirms$n)
        plot_ly(alpha = 0.5, y=Tfirms$n, x=Tfirms$date, type = "bar", name = "registrations", showlegend= F) %>%
          layout(xaxis=list(type='auto')) %>%
          add_segments(x = min(Tfirms$date), xend= max(Tfirms$date), y=med_incorp, yend=med_incorp, name = "of selected period", showlegend=T) %>%
          add_segments(x = min(Tfirms$date), xend= max(Tfirms$date), y=med_incorp_during_ldIIIENG, yend=med_incorp_during_ldIIIENG, name = "during lockdownIII", showlegend=T) %>%
          layout(title = paste0('Daily company registrations',
                                '<br>',
                                '<sup>',
                                'between ', input$dateAgg[1], " and ", input$dateAgg[2],
                                '</sup>'),
                 yaxis = list(title = "Number of registrations", showgrid = F),
                 legend=list(title=list(text='<b> Median </b>')))
      } # COUNTRY IS ENGLAND
      else if (input$pickCountry == 'NI') {
        Tfirmsfilt <- Tfirmsfilt() %>% filter(Country == "Northern Ireland" )
        Tfirms <- Tfirmsfilt %>%
          group_by(date) %>%
          count()

        med_incorp <- median(Tfirms$n)
        plot_ly(alpha = 0.5, y=Tfirms$n, x=Tfirms$date, type = "bar", name = "registrations", showlegend= F) %>%
          layout(xaxis=list(type='auto')) %>%
          add_segments(x = min(Tfirms$date), xend= max(Tfirms$date), y=med_incorp, yend=med_incorp, name = "of selected period", showlegend=T) %>%
          add_segments(x = min(Tfirms$date), xend= max(Tfirms$date), y=med_incorp_during_ldIIINI, yend=med_incorp_during_ldIIINI, name = "during lockdownIII", showlegend=T) %>%
          layout(title = paste0('Daily company registrations',
                                '<br>',
                                '<sup>',
                                'between ', input$dateAgg[1], " and ", input$dateAgg[2],
                                '</sup>'),
                 yaxis = list(title = "Number of registrations", showgrid = F),
                 legend=list(title=list(text='<b> Median </b>')))
      } # COUNTRY IS NI
      else if (input$pickCountry == 'Scotland') {
        Tfirmsfilt <- Tfirmsfilt() %>% filter(Country == "Scotland" )
        Tfirms <- Tfirmsfilt %>%
          group_by(date) %>%
          count()

        med_incorp <- median(Tfirms$n)
        plot_ly(alpha = 0.5, y=Tfirms$n, x=Tfirms$date, type = "bar", name = "registrations", showlegend= F) %>%
          layout(xaxis=list(type='auto')) %>%
          add_segments(x = min(Tfirms$date), xend= max(Tfirms$date), y=med_incorp, yend=med_incorp, name = "of selected period", showlegend=T) %>%
          add_segments(x = min(Tfirms$date), xend= max(Tfirms$date), y=med_incorp_during_ldIIISC, yend=med_incorp_during_ldIIISC, name = "during lockdownIII", showlegend=T) %>%
          layout(title = paste0('Daily company registrations',
                                '<br>',
                                '<sup>',
                                'between ', input$dateAgg[1], " and ", input$dateAgg[2],
                                '</sup>'),
                 yaxis = list(title = "Number of registrations", showgrid = F),
                 legend=list(title=list(text='<b> Median </b>')))
      } # COUNTRY IS Scotland
      else if (input$pickCountry == 'Wales') {
        Tfirmsfilt <- Tfirmsfilt() %>% filter(Country == "Wales" )
        Tfirms <- Tfirmsfilt %>%
          group_by(date) %>%
          count()

        med_incorp <- median(Tfirms$n)
        plot_ly(alpha = 0.5, y=Tfirms$n, x=Tfirms$date, type = "bar", name = "registrations", showlegend= F) %>%
          layout(xaxis=list(type='auto')) %>%
          add_segments(x = min(Tfirms$date), xend= max(Tfirms$date), y=med_incorp, yend=med_incorp, name = "of selected period", showlegend=T) %>%
          add_segments(x = min(Tfirms$date), xend= max(Tfirms$date), y=med_incorp_during_ldIIIW, yend=med_incorp_during_ldIIIW, name = "during lockdownIII", showlegend=T) %>%
          layout(title = paste0('Daily company registrations',
                                '<br>',
                                '<sup>',
                                'between ', input$dateAgg[1], " and ", input$dateAgg[2],
                                '</sup>'),
                 yaxis = list(title = "Number of registrations", showgrid = F),
                 legend=list(title=list(text='<b> Median </b>')))
      } # COUNTRY IS wales

    }) #histogramTotal
    output$rollAv <- renderPlotly({
      if (input$pickCountry == 'UK') {

      Tfirms <- Tfirmsfilt() %>%
        group_by(date) %>%
        count()
      #calculate rolling av
      Tfirms$av3 <- frollmean(Tfirms$n, n=3)
      Tfirms$av7 <- frollmean(Tfirms$n, n=7)

      if (input$lockdown == TRUE) {

      #graph
      plot_ly(alpha = 0.5)%>%
        add_trace(y = Tfirms$av7, x= Tfirms$date, name = '7-days average of selection', type= 'scatter',mode = 'lines') %>%
  #      add_trace(y = Tfirms_2019c$av7, x= Tfirms_2019c$date, name = '2019', mode = 'lines') %>%
  #      add_segments(x=min(Week2021_filtered$week), xend = max(Week2021_filtered$week), y=1, yend=1, line = list(dash = "dash"), showlegend=F)
      layout(title = list(text = paste0('Company registrations 7-day rolling average',
                                                                 '<br>',
                                                                 '<sup>',
                                                                 'For registrations between', input$dateAgg[1], ' and ', input$dateAgg[2],
                                                                 '</sup>')),
                                      xaxis = list(showgrid = F),
                                      yaxis = list(title = "Number of registrations", showgrid = F),
             shapes = list(
               list(type = "rect",
                    fillcolor = "gray", line = list(color = "gray"), opacity = 0.2,
                    x0 = "2020-03-23", x1 = "2020-07-04", xref = "x",
                    y0 = 0, y1 = 3100, yref = "y"),
               list(type = "rect",
                    fillcolor = "gray", line = list(color = "gray"), opacity = 0.2,
                    x0 = "2020-11-05", x1 = "2020-12-02", xref = "x",
                    y0 = 0, y1 = 3100, yref = "y"),
               list(type = "rect",
                    fillcolor = "gray", line = list(color = "gray"), opacity = 0.2,
                    x0 = "2021-01-05", x1 = "2021-04-12", xref = "x",
                    y0 = 0, y1 = 3100, yref = "y")
             ))
      }
      else {
        plot_ly(alpha = 0.5)%>%
          add_trace(y = Tfirms$av7, x= Tfirms$date, name = '7-days average of selection', type= 'scatter',mode = 'lines') %>%
          #      add_trace(y = Tfirms_2019c$av7, x= Tfirms_2019c$date, name = '2019', mode = 'lines') %>%
          #      add_segments(x=min(Week2021_filtered$week), xend = max(Week2021_filtered$week), y=1, yend=1, line = list(dash = "dash"), showlegend=F)
          layout(title = list(text = paste0('Company registrations 7-day rolling average',
                                            '<br>',
                                            '<sup>',
                                            'For registrations between', input$dateAgg[1], ' and ', input$dateAgg[2],
                                            '</sup>')),
                 xaxis = list(showgrid = F),
                 yaxis = list(title = "Number of registrations", showgrid = F)
                )
      } # no-lockdowns
      } # COUNTRY IS UK
      else if (input$pickCountry == 'Eng') {
        # filter the days user needs in 2021
        Tfirmsfilt <- Tfirmsfilt() %>% filter(Country == "England" )
        Tfirms <- Tfirmsfilt %>%
          group_by(date) %>%
          count()
        #calculate rolling av
        Tfirms$av3 <- frollmean(Tfirms$n, n=3)
        Tfirms$av7 <- frollmean(Tfirms$n, n=7)

        #identify same dates but in 2019
        if (input$lockdown == TRUE) {

        #graph
        plot_ly(alpha = 0.5)%>%
          add_trace(y = Tfirms$av7, x= Tfirms$date, name = '7-days average of selection', type= 'scatter',mode = 'lines') %>%
          #      add_trace(y = Tfirms_2019c$av7, x= Tfirms_2019c$date, name = '2019', mode = 'lines') %>%
          #      add_segments(x=min(Week2021_filtered$week), xend = max(Week2021_filtered$week), y=1, yend=1, line = list(dash = "dash"), showlegend=F)
          layout(title = list(text = paste0('Company registrations 7-day rolling average',
                                            '<br>',
                                            '<sup>',
                                            'For registrations between', input$dateAgg[1], ' and ', input$dateAgg[2],
                                            '</sup>')),
                 xaxis = list(showgrid = F),
                 yaxis = list(title = "Number of registrations", showgrid = F),
                 shapes = list(
                   list(type = "rect",
                        fillcolor = "gray", line = list(color = "gray"), opacity = 0.2,
                        x0 = "2020-03-23", x1 = "2020-07-04", xref = "x",
                        y0 = 0, y1 = 3100, yref = "y"),
                   list(type = "rect",
                        fillcolor = "gray", line = list(color = "gray"), opacity = 0.2,
                        x0 = "2020-11-05", x1 = "2020-12-02", xref = "x",
                        y0 = 0, y1 = 3100, yref = "y"),
                   list(type = "rect",
                        fillcolor = "gray", line = list(color = "gray"), opacity = 0.2,
                        x0 = "2021-01-05", x1 = "2021-04-12", xref = "x",
                        y0 = 0, y1 = 3100, yref = "y")
                 ))
        }
        else {
          plot_ly(alpha = 0.5)%>%
            add_trace(y = Tfirms$av7, x= Tfirms$date, name = '7-days average of selection', type= 'scatter',mode = 'lines') %>%
            #      add_trace(y = Tfirms_2019c$av7, x= Tfirms_2019c$date, name = '2019', mode = 'lines') %>%
            #      add_segments(x=min(Week2021_filtered$week), xend = max(Week2021_filtered$week), y=1, yend=1, line = list(dash = "dash"), showlegend=F)
            layout(title = list(text = paste0('Company registrations 7-day rolling average',
                                              '<br>',
                                              '<sup>',
                                              'For registrations between ', input$dateAgg[1], ' and ', input$dateAgg[2],
                                              '</sup>')),
                   xaxis = list(showgrid = F),
                   yaxis = list(title = "Number of registrations", showgrid = F))
        }
      } # COUNTRY IS ENGLAND
      else if (input$pickCountry == 'NI') {
        # filter the days user needs in 2021
        Tfirmsfilt <- Tfirmsfilt() %>% filter(Country == "Northern Ireland" )
        Tfirms <- Tfirmsfilt %>%
          group_by(date) %>%
          count()
        #calculate rolling av
        Tfirms$av3 <- frollmean(Tfirms$n, n=3)
        Tfirms$av7 <- frollmean(Tfirms$n, n=7)

        #identify same dates but in 2019
        if (input$lockdown == TRUE) {

        #graph
        plot_ly(alpha = 0.5)%>%
          add_trace(y = Tfirms$av7, x= Tfirms$date, name = '7-days average of selection', type= 'scatter',mode = 'lines') %>%
          #      add_trace(y = Tfirms_2019c$av7, x= Tfirms_2019c$date, name = '2019', mode = 'lines') %>%
          #      add_segments(x=min(Week2021_filtered$week), xend = max(Week2021_filtered$week), y=1, yend=1, line = list(dash = "dash"), showlegend=F)
          layout(title = list(text = paste0('Company registrations 7-day rolling average',
                                            '<br>',
                                            '<sup>',
                                            'For registrations between', input$dateAgg[1], ' and ', input$dateAgg[2],
                                            '</sup>')),
                 xaxis = list(showgrid = F),
                 yaxis = list(title = "Number of registrations", showgrid = F),
                 shapes = list(
                   list(type = "rect",
                        fillcolor = "gray", line = list(color = "gray"), opacity = 0.2,
                        x0 = "2020-03-23", x1 = "2020-07-04", xref = "x",
                        y0 = 0, y1 = 3100, yref = "y"),
                   list(type = "rect",
                        fillcolor = "gray", line = list(color = "gray"), opacity = 0.2,
                        x0 = "2020-11-05", x1 = "2020-12-02", xref = "x",
                        y0 = 0, y1 = 3100, yref = "y"),
                   list(type = "rect",
                        fillcolor = "gray", line = list(color = "gray"), opacity = 0.2,
                        x0 = "2021-01-05", x1 = "2021-04-12", xref = "x",
                        y0 = 0, y1 = 3100, yref = "y")
                 ))
        }
        else {
          plot_ly(alpha = 0.5)%>%
            add_trace(y = Tfirms$av7, x= Tfirms$date, name = '7-days average of selection', type= 'scatter',mode = 'lines') %>%
            #      add_trace(y = Tfirms_2019c$av7, x= Tfirms_2019c$date, name = '2019', mode = 'lines') %>%
            #      add_segments(x=min(Week2021_filtered$week), xend = max(Week2021_filtered$week), y=1, yend=1, line = list(dash = "dash"), showlegend=F)
            layout(title = list(text = paste0('Company registrations 7-day rolling average',
                                              '<br>',
                                              '<sup>',
                                              'For registrations between ', input$dateAgg[1], ' and ', input$dateAgg[2],
                                              '</sup>')),
                   xaxis = list(showgrid = F),
                   yaxis = list(title = "Number of registrations", showgrid = F))
        }
      } # COUNTRY IS ni
      else if (input$pickCountry == 'Scotland') {
        # filter the days user needs in 2021
        Tfirmsfilt <- Tfirmsfilt() %>% filter(Country == "Scotland" )

        Tfirms <- Tfirmsfilt %>%
          group_by(date) %>%
          count()
        #calculate rolling av
        Tfirms$av3 <- frollmean(Tfirms$n, n=3)
        Tfirms$av7 <- frollmean(Tfirms$n, n=7)

        #identify same dates but in 2019
        if (input$lockdown == TRUE) {

        #graph
        plot_ly(alpha = 0.5)%>%
          add_trace(y = Tfirms$av7, x= Tfirms$date, name = '7-days average of selection', type= 'scatter',mode = 'lines') %>%
          #      add_trace(y = Tfirms_2019c$av7, x= Tfirms_2019c$date, name = '2019', mode = 'lines') %>%
          #      add_segments(x=min(Week2021_filtered$week), xend = max(Week2021_filtered$week), y=1, yend=1, line = list(dash = "dash"), showlegend=F)
          layout(title = list(text = paste0('Company registrations 7-day rolling average',
                                            '<br>',
                                            '<sup>',
                                            'For registrations between ', input$dateAgg[1], ' and ', input$dateAgg[2],
                                            '</sup>')),
                 xaxis = list(showgrid = F),
                 yaxis = list(title = "Number of registrations", showgrid = F),
                 shapes = list(
                   list(type = "rect",
                        fillcolor = "gray", line = list(color = "gray"), opacity = 0.2,
                        x0 = "2020-03-23", x1 = "2020-07-04", xref = "x",
                        y0 = 0, y1 = 3100, yref = "y"),
                   list(type = "rect",
                        fillcolor = "gray", line = list(color = "gray"), opacity = 0.2,
                        x0 = "2020-11-05", x1 = "2020-12-02", xref = "x",
                        y0 = 0, y1 = 3100, yref = "y"),
                   list(type = "rect",
                        fillcolor = "gray", line = list(color = "gray"), opacity = 0.2,
                        x0 = "2021-01-05", x1 = "2021-04-12", xref = "x",
                        y0 = 0, y1 = 3100, yref = "y")
                 ))
        }
        else {
          plot_ly(alpha = 0.5)%>%
            add_trace(y = Tfirms$av7, x= Tfirms$date, name = '7-days average of selection', type= 'scatter',mode = 'lines') %>%
            #      add_trace(y = Tfirms_2019c$av7, x= Tfirms_2019c$date, name = '2019', mode = 'lines') %>%
            #      add_segments(x=min(Week2021_filtered$week), xend = max(Week2021_filtered$week), y=1, yend=1, line = list(dash = "dash"), showlegend=F)
            layout(title = list(text = paste0('Company registrations 7-day rolling average',
                                              '<br>',
                                              '<sup>',
                                              'For registrations between ', input$dateAgg[1], ' and ', input$dateAgg[2],
                                              '</sup>')),
                   xaxis = list(showgrid = F),
                   yaxis = list(title = "Number of registrations", showgrid = F))

        }
      } # COUNTRY IS Scotland
      else if (input$pickCountry == 'Wales') {
        # filter the days user needs in 2021
        Tfirmsfilt <- Tfirmsfilt() %>% filter(Country == "Wales" )
        Tfirms <- Tfirmsfilt %>%
          group_by(date) %>%
          count()
        #calculate rolling av
        Tfirms$av3 <- frollmean(Tfirms$n, n=3)
        Tfirms$av7 <- frollmean(Tfirms$n, n=7)

        #identify same dates but in 2019
        if (input$lockdown == TRUE) {

        #graph
        plot_ly(alpha = 0.5)%>%
          add_trace(y = Tfirms$av7, x= Tfirms$date, name = '7-days average of selection', type= 'scatter',mode = 'lines') %>%
          #      add_trace(y = Tfirms_2019c$av7, x= Tfirms_2019c$date, name = '2019', mode = 'lines') %>%
          #      add_segments(x=min(Week2021_filtered$week), xend = max(Week2021_filtered$week), y=1, yend=1, line = list(dash = "dash"), showlegend=F)
          layout(title = list(text = paste0('Company registrations 7-day rolling average',
                                            '<br>',
                                            '<sup>',
                                            'For registrations between ', input$dateAgg[1], ' and ', input$dateAgg[2],
                                            '</sup>')),
                 xaxis = list(showgrid = F),
                 yaxis = list(title = "Number of registrations", showgrid = F),
                 shapes = list(
                   list(type = "rect",
                        fillcolor = "gray", line = list(color = "gray"), opacity = 0.2,
                        x0 = "2020-03-23", x1 = "2020-07-04", xref = "x",
                        y0 = 0, y1 = 3100, yref = "y"),
                   list(type = "rect",
                        fillcolor = "gray", line = list(color = "gray"), opacity = 0.2,
                        x0 = "2020-11-05", x1 = "2020-12-02", xref = "x",
                        y0 = 0, y1 = 3100, yref = "y"),
                   list(type = "rect",
                        fillcolor = "gray", line = list(color = "gray"), opacity = 0.2,
                        x0 = "2021-01-05", x1 = "2021-04-12", xref = "x",
                        y0 = 0, y1 = 3100, yref = "y")
                 ))
        }
        else {
          plot_ly(alpha = 0.5)%>%
            add_trace(y = Tfirms$av7, x= Tfirms$date, name = '7-days average of selection', type= 'scatter',mode = 'lines') %>%
            #      add_trace(y = Tfirms_2019c$av7, x= Tfirms_2019c$date, name = '2019', mode = 'lines') %>%
            #      add_segments(x=min(Week2021_filtered$week), xend = max(Week2021_filtered$week), y=1, yend=1, line = list(dash = "dash"), showlegend=F)
            layout(title = list(text = paste0('Company registrations 7-day rolling average',
                                              '<br>',
                                              '<sup>',
                                              'For registrations between ', input$dateAgg[1], ' and ', input$dateAgg[2],
                                              '</sup>')),
                   xaxis = list(showgrid = F),
                   yaxis = list(title = "Number of registrations", showgrid = F))

        }
      } # COUNTRY IS Wales


    })#rolling average graph
    output$sectorsChartFull <- renderAmCharts({
      if (input$pickCountry == 'UK'){

      # Calculate total by date and Section
      Tfirms <- Tfirmsfilt() %>%
        group_by(date, Section) %>%
        count()
      # Aggregate by Section
      firmsAgg <- aggregate(Tfirms$n, by=list(Section=Tfirms$Section), FUN=sum)
      firmsAgg <- merge(firmsAgg, convert[,c("Section", "SectionAbb")],by="Section")
      firmsAgg <- firmsAgg %>% distinct(Section, .keep_all = TRUE)


      pipeR::pipeline(
      amPieChart(valueField = 'x', titleField = 'SectionAbb', innerRadius = 70,
                dataProvider = firmsAgg, startDuration = 0, theme = 'light', alpha = 0.7,
                labelsEnabled = F, creditsPosition =  "bottom-right"),
      addTitle(text = 'Sectors overview'),
      addTitle(text = paste0('Between ', input$dateAgg[1], ' and ', input$dateAgg[2]), size=10),
      setExport(enabled = TRUE),
      setLegend(markerType = 'circle', position = 'bottom', autoMargins = T,
                markerSize = 10)
      )
      } # COUNTRY IS UK
      else if (input$pickCountry == 'Eng'){
        # select period
        Tfirmsfilt <- Tfirmsfilt() %>% filter(Country == "England" )

        # Calculate total by date and Section
        Tfirms <- Tfirmsfilt %>%
          group_by(date, Section) %>%
          count()
        # Aggregate by Section
        firmsAgg <- aggregate(Tfirms$n, by=list(Section=Tfirms$Section), FUN=sum)
        firmsAgg <- merge(firmsAgg, convert[,c("Section", "SectionAbb")],by="Section")
        firmsAgg <- firmsAgg %>% distinct(Section, .keep_all = TRUE)


        pipeR::pipeline(
          amPieChart(valueField = 'x', titleField = 'SectionAbb', innerRadius = 70,
                     dataProvider = firmsAgg, startDuration = 0, theme = 'light', alpha = 0.7,
                     labelsEnabled = F, creditsPosition =  "bottom-right"),
          addTitle(text = 'Sectors overview'),
          addTitle(text = paste0('Between ', input$dateAgg[1], ' and ', input$dateAgg[2]), size=10),
          setExport(enabled = TRUE),
          setLegend(markerType = 'circle', position = 'bottom', autoMargins = T,
                    markerSize = 10)
          #     addListener('clickSlice' , 'function(event){alert(\'Click slice !\');}')
        )
      } # COUNTRY IS England
      else if (input$pickCountry == 'NI'){
        # select period
        Tfirmsfilt <- Tfirmsfilt() %>% filter(Country == "Northern Ireland" )
        # Calculate total by date and Section
        Tfirms <- Tfirmsfilt %>%
          group_by(date, Section) %>%
          count()
        # Aggregate by Section
        firmsAgg <- aggregate(Tfirms$n, by=list(Section=Tfirms$Section), FUN=sum)
        firmsAgg <- merge(firmsAgg, convert[,c("Section", "SectionAbb")],by="Section")
        firmsAgg <- firmsAgg %>% distinct(Section, .keep_all = TRUE)


        pipeR::pipeline(
          amPieChart(valueField = 'x', titleField = 'SectionAbb', innerRadius = 70,
                     dataProvider = firmsAgg, startDuration = 0, theme = 'light', alpha = 0.7,
                     labelsEnabled = F, creditsPosition =  "bottom-right"),
          addTitle(text = 'Sectors overview'),
          addTitle(text = paste0('Between ', input$dateAgg[1], ' and ', input$dateAgg[2]), size=10),
          setExport(enabled = TRUE),
          setLegend(markerType = 'circle', position = 'bottom', autoMargins = T,
                    markerSize = 10)
          #     addListener('clickSlice' , 'function(event){alert(\'Click slice !\');}')
        )
      } # COUNTRY IS ni
      else if (input$pickCountry == 'Scotland'){
        # select period
        Tfirmsfilt <- Tfirmsfilt() %>% filter(Country == "Scotland" )
        # Calculate total by date and Section
        Tfirms <- Tfirmsfilt %>%
          group_by(date, Section) %>%
          count()
        # Aggregate by Section
        firmsAgg <- aggregate(Tfirms$n, by=list(Section=Tfirms$Section), FUN=sum)
        firmsAgg <- merge(firmsAgg, convert[,c("Section", "SectionAbb")],by="Section")
        firmsAgg <- firmsAgg %>% distinct(Section, .keep_all = TRUE)


        pipeR::pipeline(
          amPieChart(valueField = 'x', titleField = 'SectionAbb', innerRadius = 70,
                     dataProvider = firmsAgg, startDuration = 0, theme = 'light', alpha = 0.7,
                     labelsEnabled = F, creditsPosition =  "bottom-right"),
          addTitle(text = 'Sectors overview'),
          addTitle(text = paste0('Between ', input$dateAgg[1], ' and ', input$dateAgg[2]), size=10),
          setExport(enabled = TRUE),
          setLegend(markerType = 'circle', position = 'bottom', autoMargins = T,
                    markerSize = 10)
          #     addListener('clickSlice' , 'function(event){alert(\'Click slice !\');}')
        )
      } # COUNTRY IS Scotland
      else if (input$pickCountry == 'Wales'){
        # select period
        Tfirmsfilt <- Tfirmsfilt() %>% filter(Country == "Wales" )
        # Calculate total by date and Section
        Tfirms <- Tfirmsfilt %>%
          group_by(date, Section) %>%
          count()
        # Aggregate by Section
        firmsAgg <- aggregate(Tfirms$n, by=list(Section=Tfirms$Section), FUN=sum)
        firmsAgg <- merge(firmsAgg, convert[,c("Section", "SectionAbb")],by="Section")
        firmsAgg <- firmsAgg %>% distinct(Section, .keep_all = TRUE)


        pipeR::pipeline(
          amPieChart(valueField = 'x', titleField = 'SectionAbb', innerRadius = 70,
                     dataProvider = firmsAgg, startDuration = 0, theme = 'light', alpha = 0.7,
                     labelsEnabled = F, creditsPosition =  "bottom-right"),
          addTitle(text = 'Sectors overview'),
          addTitle(text = paste0('Between ', input$dateAgg[1], ' and ', input$dateAgg[2]), size=10),
          setExport(enabled = TRUE),
          setLegend(markerType = 'circle', position = 'bottom', autoMargins = T,
                    markerSize = 10)
          #     addListener('clickSlice' , 'function(event){alert(\'Click slice !\');}')
        )
      } # COUNTRY IS Wales

    }) # sectors pie chart in Aggregate Analysis

    # Use the function as.sunburstDF from https://stackoverflow.com/a/58481176/4874341
    as.sunburstDF <- function(DF, valueCol = NULL){
      require(data.table)

      colNamesDF <- names(DF)

      if(is.data.table(DF)){
        DT <- copy(DF)
      } else {
        DT <- data.table(DF, stringsAsFactors = FALSE)
      }

      DT[, root := "Total"]
      colNamesDT <- names(DT)

      if(is.null(valueCol)){
        setcolorder(DT, c("root", colNamesDF))
      } else {
        setnames(DT, valueCol, "values", skip_absent=TRUE)
        setcolorder(DT, c("root", setdiff(colNamesDF, valueCol), "values"))
      }

      hierarchyCols <- setdiff(colNamesDT, "values")
      hierarchyList <- list()

      for(i in seq_along(hierarchyCols)){
        currentCols <- colNamesDT[1:i]
        if(is.null(valueCol)){
          currentDT <- unique(DT[, ..currentCols][, values := .N, by = currentCols], by = currentCols)
        } else {
          currentDT <- DT[, lapply(.SD, sum, na.rm = TRUE), by=currentCols, .SDcols = "values"]
        }
        setnames(currentDT, length(currentCols), "labels")
        hierarchyList[[i]] <- currentDT
      }

      hierarchyDT <- rbindlist(hierarchyList, use.names = TRUE, fill = TRUE)

      parentCols <- setdiff(names(hierarchyDT), c("labels", "values", valueCol))
      hierarchyDT[, parents := apply(.SD, 1, function(x){fifelse(all(is.na(x)), yes = NA_character_, no = paste(x[!is.na(x)], sep = ":", collapse = " - "))}), .SDcols = parentCols]
      hierarchyDT[, ids := apply(.SD, 1, function(x){paste(x[!is.na(x)], collapse = " - ")}), .SDcols = c("parents", "labels")]
      hierarchyDT[, c(parentCols) := NULL]
      return(hierarchyDT)
    }
    output$treemap <- renderPlotly({
      if (input$pickCountry == 'UK'){

      # Calculate total by date and Section
      Tfirms <- Tfirmsfilt() %>%
        group_by(SectionAbb, Division.name) %>%
        count()

       # my sunburstDF
     sunburstDF <- as.sunburstDF(Tfirms, valueCol = "n")

      plot_ly(alpha = 0.5, data = sunburstDF, ids = ~ids, labels= ~labels,
               parents = ~parents, values= ~values, type='treemap',
               branchvalues = 'total',
               textinfo="label+value+percent parent+parent+percent root+root",
               hoverinfo="label+value+percent parent+parent",
               pathbar = "visible",
               insidetextfont = list(size = 10)
      )  %>%
        layout(
          title = paste0('Distribution of new registrations by SIC codes',
                 '<br>',
                 '<sup>',
                 'For registrations between ', input$dateAgg[1], ' and ', input$dateAgg[2],
                 '</sup>'),
          uniformtext=list(minsize=10)
        )
      } # COUNTRY IS UK
      else if (input$pickCountry == 'Eng'){
        # select period
        Tfirmsfilt <- Tfirmsfilt() %>% filter(Country == "England" )
        # Calculate total by date and Section
        Tfirms <- Tfirmsfilt %>%
          group_by(SectionAbb, Division.name) %>%
          count()

        sunburstDF <- as.sunburstDF(Tfirms, valueCol = "n")

        plot_ly(alpha = 0.5, data = sunburstDF, ids = ~ids, labels= ~labels,
                parents = ~parents, values= ~values, type='treemap',
                branchvalues = 'total',
                textinfo="label+value+percent parent+parent+percent root+root",
                hoverinfo="label+value+percent parent+parent",
                pathbar = "visible",
                insidetextfont = list(size = 10)
        )  %>%
          layout(
            title = paste0('Distribution of new registrations by SIC codes',
                           '<br>',
                           '<sup>',
                           'For registrations between ', input$dateAgg[1], ' and ', input$dateAgg[2],
                           '</sup>'),
            uniformtext=list(minsize=10)
          )

      } # COUNTRY IS England
      else if (input$pickCountry == 'NI'){
        # select period
        Tfirmsfilt <- Tfirmsfilt() %>% filter(Country == "Northern Ireland" )
        # Calculate total by date and Section
        Tfirms <- Tfirmsfilt %>%
          group_by(SectionAbb, Division.name) %>%
          count()

        sunburstDF <- as.sunburstDF(Tfirms, valueCol = "n")

        plot_ly(alpha = 0.5, data = sunburstDF, ids = ~ids, labels= ~labels,
                parents = ~parents, values= ~values, type='treemap',
                branchvalues = 'total',
                textinfo="label+value+percent parent+parent+percent root+root",
                hoverinfo="label+value+percent parent+parent",
                pathbar = "visible",
                insidetextfont = list(size = 10)
        )  %>%
          layout(
            title = paste0('Distribution of new registrations by SIC codes',
                           '<br>',
                           '<sup>',
                           'For registrations between ', input$dateAgg[1], ' and ', input$dateAgg[2],
                           '</sup>'),
            uniformtext=list(minsize=10)
          )
      } # COUNTRY IS NI
      else if (input$pickCountry == 'Scotland'){
        # select period
        Tfirmsfilt <- Tfirmsfilt() %>% filter(Country == "Scotland" )
        # Calculate total by date and Section
        Tfirms <- Tfirmsfilt %>%
          group_by(SectionAbb, Division.name) %>%
          count()

        sunburstDF <- as.sunburstDF(Tfirms, valueCol = "n")

        plot_ly(alpha = 0.5, data = sunburstDF, ids = ~ids, labels= ~labels,
                parents = ~parents, values= ~values, type='treemap',
                branchvalues = 'total',
                textinfo="label+value+percent parent+parent+percent root+root",
                hoverinfo="label+value+percent parent+parent",
                pathbar = "visible",
                insidetextfont = list(size = 10)
        )  %>%
          layout(
            title = paste0('Distribution of new registrations by SIC codes',
                           '<br>',
                           '<sup>',
                           'For registrations between ', input$dateAgg[1], ' and ', input$dateAgg[2],
                           '</sup>'),
            uniformtext=list(minsize=10)
          )
      } # COUNTRY IS Scotland
      else if (input$pickCountry == 'Wales'){
        # select period
        Tfirmsfilt <- Tfirmsfilt() %>% filter(Country == "Wales" )
        # Calculate total by date and Section
        Tfirms <- Tfirmsfilt %>%
          group_by(SectionAbb, Division.name) %>%
          count()

        sunburstDF <- as.sunburstDF(Tfirms, valueCol = "n")

        plot_ly(alpha = 0.5, data = sunburstDF, ids = ~ids, labels= ~labels,
                parents = ~parents, values= ~values, type='treemap',
                branchvalues = 'total',
                textinfo="label+value+percent parent+parent+percent root+root",
                hoverinfo="label+value+percent parent+parent",
                pathbar = "visible",
                insidetextfont = list(size = 10)
        )  %>%
          layout(
            title = paste0('Distribution of new registrations by SIC codes',
                           '<br>',
                           '<sup>',
                           'For registrations between ', input$dateAgg[1], ' and ', input$dateAgg[2],
                           '</sup>'),
            uniformtext=list(minsize=10)
          )
      } # COUNTRY IS Wales

    }) # plotly treemap

    output$UKmap <- renderLeaflet({

      n_iPC <- Tfirmsfilt() %>%
        group_by(postcode) %>%
        count()
      pcReg <- pc2nuts %>%
        left_join(n_iPC, by = c("postcode"))
      pcReg <- pcReg %>% distinct(postcode, .keep_all = TRUE)

      #measure by nuts2
      n_pcReg <- pcReg %>%
        group_by(NUTS218CD) %>%
        summarise(n = sum(n, na.rm=TRUE))
      n_pcReg <- n_pcReg %>% left_join(pc2nuts, by = c("NUTS218CD"))
      n_pcReg <- n_pcReg %>% distinct(NUTS218CD, .keep_all = TRUE)

      uk_nuts2 <- geojsonio::geojson_read("http://geoportal1-ons.opendata.arcgis.com/datasets/48b6b85bb7ea43699ee85f4ecd12fd36_2.geojson?outSR={%22latestWkid%22:27700,%22wkid%22:27700}", what = "sp")

      map <- n_pcReg %>% rename (nuts218cd = NUTS218CD)

      uk_nuts2@data<- uk_nuts2@data %>%
        left_join(map, by = c("nuts218cd"))
      # creating a colour palette that provides a diff colour for regions
      # in different country i.e., Scotland, Ireland, Wales, etc.
      # Create a continuous palette function
      pal <- colorNumeric(palette = nord("afternoon_prarie"), domain = uk_nuts2@data$n)

      # Create the popup labels
      labels <- sprintf("<strong>%s</strong><br/>%g new registrations",
                        uk_nuts2$nuts218nm, uk_nuts2$n) %>%
        lapply(htmltools::HTML)

      #Code to add the polygons and to colour the polygons based on the value
      m <- leaflet(uk_nuts2) %>%
        addPolygons(fillColor = ~pal(n), weight = 2, opacity = 0,
                    color = "white", dashArray = "2", fillOpacity = 0.7,
                    highlight = highlightOptions(weight = 5, color = "white",
                                                 dashArray = "", fillOpacity = 2,
                                                 bringToFront = TRUE), label = labels,
                    labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                             padding = "3px 8px"),
                                                textsize = "13px", direction = "auto"))

      #Add a legend
      m %>% addLegend(pal = pal, values = ~n, opacity = 0.7,
                      title = paste0('New registrations',
                                                 '<br>',
                                                 '<sup>',
                                                 'between ', input$dateAgg[1], ' and ', input$dateAgg[2],
                                                 '</sup>'),
                           position = "bottomright") %>%
        setMapWidgetStyle(list(background= "white"))








      })

    output$group1 <- renderDT({
      sec <- registerSecReg[which(registerSecReg$Group %in% input$groupPicker), ] %>%
        group_by(Group) %>%
        summarise(across(everything(), last))

      tbl <- data.frame("A" = c( sec$Group),
                        "B" = c( sec$Group.name),
                        "C" = c( sec$Section),
                        "D" = c( sec$Section.name))

      container_dt= withTags(table(
        class = 'display',
        thead(
          tr(
            th(class = 'dt-center',colspan = 2, '3-digit classification'),
            th(class = 'dt-center',colspan = 2, '1-digit classification')),
          tr(
            lapply((c('Group', 'Group Name', 'Section', 'Section Name')), th)))))
      datatable(tbl, container = container_dt, rownames = F, class = "",
                options = list(autoWidth = T, lengthChange = FALSE,
                               columnDefs = list(list(className = "dt-center",
                                                      targets = "_all"),
                                                 list(width = "40px",
                                                      target = "_all"))))





    })

    Tfirmsfilt2 <- reactive({
      registerSecReg %>% filter(date >= input$dateIndustry[1] &
                                  date <= input$dateIndustry[2] &
                                  Group %in% input$groupPicker )
    })

    output$classPlot <- renderPlotly({
      if (input$CountryPicker == 'UK'){

        # Calculate total by date and Class
        Tfirms <- Tfirmsfilt2() %>%
          group_by(date, Group) %>%
          count()

       # Tfirms$av7 <- frollmean(Tfirms$n, n=7)

        firms <- left_join(Tfirms,convert,by="Group")
        # Remove duplicates based on week columns
        firms <- firms %>%
          distinct(Group, date, .keep_all = TRUE)

        firms$av7 <- frollmean(firms$n, n=7)

        if (input$graphPref == "daily") {

        plot_ly(x = firms$date,
                y = firms$n, type = 'scatter', mode = 'lines+markers',
                color = firms$Group.name) %>%
          layout(
            title = paste0('New registrations by industry group',
                           '<br>',
                           '<sup>',
                           'Between ', input$dateIndustry[1], ' and ', input$dateIndustry[2],
                           '</sup>'),
            xaxis = list(
           #   range=c(input$dateIndustry[1], input$dateIndustry[2]),
              showgrid = F
            ),
            yaxis = list(title = "Number of registrations", showgrid = F),
            legend = list(orientation = 'h',
                          font = list(size = 7),
                          xanchor = "center",  # use center of legend as anchor
                          x = 0.5,             # put legend in center of x-axis))
                          yanchor = -.7)
          )
        } # daily
        else {

          plot_ly(x = firms$date,
                  y = firms$av7, type = 'scatter', mode = 'lines+markers',
                  color = firms$Group.name) %>%
            layout(
              title = paste0('7-day rolling average of new registrations by industry group',
                             '<br>',
                             '<sup>',
                             'Between ', input$dateIndustry[1], ' and ', input$dateIndustry[2],
                             '</sup>'),
              xaxis = list(
                #   range=c(input$dateIndustry[1], input$dateIndustry[2]),
                showgrid = F
              ),
              yaxis = list(title = "Number of registrations", showgrid = F),
              legend = list(orientation = 'h',
                            font = list(size = 7),
                            xanchor = "center",  # use center of legend as anchor
                            x = 0.5,             # put legend in center of x-axis))
                            yanchor = -.7)
            )
        } # 7-days




      }# UK
      else if (input$CountryPicker == 'LDN'){
        # select period
        Tfirmsfilt <- Tfirmsfilt2() %>%
          filter(Country == "England" &
                 Region == "London")

        # Calculate total by date and Class
        Tfirms <- Tfirmsfilt %>%
          group_by(date, Group) %>%
          count()

        firms <- left_join(Tfirms,convert,by="Group")
        # Remove duplicates based on week columns
        firms <- firms %>%
          distinct(Group, date, .keep_all = TRUE)

        firms$av7 <- frollmean(firms$n, n=7)

        if (input$graphPref == "daily") {

        plot_ly(x = firms$date,
                y = firms$n, type = 'scatter', mode = 'lines+markers',
                color = firms$Group.name) %>%
          layout(
            title = paste0('New registrations by industry group',
                           '<br>',
                           '<sup>',
                           'Between ', input$dateIndustry[1], ' and ', input$dateIndustry[2],
                           '</sup>'),
            xaxis = list(
              #   range=c(input$dateIndustry[1], input$dateIndustry[2]),
              showgrid = F
            ),
            yaxis = list(title = "Number of registrations", showgrid = F),
            legend = list(orientation = 'h',
                          font = list(size = 7),
                          xanchor = "center",  # use center of legend as anchor
                          x = 0.5,             # put legend in center of x-axis))
                          yanchor = -.7)
          )
        } # daily
        else {

          plot_ly(x = firms$date,
                  y = firms$av7, type = 'scatter', mode = 'lines+markers',
                  color = firms$Group.name) %>%
            layout(
              title = paste0('7-day rolling average of new registrations by industry group',
                             '<br>',
                             '<sup>',
                             'Between ', input$dateIndustry[1], ' and ', input$dateIndustry[2],
                             '</sup>'),
              xaxis = list(
                #   range=c(input$dateIndustry[1], input$dateIndustry[2]),
                showgrid = F
              ),
              yaxis = list(title = "Number of registrations", showgrid = F),
              legend = list(orientation = 'h',
                            font = list(size = 7),
                            xanchor = "center",  # use center of legend as anchor
                            x = 0.5,             # put legend in center of x-axis))
                            yanchor = -.7)
            )
        } # 7-day
      }# London
      else if (input$CountryPicker == 'Eng'){
        # select period
        countryfilt <- registerSecReg[registerSecReg$Country == "England" &
                                           registerSecReg$Region != "London" ]
        Tfirmsfilt  <- countryfilt[countryfilt$date >= input$dateIndustry[1] &
                                          countryfilt$date <= input$dateIndustry[2] &
                                          countryfilt$Group %in% input$groupPicker ]

        # Calculate total by date and Class
        Tfirms <- Tfirmsfilt %>%
          group_by(date, Group) %>%
          count()

    #    Tfirms$av7 <- frollmean(Tfirms$n, n=7)

        firms <- left_join(Tfirms,convert,by="Group")
        # Remove duplicates based on week columns
        firms <- firms %>%
          distinct(Group, date, .keep_all = TRUE)
        firms$av7 <- frollmean(firms$n, n =7)

        if (input$graphPref == "daily") {

        plot_ly(x = firms$date,
                y = firms$n, type = 'scatter', mode = 'lines+markers',
                color = firms$Group.name) %>%
          layout(
            title = paste0('New registrations by industry group',
                           '<br>',
                           '<sup>',
                           'Between ', input$dateIndustry[1], ' and ', input$dateIndustry[2],
                           '</sup>'),
            xaxis = list(
              #   range=c(input$dateIndustry[1], input$dateIndustry[2]),
              showgrid = F
            ),
            yaxis = list(title = "Number of registrations", showgrid = F),
            legend = list(orientation = 'h',
                          font = list(size = 7),
                          xanchor = "center",  # use center of legend as anchor
                          x = 0.5,             # put legend in center of x-axis))
                          yanchor = -.7)
          ) } # daily
        else {

          plot_ly(x = firms$date,
                  y = firms$av7, type = 'scatter', mode = 'lines+markers',
                  color = firms$Group.name) %>%
            layout(
              title = paste0('7-day rolling average of new registrations by industry group',
                             '<br>',
                             '<sup>',
                             'Between ', input$dateIndustry[1], ' and ', input$dateIndustry[2],
                             '</sup>'),
              xaxis = list(
                #   range=c(input$dateIndustry[1], input$dateIndustry[2]),
                showgrid = F
              ),
              yaxis = list(title = "Number of registrations", showgrid = F),
              legend = list(orientation = 'h',
                            font = list(size = 7),
                            xanchor = "center",  # use center of legend as anchor
                            x = 0.5,             # put legend in center of x-axis))
                            yanchor = -.7)
            ) } # 7-day average

      }# England
      else if (input$CountryPicker == 'NI'){
        # select period
        Tfirmsfilt <- Tfirmsfilt2() %>%
          filter(Country == "Northern Ireland" )

        # Calculate total by date and Class
        Tfirms <- Tfirmsfilt %>%
          group_by(date, Group) %>%
          count()

      #  Tfirms$av7 <- frollmean(Tfirms$n, n=7)

        firms <- left_join(Tfirms,convert,by="Group")
        # Remove duplicates based on week columns
        firms <- firms %>%
          distinct(Group, date, .keep_all = TRUE)

        firms$av7 <- frollmean(firms$n, n = 7)

        if (input$graphPref == "daily") {

        plot_ly(x = firms$date,
                y = firms$n, type = 'scatter', mode = 'lines+markers',
                color = firms$Group.name) %>%
          layout(
            title = paste0('New registrations by industry group',
                           '<br>',
                           '<sup>',
                           'Between ', input$dateIndustry[1], ' and ', input$dateIndustry[2],
                           '</sup>'),
            xaxis = list(
              #   range=c(input$dateIndustry[1], input$dateIndustry[2]),
              showgrid = F
            ),
            yaxis = list(title = "Number of registrations", showgrid = F),
            legend = list(orientation = 'h',
                          font = list(size = 7),
                          xanchor = "center",  # use center of legend as anchor
                          x = 0.5,             # put legend in center of x-axis))
                          yanchor = -.7)
          )} #daily
        else {

          plot_ly(x = firms$date,
                  y = firms$av7, type = 'scatter', mode = 'lines+markers',
                  color = firms$Group.name) %>%
            layout(
              title = paste0('7-day rollin average of new registrations by industry group',
                             '<br>',
                             '<sup>',
                             'Between ', input$dateIndustry[1], ' and ', input$dateIndustry[2],
                             '</sup>'),
              xaxis = list(
                #   range=c(input$dateIndustry[1], input$dateIndustry[2]),
                showgrid = F
              ),
              yaxis = list(title = "Number of registrations", showgrid = F),
              legend = list(orientation = 'h',
                            font = list(size = 7),
                            xanchor = "center",  # use center of legend as anchor
                            x = 0.5,             # put legend in center of x-axis))
                            yanchor = -.7)
            ) } # 7-day

      }# Northern Ireland
      else if (input$CountryPicker == 'Scotland'){
        # select period
        Tfirmsfilt <- Tfirmsfilt2() %>%
          filter(Country == "Scotland" )

        # Calculate total by date and Class
        Tfirms <- Tfirmsfilt %>%
          group_by(date, Group) %>%
          count()

     #   Tfirms$av7 <- frollmean(Tfirms$n, n=7)

        firms <- left_join(Tfirms,convert,by="Group")
        # Remove duplicates based on week columns
        firms <- firms %>%
          distinct(Group, date, .keep_all = TRUE)
        firms$av7 <- frollmean(firms$n, n = 7)

        if (input$graphPref == "daily") {
        plot_ly(x = firms$date,
                y = firms$n, type = 'scatter', mode = 'lines+markers',
                color = firms$Group.name) %>%
          layout(
            title = paste0('New registrations by industry group',
                           '<br>',
                           '<sup>',
                           'Between ', input$dateIndustry[1], ' and ', input$dateIndustry[2],
                           '</sup>'),
            xaxis = list(
              #   range=c(input$dateIndustry[1], input$dateIndustry[2]),
              showgrid = F
            ),
            yaxis = list(title = "Number of registrations", showgrid = F),
            legend = list(orientation = 'h',
                          font = list(size = 7),
                          xanchor = "center",  # use center of legend as anchor
                          x = 0.5,             # put legend in center of x-axis))
                          yanchor = -.7)
          )} # daily
        else {

          plot_ly(x = firms$date,
                  y = firms$av7, type = 'scatter', mode = 'lines+markers',
                  color = firms$Group.name) %>%
            layout(
              title = paste0('7-day rollin average of new registrations by industry group',
                             '<br>',
                             '<sup>',
                             'Between ', input$dateIndustry[1], ' and ', input$dateIndustry[2],
                             '</sup>'),
              xaxis = list(
                #   range=c(input$dateIndustry[1], input$dateIndustry[2]),
                showgrid = F
              ),
              yaxis = list(title = "Number of registrations", showgrid = F),
              legend = list(orientation = 'h',
                            font = list(size = 7),
                            xanchor = "center",  # use center of legend as anchor
                            x = 0.5,             # put legend in center of x-axis))
                            yanchor = -.7)
            ) } # 7-day




      }# Scotland
      else if (input$CountryPicker == 'Wales'){
        # select period
        Tfirmsfilt <- Tfirmsfilt2() %>%
          filter(Country == "Wales" )

        # Calculate total by date and Class
        Tfirms <- Tfirmsfilt %>%
          group_by(date, Group) %>%
          count()

      #  Tfirms$av7 <- frollmean(Tfirms$n, n=7)

        firms <- left_join(Tfirms,convert,by="Group")
        # Remove duplicates based on week columns
        firms <- firms %>%
          distinct(Group, date, .keep_all = TRUE)
        firms$av7 <- frollmean(firms$n, n=7)
        if (input$graphPref == "daily") {
        plot_ly(x = firms$date,
                y = firms$n, type = 'scatter', mode = 'lines+markers',
                color = firms$Group.name) %>%
          layout(
            title = paste0('New registrations by industry group',
                           '<br>',
                           '<sup>',
                           'Between ', input$dateIndustry[1], ' and ', input$dateIndustry[2],
                           '</sup>'),
            xaxis = list(
              #   range=c(input$dateIndustry[1], input$dateIndustry[2]),
              showgrid = F
            ),
            yaxis = list(title = "Number of registrations", showgrid = F),
            legend = list(orientation = 'h',
                          font = list(size = 7),
                          xanchor = "center",  # use center of legend as anchor
                          x = 0.5,             # put legend in center of x-axis))
                          yanchor = -.7)
          )} # daily
        else {
          plot_ly(x = firms$date,
                  y = firms$av7, type = 'scatter', mode = 'lines+markers',
                  color = firms$Group.name) %>%
            layout(
              title = paste0('7-day rollin average of new registrations by industry group',
                             '<br>',
                             '<sup>',
                             'Between ', input$dateIndustry[1], ' and ', input$dateIndustry[2],
                             '</sup>'),
              xaxis = list(
                #   range=c(input$dateIndustry[1], input$dateIndustry[2]),
                showgrid = F
              ),
              yaxis = list(title = "Number of registrations", showgrid = F),
              legend = list(orientation = 'h',
                            font = list(size = 7),
                            xanchor = "center",  # use center of legend as anchor
                            x = 0.5,             # put legend in center of x-axis))
                            yanchor = -.7)
            ) } # 7-day




      }# Wales

    })

    secIwant <- reactive({
      sec <- registerSecReg[which(registerSecReg$Group %in% input$groupPicker), ]
      unique(sec$Section)
    })
    Tfirmsfilt3 <- reactive({
      registerSecReg[registerSecReg$date >= input$dateIndustry[1] &
                       registerSecReg$date <= input$dateIndustry[2] &
                       registerSecReg$Section %in% secIwant() ]
    })

    output$sectorPlot <- renderPlotly({
      if (input$CountryPicker == 'UK'){

        # Calculate total by date and Class
        Tfirms <- Tfirmsfilt3() %>%
          group_by(date, Section) %>%
          count()

        firms <- left_join(Tfirms,convert,by="Section")
        # Remove duplicates based on week columns
        firms <- firms %>%
          distinct(Section, date, .keep_all = TRUE)
        firms$av7 <- frollmean(firms$n, n=7)
        if (input$graphPref == "daily") {
        plot_ly(x = firms$date,
                y = firms$n, type = 'scatter', mode = 'lines+markers',
                color = firms$Section.name) %>%
          layout(
            title = paste0('New registrations by industry Section',
                           '<br>',
                           '<sup>',
                           'Between ', input$dateIndustry[1], ' and ', input$dateIndustry[2],
                           '<br>',
                           'Selected Sections shown according to Groups choice.',
                           '</sup>'),
            xaxis = list(
              #   range=c(input$dateIndustry[1], input$dateIndustry[2]),
              showgrid = F
            ),
            yaxis = list(title = "Number of registrations", showgrid = F),
            legend = list(orientation = 'h',
                          font = list(size = 7),
                          xanchor = "center",  # use center of legend as anchor
                          x = 0.5,             # put legend in center of x-axis))
                          yanchor = -.7)
          )
        } # daily
        else {
          plot_ly(x = firms$date,
                  y = firms$av7, type = 'scatter', mode = 'lines+markers',
                  color = firms$Section.name) %>%
            layout(
              title = paste0('7-day rolling average of new registrations by industry Section',
                             '<br>',
                             '<sup>',
                             'Between ', input$dateIndustry[1], ' and ', input$dateIndustry[2],
                             '<br>',
                             'Selected Sections shown according to Groups choice.',
                             '</sup>'),
              xaxis = list(
                #   range=c(input$dateIndustry[1], input$dateIndustry[2]),
                showgrid = F
              ),
              yaxis = list(title = "Number of registrations", showgrid = F),
              legend = list(orientation = 'h',
                            font = list(size = 7),
                            xanchor = "center",  # use center of legend as anchor
                            x = 0.5,             # put legend in center of x-axis))
                            yanchor = -.7)
            )
        } # 7-day rolling



      } #uk
      else  if (input$CountryPicker == 'LDN'){
        Tfirmsfilt <- Tfirmsfilt3() %>%
          filter(Country == "England" &
                 Region == "London" )


        # Calculate total by date and Class
        Tfirms <- Tfirmsfilt %>%
          group_by(date, Section) %>%
          count()

        firms <- left_join(Tfirms,convert,by="Section")
        # Remove duplicates based on week columns
        firms <- firms %>%
          distinct(Section, date, .keep_all = TRUE)
        firms$av7 <- frollmean(firms$n, n=7)
        if (input$graphPref == "daily") {
          plot_ly(x = firms$date,
                  y = firms$n, type = 'scatter', mode = 'lines+markers',
                  color = firms$Section.name) %>%
            layout(
              title = paste0('New registrations by industry Section',
                             '<br>',
                             '<sup>',
                             'Between ', input$dateIndustry[1], ' and ', input$dateIndustry[2],
                             '<br>',
                             'Selected Sections shown according to Groups choice.',
                             '</sup>'),
              xaxis = list(
                #   range=c(input$dateIndustry[1], input$dateIndustry[2]),
                showgrid = F
              ),
              yaxis = list(title = "Number of registrations", showgrid = F),
              legend = list(orientation = 'h',
                            font = list(size = 7),
                            xanchor = "center",  # use center of legend as anchor
                            x = 0.5,             # put legend in center of x-axis))
                            yanchor = -.7)
            )
        } # daily
        else {
          plot_ly(x = firms$date,
                  y = firms$av7, type = 'scatter', mode = 'lines+markers',
                  color = firms$Section.name) %>%
            layout(
              title = paste0('7-day rolling average of new registrations by industry Section',
                             '<br>',
                             '<sup>',
                             'Between ', input$dateIndustry[1], ' and ', input$dateIndustry[2],
                             '<br>',
                             'Selected Sections shown according to Groups choice.',
                             '</sup>'),
              xaxis = list(
                #   range=c(input$dateIndustry[1], input$dateIndustry[2]),
                showgrid = F
              ),
              yaxis = list(title = "Number of registrations", showgrid = F),
              legend = list(orientation = 'h',
                            font = list(size = 7),
                            xanchor = "center",  # use center of legend as anchor
                            x = 0.5,             # put legend in center of x-axis))
                            yanchor = -.7)
            )
        } # 7-day rolling



      } # London
      else if (input$CountryPicker == 'Eng'){
        # select period

        Tfirmsfilt <- Tfirmsfilt3() %>% filter(Country == "England" &
                                             Region != "London" )


        # Calculate total by date and Class
        Tfirms <- Tfirmsfilt %>%
          group_by(date, Section) %>%
          count()

        firms <- left_join(Tfirms,convert,by="Section")
        # Remove duplicates based on week columns
        firms <- firms %>%
          distinct(Section, date, .keep_all = TRUE)
        firms$av7 <- frollmean(firms$n, n=7)
        if (input$graphPref == "daily") {
          plot_ly(x = firms$date,
                  y = firms$n, type = 'scatter', mode = 'lines+markers',
                  color = firms$Section.name) %>%
            layout(
              title = paste0('New registrations by industry Section',
                             '<br>',
                             '<sup>',
                             'Between ', input$dateIndustry[1], ' and ', input$dateIndustry[2],
                             '<br>',
                             'Selected Sections shown according to Groups choice.',
                             '</sup>'),
              xaxis = list(
                #   range=c(input$dateIndustry[1], input$dateIndustry[2]),
                showgrid = F
              ),
              yaxis = list(title = "Number of registrations", showgrid = F),
              legend = list(orientation = 'h',
                            font = list(size = 7),
                            xanchor = "center",  # use center of legend as anchor
                            x = 0.5,             # put legend in center of x-axis))
                            yanchor = -.7)
            )
        } # daily
        else {
          plot_ly(x = firms$date,
                  y = firms$av7, type = 'scatter', mode = 'lines+markers',
                  color = firms$Section.name) %>%
            layout(
              title = paste0('7-day rolling average of new registrations by industry Section',
                             '<br>',
                             '<sup>',
                             'Between ', input$dateIndustry[1], ' and ', input$dateIndustry[2],
                             '<br>',
                             'Selected Sections shown according to Groups choice.',
                             '</sup>'),
              xaxis = list(
                #   range=c(input$dateIndustry[1], input$dateIndustry[2]),
                showgrid = F
              ),
              yaxis = list(title = "Number of registrations", showgrid = F),
              legend = list(orientation = 'h',
                            font = list(size = 7),
                            xanchor = "center",  # use center of legend as anchor
                            x = 0.5,             # put legend in center of x-axis))
                            yanchor = -.7)
            )
        } # 7-day rolling



      } # England no ldn
      else  if (input$CountryPicker == 'NI'){

        Tfirmsfilt <- Tfirmsfilt3() %>% filter(Country == "Northern Ireland" )

        # Calculate total by date and Class
        Tfirms <- Tfirmsfilt %>%
          group_by(date, Section) %>%
          count()

        firms <- left_join(Tfirms,convert,by="Section")
        # Remove duplicates based on week columns
        firms <- firms %>%
          distinct(Section, date, .keep_all = TRUE)
        firms$av7 <- frollmean(firms$n, n=7)
        if (input$graphPref == "daily") {
          plot_ly(x = firms$date,
                  y = firms$n, type = 'scatter', mode = 'lines+markers',
                  color = firms$Section.name) %>%
            layout(
              title = paste0('New registrations by industry Section',
                             '<br>',
                             '<sup>',
                             'Between ', input$dateIndustry[1], ' and ', input$dateIndustry[2],
                             '<br>',
                             'Selected Sections shown according to Groups choice.',
                             '</sup>'),
              xaxis = list(
                #   range=c(input$dateIndustry[1], input$dateIndustry[2]),
                showgrid = F
              ),
              yaxis = list(title = "Number of registrations", showgrid = F),
              legend = list(orientation = 'h',
                            font = list(size = 7),
                            xanchor = "center",  # use center of legend as anchor
                            x = 0.5,             # put legend in center of x-axis))
                            yanchor = -.7)
            )
        } # daily
        else {
          plot_ly(x = firms$date,
                  y = firms$av7, type = 'scatter', mode = 'lines+markers',
                  color = firms$Section.name) %>%
            layout(
              title = paste0('7-day rolling average of new registrations by industry Section',
                             '<br>',
                             '<sup>',
                             'Between ', input$dateIndustry[1], ' and ', input$dateIndustry[2],
                             '<br>',
                             'Selected Sections shown according to Groups choice.',
                             '</sup>'),
              xaxis = list(
                #   range=c(input$dateIndustry[1], input$dateIndustry[2]),
                showgrid = F
              ),
              yaxis = list(title = "Number of registrations", showgrid = F),
              legend = list(orientation = 'h',
                            font = list(size = 7),
                            xanchor = "center",  # use center of legend as anchor
                            x = 0.5,             # put legend in center of x-axis))
                            yanchor = -.7)
            )
        } # 7-day rolling



      } #NI
      else if (input$CountryPicker == 'Scotland'){


        Tfirmsfilt <- Tfirmsfilt3() %>% filter(Country == "Scotland" )


        # Calculate total by date and Class
        Tfirms <- Tfirmsfilt %>%
          group_by(date, Section) %>%
          count()

        firms <- left_join(Tfirms,convert,by="Section")
        # Remove duplicates based on week columns
        firms <- firms %>%
          distinct(Section, date, .keep_all = TRUE)
        firms$av7 <- frollmean(firms$n, n=7)
        if (input$graphPref == "daily") {
          plot_ly(x = firms$date,
                  y = firms$n, type = 'scatter', mode = 'lines+markers',
                  color = firms$Section.name) %>%
            layout(
              title = paste0('New registrations by industry Section',
                             '<br>',
                             '<sup>',
                             'Between ', input$dateIndustry[1], ' and ', input$dateIndustry[2],
                             '<br>',
                             'Selected Sections shown according to Groups choice.',
                             '</sup>'),
              xaxis = list(
                #   range=c(input$dateIndustry[1], input$dateIndustry[2]),
                showgrid = F
              ),
              yaxis = list(title = "Number of registrations", showgrid = F),
              legend = list(orientation = 'h',
                            font = list(size = 7),
                            xanchor = "center",  # use center of legend as anchor
                            x = 0.5,             # put legend in center of x-axis))
                            yanchor = -.7)
            )
        } # daily
        else {
          plot_ly(x = firms$date,
                  y = firms$av7, type = 'scatter', mode = 'lines+markers',
                  color = firms$Section.name) %>%
            layout(
              title = paste0('7-day rolling average of new registrations by industry Section',
                             '<br>',
                             '<sup>',
                             'Between ', input$dateIndustry[1], ' and ', input$dateIndustry[2],
                             '<br>',
                             'Selected Sections shown according to Groups choice.',
                             '</sup>'),
              xaxis = list(
                #   range=c(input$dateIndustry[1], input$dateIndustry[2]),
                showgrid = F
              ),
              yaxis = list(title = "Number of registrations", showgrid = F),
              legend = list(orientation = 'h',
                            font = list(size = 7),
                            xanchor = "center",  # use center of legend as anchor
                            x = 0.5,             # put legend in center of x-axis))
                            yanchor = -.7)
            )
        } # 7-day rolling



      } # scotland
      else  if (input$CountryPicker == 'Wales'){

        Tfirmsfilt <- Tfirmsfilt3() %>% filter(Country == "Wales" )


        # Calculate total by date and Class
        Tfirms <- Tfirmsfilt %>%
          group_by(date, Section) %>%
          count()

        firms <- left_join(Tfirms,convert,by="Section")
        # Remove duplicates based on week columns
        firms <- firms %>%
          distinct(Section, date, .keep_all = TRUE)
        firms$av7 <- frollmean(firms$n, n=7)
        if (input$graphPref == "daily") {
          plot_ly(x = firms$date,
                  y = firms$n, type = 'scatter', mode = 'lines+markers',
                  color = firms$Section.name) %>%
            layout(
              title = paste0('New registrations by industry Section',
                             '<br>',
                             '<sup>',
                             'Between ', input$dateIndustry[1], ' and ', input$dateIndustry[2],
                             '<br>',
                             'Selected Sections shown according to Groups choice.',
                             '</sup>'),
              xaxis = list(
                #   range=c(input$dateIndustry[1], input$dateIndustry[2]),
                showgrid = F
              ),
              yaxis = list(title = "Number of registrations", showgrid = F),
              legend = list(orientation = 'h',
                            font = list(size = 7),
                            xanchor = "center",  # use center of legend as anchor
                            x = 0.5,             # put legend in center of x-axis))
                            yanchor = -.7)
            )
        } # daily
        else {
          plot_ly(x = firms$date,
                  y = firms$av7, type = 'scatter', mode = 'lines+markers',
                  color = firms$Section.name) %>%
            layout(
              title = paste0('7-day rolling average of new registrations by industry Section',
                             '<br>',
                             '<sup>',
                             'Between ', input$dateIndustry[1], ' and ', input$dateIndustry[2],
                             '<br>',
                             'Selected Sections shown according to Groups choice.',
                             '</sup>'),
              xaxis = list(
                #   range=c(input$dateIndustry[1], input$dateIndustry[2]),
                showgrid = F
              ),
              yaxis = list(title = "Number of registrations", showgrid = F),
              legend = list(orientation = 'h',
                            font = list(size = 7),
                            xanchor = "center",  # use center of legend as anchor
                            x = 0.5,             # put legend in center of x-axis))
                            yanchor = -.7)
            )
        } # 7-day rolling



      } # Wales



    })#sectorPlot

    output$group2 <- renderDT({
      sec <- registerSecReg[which(registerSecReg$Group %in% input$groupPicker2), ]
      groupNumberIwant <- unique(sec$Group)
      groupIwant <- unique(sec$Group.name)
      secIwant <- unique(sec$Section.name)
      sectionIwant <- unique(sec$Section)

      tbl <- data.frame("A" = c( groupNumberIwant),
                        "B" = c( groupIwant),
                        "C" = c( sectionIwant),
                        "D" = c( secIwant))
      container_dt= withTags(table(
        class = 'display',
        thead(
          tr(
            th(class = 'dt-center',colspan = 2, '3-digit classification'),
            th(class = 'dt-center',colspan = 2, '1-digit classification')),
          tr(
            lapply((c('Group', 'Group Name', 'Section', 'Section Name')), th)))))
      datatable(tbl, container = container_dt, rownames = F, class = "",
                options = list(autoWidth = T, lengthChange = FALSE,
                               columnDefs = list(list(className = "dt-center",
                                                      targets = "_all"),
                                                 list(width = "40px",
                                                      target = "_all"))))





    })

    Tfirmsfilt4 <- reactive({
      registerSecReg[registerSecReg$date >= input$dateIndustry2[1] &
                       registerSecReg$date <= input$dateIndustry2[2] &
                       registerSecReg$Group %in% input$groupPicker2 ]
    })

    output$classPlot2 <- renderPlotly({

      firms <- Tfirmsfilt4() %>%
        group_by(date, postcodeArea) %>%
        count()

      firms <- left_join(firms, regionsUK,by="postcodeArea")

      firmsLondon <- subset(firms, Region=="London")
      firmsLondonAg <- aggregate(firmsLondon$n, by = list(date = firmsLondon$date,
                                                          Country = firmsLondon$Country),
                                 FUN = sum)
      firmsLondonAg$Country <- sub("England", "London", firmsLondonAg$Country)
      firmsLondonAg$av7 <- frollmean(firmsLondonAg$x, n=7)


      '%!in%' <- function(x,y)!('%in%'(x,y))
      firmsNoLDN <- subset(firms, Region!="London")
      firmsNoLDNAg <- aggregate(firmsNoLDN$n, by = list(date = firmsNoLDN$date,
                                                        Country = firmsNoLDN$Country),
                                FUN = sum)
      firmsNoLDNAg$Country <- sub("England", "England (exl. LDN)", firmsNoLDNAg$Country)
      firmsNoLDNAg$av7 <- frollmean(firmsNoLDNAg$x, n = 7)

      if (input$graphPref2 == "daily") {

      plot_ly(x = firmsNoLDNAg$date,
              y = firmsNoLDNAg$x, type = 'scatter',
              color = firmsNoLDNAg$Country, mode = 'lines+markers') %>%
        add_trace(y = firmsLondonAg$x, x= firmsLondonAg$date,
                  name = 'London', type='scatter', mode = 'lines+markers',
                  color = firmsLondonAg$Country) %>%
        layout(
          title = paste0('New registrations by country',
                         '<br>',
                         '<sup>',
                         'Between ', input$dateIndustry[1], ' and ', input$dateIndustry[2],
                         '</sup>'),
          xaxis = list(
            showgrid = F
          ),
          yaxis = list(title = "Number of registrations", showgrid = F),
          legend = list(orientation = 'h',
                        font = list(size = 7),
                        xanchor = "center",  # use center of legend as anchor
                        x = 0.5,             # put legend in center of x-axis))
                        yanchor = -.7)
        )
      } #daily
      else {

        plot_ly(x = firmsNoLDNAg$date,
                y = firmsNoLDNAg$av7, type = 'scatter',
                color = firmsNoLDNAg$Country, mode = 'lines+markers') %>%
          add_trace(y = firmsLondonAg$av7, x= firmsLondonAg$date,
                    name = 'London', type='scatter', mode = 'lines+markers',
                    color = firmsLondonAg$Country) %>%
          layout(
            title = paste0('7-day rolling average of new registrations by country',
                           '<br>',
                           '<sup>',
                           'Between ', input$dateIndustry[1], ' and ', input$dateIndustry[2],
                           '</sup>'),
            xaxis = list(
              showgrid = F
            ),
            yaxis = list(title = "Number of registrations", showgrid = F),
            legend = list(orientation = 'h',
                          font = list(size = 7),
                          xanchor = "center",  # use center of legend as anchor
                          x = 0.5,             # put legend in center of x-axis))
                          yanchor = -.7)
          )
      } #daily

    })#group plot 1 industry by countries

    output$sectorPlot2 <- renderPlotly({
      sec <- registerSecReg[which(registerSecReg$Group %in% input$groupPicker2), ]
      secIwant <- unique(sec$Section)

      Tfirmsfilt <- registerSecReg[registerSecReg$date >= input$dateIndustry2[1] &
                                           registerSecReg$date <= input$dateIndustry2[2] &
                                           registerSecReg$Section %in% secIwant ]

      firms <- Tfirmsfilt %>%
        group_by(date, postcodeArea) %>%
        count()

      firms <- left_join(firms, regionsUK,by="postcodeArea")

      firmsLondon <- subset(firms, Region=="London")
      firmsLondonAg <- aggregate(firmsLondon$n, by = list(date = firmsLondon$date,
                                                          Country = firmsLondon$Country),
                                 FUN = sum)
      firmsLondonAg$Country <- sub("England", "London", firmsLondonAg$Country)
      firmsLondonAg$av7 <- frollmean(firmsLondonAg$x, n=7)


      '%!in%' <- function(x,y)!('%in%'(x,y))
      firmsNoLDN <- subset(firms, Region!="London")
      firmsNoLDNAg <- aggregate(firmsNoLDN$n, by = list(date = firmsNoLDN$date,
                                                        Country = firmsNoLDN$Country),
                                FUN = sum)
      firmsNoLDNAg$Country <- sub("England", "England (exl. LDN)", firmsNoLDNAg$Country)
      firmsNoLDNAg$av7 <- frollmean(firmsNoLDNAg$x, n = 7)

      if (input$graphPref2 == "daily") {

        plot_ly(x = firmsNoLDNAg$date,
                y = firmsNoLDNAg$x, type = 'scatter',
                color = firmsNoLDNAg$Country, mode = 'lines+markers') %>%
          add_trace(y = firmsLondonAg$x, x= firmsLondonAg$date,
                    name = 'London', type='scatter', mode = 'lines+markers',
                    color = firmsLondonAg$Country) %>%
          layout(
            title = paste0('New registrations by country',
                           '<br>',
                           '<sup>',
                           'Between ', input$dateIndustry[1], ' and ', input$dateIndustry[2],
                           '</sup>'),
            xaxis = list(
              showgrid = F
            ),
            yaxis = list(title = "Number of registrations", showgrid = F),
            legend = list(orientation = 'h',
                          font = list(size = 7),
                          xanchor = "center",  # use center of legend as anchor
                          x = 0.5,             # put legend in center of x-axis))
                          yanchor = -.7)
          )
      } #daily
      else {

        plot_ly(x = firmsNoLDNAg$date,
                y = firmsNoLDNAg$av7, type = 'scatter',
                color = firmsNoLDNAg$Country, mode = 'lines+markers') %>%
          add_trace(y = firmsLondonAg$av7, x= firmsLondonAg$date,
                    name = 'London', type='scatter', mode = 'lines+markers',
                    color = firmsLondonAg$Country) %>%
          layout(
            title = paste0('7-day rolling average of new registrations by country',
                           '<br>',
                           '<sup>',
                           'Between ', input$dateIndustry[1], ' and ', input$dateIndustry[2],
                           '</sup>'),
            xaxis = list(
              showgrid = F
            ),
            yaxis = list(title = "Number of registrations", showgrid = F),
            legend = list(orientation = 'h',
                          font = list(size = 7),
                          xanchor = "center",  # use center of legend as anchor
                          x = 0.5,             # put legend in center of x-axis))
                          yanchor = -.7)
          )
      } #daily



    }) # sector plot 1 industry by countries


}

# Run the application ----
shinyApp(ui = ui, server = server)
