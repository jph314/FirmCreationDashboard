# SERVER ----
server <- function(input, output) {
  # Info
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
  }) #info
  
  # Authors
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
  }) #authors
  
  # Daily registrations per NUTS1 region for entire dataset, to be further filtered and aggregated below.
  Tcountry <- registerPC %>% group_by(NUTS1, date) %>% count() %>% as.data.table()
  
  ## AGGREGATE STATS ====
  # Select NUTS1 regions according to country/region selected by user.
  countrySel <- reactive({
    countrySelect(input$pickCountry)
  })

  ### Value boxes
  # Sum registrations according to selected NUTS1 regions and relevant date ranges.
  output$vboxTotal <- renderValueBox(
    vboxes(1, input$dateAgg[1], input$dateAgg[2], countrySel(), Tcountry)
  )
  output$vboxVaccine <- renderValueBox(
    vboxes(2, as.Date("2020-12-08"), input$dateAgg[2], countrySel(), Tcountry)
  )
  output$vboxLD <- renderValueBox(
    vboxes(3, as.Date("2021-01-05"), as.Date("2021-04-11"), countrySel(), Tcountry)
  )
  output$vboxOpen <- renderValueBox(
    vboxes(4, as.Date("2021-04-12"), input$dateAgg[2], countrySel(), Tcountry)
  ) #VBoxes
  
  ### Daily registrations plot
  output$rollingAvg <- renderPlotly({
    dailyPlot(input$dateAgg[1], input$dateAgg[2], countrySel(), Tcountry)
  }) #Daily plot
  
  ### UK NUTS2 map
  output$UKmap <- renderPlotly({
    showMap(input$dateAgg[1], input$dateAgg[2])   
  })
  
  ### Sector tree map
  sunburstDF <- reactive({
    as.sunburstDF(
      # Aggregate registrations by SIC Division in selected date range and regions from full dataset.
      registerPC[which(between(registerPC$date, input$dateAgg[1], input$dateAgg[2]) & 
                         registerPC$NUTS1 %in% countrySel()),] %>% 
      group_by(SectionAbb, Division.name) %>% count(),
      valueCol = "n"
    )
  })
  #Draw treemap.
  output$treemap <- renderPlotly({
    drawTreemap(sunburstDF(), input$dateAgg[1], input$dateAgg[2])
  }) #Treemap
  
  ### Industry divisions plots
  # Count daily registrations per SIC Group in selected date range, regions and SIC Group.
  Tgrp <- reactive(registerPC[which(between(registerPC$date, input$dateAgg[1], input$dateAgg[2]) & 
                                      registerPC$Group %in% input$groupPicker & 
                                      registerPC$NUTS1 %in% countrySel()),] %>%
                     group_by(date, Group, Group.name, Section, Section.name) %>% 
                     count() %>% ungroup() %>% 
                     #Add rolling average for each Group.
                     group_by(Group) %>% mutate(avg = frollmean(n, n=7)))
  # Selected Groups in a table.
  secTable <- reactive(distinct(Tgrp()[-c(1,6,7)], Group, .keep_all = TRUE))
  output$groups <- renderDT({
    showTable(secTable())
  }) #Groups table
  # Industry groups plot
  output$groupsPlot <- renderPlotly({
    groupPlot(Tgrp())
  }) #Groups plot
  # Industry sectors plot
  output$sectorsPlot <- renderPlotly({
    sectorPlot(input$dateAgg[1], input$dateAgg[2], secTable()$Section, countrySel())
  }) #Sectors plot
  #Industry plots
  
  ### Regional comparison plots by division
  # Selected group plot.
  output$groupsRegion <- renderPlotly({
    groupRegion(input$dateAgg[1], input$dateAgg[2], input$groupPicker2, 1)
  }) #Group plot
  # Selected sector plot.
  output$sectorsRegion <- renderPlotly({
    groupRegion(input$dateAgg[1], input$dateAgg[2], input$groupPicker2, 2)
  }) #Sector plot
  
} #Server
