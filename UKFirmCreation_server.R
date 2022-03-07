# SERVER ----
server <- function(input, output) {
  # Info
  output$info <- renderMenu({
    dropdownMenu(
      type = "messages",
      icon = icon("info"), badgeStatus = "primary",
      headerText = tags$b("Project ID"),
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
  }) # info

  # Authors
  output$authors <- renderMenu({
    dropdownMenu(
      type = "notifications", icon = icon("users"), badgeStatus = "primary",
      headerText = tags$b("Team"),
      notificationItem(
        icon = icon("user"), status = "info",
        "Yannis Galanakis", href = "https://www.yannisgalanakis.com/"
      ),
      notificationItem(
        icon = icon("user"), status = "info",
        "Jonathan Hobbs"
      ),
      notificationItem(
        icon = icon("user"), status = "info",
        "Anthony Savagar", href = "https://www.asavagar.com/"
      )
    )
  }) # authors

  # Daily registrations per NUTS1 region for entire dataset, to be further filtered and aggregated below.
  Tcountry <- registerPC %>%
    group_by(NUTS1, date) %>%
    count() %>%
    as.data.table()

  ## AGGREGATE STATS ====
  # Select NUTS1 regions according to country/region selected by user.
  countrySel <- reactive({
    countrySelect(input$pickCountry)
  })
  
  # Modify text in headers depending on country/region.
  output$countryText <- renderUI(h2(paste0("Statistics of companies in ",input$pickCountry)))

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
  ) # VBoxes

  ### Daily registrations plot
  output$rollingAvg <- renderPlotly({
    dailyPlot(input$dateAgg[1], input$dateAgg[2], countrySel(), Tcountry, input$pickCountry)
  }) # Daily plot

  # Download data
  output$dailyRegDownload <- downloadHandler(
    filename = function(){paste0("Daily_Registrations_", input$dateAgg[1], "--", input$dateAgg[2], "_", input$pickCountry, ".csv")}, 
    content = function(fname){
      write.csv(dailyData(input$dateAgg[1], input$dateAgg[2], countrySel(), Tcountry, input$pickCountry), fname, row.names=F)
    }
  )

  ### UK NUTS2 map
  output$UKmap <- renderPlotly({
    showMap(input$dateAgg[1], input$dateAgg[2])
  }) # UK NUTS2 map

  # Download data
  output$NUTS2Download <- downloadHandler(
    filename = function(){paste0("Total_Registrations_byRegion_", input$dateAgg[1], "--", input$dateAgg[2],".csv")}, 
    content = function(fname){
      write.csv(NUTSdata(input$dateAgg[1], input$dateAgg[2]), fname, row.names=F)
    }
  )

  Tdivision <- reactive(
    # Aggregate registrations by SIC Division in selected date range and regions from full dataset.
    registerPC[which(between(registerPC$date, input$dateAgg[1], input$dateAgg[2]) &
      registerPC$NUTS1 %in% countrySel()), ] %>%
      group_by(SectionAbb, Division.name) %>%
      count()
  )
  ### Sector tree map
  sunburstDF <- reactive({
    as.sunburstDF(
      Tdivision(),
      valueCol = "n"
    )
  })
  # Draw treemap.
  output$treemap <- renderPlotly({
    drawTreemap(sunburstDF(), input$dateAgg[1], input$dateAgg[2], input$pickCountry)
  }) # Treemap

  # Sector donut plot
  output$donut <- renderPlotly({
    drawDonut(Tdivision(), input$dateAgg[1], input$dateAgg[2], input$pickCountry)
  }) # Donut

  # Download data from donut
  output$SectorDown <- downloadHandler(
    filename = function(){paste0("Registrations_bySector_", input$dateAgg[1], "--", input$dateAgg[2], "_", input$pickCountry, ".csv")}, 
    content = function(file){
      write.csv(donutData(Tdivision(), input$dateAgg[1], input$dateAgg[2]), file, row.names = F)
    }
  ) # download data of donut
  
  output$DivisionDownload <-downloadHandler(
    filename = function(){paste0("Registrations_byDivisionSector_", input$dateAgg[1], "--", input$dateAgg[2], "_", input$pickCountry, ".csv")}, 
    content = function(file){
      write.csv(Tdivision(), file, row.names = F)
    }
  )
  
  

  ### Industry divisions plots
  # Count daily registrations per SIC Group in selected date range, regions and SIC Group.
  Tgrp <- reactive(registerPC[which(between(registerPC$date, input$dateAgg[1], input$dateAgg[2]) &
    registerPC$Group %in% input$groupPicker &
    registerPC$NUTS1 %in% countrySel()), ] %>%
    group_by(date, Group, Group.name, Section, Section.name) %>%
    count() %>% ungroup() %>%
    # Add rolling average for each Group.
    group_by(Group) %>% mutate(avg = frollmean(n, n = 7)))
  # Selected Groups in a table.
  secTable <- reactive(distinct(Tgrp()[-c(1, 6, 7)], Group, .keep_all = TRUE))
  output$groups <- renderDT({
    showTable(secTable())
  }) # Groups table
  
  # Industry groups plot
  output$groupsPlot <- renderPlotly({
    groupPlot(Tgrp(), input$pickCountry)
  }) # Groups plot
  # Download data
  output$groupsDownload <-downloadHandler(
    filename = function(){paste0("Registrations_by_group_", input$dateAgg[1], "--", input$dateAgg[2], "_", input$pickCountry, ".csv")}, 
    content = function(file){
      write.csv(Tgrp(), file, row.names = F)
    }
  )
  
  # Daily registrations per Section for selected regions, dates and distinct Sections from selected Groups.
  Tsec <- reactive(registerPC[which(between(registerPC$date, input$dateAgg[1], input$dateAgg[2]) &
                                        registerPC$Section %in% secTable()$Section &
                                        registerPC$NUTS1 %in% countrySel()), ] %>%
                       group_by(date, Section, Section.name) %>%
                       count() %>%
                       ungroup() %>%
                       group_by(Section) %>%
                       mutate(avg = frollmean(n, n = 7)))
  # Industry sectors plot
  output$sectorsPlot <- renderPlotly({
    sectorPlot(Tsec(), input$pickCountry)
  }) # Sectors plot
  # Download data
  output$sectorsDownload <-downloadHandler(
    filename = function(){paste0("Registrations_by_sector_", input$dateAgg[1], "--", input$dateAgg[2], "_", input$pickCountry, ".csv")}, 
    content = function(file){
      write.csv(Tsec(), file, row.names = F)
    }
  )
  # Industry plots  
  
  ### Regional comparison plots by division
  # Selected group data.
  TregionGroup <- reactive(groupRegionData(input$dateAgg[1], input$dateAgg[2], input$groupPicker2, 1))
  # Selected group plot.
  output$groupsRegion <- renderPlotly({
    groupRegion(TregionGroup(), input$groupPicker2, 1)
  }) # Group plot
  # Download data
  output$regionGroupDownload <-downloadHandler(
    filename = function(){paste0("Registrations_in_",input$groupPicker2,"_by_region_", input$dateAgg[1], "--", input$dateAgg[2], ".csv")}, 
    content = function(file){
      write.csv(TregionGroup(), file, row.names = F)
    }
  )
  
  # Selected sector data.
  TregionSector <- reactive(groupRegionData(input$dateAgg[1], input$dateAgg[2], input$groupPicker2, 2))
  # Selected sector plot.
  output$sectorsRegion <- renderPlotly({
    groupRegion(TregionSector(), input$groupPicker2, 2)
  }) # Sector plot
  output$regionSectorDownload <-downloadHandler(
    filename = function(){paste0("Registrations_in_",
                                 registerPC$SectionAbb[which(registerPC$Group == input$groupPicker2)][1],
                                 "_by_region_", input$dateAgg[1], "--", input$dateAgg[2], ".csv")}, 
    content = function(file){
      write.csv(TregionSector(), file, row.names = F)
    }
  )
  
Tcustom <- registerPC %>%
    group_by(date, Class, postcodeDistrict) %>%
    count() %>%
    as.data.table()
  
output$customdata  <- renderDT({
    totalRegistrations(input$dateAgg[1], input$dateAgg[2], Tcustom, input$pickPostcode, input$pickSIC)
  })

# Download data
output$customDownload <- downloadHandler(
  filename = function(){paste0("Custom_Registrations_", input$dateAgg[1], "--", input$dateAgg[2], ".csv")}, 
  content = function(fname){
    write.csv(customDataDownload(input$dateAgg[1], input$dateAgg[2], Tcustom, input$pickPostcode, input$pickSIC), 
              fname, row.names=F)
  }
)

} # Server
