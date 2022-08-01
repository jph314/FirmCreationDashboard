# SERVER ----
server <- function(input, output, session) {
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
        href = "mailto:galanakis.gian@gmail.com",
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

  # load data depending on user's selection
  # Select data version: latest register vs. archive
  # 1. I create a reactive, register(), that contains the rows from registerPC
  #  that match the selected data
  # register <- reactive({
  #   register1[which(register1$archive == input$pickData), ]
  # })

  # Daily registrations per NUTS1 region for entire dataset, to be further filtered and aggregated below.
  # Tcountry <- reactive(
  #   register() %>%
  #     group_by(NUTS1, date) %>%
  #     count()
  # )

  ## AGGREGATE STATS ====
  # Select country/region selected by user.
  Tcountry <- reactive({
    setDT(countrySelect(input$pickCountry, register, input$dateAgg[1], input$dateAgg[2]))
  })

  # Modify text in headers depending on country/region.
  output$countryText <- renderUI(h2(paste0("Statistics of companies in ", input$pickCountry)))

  ### Value boxes
  # Sum registrations according to selected NUTS1 regions and relevant date ranges.
  output$vboxTotal <- renderValueBox(
    vboxes(1, input$dateAgg[1], input$dateAgg[2], Tcountry())
  )
  # output$vboxActive <- renderValueBox(
  #   active(register1[which(register1$archive == "Latest register"), ])
  # )

  ### Daily registrations plot
  output$rollingAvg <- renderPlotly({
    dailyPlot(input$dateAgg[1], input$dateAgg[2], Tcountry(), input$pickCountry)
  }) # Daily plot

  # Download data
  output$dailyRegDownload <- downloadHandler(
    filename = function() {
      paste0("Daily_Registrations_", input$dateAgg[1], "--", input$dateAgg[2], "_", input$pickCountry, ".csv")
    },
    content = function(fname) {
      write.csv(dailyData(input$dateAgg[1], input$dateAgg[2], countrySel(), Tcountry(), input$pickCountry), fname, row.names = F)
    }
  )

  ### UK NUTS2 map
  output$UKmap <- renderPlotly({
    showMap(register, input$dateAgg[1], input$dateAgg[2], "registrations")
  }) # UK NUTS2 map

  # Download data
  output$NUTS2Download <- downloadHandler(
    filename = function() {
      paste0("Total_Registrations_byRegion_", input$dateAgg[1], "--", input$dateAgg[2], ".csv")
    },
    content = function(fname) {
      write.csv(LAdata(register, input$dateAgg[1], input$dateAgg[2]), fname, row.names = F)
    }
  )

  Tdivision <- reactive(
    # Aggregate registrations by SIC Division in selected date range and regions from full dataset.
    Tcountry()[date>=input$dateAgg[1],list(n=sum(n)),by=list(SectionAbb, Division.name)]
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

  # # Sector donut plot
  # output$donut <- renderPlotly({
  #   drawDonut(Tdivision(), input$dateAgg[1], input$dateAgg[2], input$pickCountry)
  # }) # Donut

  # # Download data from donut
  # output$SectorDown <- downloadHandler(
  #   filename = function() {
  #     paste0("Registrations_bySector_", input$dateAgg[1], "--", input$dateAgg[2], "_", input$pickCountry, ".csv")
  #   },
  #   content = function(file) {
  #     write.csv(donutData(Tdivision(), input$dateAgg[1], input$dateAgg[2]), file, row.names = F)
  #   }
  # ) # download data of donut

  output$DivisionDownload <- downloadHandler(
    filename = function() {
      paste0("Registrations_byDivisionSector_", input$dateAgg[1], "--", input$dateAgg[2], "_", input$pickCountry, ".csv")
    },
    content = function(file) {
      write.csv(Tdivision(), file, row.names = F)
    }
  )



  ### Industry divisions plots ----
  # Count daily registrations per SIC Group in selected date range, regions and SIC Group.
  Tgrp <- reactive({
    # groupSelect(Tcountry(), input$groupPicker)
    Tcountry()[(Group %in% input$groupPicker | Section %in% input$groupPicker),list(n=sum(n)),
                     keyby=list(date, Group, Group.name, Section, Section.name)] %>% 
      group_by(Group) %>% mutate(sum = sum_run(x=n, k=28, idx=date), avg = sum/28) %>%
      ungroup()
    })
  # Selected Groups in a table.
  secTable <- reactive({
    tableData(Tgrp(),input$groupPicker)
  })
  output$groups <- renderDT({
    showTable(secTable())
  }) # Groups table

  # Industry groups plot
  output$groupsPlot <- renderPlotly({
    groupPlot(Tgrp(), input$groupPicker, input$pickCountry, input$dateAgg[1])
  }) # Groups plot
  # Download data
  output$groupsDownload <- downloadHandler(
    filename = function() {
      paste0("Registrations_by_group_", input$dateAgg[1], "--", input$dateAgg[2], "_", input$pickCountry, ".csv")
    },
    content = function(file) {
      write.csv(Tgrp(), file, row.names = F)
    }
  )

  # Daily registrations per Section for selected regions, dates and distinct Sections from selected Groups.
  Tsec <- reactive({
    sectSelect(Tcountry(), secTable()$Section)
  })

  # Download data
  output$sectorsDownload <- downloadHandler(
    filename = function() {
      paste0("Registrations_by_sector_", input$dateAgg[1], "--", input$dateAgg[2], "_", input$pickCountry, ".csv")
    },
    content = function(file) {
      write.csv(Tsec(), file, row.names = F)
    }
  )
  # Industry plots

  ### Regional plots ----
  Tlad <- reactive({
    ladSelect(Tcountry(), input$ladPicker, input$dateAgg[1], input$dateAgg[2])})
  
  # Selected local authority districts in a table.
  output$districts <- renderDT({
    ladTable(tableData2(Tlad(),input$ladPicker))
  }) # Districts table
 
  # LA districts plot
  output$districtsPlot <- renderPlotly({
    districtPlot(Tlad(), input$ladPicker, input$dateAgg[1])
  }) # Districts plot
  
  # Download data
  output$districtsDownload <- downloadHandler(
    filename = function() {
      paste0("Registrations_by_district_", input$dateAgg[1], "--", input$dateAgg[2], ".csv")
    },
    content = function(file) {
      write.csv(Tlad(), file, row.names = F)
    }
  )

  Tcustom <- reactive(register %>%
    group_by(date, Class, postcodeDistrict) %>%
    count() %>%
    as.data.table())

  output$customdata <- renderDT({
    totalRegistrations(input$dateAgg[1], input$dateAgg[2], Tcustom(), input$pickPostcode, input$pickSIC)
  })

  # Download data
  output$customDownload <- downloadHandler(
    filename = function() {
      paste0("Custom_Registrations_", input$dateAgg[1], "--", input$dateAgg[2], ".csv")
    },
    content = function(fname) {
      write.csv(customDataDownload(input$dateAgg[1], input$dateAgg[2], Tcustom(), input$pickPostcode, input$pickSIC),
        fname,
        row.names = F
      )
    }
  )

  # Dissolutions ----
  Dcountry <- reactive({
    setDT(countrySelect(input$pickCountry, dissolutions, input$dateAgg[1], input$dateAgg[2]))
  })

  ## Aggregate Analysis Dissolutions ----
  # Modify text in headers depending on country/region.
  output$countryTextDis <- renderUI(h2(paste0(
    "Statistics of company dissolutions in ",
    input$pickCountry
  )))



  ### Value boxes
  # Sum registrations according to selected NUTS1 regions and relevant date ranges.
  output$vboxTotalDis <- renderValueBox(
    vboxesDis(1, input$dateAgg[1], input$dateAgg[2], input$pickCountry, Dcountry())
  )

  ### Daily dissolutions plot
  output$rollingAvgDis <- renderPlotly({
    dailyPlot(input$dateAgg[1], input$dateAgg[2], Dcountry(), input$pickCountry) %>%
      layout(
        title = paste0(
          "Daily company dissolutions in ", input$pickCountry),
        yaxis = list(title = "Number of dissolutions"))
  }) # Daily plot

  # Download data
  output$dailyRegDisDownload <- downloadHandler(
    filename = function() {
      paste0("Daily_Dissolutions_", input$dateAgg[1], "--", input$dateAgg[2], "_", input$pickCountry, ".csv")
    },
    content = function(fname) {
      write.csv(dailyData(
        input$dateAgg[1], input$dateAgg[2], countrySel(),
        TcountryDis, input$pickCountry
      ), fname, row.names = F)
    }
  )

  ### UK NUTS2 map====
  output$UKmapDis <- renderPlotly({
    showMap(register, input$dateAgg[1], input$dateAgg[2], "dissolutions") %>%
      layout(title=paste0("Dissolutions between<br>", input$dateAgg[1], " and ", input$dateAgg[2]))
  }) # UK NUTS2 map

  # Download data
  output$NUTS2DisDownload <- downloadHandler(
    filename = function() {
      paste0("Total_Dissolutions_byRegion_", input$dateAgg[1], "--", input$dateAgg[2], ".csv")
    },
    content = function(fname) {
      write.csv(NUTSdataDis(input$dateAgg[1], input$dateAgg[2], input$pickData), fname, row.names = F)
    }
  )

  # Draw sector treemap
  Ddivision <- reactive(
    # Aggregate registrations by SIC Division in selected date range and regions from full dataset.
    Dcountry()[date>=input$dateAgg[1],list(n=sum(n)),by=list(SectionAbb, Division.name)]
  )

  sunburstDFDis <- reactive({
    as.sunburstDF(
      Ddivision(),
      valueCol = "n"
    )
  })
  
  output$treemapDis <- renderPlotly({
    drawTreemap(sunburstDFDis(), input$dateAgg[1], input$dateAgg[2], input$pickCountry) %>%
      layout(
        title = paste0(
          "Distribution of dissolutions by SIC codes",
          "<br>",
          "<sup>",
          "between ", input$dateAgg[1], " and ", input$dateAgg[2],
          " in ",
          input$pickCountry,
          "</sup>"
        ))
  }) # Treemap


  # Download data from donut
  output$SectorDownDis <- downloadHandler(
    filename = function() {
      paste0("Dissolutions_bySector_", input$dateAgg[1], "--", input$dateAgg[2], "_", input$pickCountry, ".csv")
    },
    content = function(file) {
      write.csv(donutData(TdivisionDis(), input$dateAgg[1], input$dateAgg[2]), file, row.names = F)
    }
  ) # download data of donut

  output$DivisionDisDownload <- downloadHandler(
    filename = function() {
      paste0("Dissolutions_byDivisionSector_", input$dateAgg[1], "--", input$dateAgg[2], "_", input$pickCountry, ".csv")
    },
    content = function(file) {
      write.csv(TdivisionDis(), file, row.names = F)
    }
  )

  ### Industry divisions plots ----
  Dgrp <- reactive({
    Dcountry()[(Group %in% input$groupPickerDis | Section %in% input$groupPickerDis),list(n=sum(n)),
               keyby=list(date, Group, Group.name, Section, Section.name)] %>%
      group_by(Group) %>% mutate(sum = sum_run(x=n, k=28, idx=date), avg = sum/28) %>%
      ungroup()
  })
  # Selected Groups in a table.
  secTableDis <- reactive({
    tableData(Dgrp(),input$groupPickerDis)
  })

  output$groupsDis <- renderDT({
    showTable(secTableDis())
  }) # Groups table

  # Industry groups plot
  output$groupsPlotDis <- renderPlotly({
    groupPlot(Dgrp(), input$groupPickerDis, input$pickCountry, input$dateAgg[1]) %>%
      layout(title=paste0(
        "Daily company dissolutions by Group/Sector in ", input$pickCountry))
  }) # Groups plot
  # Download data
  output$groupsDownloadDis <- downloadHandler(
    filename = function() {
      paste0("Dissolutions_by_group_", input$dateAgg[1], "--", input$dateAgg[2], "_", input$pickCountry, ".csv")
    },
    content = function(file) {
      write.csv(TgrpDis(), file, row.names = F)
    }
  )

  # Download data
  output$sectorsDownloadDis <- downloadHandler(
    filename = function() {
      paste0("Dissolutions_by_sector_", input$dateAgg[1], "--", input$dateAgg[2], "_", input$pickCountry, ".csv")
    },
    content = function(file) {
      write.csv(TsecDis(), file, row.names = F)
    }
  )
  # Industry plots

  ### Regional comparison plots by division ----
  ### Regional plots ----
  Dlad <- reactive({
    ladSelect(Dcountry(), input$ladPickerDis, input$dateAgg[1], input$dateAgg[2])})
  
  # Selected local authority districts in a table.
  output$districtsDis <- renderDT({
    ladTable(tableData2(Dlad(),input$ladPickerDis))
  }) # Districts table
  
  # LA districts plot
  output$districtsPlotDis <- renderPlotly({
    districtPlot(Dlad(), input$ladPickerDis, input$dateAgg[1]) %>%
      layout(title=paste0(
        "Daily company dissolutions by Local Authority District/County in ", input$pickCountry))
  }) # Districts plot
  
  # Download data
  output$districtsDownloadDis <- downloadHandler(
    filename = function() {
      paste0("Dissolutions_by_district_", input$dateAgg[1], "--", input$dateAgg[2], ".csv")
    },
    content = function(file) {
      write.csv(Dlad(), file, row.names = F)
    }
  )
  
} # Server
