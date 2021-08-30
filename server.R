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
  # # For UK in total
  # TUK <- Tcountry %>% group_by(date) %>% summarise(n = sum(n)) %>% as.data.table()
  
  ## AGGREGATE STATS ====
  # Select NUTS1 regions according to country/region selected by user.
  countrySel <- reactive({
    if (input$pickCountry == 'UK') {
    list("UKC", "UKD", "UKE", "UKF", "UKG", "UKH", "UKI", "UKJ", "UKK", "UKL", 
         "UKM", "UKN")
  } else if (input$pickCountry == 'Eng') {
    list("UKC", "UKD", "UKE", "UKF", "UKG", "UKH", "UKI", "UKJ", "UKK")
  } else if (input$pickCountry == 'Lon') {
    list("UKI")
  } else if (input$pickCountry == 'Eng2') {
    list("UKC", "UKD", "UKE", "UKF", "UKG", "UKH", "UKJ", "UKK")
  } else if (input$pickCountry == 'Sco') {
    list("UKM")
  } else if (input$pickCountry == 'Wal') {
    list("UKL")
  } else if (input$pickCountry == 'NI') {
    list("UKN")
  }
  })

  ### Value boxes
  # Sum registrations according to selected NUTS1 regions and relevant date ranges.
  output$vboxTotal <- renderValueBox(
    valueBox(
      # Total registration in selected period
      value = prettyNum(sum(Tcountry$n[which(Tcountry$NUTS1 %in% countrySel() & 
                                               between(Tcountry$date, input$dateAgg[1], input$dateAgg[2]))]), 
                        big.mark=",", scientific=FALSE),
      subtitle = paste0("New registrations between ", input$dateAgg[1], " and ", input$dateAgg[2]),
      color = "aqua",
      icon = icon("building")
    )
  )
  output$vboxVaccine <- renderValueBox(
    valueBox(
      # Registrations since first vaccine
      value = prettyNum(sum(Tcountry$n[which(Tcountry$NUTS1 %in% countrySel() & 
                                               between(Tcountry$date, as.Date("2020-12-08"), input$dateAgg[2]))]), 
                        big.mark=",", scientific=FALSE),
      subtitle = paste("New registrations between first vaccine and", input$dateAgg[2]),
      color = "light-blue",
      icon = icon("syringe")
    )
  )
  output$vboxLD <- renderValueBox(
    valueBox(
      # Registrations during the most recent Lockdown
      value = prettyNum(sum(Tcountry$n[which(Tcountry$NUTS1 %in% countrySel() & 
                                               between(Tcountry$date, as.Date("2021-01-05"), as.Date("2021-04-11")))]), 
                        big.mark=",", scientific=FALSE),
      subtitle = paste("New registrations during the most recent lockdown"),
      color = "yellow",
      icon = icon("store-alt-slash")
    )
  )
  output$vboxOpen <- renderValueBox(
    valueBox(
      # Registrations since non-essentials shops most recently re-opened
      value = prettyNum(sum(Tcountry$n[which(Tcountry$NUTS1 %in% countrySel() & 
                                               between(Tcountry$date, as.Date("2021-04-12"), input$dateAgg[2]))]), 
                        big.mark=",", scientific=FALSE),
      subtitle = paste("New registrations since easing the most recent lockdown and", input$dateAgg[2]),
      color = "green",
      icon = icon("store-alt")
    )
  ) #VBoxes
  
  ### Daily registrations plot
  output$rollingAvg <- renderPlotly({
    # Sum data for each day in relevant regions and date range.
    raData <- Tcountry$n[which(Tcountry$NUTS1 %in% countrySel() & between(Tcountry$date, input$dateAgg[1], input$dateAgg[2]))] %>%
      aggregate(by = list(date = Tcountry$date[which(Tcountry$NUTS1 %in% countrySel() & between(Tcountry$date, input$dateAgg[1], input$dateAgg[2]))]),
                sum) %>% rename(n = x)
    # Data for relevant regions during LD3.
    ld3Data <- Tcountry$n[which(Tcountry$NUTS1 %in% countrySel() & between(Tcountry$date, as.Date("2021-01-05"), as.Date("2021-04-12")))] %>%
      aggregate(by = list(date = Tcountry$date[which(Tcountry$NUTS1 %in% countrySel() & between(Tcountry$date, as.Date("2021-01-05"), as.Date("2021-04-12")))]),
                sum)
    medianLD3 <- median(ld3Data$x)
    # Plot rolling average/daily registrations, with or without lockdown periods.
    plot_ly() %>% add_trace(x=raData$date, y=(frollmean(raData$n, n=7)), name="7-day rolling average", 
            type='scatter', mode='lines', showlegend=FALSE, visible=TRUE) %>%
      add_trace(x=raData$date, y=raData$n, name="daily total", 
                type='bar', showlegend=FALSE, visible=FALSE) %>%
      add_segments(x = input$dateAgg[1], xend = input$dateAgg[2], 
                   y = median(raData$n), yend = median(raData$n), name = "Median; selected period", showlegend=T) %>%
      add_segments(x = min(raData$date), xend = max(raData$date), 
                   y = medianLD3, yend = medianLD3, name = "Median; Lockdown 3", showlegend=T) %>%
      layout(title = paste0('Daily company registrations',
                            '<br>',
                            '<sup>',
                            'between ', input$dateAgg[1], " and ", input$dateAgg[2],
                            '</sup>'),
             yaxis = list(title = "Number of registrations", showgrid = F, range=c(0,1.1*max(raData$n))),
            xaxis = list(range=c(input$dateAgg[1], input$dateAgg[2])),
            updatemenus = list(
            list(type = "dropdown", y = 0.95, x = 1.25, direction = "down", buttons=list(
              list(label = "Rolling average", method = "update",
                   args = list(list(visible = c(TRUE, FALSE, TRUE, TRUE)))),
              list(label = "Daily total", method = "update",
                   args = list(list(visible = c(FALSE, TRUE, TRUE, TRUE))))
            )),
            list(type = "dropdown", y = 0.75, x = 1.25, active=1, direction = "down", buttons=list(
              list(label = "Show lockdowns", method = "relayout", args = list(list(
                shapes = list(list(type = "rect",
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
                                   y0 = 0, y1 = 3100, yref = "y"))
              ))),
              list(label = "Hide lockdowns", method = "relayout", args = list(list(shapes = c())))
            ))
          ),
          legend = list(x = 1, y = 0.5)
      )
  }) #Daily plot
  
  ### UK NUTS2 map
  # Load shapefile.
  #https://geoportal.statistics.gov.uk/datasets/nuts-level-2-january-2018-ultra-generalised-clipped-boundaries-in-the-united-kingdom/
  mapNUTS <- st_read("data/map")
  output$UKmap <- renderPlotly({
    # Aggregate registrations by NUTS2 in selected date range from full dataset.
    TNUTS <- registerPC[which(between(registerPC$date, input$dateAgg[1], input$dateAgg[2])),] %>% group_by(NUTS2) %>% count()
    # Draw map.
    mapNUTS <- merge(mapNUTS, TNUTS, by.x="nuts218cd", by.y="NUTS2")
    plot_ly(mapNUTS) %>% 
      add_sf(color = ~n,
             colors = 'YlOrBr',
             split = ~nuts218nm, 
             text = ~paste0(nuts218nm, "<br>", n, " new registrations"),
             line = list(width = 0.5, color = "#CCC"),
             showlegend = FALSE, 
             hoverinfo = 'text', hoveron = 'fills', 
             type = 'scatter') %>% 
      layout() %>%
      colorbar(title = paste0('New registrations<br>between ', input$dateAgg[1], ' and ', input$dateAgg[2]), len = 0.5, bgcolor = "#CCC", x = 0.7, y = 0.65)
    
  })
  
  ### Sector tree map
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
    plot_ly(alpha = 0.5, data = sunburstDF(), ids = ~ids, labels= ~labels,
          parents = ~parents, values= ~values, type='treemap',
          branchvalues = 'total',
          textinfo="label+value+percent parent+parent+percent root+root",
          hoverinfo="label+value+percent parent+parent",
          pathbar = "visible",
          insidetextfont = list(size = 10)
    )  %>% layout(
    title = paste0('Distribution of new registrations by SIC codes',
                   '<br>',
                   '<sup>',
                   'For registrations between ', input$dateAgg[1], ' and ', input$dateAgg[2],
                   '</sup>'),
    uniformtext=list(minsize=10)
    )
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
    container_dt= withTags(table(
      class = 'display',
      thead(
        tr(
          th(class = 'dt-center',colspan = 2, '3-digit classification'),
          th(class = 'dt-center',colspan = 2, '1-digit classification')),
        tr(
          lapply((c('Group', 'Group Name', 'Section', 'Section Name')), th)))))
    datatable(secTable(), container = container_dt, rownames = F, class = "",
              options = list(autoWidth = T, lengthChange = FALSE, searching = FALSE,
                             columnDefs = list(list(className = "dt-center",
                                                    targets = "_all"),
                                               list(width = "40px",
                                                    target = "_all"))))
  }) #Groups table
  # Industry groups plot
  output$groupsPlot <- renderPlotly({
    plot_ly(data = Tgrp()) %>% 
      add_trace(x = ~date, y = ~avg, color = ~Group.name, colors = 'viridis',
                type = 'scatter', mode = 'lines', visible = TRUE,
                line = list(width = 0.8)) %>%
      add_trace(x = ~date, y = ~n, color = ~Group.name, colors = 'viridis',
                type = 'scatter', mode = 'lines', visible = FALSE,
                line = list(width = 0.8)) %>%
      layout(title = "Daily registrations by industry group",
             yaxis = list(title = "Number of registrations"),
             xaxis = list(showgrid = FALSE, title = ""),
             updatemenus = list(
               list(type = "dropdown", y = 1, x = 1.4, direction = "down", buttons=list(
                 list(label = "Rolling average", method = "update",
                      args = list(list(visible = c(rep(TRUE, length(unique(Tgrp()$Group))), rep(FALSE, length(unique(Tgrp()$Group))))))),
                 list(label = "Daily total", method = "update",
                      args = list(list(visible = c(rep(FALSE, length(unique(Tgrp()$Group))), rep(TRUE, length(unique(Tgrp()$Group)))))))
               ))),
             legend = list(x = 1, y = 0.8))
  }) #Groups plot
  # Industry sectors plot
  output$sectorsPlot <- renderPlotly({
    # Daily registrations per Section for selected regions, dates and distinct Sections from selected Groups.
    Tsec <- registerPC[which(between(registerPC$date, input$dateAgg[1], input$dateAgg[2]) & 
                               registerPC$Section %in% secTable()$Section & 
                               registerPC$NUTS1 %in% countrySel()),] %>% 
      group_by(date, Section, Section.name) %>% count() %>% ungroup() %>% 
      group_by(Section) %>% mutate(avg = frollmean(n, n=7))
    
    plot_ly(data = Tsec) %>% add_trace(x = ~date, y = ~avg, color = ~Section.name, colors = 'viridis',
                                       type = 'scatter', mode = 'lines', visible = TRUE,
                                       line = list(width = 0.8)) %>%
      add_trace(x = ~date, y = ~n, color = ~Section.name, colors = 'viridis',
                type = 'scatter', mode = 'lines', visible = FALSE,
                line = list(width = 0.8)) %>%
      layout(title = "Daily registrations by industry sector",
             yaxis = list(title = "Number of registrations"),
             xaxis = list(showgrid = FALSE, title = ""),
             updatemenus = list(
               list(type = "dropdown", y = 1, x = 1.4, direction = "down", buttons=list(
                 list(label = "Rolling average", method = "update",
                      args = list(list(visible = c(rep(TRUE, length(unique(Tsec$Section))), rep(FALSE, length(unique(Tsec$Section))))))),
                 list(label = "Daily total", method = "update",
                      args = list(list(visible = c(rep(FALSE, length(unique(Tsec$Section))), rep(TRUE, length(unique(Tsec$Section)))))))
               ))),
             legend = list(x = 1, y = 0.8))
    
  }) #Sectors plot
  #Industry plots
  
} #Server