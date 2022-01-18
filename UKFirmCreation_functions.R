# Select NUTS1 regions according to country/region selected by user.
countrySelect <- function(pickCountry) {
  if (pickCountry == "UK") {
    list(
      "UKC", "UKD", "UKE", "UKF", "UKG", "UKH", "UKI", "UKJ", "UKK", "UKL",
      "UKM", "UKN"
    )
  } else if (pickCountry == "Eng") {
    list("UKC", "UKD", "UKE", "UKF", "UKG", "UKH", "UKI", "UKJ", "UKK")
  } else if (pickCountry == "Lon") {
    list("UKI")
  } else if (pickCountry == "Eng2") {
    list("UKC", "UKD", "UKE", "UKF", "UKG", "UKH", "UKJ", "UKK")
  } else if (pickCountry == "Sco") {
    list("UKM")
  } else if (pickCountry == "Wal") {
    list("UKL")
  } else if (pickCountry == "NI") {
    list("UKN")
  }
}

# Value boxes
vboxes <- function(vb, d1, d2, country, Tcountry) {
  if (vb == 1) {
    subtitle <- paste0("New registrations between ", d1, " and ", d2)
    color <- "aqua"
    icon <- icon("building")
  } else if (vb == 2) {
    subtitle <- paste("New registrations between first vaccine and", d2)
    color <- "light-blue"
    icon <- icon("syringe")
  } else if (vb == 3) {
    subtitle <- paste("New registrations during the most recent lockdown")
    color <- "yellow"
    icon <- icon("store-alt-slash")
  } else if (vb == 4) {
    subtitle <- paste("New registrations since easing the most recent lockdown and", d2)
    color <- "green"
    icon <- icon("store-alt")
  }
  valueBox(
    value = prettyNum(sum(Tcountry$n[which(Tcountry$NUTS1 %in% country &
      between(Tcountry$date, d1, d2))]),
    big.mark = ",", scientific = FALSE
    ),
    subtitle = subtitle, color = color, icon = icon
  )
}

# Daily registrations plot
dailyPlot <- function(d1, d2, country, Tcountry, pickCountry) {
  # Sum data for each day in relevant regions and date range.
  raData <- Tcountry$n[which(Tcountry$NUTS1 %in% country & between(Tcountry$date, d1, d2))] %>%
    aggregate(
      by = list(date = Tcountry$date[which(Tcountry$NUTS1 %in% country & between(Tcountry$date, d1, d2))]),
      sum
    ) %>%
    rename(n = x)
  # Data for relevant regions during LD3.
  ld3Data <- Tcountry$n[which(Tcountry$NUTS1 %in% country & between(Tcountry$date, as.Date("2021-01-05"), as.Date("2021-04-12")))] %>%
    aggregate(
      by = list(date = Tcountry$date[which(Tcountry$NUTS1 %in% country & between(Tcountry$date, as.Date("2021-01-05"), as.Date("2021-04-12")))]),
      sum
    )
  medianLD3 <- median(ld3Data$x)
  # Data for relevant regions during 2019.
  # data2019 <- Tcountry$n[which(Tcountry$NUTS1 %in% country & between(Tcountry$date, as.Date("2019-01-01"), as.Date("2019-12-31")))] %>%
  #   aggregate(by = list(date = Tcountry$date[which(Tcountry$NUTS1 %in% country & between(Tcountry$date, as.Date("2019-01-01"), as.Date("2019-12-31")))]),
  #             sum)
  # median2019 <- median(data2019$x)
  # Saved values from archive data for greater accuracy.
  archive <- daily2019$n[which(daily2019$NUTS1 %in% country)] %>%
    aggregate(
      by = list(date = daily2019$date[which(daily2019$NUTS1 %in% country)]),
      sum
    ) %>%
    rename(n = x)
  archiveMedians <- tibble(
    Region = c("UK", "Eng", "Lon", "Eng2", "Sco", "Wal", "NI"),
    Median = c(2414.5, 2192.5, 777, 1421, 124, 68, 32)
  )
  median2019 <- archiveMedians$Median[[which(archiveMedians$Region == pickCountry)]]
  # Plot rolling average/daily registrations, with or without lockdown periods.
  plot_ly() %>%
    add_trace(
      x = raData$date, y = (frollmean(raData$n, n = 7)), name = "7-day rolling average",
      type = "scatter", mode = "lines", showlegend = FALSE, visible = TRUE
    ) %>%
    add_trace(
      x = (archive$date + 365), y = (frollmean(archive$n, n = 7)), name = "2019; 7-day RA",
      type = "scatter", mode = "lines", showlegend = TRUE, visible = TRUE,
      line = list(dash = "dash", width = 0.5)
    ) %>%
    add_trace(
      x = raData$date, y = raData$n, name = "daily total",
      type = "bar", showlegend = FALSE, visible = FALSE
    ) %>%
    add_trace(
      x = (archive$date + 365), y = archive$n, name = "2019; daily",
      type = "scatter", mode = "lines", showlegend = TRUE, visible = FALSE,
      line = list(dash = "dash", width = 0.5)
    ) %>%
    add_segments(
      x = d1, xend = d2,
      y = median(raData$n), yend = median(raData$n), name = "Median; selected period", showlegend = T
    ) %>%
    add_segments(
      x = min(raData$date), xend = max(raData$date),
      y = medianLD3, yend = medianLD3, name = "Median; Lockdown 3", showlegend = T
    ) %>%
    add_segments(
      x = min(raData$date), xend = max(raData$date),
      y = median2019, yend = median2019, name = "Median; 2019", showlegend = T
    ) %>%
    layout(
      title = paste0(
        "Daily company registrations in ", pickCountry,
        "<br>",
        "<sup>",
        "between ", d1, " and ", d2,
        "</sup>"
      ),
      yaxis = list(title = "Number of registrations", showgrid = F, range = c(0, 1.1 * max(raData$n))),
      xaxis = list(range = c(d1, d2)),
      updatemenus = list(
        list(type = "dropdown", y = 0.95, x = 1.25, direction = "down", buttons = list(
          list(
            label = "Rolling average", method = "update",
            args = list(list(visible = c(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE)))
          ),
          list(
            label = "Daily total", method = "update",
            args = list(list(visible = c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE)))
          )
        )),
        list(type = "dropdown", y = 0.75, x = 1.25, active = 1, direction = "down", buttons = list(
          list(label = "Show lockdowns", method = "relayout", args = list(list(
            shapes = list(
              list(
                type = "rect",
                fillcolor = "gray", line = list(color = "gray"), opacity = 0.2,
                x0 = "2020-03-23", x1 = "2020-07-04", xref = "x",
                y0 = 0, y1 = 4000, yref = "y"
              ),
              list(
                type = "rect",
                fillcolor = "gray", line = list(color = "gray"), opacity = 0.2,
                x0 = "2020-11-05", x1 = "2020-12-02", xref = "x",
                y0 = 0, y1 = 4000, yref = "y"
              ),
              list(
                type = "rect",
                fillcolor = "gray", line = list(color = "gray"), opacity = 0.2,
                x0 = "2021-01-05", x1 = "2021-04-12", xref = "x",
                y0 = 0, y1 = 4000, yref = "y"
              )
            )
          ))),
          list(label = "Hide lockdowns", method = "relayout", args = list(list(shapes = c())))
        ))
      ),
      legend = list(x = 1, y = 0.5)
    )
}

# Daily registrations data from plot to export as downloadable .csv
dailyData <-  function(d1, d2, country, Tcountry, pickCountry) {
  # Sum data for each day in relevant regions and date range.
  raData <- Tcountry$n[which(Tcountry$NUTS1 %in% country & between(Tcountry$date, d1, d2))] %>%
    aggregate(
      by = list(date = Tcountry$date[which(Tcountry$NUTS1 %in% country & between(Tcountry$date, d1, d2))]),
      sum
    ) %>%
    rename(n = x)
  raData$rollingAverage <- frollmean(raData$n, n = 7)
  # Saved values from archive data for greater accuracy.
  archive <- daily2019$n[which(daily2019$NUTS1 %in% country)] %>%
    aggregate(
      by = list(date = daily2019$date[which(daily2019$NUTS1 %in% country)]),
      sum
    ) %>%
    rename(n = x)
  archive$rollingAverage <- frollmean(archive$n, n = 7)
  # append raData and archive
  data <- rbind(archive, raData)
}
  
  
# UK NUTS2 map
showMap <- function(d1, d2) {
  # Load shapefile.
  # https://geoportal.statistics.gov.uk/datasets/nuts-level-2-january-2018-ultra-generalised-clipped-boundaries-in-the-united-kingdom/
  mapNUTS <- st_read("data/map")
  # Aggregate registrations by NUTS2 in selected date range from full dataset.
  TNUTS <- registerPC[which(between(registerPC$date, d1, d2)), ] %>%
    group_by(NUTS2) %>%
    count()
  # Draw map.
  mapNUTS <- merge(mapNUTS, TNUTS, by.x = "nuts218cd", by.y = "NUTS2")
  mapcolour <- c("#81A1C1", "#88C0D0", "#83AB61", "#EBCB8B", "#D08770", "#BF616A", "#D63A2F")
  plot_ly(mapNUTS) %>%
    add_sf(
      color = ~ n^0.6,
      colors = mapcolour,
      split = ~nuts218nm,
      text = ~ paste0(nuts218nm, "<br>", n, " new registrations"),
      line = list(width = 0.9, color = "#4C566A"),
      showlegend = FALSE,
      hoverinfo = "text", hoveron = "fills",
      type = "scatter"
    ) %>%
    layout() %>%
    colorbar(title = paste0("New registrations between<br>", d1, " and ", d2), 
             len = 0.5, x = 0.7, y = 0.65, 
             showticklabels = FALSE)
}

# Sector tree map
# Use the function as.sunburstDF from https://stackoverflow.com/a/58481176/4874341
as.sunburstDF <- function(DF, valueCol = NULL) {
  require(data.table)
  colNamesDF <- names(DF)
  if (is.data.table(DF)) {
    DT <- copy(DF)
  } else {
    DT <- data.table(DF, stringsAsFactors = FALSE)
  }
  DT[, root := "Total"]
  colNamesDT <- names(DT)
  if (is.null(valueCol)) {
    setcolorder(DT, c("root", colNamesDF))
  } else {
    setnames(DT, valueCol, "values", skip_absent = TRUE)
    setcolorder(DT, c("root", setdiff(colNamesDF, valueCol), "values"))
  }
  hierarchyCols <- setdiff(colNamesDT, "values")
  hierarchyList <- list()
  for (i in seq_along(hierarchyCols)) {
    currentCols <- colNamesDT[1:i]
    if (is.null(valueCol)) {
      currentDT <- unique(DT[, ..currentCols][, values := .N, by = currentCols], by = currentCols)
    } else {
      currentDT <- DT[, lapply(.SD, sum, na.rm = TRUE), by = currentCols, .SDcols = "values"]
    }
    setnames(currentDT, length(currentCols), "labels")
    hierarchyList[[i]] <- currentDT
  }
  hierarchyDT <- rbindlist(hierarchyList, use.names = TRUE, fill = TRUE)
  parentCols <- setdiff(names(hierarchyDT), c("labels", "values", valueCol))
  hierarchyDT[, parents := apply(.SD, 1, function(x) {
    fifelse(all(is.na(x)), yes = NA_character_, no = paste(x[!is.na(x)], sep = ":", collapse = " - "))
  }), .SDcols = parentCols]
  hierarchyDT[, ids := apply(.SD, 1, function(x) {
    paste(x[!is.na(x)], collapse = " - ")
  }), .SDcols = c("parents", "labels")]
  hierarchyDT[, c(parentCols) := NULL]
  return(hierarchyDT)
}
# Draw the treemap.
drawTreemap <- function(df, d1, d2) {
  plot_ly(
    alpha = 0.5, data = df, ids = ~ids, labels = ~labels,
    parents = ~parents, values = ~values, type = "treemap",
    branchvalues = "total",
    textinfo = "label+value+percent parent+parent+percent root+root",
    hoverinfo = "label+value+percent parent+parent",
    pathbar = "visible",
    insidetextfont = list(size = 10)
  ) %>% layout(
    title = paste0(
      "Distribution of new registrations by SIC codes",
      "<br>",
      "<sup>",
      "between ", d1, " and ", d2,
      "</sup>"
    ),
    uniformtext = list(minsize = 10)
  )
}

# Donut plot
drawDonut <- function(df, d1, d2) {
  Tsection <- df$n %>%
    aggregate(by = list(df$SectionAbb), sum) %>%
    rename(SectionAbb = Group.1, n = x)
  plot_ly(data = Tsection, labels = ~SectionAbb, values = ~n) %>%
    add_pie(hole = 0.6) %>%
    layout(title = paste0("New registrations by sector between ", d1, " and ", d2))
}

# donut data
donutData <- function(d1, d2, country, Tdivision, pickCountry) {
  data <- registerPC[which(between(registerPC$date, input$dateAgg[1], input$dateAgg[2]) &
                             registerPC$NUTS1 %in% countrySel()), ] %>%
    group_by(SectionAbb, Division.name) %>%
    count() %>% as.data.table()
}

# Groups table
showTable <- function(tabledata) {
  container_dt <- withTags(table(
    class = "display",
    thead(
      tr(
        th(class = "dt-center", colspan = 2, "3-digit classification"),
        th(class = "dt-center", colspan = 2, "1-digit classification")
      ),
      tr(
        lapply((c("Group", "Group Name", "Section", "Section Name")), th)
      )
    )
  ))
  datatable(tabledata,
    container = container_dt, rownames = F, class = "",
    options = list(
      autoWidth = T, lengthChange = FALSE, searching = FALSE,
      columnDefs = list(
        list(
          className = "dt-center",
          targets = "_all"
        ),
        list(
          width = "40px",
          target = "_all"
        )
      )
    )
  )
}

# Industry groups plot
groupPlot <- function(df) {
  plot_ly(data = df) %>%
    add_trace(
      x = ~date, y = ~avg, color = ~Group.name, colors = "viridis",
      type = "scatter", mode = "lines", visible = TRUE,
      line = list(width = 0.8)
    ) %>%
    add_trace(
      x = ~date, y = ~n, color = ~Group.name, colors = "viridis",
      type = "scatter", mode = "lines", visible = FALSE,
      line = list(width = 0.8)
    ) %>%
    layout(
      title = "Daily registrations by industry group",
      yaxis = list(title = "Number of registrations"),
      xaxis = list(showgrid = FALSE, title = ""),
      updatemenus = list(
        list(type = "dropdown", y = 1, x = 1.4, direction = "down", buttons = list(
          list(
            label = "Rolling average", method = "update",
            args = list(list(visible = c(rep(TRUE, length(unique(df$Group))), rep(FALSE, length(unique(df$Group))))))
          ),
          list(
            label = "Daily total", method = "update",
            args = list(list(visible = c(rep(FALSE, length(unique(df$Group))), rep(TRUE, length(unique(df$Group))))))
          )
        ))
      ),
      legend = list(x = 1, y = 0.8)
    )
}

# Industry sectors plot
sectorPlot <- function(d1, d2, section, country) {
  # Daily registrations per Section for selected regions, dates and distinct Sections from selected Groups.
  df <- registerPC[which(between(registerPC$date, d1, d2) &
    registerPC$Section %in% section &
    registerPC$NUTS1 %in% country), ] %>%
    group_by(date, Section, Section.name) %>%
    count() %>%
    ungroup() %>%
    group_by(Section) %>%
    mutate(avg = frollmean(n, n = 7))

  plot_ly(data = df) %>%
    add_trace(
      x = ~date, y = ~avg, color = ~Section.name, colors = "viridis",
      type = "scatter", mode = "lines", visible = TRUE,
      line = list(width = 0.8)
    ) %>%
    add_trace(
      x = ~date, y = ~n, color = ~Section.name, colors = "viridis",
      type = "scatter", mode = "lines", visible = FALSE,
      line = list(width = 0.8)
    ) %>%
    layout(
      title = "Daily registrations by industry sector",
      yaxis = list(title = "Number of registrations"),
      xaxis = list(showgrid = FALSE, title = ""),
      updatemenus = list(
        list(type = "dropdown", y = 1, x = 1.4, direction = "down", buttons = list(
          list(
            label = "Rolling average", method = "update",
            args = list(list(visible = c(rep(TRUE, length(unique(df$Section))), rep(FALSE, length(unique(df$Section))))))
          ),
          list(
            label = "Daily total", method = "update",
            args = list(list(visible = c(rep(FALSE, length(unique(df$Section))), rep(TRUE, length(unique(df$Section))))))
          )
        ))
      ),
      legend = list(x = 1, y = 0.8)
    )
}

groupRegion <- function(d1, d2, group, a) {
  # Daily totals for the selected dates and group for each NUTS1 region.
  if (a == 1) {
    groupName <- registerPC$Group.name[which(registerPC$Group == group)][1]
    df <- registerPC[which(between(registerPC$date, d1, d2) &
      registerPC$Group == group), ] %>%
      group_by(date, NUTS1) %>%
      count() %>%
      ungroup() %>%
      group_by(NUTS1) %>%
      mutate(avg = frollmean(n, n = 7))
    title <- paste0("Daily registrations by region in ", groupName)
  } else if (a == 2) {
    section <- registerPC$SectionAbb[which(registerPC$Group == group)][1]
    df <- registerPC[which(between(registerPC$date, d1, d2) &
      registerPC$SectionAbb == section), ] %>%
      group_by(date, NUTS1) %>%
      count() %>%
      ungroup() %>%
      group_by(NUTS1) %>%
      mutate(avg = frollmean(n, n = 7))
    title <- paste0("Daily registrations by region in ", section)
  }
  # Totals for England with/without London.
  eng1 <- df$n[which(df$NUTS1 %in% list("UKC", "UKD", "UKE", "UKF", "UKG", "UKH", "UKI", "UKJ", "UKK"))] %>%
    aggregate(by = list(date = df$date[which(df$NUTS1 %in% list("UKC", "UKD", "UKE", "UKF", "UKG", "UKH", "UKI", "UKJ", "UKK"))]), sum) %>%
    rename(n = x) %>%
    mutate(avg = frollmean(n, n = 7))
  eng2 <- df$n[which(df$NUTS1 %in% list("UKC", "UKD", "UKE", "UKF", "UKG", "UKH", "UKJ", "UKK"))] %>%
    aggregate(by = list(date = df$date[which(df$NUTS1 %in% list("UKC", "UKD", "UKE", "UKF", "UKG", "UKH", "UKJ", "UKK"))]), sum) %>%
    rename(n = x) %>%
    mutate(avg = frollmean(n, n = 7))
  plot_ly() %>%
    add_trace(
      x = eng1$date, y = eng1$avg, name = "England (inc. Lon)",
      type = "scatter", mode = "lines", visible = TRUE,
      line = list(width = 0.8, color = "#440154")
    ) %>%
    add_trace(
      x = eng2$date, y = eng2$avg, name = "England (exc. Lon)",
      type = "scatter", mode = "lines", visible = TRUE,
      line = list(width = 0.8, color = "#414487")
    ) %>%
    add_trace(
      data = df[which(df$NUTS1 == "UKI"), ], x = ~date, y = ~avg, name = "London",
      type = "scatter", mode = "lines", visible = TRUE,
      line = list(width = 0.8, color = "#2a788e")
    ) %>%
    add_trace(
      data = df[which(df$NUTS1 == "UKM"), ], x = ~date, y = ~avg, name = "Scotland",
      type = "scatter", mode = "lines", visible = TRUE,
      line = list(width = 0.8, color = "#22a884")
    ) %>%
    add_trace(
      data = df[which(df$NUTS1 == "UKL"), ], x = ~date, y = ~avg, name = "Wales",
      type = "scatter", mode = "lines", visible = TRUE,
      line = list(width = 0.8, color = "#7ad151")
    ) %>%
    add_trace(
      data = df[which(df$NUTS1 == "UKN"), ], x = ~date, y = ~avg, name = "Northern Ireland",
      type = "scatter", mode = "lines", visible = TRUE,
      line = list(width = 0.8, color = "#fde725")
    ) %>%
    add_trace(
      x = eng1$date, y = eng1$n, name = "England (inc. Lon)",
      type = "scatter", mode = "lines", visible = FALSE,
      line = list(width = 0.8, color = "#440154")
    ) %>%
    add_trace(
      x = eng2$date, y = eng2$n, name = "England (exc. Lon)",
      type = "scatter", mode = "lines", visible = FALSE,
      line = list(width = 0.8, color = "#414487")
    ) %>%
    add_trace(
      data = df[which(df$NUTS1 == "UKI"), ], x = ~date, y = ~n, name = "London",
      type = "scatter", mode = "lines", visible = FALSE,
      line = list(width = 0.8, color = "#2a788e")
    ) %>%
    add_trace(
      data = df[which(df$NUTS1 == "UKM"), ], x = ~date, y = ~n, name = "Scotland",
      type = "scatter", mode = "lines", visible = FALSE,
      line = list(width = 0.8, color = "#22a884")
    ) %>%
    add_trace(
      data = df[which(df$NUTS1 == "UKL"), ], x = ~date, y = ~n, name = "Wales",
      type = "scatter", mode = "lines", visible = FALSE,
      line = list(width = 0.8, color = "#7ad151")
    ) %>%
    add_trace(
      data = df[which(df$NUTS1 == "UKN"), ], x = ~date, y = ~n, name = "Northern Ireland",
      type = "scatter", mode = "lines", visible = FALSE,
      line = list(width = 0.8, color = "#fde725")
    ) %>%
    layout(
      title = title,
      yaxis = list(title = "Number of registrations"),
      xaxis = list(showgrid = FALSE, title = ""),
      updatemenus = list(
        list(type = "dropdown", y = 1, x = 1.4, direction = "down", buttons = list(
          list(
            label = "Rolling average", method = "update",
            args = list(list(visible = c(rep(TRUE, 6), rep(FALSE, 6))))
          ),
          list(
            label = "Daily total", method = "update",
            args = list(list(visible = c(rep(FALSE, 6), rep(TRUE, 6))))
          )
        ))
      ),
      legend = list(x = 1, y = 0.8)
    )
}

# Download data
downloadData <- function(name, output) {
  downloadHandler(
    filename = function() {
      name
    },
    content = function(file) {
      write.csv(output, file, row.names = FALSE)
    }
  )
}
