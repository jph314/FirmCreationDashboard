# Aggregate home----
# Select NUTS1 regions according to country/region selected by user.
countrySelect <- function(pickCountry, register, d1, d2) {
  register <- register[which(between(register$date, d1, d2)),]
  if (pickCountry == "United Kingdom") {
    register
  } else if (pickCountry == "England") {
    register[which(register$Country=="England"),]
  } else if (pickCountry == "London") {
    register[which(register$County %in% list("Greater London", "City of London")),]
  } else if (pickCountry == "England excl. London") {
    register[which(register$Country=="England" & !(register$County %in% list("Greater London", "City of London"))),]
  } else if (pickCountry == "Scotland") {
    register[which(register$Country=="Scotland"),]
  } else if (pickCountry == "Wales") {
    register[which(register$Country=="Wales"),]
  } else if (pickCountry == "Northern Ireland") {
    register[which(register$Country=="Northern Ireland"),]
  }
}

# Value boxes
vboxes <- function(vb, d1, d2, Tcountry) {
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
    value = prettyNum(sum(Tcountry$n[which(between(Tcountry$date, d1, d2))]),
    big.mark = ",", scientific = FALSE
    ),
    subtitle = subtitle, color = color, icon = icon
  )
}

active <- function(data) {
  valueBox(
    value = prettyNum(sum(data$n), big.mark = ",", scientific = FALSE),
    subtitle = "Total active firms", color = "red", icon = icon("store-alt")
  )
}

# Daily registrations plot
dailyPlot <- function(d1, d2, Tcountry, pickCountry) {
  # Input data is already selected by date and country but must be aggregated by date.
  plotData <- Tcountry$n %>%
    aggregate(
      by = list(date = Tcountry$date),
      sum
    )
  # Data for relevant regions during LD3.
  # ld3Data <- Tcountry$n[which(Tcountry$NUTS1 %in% country & between(Tcountry$date, as.Date("2021-01-05"), as.Date("2021-04-12")))] %>%
  #   aggregate(
  #     by = list(date = Tcountry$date[which(Tcountry$NUTS1 %in% country & between(Tcountry$date, as.Date("2021-01-05"), as.Date("2021-04-12")))]),
  #     sum
  #   )
  # medianLD3 <- median(ld3Data$x)
  archive <- countrySelect(pickCountry, register1[which(register1$archive=="Archive"),], ymd("2019-01-01"), ymd("2019-12-31"))
  archive <- archive$n %>% aggregate(
      by = list(date = archive$date),
      sum
    )
  archiveMedians <- tibble(
    Region = c("United Kingdom", "England", "London", "England excl. London", "Scotland", "Wales", "Northern Ireland"),
    Median = c(2414.5, 2192.5, 777, 1421, 124, 68, 32)
  )
  median2019 <- archiveMedians$Median[[which(archiveMedians$Region == pickCountry)]]
  archiveExtend <- data.frame(date = seq(ymd("2019-01-01"),d2,"days"))
  archiveExtend$yday <- yday(archiveExtend$date)
  archive$yday <- yday(archive$date)
  archive <- merge(archiveExtend, archive, by="yday") %>% rename(date=date.x)
  archive <- archive[order(archive$date),]
  # Plot rolling average/daily registrations, with or without lockdown periods.
  plot_ly() %>%
    add_trace(
      x = plotData$date, y = (frollmean(plotData$x, n = 7)), name = "7-day rolling average",
      type = "scatter", mode = "lines", showlegend = TRUE, visible = TRUE
    ) %>%
    add_trace(
      x = archive$date, y = (frollmean(archive$x, n = 7)), name = "2019; 7-day RA",
      type = "scatter", mode = "lines", showlegend = TRUE, visible = TRUE,
      line = list(dash = "dash", width = 0.9)
    ) %>%
    add_trace(
      x = plotData$date, y = plotData$x, name = "Daily total",
      type = "bar", showlegend = TRUE, visible = TRUE, opacity = 0.3
    ) %>%
    add_trace(
      x = archive$date, y = archive$x, name = "2019; daily",
      type = "scatter", mode = "lines", showlegend = TRUE, visible = FALSE,
      line = list(dash = "dash", width = 0.5)
    ) %>%
    add_segments(
      x = d1, xend = d2,
      y = median(plotData$x), yend = median(plotData$x), name = "Median; selected period", showlegend = T
    ) %>%
    # add_segments(
    #   x = min(raData$date), xend = max(raData$date),
    #   y = medianLD3, yend = medianLD3, name = "Median; Lockdown 3", showlegend = T
    # ) %>%
    add_segments(
      x = min(plotData$date), xend = max(plotData$date),
      y = median2019, yend = median2019, name = "Median; 2019", showlegend = T
    ) %>%
    layout(
      title = paste0(
        "Daily company registrations in ",
        pickCountry # ,
        # "<br>",
        # "<sup>",
        # "between ", d1, " and ", d2,
        # "</sup>"
      ),
      yaxis = list(title = "Number of registrations", showgrid = F, range = c(0, 1.1 * max(plotData$x))),
      xaxis = list(range = c(d1, d2)),
      updatemenus = list(
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
dailyData <- function(d1, d2, country, Tcountry, pickCountry) {
  # Sum data for each day.
  raData <- Tcountry$n %>%
    aggregate(
      by = list(date = Tcountry$date),
      sum
    ) %>%
    rename(n = x)
  raData$rollingAverage <- frollmean(raData$n, n = 7)
  # 2019 archive data.
  archive <- countrySelect(pickCountry, register1[which(register1$archive=="Archive"),], ymd("2019-01-01"), ymd("2019-12-31"))
  archive <- archive$n %>% aggregate(
    by = list(date = archive$date),
    sum
  ) %>%
    rename(n = x)
  archive$rollingAverage <- frollmean(archive$n, n = 7)
  # append raData and archive
  data <- rbind(archive, raData)
}


# UK LAD map 
map0 <- st_read("data/mapC")
showMap <- function(data0, d1, d2) {
  # Load shapefile.
  # https://geoportal.statistics.gov.uk/datasets/ons::local-authority-districts-december-2021-uk-buc/
  mapLA <- map0
  # Aggregate registrations by LAD in selected date range from full dataset.
  data0 <- data0[which(between(data0$date, d1, d2)),]
  TLA <- data0$n %>% aggregate(
    by=list(District = data0$District, Code = data0$`District Code`),
    sum) %>% rename(n=x)
  # Draw map.
  mapLA <- merge(mapLA, TLA, by.x = "LAD21CD", by.y = "Code")
  mapcolour <- c("#81A1C1", "#88C0D0", "#83AB61", "#EBCB8B", "#D08770", "#BF616A", "#D63A2F")
  plot_ly(mapLA) %>%
    add_sf(
      color = ~ n^0.3,
      colors = mapcolour,
      split = ~LAD21NM,
      text = ~ paste0(District, "<br>", n, " new registrations"),
      line = list(width = 0.5, color = "#4C566A"),
      showlegend = FALSE,
      hoverinfo = "text", hoveron = "fills",
      type = "scatter"
    ) %>%
     layout(title=paste0("New registrations between<br>", d1, " and ", d2)) %>%
     hide_colorbar()
    # colorbar(
    #   title = paste0("New registrations between<br>", d1, " and ", d2),
    #   len = 0.5, x = 0.7, y = 0.65
    # )
}

LAdata <- function(data0, d1, d2) {
  # Aggregate registrations by NUTS2.
  data0 <- data0[which(between(data0$date, d1, d2)),]
  TLA <- data0$n %>% aggregate(
    by=list(District = data0$District, Code = data0$`District Code`),
    sum) %>% rename(n=x)
} # NUTS data

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

drawTreemap <- function(df, d1, d2, pickCountry) {
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
      " in ",
      pickCountry,
      "</sup>"
    ),
    uniformtext = list(minsize = 10)
  )
}

# Donut plot
drawDonut <- function(df, d1, d2, pickCountry) {
  Tsection <- df$n %>%
    aggregate(by = list(df$SectionAbb), sum) %>%
    rename(SectionAbb = Group.1, n = x)
  plot_ly(data = Tsection, labels = ~SectionAbb, values = ~n) %>%
    add_pie(hole = 0.6) %>%
    layout(title = paste0(
      "New registrations by sector between ",
      d1, " and ", d2,
      " in ",
      pickCountry
    ))
}

donutData <- function(df, d1, d2) {
  Tsection <- df$n %>%
    aggregate(by = list(df$SectionAbb), sum) %>%
    rename(SectionAbb = Group.1, n = x)
}

# Sectors ----
# Select groups according to selection
groupSelect <- function(register, groups) {
  Tgrp <- register[which(register$Group %in% groups), ]
  Tgrp <- Tgrp$n %>% aggregate(
    by=list(date=Tgrp$date, Group=Tgrp$Group, Group.name=Tgrp$Group.name, Section=Tgrp$Section, Section.name=Tgrp$Section.name), sum) %>%
  rename(n=x) %>%
  group_by(Group) %>% mutate(avg = frollmean(n, n = 7)) %>% ungroup()
}

# Select sectors
sectSelect <- function(register, sects) {
  Tsec <- register[which(register$Section %in% sects), ]
  Tsec <- Tsec$n %>% aggregate(
    by=list(date=Tsec$date, Section=Tsec$Section, Section.name=Tsec$Section.name), sum) %>%
    rename(n=x) %>%
    group_by(Section) %>% mutate(avg = frollmean(n, n = 7)) %>% ungroup()
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
groupPlot <- function(df, pickCountry) {
  plot_ly(data = df) %>%
    add_trace(
      x = ~date, y = ~avg, color = ~Group.name, colors = "viridis",
      type = "scatter", mode = "lines", visible = TRUE,
      line = list(width = 0.8)
    ) %>%
    add_trace(
      x = ~date, y = ~n, color = ~Group.name, colors = "viridis",
      type = "bar", visible = FALSE, opacity = 0.3
    ) %>%
    layout(
      title = paste0("Daily registrations by industry group in ", pickCountry),
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
sectorPlot <- function(df, pickCountry) {
  plot_ly(data = df) %>%
    add_trace(
      x = ~date, y = ~avg, color = ~Section.name, colors = "viridis",
      type = "scatter", mode = "lines", visible = TRUE,
      line = list(width = 0.8)
    ) %>%
    add_trace(
      x = ~date, y = ~n, color = ~Section.name, colors = "viridis",
      type = "bar", visible = FALSE, opacity = 0.3
    ) %>%
    layout(
      title = paste0("Daily registrations by industry sector in ", pickCountry),
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

# Regions ----
# Select LADs
ladSelect <- function(register, lads) {
  Tlad <- register[which(register$District %in% lads),]
  Tlad <- Tlad$n %>% aggregate(
    by=list(date=Tlad$date, District=Tlad$District, County=Tlad$County, Country=Tlad$Country),
    sum) %>% rename(n=x) %>%
    group_by(District) %>% mutate(avg = frollmean(n, n = 7)) %>% ungroup()
}

ladTable <- function(tabledata) {
  container_dt <- withTags(table(
    class = "display",
    thead(
      tr(
        lapply((c("District", "County", "Country")), th)
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

# Local authority districts plot
districtPlot <- function(df) {
  plot_ly(data = df) %>%
    add_trace(
      x = ~date, y = ~avg, color = ~District, colors = "viridis",
      type = "scatter", mode = "lines", visible = TRUE,
      line = list(width = 0.8)
    ) %>%
    add_trace(
      x = ~date, y = ~n, color = ~District, colors = "viridis",
      type = "bar", visible = FALSE, opacity = 0.3
    ) %>%
    layout(
      title = paste0("Daily registrations by Local Authority District"),
      yaxis = list(title = "Number of registrations"),
      xaxis = list(showgrid = FALSE, title = ""),
      updatemenus = list(
        list(type = "dropdown", y = 1, x = 1.4, direction = "down", buttons = list(
          list(
            label = "Rolling average", method = "update",
            args = list(list(visible = c(rep(TRUE, length(unique(df$District))), rep(FALSE, length(unique(df$District))))))
          ),
          list(
            label = "Daily total", method = "update",
            args = list(list(visible = c(rep(FALSE, length(unique(df$District))), rep(TRUE, length(unique(df$District))))))
          )
        ))
      ),
      legend = list(x = 1, y = 0.8)
    )
}

#Old
groupRegionData <- function(d1, d2, group, a, pickData) {
  if (a == 1) {
    df <- registerPC[which(between(registerPC$date, d1, d2) &
      registerPC$Group == group &
      registerPC$archive == pickData), ] %>%
      group_by(date, NUTS1) %>%
      count() %>%
      ungroup() %>%
      group_by(NUTS1) %>%
      pivot_wider(id_cols = date, names_from = NUTS1, values_from = n)
  } else {
    section <- registerPC$SectionAbb[which(registerPC$Group == group)][1]
    df <- registerPC[which(between(registerPC$date, d1, d2) &
      registerPC$SectionAbb == section &
      registerPC$archive == pickData), ] %>%
      group_by(date, NUTS1) %>%
      count() %>%
      ungroup() %>%
      group_by(NUTS1) %>%
      pivot_wider(id_cols = date, names_from = NUTS1, values_from = n)
  }
  regions <- c("UKC", "UKD", "UKE", "UKF", "UKG", "UKH", "UKI", "UKJ", "UKK", "UKL", "UKM", "UKN")
  df[setdiff(regions, names(df))] <- 0
  df$Eng <- select(df, !(c(date, UKL, UKM, UKN))) %>% rowSums()
  df$EngExLon <- df$Eng - df$UKI
  df <- df %>%
    rename(Lon = UKI, Sco = UKM, Wal = UKL, NI = UKN) %>%
    select(c(date, Eng, EngExLon, Lon, Sco, Wal, NI)) %>%
    pivot_longer(!date, names_to = "Region", values_to = "n") # %>%
  df <- merge(expand.grid(date = as_date(min(df$date):max(df$date)), Region = unique(df$Region)),
    df,
    all = TRUE
  )
  df$n <- replace_na(df$n, 0)
  df <- df %>%
    group_by(Region) %>%
    mutate(avg = frollmean(n, n = 7))
  df$Region <- mapvalues(df$Region,
    from = c("Eng", "EngExLon", "Lon", "Sco", "Wal", "NI"),
    to = c("England", "England excl. London", "London", "Scotland", "Wales", "Northern Ireland")
  )
  df
}

groupRegion <- function(data, group, a) {
  # Set title.
  if (a == 1) {
    groupName <- registerPC$Group.name[which(registerPC$Group == group)][1]
    title <- paste0("Daily registrations by region in ", groupName)
  } else if (a == 2) {
    section <- registerPC$SectionAbb[which(registerPC$Group == group)][1]
    title <- paste0("Daily registrations by region in ", section)
  }
  # Plot.
  plot_ly(data = data) %>%
    add_trace(
      x = ~date, y = ~avg, color = ~Region, colors = "viridis",
      type = "scatter", mode = "lines", visible = TRUE,
      line = list(width = 0.8)
    ) %>%
    add_trace(
      x = ~date, y = ~n, color = ~Region, colors = "viridis",
      type = "scatter", mode = "lines", visible = FALSE,
      line = list(width = 0.8)
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

# Create Table by date, postcodes, 5-digit SIC, and number of registrations
totalRegistrations <- function(d1, d2, Tcustom, pickPostcode, pickSIC) {
  dataCustom <- Tcustom$n[which(Tcustom$postcodeDistrict %in% pickPostcode &
    between(Tcustom$date, d1, d2) &
    Tcustom$Class %in% pickSIC)] %>%
    aggregate(
      by = list(
        Date = Tcustom$date[which(Tcustom$postcodeDistrict %in% pickPostcode &
          between(Tcustom$date, d1, d2) &
          Tcustom$Class %in% pickSIC)],
        "Postcode district" = Tcustom$postcodeDistrict[which(Tcustom$postcodeDistrict %in% pickPostcode &
          between(Tcustom$date, d1, d2) &
          Tcustom$Class %in% pickSIC)],
        Class = Tcustom$Class[which(Tcustom$postcodeDistrict %in% pickPostcode &
          between(Tcustom$date, d1, d2) &
          Tcustom$Class %in% pickSIC)]
      ),
      sum
    ) %>%
    rename(Registrations = x)
  datatable(dataCustom,
    rownames = F, # class = "",
    extensions = "Buttons",
    options = list(
      autoWidth = T,
      dom = "Bfrtip",
      buttons = c("pageLength", "copy", "print"),
      pagelength = 10,
      lengthMenu = list(
        c(10, 25, 100, -1),
        c("10", "25", "100", "All")
      ),
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

# download custom data
customDataDownload <- function(d1, d2, Tcustom, pickPostcode, pickSIC) {
  dataCustom <- Tcustom$n[which(Tcustom$postcodeDistrict %in% pickPostcode &
    between(Tcustom$date, d1, d2) &
    Tcustom$Class %in% pickSIC)] %>%
    aggregate(
      by = list(
        Date = Tcustom$date[which(Tcustom$postcodeDistrict %in% pickPostcode &
          between(Tcustom$date, d1, d2) &
          Tcustom$Class %in% pickSIC)],
        "Postcode district" = Tcustom$postcodeDistrict[which(Tcustom$postcodeDistrict %in% pickPostcode &
          between(Tcustom$date, d1, d2) &
          Tcustom$Class %in% pickSIC)],
        Class = Tcustom$Class[which(Tcustom$postcodeDistrict %in% pickPostcode &
          between(Tcustom$date, d1, d2) &
          Tcustom$Class %in% pickSIC)]
      ),
      sum
    ) %>%
    rename(Registrations = x)
}

# Dissolutions functions ----
## Value boxes----
vboxesDis <- function(vb, d1, d2, country, TcountryDis) {
  if (vb == 1) {
    subtitle <- paste0("Dissolutions between ", d1, " and ", d2)
    color <- "red"
    icon <- icon("store-slash")
  } else if (vb == 2) {
    subtitle <- paste("Dissolutions between first vaccine and", d2)
    color <- "olive"
    icon <- icon("syringe")
  } else if (vb == 3) {
    subtitle <- paste("Dissolutions during the most recent lockdown")
    color <- "maroon"
    icon <- icon("shop-lock")
  } else if (vb == 4) {
    subtitle <- paste("Dissolutions since easing the most recent lockdown and", d2)
    color <- "purple"
    icon <- icon("building-lock")
  }
  valueBox(
    value = prettyNum(sum(TcountryDis$n[which(TcountryDis$NUTS1 %in% country &
      between(TcountryDis$date, d1, d2))]),
    big.mark = ",", scientific = FALSE
    ),
    subtitle = subtitle, color = color, icon = icon
  )
}

# Daily dissolutions plot
dailyPlotDis <- function(d1, d2, country, TcountryDis, pickCountry) {
  # Sum data for each day in relevant regions and date range.
  raData <- TcountryDis$n[which(TcountryDis$NUTS1 %in% country &
    between(TcountryDis$date, d1, d2))] %>%
    aggregate(
      by = list(date = TcountryDis$date[which(TcountryDis$NUTS1 %in% country &
        between(TcountryDis$date, d1, d2))]),
      sum
    ) %>%
    rename(n = x)
  # Data for relevant regions during LD3.
  ld3Data <- TcountryDis$n[which(TcountryDis$NUTS1 %in% country &
    between(
      TcountryDis$date,
      as.Date("2021-01-05"), as.Date("2021-04-12")
    ))] %>%
    aggregate(
      by = list(date = TcountryDis$date[which(TcountryDis$NUTS1 %in% country &
        between(
          TcountryDis$date,
          as.Date("2021-01-05"), as.Date("2021-04-12")
        ))]),
      sum
    )
  medianLD3 <- median(ld3Data$x)
  # Data for relevant regions during 2019.
  # data2019 <- Tcountry$n[which(Tcountry$NUTS1 %in% country & between(Tcountry$date, as.Date("2019-01-01"), as.Date("2019-12-31")))] %>%
  #   aggregate(by = list(date = Tcountry$date[which(Tcountry$NUTS1 %in% country & between(Tcountry$date, as.Date("2019-01-01"), as.Date("2019-12-31")))]),
  #             sum)
  # median2019 <- median(data2019$x)

  # Plot rolling average/daily registrations, with or without lockdown periods.
  plot_ly() %>%
    add_trace(
      x = raData$date, y = (frollmean(raData$n, n = 7)), name = "7-day rolling average",
      type = "scatter", mode = "lines", showlegend = FALSE, visible = TRUE
    ) %>%
    # add_trace(
    #   x = (archive$date + 365), y = (frollmean(archive$n, n = 7)), name = "2019; 7-day RA",
    #   type = "scatter", mode = "lines", showlegend = TRUE, visible = TRUE,
    #   line = list(dash = "dash", width = 0.5)
    # ) %>%
    add_trace(
      x = raData$date, y = raData$n, name = "daily total",
      type = "bar", showlegend = FALSE, visible = TRUE, opacity = 0.3
    ) %>%
    add_segments(
      x = d1, xend = d2,
      y = median(raData$n), yend = median(raData$n), name = "Median; selected period", showlegend = T
    ) %>%
    add_segments(
      x = min(raData$date), xend = max(raData$date),
      y = medianLD3, yend = medianLD3, name = "Median; Lockdown 3", showlegend = T
    ) %>%
    layout(
      title = paste0(
        "Daily company dissolutions in ",
        pickCountry # ,
        # "<br>",
        # "<sup>",
        # "between ", d1, " and ", d2,
        # "</sup>"
      ),
      yaxis = list(title = "Number of dissolutions", showgrid = F, range = c(0, 1.1 * max(raData$n))),
      xaxis = list(range = c(d1, d2)),
      updatemenus = list(
        list(type = "dropdown", y = 0.75, x = 1.25, active = 1, direction = "down", buttons = list(
          list(label = "Show lockdowns", method = "relayout", args = list(list(
            shapes = list(
              list(
                type = "rect",
                fillcolor = "gray", line = list(color = "gray"), opacity = 0.2,
                x0 = "2020-03-23", x1 = "2020-07-04", xref = "x",
                y0 = 0, y1 = 1000, yref = "y"
              ),
              list(
                type = "rect",
                fillcolor = "gray", line = list(color = "gray"), opacity = 0.2,
                x0 = "2020-11-05", x1 = "2020-12-02", xref = "x",
                y0 = 0, y1 = 1000, yref = "y"
              ),
              list(
                type = "rect",
                fillcolor = "gray", line = list(color = "gray"), opacity = 0.2,
                x0 = "2021-01-05", x1 = "2021-04-12", xref = "x",
                y0 = 0, y1 = 1000, yref = "y"
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
dailyData <- function(d1, d2, country, TcountryDis, pickCountry) {
  # Sum data for each day in relevant regions and date range.
  raData <- TcountryDis$n[which(TcountryDis$NUTS1 %in% country &
    between(TcountryDis$date, d1, d2))] %>%
    aggregate(
      by = list(date = TcountryDis$date[which(TcountryDis$NUTS1 %in% country &
        between(TcountryDis$date, d1, d2))]),
      sum
    ) %>%
    rename(n = x)
  raData$rollingAverage <- frollmean(raData$n, n = 7)

  # append raData and archive
  data <- rbind(raData)
}

## UK NUTS2 mapDissol ----
showMapDis <- function(d1, d2, pickData) {
  # Load shapefile.
  # https://geoportal.statistics.gov.uk/datasets/nuts-level-2-january-2018-ultra-generalised-clipped-boundaries-in-the-united-kingdom/
  mapNUTS <- map0
  # Aggregate registrations by NUTS2 in selected date range from full dataset.
  TNUTS <- dissolutions[which(between(dissolutions$date, d1, d2)), ] %>%
    group_by(NUTS2) %>%
    count()
  # Draw map.
  mapNUTS <- merge(mapNUTS, TNUTS, by.x = "nuts218cd", by.y = "NUTS2")
  mapcolour <- c("#81A1C1", "#88C0D0", "#83AB61", "#EBCB8B", "#D08770", "#BF616A", "#D63A2F")
  plot_ly(mapNUTS) %>%
    add_sf(
      color = ~ n^0.6,
      colors = mapcolour,
      split = ~nuts218cd,
      text = ~ paste0(nuts218nm, "<br>", n, " dissolutions"),
      line = list(width = 0.9, color = "#4C566A"),
      showlegend = FALSE,
      hoverinfo = "text", hoveron = "fills",
      type = "scatter"
    ) %>%
    layout() %>%
    colorbar(
      title = paste0("Dissolutions between<br>", d1, " and ", d2),
      len = 0.5, x = 0.7, y = 0.65
    )
}

NUTSdataDis <- function(d1, d2, pickData) {
  # Aggregate registrations by NUTS2 in selected date range from full dataset.
  TNUTS <- dissolutions[which(between(dissolutions$date, d1, d2)), ] %>%
    group_by(NUTS218NM) %>%
    count()
} # NUTS data

## treemap ----
drawTreemapDis <- function(df, d1, d2, pickCountry) {
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
      "Distribution of dissolutions by SIC codes",
      "<br>",
      "<sup>",
      "between ", d1, " and ", d2,
      " in ",
      pickCountry,
      "</sup>"
    ),
    uniformtext = list(minsize = 10)
  )
}

## Donut plot ----
drawDonut <- function(df, d1, d2, pickCountry) {
  Tsection <- df$n %>%
    aggregate(by = list(df$SectionAbb), sum) %>%
    rename(SectionAbb = Group.1, n = x)
  plot_ly(data = Tsection, labels = ~SectionAbb, values = ~n) %>%
    add_pie(hole = 0.6) %>%
    layout(title = paste0(
      "Dissolutions by sector between ",
      d1, " and ", d2,
      " in ",
      pickCountry
    ))
}

## Groups table ----
showTableDis <- function(tabledata) {
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
groupPlotDis <- function(df, pickCountry) {
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
      title = paste0("Daily dissolutions by industry group in ", pickCountry),
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
sectorPlotDis <- function(df, pickCountry) {
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
      title = paste0("Daily dissolutions by industry sector in ", pickCountry),
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

groupRegionDataDis <- function(d1, d2, group, a) {
  if (a == 1) {
    df <- dissolutions[which(between(dissolutions$date, d1, d2) &
      dissolutions$Group == group), ] %>%
      group_by(date, NUTS1) %>%
      count() %>%
      ungroup() %>%
      group_by(NUTS1) %>%
      pivot_wider(id_cols = date, names_from = NUTS1, values_from = n)
  } else {
    section <- dissolutions$SectionAbb[which(dissolutions$Group == group)][1]
    df <- dissolutions[which(between(dissolutions$date, d1, d2) &
      dissolutions$SectionAbb == section), ] %>%
      group_by(date, NUTS1) %>%
      count() %>%
      ungroup() %>%
      group_by(NUTS1) %>%
      pivot_wider(id_cols = date, names_from = NUTS1, values_from = n)
  }
  regions <- c("UKC", "UKD", "UKE", "UKF", "UKG", "UKH", "UKI", "UKJ", "UKK", "UKL", "UKM", "UKN")
  df[setdiff(regions, names(df))] <- 0
  df$Eng <- select(df, !(c(date, UKL, UKM, UKN))) %>% rowSums()
  df$EngExLon <- df$Eng - df$UKI
  df <- df %>%
    rename(Lon = UKI, Sco = UKM, Wal = UKL, NI = UKN) %>%
    select(c(date, Eng, EngExLon, Lon, Sco, Wal, NI)) %>%
    pivot_longer(!date, names_to = "Region", values_to = "n") # %>%
  df <- merge(expand.grid(date = as_date(min(df$date):max(df$date)), Region = unique(df$Region)),
    df,
    all = TRUE
  )
  df$n <- replace_na(df$n, 0)
  df <- df %>%
    group_by(Region) %>%
    mutate(avg = frollmean(n, n = 7))
  df$Region <- mapvalues(df$Region,
    from = c("Eng", "EngExLon", "Lon", "Sco", "Wal", "NI"),
    to = c("England", "England excl. London", "London", "Scotland", "Wales", "Northern Ireland")
  )
  df
}

groupRegionDis <- function(data, group, a) {
  # Set title.
  if (a == 1) {
    groupName <- dissolutions$Group.name[which(dissolutions$Group == group)][1]
    title <- paste0("Daily dissolutions by region in ", groupName)
  } else if (a == 2) {
    section <- dissolutions$SectionAbb[which(dissolutions$Group == group)][1]
    title <- paste0("Daily dissolutions by region in ", section)
  }
  # Plot.
  plot_ly(data = data) %>%
    add_trace(
      x = ~date, y = ~avg, color = ~Region, colors = "viridis",
      type = "scatter", mode = "lines", visible = TRUE,
      line = list(width = 0.8)
    ) %>%
    add_trace(
      x = ~date, y = ~n, color = ~Region, colors = "viridis",
      type = "scatter", mode = "lines", visible = FALSE,
      line = list(width = 0.8)
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
