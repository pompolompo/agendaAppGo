
# Libraries ---------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggfunnel)

# Sources -----------------------------------------------------------------
source("auxScripts/import.R")
source("auxScripts/setup.R")
source("auxScripts/functions.R")

ui <- dashboardPage(
  dashboardHeader(title = "Agenda Routines"),
  dashboardSidebar(sidebarMenu(.list = menu_items)),
  dashboardBody(tabItems(tab_funnel, tab_simula))
)
server <- function(input, output, session){
  
# Data Preparation --------------------------------------------------------
  dataKPI_history <- reactive({
  dat <- lapply(dKPI_history, function(x){
    x |> 
      filter(
        week >= min(input$weeks),
        week <= max(input$weeks)
      )
  })
  
  if(input$agg_lvl == "spain"){
    mutate(dat$spain, zone = "SPAIN") |> 
      select(zone, everything())
  }else if(input$agg_lvl == "region"){
    dat$region |> 
      filter(region == input$region) |> 
      rename(zone = region) |> 
      bind_rows(
        mutate(dat$spain, zone = "SPAIN")
      )
  }else if(input$agg_lvl == "area"){
    dat$area |> 
      filter(area == input$area) |> 
      rename(zone = area) |> 
      bind_rows(
        mutate(dat$spain, zone = "SPAIN")
      ) |> 
      bind_rows(
        dat$region |> 
          filter(region == input$region) |> 
          rename(zone = region)
      ) |> 
      ungroup() |> 
      select(-region)
  }else{
    dat$shop |> 
      filter(shop == input$shop) |> 
      rename(zone = shop) |> 
      bind_rows(
        mutate(dat$spain, zone = "SPAIN")
      ) |> 
      bind_rows(
        dat$region |> 
          filter(region == input$region) |> 
          rename(zone = region)
      ) |> 
      bind_rows(
        dat$area |> 
          filter(area == input$area) |> 
          rename(zone = area)
      ) |> 
      ungroup() |> 
      select(-c(region, area))
  }
})
  dataKPI_month <- reactive({
    if(input$agg_lvl == "spain"){
      dKPI_month$spain |> 
        mutate(zone = ifelse(year == lubridate::year(Sys.Date()) , "SPAIN", "PY")) |> 
        select(zone, everything())
    }else if(input$agg_lvl == "region"){
      dKPI_month$region |> 
        filter(region == input$region) |>
        rename(zone = region) |> 
        mutate(zone = ifelse(year == lubridate::year(Sys.Date()) , zone, "PY")) |> 
        bind_rows(
          mutate(dKPI_month$spain, zone = "SPAIN") |> 
            filter(year == lubridate::year(Sys.Date()))
        )
    }else if(input$agg_lvl == "area"){
      dKPI_month$area |> 
        filter(area == input$area) |> 
        rename(zone = area) |> 
        mutate(zone = ifelse(year == lubridate::year(Sys.Date()) , zone, "PY")) |>
        bind_rows(
          mutate(dKPI_month$spain, zone = "SPAIN") |> 
            filter(year == lubridate::year(Sys.Date()))
        ) |> 
        bind_rows(
          dKPI_month$region |> 
            filter(
              year == lubridate::year(Sys.Date()),
              region == input$region
            ) |> 
            rename(zone = region)
        ) |> 
        ungroup() |> 
        select(-region)
    }else{
      dKPI_month$shop |> 
        filter(shop == input$shop) |> 
        rename(zone = shop) |> 
        mutate(zone = ifelse(year == lubridate::year(Sys.Date()) , zone, "PY")) |>
        bind_rows(
          mutate(dKPI_month$spain, zone = "SPAIN") |> 
            filter(year == lubridate::year(Sys.Date()))
        ) |> 
        bind_rows(
          dKPI_month$region |> 
            filter(
              year == lubridate::year(Sys.Date()),
              region == input$region
            ) |> 
            rename(zone = region)
        ) |> 
        bind_rows(
          dKPI_month$area |> 
            filter(
              area == input$area,
              year == lubridate::year(Sys.Date())
            ) |> 
            rename(zone = area)
        ) |> 
        ungroup() |> 
        select(-c(region, area))
    }
  })
  dataKPI_M2D <- reactive({
    if(input$agg_lvl == "spain"){
      dKPI_M2D$spain |> 
        mutate(zone = ifelse(year == lubridate::year(Sys.Date()) , "SPAIN", "PY")) |> 
        select(zone, everything())
    }else if(input$agg_lvl == "region"){
      dKPI_M2D$region |> 
        filter(region == input$region) |>
        rename(zone = region) |> 
        mutate(zone = ifelse(year == lubridate::year(Sys.Date()) , zone, "PY")) |> 
        bind_rows(
          mutate(dKPI_M2D$spain, zone = "SPAIN") |> 
            filter(year == lubridate::year(Sys.Date()))
        ) |> 
        select(-year)
    }else if(input$agg_lvl == "area"){
      dKPI_M2D$area |> 
        filter(area == input$area) |> 
        rename(zone = area) |> 
        mutate(zone = ifelse(year == lubridate::year(Sys.Date()) , zone, "PY")) |>
        bind_rows(
          mutate(dKPI_M2D$spain, zone = "SPAIN") |> 
            filter(year == lubridate::year(Sys.Date()))
        ) |> 
        bind_rows(
          dKPI_M2D$region |> 
            filter(
              year == lubridate::year(Sys.Date()),
              region == input$region
            ) |> 
            rename(zone = region)
        ) |> 
        ungroup() |> 
        select(-c(region, year))
    }else{
      dKPI_M2D$shop |> 
        filter(shop == input$shop) |> 
        rename(zone = shop) |> 
        mutate(zone = ifelse(year == lubridate::year(Sys.Date()) , zone, "PY")) |>
        bind_rows(
          mutate(dKPI_M2D$spain, zone = "SPAIN") |> 
            filter(year == lubridate::year(Sys.Date()))
        ) |> 
        bind_rows(
          dKPI_M2D$region |> 
            filter(
              year == lubridate::year(Sys.Date()),
              region == input$region
            ) |> 
            rename(zone = region)
        ) |> 
        bind_rows(
          dKPI_M2D$area |> 
            filter(
              area == input$area,
              year == lubridate::year(Sys.Date())
            ) |> 
            rename(zone = area)
        ) |> 
        ungroup() |> 
        select(-c(region, area, year))
    }
  })
  gross_M2D <- reactive({
    dat <- 
    if(input$agg_lvl == "spain"){
      qKPI_month |> 
        filter(year == lubridate::year(Sys.Date()))
    }else if(input$agg_lvl == "region"){
      qKPI_month |> 
        filter(
          year == lubridate::year(Sys.Date()),
          region == input$region
        )
    }else if(input$agg_lvl == "area"){
      qKPI_month |> 
        filter(
          year == lubridate::year(Sys.Date()),
          area == input$area
        )
    }else{
      qKPI_month |> 
        filter(
          year == lubridate::year(Sys.Date()),
          shop == input$shop
        )
    }
    dat |> 
      pull(gross) |> 
      sum()
  })
  
# Sidebar -----------------------------------------------------------------
  observeEvent(input$region, {
    areas <- variables$areas[names(variables$areas) == input$region]
    updateSelectInput(session, "area", choices = unname(areas))
  })
  observeEvent(input$area, {
    shops <- variables$shops[names(variables$shops) == input$area]
    updateSelectInput(session, "shop", choices = unname(shops))
  })

# KPI - graphs ------------------------------------------------------------
  output$plot_KPI_history <- renderPlot({
    dataKPI_history() |> 
      filter(zone %in% c(case_when(
        "PY" == input$plot_zones ~ "PY",
        "SPAIN" == input$plot_zones ~ "SPAIN",
        "region" == input$plot_zones ~ input$region,
        "area" == input$plot_zones ~ input$area
      ), case_when(
        "spain" == input$agg_lvl ~ "SPAIN",
        "region" == input$agg_lvl ~ input$region,
        "area" == input$agg_lvl ~ input$area,
        "shop" == input$agg_lvl ~ input$shop,
      ))) |>  
      ggplot(aes(x = week, y = .data[[input$kpi]], colour = zone, group = zone)) +
      geom_line(linewidth = 1.5) +
      geom_point(size = 2) +
      labs(
        x = "Semana",
        y = "",
        colour = "Comparación"
      ) +
      theme_bw() +
      scale_y_continuous(labels = function(x) paste0(100*x, "%"))
  })
  output$plot_KPI_month <- renderPlot({
    dataKPI_month() |> 
      filter(zone %in% c(case_when(
        "PY" == input$plot_zones ~ "PY",
        "SPAIN" == input$plot_zones ~ "SPAIN",
        "region" == input$plot_zones ~ input$region,
        "area" == input$plot_zones ~ input$area
      ), case_when(
        "spain" == input$agg_lvl ~ "SPAIN",
        "region" == input$agg_lvl ~ input$region,
        "area" == input$agg_lvl ~ input$area,
        "shop" == input$agg_lvl ~ input$shop,
      ))) |>  
      ggplot(aes(x = wd, y = .data[[input$kpi]], colour = zone, group = zone)) +
      geom_line(linewidth = 1.5) +
      geom_point(size = 2) +
      labs(
        x = "Working Day",
        y = "",
        colour = "Comparación"
      ) +
      theme_bw() +
      scale_y_continuous(labels = function(x) paste0(100*x, "%"))
  })
  output$plot_KPI_M2D <- renderPlot({
    dataKPI_M2D() |> 
      filter(zone %in% c(case_when(
        "PY" == input$plot_zones ~ "PY",
        "SPAIN" == input$plot_zones ~ "SPAIN",
        "region" == input$plot_zones ~ input$region,
        "area" == input$plot_zones ~ input$area
      ), case_when(
        "spain" == input$agg_lvl ~ "SPAIN",
        "region" == input$agg_lvl ~ input$region,
        "area" == input$agg_lvl ~ input$area,
        "shop" == input$agg_lvl ~ input$shop,
      ))) |> 
      ggplot(aes(x = wd, y = .data[[input$kpi]], colour = zone, group = zone)) +
      geom_line(linewidth = 1.5) +
      geom_point(size = 2) +
      labs(
        x = "Working Day",
        y = "",
        colour = "Comparación"
      ) +
      theme_bw() +
      scale_y_continuous(labels = function(x) paste0(100*x, "%"))
  })

# KPI - tables ------------------------------------------------------------
  output$tble_KPI_history <- renderTable({
    dataKPI_history() |> 
      filter(zone %in% c(case_when(
        "PY" == input$plot_zones ~ "PY",
        "SPAIN" == input$plot_zones ~ "SPAIN",
        "region" == input$plot_zones ~ input$region,
        "area" == input$plot_zones ~ input$area
      ), case_when(
        "spain" == input$agg_lvl ~ "SPAIN",
        "region" == input$agg_lvl ~ input$region,
        "area" == input$agg_lvl ~ input$area,
        "shop" == input$agg_lvl ~ input$shop,
      ))) |> 
      ungroup() |> 
      select(week, zone, kpi = input$kpi) |> 
      mutate(
        kpi = ifelse(is.na(kpi), "00.00 %", paste0(sprintf("%05.2f", 100*kpi), "%")) |> 
          str_replace("^0 %$", "00.00 %"),
        week = as.integer(week)
      ) |> 
      pivot_wider(
        names_from = zone,
        values_from = kpi
      ) |> 
      arrange(week)
  })
  output$tble_KPI_month <- renderTable({
    dataKPI_month() |> 
      filter(zone %in% c(case_when(
        "PY" == input$plot_zones ~ "PY",
        "SPAIN" == input$plot_zones ~ "SPAIN",
        "region" == input$plot_zones ~ input$region,
        "area" == input$plot_zones ~ input$area
      ), case_when(
        "spain" == input$agg_lvl ~ "SPAIN",
        "region" == input$agg_lvl ~ input$region,
        "area" == input$agg_lvl ~ input$area,
        "shop" == input$agg_lvl ~ input$shop,
      ))) |> 
      ungroup() |> 
      select(wd, zone, kpi = input$kpi) |> 
      mutate(
        kpi = ifelse(is.na(kpi), "00.00 %", paste0(sprintf("%05.2f", 100*kpi), "%")) |> 
          str_replace("^0 %$", "00.00 %"),
        wd = as.integer(wd)
      ) |> 
      pivot_wider(
        names_from = zone,
        values_from = kpi
      ) |> 
      arrange(wd)
  })
  output$tble_KPI_M2D <- renderTable({
    dataKPI_M2D() |> 
      filter(zone %in% c(case_when(
        "PY" == input$plot_zones ~ "PY",
        "SPAIN" == input$plot_zones ~ "SPAIN",
        "region" == input$plot_zones ~ input$region,
        "area" == input$plot_zones ~ input$area
      ), case_when(
        "spain" == input$agg_lvl ~ "SPAIN",
        "region" == input$agg_lvl ~ input$region,
        "area" == input$agg_lvl ~ input$area,
        "shop" == input$agg_lvl ~ input$shop,
      ))) |> 
      ungroup() |> 
      select(wd, zone, kpi = input$kpi) |> 
      mutate(
        kpi = ifelse(is.na(kpi), "00.00 %", paste0(sprintf("%05.2f", 100*kpi), "%")) |> 
          str_replace("^0 %$", "00.00 %"),
        wd = as.integer(wd)
      ) |> 
      pivot_wider(
        names_from = zone,
        values_from = kpi
      ) |> 
      arrange(wd)
  })

# KPI - valueBox ----------------------------------------------------------
  output$valueKPI_SPAIN <- renderValueBox({
    val <- dKPI_M2D$spain |>
      filter(
        year == lubridate::year(Sys.Date()),
        wd == max(wd)
      ) |> 
      pull(input$kpi)
    
    valueBox(
      value = paste0(sprintf("%05.2f", 100*val), "%"),
      subtitle = "Spain M2D",
      icon = NULL,
      color = "aqua"
    )
  })
  output$valueKPI_UPPER <- renderValueBox({
    if(input$agg_lvl == "spain"){
      val <- dKPI_M2D$spain |> 
        filter(
          year == lubridate::year(Sys.Date()) - 1,
          wd == max(wd)
        ) |> 
        pull(input$kpi)
      texto <- "Spain M2D (PY)"
    }else if(input$agg_lvl == "region"){
      val <- dKPI_M2D$region |> 
        filter(
          year == lubridate::year(Sys.Date()) - 1,
          wd == max(wd),
          region == input$region
        ) |> 
        pull(input$kpi)
      texto <- "Región M2D (PY)"
    }else if(input$agg_lvl == "area"){
      val <- dKPI_M2D$region |> 
        filter(
          year == lubridate::year(Sys.Date()),
          wd == max(wd),
          region == input$region
        ) |> 
        pull(input$kpi)
      texto <- "Región M2D"
    }else{
      val <- dKPI_M2D$area |> 
        filter(
          year == lubridate::year(Sys.Date()),
          wd == max(wd),
          area == input$area
        ) |> 
        pull(input$kpi)
      texto <- "Área M2D"
    }
    
    valueBox(
      value = paste0(sprintf("%05.2f", 100*val), "%"),
      subtitle = texto,
      icon = NULL,
      color = "aqua"
    )
  })
   
# Simulation --------------------------------------------------------------
  observeEvent(dataKPI_M2D(), {
    kpi <- dataKPI_M2D() |> 
      filter(
        wd == max(wd),
        zone == switch(
          input$agg_lvl,
          "spain" = "SPAIN",
          "region" = input$region,
          "area" = input$area,
          "shop" = input$shop
        )
      ) |> 
      mutate(across(
        .cols = cancelRate:app2Test,
        .fns = function(x) 100*x
      ), .keep = "used" )
    
    cancelRate <- kpi$cancelRate
    showRate <- kpi$showRate
    testOnShow <- kpi$testOnShow
    testOppOnShow <- kpi$testOppOnShow
    app2Test <- kpi$app2Test
    
    updateSliderInput(session, "cancelRate", value = cancelRate)
    updateSliderInput(session, "showRate", value = showRate)
    # updateSliderInput(session, "testOnShow", value = testOnShow)
    updateSliderInput(session, "testOppOnShow", value = testOppOnShow)
    updateSliderInput(session, "app2Test", value = app2Test)
  })
  output$plot_funnelSimula <- renderPlot({
    tibble(
      step = c("gross", "net", "show", "opp"),
      size = round(gross_M2D() * c(
        1,
        (1 - input$cancelRate / 100),
        (1 - input$cancelRate / 100) * input$showRate / 100,
        (1 - input$cancelRate / 100) * input$showRate / 100 * input$testOppOnShow / 100
      ))
    ) |> 
      funnel(values = size, levels = step)
  })
}

shinyApp(ui, server)

