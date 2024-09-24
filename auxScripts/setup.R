
# Libraries ---------------------------------------------------------------
library(shiny)
library(shinydashboard)

# PreDefine ---------------------------------------------------------------
variables1 <- list(
  areas = variables$areas[names(variables$areas) == variables$regions[1]],
  shops = variables$shops[names(variables$shops) == variables$areas[1]]
) |> lapply(unname)


# Sidebar -----------------------------------------------------------------
menu_items <- list(
  menuItem("Funnel", tabName = "funnel", icon = icon("filter-circle-dollar")),
  menuItem("Simulación", tabName = "simula", icon = icon("cloud")),
  menuItem("Filtros", icon = icon("map"),
    selectInput("agg_lvl", NULL, setNames(c("spain", "region", "area", "shop"), 
                                          c("España", "Región", "Área", "Tienda"))),
    selectInput("region", NULL, variables$regions),
    selectInput("area", NULL, variables1$areas),
    selectInput("shop", NULL, variables1$shops),
     sliderInput("weeks", NULL, value = range(qKPI_history$week), step = 1, 
                 min = min(qKPI_history$week), max = max(qKPI_history$week))
  )
)


# Body --------------------------------------------------------------------

## Funnel -----------------------------------------------------------------
history_KPI.graph <- tabPanel("Evolución", plotOutput("plot_KPI_history"))
month_KPI.graph <- tabPanel("Daily", plotOutput("plot_KPI_month"))
M2D_KPI.graph <- tabPanel("M2D", plotOutput("plot_KPI_M2D"))

history_KPI.tble <- tabPanel("Evolución", tableOutput("tble_KPI_history"))
month_KPI.tble <- tabPanel("Daily", tableOutput("tble_KPI_month"))
M2D_KPI.tble <- tabPanel("M2D", tableOutput("tble_KPI_M2D"))

kpi_selector <- selectInput("kpi", NULL, setNames(
  c("cancelRate", "showRate", "testOnShow", "testOppOnShow", "app2Test"),
  c("Cancel Rate", "Show Rate", "Test on Show", "Test Opportunity on Show", "App to Test")
))
zone_selector <- checkboxGroupInput("plot_zones", NULL, inline = TRUE, setNames(
  c("PY", "SPAIN", "region", "area"),
  c("PY", "SPAIN", "Región", "Área")
))

tab_funnel <- tabItem(
  tabName = "funnel",
  tabBox(width = 8, side = "left", history_KPI.graph, month_KPI.graph, M2D_KPI.graph),
  box(width = 4, kpi_selector, zone_selector),
  tabBox(width = 4, side = "left",  history_KPI.tble, month_KPI.tble, M2D_KPI.tble)
  # valueBoxOutput("valueKPI_SPAIN", width = 3),
  # valueBoxOutput("valueKPI_FCST", width = 3),
  # valueBoxOutput("valueKPI_UPPER", width = 3)
)


# Simulation --------------------------------------------------------------
sliders <- list(
  cancelRate_slider <- sliderInput("cancelRate", "Cancel Rate:",
    min = 0, max = 100, value = 50, step = 1, post = "%"
  ),
  showRate_slider <- sliderInput("showRate", "Show Rate:",
    min = 0, max = 100, value = 50, step = 1, post = "%"
  ),
  testOppOnShow_slider <- sliderInput("testOppOnShow", "Test Opportunity on Show:",
    min = 0, max = 100, value = 50, step = 1, post = "%"
  )
)

tab_simula <- tabItem(
  tabName = "simula",
  box(width = 4, !!!sliders),
  box(width = 8, plotOutput("plot_funnelSimula"))
)
