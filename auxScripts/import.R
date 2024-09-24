

# Libraries ---------------------------------------------------------------
library(readxl)
library(dplyr)
library(stringr)


# Paths -------------------------------------------------------------------
pathBase <- "C://Users//frgarcia//OneDrive - Amplifon S.p.A//General//"
pathPlantilla <- "01. Reporting//Estructura Reg-Area-tienda//"
pathKPI <- "//04. Personal//Ferran Garcia//ShinyApps//agendaRutineApp//data//"


# Import ------------------------------------------------------------------
plantilla <- readxl::read_xlsx(
  path = paste0(pathBase, pathPlantilla, "Plantilla Reg-area-codigo.xlsx"),
  sheet = "Tiendas",
  skip = 1,
  col_names = c("region", "areaCode", "area", "shop", "shopCode", 
                "atg1", "sym", "wave", "active", "new", "codeBI", 
                "type", "areaNew", "AM", "nada", "atg2", rep("nah", 4))
) |> select(region, area, shop) |> 
  mutate(
    region = str_extract(region, "\\d \\w+"),
    area = str_extract(area, "A\\d{2} \\w+")
  )

qKPI_month <- readxl::read_xlsx(
  path = paste0(pathBase, pathKPI, "dataAH.xlsx"),
  sheet = "qKPI_month",
  na = "N/A N/A",
  skip = 1,
  col_names = c("shop", "year", "wd", "gross", "cancel", 
                "net", "complet", "valid", "opp", "sales")
) |> select(-sales)

qKPI_history <- readxl::read_xlsx(
  path = paste0(pathBase, pathKPI, "dataAH.xlsx"),
  sheet = "qKPI_history",
  na = "N/A N/A",
  skip = 1,
  col_names = c("shop", "week", "gross", "cancel", 
                "net", "complet", "valid", "opp", "sales")
) |> select(-sales)

# Clean -------------------------------------------------------------------
qKPI_month <- inner_join(qKPI_month, plantilla, join_by(shop)) |> 
  select(region, area, shop, year, wd, everything()) |> 
  mutate(
    region = str_extract(region, "\\d \\w+"),
    area = str_extract(area, "A\\d{2} \\w+")
  ) |> 
  mutate(across(
    .cols = gross:opp,
    .fns = ~ ifelse(is.na(.x), 0, .x)
  ))

qKPI_history <- inner_join(qKPI_history, plantilla, join_by(shop)) |> 
  select(region, area, shop, week, everything()) |> 
  mutate(
    region = str_extract(region, "\\d \\w+"),
    area = str_extract(area, "A\\d{2} \\w+")
  ) |> 
  mutate(across(
    .cols = gross:opp,
    .fns = ~ ifelse(is.na(.x), 0, .x)
  ))


# Calculate KPI -----------------------------------------------------------
dKPI_month <- list(
  spain = qKPI_month |> 
    group_by(year, wd) |> 
    summarise(across(
      .cols = gross:opp,
      .fns = ~ sum(.x)
    )) |> 
    mutate(
      cancelRate = ifelse(gross == 0, 0, cancel / gross),
      showRate = ifelse(net == 0, 0, complet / net),
      testOnShow = ifelse(complet == 0, 0, valid / complet),
      testOppOnShow = ifelse(complet == 0, 0, opp / complet),
      app2Test = ifelse(gross == 0, 0, opp / gross),
      .keep = "unused"
    ),
  region = qKPI_month |> 
    group_by(region, year, wd) |> 
    summarise(across(
      .cols = gross:opp,
      .fns = ~ sum(.x)
    )) |> 
    mutate(
      cancelRate = ifelse(gross == 0, 0, cancel / gross),
      showRate = ifelse(net == 0, 0, complet / net),
      testOnShow = ifelse(complet == 0, 0, valid / complet),
      testOppOnShow = ifelse(complet == 0, 0, opp / complet),
      app2Test = ifelse(gross == 0, 0, opp / gross),
      .keep = "unused"
    ),
  area = qKPI_month |> 
    group_by(region, area, year, wd) |> 
    summarise(across(
      .cols = gross:opp,
      .fns = ~ sum(.x)
    )) |> 
    mutate(
      cancelRate = ifelse(gross == 0, 0, cancel / gross),
      showRate = ifelse(net == 0, 0, complet / net),
      testOnShow = ifelse(complet == 0, 0, valid / complet),
      testOppOnShow = ifelse(complet == 0, 0, opp / complet),
      app2Test = ifelse(gross == 0, 0, opp / gross),
      .keep = "unused"
    ),
  shop = qKPI_month |> 
    group_by(region, area, shop, year, wd) |> 
    summarise(across(
      .cols = gross:opp,
      .fns = ~ sum(.x)
    )) |> 
    mutate(
      cancelRate = ifelse(gross == 0, 0, cancel / gross),
      showRate = ifelse(net == 0, 0, complet / net),
      testOnShow = ifelse(complet == 0, 0, valid / complet),
      testOppOnShow = ifelse(complet == 0, 0, opp / complet),
      app2Test = ifelse(gross == 0, 0, opp / gross),
      .keep = "unused"
    )
)

dKPI_M2D <- list(
  spain = qKPI_month |> 
    group_by(year, wd) |>
    summarise(across(
      .cols = gross:opp,
      .fns = ~ sum(.x)
    )) |> 
    group_by(year) |> 
    arrange(wd, .by_group = TRUE) |> 
    mutate(across(
      .cols = gross:opp,
      .fns = cumsum
    )) |> 
    mutate(
      cancelRate = ifelse(gross == 0, 0, cancel / gross),
      showRate = ifelse(net == 0, 0, complet / net),
      testOnShow = ifelse(complet == 0, 0, valid / complet),
      testOppOnShow = ifelse(complet == 0, 0, opp / complet),
      app2Test = ifelse(gross == 0, 0, opp / gross),
      .keep = "unused"
    ),
  region = qKPI_month |> 
    group_by(region, year, wd) |>
    summarise(across(
      .cols = gross:opp,
      .fns = ~ sum(.x)
    )) |> 
    group_by(region, year) |> 
    arrange(wd, .by_group = TRUE) |> 
    mutate(across(
      .cols = gross:opp,
      .fns = cumsum
    )) |> 
    mutate(
      cancelRate = ifelse(gross == 0, 0, cancel / gross),
      showRate = ifelse(net == 0, 0, complet / net),
      testOnShow = ifelse(complet == 0, 0, valid / complet),
      testOppOnShow = ifelse(complet == 0, 0, opp / complet),
      app2Test = ifelse(gross == 0, 0, opp / gross),
      .keep = "unused"
    ),
  area = qKPI_month |> 
    group_by(region, area, year, wd) |>
    summarise(across(
      .cols = gross:opp,
      .fns = ~ sum(.x)
    )) |> 
    group_by(region, area, year) |> 
    arrange(wd, .by_group = TRUE) |> 
    mutate(across(
      .cols = gross:opp,
      .fns = cumsum
    )) |> 
    mutate(
      cancelRate = ifelse(gross == 0, 0, cancel / gross),
      showRate = ifelse(net == 0, 0, complet / net),
      testOnShow = ifelse(complet == 0, 0, valid / complet),
      testOppOnShow = ifelse(complet == 0, 0, opp / complet),
      app2Test = ifelse(gross == 0, 0, opp / gross),
      .keep = "unused"
    ),
  shop = qKPI_month |> 
    group_by(region, area, year, shop, wd) |>
    summarise(across(
      .cols = gross:opp,
      .fns = ~ sum(.x)
    )) |> 
    group_by(region, area, shop, year) |> 
    arrange(wd, .by_group = TRUE) |> 
    mutate(across(
      .cols = gross:opp,
      .fns = cumsum
    )) |> 
    mutate(
      cancelRate = ifelse(gross == 0, 0, cancel / gross),
      showRate = ifelse(net == 0, 0, complet / net),
      testOnShow = ifelse(complet == 0, 0, valid / complet),
      testOppOnShow = ifelse(complet == 0, 0, opp / complet),
      app2Test = ifelse(gross == 0, 0, opp / gross),
      .keep = "unused"
    )
)

dKPI_history <- list(
  spain = qKPI_history |> 
    group_by(week) |> 
    summarise(across(
      .cols = gross:opp,
      .fns = ~ sum(.x)
    )) |> 
    mutate(
      cancelRate = ifelse(gross == 0, 0, cancel / gross),
      showRate = ifelse(net == 0, 0, complet / net),
      testOnShow = ifelse(complet == 0, 0, valid / complet),
      testOppOnShow = ifelse(complet == 0, 0, opp / complet),
      app2Test = ifelse(gross == 0, 0, opp / gross),
      .keep = "unused"
    ),
  region = qKPI_history |> 
    group_by(region, week) |> 
    summarise(across(
      .cols = gross:opp,
      .fns = ~ sum(.x)
    )) |> 
    mutate(
      cancelRate = ifelse(gross == 0, 0, cancel / gross),
      showRate = ifelse(net == 0, 0, complet / net),
      testOnShow = ifelse(complet == 0, 0, valid / complet),
      testOppOnShow = ifelse(complet == 0, 0, opp / complet),
      app2Test = ifelse(gross == 0, 0, opp / gross),
      .keep = "unused"
    ),
  area = qKPI_history |> 
    group_by(region, area, week) |> 
    summarise(across(
      .cols = gross:opp,
      .fns = ~ sum(.x)
    )) |> 
    mutate(
      cancelRate = ifelse(gross == 0, 0, cancel / gross),
      showRate = ifelse(net == 0, 0, complet / net),
      testOnShow = ifelse(complet == 0, 0, valid / complet),
      testOppOnShow = ifelse(complet == 0, 0, opp / complet),
      app2Test = ifelse(gross == 0, 0, opp / gross),
      .keep = "unused"
    ),
  shop = qKPI_history |> 
    group_by(region, area, shop, week) |> 
    summarise(across(
      .cols = gross:opp,
      .fns = ~ sum(.x)
    )) |> 
    mutate(
      cancelRate = ifelse(gross == 0, 0, cancel / gross),
      showRate = ifelse(net == 0, 0, complet / net),
      testOnShow = ifelse(complet == 0, 0, valid / complet),
      testOppOnShow = ifelse(complet == 0, 0, opp / complet),
      app2Test = ifelse(gross == 0, 0, opp / gross),
      .keep = "unused"
    )
)


# Variables ---------------------------------------------------------------
variables <- list(
  regions = sort(unique(plantilla$region)),
  areas = setNames(plantilla$area, plantilla$region) %>%
    .[!duplicated(.)] |> sort(),
  shops = setNames(plantilla$shop, plantilla$area) |> 
    sort(),
  actualWeek = max(qKPI_history$week)
)


