calculaKPI <- function(data, groups){
  data |> 
    group_by(select(data, matches(groups))) |> 
    summarise(across(
      .cols = app:sales,
      .fns = ~ sum(.x)
    )) |> 
    mutate(
      cancelRate = cancel / (app + cancel),
      showRate = show / app,
      testShow = valid / show,
      testOppShow = opp / show,
      test2Sales = sales / valid
    ) |> 
    select(matches(paste(groups, "*Rate|test*", sep = "|"))) |> 
    pivot_longer(cancelRate:test2Sales, names_to = "kpi")
}
