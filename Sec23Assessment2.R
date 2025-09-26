library(HistData)
data("GaltonFamilies")
set.seed(1)

## the purpose of this seems to be tp 
galton_heights <- GaltonFamilies |>
  group_by(family, gender) |>
  sample_n(1) |>
  ungroup()

cors <- galton_heights |> 
  pivot_longer(father:mother, names_to = "parent", values_to = "parentHeight") |>
  mutate(child = ifelse(gender == "female", "daughter", "son")) |>
  unite(pair, c("parent", "child")) |> 
  group_by(pair) |>
  summarize(cor = cor(parentHeight, childHeight))

####################################


