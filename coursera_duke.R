install.packages("openintro")
install.packages("tidyverse")
library(openintro)

data(present)

library(tidyverse)
present <- present |> mutate (total = girls + boys, prop_boys = boys/(girls+boys))
present |> ggplot(aes(year, prop_boys)) + geom_line()

present <- present |> mutate (more_boys = boys > girls)
present$year[which.max(present$total)]
present|> ggplot(aes(year, total)) + geom_point()
