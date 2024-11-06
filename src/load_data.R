# load and/or process data in a particular way
library("dplyr")

# load csv
data <- read.csv("data/user_behavior_dataset.csv")

# process data

# factors Device.Model, Operating.System, Gender
# User.Behavior.Class is likert scale
data <- data |>
  mutate(across(where(is.character), as.factor),
         User.Behavior.Class = as.factor(User.Behavior.Class)) |>
  select(-User.ID)

# str(data)
