# data exploration tab content

categorical_list <- function(data) {
  names(data)[sapply(data, is.factor)]
}

numerical_list <- function(data) {
  names(data)[!sapply(data, is.factor)]
}

# One-way and Two-way contingency tables
contingency_table <- function(data, ...) {
  # get ellipsis values
  params <- list(...)
  # get the ellipsis that match the names in the data
  params_match <- unlist(params[params %in% names(data)])
  if (length(params_match) == 0) {
    return(data.frame(Category = "All", Frequency = nrow(data)))
  }
  # create the contingency table
  table(data[params_match])
}
# contingency_table(data, "Gender")
# contingency_table(data,"Gender", "Operating.System")
# contingency_table(data,"Gender", "Operating.System", "User.Behavior.Class")

# might want some form of value subsetting? one is choosing levels, another is range of values

# Numerical summaries (means, medians, sds, etc.) for quantitative variables at levels of categorical variables
numerical_summaries <- function(data, ...) {
  # get ellipsis values
  params <- list(...)
  # get the ellipsis that match the names in the data
  params_match <- unlist(params[params %in% names(data)])
  # get quantitative list
  quant_list <- params_match[!params_match %in% names(data)[sapply(data, is.factor)]]
  # get categorical list
  cat_list <- params_match[params_match %in% names(data)[sapply(data, is.factor)]]

  # select by chosen data, group by categorical and then get summaries of numerical
  new_data <- data |>
    select(params_match) |>
    group_by(across(all_of(cat_list))) |>
    mutate(across((quant_list), list(mean = mean, median = median, sd = sd), .names = "{.col}_{.fn}"))
  return(new_data)
}

head(numerical_summaries(data, "Age", "Gender"), 10)
# head(numerical_summaries(data, "Age", "Gender", "User.Behavior.Class"), 10)


# plots
## at least 6
## At least four of these should display multivariate information via the type of graph, by utilizing coloring, grouping, etc.
## At least two plots should be a plot that we didn’t cover in class (say a heatmap or something like that - depends on your data - lots of good examples to consider here https://exts.ggplot2. tidyverse.org/gallery/)
## Some kind of faceting should be used somewhere.

library(tidyverse)

# boxplot
# 1 numerical, 1 categorical, another one for faceting
boxplot <- function(data, x_var=NULL, fill_var=NULL, facet_var=NULL, flip=FALSE, ...) {
  if (flip) {
    mapping <- list(y = sym(x_var))
  } else {
    mapping <- list(x = sym(x_var))
  }
  if (!is.null(fill_var)) {
    mapping$fill <- sym(fill_var)
  }
  plot <- ggplot(data) +
    geom_boxplot(mapping=do.call(aes, mapping), ...)

  if (!is.null(facet_var)) {
    plot <- plot + facet_wrap(~data[[facet_var]])
  }

  return (plot)
}
# boxplot(data, "Age", "User.Behavior.Class", "Gender")

# histogram
# 1 numerical, 1 categorical, another one for faceting
histogram <- function(data, x_var=NULL, fill_var=NULL, facet_var=NULL, ...) {
  mapping <- list(x = sym(x_var))
  if (!is.null(fill_var)) {
      mapping$fill <- sym(fill_var)
  }
  plot <- ggplot(data) +
    geom_histogram(mapping=do.call(aes, mapping), ...)

  if (!is.null(facet_var)) {
    plot <- plot + facet_wrap(~data[[facet_var]])
  }

  return (plot)
}
# Can easily see the cutoffs and counts, for example
# histogram(data, "Data.Usage..MB.day.", "User.Behavior.Class", "Gender", binwidth = 30)

# scatterplot
scatterplot <- function(data, x_var, y_var, color_var=NULL, facet_var=NULL, ...) {
  mapping <- list(x = sym(x_var), y = sym(y_var))
  if (!is.null(color_var)) {
    mapping$color <- sym(color_var)
  }
  plot <- ggplot(data) +
    geom_point(mapping=do.call(aes, mapping), ...)

  if (!is.null(facet_var)) {
    plot <- plot + facet_wrap(~data[[facet_var]])
  }

  return (plot)
}

# scatterplot(data, "Age", "Data.Usage..MB.day.", "User.Behavior.Class", "Gender")

# density plot
density_plot <- function(data, x_var, fill_var=NULL, facet_var=NULL, ...) {
  mapping <- list(x = sym(x_var))
  if (!is.null(fill_var)) {
    mapping$fill <- sym(fill_var)
  }
  plot <- ggplot(data) +
    geom_density(mapping=do.call(aes, mapping), ...)

  if (!is.null(facet_var)) {
    plot <- plot + facet_wrap(~data[[facet_var]])
  }

  return (plot)
}

# density_plot(data, "Data.Usage..MB.day.", "User.Behavior.Class", "Gender", alpha=.9)

library(ggradar)
library(scales)
# https://github.com/ricardo-bion/ggradar

# radar plot based on categorical variable and the remaining numeric variables
radar_plot <- function(data, cat_var) {
  # get the numerical variables
  num_vars <- numerical_list(data)
  # get the data for the radar plot
  new_data <- data |>
    select(cat_var, unlist(num_vars)) |>
    group_by(!!sym(cat_var)) |>
    rename(group = !!sym(cat_var)) |>
    summarise(across(all_of(num_vars), mean)) |>
    mutate(across(all_of(num_vars), ~ rescale(.x, to = c(0, 1), from = range(data[[cur_column()]]))))

  ggradar(new_data)
}
# radar_data <- radar_plot(data, "Operating.System")
# ggradar(radar_data)

# pairs(data |> select(where(is.numeric)))
pairs_plot <- function(data) {
  pairs(data |> select(where(is.numeric)))
}