
# This file contains all ploting and data visualisation code

# Displays the transition diagrams
plot(tmatrix.9H)
plot(tmatrix.everything)
plot(tmatrix.nothing)

# Plots the state counts by strategy (original and updated models)
plot(results.model)
plot(results.updated.model, type = "counts")

# Plots the state counts by state (original and updated models)
plot(results.model, type = "counts", panel = "by_state", free_y = TRUE) 
plot(results.updated.model, type = "counts", panel = "by_state", free_y = TRUE)


# provides the state counts and values
get_values(results.model)
get_counts(results.model)

get_values(results.updated.model)
get_counts(results.updated.model)


#

plot(res_mod, type = "values", panel = "by_strategy") +
    xlab("Time") +
    theme_bw() +
    scale_color_brewer(
    name = "State",
    palette = "Set1"
  )


plot(res_mod, type = "count", panel = "by_state") +
    xlab("Time") +
    theme_bw() +
    scale_color_brewer(
    name = "Strategy",
    palette = "Set1"
  )


ggplot(aust, aes(x = AGEP)) +
    geom_histogram(binwidth = 2)


