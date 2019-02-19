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


