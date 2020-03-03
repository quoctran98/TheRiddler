for (i in 1:10) {
  ggplot(data.frame(time = simBarbers(i, simNum = 50000)), aes(x = time)) +
    geom_histogram(binwidth = 1) +
    labs(title = "Simulated Probability Density of Expected Wait Time",
         subtitle = paste("People ahead in line:",i)) +
    xlab("Wait Time (minutes)") +
    ylab("Probability Density") +
    xlim(c(0, 125)) +
    ylim(c(0, 3000)) +
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  ggsave(paste("anim/", i, ".png", sep = ""))
}