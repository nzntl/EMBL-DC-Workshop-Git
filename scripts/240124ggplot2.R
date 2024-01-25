plt <- ggplot(
  data = surveys_complete, 
  mapping = aes(x = weight, y = hindfoot_length)
)

plt
str(plt)

plt +
geom_point()

plt +
  geom_point() +
  ggtitle("my first plot!")

# 1. define ggplot object
# plt <-  ggplot(data = <data.frame>, mapping = <aestethics>)
# x/y/color/shap aestethics

# 2. add geometry layers
# geometry funktions have predictable names
# geom_{point, line, bar, histogram, violin, hex, ...}

plt <- ggplot(data=surveys_complete, mapping = aes(x=weight, y=hindfoot_length))+
  geom_point()

plt +
  ggtitle("weight vs hindfoot length")


install.packages("hexbin")
library(hexbin)
ggplot(data=surveys_complete, mapping = aes(x=weight, y=hindfoot_length))+
  geom_hex()


ggplot(data=surveys_complete, mapping = aes(x=weight, y=hindfoot_length))+
  geom_point(alpha = 0.1)

ggplot(data=surveys_complete, mapping = aes(x=weight, y=hindfoot_length))+
  geom_point(alpha = 0.1, color = "blue")

ggplot(data=surveys_complete, mapping = aes(x=weight, y=hindfoot_length))+
  geom_point(alpha = 0.1, aes(color = sex))

ggplot(data=surveys_complete, 
       mapping = aes(
         x=weight, 
         y=hindfoot_length, 
         color=species_id
  )
)+
  geom_point(alpha = 0.25)


# how to color NA in gender as well???
ggplot(data=surveys, mapping = aes(x=weight, y=hindfoot_length))+
  geom_point(alpha = 0.1, aes(color = ifelse(is.na(sex), "NA", sex)))

#levels(sex)[3] <- "undetermined"
#levels(sex)[1:2] <- c("female", "male")
#sex <- factor(sex, levels = c("undetermined", "female", "male"))
######

# challenge scatterplot weight vs secies_ide color by plot_type
ggplot(data=surveys_complete, 
       mapping = aes(
         x=species_id, 
         y=weight)
       )+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter (alpha = 0.3, color = "salmon")
  #geom_point(alpha = 0.1, aes(color = plot_type))+
  ggtitle("weight vs species_id")


  
  ggplot(data=surveys_complete, 
         mapping = aes(
           x=species_id, 
           y=weight)
  )+
    geom_jitter (alpha = 0.3, color = "salmon")+
    geom_boxplot(outlier.shape = NA, fill = NA)

  
  ggplot(data=surveys_complete, 
         mapping = aes(
           x=species_id, 
           y=weight)
  )+
    geom_violin ()
  
  
  ggplot(data=surveys_complete, 
         mapping = aes(
           x=species_id, 
           y=weight)
  )+
    geom_violin ()+
    scale_y_log10()+
    ylab("Weight (log10)")
  
# new box plot + jittered scatterplot hindfoot_length by species id. box in front of dots and filled with white :)
  ggplot(data=surveys_complete, 
         mapping = aes(
           x=species_id, 
           y=hindfoot_length)
  )+
    geom_jitter (alpha = 0.3, color = "firebrick")+
    geom_boxplot(fill = "white")
    
  
  
  ggplot(data=surveys_complete, 
         mapping = aes(
           x=species_id, 
           y=hindfoot_length)
  )+
    geom_jitter (alpha = 0.3, color = rgb(.1,.8,.4))+
    geom_boxplot(outlier.shape = NA)
  
  
  
  
  ggplot(data=surveys_complete, 
         mapping = aes(
           x=species_id, 
           y=hindfoot_length)
  )+
    geom_jitter (alpha = 0.3, color = "#dedede")+
    geom_boxplot(outlier.shape = NA)
  
  
  
  ggplot(data=surveys_complete, 
         mapping = aes(
           x=species_id, 
           y=hindfoot_length)
  )+
    geom_jitter (aes(color = plot_id))+
    geom_boxplot(outlier.shape = NA)
  
  
  ggplot(data=surveys_complete, 
         mapping = aes(
           x=species_id, 
           y=hindfoot_length)
  )+
    geom_jitter (aes(color = factor(plot_id)))+
    geom_boxplot(outlier.shape = NA)
  
####### yearly count genuses
  yearly_count <-  surveys_complete %>% 
    count(year, genus)
  
  ggplot(
    data = yearly_count, 
    mapping = aes(
      x= year, 
      y= n, 
      color = genus))+
    geom_line()+
    theme(panel.grid = element_blank())
  
  
  
  
  ggplot(
    data = yearly_count, 
    mapping = aes(
      x= year, 
      y= n, 
      shape = genus,
      group = genus))+
    geom_line()+
    geom_point()
  
  
  yearly_count %>% 
    ggplot(mapping = aes(
      x = year, 
      y=n, 
      color=genus))+
    geom_line()
  
yearly_count_graph <- surveys_complete %>% 
    count(year, genus) %>% 
    ggplot(mapping= aes(
      x=year, 
      y=n, 
      color=genus
    ))+
    geom_line()
    
  ggplot(
    data = yearly_count,
    mapping = aes(
      x=year, 
      y=n))+
    geom_line()+
    facet_wrap(facets=vars(genus))
  
  surveys_complete %>% 
    count(year, genus, sex) %>% 
    ggplot(
      mapping = aes(
        x=year, 
        y=n, 
        color=sex))+
    geom_line()+
    facet_wrap(facets=vars(genus))
  
  
  surveys_complete %>% 
    count(year, genus, sex) %>% 
    ggplot(
      mapping = aes(
        x=year, 
        y=n, 
        color=genus))+
    geom_line()+
    facet_wrap(facets=vars(sex))
  
  
  surveys_complete %>% 
    count(year, genus, sex) %>% 
    ggplot(
      mapping = aes(
        x=year, 
        y=n, 
        color=sex))+
    geom_line()+
    facet_grid(
      rows = vars(sex), 
      cols = vars(genus))
    
  
  surveys_complete %>% 
    count(year, genus, sex) %>% 
    ggplot(
      mapping = aes(
        x=year, 
        y=n, 
        color=sex))+
    geom_line()+
    facet_grid(
      rows = vars(genus), 
    )
  
  
  surveys_complete %>% 
    count(year, genus, sex) %>% 
    ggplot(
      mapping = aes(
        x=year, 
        y=n, 
        color=sex))+
    geom_line()+
    facet_grid(
      cols = vars(genus), 
    )
  
  
  
  
  
  plt <- surveys_complete %>% 
    count(year, genus, sex) %>% 
    ggplot(
      mapping = aes(
        x=year, 
        y=n, 
        color=sex))+
    geom_line()+
    facet_wrap(facets=vars(genus))+
    xlab("year of observation")+
    ylab("number of individuals")+
    ggtitle("observed general over time")+
    theme_bw(base_size = 14)+
    theme(
      legend.position = "bottom",
      aspect.ratio = 1, 
      axis.text.x = element_text(
        angle = 45,
        hjust = 1),
      panel.grid = element_blank()
    )
  plt
  ggsave(filename = "data/plot3.pdf", 
         plot = plt, 
         width = 10, 
         height = 10)
  
  
  
  # how to color NA in gender as well???
  #ggplot(data=surveys, mapping = aes(x=weight, y=hindfoot_length))+
  #  geom_point(alpha = 0.1, aes(color = ifelse(is.na(sex), "NA", sex)))
  
  #levels(sex)[3] <- "undetermined"
  #levels(sex)[1:2] <- c("female", "male")
  #sex <- factor(sex, levels = c("undetermined", "female", "male"))
  ######
  
  
  
  plt <- surveys %>% 
    count(year, genus, sex, na.rm = TRUE) %>% 
    ggplot(
      mapping = aes(
        x = year, 
        y = n, 
        color = ifelse(is.na(sex), "NA", sex)))+
    geom_line()+
    facet_wrap(facets=vars(genus))+
    xlab("year of observation")+
    ylab("number of individuals")+
    ggtitle("observed general over time")+
    theme_bw(base_size = 14)+
    theme(
      legend.position = "bottom",
      aspect.ratio = 1, 
      axis.text.x = element_text(
        angle = 45,
        hjust = 1),
      panel.grid = element_blank()
    )
  plt
  ggsave(filename = "data/plot4.pdf", 
         plot = plt, 
         width = 10, 
         height = 10)
  
  
  
  
  plt <- surveys_complete %>% 
    count(year, genus, sex) %>% 
    ggplot(
      mapping = aes(
        x=year, 
        y=n, 
        color=sex))+
    geom_line()+
    facet_wrap(facets=vars(genus))+
    scale_color_manual(
      values = c("tomato", "dodgerblue")    )+
    xlab("year of observation")+
    ylab("number of individuals")+
    ggtitle("observed general over time")+
    theme_bw(base_size = 14)+
    theme(
      legend.position = "bottom",
      aspect.ratio = 1, 
      axis.text.x = element_text(
        angle = 45,
        hjust = 1),
      plot.title = element_text(hjust = 0.5)
      )
      panel.grid = element_blank()
  plt
  ggsave(filename = "data/plot6.pdf", 
         plot = plt, 
         width = 10, 
         height = 10)
  
  
  
  plt <- surveys_complete %>% 
    count(year, genus, sex) %>% 
    ggplot(
      mapping = aes(
        x=year, 
        y=n, 
        color=sex))+
    geom_line()+
    facet_wrap(facets=vars(genus))+
    scale_color_manual(
      values = c("tomato", "dodgerblue"))+ 
    xlab("year of observation")+
    ylab("number of individuals")+
    ggtitle("observed general over time")+
    theme_bw(base_size = 14)+
    theme(
      legend.position = "none",
      aspect.ratio = 1, 
      axis.text.x = element_text(
        angle = 45,
        hjust = 1),
      plot.title = element_text(hjust = 0.5)
    )
  panel.grid = element_blank()
  plt
  ggsave(filename = "data/plot7.pdf", 
         plot = plt, 
         width = 10, 
         height = 10)
  
  
  
  
  plt <- surveys_complete %>% 
    count(year, genus, sex) %>% 
    ggplot(
      mapping = aes(
        x=year, 
        y=n, 
        color=sex))+
    geom_line()+
    facet_wrap(facets=vars(genus),
    scales = "free_y")+
    scale_color_manual(
      values = c("tomato", "dodgerblue"))+ 
    xlab("year of observation")+
    ylab("number of individuals")+
    ggtitle("observed general over time")+
    theme_bw(base_size = 14)+
    theme(
      legend.position = "none",
      aspect.ratio = 1, 
      axis.text.x = element_text(
        angle = 45,
        hjust = 1),
      plot.title = element_text(hjust = 0.5)
    )
  panel.grid = element_blank()
  plt
  ggsave(filename = "data/plot8.pdf", 
         plot = plt, 
         width = 10, 
         height = 10)