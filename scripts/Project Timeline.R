## ---------------------------
## Title: Project Timeline Visualisation
## Purpose: See ongoing and completed projects
## ---------------------------
## Author: Dan Fellowes
## Github: https://www.github.com/filmicaesthetic
## Date Created: 2022-08-18
## ---------------------------

## load required packages with pacman - p_load will load packages and install them if required

pacman::p_load(dplyr, ggplot2, ggforce, tidyr, showtext)

## ---------------------------

# opposite of %in% function
'%!in%' <- function(x,y)!('%in%'(x,y))

# Set the levels for each project based on lowest available position on the plot
plot_levels <- function(data, condensed = TRUE, start_date = "start_date", end_date = "end_date") {
  
  # add project id
  data$proj_id <- 1:nrow(data)
  
  # pass inputs as variable names
  start_name <- start_date
  start <- as.name(start_name)
  end_name <- end_date
  end <- as.name(end_name)

  # create empty calculation lists
  level_list <- c()
  free_levels <- c(seq(1:nrow(data)))
  temp_list <- data.frame("proj_id" = c(),
                          "level" = c())

  if (condensed == TRUE) {
    
      for (i in 1:nrow(data)) {
        
        # remove ended projects
        if (nrow(temp_list) > 0) {
          temp_list <- temp_list %>% filter(!!end > data[i, which(colnames(data) == start)])
        }
        
        free_levels <- c(seq(1:nrow(data))[seq(1:nrow(data)) %!in% temp_list$level])
        
        temp_it <- data.frame("proj_id" = data$proj_id[i],
                              "level" = min(free_levels))
        
        temp_it[, end_name] <- data[i, which(colnames(data) == end)]
        
        temp_list <- rbind(temp_list, temp_it)
        
        level_list <- c(level_list, min(free_levels))
        
      }
      
    } else {
      level_list <- 1:nrow(data)
    }
    
    
  data$level <- level_list
  
  return(data)
    
}

# create additional data to help with the shape of the bars
plot_prep <- function (data, start_date = "start_date", end_date = "end_date") {
  
  # pass inputs as variable names
  start_name <- start_date
  start <- as.name(start_name)
  end_name <- end_date
  end <- as.name(end_name)
  
  plot_df <- data %>%
    pivot_longer(cols = c(!!start, !!end), names_to = "date_type", values_to = "date")
  
  plot_df_plus <- plot_df %>%
    mutate(level = level + 0.5)
  
  plot_df <- rbind(plot_df, plot_df_plus)
  
  return(plot_df)
}

# create outline of points - in case of more than 4 points
find_outline <- function (data, project = "project", start_date = "start_date", end_date = "end_date") {

  # pass inputs as variable names
  start_name <- start_date
  start <- as.name(start_name)
  end_name <- end_date
  end <- as.name(end_name)
  proj_name <- project
  proj <- as.name(proj_name)

  find_hull <- function(df) df[chull(as.Date(df$date), df$level), ]
  library(plyr)
  hulls <- ddply(data, proj_name, find_hull)
  detach("package:plyr", unload=TRUE)

  return(hulls)

}

# create dummy dataset
project_start_end <- data.frame(project= c("Project 1", "Project 2", "Project 3", "Project 4", "Project 5", "Project 6", "Project 7", "Project 8", "Project 9", "Project 10", "Project 11", "Project 12"),
                                type = c("A", "B", "B", "A", "A", "B", "B", "B", "A", "A", "B", "A"),
                                start_date = c("2022-04-01", "2022-04-12", "2022-04-25", "2022-05-14", "2022-05-21", "2022-06-03", "2022-06-10", "2022-06-29", "2022-07-05", "2022-07-12", "2022-07-20", "2022-07-25"),
                                end_date = c("2022-05-01", "2022-07-02", "2022-05-18", "2022-07-23", "2022-07-13", "2022-06-24", "2022-07-10", "2022-08-20", "2022-09-01", "2022-09-01", "2022-07-25", "2022-08-21"))

# Plot setup

# create palette
pal <- c("A" = "#29d6a8",
         "B" = "#f7a411")

font_add_google("Quicksand", "Quicksand")
showtext_auto()
gfont <- "Quicksand"

# Condensed Plot

# create dataset with defined functions
level_df <- plot_levels(project_start_end, condensed = TRUE)
prep_df <- plot_prep(level_df)
plot_outlines <- find_outline(prep_df)

# create visualisation
data.frame(plot_outlines) %>%
  ggplot(aes(x = as.Date(date), y = level)) +
  geom_shape(aes(group = as.factor(project), fill = as.factor(type)), 
             radius = unit(5 / nrow(plot_outlines), 'cm'), alpha = 0.8) +
  labs(x="Date", fill = "Category") +
  scale_fill_manual(values = pal) +
  ggtitle("PROJECT TIMELINE") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#1d1d1d", color = "#1d1d1d"),
        panel.background = element_rect(fill = "#1d1d1d", color = "#1d1d1d"),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(margin = margin(b = 50, unit = "pt")),
        panel.grid = element_line(color = "#333333"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 100, margin = margin(t = 50, unit = "pt")),
        legend.position = "top",
        legend.box.margin = margin(b = 50, unit = "pt"),
        text = element_text(color = "#898989", family = gfont, size = 50)
  )

# save plot
ggsave("outputs/timeline_condensed.png", width = 10, height = 10)


# Non-condensed plot

# create dataset with defined functions
level_df <- plot_levels(project_start_end, condensed = FALSE)
prep_df <- plot_prep(level_df)
plot_outlines <- find_outline(prep_df)

# create visualisation
data.frame(plot_outlines) %>%
  ggplot(aes(x = as.Date(date), y = level)) +
  geom_shape(aes(group = as.factor(project), fill = as.factor(type)), 
             radius = unit(5 / nrow(plot_outlines), 'cm'), alpha = 0.8) +
  labs(x="Date", fill = "Category") +
  scale_fill_manual(values = pal) +
  ggtitle("PROJECT TIMELINE") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#1d1d1d", color = "#1d1d1d"),
        panel.background = element_rect(fill = "#1d1d1d", color = "#1d1d1d"),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(margin = margin(b = 50, unit = "pt")),
        panel.grid = element_line(color = "#333333"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 100, margin = margin(t = 50, unit = "pt")),
        legend.position = "top",
        legend.box.margin = margin(b = 50, unit = "pt"),
        text = element_text(color = "#898989", family = gfont, size = 50)
  )

# save plot
ggsave("outputs/timeline_non_condensed.png", width = 10, height = 10)
