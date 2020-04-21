
# ------------###  Functions for Regional Dashboard and Reports ### ----------------

##Graph 1a. Average Percent Correct By Standard##
regional_by_standards <- function(regional_data, tiles){
  regional_data %>%
    mutate(y_axis_2 = -0.125
    ) %>%
    ggplot() +
    geom_rect(data = tiles,
              aes(xmin = x1,
                  xmax = x2,
                  ymin = y1,
                  ymax = y2,
                  fill = fill_c), 
              alpha = 0.37) +
    geom_point(aes(x = pct_correct,
                   y = y_axis_1,
                   color = color_0),
               stat = "identity",
               size = 1,
               shape = 16) +
    scale_fill_manual("",
                      values =  c("#FF001A", #red
                                  "#FFBF42", #orange
                                  "#FEFE56", #yellow
                                  "#91FD57", #light green
                                  "#00CA3F")) +
    scale_color_manual("",
                       values = "black") +
    guides(fill = "none") +
    labs(x = "Average % Corect",
         y = ""
    ) +
    facet_wrap(~Standards, ncol = 1) +
    ggrepel::geom_text_repel(aes(x = pct_correct,
                                 y = y_axis_1,
                                 label = school),
                             force = .25,
                             size = 2.5) +
    ggrepel::geom_text_repel(aes(x = pct_correct,
                                 y = y_axis_2,
                                 label = label_0),
                             segment.alpha = 0,
                             size = 2.5,
                             force = .5
    ) +
    geom_vline(aes(xintercept = region,
                   linetype = line_0)) +
    scale_linetype_manual("",
                          values = 2) +
    scale_y_continuous(limits = c(-1,2)) +
    scale_x_continuous(breaks = seq(0, 100, 20),
                       limits = c(0,100),
                       minor_breaks = seq(10, 90, 20)) +
    theme_bw(base_family = "Calibri") + #adding font family
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          legend.position = "bottom")
}

##Graph 1b. Average Percent Correct By School##
stds_school_grade <- function(regional_data, tiles, ord_stds) {
  regional_data %>%
    ggplot() +
    geom_rect(data = tiles,
              aes(xmin = x1,
                  xmax = x2,
                  ymin = y1,
                  ymax = y2,
                  fill = fill_c
              ), alpha = 0.37) +
    scale_x_continuous(breaks = c(0, 20, 40, 70, 80, 100),
                       limits = c(0,100),
                       minor_breaks = NULL) +
    geom_segment(aes(x = pct_correct,
                     xend = region,
                     y = custom_code_2,
                     yend = custom_code_2,
                     linetype = shape_0)) +
    geom_point(data = regional_data,
               aes(x = pct_correct,
                   y = custom_code_2,
                   color = pct_correct < region),
               stat = "identity",
               size = 3#4
    ) +
    geom_point(aes(x = region,
                   y = custom_code_2,
                   shape = shape_0),
               size = 4#5
    ) +
    scale_fill_manual("",
                      values =  c("#FF001A", #red
                                  "#FFBF42", #orange
                                  "#FEFE56", #yellow
                                  "#91FD57", #light green
                                  "#00CA3F")) +
    scale_color_manual("",
                       values =  c("black",
                                   "red"),
                       labels = c("Above Average",
                                  "Below Average")) +
    scale_shape_manual("",
                       values = "|") +
    scale_linetype_manual("",
                          values = 1,
                          labels = "Difference Between School & Regional Average") +
    facet_wrap(~school, ncol = 1) +
    scale_y_continuous(breaks = c(1:nrow(ord_stds)), labels = ord_stds$Standards) +
    
    guides(fill = "none",
           color = "legend",
           shape = "legend") +
    labs(x = "Average % Correct",
         y = "Standards" 
    ) +
    theme_bw(base_family = "Calibri") +
    theme(legend.position = "bottom")
}

##Graph 2. Average Percent Correct Across Grades##
line_pct_correct <- function(pct_correct_data){
  ggplot(data = pct_correct_data) +
    geom_vline(aes(xintercept = reg_pct_correct,
                   linetype = line_0),
               color = "black") +
    geom_segment(aes(x = pct_correct, y = schools,
                     xend = reg_pct_correct,
                     yend = schools)) +
    geom_point(aes(x = pct_correct, y = schools,
                   color = dot_color_c)) +
    ggrepel::geom_text_repel(aes(x = pct_correct,
                                 y = schools,
                                 label = sprintf("%s%%", pct_correct)),
                             size = 5,  #changed from 5 to 4 for IA 2 PMM roll-up
                             nudge_y = -.25,
                             nudge_x = .25,
                             #force = .25, 
                             segment.alpha = 0) +
    facet_wrap(~grade_level, ncol = 1) +
    scale_color_manual("", breaks = c("black", "red"),
                       values = c("black", "red"),
                       labels = c("Above Average",
                                  "Below Average")) +
    scale_linetype_manual("",
                          values = 2) +
    theme_bw(base_family = "Calibri") +
    labs(x = "Avg % Correct",
         y = ""
    ) +
    theme(panel.grid.minor.x = element_blank(),
          legend.position = "bottom")
}

##Graph 3. Average Percent Correct - Homeroom Comparison##

home_room_avg <- function(homeroom_avg_data){
  ggplot(data = homeroom_avg_data %>%
           mutate(line_0 = "Grade Level Average")
  ) +
    geom_vline(aes(xintercept = grade_pct_correct,
                   linetype = line_0),
               color = "black") +
    geom_segment(aes(x = avg_pct_correct, 
                     y = home_room,
                     xend = grade_pct_correct,
                     yend = home_room)) +
    geom_point(aes(x = avg_pct_correct, 
                   y = home_room,
                   color = dot_color_c)) +
    ggrepel::geom_text_repel(aes(x = avg_pct_correct,
                                 y = home_room,
                                 label = sprintf("%s%%", avg_pct_correct)),
                             size = 5,
                             nudge_y = -.25,
                             nudge_x = .25,
                             segment.alpha = 0) +
    facet_wrap(grade_level~school, scales = "free_y", ncol = 1) +
    scale_color_manual("", 
                       breaks = c("black", "red"),
                       values = c("black", "red"),
                       labels = c("Above Average",
                                  "Below Average")) +
    scale_linetype_manual("",
                          values = 2) +
    theme_bw(base_family = "Calibri") +
    labs(x = "",
         y = "") +
    theme(panel.grid.major.x = element_blank(),
          legend.position = "bottom")
}


##Graph 4. Standards Mastered Grade Level View##
stds_mastered_grade_level <- function(grade_mastery_data, h_room){
  grade_mastery_data %>%
    dplyr::filter(home_room == h_room) %>%
    ggplot(aes(x = mastered_level,
               y = pct_rank),
           size = 1) + #.5
    geom_text(aes(label = firstlast,
                  color = mastered_level),
              size = 3) +
    # geom_text_repel(aes(label = firstlast,
    #               color = mastered_level),
    #               min.segment.length = unit(100, "lines"),
    #               force = .15,
    #               #nudge_y = 2
    #               size = 3) +
    facet_grid(custom_code ~ home_room, scales = "free_y") +
    #facet_wrap(~custom_code, ncol = 1) +
    scale_color_manual("",
                       
                       values =  c("Not Yet Mastered" = "#FF001A", #red
                                   "Partially" = "#FFBF42", #orange
                                   "Near Mastery" = "goldenrod",#FFDC00", #yellow
                                   "Mastered" = "#00CA3F"),
                       labels = c("Mastered: 70% and above Correct",
                                  "Near Mastery: 41-69% Correct",
                                  "Partially: 21-40% Correct",
                                  "Not Yet Mastered: < 21% Correct"
                       )) +
    theme_bw(base_family = "Calibri") +
    labs(x = "Mastery Type",
         y = "Count of Students"
    ) +
    theme(legend.position = "bottom")
}

##Graph 5. Standards Mastered Homeroom View##
student_mastery_homeroom <- function(stds_mastered, homeroom_mastery_data, order_db, colnames_db){ 
  stds_mastered %>%
    ungroup() %>%
    left_join(homeroom_mastery_data %>%
                
                ungroup(),
              by = c("student_id",
                     "firstlast")) %>%
    mutate(firstlast = factor(firstlast,
                              levels = order_db$firstlast),
           custom_code = factor(custom_code,
                                levels = colnames_db),
           mastered_level = factor(mastered_level, 
                                   levels = c("Mastered",
                                              "Near Mastery",
                                              "Partially",
                                              "Not Yet Mastered"))) %>%
    ggplot(aes(x = custom_code,
               y = forcats::fct_rev(firstlast))) +
    geom_tile(aes(fill = mastered_level),
              color = "white") +
    scale_fill_manual("",
                      values =  c("Not Yet Mastered" = "#FF001A", #red
                                  "Partially" = "#FFBF42", #orange
                                  "Near Mastery" = "#FEFE56", #yellow
                                  "Mastered" = "#00CA3F")) + #green) +
    labs(y = "",
         x = ""
    ) +
    theme(legend.position = "bottom",
          axis.text.x=element_text(angle=45,hjust=1),
          text = element_text(family = "Calibri")
    )
}