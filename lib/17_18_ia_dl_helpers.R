##Helpers - Diverse Learners##
##Functions for IA graphics##

##Helpers##
##Functions for IA graphics##

##Table 1. Grade Level View##
dt_grade_level_view_dl <- function(grade_data){
  c_names <- colnames(grade_data)
  grade_data %>%
    DT::datatable(rownames = FALSE,
                  # width = "40%", 
                  options = list(dom = "t",
                                 bSort = FALSE,
                                 autoWidth = FALSE#,
                                 # columnDefs = list(
                                 #   list(width = '70px', 
                                 #        targets = c_names[-1]))
                  )) %>%
    formatStyle(c_names[-1],
                background = styleInterval(c(.20, .41, .6999, .81),
                                           c("#FF001A", #red
                                             "#FFBF42", #orange
                                             "#FEFE56", #yellow
                                             "#91FD57", #light green
                                             "#00CA3F") #green
                ))%>%
    formatStyle(c_names,
                `font-size` = "16px") %>%
    formatPercentage(c_names[-1], 2)
}


##Table 2. Standards View##
dt_stds_view_dl <- function(stds_data) {
  c_names <- colnames(stds_data)
  stds_data %>%
    DT::datatable(rownames = FALSE,
                  width = "400px",
                  height = "300px",
                  options = list(
                    pageLength = nrow(stds_data),
                    dom = "t",
                    bSort = FALSE,
                    autoWidth = TRUE,
                    columnDefs = list(list(
                      width = '100px', 
                      targets = c_names))
                  )) %>%
    formatStyle(c_names[-c(1,2)],
                background = styleInterval(c(.20, .41, .6999, .81),
                                           c(rgb(1, 0, 26/255), 
                                             #"#FF001A", #red
                                             rgb(1,191/255,66/255),
                                             #"#FFBF42", #orange
                                             rgb(254/255,254/255,86/255),
                                             #"#FEFE56", #yellow
                                             rgb(145/255,253/255,87/255),
                                             #"#91FD57", #light green
                                             rgb(0,202/255,63/255))
                                           #"#00CA3F") #green
                )
    ) %>%
    formatStyle(c_names,
                `font-size` = c('18px')
    ) %>%
    formatPercentage(c_names[-c(1,2)], 2)
}

##Table 3a Not Yet Mastered##
not_yet_mastered <- function(bottom_band) {
  c_names <- colnames(bottom_band)
  bottom_band %>%
    DT::datatable(rownames = FALSE,
                  # width = "400px",
                  # height = "300px",
                  options = list(
                    pageLength = nrow(bottom_band),
                    dom = "t",
                    bSort = FALSE,
                    autoWidth = TRUE,
                    columnDefs = list(list(
                      width = '100px', 
                      targets = c_names))
                  )) %>%
    formatStyle(c_names,
                `font-size` = c('18px')
    ) %>%
    formatPercentage(c_names[-1], 2) %>%
    formatStyle(c_names, `font-family` = "Calibri") %>% 
    formatStyle(c_names[-1], 
                color = styleEqual(0, rgb(0,202/255,63/255))
    )
}

##Table 3b. Mastery##
mastered <- function(mastered_band) {
  c_names <- colnames(mastered_band)
  mastered_band %>%
    DT::datatable(rownames = FALSE,
                  #width = "400px",
                  #height = "300px",
                  options = list(
                    pageLength = nrow(mastered_band),
                    dom = "t",
                    bSort = FALSE,
                    autoWidth = TRUE,
                    columnDefs = list(list(
                      width = '100px', 
                      targets = c_names))
                  )) %>%
    formatStyle(c_names,
                `font-size` = c('18px')
    ) %>%
    formatPercentage(c_names[-1], 2) %>%
    formatStyle(c_names, `font-family` = "Calibri") %>% 
    formatStyle(c_names[-1], 
                background = styleInterval(c(.20, .41, .6999, .81),
                                           c(rgb(1, 0, 26/255), 
                                             #"#FF001A", #red
                                             rgb(1,191/255,66/255),
                                             #"#FFBF42", #orange
                                             rgb(254/255,254/255,86/255),
                                             #"#FEFE56", #yellow
                                             rgb(145/255,253/255,87/255),
                                             #"#91FD57", #light green
                                             rgb(0,202/255,63/255)))
    )
}

##Graph 1. Standards Mastered Grade Level View##
stds_mastered_grade_level_dl <- function(grade_mastery_data){
  grade_mastery_data %>%
    #dplyr::filter(home_room == h_room) %>%
    ggplot(aes(x = mastered_level,
               y = pct_rank),
           size = 1) +
    geom_text(aes(label = firstlast,
                  color = mastered_level),
              size = 3) +
    facet_grid(custom_code ~ teacher_name,
               scales = "free_y") +
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
student_mastery_homeroom <- function(stds_mastered, homeroom_mastery_data, order_names, col_order){ 
  stds_mastered %>%
    ungroup() %>%
    left_join(homeroom_mastery_data %>%
                
                ungroup(),
              by = c("student_id",
                     "firstlast")) %>%
    mutate(firstlast = factor(firstlast,
                              levels = order_names$firstlast),
           custom_code = factor(custom_code,
                                levels = col_order),
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
          axis.text.x=element_text(angle=45,hjust=1))
}

##for KAMS IA Report ELA G5##
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
                       labels = c("Mastered: > 69% Correct",
                                  "Near Mastery: 41-69% Correct",
                                  "Partially: 21-40% Correct",
                                  "Not Yet Mastered: < 21% Correct"
                       )) +
    theme_bw() +
    labs(x = "Mastery Type",
         y = "Count of Students"
    ) +
    theme(legend.position = "bottom")
}
