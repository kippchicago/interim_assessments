##IA script for comparing 1718 and 1819 IAs

library(dplyr)
library(silounloadr)
library(readr)
library(lubridate)
library(purrr)
library(stringr)
library(forcats)
library(ggplot2)
library(gridExtra)
library(googledrive)
library(googlesheets4)
library(here)
library(kippcolors)

schools <- data_frame(schoolid = c(78102, 7810, 400146,4001462, 400163, 4001802, 400180),
                      school_name = c("Ascend Primary", "Ascend Middle", "Academy", "Academy Primary", "Bloom", "One Primary", "One Academy"),
                      school_abbrev =c("KAP", "KAMS", "KAC", "KACP", "KBCP", "KOP", "KOA"))

drop_fivetran_cols <- . %>% select(-starts_with("_fivetran"))


ia_1_1718 <- c(34901, 34904, 34903, 34902, #ELA 5-8
               34906, 34907, 34909, 34908) #Math 5-8

ia_2_1718 <- c(40828, 40827, 40831, 40830, #ELA 5-8
               40826, 40835, 40836, 40837) #Math 5-8

ia_3_1718 <- c(45669, 45668, 45666, 45664, #ELA 5-8
               45663, 45662, 45665, 45667) #Math 5-8

ia_1_1819_assess_ids <- c(70732, 70733, 70504, 70962, #math
                          70735, 70965, 70972, 70966, #ela
                          61095, 61096, 70734, 70731, #science
                          70971, 71052, #math 4th, 3rd
                          71184, 71256) #ela 3rd, 4th 

ia_2_1819_assess_ids <- c(72861, 73363, 73307, 72868, #math
                          73667, 73981, 72849, 72850, #ela
                          72937, 73375, 73361, 73270, #science
                          73527, 73388, #math 4th, 3rd
                          73380, 73386) #ela 3rd, 4th

ia_3_1819_assess_ids <-  c(75169, 75184, 75108, 75194,
                           75484, 75482, 75604, 75514,
                           75500, 75501, 75483, 75428,
                           75558, 75196, 75625, 75198)

dl_ia_1_1718 <- c(36191,#5-8 Eureka Bridge
                  36194, #7-8 ELA
                  36193, #5-6 ELA
                  34906 #G5 GenEd
)

dl_ia_2_1718 <- c(40808, #5-8 DL foundations math
                  40870, #5-8 Eureka Bridge
                  40834, #7-8 ELA 
                  40844 #5-6 ELA
)


dl_ia_3_1718 <- c(45687, #5-6 ELA
                  45851, #5-6 ELA MO
                  45857, #G6 DL KOA
                  45688, #7-8 ELA
                  45844, #7-8 ELA MO
                  45685, #5-8 Eureka Bridge
                  45684 #5-8 Foundations Math
)

dl_ia_1_1819 <- c(71263, #17-18 5th IA 1
                  71230, #17-18 5th IA 2,
                  71217, #17-18 5th IA 3,
                  71264, #17-18 6th IA 1,
                  71318, #17-18 6th IA 3,
                  71265, #17-18 7th IA 1,
                  70974, #<160s RCNS,
                  70976, #170s RNCS,
                  71317, #180s RCNS,
                  70979, #190s RCNS,
                  70981, #200s RCNS,
                  70984, #5-6 ELA,
                  70983  #7-8 ELA
)

dl_ia_2_1819 <- c(71263, #17-18 5th IA 1
                  71230, #17-18 5th IA 2
                  71217, #17-18 5th IA 3
                  71264, #17-18 6th IA 1
                  71318, #17-18 6th IA 3
                  70976, #RIT 170s RCNS
                  70977, #RIT 180s GSP
                  70980, #RIT 190s GSP
                  70973, #RIT 160s GSP
                  70981, #RIT 200s RCNS
                  70979, #RIT 190s RCNS
                  71317, #180s RCNS
                  72914, #5-6 ELA
                  71647 #7-8 ELA
)

dl_ia_3_1819 <- c(71217, #17-18 5th IA 3
                  71264, #17-18 6th IA 1 -
                  70976, #RIT 170s RCNS -
                  70977, #RIT 180s GSP -
                  70980, #RIT 190s GSP -
                  70982, #RIT 200s GSP -
                  70973, #RIT 160s GSP -
                  70981, #RIT 200s RCNS - 
                  70979, #RIT 190s RCNS -
                  71317, #180s RCNS - 
                  70974, #160s RCNS - 
                  74070, #5-6 ELA
                  74067 #7-8 ELA
)

con <- dbplyr::simulate_dbi()
assessments_1920 <- get_illuminate("assessments") %>% 
  filter(dbplyr::build_sql("REGEXP_CONTAINS(local_assessment_id,'IA.1920.(0[3-8]).(ELA|Math|Science).1')", con = con)) %>% 
  collect()

ia_1920_gened <- assessments_1920 %>% 
  filter(!grepl("DL", local_assessment_id))

ia_1920_dl <- assessments_1920 %>% 
  filter(grepl("DL", local_assessment_id))

all_ias <- c(ia_1_1718, 
             ia_2_1718,
             ia_3_1718,
             ia_1_1819_assess_ids,
             ia_2_1819_assess_ids,
             ia_3_1819_assess_ids,
             ia_1920_gened$assessment_id)

dl_ids <- c(dl_ia_1_1718,
            dl_ia_2_1718,
            dl_ia_3_1718,
            dl_ia_1_1819,
            dl_ia_2_1819,
            dl_ia_3_1819,
            ia_1920_dl$assessment_id)

dl_gen_ed <- c(dl_ids,
               all_ias)



assessments <- get_illuminate("assessments") %>% 
  filter(assessment_id %in% dl_gen_ed) %>% 
  collect()

agg_student_responses_table <- get_illuminate("agg_student_responses") %>%
  dplyr::filter(assessment_id %in% dl_gen_ed) %>%
  collect(n = Inf)

agg_student_responses <- agg_student_responses_table %>%
  group_by(student_assessment_id, 
           student_id, 
           assessment_id) %>% 
  dplyr::filter(`_fivetran_synced` == max(`_fivetran_synced`)) %>%
  drop_fivetran_cols() %>%
  ungroup()

archived_assess <- get_illuminate("students_assessments_archive") %>% 
  collect()

agg_student_responses_standard_table <- get_illuminate("agg_student_responses_standard") %>%
  dplyr::filter(assessment_id %in% dl_gen_ed) %>%
  collect()

agg_student_responses_standard <- agg_student_responses_standard_table %>%
  group_by(student_assessment_id, 
           student_id, 
           assessment_id, 
           standard_id) %>% 
  dplyr::filter(`_fivetran_synced` == max(`_fivetran_synced`)) %>%
  drop_fivetran_cols() %>%
  ungroup()

standards <- get_illuminate("standards", schema = "standards") %>%
  drop_fivetran_cols() %>%
  collect()

students <- get_illuminate("students", "public") %>%
  select(student_id,
         local_student_id) %>%
  collect()

ps_stus <- get_powerschool("students") %>% 
  drop_fivetran_cols %>%
  select(
    studentid = id,
    first_name,
    last_name,
    grade_level,
    schoolid,
    home_room,
    student_number) %>%
  collect() #%>% 
#mutate(home_room = if_else(student_number %in% 50140305, "7th LSU", home_room))

#ttt <- gs_auth()

get_dl_student_list <- drive_find("Student Rostering for Interim Assessments", n_max = 30)
dl_g5_math <- read_sheet(get_dl_student_list$id, sheet = 3)

dl_g5_st_ids <- dl_g5_math %>% 
  janitor::clean_names() %>% 
  select(student_number = id)

asr_assessments <- 
  agg_student_responses %>% 
  inner_join(assessments %>% 
               drop_fivetran_cols,
             by = "assessment_id") %>%
  dplyr::filter(!student_assessment_id %in% archived_assess$student_assessment_id) %>% 
  left_join(students,
            by = c("student_id")) %>% 
  mutate(sy = stringr::str_extract(local_assessment_id, "1718|1819|1920"),
         dl_0 = grepl("DL", local_assessment_id),
         dl = if_else(local_student_id %in% dl_g5_st_ids$student_number &
                        local_assessment_id %in% "IA.1718.05.Math.1",
                      TRUE,
                      dl_0),
         ia_0 = if_else(sy %in% c("1718", "1920"), #this won't work for 1920 DL round 3 - keep in mind!
                        stringr::str_extract(local_assessment_id, "\\d$"),
                        NA_character_),
         ia_1 = if_else(is.na(ia_0) & sy %in% "1819" & date_taken <= ymd("2018-10-31"),
                        "1",
                        if_else(is.na(ia_0) & sy %in% "1819" & date_taken >= ymd("2019-01-14") & date_taken <= ymd("2019-01-30"),
                                "2", 
                                if_else(is.na(ia_0) & sy %in% "1819" & date_taken > ymd("2019-03-01") & date_taken < ymd("2019-04-10"),
                                        "3",
                                        ia_0))), 
         ia_dl_2 = if_else(sy %in% "1920",
                           str_extract(local_assessment_id, "\\d.DL|\\dDL"),
                           NA_character_),
         ia = if_else(sy %in% "1920" & !is.na(ia_dl_2),
                      str_extract(ia_dl_2, "\\d"),
                      ia_1),
         subject = stringr::str_extract(local_assessment_id, "ELA|Math|Science"),
         subject = if_else(is.na(subject), "Math", subject)) %>%
  filter(!is.na(ia)) %>% 
  select(-c(dl_0,
            ia_0,
            ia_1,
            ia_dl_2
  ))



assessment_ids <- asr_assessments %>%
  select(assessment_id,
         local_assessment_id) %>%
  unique()

assessment_stus <- asr_assessments %>%
  mutate(local_student_id = as.integer(local_student_id)) %>%
  left_join(ps_stus %>% 
              select(
                ps_studentid = studentid,
                first_name,
                last_name,
                #lastfirst,
                grade_level,
                schoolid,
                home_room,
                student_number),
            by=c("local_student_id" = "student_number")) %>% 
  #removing KAC 4th ELA 1 taken during IA 2
  filter(!(grepl("04.ELA.1", local_assessment_id) & schoolid == 400146),
         !(grepl("03.ELA.1", local_assessment_id) & schoolid == 400146),
         !(grepl("03.ELA.1", local_assessment_id) & schoolid == 4001462))

###UPDATE YEARS
sy_1718 <- calc_academic_year(today() - years(2), "first_year") #increase number next year

yearid_1718 <- calc_ps_termid(sy_1718)

sy_1819 <- calc_academic_year(today() - years(1), "first_year") #increase number next year

yearid_1819 <- calc_ps_termid(sy_1819)

year_ids <- c(yearid_1718,yearid_1819)/100

sy_1920 <- calc_academic_year(today(), "first_year") #increase number next year

yearid_1920 <- calc_ps_termid(sy_1920)

year_ids <- c(yearid_1718,yearid_1819, yearid_1920)/100

enrollment <- get_powerschool("ps_enrollment_all") %>%
  filter(yearid %in% year_ids) %>% 
  select(studentid,
         schoolid,
         entrydate,
         exitdate,
         grade_level) %>% 
  collect()

drop_duplicate_enrollments <- function(s_y){
  assessment_stus %>% 
    filter(sy == s_y) %>% 
    select(-schoolid,
           -grade_level) %>% 
    left_join(enrollment,
              by = c("ps_studentid" = "studentid")) %>% 
    mutate(keep = date_taken >= entrydate & 
             date_taken < exitdate) %>% 
    filter(keep)
}  

assessment_stus_rev <- drop_duplicate_enrollments("1718") %>% 
  bind_rows(drop_duplicate_enrollments("1819")) %>% 
  bind_rows(drop_duplicate_enrollments("1920"))


mastery_pct_correct <- assessment_stus_rev %>% 
  filter(!is.na(ia)) %>% 
  mutate(pct_correct = points/points_possible,
         mastered_level = if_else(pct_correct < .21, 
                                  "Not Yet Mastered",
                                  if_else(pct_correct < .41 &
                                            pct_correct >= .21,
                                          "Partially",
                                          if_else(pct_correct < .70 &
                                                    pct_correct >= .41,
                                                  "Near Mastery",
                                                  if_else(pct_correct >= .70, 
                                                          "Mastered", "NA"))))) %>% 
  left_join(schools %>% 
              select(-school_name),
            by = "schoolid")
#Removing 3rd and 4th ELA scores for KAC (KAC/KACP 4th graders took 03.ELA.1 for IA 1 and 04.ELA.1 for IA 2)
#Removing 8th Math scores for KOA (1819 7th graders took 1819 8th IAs)
#BY IA 3 you'll have 1 KOA cohort taking 8th IA 3 and two 7th cohorts taking 8th IA 1 & 2, so I added a grade_level filter
not_yet <- mastery_pct_correct %>% 
  filter(!(grepl("04.ELA.1", local_assessment_id) & schoolid == 400146 & grade_level == 4),
         !(grepl("03.ELA.1", local_assessment_id) & schoolid == 400146 & grade_level == 4),
         !(grepl("03.ELA.1", local_assessment_id) & schoolid == 4001462 & grade_level == 4),
         !(grepl("08.Math", local_assessment_id) & schoolid == 400180 & grade_level == 7)) %>% 
  select(local_assessment_id,
         sy,
         ia,
         dl,
         local_student_id,
         mastered_level) %>% 
  group_by(local_assessment_id,
           sy,
           ia,
           dl,
           mastered_level) %>% 
  count() %>% 
  group_by(local_assessment_id,
           sy,
           ia,
           dl) %>% 
  mutate(nn = sum(n)) %>% 
  rename(n_mastery = n,
         n_total = nn) %>% 
  mutate(pct_mastery = n_mastery/n_total)

science_1718_2 <- tribble(~"sy", ~"ia",~"subject", ~"school", ~"grade", ~"mastered_level", ~"n_mastery", ~"n_total",
                          "1718", "2", "Science", "KAC", "5", "Not Yet", 0, 84,
                          "1718", "2", "Science", "KAC", "5", "Mastered", 34, 84,
                          "1718", "2", "Science", "KAMS", "5", "Not Yet", 2, 87,
                          "1718", "2", "Science", "KAMS", "5", "Mastered", 23, 87,
                          "1718", "2", "Science", "KBCP", "5", "Not Yet", 2, 86,
                          "1718", "2", "Science", "KBCP", "5", "Mastered", 29, 86,
                          "1718", "2", "Science", "KOA", "5", "Not Yet", 0, 96,
                          "1718", "2", "Science", "KOA", "5", "Mastered", 50, 96,
                          "1718", "2", "Science", "KAMS", "6", "Not Yet", 5, 87,
                          "1718", "2", "Science", "KAMS", "6", "Mastered", 20, 87,
                          "1718", "2", "Science", "KAC", "6", "Not Yet", 4, 80,
                          "1718", "2", "Science", "KAC", "6", "Mastered", 21, 80,
                          "1718", "2", "Science", "KBCP", "6", "Not Yet", 0, 81,
                          "1718", "2", "Science", "KBCP", "6", "Mastered", 47, 81,
                          "1718", "2", "Science", "KOA", "6", "Not Yet", 5, 86,
                          "1718", "2", "Science", "KOA", "6", "Mastered", 29, 86,
                          "1718", "2", "Science", "KAMS", "7", "Not Yet", 3, 84,
                          "1718", "2", "Science", "KAMS", "7", "Mastered", 29, 84,
                          "1718", "2", "Science", "KAC", "7", "Not Yet", 1, 81,
                          "1718", "2", "Science", "KAC", "7", "Mastered", 42, 81,
                          "1718", "2", "Science", "KBCP", "7", "Not Yet", 2, 91,
                          "1718", "2", "Science", "KBCP", "7", "Mastered", 67, 91,
                          "1718", "2", "Science", "KBCP", "8", "Not Yet", 2, 77,
                          "1718", "2", "Science", "KBCP", "8", "Mastered", 33, 77,
                          "1718", "2", "Science", "KAC", "8", "Not Yet", 0, 79,
                          "1718", "2", "Science", "KAC", "8", "Mastered", 36, 79) %>% 
  mutate(local_assessment_id = sprintf("IA.%s.0%s.%s.%s", sy,grade,subject,ia))

science_1718_3 <- tribble(~"sy", ~"ia",~"subject", ~"school", ~"grade", ~"mastered_level", ~"n_mastery", ~"n_total",
                          "1718", "3", "Science", "KAC", "5", "Not Yet", 2, 81,
                          "1718", "3", "Science", "KAC", "5", "Mastered", 32, 81,
                          "1718", "3", "Science", "KAMS", "5", "Not Yet", 4, 86,
                          "1718", "3", "Science", "KAMS", "5", "Mastered", 35, 86,
                          "1718", "3", "Science", "KBCP", "5", "Not Yet", 3, 83,
                          "1718", "3", "Science", "KBCP", "5", "Mastered", 37, 83,
                          "1718", "3", "Science", "KOA", "5", "Not Yet", 3, 96,
                          "1718", "3", "Science", "KOA", "5", "Mastered", 55, 96,
                          "1718", "3", "Science", "KAMS", "6", "Not Yet", 3, 82,
                          "1718", "3", "Science", "KAMS", "6", "Mastered", 33, 82,
                          "1718", "3", "Science", "KAC", "6", "Not Yet", 4, 76,
                          "1718", "3", "Science", "KAC", "6", "Mastered", 35, 76,
                          "1718", "3", "Science", "KBCP", "6", "Not Yet", 9, 79,
                          "1718", "3", "Science", "KBCP", "6", "Mastered", 18, 79,
                          "1718", "3", "Science", "KOA", "6", "Not Yet", 0, 90,
                          "1718", "3", "Science", "KOA", "6", "Mastered", 47, 90,
                          "1718", "3", "Science", "KAMS", "7", "Not Yet", 3, 81,
                          "1718", "3", "Science", "KAMS", "7", "Mastered", 32, 81,
                          "1718", "3", "Science", "KAC", "7", "Not Yet", 1, 85,
                          "1718", "3", "Science", "KAC", "7", "Mastered", 61, 85,
                          "1718", "3", "Science", "KBCP", "7", "Not Yet", 8, 87,
                          "1718", "3", "Science", "KBCP", "7", "Mastered", 32, 87,
                          "1718", "3", "Science", "KBCP", "8", "Not Yet", 6, 78,
                          "1718", "3", "Science", "KBCP", "8", "Mastered", 23, 78,
                          "1718", "3", "Science", "KAC", "8", "Not Yet", 3, 80,
                          "1718", "3", "Science", "KAC", "8", "Mastered", 45, 80) %>% 
  mutate(local_assessment_id = sprintf("IA.%s.0%s.%s.%s", sy,grade,subject,ia))

science_1718 <- science_1718_2 %>% 
  bind_rows(science_1718_3)

regional_mastery <- mastery_pct_correct %>% 
  filter(!(grepl("04.ELA.1", local_assessment_id) & schoolid == 400146),
         !(grepl("03.ELA.1", local_assessment_id) & schoolid == 400146),
         !(grepl("03.ELA.1", local_assessment_id) & schoolid == 4001462 & grade_level == 4),
         !(grepl("08.Math", local_assessment_id) & schoolid == 400180 & grade_level == 7)) %>% 
  group_by(#local_assessment_id,
    ia,
    sy,
    subject,
    dl,
    grade_level) %>% 
  count(mastered_level) %>% 
  group_by(ia,
           sy,
           subject,
           dl,
           grade_level) %>% 
  mutate(nn = sum(n)) %>% 
  rename(n_mastery = n,
         n_total = nn) %>% 
  mutate(#subject = stringr::str_extract(local_assessment_id, "ELA|Math|Science"),
    #        grade = stringr::str_extract(local_assessment_id, "0\\d"),
    #        grade = gsub("0", "", grade),
    mastered_level = if_else(mastered_level == "Near Mastery", "Near",
                             if_else(mastered_level == "Not Yet Mastered", "Not Yet", mastered_level)),
    mastered_level = factor(mastered_level, levels = c("Not Yet",
                                                       "Partially",
                                                       "Near",
                                                       "Mastered")),
    pct_mastered = round(n_mastery/n_total *100,0),
    y_place = if_else(pct_mastered < 20, n_mastery+20, n_mastery/2),
    label_pct = sprintf("%s%%", pct_mastered),
    label_pct = sprintf("%s\n(%s)", label_pct, n_mastery)) 

# ids_ia_1_2 <-  mastery_pct_correct %>%
#   filter(!(grepl("04.ELA.1", local_assessment_id) & schoolid == 400146),
#          !(grepl("03.ELA.1", local_assessment_id) & schoolid == 400146),
#          !(grepl("03.ELA.1", local_assessment_id) & schoolid == 4001462 & grade_level == 4),
#          !(grepl("08.Math", local_assessment_id) & schoolid == 400180 & grade_level == 7)) %>% 
#   filter(sy == "1819",
#          ia == "1",
#          !dl) %>% #select(local_assessment_id) %>% unique()
#   select(local_student_id) %>% 
#   unique() %>% 
#   inner_join(mastery_pct_correct %>% 
#                filter(sy == "1819",
#                       ia == "2",
#                       !dl) %>% 
#                select(local_student_id) %>% 
#                unique()) %>% 
#   inner_join(mastery_pct_correct %>% 
#                filter(sy == "1819",
#                       ia == "3",
#                       !dl) %>% 
#                select(local_student_id) %>% 
#                unique())

ids_ia_1_2 <-  mastery_pct_correct %>%
  filter(!(grepl("04.ELA.1", local_assessment_id) & schoolid == 400146),
         !(grepl("03.ELA.1", local_assessment_id) & schoolid == 400146),
         !(grepl("03.ELA.1", local_assessment_id) & schoolid == 4001462 & grade_level == 4),
         !(grepl("08.Math", local_assessment_id) & schoolid == 400180 & grade_level == 7)) %>% 
  filter(sy == "1920",
         ia == "1",
         !dl) %>% #select(local_assessment_id) %>% unique()
  select(local_student_id) %>% 
  unique() 
##of the 1,348 students that took gen ed IA 1, 1,293 took IA 2
## of the 1,587 studens that took gen ed IA 1, 1,530 took IA 2, and 1, 467 took IA 3
same_students <- mastery_pct_correct %>% 
  filter(!(grepl("04.ELA.1", local_assessment_id) & schoolid == 400146),
         !(grepl("03.ELA.1", local_assessment_id) & schoolid == 400146),
         !(grepl("08.Math", local_assessment_id) & schoolid == 400180 & grade_level == 7)) %>% 
  filter(sy == "1819",
         local_student_id %in% ids_ia_1_2$local_student_id) %>% 
  group_by(local_assessment_id,
           ia,
           sy) %>% 
  count(mastered_level) %>% 
  mutate(n_mastered_level = n) %>% 
  add_tally(n_mastered_level) %>% 
  mutate(subject = stringr::str_extract(local_assessment_id, "ELA|Math|Science"),
         grade = stringr::str_extract(local_assessment_id, "0\\d"),
         grade = gsub("0", "", grade),
         mastered_level = if_else(mastered_level == "Near Mastery", "Near",
                                  if_else(mastered_level == "Not Yet Mastered", "Not Yet", mastered_level)),
         mastered_level = factor(mastered_level, levels = c("Not Yet",
                                                            "Partially",
                                                            "Near",
                                                            "Mastered")),
         pct_mastered = round(n_mastered_level/n *100,0),
         y_place = if_else(pct_mastered < 20, n_mastered_level+20, n_mastered_level/2),
         label_pct = sprintf("%s%%", pct_mastered)) 



assess_summary_school <- assessment_stus_rev %>%
  filter(!(grepl("04.ELA.1", local_assessment_id) & schoolid == 400146),
         !(grepl("03.ELA.1", local_assessment_id) & schoolid == 400146),
         !(grepl("03.ELA.1", local_assessment_id) & schoolid == 4001462 & grade_level == 4),
         !(grepl("08.Math", local_assessment_id) & schoolid == 400180 & grade_level == 7)) %>% 
  group_by(#local_assessment_id,
    #assessment_id,
    subject,
    schoolid, 
    grade_level,
    dl
    
  ) %>%  
  summarize(total_pts = sum(points_possible),
            pts_scored = sum(points),
            pct_correct = pts_scored/total_pts) %>%
  left_join(schools,
            by = "schoolid")

assess_summary_region <- assessment_stus_rev %>%
  filter(!(grepl("04.ELA.1", local_assessment_id) & schoolid == 400146),
         !(grepl("03.ELA.1", local_assessment_id) & schoolid == 400146),
         !(grepl("03.ELA.1", local_assessment_id) & schoolid == 4001462 & grade_level == 4),
         !(grepl("08.Math", local_assessment_id) & schoolid == 400180 & grade_level == 7),
         !is.na(ia)) %>%
  group_by(#local_assessment_id,
    #assessment_id,
    #title,
    sy,
    ia,
    dl,
    subject,
    grade_level
  ) %>%  
  summarize(total_pts = sum(points_possible),
            pts_scored = sum(points),
            reg_pct_correct = pts_scored/total_pts)

reg_1_2_compare <- assess_summary_region %>% 
  ungroup() %>% 
  bind_rows(tribble(~"local_assessment_id", ~"title", ~"ia", ~"sy", ~"reg_pct_correct",
                    "IA.1718.05.Science.2", "G5 Science", "2", "1718", .5972,
                    "IA.1718.06.Science.2", "G6 Science", "2", "1718", .6096,
                    "IA.1718.07.Science.2", "G7 Science", "2", "1718", .6813,
                    "IA.1718.08.Science.2", "G8 Science", "2", "1718", .6410,
                    "IA.1718.05.Science.3", "G5 Science", "3", "1718", .6370,
                    "IA.1718.06.Science.3", "G6 Science", "3", "1718", .6324,
                    "IA.1718.07.Science.3", "G7 Science", "3", "1718", .6722,
                    "IA.1718.08.Science.3", "G8 Science", "3", "1718", .5761) %>% 
              mutate(reg_pct_correct = round(reg_pct_correct *100, 0),
                     subject = stringr::str_extract(local_assessment_id, "ELA|Math|Science"),
                     grade_level = as.integer(stringr::str_extract(title, "\\d"))
                     #grade_level = gsub("G", "", grade_level))
              )) %>% 
  select(-local_assessment_id,
         -title)




# regional_school_rollup <- assess_summary_school %>%
#   ungroup() %>%
#   select(-c(schoolid,
#             total_pts,
#             pts_scored,
#             school_name)) %>%
#   tidyr::spread(key = school_abbrev,
#                 value = pct_correct) %>%
#   inner_join(assess_summary_region %>%
#                ungroup() %>%
#                select(local_assessment_id,
#                       #grade_level,
#                       reg_pct_correct),
#              by = "local_assessment_id")


assessment_results_by_standard <- agg_student_responses_standard %>%
  dplyr::filter(assessment_id %in% all_ias) %>%
  select(student_id,
         standard_id,
         assessment_id,
         student_assessment_id,
         mastered,
         points,
         points_possible,
         number_of_questions) %>%
  inner_join(students %>%
               select(student_id,
                      local_student_id),
             by = "student_id") %>%
  inner_join(standards %>%
               select(standard_id,
                      parent_standard_id,
                      level,
                      description,
                      custom_code),
             by = "standard_id") %>%
  mutate(local_student_id = as.integer(local_student_id)) %>% 
  left_join(ps_stus %>% 
              select(student_number,
                     schoolid, 
                     grade_level),
            by = c("local_student_id" = "student_number")) %>% 
  mutate(custom_code_0 = custom_code,
         custom_code = if_else(grepl("SCI", custom_code_0), gsub("ABDCID).|NGSSTV.", "", custom_code_0), custom_code_0)) %>%
  inner_join(assessment_ids,
             by = "assessment_id") %>%
  filter(!student_assessment_id %in% archived_assess$student_assessment_id,
         #removing KAC 4th ELA 1 taken during IA 2
         !(grepl("04.ELA.1", local_assessment_id) & schoolid == 400146),
         !(grepl("03.ELA.1", local_assessment_id) & schoolid == 400146),
         !(grepl("03.ELA.1", local_assessment_id) & schoolid == 4001462 & grade_level == 4),
         !(grepl("08.Math", local_assessment_id) & schoolid == 400180 & grade_level == 7))

stu_assessment_id <- assessment_stus_rev %>% 
  select(student_assessment_id) %>% 
  unique() %>% 
  inner_join(assessment_results_by_standard %>% 
               select(student_assessment_id) %>%
               unique(),
             by = "student_assessment_id")

student_mastery_stds <- assessment_results_by_standard %>% 
  filter(student_assessment_id %in% stu_assessment_id$student_assessment_id) %>% 
  mutate(pct_mastered =  points/points_possible,
         m_level = pct_mastered >= .7) 

stds_100 <- student_mastery_stds %>% 
  group_by(local_assessment_id,
           local_student_id,
           m_level) %>% 
  count() %>% 
  group_by(local_assessment_id,
           local_student_id) %>%
  mutate(nn = sum(n)) %>% 
  rename(m_level_n = n,
         total_n = nn) %>% 
  filter(m_level, 
         m_level_n == total_n) %>% 
  mutate(subject = stringr::str_extract(local_assessment_id, "ELA|Math|Science"),
         grade = stringr::str_extract(local_assessment_id, "0\\d"),
         grade = gsub("0", "", grade),
         sy = stringr::str_extract(local_assessment_id, "1718|1819|1920"),
         ia = stringr::str_extract(local_assessment_id, "[1-3]$"))

all_students_testing <- student_mastery_stds %>% 
  select(local_assessment_id,
         local_student_id) %>% 
  unique() %>% 
  mutate(subject = stringr::str_extract(local_assessment_id, "ELA|Math|Science"),
         grade = stringr::str_extract(local_assessment_id, "0\\d"),
         grade = gsub("0", "", grade),
         sy = stringr::str_extract(local_assessment_id, "1718|1819|1920"),
         ia = stringr::str_extract(local_assessment_id, "[1-3]$"),
         dl = grepl("DL", local_assessment_id)) %>% 
  group_by(sy,
           ia,
           subject) %>% 
  count() %>% 
  rename(total_students = n)

all_students_testing_grade <- student_mastery_stds %>% 
  select(local_assessment_id,
         local_student_id) %>% 
  unique() %>% 
  mutate(subject = stringr::str_extract(local_assessment_id, "ELA|Math|Science"),
         grade = stringr::str_extract(local_assessment_id, "0\\d"),
         grade = gsub("0", "", grade),
         sy = stringr::str_extract(local_assessment_id, "1718|1819|1920"),
         ia = stringr::str_extract(local_assessment_id, "[1-3]$")) %>% 
  group_by(sy,
           ia,
           subject,
           grade) %>% 
  count() %>% 
  rename(total_students = n)

####
#KOA % Mastered By Standard ##
assessment_results_by_standard %>%
  mutate(local_student_id  = as.integer(local_student_id)) %>%
  left_join(ps_stus %>%
              select(schoolid,
                     student_number),
            by = c("local_student_id" = "student_number",
                   "schoolid")) %>% 
  filter(schoolid == 400180,
         grepl("1819", local_assessment_id),
         grepl(".3$", local_assessment_id)) %>% 
  group_by(schoolid,
           local_assessment_id,
           local_student_id) %>% 
  summarize(points = sum(points),
            total_pt = sum(points_possible),
            avg_std = points/total_pt) %>% 
  mutate(over_60 = avg_std >= .6) %>%
count(over_60) %>% 
  group_by(schoolid,
           local_assessment_id) %>% 
  mutate(nn = sum(n)) %>% 
  mutate(pct_over_60 = n/nn) %>% 
  mutate(mastery = if_else(over_60, "60%+ Mastery", "Not Yet Mastered"),
         Mastery = factor(mastery, levels = c("60%+ Mastery", "Not Yet Mastered")),
         grade = stringr::str_extract(local_assessment_id, "0[\\d]"),
         grade = as.double(grade),
         grade = factor(grade, levels = c("7", "6", "5")),
         subject = stringr::str_extract(local_assessment_id, "(Math|ELA|Science)"),
         grade_subj = sprintf("%sth %s", grade, subject),
         pct_over_60 = pct_over_60 *100,
         label_p = sprintf("%s%%", round(pct_over_60, 0)),
         text_axis = if_else(over_60, 90, 15)) %>% 
  ggplot2::ggplot() +
  geom_bar(aes(x = grade, y = pct_over_60, 
               fill = Mastery),
           stat = "identity",
           position = "stack") +
  facet_wrap("subject") +
  geom_text(aes(x = grade,
                y = text_axis,
                label = label_p),
            force = 2,
            size = 4,
            color = "white") +
  
  scale_fill_kipp("kipp_bluegray") +
  scale_y_continuous(breaks=c(0, 50, 100),
                     labels = c("0", "50", "100"),
                     name = "% Students with 60%+ Mastery") +
  scale_x_discrete(name = "Grade") +
  #theme_kipp_light(base_family = "Lato") +
  coord_flip()
####

save(mastery_pct_correct,
     not_yet,
     regional_mastery,
     assess_summary_region,
     reg_1_2_compare,
     stds_100,
     all_students_testing,
     all_students_testing_grade,
     science_1718,
     file = here::here("data/IA_1_db_cont.Rda"))


