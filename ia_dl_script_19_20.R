library(dplyr)
library(silounloadr)
library(readr)
library(lubridate)
library(purrr)
library(stringr)
library(forcats)
library(ggplot2)
library(gridExtra)
library(googlesheets)

schools <- data_frame(schoolid = c(78102, 7810, 400146, 4001462, 400163, 4001802, 400180),
                      school_abbrev =c("KAP", "KAMS", "KAC", "KACP", "KBCP", "KOP", "KOA"))

drop_fivetran_cols <- . %>% select(-starts_with("_fivetran"))


clean_bq_col_names <- function(.data) {
  old_names <- names(.data)
  new_names <- old_names %>% str_replace("^.{10}_", "")
  names(.data) <- new_names
  
  # return
  .data
}

con <- dbplyr::simulate_dbi()

assessments <- get_illuminate("assessments") %>% 
  filter(dbplyr::build_sql("REGEXP_CONTAINS(local_assessment_id,'IA.1920.(0[3-8]).(ELA|Math|Science).1(.|)DL')", con = con)) %>% 
  collect()


#dl_assessment_ids <- c(40834, 40870, 40808, 40844)     IA 2
# dl_assessment_ids <- c(71263, #17-18 5th IA 1
#                        71230, #17-18 5th IA 2,
#                        71217, #17-18 5th IA 3,
#                        71264, #17-18 6th IA 1,
#                        71318, #17-18 6th IA 3,
#                        71265, #17-18 7th IA 1,
#                        70974, #<160s RCNS,
#                        70976, #170s RNCS,
#                        71317, #180s RCNS,
#                        70979, #190s RCNS,
#                        70981, #200s RCNS,
#                        70984, #5-6 ELA,
#                        70983  #7-8 ELA
#                        )

# dl_assessment_ids <- c(71263, #17-18 5th IA 1
#                        71230, #17-18 5th IA 2
#                        71217, #17-18 5th IA 3
#                        71264, #17-18 6th IA 1
#                        71318, #17-18 6th IA 3
#                        70976, #RIT 170s RCNS
#                        70977, #RIT 180s GSP
#                        70980, #RIT 190s GSP
#                        70973, #RIT 160s GSP
#                        70981, #RIT 200s RCNS
#                        70979, #RIT 190s RCNS
#                        71317, #180s RCNS
#                        72914, #5-6 ELA
#                        71647 #7-8 ELA
#                        )

# dl_assessment_ids <- c(71217, #17-18 5th IA 3
#                        71264, #17-18 6th IA 1 -
#                        70976, #RIT 170s RCNS -
#                        70977, #RIT 180s GSP -
#                        70980, #RIT 190s GSP -
#                        70982, #RIT 200s GSP -
#                        70973, #RIT 160s GSP -
#                        70981, #RIT 200s RCNS - 
#                        70979, #RIT 190s RCNS -
#                        71317, #180s RCNS - 
#                        70974, #160s RCNS - 
#                        74070, #5-6 ELA
#                        74067 #7-8 ELA
# )

dl_assessment_ids <- assessments$assessment_id

agg_student_responses_table <- get_illuminate("agg_student_responses") %>%
  dplyr::filter(assessment_id %in% dl_assessment_ids) %>%
  collect(n = Inf)


agg_student_responses <- agg_student_responses_table %>%
  group_by(student_assessment_id, 
           student_id, 
           assessment_id) %>% 
  dplyr::filter(`_fivetran_synced` == max(`_fivetran_synced`)) %>%
  drop_fivetran_cols() %>%
  ungroup()

agg_student_responses_standard_table <- get_illuminate("agg_student_responses_standard") %>%
  dplyr::filter(assessment_id %in% dl_assessment_ids) %>%
  collect(n = Inf)


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
  #dplyr::filter(date_taken >= bq_date) %>%
  collect(n = Inf)

students <- get_illuminate("students", "public", collect = TRUE)

ps_stus <- get_powerschool("students") %>% 
  drop_fivetran_cols %>%
  select(
    ps_id = id,
    first_name,
    last_name,
    middle_name,
    grade_level,
    schoolid,
    home_room,
    student_number) %>%
  collect() 

sy <- calc_academic_year(today(), "first")

ps_termid <- calc_ps_termid(sy)

cc <- get_powerschool("cc") %>%
  filter(termid == ps_termid) %>% 
  select(ps_id = studentid,
         schoolid,
         course_number,
         section_number,
         teacherid,
         dateenrolled,
         dateleft) %>% 
  collect()

schoolstaff <- get_powerschool("schoolstaff") %>% 
  select(users_dcid,
         teacherid = id) %>% 
  collect()

users <- get_powerschool("users") %>% 
  select(users_dcid = dcid,
         first_name, 
         last_name) %>% 
  collect()

today_date <- today()

sy <- calc_academic_year(today_date, format = "first_year")

sy_short <- calc_academic_year(today_date, format = "short")

ps_termid <- calc_ps_termid(sy)

ps_yearid <- ps_termid/100

terms <- get_powerschool("terms") %>% 
  filter(yearid == ps_yearid,
         abbreviation == sy_short) %>% 
  select(lastday) %>% 
  collect() %>% 
  unique()

course_teacher <- cc %>% 
  filter(dateleft >= ymd(terms$lastday),
         section_number > 4) %>% 
  left_join(schoolstaff,
            by = "teacherid") %>% 
  left_join(users,
            by = "users_dcid") %>% 
  left_join(schools,
            by = "schoolid") %>% 
  tidyr::unite(teacher_name, last_name, school_abbrev, sep = " ") %>% 
  mutate(subject = stringr::str_extract(course_number, "ela|math"),
         subject = ifelse(subject %in% "ela", "ELA", 
                          ifelse(subject %in% "math", "Math",
                                 NA)))

##IA DL Assessments
# dl_assessments <- c("IA.DL1819.05.Math.1",
#                     "IA.DL1819.05.Math.2",
#                     "IA.DL1819.05.Math.3",
#                     "IA.DL.1819.06.Math.1",
#                     "IA.DL.1819.06.Math.3",
#                     "IA.DL.1819.07.Math.1",
#                     "IA.1819.DL.170.RCNSOA",
#                     "IA.1819.DL.190.RCNSOA",
#                     "IA.1819.DL.200.RCNSOA",
#                     "IA.1819.DL.180.RCNSOA",
#                     "IA.1819.DL.160.GSP",
#                     "IA.1819.DL.190.GSP",
#                     "IA.1819.DL.180.GSP",
#                     "IA.1819.5-6.ELA.DL.2",
#                     "IA.1819.7-8.ELA.DL.2") 

archived_assess <- get_illuminate("students_assessments_archive") %>% 
  filter(date_taken >= "2019-09-01", #"2019-03-25",
         archived_at >= "2019-10-27",
         is.na(`_fivetran_deleted`)) %>% 
  collect() 

asr_assessments <- 
  agg_student_responses %>% 
  inner_join(assessments %>% drop_fivetran_cols,
             by = c("assessment_id"="assessment_id")) %>% 
  filter(date_taken >= "2019-09-01",
         date_taken <= "2019-11-04")#ymd("2019-03-25"))




#removal_ids <- c(50407853, 50203812, 50083417)

assessment_stus <- asr_assessments %>%
  mutate(#title = if_else(title %in% "18-19 DL IA RIT &lt; 160 G & SP",
    #                        "18-19 DL IA RIT < 160 G & SP", title),
    #        title = if_else(title %in% "18-19 DL IA RIT &lt; 160 RCNS & OA",
    #                        "18-19 DL IA RIT < 160 RCNS & OA", title),
    subject = stringr::str_extract(local_assessment_id, "(ELA|Math)")) %>%
  filter(#date_taken >= ymd("2019-03-25"),
    # date_taken <= ymd("2019-04-09"),
    !student_assessment_id %in% archived_assess$student_assessment_id) %>% 
  inner_join(students %>% 
               select(student_id, 
                      local_student_id) %>%
               mutate(local_student_id = as.integer(local_student_id)), 
             by = "student_id") %>% 
  left_join(ps_stus %>%
              select(
                ps_id,
                first_name,
                last_name, 
                middle_name,
                #lastfirst,
                grade_level,
                schoolid,
                home_room,
                student_number),
            by=c("local_student_id" = "student_number")) %>% # inner_join
  left_join(schools,
            by = "schoolid") %>% 
  left_join(course_teacher %>% 
              select(ps_id,
                     teacher_name,
                     subject),
            by = c("ps_id",
                   "subject")) %>% #filter(is.na(teacher_name))
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
  mutate(teacher_name = if_else(is.na(teacher_name), school_abbrev, teacher_name))
#        remove_stus = if_else(local_student_id %in% removal_ids, TRUE, FALSE)) %>% 
# filter(!remove_stus)

assessment_ids <- assessment_stus %>%
  select(assessment_id,
         local_assessment_id,
         title) %>%
  unique()

assess_summary_school_sped <- assessment_stus %>% #assess_deduped %>% 
  group_by(local_assessment_id,
           assessment_id,
           title, 
           #schoolid, 
           #grade_level
           teacher_name) %>%
  summarize(total_pts = sum(points_possible),
            pts_scored = sum(points),
            pct_correct = pts_scored/total_pts)

assess_summary_region_sped <- assessment_stus %>%
  group_by(local_assessment_id,
           assessment_id,
           title) %>%  
  summarize(total_pts = sum(points_possible),
            pts_scored = sum(points),
            reg_pct_correct = pts_scored/total_pts)

regional_school_rollup_sped <- assess_summary_school_sped %>%
  ungroup() %>% 
  select(-c(assessment_id,
            total_pts,
            pts_scored)) %>%
  tidyr::spread(key = teacher_name,
                value = pct_correct) %>%
  inner_join(assess_summary_region_sped %>%
               ungroup() %>% 
               select(local_assessment_id,
                      #grade_level,
                      reg_pct_correct),
             by = "local_assessment_id") %>% 
  rename(Region = reg_pct_correct)

total_students_sped_0 <- assessment_stus %>% #filter(local_assessment_id %in% "IA.1819.06.Science.1", school_abbrev == "KOA")
  group_by(subject,
           grade_level,
           schoolid) %>% 
  summarize(n_students = n()) %>% 
  ungroup() %>% 
  mutate(subject = sprintf("DL %s %s", subject, grade_level)) %>% 
  left_join(schools,
            by = "schoolid") %>% 
  select(-schoolid,
         -grade_level)

total_students_sped <- assessment_stus %>% #filter(local_assessment_id %in% "IA.1819.06.Science.1", school_abbrev == "KOA")
  group_by(local_assessment_id,
           title,
           teacher_name,
           grade_level) %>% 
  summarize(n_students = n())

region_btm_band_sped <- total_students_sped %>% 
  left_join(assessment_stus %>% 
              filter(mastered_level %in% "Not Yet Mastered") %>% 
              group_by(local_assessment_id,
                       teacher_name,
                       grade_level) %>% 
              summarize(not_yet = n()),
            by = c("local_assessment_id",
                   "teacher_name",
                   "grade_level")) %>% 
  group_by(local_assessment_id,
           title) %>% 
  summarise(reg_not_yet = sum(not_yet, na.rm = TRUE),
            reg_n_students = sum(n_students, na.rm = TRUE),
            reg_pct = reg_not_yet/reg_n_students)

# bottom_band_sped <- assessment_stus %>%
#   filter(mastered_level %in% "Not Yet Mastered") %>%
#   group_by(local_assessment_id,
#            title,
#            teacher_name,
#            grade_level) %>%
#   summarize(not_yet = n()) %>%
#   left_join(total_students_sped,
#             by = c("local_assessment_id",
#                    "teacher_name",
#                    "title",
#                    "grade_level")) %>%
#   group_by(local_assessment_id,
#            title,
#            teacher_name) %>%
#   summarize(btm_band = sum(not_yet)/sum(n_students))

bottom_band_sped <- total_students_sped %>% 
  left_join(assessment_stus %>% 
              filter(mastered_level %in% "Not Yet Mastered") %>% 
              group_by(local_assessment_id,
                       title,
                       teacher_name,
                       grade_level) %>% 
              summarize(not_yet = n()),
            by = c("local_assessment_id",
                   "teacher_name",
                   "title",
                   "grade_level")) %>% 
  group_by(local_assessment_id,
           title,
           teacher_name) %>% 
  summarize(btm_band = sum(not_yet)/sum(n_students)) %>% 
  mutate(btm_band = if_else(is.na(btm_band), 0, btm_band))

# sped_bottom_band_reg <- assessment_stus %>% 
#   filter(mastered_level %in% "Not Yet Mastered") %>% 
#   mutate(subject = stringr::str_extract(local_assessment_id, "ELA|Math"),
#          # subject = ifelse(subject %in% c("RCNS", "GSP"), "Math", subject),
#          subject = if_else(grade_level <= 4, sprintf("%s 03-4", subject), sprintf("%s 05-8", subject))) %>% 
#   group_by(subject) %>% 
#   summarize(not_yet = n()) %>% 
#   left_join(total_students_sped %>% 
#               mutate(subject = stringr::str_extract(local_assessment_id, "ELA|Math"),
#                      # subject = ifelse(subject %in% c("RCNS", "GSP"), "Math", subject),
#                      subject = if_else(grade_level <= 4, sprintf("%s 03-4", subject), sprintf("%s 05-8", subject))) %>% 
#               group_by(subject) %>% 
#               summarise(all_students = sum(n_students)),
#             by = c("subject")) %>% 
#   mutate(pct_btm_band = not_yet/all_students)

sped_bottom_band_reg <- total_students_sped %>% 
  mutate(subject = stringr::str_extract(local_assessment_id, "ELA|Math"),
         # subject = ifelse(subject %in% c("RCNS", "GSP"), "Math", subject),
         subject = if_else(grade_level <= 4, sprintf("%s 03-4", subject), sprintf("%s 05-8", subject))) %>% 
  group_by(subject) %>% 
  summarise(all_students = sum(n_students)) %>% 
  left_join(assessment_stus %>% 
              filter(mastered_level %in% "Not Yet Mastered") %>% 
              mutate(subject = stringr::str_extract(local_assessment_id, "ELA|Math"),
                     # subject = ifelse(subject %in% c("RCNS", "GSP"), "Math", subject),
                     subject = if_else(grade_level <= 4, sprintf("%s 03-4", subject), sprintf("%s 05-8", subject))) %>% 
              group_by(subject) %>% 
              summarize(not_yet = n()),
            by = c("subject")) %>% 
  mutate(not_yet = if_else(is.na(not_yet), 0, as.double(not_yet)),
         pct_btm_band = not_yet/all_students)



stds_mastered_by_student_sped <- agg_student_responses_standard %>%
  left_join(standards %>%
              select(standard_id,
                     custom_code),
            by = "standard_id") %>%
  inner_join(assessment_stus %>%
               select(assessment_id,
                      student_assessment_id,
                      local_assessment_id,
                      title,
                      local_student_id,
                      student_id,
                      first_name,
                      last_name,
                      middle_name,
                      teacher_name) %>%
               mutate(assessment_id = as.integer(assessment_id)),
             #schoolid,
             #home_room),
             by = c("assessment_id",
                    "student_id",
                    "student_assessment_id")) %>%
  mutate(mastered_level = if_else(percent_correct < 21, 
                                  "Not Yet Mastered",
                                  if_else(percent_correct < 41 &
                                            percent_correct >= 21,
                                          "Partially",
                                          if_else(percent_correct < 70 &
                                                    percent_correct >= 41,
                                                  "Near Mastery",
                                                  if_else(percent_correct >= 70, 
                                                          "Mastered", "NA"))))) %>%
  # mutate(number_of_questions_0 = number_of_questions,
  #        number_of_questions_1 = ifelse(custom_code %in% "CCSS.MA.4.4.NF.3.a" & assessment_id %in% 71317, number_of_questions_0 - 1, number_of_questions_0)) %>% 
  filter(!student_assessment_id %in% archived_assess$student_assessment_id)
#number_of_questions = ifelse(custom_code %in% "CCSS.Math.Content.5.NF.A.1" & assessment_id %in% 70981, number_of_questions_1 - 1, number_of_questions_1),
#        remove_stus = if_else(local_student_id %in% removal_ids, TRUE, FALSE)
# ) %>% 
# filter(!remove_stus)

m_total_students_sped <- stds_mastered_by_student_sped %>% #filter(local_assessment_id %in% "IA.1819.05.Science.1", school_abbrev == "KOA")
  group_by(local_assessment_id,
           title,
           teacher_name,
           custom_code) %>% 
  summarize(n_students = n())

region_m_band_sped <- m_total_students_sped %>% 
  left_join(stds_mastered_by_student_sped %>% 
              filter(mastered_level %in% "Mastered") %>% 
              group_by(local_assessment_id,
                       title,
                       teacher_name,
                       custom_code) %>% 
              summarize(mastered_band = n()),
            by = c("local_assessment_id",
                   "title",
                   "teacher_name",
                   "custom_code")) %>%
  mutate(mastered_band = if_else(is.na(mastered_band), 0, as.double(mastered_band))) %>% 
  group_by(local_assessment_id,
           title,
           custom_code) %>% 
  summarise(reg_mastered = sum(mastered_band, na.rm = TRUE),
            reg_n_students = sum(n_students, na.rm = TRUE),
            reg_pct = reg_mastered/reg_n_students)

# mastered_band_sped <- stds_mastered_by_student_sped %>% 
#   filter(mastered_level %in% "Mastered") %>% 
#   group_by(local_assessment_id,
#            title,
#            teacher_name,
#            custom_code) %>% 
#   summarize(mastered = n()) %>% 
#   left_join(m_total_students_sped,
#             by = c("local_assessment_id",
#                    "title",
#                    "teacher_name",
#                    "custom_code")) %>% 
#   mutate(mastered_band = mastered/n_students) %>% 
#   select(-c(mastered,
#             n_students))

mastered_band_sped <- m_total_students_sped %>% 
  left_join(stds_mastered_by_student_sped %>% 
              filter(mastered_level %in% "Mastered") %>% 
              group_by(local_assessment_id,
                       title,
                       teacher_name,
                       custom_code) %>% 
              summarize(mastered = n()),
            by = c("local_assessment_id",
                   "title",
                   "teacher_name",
                   "custom_code")) %>% 
  mutate(mastered = if_else(is.na(mastered), 0, as.double(mastered)),
         mastered_band = mastered/n_students) %>% 
  select(-c(mastered,
            n_students))


regional_sped <- stds_mastered_by_student_sped %>%
  group_by(assessment_id,
           local_assessment_id,
           title,
           custom_code
  ) %>%
  summarise(n_students = n(),
            total_pts = sum(points_possible),
            pts_scored = sum(points),
            reg_pct_correct = pts_scored/total_pts) 



# stds_teacher_reg_sped <- standard_results_summary_sped %>% 
#   ungroup() %>% 
#   select(teacher_name,
#          local_assessment_id,
#          custom_code,
#          pct_correct) %>% 
#   tidyr::spread(teacher_name,
#                 pct_correct) %>% 
#   left_join(regional_sped %>% 
#               ungroup() %>% 
#               select(local_assessment_id,
#                      Region = reg_pct_correct,
#                      custom_code),
#             by = c("local_assessment_id",
#                    "custom_code"))

sped_grade <- assessment_stus %>% 
  group_by(grade_level,
           subject,
           schoolid) %>%
  summarise(total_pts = sum(points),
            total_pts_possible = sum(points_possible),
            pct_correct = total_pts/total_pts_possible) %>% 
  left_join(schools,
            by = "schoolid")

sped_grade_reg <- sped_grade %>%
  select(-c(total_pts,
            total_pts_possible,
            schoolid)) %>% 
  tidyr::spread(school_abbrev,
                pct_correct) %>% 
  left_join(assessment_stus %>% 
              group_by(grade_level,
                       subject) %>%
              summarise(total_pts = sum(points),
                        total_pts_possible = sum(points_possible),
                        reg_pct_correct = total_pts/total_pts_possible) %>% 
              select(grade_level,
                     subject,
                     Region = reg_pct_correct),
            by = c("grade_level",
                   "subject"))

sped_school_subject <- assessment_stus %>% 
  mutate(grade_group_0 = if_else(grepl("0[3-4]", local_assessment_id), "03-4", ""),
         grade_group = if_else(grepl("0[5-8]", local_assessment_id), "05-8", grade_group_0),
         subj_grade = sprintf("DL %s %s", subject, grade_group)) %>% 
  group_by(subj_grade,
           schoolid) %>%
  summarise(total_pts = sum(points),
            total_pts_possible = sum(points_possible),
            pct_correct = total_pts/total_pts_possible) %>% 
  left_join(schools,
            by = "schoolid") %>% 
  select(school_abbrev,
         pct_correct,
         subj_grade)

sped_region <- assessment_stus %>% 
  mutate(grade_group_0 = if_else(grepl("0[3-4]", local_assessment_id), "03-4", ""),
         grade_group = if_else(grepl("0[5-8]", local_assessment_id), "05-8", grade_group_0),
         subj_grade = sprintf("DL %s %s", subject, grade_group)) %>% 
  group_by(subj_grade) %>%
  summarise(total_pts = sum(points),
            total_pts_possible = sum(points_possible),
            reg_pct_correct = total_pts/total_pts_possible) %>% 
  select(subj_grade,
         reg_pct_correct)

# sped_grade_b_band_0 <- assessment_stus %>%
#   filter(mastered_level %in% "Not Yet Mastered") %>% 
#   group_by(grade_level,
#            subject,
#            schoolid) %>%
#   summarise(not_yet = n()) %>% 
#   left_join(schools,
#             by = "schoolid") %>% 
#   left_join(assessment_stus %>% 
#               group_by(grade_level,
#                        subject,
#                        schoolid) %>% 
#               summarise(n_students = n()),
#             by = c("grade_level",
#                    "subject", 
#                    "schoolid")) %>% 
#   mutate(pct = not_yet/n_students) 

sped_grade_b_band_0 <- assessment_stus %>% 
  group_by(grade_level,
           subject,
           schoolid) %>% 
  summarise(n_students = n()) %>% 
  left_join(schools,
            by = "schoolid") %>%
  left_join(assessment_stus %>%
              filter(mastered_level %in% "Not Yet Mastered") %>% 
              group_by(grade_level,
                       subject,
                       schoolid) %>%
              summarise(not_yet = n()),
            by = c("grade_level",
                   "subject", 
                   "schoolid")) %>% 
  mutate(not_yet = if_else(is.na(not_yet), 0, as.double(not_yet)),
         pct = not_yet/n_students) 

sped_grade_b_band <- sped_grade_b_band_0 %>%  
  select(school_abbrev,
         grade_level,
         subject, 
         pct)


# sped_school_b_band_0 <- assessment_stus %>%
#   filter(mastered_level %in% "Not Yet Mastered") %>% 
#   group_by(subject,
#            schoolid,
#            grade_level) %>%
#   summarise(not_yet = n()) %>% 
#   ungroup() %>% 
#   mutate(subject = if_else(grade_level <= 4, sprintf("%s 03-4", subject), sprintf("%s 05-8", subject))) %>% 
#   group_by(subject, 
#            schoolid) %>% 
#   summarize(not_yet = sum(not_yet)) %>% 
#   left_join(schools,
#             by = "schoolid") %>% 
#   left_join(assessment_stus %>%
#               mutate(subject = if_else(grade_level <= 4, sprintf("%s 03-4", subject), sprintf("%s 05-8", subject))) %>% 
#               group_by(subject,
#                        schoolid) %>% 
#               summarise(n_students = n()),
#             by = c("subject", 
#                    "schoolid")) %>% 
#   mutate(pct = not_yet/n_students)

sped_school_b_band_0 <- assessment_stus %>%
  mutate(subject = if_else(grade_level <= 4, sprintf("%s 03-4", subject), sprintf("%s 05-8", subject))) %>% 
  group_by(subject,
           schoolid) %>% 
  summarise(n_students = n()) %>% 
  left_join(assessment_stus %>%
              filter(mastered_level %in% "Not Yet Mastered") %>% 
              ungroup() %>% 
              mutate(subject = if_else(grade_level <= 4, sprintf("%s 03-4", subject), sprintf("%s 05-8", subject))) %>% 
              group_by(subject,
                       schoolid) %>%
              summarise(not_yet = n()),
            by = c("subject", 
                   "schoolid")) %>% 
  left_join(schools,
            by = "schoolid") %>% 
  mutate(not_yet = if_else(is.na(not_yet), 0, as.double(not_yet)),
         pct = not_yet/n_students)

sped_school_b_band <- sped_school_b_band_0 %>% 
  select(school_abbrev,
         subject, 
         pct)

# sped_reg_b_band <- assessment_stus %>%
#   filter(mastered_level %in% "Not Yet Mastered") %>% 
#   group_by(subject,
#            grade_level) %>%
#   summarise(not_yet = n()) %>% 
#   left_join(assessment_stus %>% 
#               group_by(subject,
#                        grade_level) %>% 
#               summarise(n_students = n()),
#             by = c("subject", 
#                    "grade_level")) %>% 
#   mutate(pct = not_yet/n_students) %>% 
#   select(subject,
#          grade_level,
#          pct)

sped_reg_b_band <- assessment_stus %>% 
  group_by(subject,
           grade_level) %>% 
  summarise(n_students = n()) %>% 
  left_join(assessment_stus %>%
              filter(mastered_level %in% "Not Yet Mastered") %>% 
              group_by(subject,
                       grade_level) %>%
              summarise(not_yet = n()),
            by = c("subject", 
                   "grade_level")) %>% 
  mutate(not_yet = if_else(is.na(not_yet), 0, as.double(not_yet)),
         pct = not_yet/n_students) %>% 
  select(subject,
         grade_level,
         pct)


# sped_reg_grade_b_band <- assessment_stus %>%
#   filter(mastered_level %in% "Not Yet Mastered") %>% 
#   group_by(subject,
#            grade_level) %>%
#   summarise(not_yet = n()) %>% 
#   left_join(assessment_stus %>% 
#               group_by(subject,
#                        grade_level) %>% 
#               summarise(n_students = n()),
#             by = c("subject", 
#                    "grade_level")) %>% 
#   ungroup() %>% 
#   mutate(subject = if_else(grade_level <= 4, sprintf("%s 03-4", subject), sprintf("%s 05-8", subject))) %>%
#   group_by(subject) %>% 
#   summarize(pct = sum(not_yet)/sum(n_students))

sped_reg_grade_b_band <- assessment_stus %>% 
  group_by(subject,
           grade_level) %>% 
  summarise(n_students = n()) %>% 
  left_join(assessment_stus %>%
              filter(mastered_level %in% "Not Yet Mastered") %>% 
              group_by(subject,
                       grade_level) %>%
              summarise(not_yet = n()),
            by = c("subject", 
                   "grade_level")) %>% 
  ungroup() %>% 
  mutate(subject = if_else(grade_level <= 4, sprintf("%s 03-4", subject), sprintf("%s 05-8", subject)),
         not_yet = if_else(is.na(not_yet), 0, as.double(not_yet))) %>%
  group_by(subject) %>% 
  summarize(pct = sum(not_yet)/sum(n_students))


save(assess_summary_school_sped,
     assess_summary_region_sped,
     bottom_band_sped,
     region_btm_band_sped,
     region_m_band_sped,
     stds_mastered_by_student_sped,
     mastered_band_sped,
     sped_grade_reg,
     sped_school_subject,
     sped_region,
     sped_bottom_band_reg,
     sped_grade_b_band,
     sped_grade_b_band_0,
     sped_school_b_band,
     sped_school_b_band_0,
     sped_reg_b_band,
     sped_reg_grade_b_band,
     total_students_sped_0,
     file = here("data/1920_dl_IA_1.Rda"))





