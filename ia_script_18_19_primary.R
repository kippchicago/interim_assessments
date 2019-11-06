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
library(here)

schools <- tribble(~"schoolid", ~"school_abbrev",
                   7810, "KAMS",
                   78102, "KAP",
                   400146, "KAC",
                   4001462, "KACP",
                   400163, "KBCP",
                   4001632, "KBP",
                   400180, "KOA",
                   4001802, "KOP")


drop_fivetran_cols <- . %>% 
  select(-starts_with("_fivetran"))

##Pull Assessments with SQL regexp (to be filtered by BigQuery) 
con <- dbplyr::simulate_dbi()
assessments <- get_illuminate("assessments") %>% 
  filter(dbplyr::build_sql("REGEXP_CONTAINS(local_assessment_id,'IA.1920.(0[3-8]).(ELA|Math|Science|Algebra).1')", con = con),
         !dbplyr::build_sql("REGEXP_CONTAINS(local_assessment_id,'DL')", con = con)) %>% 
  collect()

#Internal Illuminate assessment IDs to filter responses and standards before collecting
ia_assess_ids <- assessments$assessment_id

#Pull student responses but do not collect
agg_student_responses_uncollected <- get_illuminate("agg_student_responses") %>%
  dplyr::filter(assessment_id %in% ia_assess_ids)

#Pull archived assessments and perform inner_join via BigQuery
archived_assess <- get_illuminate("students_assessments_archive") %>% #glimpse()
  inner_join(agg_student_responses_uncollected,
             by = c("assessment_id",
                    "student_id",
                    "student_assessment_id")) %>% 
  select(archived_at,
         assessment_id,
         student_id,
         student_assessment_id) %>%
  collect()


agg_student_responses <- agg_student_responses_uncollected %>%
  collect() %>% 
  group_by(student_assessment_id, 
           student_id, 
           assessment_id) %>% 
  dplyr::filter(`_fivetran_synced` == max(`_fivetran_synced`)) %>%
  drop_fivetran_cols() %>%
  ungroup()


#Pull student responses with standards by assessment ids
agg_student_responses_standard_table <- get_illuminate("agg_student_responses_standard") %>%
  dplyr::filter(assessment_id %in% ia_assess_ids) %>%
  collect()

agg_student_responses_standard <- agg_student_responses_standard_table %>%
  group_by(student_assessment_id, 
           student_id, 
           assessment_id, 
           standard_id) %>% 
  dplyr::filter(`_fivetran_synced` == max(`_fivetran_synced`)) %>%
  drop_fivetran_cols() %>%
  ungroup()


#Identify standards used in the current set of assessments
ia_standards <- agg_student_responses_standard$standard_id %>% 
  unique()

#Pull standards table, filter by standards used in assessments
standards <- get_illuminate("standards", schema = "standards") %>% 
  drop_fivetran_cols() %>%
  filter(standard_id %in% ia_standards) %>% 
  collect()

#Pull Illuminate student IDs and corresponding PS ID
students <- get_illuminate("students", "public") %>%
  select(student_id,
         local_student_id) %>%
  collect()

#Pull PS students
ps_stus <- get_powerschool("students") %>% 
  select(
    first_name,
    last_name,
    grade_level,
    schoolid,
    home_room,
    student_number) %>%
  collect() 

#join student responses with assessments
asr_assessments <- 
  agg_student_responses %>% 
  inner_join(assessments %>% 
               select(assessment_id,
                      administered_at,
                      deleted_at,
                      local_assessment_id,
                      title),
             by = "assessment_id") %>%
  left_join(students,
            by = "student_id")

assessment_ids <- asr_assessments %>%
  select(assessment_id,
         local_assessment_id) %>%
  unique()

#Join assessment responses with student data
assessment_stus <- asr_assessments %>%
  mutate(local_student_id = as.integer(local_student_id)) %>%
  left_join(ps_stus %>% 
              select(
                first_name,
                last_name,
                grade_level,
                schoolid,
                home_room,
                student_number),
            by=c("local_student_id" = "student_number"))

#Revising # of questions for IAs with errors
#Removing archived assessments
assessment_stus_rev <- assessment_stus %>% 
  # filter(!percent_correct == 0) %>%
  # mutate(number_of_questions_0 = number_of_questions,
  #        number_of_questions = if_else(grepl("05.ELA", local_assessment_id), number_of_questions_0 - 2, as.double(number_of_questions_0))
  # ) %>% 
  # mutate(number_of_questions_0 = number_of_questions,
  #        number_of_questions_1 = if_else(grepl("06.Math", local_assessment_id), number_of_questions_0 - 1, as.double(number_of_questions_0)),
  #        number_of_questions = if_else(grepl("08.Math", local_assessment_id), number_of_questions_1 - 1, as.double(number_of_questions_1))
  # ) %>% 
  filter(#date_taken >= ymd("2019-03-14"),
    #date_taken <= ymd("2019-04-09"), 
    !student_assessment_id %in% archived_assess$student_assessment_id) %>% 
  mutate(subject = stringr::str_extract(local_assessment_id, "Algebra|ELA|Math|Science"),
         subject = if_else(grade_level < 5, paste(subject, "03-4", sep = " "),
                           paste(subject, "05-8", sep = " ")),
         subject = if_else(grepl("Algebra", subject), "Algebra 8", subject))

#Calculating % Correct by IA - School
pct_correct_school <- assessment_stus_rev %>% 
  group_by(schoolid,
           subject) %>% 
  summarize(total_points = sum(points),
            total_points_possible = sum(points_possible),
            school_pct = total_points/total_points_possible) %>% 
  left_join(schools,
            by = "schoolid") %>% 
  select(-c(total_points,
            total_points_possible)) 

#Calculating % Correct by IA - Regionally
pct_correct_reg <- assessment_stus_rev %>% 
  group_by(subject) %>% 
  summarize(total_points = sum(points),
            total_points_possible = sum(points_possible),
            Region = total_points/total_points_possible) %>% 
  select(-c(total_points,
            total_points_possible)) 

#Calculating Performance Band/Mastery Level
mastery_pct_correct <- assessment_stus_rev %>% 
  mutate(pct_correct = points/points_possible,
         mastered_level = case_when(
           pct_correct < .21 ~ "Not Yet Mastered",
           pct_correct < .41 &
             pct_correct >= .21 ~ "Partially",
           pct_correct < .70 &
             pct_correct >= .41 ~ "Near Mastery",
           TRUE ~ "Mastered"
         )) %>% 
  left_join(schools,
            by = "schoolid")

#Count total students taking IAs: this is the denominator for % in bottom band and % mastered
total_students <- mastery_pct_correct %>% 
  group_by(local_assessment_id,
           school_abbrev) %>% 
  summarize(n_students = n()) %>% 
  mutate(subject = stringr::str_extract(local_assessment_id, "ELA|Math|Science|Algebra"),
         grade = if_else(grepl("03|04", local_assessment_id), "03-4", "05-8"),
         subject = paste(subject, grade, sep = " "),
         subject = if_else(grepl("Algebra", subject), "Algebra 8", subject)) %>% 
  select(-grade)

total_students_grade <- mastery_pct_correct %>% 
  group_by(local_assessment_id,
           school_abbrev) %>% 
  summarize(n_students = n()) %>% 
  mutate(subject = stringr::str_extract(local_assessment_id, "ELA|Math|Science|Algebra"),
         grade = as.integer(str_extract(local_assessment_id, "0\\d")),
         subject = sprintf("%s %s", subject, grade),
         subject = if_else(grepl("Algebra", subject), "Algebra 8", subject)) %>% 
  select(-grade)

bottom_band_grade <- mastery_pct_correct %>% 
  filter(mastered_level %in% "Not Yet Mastered") %>% 
  group_by(local_assessment_id,
           school_abbrev) %>% 
  summarize(not_yet = n()) %>% 
  mutate(subject = stringr::str_extract(local_assessment_id, "ELA|Math|Science|Algebra"),
         grade = as.integer(str_extract(local_assessment_id, "0\\d")),
         subject = sprintf("%s %s", subject, grade),
         subject = if_else(grepl("Algebra", subject), "Algebra 8", subject)) %>% 
  select(-grade)

n_btm_band_grade <- total_students_grade %>% 
  left_join(bottom_band_grade,
            by = c("local_assessment_id",
                   "school_abbrev",
                   "subject")) %>% 
  mutate(not_yet = if_else(is.na(not_yet), 0, as.double(not_yet))) 


#N students in bottom band by School and IA
bottom_band_0 <- mastery_pct_correct %>% 
  filter(mastered_level %in% "Not Yet Mastered") %>% 
  group_by(local_assessment_id,
           school_abbrev) %>% 
  summarize(not_yet = n()) %>% 
  mutate(subject = stringr::str_extract(local_assessment_id, "ELA|Math|Science|Algebra"),
         grade = if_else(grepl("03|04", local_assessment_id), "03-4", "05-8"),
         subject = paste(subject, grade, sep = " "),
         subject = if_else(grepl("Algebra", subject), "Algebra 8", subject)) %>% 
  select(-grade)

#% students in bottom band - Regionally
region_btm_band <- total_students %>% 
  left_join(bottom_band_0,
            by = c("local_assessment_id",
                   "school_abbrev")) %>% 
  group_by(local_assessment_id) %>% 
  summarise(reg_not_yet = sum(not_yet, na.rm = TRUE),
            reg_n_students = sum(n_students, na.rm = TRUE),
            reg_pct = reg_not_yet/reg_n_students)

#Bottom Band table with school and regional %
bottom_band <- total_students %>% #bottom_band_0 %>% 
  left_join(bottom_band_0, #left_join(total_students,
            by = c("local_assessment_id",
                   "school_abbrev",
                   "subject")) %>% 
  mutate(not_yet = if_else(is.na(not_yet), 0, as.double(not_yet)), #4/4 added this line
         btm_band = not_yet/n_students) %>% 
  select(-c(not_yet,
            n_students,
            subject)) %>% 
  tidyr::spread(key = school_abbrev,
                value = btm_band) %>%  
  # fill = 0) %>% 
  # mutate(KOA = ifelse(grepl("08", local_assessment_id),NA , KOA),
  #        KAC = ifelse(grepl("05.Math", local_assessment_id),NA , KAC)) %>% 
  left_join(region_btm_band %>% 
              select(local_assessment_id,
                     reg_pct),
            by = "local_assessment_id")

#Total students by subject 
total_students_subj <- total_students %>% 
  group_by(subject) %>% 
  summarise(all_students = sum(n_students))

#% of students (by subject) in bottom band - Regionally  
bottom_band_reg <- bottom_band_0 %>% 
  group_by(subject) %>% 
  summarise(not_yet = sum(not_yet)) %>% 
  left_join(total_students_subj,
            by = c("subject")) %>% 
  mutate(pct_btm_band = not_yet/all_students) %>% 
  select(subject,
         subj_pct_btm_band = pct_btm_band) 

total_students_school_subj <-  total_students %>% 
  group_by(school_abbrev,
           subject) %>% 
  summarise(all_students = sum(n_students))

#% of students (by subject) in bottom band - School
bottom_band_school_sub <- total_students_school_subj %>% 
  left_join(bottom_band_0 %>% 
              group_by(school_abbrev,
                       subject) %>% 
              summarize(not_yet = sum(not_yet)),
            by = c("subject",
                   "school_abbrev")
  ) %>% 
  mutate(not_yet = if_else(is.na(not_yet), 0, as.double(not_yet)),
         pct_btm_band = not_yet/all_students) %>% 
  ungroup() %>% 
  select(subject,
         school_abbrev,
         school_pct_btm_band = pct_btm_band) 

#Agg % Correct by IA, by School
assess_summary_school <- assessment_stus_rev %>%
  group_by(local_assessment_id,
           assessment_id,
           title, 
           schoolid) %>%  
  summarize(total_pts = sum(points_possible),
            pts_scored = sum(points),
            pct_correct = pts_scored/total_pts) %>%
  left_join(schools,
            by = "schoolid")

#Agg % Correct by IA, by Region
assess_summary_region <- assessment_stus_rev %>%
  group_by(local_assessment_id,
           assessment_id,
           title) %>%  
  summarize(total_pts = sum(points_possible),
            pts_scored = sum(points),
            reg_pct_correct = pts_scored/total_pts)

#Join School and Regional % Correct by IA
regional_school_rollup <- assess_summary_school %>%
  ungroup() %>% 
  select(-c(assessment_id,
            title,
            schoolid,
            total_pts,
            pts_scored)) %>%
  tidyr::spread(key = school_abbrev,
                value = pct_correct) %>%
  inner_join(assess_summary_region %>%
               ungroup() %>% 
               select(local_assessment_id,
                      reg_pct_correct),
             by = "local_assessment_id")

#Agg % Correct by IA - by Homeroom
assess_summary_hr <- assessment_stus_rev %>%
  mutate(home_room = if_else(local_student_id == 50140305 &
                               grepl("08.Algebra", local_assessment_id),
                             "8th Georgetown",
                             home_room)) %>%
  group_by(local_assessment_id,
           assessment_id,
           title, 
           schoolid, 
           home_room) %>%  
  summarize(total_pts = sum(points_possible),
            pts_scored = sum(points),
            pct_correct = pts_scored/total_pts) %>%
  left_join(schools,
            by = "schoolid")

#Joining school/IA % correct with homeroom % correct
assess_summary_hr_reg <- assess_summary_hr %>%
  ungroup() %>% 
  select(-c(assessment_id,
            title,
            schoolid,
            total_pts,
            pts_scored)) %>% 
  inner_join(assess_summary_school %>%
               ungroup() %>% 
               select(local_assessment_id,
                      schoolid,
                      school_abbrev,
                      grade_pct_correct = pct_correct),
             by = c("local_assessment_id",
                    "school_abbrev")) %>% 
  mutate(grade_level = stringr::str_extract(local_assessment_id, "0\\d"),
         grade_level = as.double(grade_level)) %>% 
  rename(school = school_abbrev) 

#Combining assessment results by standard with students and standards table
assessment_results_by_standard_0 <- agg_student_responses_standard %>%
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
  mutate(custom_code_0 = custom_code,
         custom_code = if_else(grepl("SCI", custom_code_0), gsub("ABDCID).|NGSSTV.", "", custom_code_0), custom_code_0)
         # no_questions_0 = number_of_questions, #1819 IA 3 - throwing out a question (6th Math #2 and 8th Math 24c)
         # no_questions_1 = if_else(custom_code_0 %in% "CCSS.MA.6.6.EE.3", no_questions_0 - 1, as.double(no_questions_0)),
         # no_questions_2 = if_else(custom_code_0 %in% "CCSS.MA.8.8.EE.8", no_questions_1 - 1, as.double(no_questions_1)),
         # number_of_questions = if_else(custom_code_0 %in% "CCSS.LA.7.RI.7.9", no_questions_2 - 1, as.double(no_questions_2)),
         # pts_pos = points_possible,
         # points_possible = if_else(custom_code_0 %in% "CCSS.LA.7.RI.7.9", pts_pos - 4, as.double(pts_pos))
         # no_questions_0 = number_of_questions, #1819 IA 1 - throwing out a question (5th ELA Q4, 5)
         # no_questions_1 = if_else(custom_code_0 %in% "CCSS.ELA-Literacy.RL.3.2", no_questions_0 - 1, as.double(no_questions_0)),
         # number_of_questions = if_else(custom_code_0 %in% "CCSS.ELA-Literacy.RL.3.3", no_questions_1 - 1, as.double(no_questions_1))#,
  ) %>%
  inner_join(assessment_ids,
             by = "assessment_id") %>%
  mutate(local_student_id = as.integer(local_student_id))


# points_ri_7_9 <- assessment_results_by_standard_0 %>% 
#   filter(custom_code %in% c("CCSS.LA.7.RI.7.9",
#                             "CCSS.LA.7.W.7.2")) %>% #select(student_id) %>% unique()
#   select(student_id,
#          assessment_id,
#          student_assessment_id,
#          local_assessment_id,
#          local_student_id,
#          custom_code,
#          points) %>% 
#   tidyr::spread(custom_code, points) %>% 
#   rename(ri_7_9_pts = `CCSS.LA.7.RI.7.9`,
#          w_7_2_pts = `CCSS.LA.7.W.7.2`) %>% 
#   mutate(points_ri_7_9 = ri_7_9_pts - w_7_2_pts,
#          custom_code = "CCSS.LA.7.RI.7.9") %>% 
#   select(-c(w_7_2_pts,
#             ri_7_9_pts))

assessment_results_by_standard <-   assessment_results_by_standard_0 #%>% 
# left_join(points_ri_7_9,
#           by = c("student_id",
#                  "assessment_id",
#                  "student_assessment_id",
#                  "local_assessment_id",
#                  "local_student_id",
#                  "custom_code")) %>% #select(custom_code, points, points_ri_7_9, points_possible) %>% View()
# mutate(points = if_else(custom_code %in% c("CCSS.LA.7.RI.7.9"), points_ri_7_9, points)) %>% 
# filter(!custom_code %in% c("CCSS.LA.7.W.7.7",
#                            "CCSS.LA.6.W.6.7",
#                            "CCSS.LA.5.W.5.9",
#                            "CCSS.LA.5.W.5.4"))

#% Mastered by Standard - School
standard_results_summary <- assessment_results_by_standard %>%
  filter(student_assessment_id %in% assessment_stus_rev$student_assessment_id) %>% 
  mutate(local_student_id  = as.integer(local_student_id)) %>%
  left_join(ps_stus %>%
              select(
                schoolid,
                student_number),
            by=c("local_student_id" = "student_number")) %>%
  mutate(mastered_level = if_else(points/points_possible >= .7, TRUE, FALSE)) %>% 
  group_by(schoolid,
           assessment_id,
           local_assessment_id,
           standard_id,
           custom_code,
           description) %>%
  summarize(n_students = n(),
            sum_mastered = sum(as.integer(mastered_level)),
            pct_mastered = sum_mastered/n_students,
            points_possible = sum(points_possible),
            sum_points = sum(points),
            pct_correct = sum_points/points_possible) %>%
  inner_join(schools, by = "schoolid")


#Student level standard performance
standards_all_assessments <- assessment_results_by_standard %>%
  filter(student_assessment_id %in% assessment_stus_rev$student_assessment_id) %>% 
  mutate(local_student_id  = as.double(local_student_id)) %>%
  left_join(ps_stus,
            by=c("local_student_id" = "student_number")) %>%
  left_join(schools %>%
              select(schoolid,
                     school = school_abbrev),
            by = "schoolid")

#Student level standard performance w/ mastery level  
student_stds <- standards_all_assessments %>% 
  mutate(custom_code = gsub("-", ".", custom_code),
         home_room = if_else(local_student_id == 50140305 &
                               grepl("08.Algebra", local_assessment_id), 
                             "8th Georgetown", 
                             home_room)) %>% 
  group_by(schoolid,
           assessment_id,
           local_assessment_id,
           standard_id,
           custom_code,
           description,
           student_id,
           local_student_id,
           first_name,
           last_name,
           home_room,
           points,
           points_possible) %>%
  summarise() %>% 
  mutate(pct_correct = points/points_possible,
         mastered_level = case_when(
           pct_correct < .21 ~ "Not Yet Mastered",
           pct_correct < .41 &
             pct_correct >= .21 ~ "Partially",
           pct_correct < .70 &
             pct_correct >= .41 ~ "Near Mastery",
           TRUE ~ "Mastered"
         )) %>% 
  left_join(schools,
            by = "schoolid") %>% 
  mutate(grade_level = stringr::str_extract(local_assessment_id, "0\\d"),
         grade_level = as.double(grade_level))

#Students per standard
m_total_students <- student_stds %>% 
  group_by(local_assessment_id,
           school_abbrev,
           custom_code) %>% 
  summarize(n_students = n())

#No Students Mastering ea standard
stds_mastered <- student_stds %>% 
  filter(mastered_level %in% "Mastered") %>% 
  group_by(local_assessment_id,
           school_abbrev,
           custom_code) %>% 
  summarize(mastered = n())

#% Mastered by standard - Regionally
region_m_band <- m_total_students %>% 
  left_join(stds_mastered,
            by = c("local_assessment_id",
                   "school_abbrev",
                   "custom_code")) %>% 
  group_by(local_assessment_id,
           custom_code) %>% 
  summarise(reg_mastered = sum(mastered, na.rm = TRUE),
            reg_n_students = sum(n_students, na.rm = TRUE),
            reg_pct = reg_mastered/reg_n_students)

#% Mastered by standard
mastered_band <- m_total_students  %>% 
  left_join(stds_mastered,
            by = c("local_assessment_id",
                   "school_abbrev",
                   "custom_code")) %>% 
  mutate(mastered = if_else(is.na(mastered), 0, as.double(mastered)),
         mastered_band = mastered/n_students) %>% 
  select(-c(mastered,
            n_students)) %>% 
  tidyr::spread(key = school_abbrev,
                value = mastered_band) %>% 
  #fill = 0) %>% 
  # mutate(KOA = ifelse(grepl("08", local_assessment_id),NA , KOA)) %>% 
  left_join(region_m_band %>% 
              select(local_assessment_id,
                     custom_code,
                     reg_pct),
            by = c("local_assessment_id",
                   "custom_code"))

#% Correct by Standard - School
standards_school <- standards_all_assessments %>%
  #standards_grade <- stds_mastered_by_student %>% 
  group_by(assessment_id, 
           local_assessment_id,
           standard_id,
           custom_code,
           description,
           school
  ) %>%
  mutate(n_students = n(),
         total_pts = points_possible * n_students,
         n_correct = sum(points),
         pct_correct = n_correct/total_pts) %>% 
  group_by(assessment_id, 
           local_assessment_id,
           standard_id,
           custom_code,
           description,
           school,
           points_possible,
           number_of_questions,
           n_students,
           total_pts,
           n_correct,
           pct_correct) %>% 
  summarise() %>% 
  ungroup() 

#% Correct by Standard - Region
standards_regional <- standards_all_assessments %>%
  group_by(assessment_id,
           local_assessment_id,
           custom_code,
           standard_id
  ) %>%
  summarise(n_students = n(),
            total_pts = sum(points_possible),
            pts_scored = sum(points),
            reg_pct_correct = pts_scored/total_pts) 

#Joining % correct by standards School & Region
stds_school_reg <- standards_school %>% 
  left_join(standards_regional %>%
              ungroup() %>% 
              select(local_assessment_id,
                     custom_code,
                     reg_pct_correct),
            by = c("local_assessment_id",
                   "custom_code")) %>% 
  mutate(grade_level = stringr::str_extract(local_assessment_id, "0\\d"),
         grade_level = as.double(grade_level),
         custom_code = gsub("-", ".", custom_code)) 


# writing_codes <- c("CCSS.LA.5.W.5.1",
#                    "CCSS.LA.6.W.6.1",
#                    "CCSS.LA.7.W.7.2",
#                    "CCSS.LA.8.W.8.2",
#                    "CCSS.LA.3.RL.3.3",
#                    "CCSS.LA.3.RI.3.9",
#                    "CCSS.LA.4.W.4.1")
#Writing table
ela_writing <- standards_all_assessments %>%
  # filter(custom_code %in% writing_codes) %>% 
  filter(grepl("W", custom_code)) %>% 
  group_by(assessment_id,
           local_assessment_id,
           schoolid,
           student_id,
           local_student_id,
           custom_code,
           points,
           points_possible) %>% 
  summarise() %>%
  rename(w_points = points,
         w_points_possible = points_possible)


#ELA Reading table
ela_reading <- assessment_stus_rev %>% 
  select(assessment_id,
         local_assessment_id,
         schoolid,
         student_id,
         local_student_id,
         total_points = points,
         total_points_possible = points_possible) %>% 
  inner_join(ela_writing,
             by = c("assessment_id",
                    "local_assessment_id",
                    "schoolid",
                    "student_id",
                    "local_student_id")) %>% 
  mutate(r_points = total_points - w_points,
         r_points_possible = total_points_possible - w_points_possible)


#ELA Multiple Choice - School & Region
ela_rollup_mc <- ela_reading %>% 
  left_join(schools ,
            by = "schoolid") %>% 
  group_by(assessment_id,
           local_assessment_id,
           # custom_code,
           school_abbrev) %>% 
  summarise(total_points= sum(r_points_possible),
            pts_scored = sum(r_points),
            pct_correct = pts_scored/total_points) %>% 
  select(-c(total_points,
            pts_scored)) %>% 
  tidyr::spread(key = school_abbrev,
                value = pct_correct) %>%
  left_join(ela_reading %>% 
              group_by(assessment_id,
                       local_assessment_id
                       # custom_code
              ) %>% 
              summarise(total_points= sum(r_points_possible),
                        pts_scored = sum(r_points),
                        reg_pct_correct = pts_scored/total_points) %>% 
              ungroup() %>% 
              select(local_assessment_id,
                     Region = reg_pct_correct),
            by = "local_assessment_id") %>% 
  ungroup() %>% 
  select(-c(assessment_id))


# ELA Writing - School & Region  
ela_rollup_cr <- ela_writing %>% 
  left_join(schools,
            by = "schoolid") %>% 
  group_by(assessment_id,
           local_assessment_id,
           # custom_code,
           school_abbrev) %>% 
  summarise(total_points= sum(w_points_possible),
            pts_scored = sum(w_points),
            pct_correct = pts_scored/total_points) %>% 
  select(-c(total_points,
            pts_scored)) %>% 
  tidyr::spread(key = school_abbrev,
                value = pct_correct) %>%
  left_join(ela_writing %>% 
              group_by(assessment_id,
                       local_assessment_id
                       # custom_code
              ) %>% 
              summarise(total_points= sum(w_points_possible),
                        pts_scored = sum(w_points),
                        reg_pct_correct = pts_scored/total_points) %>% 
              ungroup() %>% 
              select(local_assessment_id,
                     Region = reg_pct_correct),
            by = "local_assessment_id") %>% 
  ungroup() %>% 
  select(-c(assessment_id))



save(pct_correct_school,
     pct_correct_reg,
     regional_school_rollup,
     assess_summary_hr_reg,
     student_stds,
     stds_school_reg,
     ela_rollup_mc,
     ela_rollup_cr,
     bottom_band,
     mastered_band,
     bottom_band_reg,
     bottom_band_school_sub,
     n_btm_band_grade,
     file = here("data/1920_IA_1.Rda"))





