
# ------------------------ ## Assessment Tables - Regional ## --------------------------------

asr_assessments <- 
  agg_student_responses %>% 
  inner_join(assessments %>% 
               drop_fivetran_cols,
             by = "assessment_id") %>%
  dplyr::filter(!student_assessment_id %in% archived_assess$student_assessment_id) %>% 
  left_join(students,
            by = c("student_id")) %>% 
  mutate(sy = stringr::str_extract(local_assessment_id, "1718|1819|1920"),
         
         # mutate a DL column if the name of the assessment contains DL or if student number is from the list of DL students 
         # (necessary because no naming convention in 1718)
         dl_0 = grepl("DL", local_assessment_id),                       
         dl = if_else(local_student_id %in% dl_g5_st_ids$student_number &
                        local_assessment_id %in% "IA.1718.05.Math.1",
                      TRUE,
                      dl_0),
         
         # Goal = get IA number from local assessment ID
         # Tip to make sure this runs: when assigning local_identifiers on Illuminate, always make sure the test round number comes last
         # also make sure that DL Math IA 2 has the local identifier 3 at the end because 5,6,7,8 DL math only take 2 IAs but local assessment id 
         # needs to have 3 in it (ie IA.1920.07.Math.DL.3) for filtering purposes
         ia = case_when(
           sy %in% c("1718", "1920") ~ as.character(str_extract(local_assessment_id, "\\d$")),
           sy %in% "1819" & date_taken <= ymd("2018-12-12") ~ "1",
           sy %in% "1819" & date_taken >= ymd("2019-01-14") & date_taken <= ymd("2019-02-10") ~ "2",
           sy %in% "1819" & date_taken > ymd("2019-03-01") & date_taken < ymd("2019-04-10") ~ "3"),
         subject = stringr::str_extract(local_assessment_id, "ELA|Math|Science"))# %>% 
 #filter(!is.na(ia))# %>% tabyl(local_assessment_id) # make sure filter doesn't take anything out, problem if so

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
            by=c("local_student_id" = "student_number"))

assessment_stus_rev <- drop_duplicate_enrollments("1718") %>% 
  bind_rows(drop_duplicate_enrollments("1819")) %>% 
  bind_rows(drop_duplicate_enrollments("1920"))

mastery_pct_correct <- assessment_stus_rev %>% 
  filter(!is.na(ia)) %>% 
  mutate(pct_correct = points/points_possible,
          mastered_level = case_when(
           pct_correct < .21 ~ "Not Yet Mastered",
           pct_correct < .41 & pct_correct >= .21 ~  "Partially",
           pct_correct < .70 & pct_correct >= .41 ~  "Near Mastery",
           pct_correct >= .70 ~ "Mastered",
           TRUE ~ "NA")) %>%
  left_join(schools %>% 
              select(-school_name),
            by = "schoolid") %>% 
  
  # converting numeric school and grade level columns to character
  # in order for anti fuzzy join to table of tests/schools/grade levels that need to be removed
  
  mutate(schoolid = as.character(schoolid),                 
         grade_level = as.character(grade_level)) %>%        
  fuzzy_anti_join(filter_out_tests, by = c("local_assessment_id", "schoolid", "grade_level"),
                  match_fun = str_detect) %>%
  mutate(schoolid = as.double(schoolid),
         grade_level = as.integer(grade_level))


not_yet <- mastery_pct_correct %>% 
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

regional_mastery <- mastery_pct_correct %>% 
  group_by(ia,
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
  mutate(mastered_level = if_else(mastered_level == "Near Mastery", "Near",
                             if_else(mastered_level == "Not Yet Mastered", "Not Yet", mastered_level)),
    mastered_level = factor(mastered_level, levels = c("Not Yet",
                                                       "Partially",
                                                       "Near",
                                                       "Mastered")),
    pct_mastered = round(n_mastery/n_total *100,0),
    y_place = if_else(pct_mastered < 20, n_mastery+20, n_mastery/2),
    label_pct = sprintf("%s%%", pct_mastered),
    label_pct = sprintf("%s\n(%s)", label_pct, n_mastery)) 

# CAROLINE LEFT OFF HERE

ids_ia_1_2 <-  mastery_pct_correct %>%
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

# save(mastery_pct_correct,
#      not_yet,
#      regional_mastery,
#      assess_summary_region,
#      reg_1_2_compare,
#      stds_100,
#      all_students_testing,
#      all_students_testing_grade,
#      science_1718,
#      file = here::here("data/IA_1_db_cont.Rda"))


