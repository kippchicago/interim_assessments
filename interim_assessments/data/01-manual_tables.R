
# ------------------- ## Paramteters ## -------------------

current_first_year <- calc_academic_year(lubridate::today(),
                                         format = "first_year")

current_last_year <- calc_academic_year(today(), 
                                        format = "second_year")

ps_termid <- calc_ps_termid(current_first_year)

terms <- get_powerschool("terms") %>%
  filter(id >= ps_termid) %>%
  select(id,
         abbreviation,
         firstday,
         lastday) %>%
  collect()  %>%
  unique() %>%
  arrange(id)

sy_abbreviation_hypen <- terms$abbreviation[1]

sy_abbreviation <- str_replace(sy_abbreviation_hypen, "-", "")

year_table <- terms %>%
  filter(id == ps_termid)

year_term_id <- year_table$id

year_first_day <- year_table$firstday

# Determine which round of testing we're currently in based on today's month
# might need to change depending on what data you'd like to pull down

ia_round <- case_when(
  month(today()) %in% c(8, 9, 10, 11, 12) ~ "1",
  month(today()) %in% c(1, 2, 3) ~ "2",
  month(today()) %in% c(4, 5, 6, 7) ~ "3"
)

# Determine Year IDs
# Beginning with SY 17-18 (first year KIPP Chicago implemented IAs) and 
# ending with the first year of the current school year inputted in to the function

year_ids <- year_id_list_function(current_first_year)


# ------------------- ## Manual Tables ## -------------------------

schools <- tibble(schoolid = c(78102, 7810, 400146, 4001462, 400163, 4001802, 400180),
                  school_name = c("Ascend Primary", "Ascend Middle", "Academy", "Academy Primary", "Bloom", "One Primary", "One Academy"),
                  school_abbrev =c("KAP", "KAMS", "KAC", "KACP", "KBCP", "KOP", "KOA"))


# ------------------- ## Past IAs ## -----------------------------
# Needed to have manual tables due to vast number of customizable DL tests

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

# ------------------------ ## DL Students that took past IAs ## ----------------
# Old system = excluding students from Gen Ed analysis by filter by student number
# Tables created from Google sheet titled "Student Rostering for Interim Assessments (https://docs.google.com/spreadsheets/d/1v02EirKo1M3CsC0-mS_wTsAzuTXOOpUwTRftch8g9Hw/edit#gid=1292661063)

dl_g5_st_ids <- tribble(
  ~student_number,
  50192084,
  50202449,
  50181237,
  50173550,
  50232481,
  50206506,
  50173550,
  50297673,
  50024289,
  45273873,
  50145043,
  50230976,
  50099742,
  45161951,
  50123280,
  50139496,
  50122588,
  50034984,
  43654209,
  50253721,
  50464769,
  50111943,
  45255387,
  50484614,
  50058266,
  50110158,
  50022543,
  50484441,
  50063452,
  44991640,
  50061318,
  50200736,
  50190356,
  50129792,
  50173245,
  50231442,
  50160150,
  50113767,
  50290923,
  50408898,
  50088975,
  50110514,
  50194348,
  45673324,
  50051252,
  50154483,
  50186098,
  50131487,
  50140857,
  50050466,
  45041689,
  45303802,
  50132145,
  45417611,
  45665836,
  50263925,
  50195932,
  50303490,
  50207657,
  50199284,
  50172546,
  50173041,
  50189058,
  50243740,
  50232569,
  50136158,
  50135089,
  50175166,
  43237403,
  50077686,
  50102637,
  50218130,
  50254231,
  50149762,
  50132017,
  50201553,
  50410555,
  50151099,
  50230016,
  50236442,
  50159567,
  44461986,
  50090053,
  50173756,
  50138653,
  45619400,
  50171634,
  50060413,
  50299507,
  50229500,
  50317437,
  50049503,
  50079483,
  50178658,
  50124035,
  50496985,
  50238034,
  50283026,
  50149763
)


# -------------------------------- ## Science Performance Bands ## -------------------
# Weren't set when test was first administered and can't change on Illuminate now

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


# ---------------------- ## Tests to Remove ## --------------------------------

# Some tests need to be removed from analysis because no meaningful comparison across district

# removing KAC/KACP 4th taking 3rd ELA 1 during IA Round 1 in 1819 and 1920 then 4th ELA 1 during IA Round 2
# removing KOA 7th taking 8th Math in any round because they're advanced class is on different schedule

filter_out_tests <- tribble(
  ~local_assessment_id, ~schoolid, ~grade_level,
  "03.ELA.1", 400146, 4,
  "04.ELA.1", 400146, 4,
  "03.ELA.1", 4001462, 4,
  "04.ELA.1", 4001462, 4,
  "08.Math", 400180, 7) %>%
  mutate(schoolid = as.character(schoolid),
         grade_level = as.character(grade_level))





