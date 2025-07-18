### LISS Data Transformation Politics

rm(list=ls())

# packages
pacman::p_load(data.table, tidyverse, readstata13, furrr)

# set wd
setwd("~/Dropbox (Princeton)/Data/Panel_Surveys/LISS/politics/")


# read .dta files  --------------------------------------------------------

# use multisession evaluation
plan(multisession)

pol <- 
  # list all .dta files in directory
  list.files(pattern = "*.dta") |>
  # read all .dta files in directory
  future_map(~read.dta13(.)) |>
  # delete first five characters of all variable names starting with c
  future_map(~rename_with(.,str_sub, start = 6L, .cols = starts_with("c"))) |>
  # delete first first characters of all variable names starting with _
  future_map(~rename_with(.,str_sub, start = 2L, .cols = starts_with("_"))) |>
  # get column on year and month
  future_map(~if(!("m" %in% names(.x))) .x else .x |> rename(date = m)) |>
  future_map(~if(!("date" %in% names(.x))) .x else .x |> mutate(year = lubridate::year(lubridate::ym(date)))) |>
  future_map(~if(!("date" %in% names(.x))) .x else .x |> mutate(month = lubridate::month(lubridate::ym(date)))) |>
  future_map(~if(!("maandnr" %in% names(.x))) .x else .x |> mutate(year = lubridate::year(lubridate::ym(maandnr)))) |>
  future_map(~if(!("maandnr" %in% names(.x))) .x else .x |> mutate(month = lubridate::month(lubridate::ym(maandnr)))) |>
  future_map(~if(!("m1" %in% names(.x))) .x else .x |> mutate(year = lubridate::year(lubridate::ym(m1)))) |>
  future_map(~if(!("m1" %in% names(.x))) .x else .x |> mutate(month = lubridate::month(lubridate::ym(m1)))) |>
  future_map(~if(!("m2" %in% names(.x))) .x else .x |> mutate(year2 = lubridate::year(lubridate::ym(m3)))) |>
  future_map(~if(!("m2" %in% names(.x))) .x else .x |> mutate(month2 = lubridate::month(lubridate::ym(m2)))) |>
  future_map(~if(!("m3" %in% names(.x))) .x else .x |> mutate(year3 = lubridate::year(lubridate::ym(m3)))) |>
  future_map(~if(!("m3" %in% names(.x))) .x else .x |> mutate(month3 = lubridate::month(lubridate::ym(m3)))) |>
  # every column to character
  future_map(~mutate(.x, across(everything(), as.character))) |>
  # bind all data frames
  bind_rows() |>
  # get one year and month variable
  mutate(year = ifelse(!is.na(year2),
                       year2,
                       ifelse(!is.na(year3),
                              year3,
                              year)),
         month = ifelse(!is.na(month2),
                        month2,
                        ifelse(!is.na(month3),
                               month3,
                               month)),
         date = ifelse(!is.na(maandnr_lang),
                       maandnr_lang,
                       ifelse(!is.na(maandnr_deel1),
                              maandnr_deel1,
                              ifelse(!is.na(maandnr_a),
                                     maandnr_a,
                                     date))),
         year = ifelse(is.na(year),
                       lubridate::year(lubridate::ym(date)),
                       year),
         month = ifelse(is.na(month),
                        lubridate::month(lubridate::ym(date)),
                        month),
         month = paste0("0",month),
         month = recode(month,
                        `010` = "10",
                        `011` = "11",
                        `012` = "12"),
         # get date
         date = paste0(year,month),
         # get wave
         wave = recode(date, 
                       `200712` = "08",
                       `200803` = "08",
                       `200812` = "09",
                       `200901` = "09",
                       `200912` = "10",
                       `201001` = "10",
                       `201002` = "10",
                       `201012` = "11",
                       `201101` = "11",
                       `201112` = "12",
                       `201201` = "12",
                       `201212` = "13",
                       `201301` = "13",
                       `201312` = "14",
                       `201401` = "14",
                       `201512` = "16",
                       `201601` = "16",
                       `201612` = "17",
                       `201701` = "17",
                       `201702` = "17",
                       `201712` = "18",
                       `201801` = "18",
                       `201802` = "18",
                       `201803` = "18",
                       `201812` = "19",
                       `201901` = "19",
                       `201902` = "19",
                       `201903` = "19",
                       `201912` = "20",
                       `202001` = "20",
                       `202002` = "20",
                       `202003` = "20",
                       `202012` = "21",
                       `202101` = "21",
                       `202102` = "21",
                       `202103` = "21",
                       `202112` = "22",
                       `202201` = "22",
                       `202202` = "22",
                       `202203` = "22",
                       `202212` = "23",
                       `202301` = "23",
                       `202302` = "23",
                       `202303` = "23")) |>
# Rename ------------------------------------------------------------------
  # select & rename
  select(
    nomem_encr,nohouse_encr,wave,date,
    year,month,
    age_poldf = `160`,
    satisfac_gov_actions = `001`,
    satisfac_gov = `030`,
    satisfac_parl = `031`,
    satisfac_legsys = `032`,
    satisfac_police = `033`,
    satisfac_politicians = `034`,
    satisfac_parties = `035`,
    satisfac_EP = `036`,
    satisfac_UN = `037`,
    satisfac_media = `038`,
    satisfac_military = `039`,
    satisfac_edusys = `040`,
    satisfac_healthcare = `041`,
    satisfac_science = `042`,
    satisfac_econ = `043`,
    satisfac_dem = `044`,
    satisfac_firms_pers = `045`,
    satisfac_firms_internet = `046`,
    conf_gov = `013`,
    conf_parl = `014`,
    conf_legsys = `015`,
    conf_police = `016`,
    conf_politicians = `017`,
    conf_parties = `018`,
    conf_EP = `019`,
    conf_UN = `020`,
    conf_media = `021`,
    conf_military = `022`,
    conf_edusys = `023`,
    conf_healthcare = `024`,
    conf_science = `025`,
    conf_econ = `026`,
    conf_dem = `027`,
    conf_firms_pers = `028`,
    conf_firms_internet = `029`,
    news_follow = `167`,
    news_follow_how = `168`,
    news_tv_radio = `002`,
    news_internet = `003`,
    news_socialmedia = `335`,
    newspaper_int_read = `011`,
    news_freenewspaper = `004`,
    news_boughtnewspaper_subscription = `005`,
    news_paidnewspaper = `166`,
    news_podcasts = `336`,
    news_none_hardly = `006`,
    news_dontknow = `007`,
    news_interest = `008`,
    newspaper_nat_read = `009`,
    news_conversation_participate = `010`,
    politics_interest = `012`,
    parliamentarians_dontcare_myopinion = `047`,
    parties_dontcare_myopinion = `048`,
    noinfluence_on_pol = `049`,
    capable_active_role_pol = `050`,
    clear_pic_polissue = `051`,
    pol_complicated = `052`,
    vote_last_parl_elect = `053`,
    party_last_parl_elect_Nov2006 = `054`,
    party_last_parl_elect_Jun2010 = `169`,
    party_last_parl_elect_Sep2012 = `207`,
    party_last_parl_elect_Mar2017 = `307`,
    party_last_parl_elect_other_Nov2006 = `055`,
    party_last_parl_elect_other_Jun2010 = `170`,
    party_last_parl_elect_other = `208`,
    party_last_prov_elect_Mar2007 = `056`,
    party_last_prov_elect_Mar2011 = `195`,
    party_last_prov_elect_other_Mar2007 = `057`,
    party_last_prov_elect_other_Mar2011 = `196`,
    elect_today_vote = `243`,
    elect_today_party_08 = `058`,
    elect_today_party_11 = `171`,
    elect_today_party_13 = `209`,
    elect_today_party_17 = `244`,
    elect_today_party_18 = `308`,
    elect_today_party_other_08 = `059`,
    elect_today_party_other_11 = `172`,
    elect_today_party_other_13 = `210`,
    elect_today_party_other_22 = `210`,
    elect_today_notvote = `245`,
    elect_today_VVD = `246`,
    elect_today_D66 = `251`,
    elect_today_PVV = `248`,
    elect_today_CDA = `250`,
    elect_today_SP = `249`,
    elect_today_PvdA = `247`,
    elect_today_GL = `253`,
    elect_today_FvD = `304`,
    elect_today_PvdD = `255`,
    elect_today_CU = `252`,
    elect_today_Volt = `321`,
    elect_today_JA21 = `322`,
    elect_today_SGP = `254`,
    elect_today_DENK = `257`,
    elect_today_50PLUS = `256`,
    elect_today_BBB = `323`,
    elect_today_BIJ1 = `324`,
    elect_today_Blank = `260`,
    elect_today_VNL = `258`,
    elect_today_other = `259`,
    elect_today_dontknow = `261`,
    elect_today_notsay = `262`,
    elect_today_whichother = `263`,
    own_polinfluence_through_media = `065`,
    own_polinfluence_through_party = `066`,
    own_polinfluence_through_hearing = `067`,
    own_polinfluence_through_politician = `068`,
    own_polinfluence_through_group = `069`,
    own_polinfluence_through_protest = `070`,
    own_polinfluence_through_online = `071`,
    own_polinfluence_through_other = `072`,
    own_polinfluence_through_no = `073`,
    own_polinfluence_through_dontknow = `074`,
    own_polinfluence_through_whichother = `075`,
    # Parties
    thermo_VVD_08 = `078`,
    thermo_VVD_11 = `173`,
    thermo_VVD_22 = `211`,
    thermo_D66_08 = `081`,
    thermo_D66_11 = `178`,
    thermo_D66_22 = `216`,
    thermo_PVV_08 = `085`,
    thermo_PVV_11 = `175`,
    thermo_PVV_22 = `213`,
    thermo_CDA_08 = `076`,
    thermo_CDA_11 = `176`,
    thermo_CDA_22 = `215`,
    thermo_SP_08 = `079`,
    thermo_SP_11 = `177`,
    thermo_SP_22 = `214`,
    thermo_PvdA_08 = `077`,
    thermo_PvdA_11 = `174`,
    thermo_PvdA_22 = `212`,
    thermo_GL_08 = `080`,
    thermo_GL_11 = `179`,
    thermo_GL_22 = `218`,
    thermo_FvD = `305`,
    thermo_PvdD_08 = `086`,
    thermo_PvdD_11 = `182`,
    thermo_PvdD_22 = `220`,
    thermo_CU_08 = `082`,
    thermo_CU_11 = `180`,
    thermo_CU_22 = `217`,
    thermo_TON = `084`,
    thermo_Volt = `325`,
    thermo_JA21 = `326`,
    thermo_SGP_08 = `083`,
    thermo_SGP_11 = `181`,
    thermo_SGP_22 = `219`,
    thermo_DENK = `264`,
    thermo_50PLUS = `221`,
    thermo_BBB = `327`,
    thermo_BIJ1 = `328`,
    thermo_VNL = `265`,
    # Politicians
    thermo_Balkenende = `087`,
    thermo_Bos = `088`,
    thermo_Rutte_08 = `089`,
    thermo_Rutte_11 = `183`,
    thermo_Rutte_13 = `222`,
    thermo_Marijnissen = `090`,
    thermo_Halsema_08 = `091`,
    thermo_Halsema_11 = `189`,
    thermo_Pechthold_08 = `092`,
    thermo_Pechthold_11 = `188`,
    thermo_Pechthold_13 = `227`,
    thermo_Rouvoet_08 = `093`,
    thermo_Rouvoet_11 = `190`,
    thermo_Vlies = `094`,
    thermo_Verdonk = `095`,
    thermo_Wilders_08 = `096`,
    thermo_Wilders_11 = `185`,
    thermo_Wilders_13 = `224`,
    thermo_Thieme_08 = `097`,
    thermo_Thieme_11 = `192`,
    thermo_Thieme_13 = `231`,
    thermo_Sap = `197`,
    thermo_Slob_11 = `198`,
    thermo_Slob_13 = `228`,
    thermo_Ojek = `229`,
    thermo_Cohen = `184`,
    thermo_Verhagen = `186`,
    thermo_Roemer_11 = `187`,
    thermo_Roemer_13 = `225`,
    thermo_Samson = `223`,
    thermo_Haersma = `226`,
    thermo_Kaag = `318`,
    thermo_Hoekstra = `317`,
    thermo_Haan = `319`,
    thermo_Ploumen = `329`,
    thermo_Nagel = `239`,
    thermo_Klaver = `240`,
    thermo_Baudet = `306`,
    thermo_Jetten = `313`,
    thermo_Heerma = `314`,
    thermo_Ouwehand = `315`,
    thermo_Kuzu_17 = `268`,
    thermo_Kuzu_20 = `316`,
    thermo_Roos = `269`,
    thermo_Asscher = `266`,
    thermo_Segers = `267`,
    thermo_Dassen = `331`,
    thermo_Eerdmans = `332`,
    thermo_Staaij_11 = `191`,
    thermo_Staaij_13 = `230`,
    thermo_Krol_13 = `232`,
    thermo_Krol_16 = `241`,
    thermo_Azarkan_19 = `312`,
    thermo_Azarkan_21 = `320`,
    thermo_Rooijen = `330`,
    thermo_Plas = `333`,
    thermo_Simons = `334`,
    thermo_Kuiken = `337`,
    thermo_Bikker = `338`,
    # partisan
    partisan = `199`, # Which political party are you an adherent of?
    party_attraction = `200`, # Do you feel more attracted to one of the political parties than to others?
    partisan_party_12 = `201`,
    partisan_party_13 = `233`,
    partisan_party_18 = `309`,
    partisan_party_other_12 = `202`,
    partisan_party_other_13 = `234`,
    partisan_conviction = `205`,
    party_attraction_party_12 = `203`,
    party_attraction_party_13 = `235`,
    party_attraction_party_18 = `310`,
    party_attraction_party_other_12 = `204`,
    party_attraction_party_other_13 = `236`,
    party_member = `098`, # Are you a member of a political party?
    party_member_party_08 = `099`,
    party_member_party_11 = `193`,
    party_member_party_13 = `237`,
    party_member_party_18 = `311`,
    party_member_party_other_08 = `100`,
    party_member_party_other_11 = `194`,
    party_member_party_other_13 = `238`,
    lr_scale = `101`,
    pref_euthanasia = `102`,
    pref_redistribution = `103`,
    pref_immigrant_adapt = `104`,
    pref_EU_unification = `105`,
    pref_1choice = `106`,
    pref_2choice = `107`,
    pref_3choice = `108`,
    workingmother_relation = `109`,
    workingmother_childsuffer = `110`,
    workingmother_famsuffer = `111`,
    fam_bothcontribute_income = `112`,
    fam_father_earnmoney = `113`,
    fam_father_morehouehold = `114`,
    fam_father_morechildcare = `115`,
    foreign_cultures = `116`,
    foreign_accepted = `117`,
    foreign_asylum = `118`,
    foreign_socialsec = `119`,
    foreign_toomany = `120`,
    foreign_notaccepted = `121`,
    foreign_sectors = `122`,
    foreign_neighborhood = `123`,
    marry_happy = `124`,
    marry_children = `125`,
    marry_singleparent = `126`,
    marry_finewithout = `127`,
    marry_living = `128`,
    marry_divorce = `129`,
    marry_divorce_children = `130`,
    childparents_care = `131`,
    childparents_live = `132`,
    childparents_visit = `133`,
    childparents_leave = `134`,
    children_complete = `135`,
    children_duty = `136`,
    children_atease = `137`,
    children_nothappy = `138`,
    work_duty = `139`,
    work_hard = `140`,
    work_happy = `141`,
    work_priority = `142`,
    women_job_baby = `143`,
    women_job_noschool = `144`,
    women_job_primary = `145`,
    women_job_secondary = `146`,
    unions_politics = `147`,
    unions_advise = `148`,
    union_fight = `149`,
    class_diff_smaller = `150`,
    gender_rearing = `151`,
    gender_education = `152`,
    gender_reared_lib = `153`,
    gender_firms = `154`,
    vote_votingcomputer = `060`,
    vote_votingslip = `061`,
    vote_phone = `062`,
    vote_homecomputer = `063`,
    vote_mail = `064`
    ) |>

# Thermo variables --------------------------------------------------------
  mutate(thermo_VVD = ifelse(!is.na(thermo_VVD_08),
                             thermo_VVD_08,
                             ifelse(!is.na(thermo_VVD_11),
                                    thermo_VVD_11,
                                    ifelse(!is.na(thermo_VVD_22),
                                           thermo_VVD_22,
                                           NA))),
         thermo_D66 = ifelse(!is.na(thermo_D66_08),
                             thermo_D66_08,
                             ifelse(!is.na(thermo_D66_11),
                                    thermo_D66_11,
                                    ifelse(!is.na(thermo_D66_22),
                                           thermo_D66_22,
                                           NA))),
         thermo_PVV = ifelse(!is.na(thermo_PVV_08),
                             thermo_PVV_08,
                             ifelse(!is.na(thermo_PVV_11),
                                    thermo_PVV_11,
                                    ifelse(!is.na(thermo_PVV_22),
                                           thermo_PVV_22,
                                           NA))),
         thermo_CDA = ifelse(!is.na(thermo_CDA_08),
                             thermo_CDA_08,
                             ifelse(!is.na(thermo_CDA_11),
                                    thermo_CDA_11,
                                    ifelse(!is.na(thermo_CDA_22),
                                           thermo_CDA_22,
                                           NA))),
         thermo_SP = ifelse(!is.na(thermo_SP_08),
                             thermo_SP_08,
                             ifelse(!is.na(thermo_SP_11),
                                    thermo_SP_11,
                                    ifelse(!is.na(thermo_SP_22),
                                           thermo_SP_22,
                                           NA))),
         thermo_PvdA = ifelse(!is.na(thermo_PvdA_08),
                             thermo_PvdA_08,
                             ifelse(!is.na(thermo_PvdA_11),
                                    thermo_PvdA_11,
                                    ifelse(!is.na(thermo_PvdA_22),
                                           thermo_PvdA_22,
                                           NA))),
         thermo_GL = ifelse(!is.na(thermo_GL_08),
                             thermo_GL_08,
                             ifelse(!is.na(thermo_GL_11),
                                    thermo_GL_11,
                                    ifelse(!is.na(thermo_GL_22),
                                           thermo_GL_22,
                                           NA))),
         thermo_PvdD = ifelse(!is.na(thermo_PvdD_08),
                             thermo_PvdD_08,
                             ifelse(!is.na(thermo_PvdD_11),
                                    thermo_PvdD_11,
                                    ifelse(!is.na(thermo_PvdD_22),
                                           thermo_PvdD_22,
                                           NA))),
         thermo_CU = ifelse(!is.na(thermo_CU_08),
                             thermo_CU_08,
                             ifelse(!is.na(thermo_CU_11),
                                    thermo_CU_11,
                                    ifelse(!is.na(thermo_CU_22),
                                           thermo_CU_22,
                                           NA))),
         thermo_SGP = ifelse(!is.na(thermo_SGP_08),
                             thermo_SGP_08,
                             ifelse(!is.na(thermo_SGP_11),
                                    thermo_SGP_11,
                                    ifelse(!is.na(thermo_SGP_22),
                                           thermo_SGP_22,
                                           NA))),
         # Politicians
         thermo_Rutte = ifelse(!is.na(thermo_Rutte_08),
                             thermo_Rutte_08,
                             ifelse(!is.na(thermo_Rutte_11),
                                    thermo_Rutte_11,
                                    ifelse(!is.na(thermo_Rutte_13),
                                           thermo_Rutte_13,
                                           NA))),
         thermo_Pechthold = ifelse(!is.na(thermo_Pechthold_08),
                             thermo_Pechthold_08,
                             ifelse(!is.na(thermo_Pechthold_11),
                                    thermo_Pechthold_11,
                                    ifelse(!is.na(thermo_Pechthold_13),
                                           thermo_Pechthold_13,
                                           NA))),
         thermo_Wilders = ifelse(!is.na(thermo_Wilders_08),
                             thermo_Wilders_08,
                             ifelse(!is.na(thermo_Wilders_11),
                                    thermo_Wilders_11,
                                    ifelse(!is.na(thermo_Wilders_13),
                                           thermo_Wilders_13,
                                           NA))),
         thermo_Thieme = ifelse(!is.na(thermo_Thieme_08),
                             thermo_Thieme_08,
                             ifelse(!is.na(thermo_Thieme_11),
                                    thermo_Thieme_11,
                                    ifelse(!is.na(thermo_Thieme_13),
                                           thermo_Thieme_13,
                                           NA))),
         thermo_Halsema = ifelse(!is.na(thermo_Halsema_08),
                             thermo_Halsema_08,
                             ifelse(!is.na(thermo_Halsema_11),
                                    thermo_Halsema_11,
                                    NA)),
         thermo_Rouvoet = ifelse(!is.na(thermo_Rouvoet_08),
                             thermo_Rouvoet_08,
                             ifelse(!is.na(thermo_Rouvoet_11),
                                    thermo_Rouvoet_11,
                                    NA)),
         thermo_Slob = ifelse(!is.na(thermo_Slob_11),
                             thermo_Slob_11,
                             ifelse(!is.na(thermo_Slob_13),
                                    thermo_Slob_13,
                                    NA)),
         thermo_Roemer = ifelse(!is.na(thermo_Roemer_11),
                             thermo_Roemer_11,
                             ifelse(!is.na(thermo_Roemer_13),
                                    thermo_Roemer_13,
                                    NA)),
         thermo_Kuzu = ifelse(!is.na(thermo_Kuzu_17),
                              thermo_Kuzu_17,
                              ifelse(!is.na(thermo_Kuzu_20),
                                     thermo_Kuzu_20,
                                     NA)),
         thermo_Staaij = ifelse(!is.na(thermo_Staaij_11),
                                thermo_Staaij_11,
                                ifelse(!is.na(thermo_Staaij_13),
                                       thermo_Staaij_13,
                                       NA)),
         thermo_Krol = ifelse(!is.na(thermo_Krol_13),
                              thermo_Krol_13,
                                ifelse(!is.na(thermo_Krol_16),
                                       thermo_Krol_16,
                                       NA)),
         thermo_Azarkan = ifelse(!is.na(thermo_Azarkan_19),
                                 thermo_Azarkan_19,
                              ifelse(!is.na(thermo_Azarkan_21),
                                     thermo_Azarkan_21,
                                     NA))
         ) |>
  # delete unnecessary variables
  select(-c(thermo_VVD_08:thermo_GL_22,
            thermo_PvdD_08:thermo_CU_22,
            thermo_SGP_08:thermo_SGP_22,
            thermo_Rutte_08:thermo_Rutte_13,
            thermo_Halsema_08:thermo_Rouvoet_11,
            thermo_Wilders_08:thermo_Thieme_13,
            thermo_Slob_11:thermo_Slob_13,
            thermo_Roemer_11:thermo_Roemer_13,
            thermo_Kuzu_17:thermo_Kuzu_20,
            thermo_Staaij_11:thermo_Azarkan_21)) |>
  # recode wave to NA if NA01,NA02
  mutate(wave = na_if(wave, "NA01"),
         wave = na_if(wave, "NA02")) |>
  # arrange
  arrange(nomem_encr) |>
  relocate(c(thermo_VVD:thermo_SGP), .before = thermo_FvD) |>
  relocate(c(thermo_Rutte:thermo_Azarkan), .before = thermo_Marijnissen) |>

# Vote choice & partisanship variables ------------------------------------
# get vote choice dummies
  mutate(
## Vote choice last election -----------------------------------------------
    # CDA (Christian democrat party)
    party_last_parl_elect_CDA = ifelse((party_last_parl_elect_Nov2006 == 1 & !is.na(party_last_parl_elect_Nov2006)) |
                                         (party_last_parl_elect_Jun2010 ==  4 & !is.na(party_last_parl_elect_Nov2006)) |
                                         (party_last_parl_elect_Sep2012 ==  5 & !is.na(party_last_parl_elect_Nov2006)) |
                                         (party_last_parl_elect_Mar2017 == 3 & !is.na(party_last_parl_elect_Nov2006)),
                                       1,0),
    # PvdA (labor party)
    party_last_parl_elect_pvdA = ifelse((party_last_parl_elect_Nov2006 == 2 & !is.na(party_last_parl_elect_Nov2006)) |
                                         (party_last_parl_elect_Jun2010 ==  2 & !is.na(party_last_parl_elect_Jun2010)) |
                                         (party_last_parl_elect_Sep2012 ==  2 & !is.na(party_last_parl_elect_Sep2012)) |
                                         (party_last_parl_elect_Mar2017 == 7 & !is.na(party_last_parl_elect_Mar2017)),
                                       1,0),
    # VVD (liberal party)
    party_last_parl_elect_VVD =  ifelse((party_last_parl_elect_Nov2006 == 3 & !is.na(party_last_parl_elect_Nov2006)) |
                                          (party_last_parl_elect_Jun2010 ==  1 & !is.na(party_last_parl_elect_Jun2010)) |
                                          (party_last_parl_elect_Sep2012 ==  1 & !is.na(party_last_parl_elect_Sep2012)) |
                                          (party_last_parl_elect_Mar2017 == 1 & !is.na(party_last_parl_elect_Mar2017)),
                                        1,0),
    # Socialistische Partij (SP) (socialist party)
    party_last_parl_elect_SP = ifelse((party_last_parl_elect_Nov2006 == 4 & !is.na(party_last_parl_elect_Nov2006)) |
                                        (party_last_parl_elect_Jun2010 ==  5 & !is.na(party_last_parl_elect_Jun2010)) |
                                        (party_last_parl_elect_Sep2012 ==  4 & !is.na(party_last_parl_elect_Sep2012)) |
                                        (party_last_parl_elect_Mar2017 == 6 & !is.na(party_last_parl_elect_Mar2017)),
                                      1,0),
    # GroenLinks (green party)
    party_last_parl_elect_GL = ifelse((party_last_parl_elect_Nov2006 == 5 & !is.na(party_last_parl_elect_Nov2006)) |
                                        (party_last_parl_elect_Jun2010 ==  7 & !is.na(party_last_parl_elect_Jun2010)) |
                                        (party_last_parl_elect_Sep2012 ==  8 & !is.na(party_last_parl_elect_Sep2012)) |
                                        (party_last_parl_elect_Mar2017 == 5 & !is.na(party_last_parl_elect_Mar2017)),
                                      1,0),
    # Lijst vijf Fortuyn \ LPF (Fortuyn party)
    party_last_parl_elect_LPF = ifelse(party_last_parl_elect_Nov2006 == 6,1,0),
    # D66 (social-liberal party)
    party_last_parl_elect_D66 = ifelse((party_last_parl_elect_Nov2006 == 7 & !is.na(party_last_parl_elect_Nov2006)) |
                                         (party_last_parl_elect_Jun2010 ==  6 & !is.na(party_last_parl_elect_Jun2010)) |
                                         (party_last_parl_elect_Sep2012 ==  6 & !is.na(party_last_parl_elect_Sep2012)) |
                                         (party_last_parl_elect_Mar2017 == 4 & !is.na(party_last_parl_elect_Mar2017)),
                                       1,0),
    # ChristenUnie (Christian union party)
    party_last_parl_elect_CU = ifelse((party_last_parl_elect_Nov2006 == 8 & !is.na(party_last_parl_elect_Nov2006)) |
                                        (party_last_parl_elect_Jun2010 ==  8 & !is.na(party_last_parl_elect_Jun2010)) |
                                        (party_last_parl_elect_Sep2012 ==  7 & !is.na(party_last_parl_elect_Sep2012)) |
                                        (party_last_parl_elect_Mar2017 == 8 & !is.na(party_last_parl_elect_Mar2017)),
                                      1,0),
    # SGP (Christian Reformed party)
    party_last_parl_elect_SGP = ifelse((party_last_parl_elect_Nov2006 == 9 & !is.na(party_last_parl_elect_Nov2006)) |
                                         (party_last_parl_elect_Jun2010 ==  9 & !is.na(party_last_parl_elect_Jun2010)) |
                                         (party_last_parl_elect_Sep2012 ==  9 & !is.na(party_last_parl_elect_Sep2012)) |
                                         (party_last_parl_elect_Mar2017 == 11 & !is.na(party_last_parl_elect_Mar2017)),
                                       1,0),
    # Groep Wilders \ Partij voor de Vrijheid (Wilders' freedom party)
    party_last_parl_elect_PVV = ifelse((party_last_parl_elect_Nov2006 == 11 & !is.na(party_last_parl_elect_Nov2006)) |
                                         (party_last_parl_elect_Jun2010 ==  3 & !is.na(party_last_parl_elect_Jun2010)) |
                                         (party_last_parl_elect_Sep2012 ==  3 & !is.na(party_last_parl_elect_Sep2012)) |
                                         (party_last_parl_elect_Mar2017 == 2 & !is.na(party_last_parl_elect_Mar2017)),
                                       1,0),
    # Partij voor de Dieren (animal welfare party)
    party_last_parl_elect_PvdD = ifelse((party_last_parl_elect_Nov2006 == 12 & !is.na(party_last_parl_elect_Nov2006)) |
                                          (party_last_parl_elect_Jun2010 ==  10 & !is.na(party_last_parl_elect_Jun2010)) |
                                          (party_last_parl_elect_Sep2012 ==  10 & !is.na(party_last_parl_elect_Sep2012)) |
                                          (party_last_parl_elect_Mar2017 == 9 & !is.na(party_last_parl_elect_Mar2017)),
                                        1,0),
    # 50Plus (fifty plus party)
    party_last_parl_elect_50PLUS = ifelse((party_last_parl_elect_Sep2012 ==  11 & !is.na(party_last_parl_elect_Sep2012)) |
                                            (party_last_parl_elect_Mar2017 == 10 & !is.na(party_last_parl_elect_Mar2017)),
                                          1,0),
    # DENK
    party_last_parl_elect_DENK = ifelse(party_last_parl_elect_Mar2017 == 12,1,0),
    # Forum voor Democratie
    party_last_parl_elect_FvD = ifelse(party_last_parl_elect_Mar2017 == 13,1,0),
    # Volt Nederland
    party_last_parl_elect_Volt = ifelse(party_last_parl_elect_Mar2017 == 16,1,0),
    # JA21
    party_last_parl_elect_JA21 = ifelse(party_last_parl_elect_Mar2017 == 17,1,0),
    # BBB (Farmer-Citizen Movement)
    party_last_parl_elect_BBB = ifelse(party_last_parl_elect_Mar2017 == 18,1,0),
    # BIJ1
    party_last_parl_elect_BIJ1 = ifelse(party_last_parl_elect_Mar2017 == 19,1,0),
    # other party
    party_last_parl_elect_other = ifelse((party_last_parl_elect_Nov2006 == 13 & !is.na(party_last_parl_elect_Nov2006)) |
                                           (party_last_parl_elect_Jun2010 ==  11 & !is.na(party_last_parl_elect_Jun2010)) |
                                           (party_last_parl_elect_Sep2012 ==  12 & !is.na(party_last_parl_elect_Sep2012)) |
                                           (party_last_parl_elect_Mar2017 == 15 & !is.na(party_last_parl_elect_Mar2017)),
                                         1,0),
## Vote choice last provincial election ------------------------------------
    # CDA (Christian democrat party)
    party_last_prov_elect_CDA = ifelse((party_last_prov_elect_Mar2007 == 3 & !is.na(party_last_prov_elect_Mar2007)) |
                                         (party_last_prov_elect_Mar2011 == 5 & !is.na(party_last_prov_elect_Mar2011)),
                                       1,0),
    # PvdA (labor party)
    party_last_prov_elect_pvdA = ifelse((party_last_prov_elect_Mar2007 == 4 & !is.na(party_last_prov_elect_Mar2007)) |
                                          (party_last_prov_elect_Mar2011 == 4 & !is.na(party_last_prov_elect_Mar2011)),
                                        1,0),
    # VVD (liberal party)
    party_last_prov_elect_VVD = ifelse((party_last_prov_elect_Mar2007 == 5 & !is.na(party_last_prov_elect_Mar2007)) |
                                        (party_last_prov_elect_Mar2011 == 3 & !is.na(party_last_prov_elect_Mar2011)),
                                      1,0),
    # Socialistische Partij (SP) (socialist party)
    party_last_prov_elect_SP = ifelse((party_last_prov_elect_Mar2007 == 6 & !is.na(party_last_prov_elect_Mar2007)) |
                                        (party_last_prov_elect_Mar2011 == 7 & !is.na(party_last_prov_elect_Mar2011)),
                                      1,0),
    # GroenLinks (green party)
    party_last_prov_elect_GL = ifelse((party_last_prov_elect_Mar2007 == 7 & !is.na(party_last_prov_elect_Mar2007)) |
                                        (party_last_prov_elect_Mar2011 == 9 & !is.na(party_last_prov_elect_Mar2011)),
                                      1,0),
    # D66 (social-liberal party)
    party_last_prov_elect_D66 = ifelse((party_last_prov_elect_Mar2007 == 8 & !is.na(party_last_prov_elect_Mar2007)) |
                                         (party_last_prov_elect_Mar2011 == 8 & !is.na(party_last_prov_elect_Mar2011)),
                                       1,0),
    # ChristenUnie (Christian union party)
    party_last_prov_elect_CU = ifelse((party_last_prov_elect_Mar2007 == 9 & !is.na(party_last_prov_elect_Mar2007)) |
                                        (party_last_prov_elect_Mar2011 == 10 & !is.na(party_last_prov_elect_Mar2011)),
                                      1,0),
    # SGP (Christian Reformed party)
    party_last_prov_elect_SGP = ifelse((party_last_prov_elect_Mar2007 == 10 & !is.na(party_last_prov_elect_Mar2007)) |
                                         (party_last_prov_elect_Mar2011 == 11 & !is.na(party_last_prov_elect_Mar2011)),
                                       1,0),
    # Partij voor de Dieren (animal welfare party)
    party_last_prov_elect_PvdD = ifelse((party_last_prov_elect_Mar2007 == 11 & !is.na(party_last_prov_elect_Mar2007)) |
                                          (party_last_prov_elect_Mar2011 == 13 & !is.na(party_last_prov_elect_Mar2011)),
                                        1,0),
    # 50Plus (fifty plus party)
    party_last_prov_elect_50PLUS = ifelse(party_last_prov_elect_Mar2011 == 12,1,0),
    # other party
    party_last_prov_elect_other = ifelse((party_last_prov_elect_Mar2007 > 11 & party_last_prov_elect_Mar2007 < 21 & !is.na(party_last_prov_elect_Mar2007)) |
                                           (party_last_prov_elect_Mar2011 > 14 & party_last_prov_elect_Mar2011 < 23 & !is.na(party_last_prov_elect_Mar2011)),
                                         1,0),
## Vote choice today -------------------------------------------------------
    # CDA (Christian democrat party)
    vote_today_CDA = ifelse((elect_today_party_08 == 3 & !is.na(elect_today_party_08))|
                               (elect_today_party_11 == 6 & !is.na(elect_today_party_11))|
                               (elect_today_party_13 == 7 & !is.na(elect_today_party_13))|
                               (elect_today_party_17 == 6 & !is.na(elect_today_party_17))|
                               (elect_today_party_18 == 4 & !is.na(elect_today_party_18)),
                             1,0),
    # PvdA (labor party)
    vote_today_pvdA = ifelse((elect_today_party_08 == 4 & !is.na(elect_today_party_08))|
                               (elect_today_party_11 == 4 & !is.na(elect_today_party_11))|
                               (elect_today_party_13 == 4 & !is.na(elect_today_party_13))|
                               (elect_today_party_17 == 3 & !is.na(elect_today_party_17))|
                               (elect_today_party_18 == 8 & !is.na(elect_today_party_18)),
                             1,0),
    # VVD (liberal party)
    vote_today_VVD = ifelse((elect_today_party_08 == 5 & !is.na(elect_today_party_08))|
                              (elect_today_party_11 == 3 & !is.na(elect_today_party_11))|
                              (elect_today_party_13 == 3 & !is.na(elect_today_party_13))|
                              (elect_today_party_17 == 2 & !is.na(elect_today_party_17))|
                              (elect_today_party_18 == 2 & !is.na(elect_today_party_18)),
                            1,0),
    # Socialistische Partij (SP) (socialist party)
    vote_today_SP = ifelse((elect_today_party_08 == 6 & !is.na(elect_today_party_08))|
                             (elect_today_party_11 == 7 & !is.na(elect_today_party_11))|
                             (elect_today_party_13 == 6 & !is.na(elect_today_party_13))|
                             (elect_today_party_17 == 5 & !is.na(elect_today_party_17))|
                             (elect_today_party_18 == 7 & !is.na(elect_today_party_18)),
                           1,0),
    # GroenLinks (green party)
    vote_today_GL = ifelse((elect_today_party_08 == 7 & !is.na(elect_today_party_08))|
                             (elect_today_party_11 == 9 & !is.na(elect_today_party_11))|
                             (elect_today_party_13 == 10 & !is.na(elect_today_party_13))|
                             (elect_today_party_17 == 9 & !is.na(elect_today_party_17))|
                             (elect_today_party_18 == 6 & !is.na(elect_today_party_18)),
                           1,0),
    # D66 (social-liberal party)
    vote_today_D66 = ifelse((elect_today_party_08 == 8 & !is.na(elect_today_party_08))|
                              (elect_today_party_11 == 8 & !is.na(elect_today_party_11))|
                              (elect_today_party_13 == 8 & !is.na(elect_today_party_13))|
                              (elect_today_party_17 == 7 & !is.na(elect_today_party_17))|
                              (elect_today_party_18 == 5 & !is.na(elect_today_party_18)),
                            1,0),
    # ChristenUnie (Christian union party)
    vote_today_CU = ifelse((elect_today_party_08 == 9 & !is.na(elect_today_party_08))|
                             (elect_today_party_11 == 10 & !is.na(elect_today_party_11))|
                             (elect_today_party_13 == 9 & !is.na(elect_today_party_13))|
                             (elect_today_party_17 == 8 & !is.na(elect_today_party_17))|
                             (elect_today_party_18 == 9 & !is.na(elect_today_party_18)),
                           1,0),
    # SGP (Christian Reformed party)
    vote_today_SGP = ifelse((elect_today_party_08 == 10 & !is.na(elect_today_party_08))|
                              (elect_today_party_11 == 11 & !is.na(elect_today_party_11))|
                              (elect_today_party_13 == 11 & !is.na(elect_today_party_13))|
                              (elect_today_party_17 == 10 & !is.na(elect_today_party_17))|
                              (elect_today_party_18 == 12 & !is.na(elect_today_party_18)),
                            1,0),
    # Groep Wilders \ Partij voor de Vrijheid (Wilders' freedom party)
    vote_today_PVV = ifelse((elect_today_party_08 == 12 & !is.na(elect_today_party_08))|
                              (elect_today_party_11 == 5 & !is.na(elect_today_party_11))|
                              (elect_today_party_13 == 3 & !is.na(elect_today_party_13))|
                              (elect_today_party_17 == 4 & !is.na(elect_today_party_17))|
                              (elect_today_party_18 == 3 & !is.na(elect_today_party_18)),
                            1,0),
    # Partij voor de Dieren (animal welfare party)
    vote_today_PvdD = ifelse((elect_today_party_08 == 13 & !is.na(elect_today_party_08))|
                               (elect_today_party_11 == 12 & !is.na(elect_today_party_11))|
                               (elect_today_party_13 == 12 & !is.na(elect_today_party_13))|
                               (elect_today_party_17 == 11 & !is.na(elect_today_party_17))|
                               (elect_today_party_18 == 10 & !is.na(elect_today_party_18)),
                             1,0),
    # 50PLUS (fifty plus party)
    vote_today_50PLUS = ifelse((elect_today_party_13 == 13 & !is.na(elect_today_party_13))|
                                 (elect_today_party_17 == 12 & !is.na(elect_today_party_17))|
                                 (elect_today_party_18 == 11 & !is.na(elect_today_party_18)),
                               1,0),
    # DENK
    vote_today_DENK =  ifelse((elect_today_party_17 == 14 & !is.na(elect_today_party_17))|
                                (elect_today_party_18 == 13 & !is.na(elect_today_party_18)),
                              1,0),
    # VNL (Pro Netherlands)
    vote_today_VNL = ifelse(elect_today_party_17 == 14,1,0),
    # Forum voor Democratie (Party for Democracy)
    vote_today_FvD = ifelse(elect_today_party_18 == 14,1,0),
    # Volt Nederland
    vote_today_Volt = ifelse(elect_today_party_18 == 17,1,0),
    # JA21 (Right Answer 2021)
    vote_today_JA21 = ifelse(elect_today_party_18 == 18,1,0),
    # BBB (Farmer–Citizen Movement)
    vote_today_BBB = ifelse(elect_today_party_18 == 19,1,0),
    # BIJ1
    vote_today_BIJ1 = ifelse(elect_today_party_18 == 20,1,0),
    # other Party
    vote_today_other =  ifelse((elect_today_party_08 == 14 & !is.na(elect_today_party_08))|
                                 (elect_today_party_11 == 13 & !is.na(elect_today_party_11))|
                                 (elect_today_party_13 == 14 & !is.na(elect_today_party_13))|
                                 (elect_today_party_17 == 15 & !is.na(elect_today_party_17))|
                                 (elect_today_party_18 == 16 & !is.na(elect_today_party_18)),
                               1,0),
## Partisan ----------------------------------------------------------------
    # CDA (Christian democrat party)
    partisan_CDA = ifelse((partisan_party_12 == 4 & !is.na(partisan_party_12))|
                            (partisan_party_13 == 5 & !is.na(partisan_party_13))|
                            (partisan_party_18 == 3 & !is.na(partisan_party_18)),
                          1,0),
    # PvdA (labor party)
    partisan_pvdA =  ifelse((partisan_party_12 == 2 & !is.na(partisan_party_12))|
                              (partisan_party_13 == 2 & !is.na(partisan_party_13))|
                              (partisan_party_18 == 7 & !is.na(partisan_party_18)),
                            1,0),
    # VVD (liberal party)
    partisan_VVD =  ifelse((partisan_party_12 == 1 & !is.na(partisan_party_12))|
                             (partisan_party_13 == 1 & !is.na(partisan_party_13))|
                             (partisan_party_18 == 1 & !is.na(partisan_party_18)),
                           1,0),
    # Socialistische Partij (SP) (socialist party)
    partisan_SP =  ifelse((partisan_party_12 == 5 & !is.na(partisan_party_12))|
                            (partisan_party_13 == 4 & !is.na(partisan_party_13))|
                            (partisan_party_18 == 6 & !is.na(partisan_party_18)),
                          1,0),
    # GroenLinks (green party)
    partisan_GL =  ifelse((partisan_party_12 == 7 & !is.na(partisan_party_12))|
                            (partisan_party_13 == 8 & !is.na(partisan_party_13))|
                            (partisan_party_18 == 5 & !is.na(partisan_party_18)),
                          1,0),
    # D66 (social-liberal party)
    partisan_D66 =  ifelse((partisan_party_12 == 6 & !is.na(partisan_party_12))|
                             (partisan_party_13 == 6 & !is.na(partisan_party_13))|
                             (partisan_party_18 == 4 & !is.na(partisan_party_18)),
                           1,0),
    # SGP (Christian Reformed party)
    partisan_SGP =  ifelse((partisan_party_12 == 9 & !is.na(partisan_party_12))|
                             (partisan_party_13 == 9 & !is.na(partisan_party_13))|
                             (partisan_party_18 == 11 & !is.na(partisan_party_18)),
                           1,0),
    # ChristenUnie (Christian union party)
    partisan_CU =  ifelse((partisan_party_12 == 8 & !is.na(partisan_party_12))|
                            (partisan_party_13 == 7 & !is.na(partisan_party_13))|
                            (partisan_party_18 == 8 & !is.na(partisan_party_18)),
                          1,0),
    # Groep Wilders \ Partij voor de Vrijheid (Wilders' freedom party)
    partisan_PVV =  ifelse((partisan_party_12 == 3 & !is.na(partisan_party_12))|
                             (partisan_party_13 == 3 & !is.na(partisan_party_13))|
                             (partisan_party_18 == 2 & !is.na(partisan_party_18)),
                           1,0),
    # Partij voor de Dieren (animal welfare party)
    partisan_PvdD =  ifelse((partisan_party_12 == 10 & !is.na(partisan_party_12))|
                              (partisan_party_13 == 10 & !is.na(partisan_party_13))|
                              (partisan_party_18 == 9 & !is.na(partisan_party_18)),
                            1,0),
    # 50Plus (fifty plus party)
    partisan_50PLUS =  ifelse((partisan_party_13 == 11 & !is.na(partisan_party_13))|
                                (partisan_party_18 == 10 & !is.na(partisan_party_18)),
                              1,0),
    # DENK
    partisan_DENK = ifelse(partisan_party_18 == 12,1,0),
    # Forum voor Democratie
    partisan_FvD = ifelse(partisan_party_18 == 13,1,0),
    # Volt Nederland
    partisan_Volt = ifelse(partisan_party_18 == 15,1,0),
    # JA21 (Right Answer 2021)
    partisan_JA21 = ifelse(partisan_party_18 == 16,1,0),
    # BBB (Farmer–Citizen Movement)
    partisan_BBB = ifelse(partisan_party_18 == 17,1,0),
    # BIJ1
    partisan_BIJ1 = ifelse(partisan_party_18 == 18,1,0),
    # other party
    partisan_other = ifelse((partisan_party_12 == 12 & !is.na(partisan_party_12))|
                              (partisan_party_13 == 12 & !is.na(partisan_party_13))|
                              (partisan_party_18 == 14 & !is.na(partisan_party_18)),
                            1,0),
## Party attraction --------------------------------------------------------
    # CDA (Christian democrat party)
    party_attraction_CDA = ifelse((party_attraction_party_12 == 4 & !is.na(party_attraction_party_12))|
                                    (party_attraction_party_13 == 5 & !is.na(party_attraction_party_13))|
                                    (party_attraction_party_18 == 3 & !is.na(party_attraction_party_18)),
                                  1,0),
    # PvdA (labor party)
    party_attraction_pvdA = ifelse((party_attraction_party_12 == 2 & !is.na(party_attraction_party_12))|
                                     (party_attraction_party_13 == 2 & !is.na(party_attraction_party_13))|
                                     (party_attraction_party_18 == 7 & !is.na(party_attraction_party_18)),
                                   1,0),
    # VVD (liberal party)
    party_attraction_VVD = ifelse((party_attraction_party_12 == 1 & !is.na(party_attraction_party_12))|
                                    (party_attraction_party_13 == 1 & !is.na(party_attraction_party_13))|
                                    (party_attraction_party_18 == 1 & !is.na(party_attraction_party_18)),
                                  1,0),
    # Socialistische Partij (SP) (socialist party)
    party_attraction_SP = ifelse((party_attraction_party_12 == 5 & !is.na(party_attraction_party_12))|
                                   (party_attraction_party_13 == 4 & !is.na(party_attraction_party_13))|
                                   (party_attraction_party_18 == 6 & !is.na(party_attraction_party_18)),
                                 1,0),
    # GroenLinks (green party)
    party_attraction_GL = ifelse((party_attraction_party_12 == 7 & !is.na(party_attraction_party_12))|
                                   (party_attraction_party_13 == 8 & !is.na(party_attraction_party_13))|
                                   (party_attraction_party_18 == 5 & !is.na(party_attraction_party_18)),
                                 1,0),
    # D66 (social-liberal party)
    party_attraction_D66 = ifelse((party_attraction_party_12 == 6 & !is.na(party_attraction_party_12))|
                                    (party_attraction_party_13 == 6 & !is.na(party_attraction_party_13))|
                                    (party_attraction_party_18 == 4 & !is.na(party_attraction_party_18)),
                                  1,0),
    # SGP (Christian Reformed party)
    party_attraction_SGP = ifelse((party_attraction_party_12 == 9 & !is.na(party_attraction_party_12))|
                                    (party_attraction_party_13 == 9 & !is.na(party_attraction_party_13))|
                                    (party_attraction_party_18 == 11 & !is.na(party_attraction_party_18)),
                                  1,0),
    # ChristenUnie (Christian union party)
    party_attraction_CU = ifelse((party_attraction_party_12 == 8 & !is.na(party_attraction_party_12))|
                                   (party_attraction_party_13 == 7 & !is.na(party_attraction_party_13))|
                                   (party_attraction_party_18 == 8 & !is.na(party_attraction_party_18)),
                                 1,0),
    # Groep Wilders \ Partij voor de Vrijheid (Wilders' freedom party)
    party_attraction_PVV = ifelse((party_attraction_party_12 == 3 & !is.na(party_attraction_party_12))|
                                    (party_attraction_party_13 == 3 & !is.na(party_attraction_party_13))|
                                    (party_attraction_party_18 == 2 & !is.na(party_attraction_party_18)),
                                  1,0),
    # Partij voor de Dieren (animal welfare party)
    party_attraction_PvdD = ifelse((party_attraction_party_12 == 10 & !is.na(party_attraction_party_12))|
                                     (party_attraction_party_13 == 10 & !is.na(party_attraction_party_13))|
                                     (party_attraction_party_18 == 9 & !is.na(party_attraction_party_18)),
                                   1,0),
    # 50Plus (fifty plus party)
    party_attraction_50PLUS = ifelse((party_attraction_party_13 == 10 & !is.na(party_attraction_party_13))|
                                       (party_attraction_party_18 == 10 & !is.na(party_attraction_party_18)),
                                     1,0),
    # DENK
    party_attraction_DENK = ifelse(party_attraction_party_18 == 12,1,0),
    # Forum voor Democratie
    party_attraction_FvD = ifelse(party_attraction_party_18 == 13,1,0),
    # Volt Nederland
    party_attraction_Volt = ifelse(party_attraction_party_18 == 15,1,0),
    # JA21 (Right Answer 2021)
    party_attraction_JA21 = ifelse(party_attraction_party_18 == 16,1,0),
    # BBB (Farmer–Citizen Movement)
    party_attraction_BBB = ifelse(party_attraction_party_18 == 17,1,0),
    # BIJ1
    party_attraction_BIJ1 = ifelse(party_attraction_party_18 == 18,1,0),
    # other party
    party_attraction_other = ifelse((party_attraction_party_12 == 11 & !is.na(party_attraction_party_12))|
                                      (party_attraction_party_13 == 12 & !is.na(party_attraction_party_13))|
                                      (party_attraction_party_18 == 14 & !is.na(party_attraction_party_18)),
                                    1,0),
## Party member ------------------------------------------------------------
    # CDA (Christian democrat party)
    party_member_CDA = ifelse((party_member_party_08 == 1 & !is.na(party_member_party_08))|
                                (party_member_party_11 == 4 & !is.na(party_member_party_11))|
                                (party_member_party_13 == 5 & !is.na(party_member_party_13))|
                                (party_member_party_18 == 3 & !is.na(party_member_party_18)),
                              1,0),
    # PvdA (labor party)
    party_member_pvdA = ifelse((party_member_party_08 == 2 & !is.na(party_member_party_08))|
                                 (party_member_party_11 == 2 & !is.na(party_member_party_11))|
                                 (party_member_party_13 == 2 & !is.na(party_member_party_13))|
                                 (party_member_party_18 == 7 & !is.na(party_member_party_18)),
                               1,0),
    # VVD (liberal party)
    party_member_VVD = ifelse((party_member_party_08 == 3 & !is.na(party_member_party_08))|
                                (party_member_party_11 == 1 & !is.na(party_member_party_11))|
                                (party_member_party_13 == 1 & !is.na(party_member_party_13))|
                                (party_member_party_18 == 1 & !is.na(party_member_party_18)),
                              1,0),
    # Socialistische Partij (SP) (socialist party)
    party_member_SP = ifelse((party_member_party_08 == 4 & !is.na(party_member_party_08))|
                               (party_member_party_11 == 5 & !is.na(party_member_party_11))|
                               (party_member_party_13 == 4 & !is.na(party_member_party_13))|
                               (party_member_party_18 == 6 & !is.na(party_member_party_18)),
                             1,0),
    # GroenLinks (green party)
    party_member_GL = ifelse((party_member_party_08 == 5 & !is.na(party_member_party_08))|
                               (party_member_party_11 == 7 & !is.na(party_member_party_11))|
                               (party_member_party_13 == 8 & !is.na(party_member_party_13))|
                               (party_member_party_18 == 5 & !is.na(party_member_party_18)),
                             1,0),
    # Lijst vijf Fortuyn \ LPF (Fortuyn party)
    party_member_LPF = ifelse(party_member_party_08 == 6,1,0),
    # D66 (social-liberal party)
    party_member_D66 = ifelse((party_member_party_08 == 7 & !is.na(party_member_party_08))|
                                (party_member_party_11 == 6 & !is.na(party_member_party_11))|
                                (party_member_party_13 == 6 & !is.na(party_member_party_13))|
                                (party_member_party_18 == 4 & !is.na(party_member_party_18)),
                              1,0),
    # ChristenUnie (Christian union party)
    party_member_CU = ifelse((party_member_party_08 == 8 & !is.na(party_member_party_08))|
                               (party_member_party_11 == 8 & !is.na(party_member_party_11))|
                               (party_member_party_13 == 7 & !is.na(party_member_party_13))|
                               (party_member_party_18 == 8 & !is.na(party_member_party_18)),
                             1,0),
    # SGP (Christian Reformed party)
    party_member_SGP = ifelse((party_member_party_08 == 9 & !is.na(party_member_party_08))|
                                (party_member_party_11 == 9 & !is.na(party_member_party_11))|
                                (party_member_party_13 == 9 & !is.na(party_member_party_13))|
                                (party_member_party_18 == 11 & !is.na(party_member_party_18)),
                              1,0),
    # Groep Wilders \ Partij voor de Vrijheid (Wilders' freedom party)
    party_member_PVV = ifelse((party_member_party_08 == 11 & !is.na(party_member_party_08))|
                                (party_member_party_11 == 3 & !is.na(party_member_party_11))|
                                (party_member_party_13 == 3 & !is.na(party_member_party_13))|
                                (party_member_party_18 == 2 & !is.na(party_member_party_18)),
                              1,0),
    # Partij voor de Dieren (animal welfare party)
    party_member_PvdD = ifelse((party_member_party_08 == 12 & !is.na(party_member_party_08))|
                                 (party_member_party_11 == 10 & !is.na(party_member_party_11))|
                                 (party_member_party_13 == 10 & !is.na(party_member_party_13))|
                                 (party_member_party_18 == 9 & !is.na(party_member_party_18)),
                               1,0),
    # 50Plus
    party_member_50PLUS = ifelse((party_member_party_13 == 11 & !is.na(party_member_party_13))|
                                   (party_member_party_18 == 10 & !is.na(party_member_party_18)),
                                 1,0),
    # DENK
    party_member_DENK = ifelse(party_member_party_18 == 12,1,0),
    # Forum voor Democratie
    party_member_FvD = ifelse(party_member_party_18 == 13,1,0),
    # Volt Nederland
    party_member_Volt = ifelse(party_member_party_18 == 15,1,0),
    # JA21 (Right Answer 2021)
    party_member_JA21 = ifelse(party_member_party_18 == 16,1,0),
    # BBB (Farmer–Citizen Movement)
    party_member_BBB = ifelse(party_member_party_18 == 17,1,0),
    # BIJ1
    party_member_BIJ1 = ifelse(party_member_party_18 == 18,1,0),
    # other party
    party_member_other = ifelse((party_member_party_08 == 13 & !is.na(party_member_party_08))|
                                  (party_member_party_11 == 11 & !is.na(party_member_party_11))|
                                  (party_member_party_13 == 12 & !is.na(party_member_party_13))|
                                  (party_member_party_18 == 14 & !is.na(party_member_party_18)),
                                1,0),

# Party family vote & partisanship ----------------------------------------
## Vote choice Parliament -------------------------------------------------
# Agrarian
vote_parl_agrarian = ifelse(party_last_parl_elect_SGP == 1,1,0),
# Christian Democratic
vote_parl_christdemoc = ifelse((party_last_parl_elect_CDA == 1 & !is.na(party_last_parl_elect_CDA)) |
                                     (party_last_parl_elect_CU == 1 & !is.na(party_last_parl_elect_CU)),
                                   1,0),
# Communist/Socialist
vote_parl_socialist = ifelse((party_last_parl_elect_SP == 1 & !is.na(party_last_parl_elect_SP)) |
                               (party_last_parl_elect_BIJ1 == 1 & !is.na(party_last_parl_elect_BIJ1)),
                             1,0),
# Conservative
vote_parl_conservative = ifelse(party_last_parl_elect_SGP == 1,1,0),
# Green-Ecologist
vote_parl_green = ifelse(party_last_parl_elect_GL == 1,1,0),
# Liberal
vote_parl_liberal = ifelse((party_last_parl_elect_VVD == 1 & !is.na(party_last_parl_elect_VVD)) |
                             (party_last_parl_elect_D66 == 1 & !is.na(party_last_parl_elect_D66)),
                           1,0),
# Right-wing
vote_parl_rightwing = ifelse((party_last_parl_elect_PVV == 1 & !is.na(party_last_parl_elect_PVV)) |
                               (party_last_parl_elect_LPF == 1 & !is.na(party_last_parl_elect_LPF)) |
                               (party_last_parl_elect_FvD == 1 & !is.na(party_last_parl_elect_FvD)) |
                               (party_last_parl_elect_JA21 == 1 & !is.na(party_last_parl_elect_JA21)),
                             1,0),
# Social democratic
vote_parl_socdemoc = ifelse((party_last_parl_elect_pvdA == 1 & !is.na(party_last_parl_elect_pvdA)) |
                              (party_last_parl_elect_DENK == 1 & !is.na(party_last_parl_elect_DENK)),
                            1,0),
# Special issue
vote_parl_specissue = ifelse((party_last_parl_elect_PvdD == 1 & !is.na(party_last_parl_elect_PvdD)) |
                               (party_last_parl_elect_50PLUS == 1 & !is.na(party_last_parl_elect_50PLUS)) |
                               (party_last_parl_elect_Volt == 1 & !is.na(party_last_parl_elect_Volt)),
                             1,0),
## Vote choice Province -------------------------------------------------
# Christian Democratic
vote_prov_christdemoc = ifelse((party_last_prov_elect_CDA == 1 & !is.na(party_last_prov_elect_CDA)) |
                                 (party_last_prov_elect_CU == 1 & !is.na(party_last_prov_elect_CU)),
                               1,0),
# Communist/Socialist
vote_prov_socialist = ifelse(party_last_prov_elect_SP == 1,1,0),
# Conservative
vote_prov_conservative = ifelse(party_last_prov_elect_SGP == 1,1,0),
# Green-Ecologist
vote_prov_green = ifelse(party_last_prov_elect_GL == 1,1,0),
# Liberal
vote_prov_liberal = ifelse((party_last_prov_elect_VVD == 1 & !is.na(party_last_prov_elect_VVD)) |
                             (party_last_prov_elect_D66 == 1 & !is.na(party_last_prov_elect_D66)),
                           1,0),
# Social democratic
vote_prov_socdemoc = ifelse(party_last_prov_elect_pvdA == 1,1,0),
# Special issue
vote_prov_specissue = ifelse((party_last_prov_elect_PvdD == 1 & !is.na(party_last_prov_elect_PvdD)) |
                               (party_last_prov_elect_50PLUS == 1 & !is.na(party_last_prov_elect_50PLUS)),
                             1,0),
## Vote choice Today -------------------------------------------------
# Agrarian
vote_today_agrarian = ifelse(vote_today_BBB == 1,1,0),
# Christian Democratic
vote_today_christdemoc = ifelse((vote_today_CDA == 1 & !is.na(vote_today_CDA)) |
                                  (vote_today_CU == 1 & !is.na(vote_today_CU)),
                                1,0),
# Communist/Socialist
vote_today_socialist = ifelse((vote_today_SP == 1 & !is.na(vote_today_SP)) |
                               (vote_today_BIJ1 == 1 & !is.na(vote_today_BIJ1)),
                             1,0),
# Conservative
vote_today_conservative = ifelse(vote_today_SGP == 1,1,0),
# Green-Ecologist
vote_today_green = ifelse(vote_today_GL == 1,1,0),
# Liberal
vote_today_liberal = ifelse((vote_today_VVD == 1 & !is.na(vote_today_VVD)) |
                             (vote_today_D66 == 1 & !is.na(vote_today_D66)),
                           1,0),
# Right-wing
vote_today_rightwing = ifelse((vote_today_PVV == 1 & !is.na(vote_today_PVV)) |
                               (vote_today_FvD == 1 & !is.na(vote_today_FvD)) |
                               (vote_today_JA21 == 1 & !is.na(vote_today_JA21)),
                             1,0),
# Social democratic
vote_today_socdemoc = ifelse((vote_today_pvdA == 1 & !is.na(vote_today_pvdA)) |
                              (vote_today_DENK == 1 & !is.na(vote_today_DENK)),
                            1,0),
# Special issue
vote_today_specissue = ifelse((vote_today_PvdD == 1 & !is.na(vote_today_PvdD)) |
                               (vote_today_50PLUS == 1 & !is.na(vote_today_50PLUS)) |
                               (vote_today_Volt == 1 & !is.na(vote_today_Volt)),
                             1,0),

## Partisan -------------------------------------------------
# Agrarian
partisan_agrarian = ifelse(partisan_BBB == 1,1,0),
# Christian Democratic
partisan_christdemoc = ifelse((partisan_CDA == 1 & !is.na(partisan_CDA)) |
                                  (partisan_CU == 1 & !is.na(partisan_CU)),
                                1,0),
# Communist/Socialist
partisan_socialist = ifelse((partisan_SP == 1 & !is.na(partisan_SP)) |
                                (partisan_BIJ1 == 1 & !is.na(partisan_BIJ1)),
                              1,0),
# Conservative
partisan_conservative = ifelse(partisan_SGP == 1,1,0),
# Green-Ecologist
partisan_green = ifelse(partisan_GL == 1,1,0),
# Liberal
partisan_liberal = ifelse((partisan_VVD == 1 & !is.na(partisan_VVD)) |
                              (partisan_D66 == 1 & !is.na(partisan_D66)),
                            1,0),
# Right-wing
partisan_rightwing = ifelse((partisan_PVV == 1 & !is.na(partisan_PVV)) |
                              (partisan_FvD == 1 & !is.na(partisan_FvD)) |
                                (partisan_JA21 == 1 & !is.na(partisan_JA21)),
                              1,0),
# Social democratic
partisan_socdemoc = ifelse((partisan_pvdA == 1 & !is.na(partisan_pvdA)) |
                               (partisan_DENK == 1 & !is.na(partisan_DENK)),
                             1,0),
# Special issue
partisan_specissue = ifelse((partisan_PvdD == 1 & !is.na(partisan_PvdD)) |
                                (partisan_50PLUS == 1 & !is.na(partisan_50PLUS)) |
                                (partisan_Volt == 1 & !is.na(partisan_Volt)),
                              1,0),
## Party attraction -------------------------------------------------
# Agrarian
party_attraction_agrarian = ifelse(party_attraction_BBB == 1,1,0),
# Christian Democratic
party_attraction_christdemoc = ifelse((party_attraction_CDA == 1 & !is.na(party_attraction_CDA)) |
                                  (party_attraction_CU == 1 & !is.na(party_attraction_CU)),
                                1,0),
# Communist/Socialist
party_attraction_socialist = ifelse((party_attraction_SP == 1 & !is.na(party_attraction_SP)) |
                                (party_attraction_BIJ1 == 1 & !is.na(party_attraction_BIJ1)),
                              1,0),
# Conservative
party_attraction_conservative = ifelse(party_attraction_SGP == 1,1,0),
# Green-Ecologist
party_attraction_green = ifelse(party_attraction_GL == 1,1,0),
# Liberal
party_attraction_liberal = ifelse((party_attraction_VVD == 1 & !is.na(party_attraction_VVD)) |
                              (party_attraction_D66 == 1 & !is.na(party_attraction_D66)),
                            1,0),
# Right-wing
party_attraction_rightwing = ifelse((party_attraction_PVV == 1 & !is.na(party_attraction_PVV)) |
                                (party_attraction_FvD == 1 & !is.na(party_attraction_FvD)) |
                                (party_attraction_JA21 == 1 & !is.na(party_attraction_JA21)),
                              1,0),
# Social democratic
party_attraction_socdemoc = ifelse((party_attraction_pvdA == 1 & !is.na(party_attraction_pvdA)) |
                               (party_attraction_DENK == 1 & !is.na(party_attraction_DENK)),
                             1,0),
# Special issue
party_attraction_specissue = ifelse((party_attraction_PvdD == 1 & !is.na(party_attraction_PvdD)) |
                                (party_attraction_50PLUS == 1 & !is.na(party_attraction_50PLUS)) |
                                (party_attraction_Volt == 1 & !is.na(party_attraction_Volt)),
                              1,0),
## Party member -------------------------------------------------
# Agrarian
party_member_agrarian = ifelse(party_member_BBB == 1,1,0),
# Christian Democratic
party_member_christdemoc = ifelse((party_member_CDA == 1 & !is.na(party_member_CDA)) |
                                  (party_member_CU == 1 & !is.na(party_member_CU)),
                                1,0),
# Communist/Socialist
party_member_socialist = ifelse((party_member_SP == 1 & !is.na(party_member_SP)) |
                                (party_member_BIJ1 == 1 & !is.na(party_member_BIJ1)),
                              1,0),
# Conservative
party_member_conservative = ifelse(party_member_SGP == 1,1,0),
# Green-Ecologist
party_member_green = ifelse(party_member_GL == 1,1,0),
# Liberal
party_member_liberal = ifelse((party_member_VVD == 1 & !is.na(party_member_VVD)) |
                              (party_member_D66 == 1 & !is.na(party_member_D66)),
                            1,0),
# Right-wing
party_member_rightwing = ifelse((party_member_PVV == 1 & !is.na(party_member_PVV)) |
                                (party_member_FvD == 1 & !is.na(party_member_FvD)) |
                                (party_member_JA21 == 1 & !is.na(party_member_JA21)),
                              1,0),
# Social democratic
party_member_socdemoc = ifelse((party_member_pvdA == 1 & !is.na(party_member_pvdA)) |
                               (party_member_DENK == 1 & !is.na(party_member_DENK)),
                             1,0),
# Special issue
party_member_specissue = ifelse((party_member_PvdD == 1 & !is.na(party_member_PvdD)) |
                                (party_member_50PLUS == 1 & !is.na(party_member_50PLUS)) |
                                (party_member_Volt == 1 & !is.na(party_member_Volt)),
                              1,0)
)

# # write .csv
# setwd("~/Dropbox (Princeton)/Data/Panel_Surveys/LISS/")
# fwrite(pol, file = "liss_politics.csv")



# Affective polarization --------------------------------------------------

# # set wd
# setwd("~/Dropbox (Princeton)/Data/Panel_Surveys/LISS/")
# pol <- fread("liss_politics.csv")


# get thermo score for each individual - wave - party observation
thermo <- pol |>
  pivot_longer(
    cols = thermo_VVD:thermo_VNL,
    names_to = "party",
    names_prefix = "thermo_",
    values_to = "thermo_score"
  ) |>
  select(nomem_encr,wave,party,thermo_score) |>
  unique() |>
  mutate(
    # create id
    id = paste0(nomem_encr,"_",wave),
    # values to NA
    thermo_score = as.numeric(thermo_score),
    thermo_score = na_if(thermo_score, -9),
    thermo_score = na_if(thermo_score, 999),
    thermo_score = ifelse(thermo_score == 11, 10, thermo_score)
    )

# get duplicates
dupes <- thermo |> 
  janitor::get_dupes(id,party) |>
  distinct(id) |>
  pull(id)
# 422 duplicates

# remove duplicates from thermo dataframe
thermo <- thermo |>
  filter(!(id %in% dupes)) |>
  select(-id)

# get partisanship for each individual - wave - party observation
partisan <- pol |>
  select(-partisan) |>
  pivot_longer(
    cols = partisan_CDA:partisan_BIJ1,
    names_to = "party",
    names_prefix = "partisan_",
    values_to = "partisan"
  ) |>
  select(nomem_encr,wave,party,partisan) |>
  unique() |>
  mutate(id = paste0(nomem_encr,"_",wave))

# get duplicates
dupes <- partisan |> 
  janitor::get_dupes(id,party) |>
  distinct(id) |>
  pull(id)

# remove from partisan dataframe
partisan <- partisan |>
  filter(!(id %in% dupes)) |>
  select(-id)

# get partisanship for each individual - wave - party observation
party_attraction <- pol |>
  select(-party_attraction) |>
  pivot_longer(
    cols = party_attraction_CDA:party_attraction_BIJ1,
    names_to = "party",
    names_prefix = "party_attraction_",
    values_to = "party_attraction"
  ) |>
  select(nomem_encr,wave,party,party_attraction) |>
  unique() |>
  mutate(id = paste0(nomem_encr,"_",wave))

# get duplicates
dupes <- party_attraction |> 
  janitor::get_dupes(id,party) |>
  distinct(id) |>
  pull(id)

# remove from party_attraction dataframe
party_attraction <- party_attraction |>
  filter(!(id %in% dupes)) |>
  select(-id)

# combine
df <- thermo |>
  left_join(partisan, by = c("nomem_encr", "wave", "party")) |>
  left_join(party_attraction, by = c("nomem_encr", "wave", "party"))

# create partisan combined variable
df <- df |>
  mutate(partisan_comb = ifelse(partisan == 1, 1,
                                ifelse(party_attraction == 1, 1, 
                                       0)))


## Vote share data ---------------------------------------------------------
vs2006 <- readxl::read_excel("~/Dropbox (Princeton)/Data/Panel_Surveys/LISS/election_results/2006_11.xlsx") |>
  mutate(election_year = 2006)
vs2010 <- readxl::read_excel("~/Dropbox (Princeton)/Data/Panel_Surveys/LISS/election_results/2010_06.xlsx") |>
  mutate(election_year = 2010)
vs2012 <- readxl::read_excel("~/Dropbox (Princeton)/Data/Panel_Surveys/LISS/election_results/2012_09.xlsx") |>
  mutate(election_year = 2012)
vs2017 <- readxl::read_excel("~/Dropbox (Princeton)/Data/Panel_Surveys/LISS/election_results/2017_03.xlsx") |>
  mutate(election_year = 2017)

# combine
vs <- bind_rows(vs2006, vs2010, vs2012, vs2017) |>
  mutate(
    # mutate party abbreviations for merging
    party = ifelse(party == "GrLinks", "GL", party),
    party = ifelse(party == "DBB", "BBB", party),
    party = ifelse(party == "DPK", "TON", party)
  ) |>
  select(party, election_year, share)

# add vote share to df
df <- df |>
  mutate(election_year = ifelse(wave < 10, 
                                2006,
                                ifelse(wave >= 10 & wave < 12,
                                       2010,
                                       ifelse(wave >= 12 & wave < 17,
                                              2012,
                                              2017)))) |>
  left_join(vs, by = c("party", "election_year"))

# Calculate share of voters for Volt and JA21 (since 2018)
volt <- pol |>
  select(nomem_encr,wave,vote_today_Volt) |>
  filter(!is.na(vote_today_Volt)) |>
  group_by(wave) |>
  summarize(share = mean(vote_today_Volt, na.rm = TRUE)) |>
  filter(!is.na(wave)) |>
  mutate(party = "Volt")

ja21 <- pol |>
  select(nomem_encr,wave,vote_today_JA21) |>
  filter(!is.na(vote_today_JA21)) |>
  group_by(wave) |>
  summarize(share = mean(vote_today_JA21, na.rm = TRUE)) |>
  filter(!is.na(wave)) |>
  mutate(party = "JA21")

volt_ja21 <- volt |> bind_rows(ja21)

# merge volt & ja21 with df
df <- df |>
  left_join(volt_ja21, by = c("party", "wave")) |>
  mutate(share = ifelse(!is.na(share.y), share.y, share.x)) |>
  select(-c(share.y,share.x))


# ingroup scores
ingroup <- df |>
  filter(partisan_comb == 1) |>
  # sometimes people are partisans of more than one party: select highest score
  group_by(nomem_encr, wave) |>
  filter(thermo_score == max(thermo_score, na.rm = TRUE)) |>
  mutate(ingroup_share = sum(share, na.rm = TRUE)) |>
  ungroup() |>
  select(
    nomem_encr, wave,
    ingroup_score = thermo_score,
    ingroup_share) |>
  distinct()
# ingroup |> janitor::get_dupes(nomem_encr, wave)

# merge to df
df <- df |>
  left_join(ingroup, by = c("nomem_encr", "wave")) |>
  mutate(
    # denominator for party weights (1-ingroup-share)
    total_outgroup_share = 1 - ingroup_share,
    # distance ingroup - outgroup
    distance_to_ingroup_score = ingroup_score - thermo_score,
    weighted_distance_to_ingroup_score = distance_to_ingroup_score * share / (1-ingroup_share)
    )



## Partisan affect score ---------------------------------------------------

# calculate partisan affect score for each individual 
# (see Boxell Gentzkow Shapiro (2023) for how to calculate this score)
aff_pol <- df |>
  filter(partisan_comb != 1 & !is.na(ingroup_score)) |>
  group_by(nomem_encr, wave) |>
  summarise(partisan_affect = sum(weighted_distance_to_ingroup_score, na.rm = TRUE)) |>
  ungroup()

# merge
pol <- pol |>
  left_join(aff_pol, by = c("nomem_encr", "wave"))



## Spread ------------------------------------------------------------------

# calculate spread score for each individual
# (see Wagner 2021)
spread <- df |>
  group_by(nomem_encr, wave) |>
  # weighted mean of the thermo scores
  mutate(mean_like = sum(share * thermo_score, na.rm = TRUE)) |>
  ungroup() |>
  # weighted spread
  mutate(weighted_spread = (thermo_score - mean_like)^2 * share) |>
  # sum up weighted spread scores for each individual in each wave
  group_by(nomem_encr, wave) |>
  summarize(spread = sqrt(sum(weighted_spread, na.rm = TRUE)))

# merge
pol <- pol |>
  left_join(spread, by = c("nomem_encr", "wave"))


## Distance ----------------------------------------------------------------

# calculate distance score for each individual
# (see Wagner 2021)
distance <- df |>
  group_by(nomem_encr, wave) |>
  filter(thermo_score == max(thermo_score, na.rm = TRUE)) |>
  mutate(like_max = thermo_score) |>
  ungroup() |>
  select(nomem_encr, wave, like_max) |>
  distinct() |>
  # merge with other variables
  right_join(df, by = c("nomem_encr", "wave")) |>
  # calculate weighted distance to max score
  mutate(weighted_distance_to_max_score = (thermo_score - like_max)^2 * share) |> 
  # sum up weighted distance scores for each individual in each wave
  group_by(nomem_encr, wave) |>
  summarize(distance = sqrt(sum(weighted_distance_to_max_score, na.rm = TRUE)))


# merge
pol <- pol |>
  left_join(distance, by = c("nomem_encr", "wave"))



## Most liked and most disliked scores -------------------------------------

# calculate max and min score for each individual (see Chagai Weiss (2023))
max <- df |>
  filter(!is.na(thermo_score)) %>%
  group_by(nomem_encr, wave) %>%
  filter(thermo_score == max(thermo_score, na.rm = TRUE)) |>
  mutate(like_max = thermo_score) |>
  ungroup() |>
  select(nomem_encr, wave, like_max) |>
  distinct()

min <- df |>
  filter(!is.na(thermo_score)) %>%
  group_by(nomem_encr, wave) %>%
  filter(thermo_score == min(thermo_score, na.rm = TRUE)) |>
  mutate(like_min = thermo_score) |>
  ungroup() |>
  select(nomem_encr, wave, like_min) |>
  distinct()

# merge
pol <- pol |>
  left_join(max, by = c("nomem_encr", "wave")) |>
  left_join(min, by = c("nomem_encr", "wave"))

# write .csv
setwd("~/Dropbox (Princeton)/Data/Panel_Surveys/LISS/")
fwrite(pol, file = "liss_politics.csv")

### END