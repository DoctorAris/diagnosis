
/*Knowledge base*/
/*Cough*/
/*kb(r05_cough,
  [icd_10],
  [condition],
  [nature_of_patient],
  [nature_of_symptoms],
  [associated_symptoms],
  [precipitating_and_aggravating_factors],
  [amelio__rating_factors],
  [physical_findings],
  [diagnostic_studies],
  [location_of_pain]).*/
kb(acute_cough,
  [j00_j06],
  ['Viral upper respiratory tract infections'],
  [most_common_cause_of_acute_cough_in_all_ages],
  [acute_onset_of_noisy_cough_over_hours_or_days,cough_worse_at_night_and_may_persist_for_7_10_days,sputum_thick_and_yellowish_but_minimal_amount_produced],
  [fever,runny_nose,sore_throat,general_aches_and_pains],
  [],
  [],
  [pharynx_injected_or_pale_boggy_and_swollen,coarse_rhonchi],
  [],
  []).
kb(acute_cough,
  [b960_mycoplasma_pneumoniae_as_the_cause_of_diseases_classified_to_other_chapters],
  ['Mycoplasmal bronchitis or pneumonia'],
  [common_cause_of_acute_cough_in_school_age_children,frequent_cause_of_persistent_cough_in_adults],
  [long_incubation_period_about_21_days,severe_cough_may_persist_for_1_4_months,other_symptoms_e_g_fever_abate_within_10_days],
  [same_as_for_bacterial_pneumonia_but_not_usually_as_severe],
  [],
  [],
  [scattered_rales,signs_of_pneumonia],
  [chest_radiograph,cold_agglutinins,complement_fixation],
  []).
kb(acute_cough,
  [j20_acute_bronchitis],
  ['Viral bronchitis'],
  [recurrent_infections_are_most_common_cause_of_persistent_cough_in_children_who_are_often_asthmatic],
  [cough_may_persist_for_7_10_days],
  [fever],
  [],
  [],
  [],
  [],
  []).
kb(acute_cough,
  [t784_allergy,unspecified],
  ['Allergies'],
  [history_of_allergy_in_family],
  [minimally_productive,may_be_nocturnal,recurrent_cough_without_dyspnea,may_have_seasonal_incidence],
  [sneezing,conjunctivitis,tearing,itching_of_eyes_and_roof_of_mouth,postnasal_drip],
  [],
  [antihistamines],
  [boggy_edematous_nasal_mucosa],
  [stained_sputum_smear_for_eosinophils],
  []).
kb(acute_cough,
  [j09_j18_10_influenza_and_pneumonia],
  ['Bacterial pneumonia'],
  [],
  [noisy_cough,incidence_highest_in_winter,acute_onset,cough_worse_at_night],
  [fever,chills,signs_of_acute_infection,pharyngitis,conjunctivitis,otitis,abdominal_pain,headache,pleuritic_chest_pain],
  [chronic_debilitating_conditions],
  [],
  [signs_of_pneumonia],
  [chest_radiograph,sputum_and_blood_cultures],
  []).
kb(chronic_or_recurrent_cough,
  [icd10codemissing],
  ['Postnasal drip'],
  [may_not_be_aware_of_condition],
  [frequent_throat_clearing_and_hawking,cough_worse_in_morning],
  [],
  [recumbency,chronic_sinusitis,vasomotor_rhinitis,allergic_rhinitis,nonallergic_rhinitis_with_eosinophilia],
  [],
  [mucoid_secretions_in_posterior_pharynx,palpation_percussion_and_transillumination_of_sinuses_reveal_sinusitis,mucosa_of_nose_oropharynx_cobblestone_appearance],
  [],
  []).
kb(chronic_or_recurrent_cough,
  [j45_asthma],
  ['Asthma'],
  [may_have_family_history_of_allergies_atopy_or_asthma],
  [recurrent_cough,minimally_or_not_productive_if_productive_secretions_clear_and_mucoid],
  [shortness_of_breath],
  [exercise,may_be_worse_during_seasonal_allergies],
  [],
  [bilateral_wheezing],
  [pulmonary_function_tests,response_to_isoproterenol_and_methacholine],
  []).
kb(chronic_or_recurrent_cough,
  [j44_other_chronic_obstructive_pulmonary_disease],
  ['Chronic obstructive pulmonary disease'],
  [elderly_patients],
  [],
  [shortness_of_breath],
  [],
  [],
  [lungs_hyperresonant_to_percussion,auscultation_reveals_distant_breath_sounds_scattered_rhonchi_wheezes_or_prolonged_expiration],
  [pulmonary_function_tests],
  []).
kb(chronic_or_recurrent_cough,
  [j41_simple_and_mucopurulent_chronic_bronchitis],
  ['Chronic bronchitis'],
  [most_common_cause_of_chronic_cough_in_adults_especially_smokers],
  [may_be_minimally_productive,often_worse_in_morning],
  [],
  [],
  [smoking_cessation],
  [scattered_rhonchi],
  [],
  []).
kb(chronic_or_recurrent_cough,
  [i500_congestive_heart_failure],
  ['Congestive heart failure'],
  [elderly_patients_present_differently,may_have_only_chronic_unexplained_cough],
  [cough_often_nocturnal],
  [dyspnea_on_exertion,paroxysmal_nocturnal_dyspnea],
  [recumbency,exercise],
  [diuresis],
  [rales,pitting_edema,tachycardia,gallop],
  [chest_radiograph,potent_diuretic_for_2_3_days_should_improve_symptoms,ejection_fraction],
  []).
kb(chronic_or_recurrent_cough,
  [k21_gastro_oesophageal_reflux_disease],
  ['Gastroesophageal reflux'],
  [usually_adults],
  [irritative,nonproductive_cough],
  [heartburn,eructation],
  [recumbency,ingestion_of_chocolate,caffeine,or_alcohol,exercise],
  [antireflux_measures_including_diet_drugs_and_elevating_head_of_bed],
  [usually_none],
  [upper_gastrointestinal_radiograph,esophagoscopy,esophageal_ph_monitoring],
  []).
/* Adbominal pain + location of pain */
/*kb(abdominal_pain,
  [icd_10],
  [cause],
  [nature_of_patient],
  [nature_of_pain],
  [associated_symptoms],
  [precipitating_and_aggravating_factors],
  [ameliorating_factors],
  [physical_findings],
  [diagnostic_studies],
  [location_of_pain]).*/
kb(abdominal_pain,
  [gastroenteritis],
  ['Gastroenteritis'],
  [any_age],
  [crampy],
  [nausea,vomiting,diarrhea,fever],
  [food],
  [occasional_relief_with_vomiting_or_diarrhea],
  [hyperactive_peristalsis],
  [],
  [diffuse]).
kb(abdominal_pain,
  [gastritis],
  ['Gastritis'],
  [especially_alcoholic_patients],
  [constant_burning],
  [hemorrhage,nausea,vomiting,diarrhea,fever],
  [alcohol,nsaids,salicylates,food_occasionally],
  [],
  [],
  [gastroscopy],
  [epigastrium]).
kb(abdominal_pain,
  [appendicitis],
  ['Appendicitis'],
  [any_age_peak_age_10_20_yr_m_more_than_f],
  [colicky,progressing_to_constant],
  [vomiting_after_pain_has_started,constipation,fever],
  [pain_worse_with_movement_and_coughing],
  [lying_still],
  [rlq_involuntary_guarding,rebound_to_rlq],
  [cbc_with_differential_count,ultrasonography,laparoscopy,especially_in_fertile_women_ct_scan],
  [early_epigastrium,periumbilicus,later_rlq]).
kb(abdominal_pain,
  [cholecystitis,cholelithiasis],
  ['Cholecystitis','Cholelithiasis'],
  [adults_f_more_than_m],
  [colicky,progressing_to_constant],
  [nausea,vomiting,dark_urine,light_stool,jaundice],
  [fatty_foods,drugs,oral_contraceptives,cholestyramine],
  [],
  [tenderness_on_palpation_or_percussion_in_ruq],
  [liver_function_tests,cbc,amylase,ultrasonography,isotopic_gallbladder_scan,limited_mri],
  [ruq,radiates_to_inferior_angle_of_right_scapula]).
kb(abdominal_pain,
  [diverticulitis],
  ['Diverticulitis'],
  [more_common_in_elderly],
  [intermittent_cramping],
  [constipation,diarrhea],
  [],
  [],
  [palpable_mass_in_llq],
  [laparoscopy,especially_in_women],
  [llq]).
kb(abdominal_pain,
  [pancreatitis],
  ['Pancreatitis'],
  [more_common_in_alcoholic_patients_and_patients_with_cholelithiasis],
  [steady,mild_to_sever],
  [nausea,vomiting,prostration,diaphoresis],
  [lying_supine],
  [leaning_forward],
  [abdominal_distention,decreased_bowel_sounds,diffuse_rebound],
  [amylase_measurement,ultrasonography,ct_scan,abdominal_radiographs],
  [luq,epigastric,radiates_to_back]).
kb(abdominal_pain,
  [intestinal_obstruction],
  ['Intestinal_obstruction'],
  [elderly,prior_abdominal_surgery],
  [colicky,sudden_onset],
  [vomiting,obstipation],
  [],
  [],
  [hyperactive_peristalsis_in_small_bowel_obstruction],
  [ct_scan],
  []).
kb(abdominal_pain,
  [intestinal_perforation],
  ['Intestinal_perforation'],
  [elderly_f_more_than_m],
  [sudden_onset,severe],
  [guarding,rebound],
  [pain_worse_with_movement_or_coughing],
  [lying_still],
  [decreased_bowel_sounds,guarding],
  [abdominal_radiographs,ct_scan],
  [diffuse]).
kb(abdominal_pain,
  [peritonitis],
  ['Peritonitis'],
  [],
  [sudden_or_gradual_onset],
  [],
  [],
  [],
  [diffuse_rebound],
  [],
  []).
kb(abdominal_pain,
  [salpingitis],
  ['Salpingitis'],
  [menstruating_females],
  [cramplike],
  [chandelier_sign],
  [pain_worse_while_descending_stairs_and_around_time_of_menstruation],
  [],
  [adnexa_and_cervix_tender_on_manipulation],
  [hcg_measurement_to_rule_out_ectopic_pregnancy,laparoscopy],
  [rlq,llq,nonradiating]).
kb(abdominal_pain,
  [ectopic_pregnancy],
  ['Ectopic_pregnancy'],
  [fertile_female_with_history_of_menstrual_irregularity],
  [sudden_onset,persistent_pain],
  [tender_adnexal_mass_vaginal_bleeding],
  [],
  [],
  [],
  [hcg_measurement,ultrasonography],
  [lower_quadrant]).
kb(abdominal_pain,
  [peptic_ulcer_with_perforation],
  ['Peptic ulcer with perforation'],
  [between_30_50_yr_m_more_than_f],
  [gnawing,burning,sudden_onset],
  [],
  [empty_stomach,stress,alcohol],
  [food,alkali],
  [epigastric_tenderness_on_palpation_or_percussion],
  [endoscopy,radiography],
  [epigastric_radiating_to_sides,back,or_right_shoulder]).
kb(abdominal_pain,
  [mesenteric_adenitis],
  ['Mesenteric adenitis'],
  [children,adolescents,after_respiratory_infection],
  [constant],
  [vomiting,rebound,constipation,diarrhea,fever],
  [],
  [],
  [],
  [cbc_with_differential_count],
  [rlq]).
kb(abdominal_pain,
  [ureterolithiasis],
  ['Ureterolithiasis'],
  [],
  [colicky,occasionally_progresses_to_constant,severe,sudden_onset],
  [nausea,vomiting,abdominal_distention,chills,fever],
  [],
  [],
  [costovertebral_angle_tenderness,hematuria],
  [urinalysis,intravenous_pyelography,nonenhanced_helical_ct],
  [lower_abdomen,flank,radiating_to_groin]).
kb(abdominal_pain,
  [dissection_or_rupture_of_aortic_aneurysm],
  ['Dissection or rupture of aortic aneurysm'],
  [elderly,hypertensive_40_70_yr],
  [unbearable,sudden_onset],
  [shock],
  [],
  [],
  [shock,decrease_or_difference_in_femoral_pulses],
  [radiography,ultrasonography,ct_scan],
  [chest_or_abdomen,may_radiate_to_back_and_legs]).
kb(abdominal_pain,
  [reflux_peptic_esophagitis],
  ['Reflux peptic esophagitis'],
  [],
  [burning,gnawing],
  [],
  [recumbency],
  [antacids],
  [],
  [upper_gl_radiographs,endoscopy],
  [midepigastrium,occasionally_radiating_to_jaw]).
kb(abdominal_pain,
  [irritable_bowel_syndrome],
  ['Irritable bowel syndrome'],
  [more_common_in_young_women],
  [crampy,recurrent],
  [mucus_in_stools,rome_ii_criteria],
  [],
  [pain_occasionally_relieved_by_defecation],
  [colon_tender_on_palpation],
  [],
  [most_common_in_llq]).
kb(abdominal_pain,
  [incarcerated_hernia],
  ['Incarcerated hernia'],
  [more_common_in_elderly],
  [constant],
  [],
  [coughing,straining],
  [],
  [hernia_or_mass],
  [upper_gl_radiographs],
  [rlq,llq]).
kb(abdominal_pain,
  [mesenteric_infarction],
  ['Mesenteric infarction'],
  [elderly],
  [may_be_severe],
  [tachycardia,hypotension],
  [],
  [],
  [decreased_bowel_sounds,blood_in_stools],
  [abdominal_radiographs,ct_scan],
  [diffuse]).
/*Chest pain*/
/*kb(chest_pain,
  [icd_10],
  [condition],
  [nature_of_patient],
  [nature_of_pain],
  [associated_symptoms],
  [precipitating_and_aggravating_factors],
  [ameliorating_factors],
  [physical_findings],
  [diagnostic_studies],
  []).*/
kb(chest_pain,
  [i20_angina_pectoris],
  ['I20 angina pectoris'],
  [adult],
  [nature_of_pain_achy,nature_of_pain_dull,nature_of_pain_tight,nature_of_pain_severe,nature_of_pain_pressing,nature_of_pain_not_usually_sharp_or_sticking,nature_of_pain_substernal],
  [women_are_more_likely_to_have_atypical_symptoms_such_as_back_pain,nausea,and_fatigue],
  [exertion,cold_exposure,emotional_stress],
  [nitroglycerin,rest,valsalva_s_maneuver],
  [sinus_tachycardia,bradycardia,or_apical_systolic_bulge_coincident_with_pain,xanthomas,signs_of_heart_failure],
  [exercise_ecg,coronary_arteriography,radionuclide_tests,stress_echocardiography],
  []).
kb(chest_pain,
  [variant_angina_prinzmetal_s_angina],
  ['Variant angina prinzmetals angina'],
  [adult],
  [nature_of_pain_achy,nature_of_pain_dull,tight,nature_of_pain_severe,nature_of_pain_pressing,nature_of_pain_not_usually_sharp_or_sticking,nature_of_pain_substernal],
  [],
  [often_occurs_at_rest_or_at_night],
  [nitroglycerin,rest],
  [],
  [ecg_during_attack,coronary_arteriography],
  []).
kb(chest_pain,
  [k21_gastroesophageal_reflux_disease],
  ['K21 gastroesophageal reflux disease'],
  [any_age],
  [nature_of_pain_burning,nature_of_pain_tightness,nature_of_pain_may_be_identical_to_that_of_angina],
  [water_brash_,heartburn],
  [overeating,recumbency_may_awaken_from_sleep,occasionally_precipitated_by_exertion],
  [antacids,proton_pump_inhibitors],
  [],
  [esophagoscopy,ambulatory_monitoring_of_esophageal_ph,short_course_of_high_dose_proton_pump_inhibitors],
  []).
kb(chest_pain,
  [esophageal_spasm],
  ['Esophageal spasm'],
  [especially_obese_adults],
  [nature_of_pain_may_be_identical_in_quality_to_angina],
  [],
  [often_induced_by_ingestion_of_alcohol_or_cold_liquids],
  [occasionally_relieved_by_nitroglycerin],
  [],
  [esophageal_manometry],
  []).
kb(chest_pain,
  [mitral_valve_prolapse],
  ['Mitral valve prolapse'],
  [any_age],
  [not_usually_substernal,often_has_sticking_quality,may_last_several_hours,not_typical_of_angina],
  [palpitations,arrhythmias,often_occurs_at_rest,syncope],
  [],
  [beta_blockers,recumbency],
  [click_late_systolic_murmur],
  [echocardiogram,phonocardiogram],
  []).
kb(chest_pain,
  [hypertrophic_cardiomyopathy],
  ['Hypertrophic cardiomyopathy'],
  [any_age],
  [pain_may_be_similar_to_that_of_angina],
  [dyspnea,arrhythmias,lightheadedness],
  [pain_may_be_aggravated_by_nitroglycerin],
  [beta_blockers,squatting],
  [murmur_intensified_by_nitroglycerin,valsalva_s_maneuver,decreased_by_squatting],
  [echocardiogram],
  []).
kb(chest_pain,
  [intercostal_myositis],
  ['Intercostal myositis'],
  [any_age_but_more_common_in_children_and_athletes],
  [nature_of_pain_may_have_sticking_quality],
  [],
  [may_intensify_with_inspiration],
  [],
  [localized_tenderness_on_palpation,no_pleural_friction],
  [],
  []).
kb(chest_pain,
  [costochondritis],
  ['Costochondritis'],
  [],
  [],
  [],
  [severe_coughing],
  [splinting_of_tender_area],
  [],
  [],
  []).
kb(chest_pain,
  [cervicodorsal_arthritis],
  ['Cervicodorsal arthritis'],
  [adult],
  [nature_of_pain_may_be_sharp_or_sticking,nature_of_pain_duration_only_a_few_seconds],
  [],
  [may_be_precipitated_by_certain_movements_eg,neck_exercises_and_twisting,not_related_to_stress],
  [],
  [],
  [radiographs_of_cervicodorsal_spine],
  []).
kb(chest_pain,
  [pulmonary_embolus],
  ['Pulmonary embolus'],
  [usually_adult],
  [nature_of_pain_sharp,nature_of_pain_severe,nature_of_pain_often_pleuritic],
  [tachypnea,hemoptysis],
  [prolonged_immobilization,oral_contraceptives,especially_in_smokers],
  [],
  [deep_vein_thrombosis,tachypnea,minimal_cyanosis],
  [spiral_ct_d_dimer_assay_v/q_scan,pulmonary_anqioqraphy],
  []).
kb(chest_pain,
  [pneumonia],
  ['Pneumonia'],
  [],
  [],
  [fever,cough],
  [],
  [],
  [egophony,dullness_on_percussion],
  [],
  []).
kb(chest_pain,
  [chest_wall_syndrome],
  ['Chest wall syndrome'],
  [adult,more_common_in_athletes],
  [nature_of_pain_often_sharp_and_sticking,nature_of_pain_fleeting],
  [],
  [may_be_aggravated_by_recumbency_and_certain_positions],
  [],
  [local_tenderness_on_palpation_crowing_rooster_maneuver_may_precipitate_pain],
  [],
  []).
kb(chest_pain,
  [pericarditis],
  ['Pericarditis'],
  [any_age],
  [nature_of_pain_sharp_or_dull,nature_of_pain_protracted_duration],
  [fever,recent_viral_infection],
  [],
  [],
  [pericardial_friction],
  [ecg,echocardiogram,cbc],
  []).
kb(chest_pain,
  [myocardial_infarction],
  ['Myocardial infarction'],
  [adult],
  [nature_of_pain_severe,nature_of_pain_crushing,precordial,nature_of_pain_protracted_duration],
  [sweating,fatigue,nausea],
  [],
  [not_relieved_by_nitroglycerin],
  [],
  [cardiac_troponins,ecg,serial_ck_mb_levels,radionuclide_studies],
  []).
kb(chest_pain,
  [gas_entrapment_syndrome],
  ['Gas entrapment syndrome'],
  [any_age,often_obese],
  [nature_of_pain_dull,nature_of_pain_achy],
  [flatulence],
  [aggravated_by_bending_and_tight_garments],
  [passage_of_flatus,nitroglycerin],
  [flexing_thigh_and_palpation_of_colon_may_elicit_pain],
  [gas_in_hepatic_or_splenic_flexure_on_radiographs],
  []).
