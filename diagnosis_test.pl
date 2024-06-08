:- use_module(diagnosis).
:- begin_tests(test).

test(a) :-
        test_vresAsthenies(
           acute_cough
           , [cough_worse_at_night,acute_onset]
           , ['Bacterial pneumonia']
           , []
        )

        , test_vresAsthenies(
          acute_cough
          , [tachycardia]
          , []
          , ['Bacterial pneumonia']
        )

        , test_vresAsthenies(
           acute_cough
           , [wheezing,chronic_debilitating_conditions,chills,abdominal_pain]
           , ['Bacterial pneumonia']
           , ['Viral bronchitis']
        )

        , test_vresAsthenies(
           chronic_or_recurrent_cough
           , [wheezing,chronic_debilitating_conditions,chills,abdominal_pain]
           , []
           , ['Postnasal_drip', 'Asthma', 'Gastroesophageal reflux']
        )

        , test_vresAsthenies(
           abdominal_pain
           , [lying_still,dark_urine,cramplike]
           , ['Salpingitis', 'Cholecystitis','Cholelithiasis', 'Appendicitis', 'Intestinal_perforation']
           , []
        ).

:- end_tests(test).

test_vresAsthenies(Complaint, Symptoms, Results1, Results2) :-
    format("~n ~w ~a ~n ~w ~p ~n", ["Complaint: ", Complaint, "Symptoms: ", Symptoms]),

    vresAsthenies(
      Symptoms
      ,ResultKB1
      ,Complaint
      ,kb
    ),

    maplist(firstElem, ResultKB1, ConditionsKB1),
    flatten(ConditionsKB1, ConditionsKB1Flat),
    format("    ~w ~p ~n    ~w ~p ~n", ["KB1 expected result: ", Results1, "Actual result: ", ConditionsKB1Flat]),
    assertion(ConditionsKB1Flat == Results1),

    vresAsthenies(
      Symptoms
      ,ResultKB2
      ,Complaint
      ,kb2
    ),

    maplist(firstElem, ResultKB2, ConditionsKB2),
    flatten(ConditionsKB2, ConditionsKB2Flat),
    format("    ~w ~p ~n    ~w ~p ~n", ["KB2 expected result: ", Results2, "Actual result: ", ConditionsKB2Flat]),
    assertion(ConditionsKB2Flat == Results2).

firstElem((C, _, _, _, _, _, _), C).
