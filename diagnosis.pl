:- module(diagnosis, [vresAsthenies/4]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_error)).
:- use_module(library(http/http_parameters)).
:- use_module(library(uri)).
:- use_module(library(pairs)).

:- http_handler(root(.), ask_complaint, []).
:- http_handler(root(symptoms), ask_symptoms, []).
:- http_handler(root(calculate), calculate, []).

:- include(kb1).
:- include(kb2).

server(Port) :-
	http_server(http_dispatch, [port(Port)]).

bootstrap -->
	a_css_link('https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/css/bootstrap.min.css'),
	a_css_link('https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/css/bootstrap-theme.min.css').

a_css_link(Url) -->
  html(link([type('text/css'),rel('stylesheet'),href(Url)])).

ask_complaint(_Request) :-
	reply_html_page(title('Complaints'),
			\bootstrap,
			[
			html(
			div( class="container"
			, [ div( class="card",
				[ div( class="card-body", h1(class="card-title", 'Select a common complaint:')),

				div( class="card-body",
				div( class="input-group", span([class="input-group-addon"],
					form([class="form-check", action='/symptoms'],
					[ html(p([class="text-left"], html_form:form_field(input([type='radio', class="form-check-input", name="complaint", value="acute_cough", checked], ["Acute cough"]))))
					, html(p([class="text-left"], html_form:form_field(input([type='radio', class="form-check-input", name="complaint", value="chronic_or_recurrent_cough"], ["Chronic or recurrent cough"]))))
					, html(p([class="text-left"], html_form:form_field(input([type='radio', class="form-check-input", name="complaint", value="abdominal_pain"], ["Abdominal pain"]))))
					, html(p([class="text-left"], html_form:form_field(input([type='radio', class="form-check-input", name="complaint", value="chest_pain"], ["Chest pain"]))))

					, html(p(html_form:form_field(input([type='submit', name='submit', class="btn btn-primary"], []))))
					]
			)))
			)]
			)]
		)
		)
		]).

remove_last([], []).
remove_last(L, T) :- reverse(L, [_H|T]).

ask_symptoms(Request) :-
	http_parameters(Request, [], [ form_data(Form) ]),
	remove_last(Form, T), % Remove the submit parameter from the list
	maplist(value, T, [Complaint]),

	setof(
			[ Nature_Of_Patient
			, Nature_Of_Symptoms
			, Associated_Symptoms
			, Precipitating_And_Aggravating_Factors
			, Ameliorating_Factors
			, Physical_Findings
			]
		, kb( Complaint
				, Icd10
				, Condition
				, Nature_Of_Patient
				, Nature_Of_Symptoms
				, Associated_Symptoms
				, Precipitating_And_Aggravating_Factors
				, Ameliorating_Factors
				, Physical_Findings
				, Diagnostic_Studies
				, Location_Of_Pain
				)
				^kb( Complaint
						, Icd10
						, Condition
						, Nature_Of_Patient
						, Nature_Of_Symptoms
						, Associated_Symptoms
						, Precipitating_And_Aggravating_Factors
						, Ameliorating_Factors
						, Physical_Findings
						, Diagnostic_Studies
						, Location_Of_Pain
						)
		, L1
		)

	, setof(
			[ Nature_Of_Patient
			, Nature_Of_Symptoms
			, Associated_Symptoms
			, Precipitating_And_Aggravating_Factors
			, Ameliorating_Factors
			, Physical_Findings
			]
			, kb2( Complaint
						, Icd10
						, Condition
						, Nature_Of_Patient
						, Nature_Of_Symptoms
						, Associated_Symptoms
						, Precipitating_And_Aggravating_Factors
						, Ameliorating_Factors
						, Physical_Findings
						, Diagnostic_Studies
						, Location_Of_Pain
						)
				^kb2( Complaint
						, Icd10
						, Condition
						, Nature_Of_Patient
						, Nature_Of_Symptoms
						, Associated_Symptoms
						, Precipitating_And_Aggravating_Factors
						, Ameliorating_Factors
						, Physical_Findings
						, Diagnostic_Studies
						, Location_Of_Pain
					)
			, L2
		)

	, append(L1, L2, L)
	, flatten(L, SymptomsFlat)
	, list_to_set(SymptomsFlat, SymptomataNoDup)
	, sort(SymptomataNoDup, SymptomataSorted)
	, maplist(convertToSpaces, SymptomataSorted, SymptomataDisplay)

	, reply_html_page(title('Symptoms')
			, \bootstrap
			, [
				html(
				div( class="container"
				, [ div( class="card"
				, [ div( class="card-body"
						, h1(class="card-title", 'Select symptoms:')
						)
					, div( class="card-body"
					, div( class="form-check"
						, span([class="input-group-addon"]
						,	form([class="navbar-form navbar-left", action='/calculate']
						,	[ html(
								html_form:form_field(
								input( [ type='hidden', name='complaint', value=Complaint], [])
								))
							, \symptomata(SymptomataDisplay)
							]
						)))
				)]
			)]
		)
		)
		]).

convertToSpaces(S, (S, SNoUnderscores)) :-
	re_replace("_"/g, " ", S, SNoUnderscores).

symptomata([]) -->
		html(p(html_form:form_field(input([type='submit', name='submit', class="btn btn-primary"], [])))).
symptomata([H|T]) -->
	symptoma(H), symptomata(T).

symptoma((S, S2)) -->
	html( p([class="text-left"]
			, html_form:form_field(input([type='checkbox', class="form-check-input", name=S], [S2]))
			)).

calculate(Request) :-
				http_parameters(Request, [ complaint(Complaint, [])], [ form_data(Form) ])
			, remove_last(Form, T)
			, maplist(name, T, PosSym)
			, include(exclude_hidden, PosSym, PositiveSymptoms)   % remove the hidden input element for complaint from the list

			, vresAsthenies(PositiveSymptoms, Asthenies, Complaint, kb)
			% , print_message(debug, (PositiveSymptoms, Asthenies, Complaint, kb))
			, calculateConfidence(Asthenies, PositiveSymptoms, Result)

			, vresAsthenies(PositiveSymptoms, Asthenies2, Complaint, kb2)
			, calculateConfidence(Asthenies2, PositiveSymptoms, Result2)
			% , print_message(debug, (PositiveSymptoms, Asthenies2, Complaint, kb2))

			, append(Result, Result2, L)
				%sort by confidence
			, sort(1, @>=, L, SortedResults)
				%keep only highest confidence condition if duplicate entries exist
			, dedupConditions(SortedResults, DedupResults)
			% re-sort
			, sort(1, @>=, DedupResults, SortedDedupResults)

			, reply_html_page(
					title('Results'),
					\bootstrap,
					[
					html(
						div( class="container"
						, [ div( class=""
							, [ div( class="card-body"
								,	h1(class="card-title, display-4", 'Potential conditions'))

								, div( class="row"
									, [ div(class="col", [
												div( class=" "
												,	h2(class="card-title", 'According to Symons and Seller:'))

												, table( class="table table-striped"
													, [ thead(tr([th(scope="col", "Condition"), th(scope="col", "Confidence"), th(scope="col", "Present Symptoms"), th(scope="col", "Condition's Associated Symptoms")]))
														, tbody(\show_conditions(Result))
														]
												)

											])
											, div(class="col", [
												div( class="card-body"
												,	h2(class="card-title", 'According to Nadler and Gonzales:'))

												, table( class="table table-striped"
													, [ thead(tr([th(scope="col", "Condition"), th(scope="col", "Confidence"), th(scope="col", "Present Symptoms"), th(scope="col", "Condition's Associated Symptoms")]))
														, tbody(\show_conditions(Result2))
														]
												)
											])

											, div(class="col", [
												div( class="card-body"
												,	h2(class="card-title", 'Combined results:'))

												, table( class="table table-striped"
													, [ thead(tr([th(scope="col", "Condition"), th(scope="col", "Confidence")]))
														, tbody(\show_conditions_combined(SortedDedupResults))
														]
												)
											])
										]
								)]
							)]
						)
					)
				]
			).

dedupConditions([], []).
dedupConditions(In, Out) :- dedupConditions(In, Out, [], []).

dedupConditions([], Out, Out, _).
% The list is already sorted by confidence so only keep the first occurence (highest confidence) of each condition
dedupConditions([(ConfPc, (Cond, ConfTxt, PosSym, AssocSym))|T], Out, Result, Seen) :-
	 member(Cond, Seen) % already seen: so do nothing
	, dedupConditions(T, Out, Result, Seen)
	; dedupConditions(T, Out, [(ConfPc, (Cond, ConfTxt, PosSym, AssocSym))|Result], [Cond|Seen]).


% Returns (Confidence (as integer), (Condition, Confidence (as text), RelevantPositiveSymptoms, AssociatedSymptoms))
calculateConfidence([], _, []).
calculateConfidence(Asthenies, Pos, Result) :- calculateConfidence(Asthenies, Pos, [], Result).
calculateConfidence([], _Pos, Result, Result).

calculateConfidence([(Asthenia, AssociatedSymptoms, NatureOfSymptoms, NatureOfPatient, ParticipatingFactors, AmelioratingFactors, PhysicalFindings)|T], PositiveSymptoms, Answer, Result) :-

	append([AssociatedSymptoms, NatureOfSymptoms, NatureOfPatient, ParticipatingFactors, AmelioratingFactors, PhysicalFindings]
					, AssociatedAndNatureOfSymptoms),
	list_to_set(AssociatedAndNatureOfSymptoms, AssociatedAndNatureOfSymptomsNoDup),
  length(AssociatedAndNatureOfSymptomsNoDup, LengthOfAssociatedAndNatureOfSymptoms),

  intersection(PositiveSymptoms, AssociatedAndNatureOfSymptomsNoDup, RelevantPositiveSymptoms),
  length(RelevantPositiveSymptoms, LengthOfRelevantPositiveSymptoms),

	maplist(convertToSpaces, RelevantPositiveSymptoms, RelevantPositiveSymptomsDisplay),
	maplist(convertToSpaces, AssociatedAndNatureOfSymptomsNoDup, AssociatedSymptomsDisplay),
	ConfPercent is floor((LengthOfRelevantPositiveSymptoms / LengthOfAssociatedAndNatureOfSymptoms) * 100),

	calculateConfidence(T
					, PositiveSymptoms
					, [ (ConfPercent,
								(Asthenia
								, format('~d ~w ~w ~d', [LengthOfRelevantPositiveSymptoms, " out", " of ", LengthOfAssociatedAndNatureOfSymptoms])
								, RelevantPositiveSymptomsDisplay
								, AssociatedSymptomsDisplay
								)
							)
						| Answer]
					, Result
					).

show_conditions([]) --> [].
show_conditions([(_ConfPercent, (Condition, Confidence, PositiveSymptoms, AssocSymptoms))|T]) -->
		html(tr([td(Condition), td(Confidence), td(\predicates_ul(PositiveSymptoms)), td(\predicates_ul(AssocSymptoms))]))
		, show_conditions(T).

show_conditions_combined([]) --> [].
show_conditions_combined([(ConfPercent, (Condition, _Confidence, _PositiveSymptoms, _AssocSymptoms))|T]) -->
		html(tr([td(Condition), td(format('~d~c',[ConfPercent, "%"]))]))
		, show_conditions_combined(T).

exclude_hidden(S) :- S \== complaint.
name(A=_B, A).
value(_A=B, B).

predicates_ul(Preds) -->
	html(ul([class="list-group"], \predicate_list(Preds))).

predicate_list([]) --> [].
predicate_list([(_, S2)|T]) -->
	html(li([class="list-group-item"], S2)),
	predicate_list(T).

/*logical connection between caracteristic and conditions*/
vresAsthenies([], [], _, _).
vresAsthenies(PositiveSymptoms, AstheniesWithAssociatedSymptomsNoDup, Complaint, KB) :-
	vresAsthenies(PositiveSymptoms, AstheniesWithAssociatedSymptoms, [], Complaint, KB)
	, list_to_set(AstheniesWithAssociatedSymptoms, AstheniesWithAssociatedSymptomsNoDup).

vresAsthenies([], Answer, Answer, _Complaint, _KB).
vresAsthenies([H|T], Asthenies, Answer, Complaint, KB) :-
	findall(
		(Condition
			, Associated_Symptoms
			, Nature_Of_Symptoms
			, Nature_Of_Patient
			, Precipitating_And_Aggravating_Factors
			, Ameliorating_Factors
			, Physical_Findings
		  )
		, (call(KB
			, Complaint
			, _Icd10
			, Condition
			, Nature_Of_Patient
			, Nature_Of_Symptoms
			, Associated_Symptoms
			, Precipitating_And_Aggravating_Factors
			, Ameliorating_Factors
			, Physical_Findings
			, _Diagnostic_Studies
			, _Location_Of_Pain
			)
		, ( member(H, Nature_Of_Symptoms)
			; member(H, Associated_Symptoms)
			; member(H, Nature_Of_Patient)
			; member(H, Precipitating_And_Aggravating_Factors)
			; member(H, Ameliorating_Factors)
			; member(H, Physical_Findings)
			)
		)
	, As)

	, append(As, Answer, NewAnswer)
	, vresAsthenies(T, Asthenies, NewAnswer, Complaint, KB).

