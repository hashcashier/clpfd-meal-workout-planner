:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(apply)).

% Plan/6 for a month
plan(Goal, MealsPerDay, Weight, FatPerc, ActivityVariable, Schedule):-
	plan(Goal, MealsPerDay, Weight, FatPerc, ActivityVariable, 3, 7, Schedule).

% Plan/8 for 7*Weeks + Days days.
plan(Goal, MealsPerDay, Weight, FatPerc, ActivityVariable, Weeks, Days, Schedule):-
	MealsPerDay > 2,
	MealsPerDay < 6,
	LBM is Weight*(100 - FatPerc) / 100,
	BMR is 370 + 21.6 * LBM,
	TEE is ActivityVariable * BMR,
	calories(Goal, TEE, CAL),
	protein(LBM, Protein),
	fat(LBM, Fat),
	carbs(CAL, Protein, Fat, Carbs),
	Calories is integer(CAL),
	write("Protein: "), writeln(Protein),
	write("Fat: "), writeln(Fat),
	write("Carbs: "), writeln(Carbs),
	write("Calories: "), writeln(Calories),
	% Plan/10 for 7*Weeks + Days days
	plan(Protein, Fat, Carbs, Calories, MealsPerDay, Schedule, Weeks, Days, [], []).

% Plan/10 complete. Dump Accumulators
plan(_, _, _, _, _, [WACC|SACC], 0, 0, WACC, SACC).

% Plan/10 for a new week
plan(Protein, Fat, Carbs, Calories, MPD, Schedule, Weeks, 0, WACC, SACC):-
	Weeks > 0,
	RemWeeks is Weeks-1,
	%%%%%%%%%%%%%%%%%%%noWeeksRep(WACC, SACC),
	% Plan/10 for a day
	plan(Protein, Fat, Carbs, Calories, MPD, Schedule, RemWeeks, 7, [], [WACC|SACC]).

% Plan/10 for this week
plan(Protein, Fat, Carbs, Calories, MPD, Schedule, Weeks, Days, WACC, SACC):-
	Days > 0,
	today(Protein, Fat, Carbs, Calories, MPD, Today),
	%%%%%%%%%%%%%%%%%%%%%thirdMealRule(Today, WACC),
	RD is Days - 1,
	%Plan/10 for the remaining days
	plan(Protein, Fat, Carbs, Calories, MPD, Schedule, Weeks, RD, [Today|WACC], SACC).

today(P, F, C, L, M, Sched):-
	%%%%% Calculate Nutrient Bounds
	PL is integer(P * 950),
	PH is integer(P * 1050),
	FL is integer(F * 950),
	FH is integer(F * 1050),
	CL is integer(C * 950),
	CH is integer(C * 1050),
	LL is integer(L * 0.95),
	LH is integer(L * 1.05),

	%%%%% Compile components table
	findall(
		[Component, Ps, Fs, Cs, Ls, Likes, Hates, UPSLower, UPSUpper, UPDLower, UPDUpper, SPDLower, SPDUpper],
		component(Component, Ps, Cs, Fs, Ls, Likes, Hates, UPSLower, UPSUpper, UPDLower, UPDUpper, SPDLower, SPDUpper),
		Components),
	transpose(Components, ComponentsTranspose),
	ComponentsTranspose = [
		ComponentNames,
		ComponentProteins, ComponentFats, ComponentCarbs, ComponentCalories,
		ComponentLikedIns, ComponentHatedIns,
		ComponentUPSLowers, ComponentUPSUppers,
		ComponentUPDLowers, ComponentUPDUppers, 
		ComponentSPDLowers, ComponentSPDUppers],
	Nutrients = [ComponentProteins, ComponentFats, ComponentCarbs, ComponentCalories],
	length(ComponentNames, N),

	%%%%% Component X Meal Adj Mat
	length(BooleanMatrix, N),
	maplist(length2(M), BooleanMatrix),
	maplist(boolean_dom_limits, BooleanMatrix),
	transpose(BooleanMatrix, BooleanMatrixTranspose),
	meal_bounds(BooleanMatrixTranspose),
	serving_bounds(BooleanMatrix, ComponentSPDLowers, ComponentSPDUppers),
	maplist(hated_timings, BooleanMatrix, ComponentHatedIns),

	%%%%% Component X Meal Preference Matrix
	length(PreferenceMatrix, N),
	maplist(length2(M), PreferenceMatrix),
	maplist(preferences, PreferenceMatrix, ComponentLikedIns),

	%%%%% Component X Meal Preference Value Matrix
	length(PrefValueMatrix, N),
	maplist(length2(M), PrefValueMatrix),
	maplist(product, PreferenceMatrix, BooleanMatrix, PrefValueMatrix),
	transpose(PrefValueMatrix, PrefValueMatrixTranspose),

	%%%%% Meal Preference Score Matrix
	length(PrefScoreMatrix, M),
	maplist(sum2(#=), PrefValueMatrixTranspose, PrefScoreMatrix),
	sum(PrefScoreMatrix, #=, PreferenceScore),

	%%%%% Nutrient (PFCL) X Meal Boolean Aggregate
	length(BooleanAggregate, 4),
	maplist(length2(M), BooleanAggregate),
	transpose(BooleanAggregate, BooleanAggregateTranspose),
	maplist(bind_aggregates(Nutrients), BooleanAggregateTranspose, BooleanMatrixTranspose),

	%%%%% Nutrient Sums Upper Bounds
	BooleanTotal = [BPT, BFT, BCT, BLT],
	maplist(sum2(#=), BooleanAggregate, BooleanTotal),
	BPT #=< PH,
	BFT #=< FH,
	BCT #=< CH,
	BLT #=< LH,

	%%%%% Label boolean factors
	flatten(BooleanMatrix, FlatBooleanMatrix),
	labeling([max(PreferenceScore)], FlatBooleanMatrix),
	%extract(ComponentNames, BooleanMatrixTranspose, Sched),
	%write("Meal Score: "), writeln(PreferenceScore),
	%display(1, Sched),

	%%%%% Component X Meal Flow Mat
	length(FlowMatrix, N),
	maplist(length2(M), FlowMatrix),
	maplist(dom_limits, FlowMatrix, ComponentUPSLowers, ComponentUPSUppers),
	%transpose(FlowMatrix, FlowMatrixTranspose),
	daily_bounds(FlowMatrix, ComponentUPDLowers, ComponentUPDUppers),

	%%%%% Component X Meal Serving Mat
	length(Matrix, N),
	maplist(length2(M), Matrix),
	maplist(product, FlowMatrix, BooleanMatrix, Matrix),
	transpose(Matrix, MatrixTranspose),

	%%%%% Nutrient (PFCL) X Meal Boolean Aggregate
	length(Aggregate, 4),
	maplist(length2(M), Aggregate),
	transpose(Aggregate, AggregateTranspose),
	maplist(bind_aggregates(Nutrients), AggregateTranspose, MatrixTranspose),

	%%%%% Nutrient Sums Bounds
	Total = [PT, FT, CT, LT],
	maplist(sum2(#=), Aggregate, Total),
	PL #=< PT, PT #=< PH,
	FL #=< FT, FT #=< FH,
	CL #=< CT, CT #=< CH,
	LL #=< LT, LT #=< LH,

	%%%%% Label factors
	flatten(Matrix, FlatMatrix),
	label(FlatMatrix),
	extract(ComponentNames, MatrixTranspose, Sched),
	write("Meal Score: "), writeln(PreferenceScore),
	display(1, Sched).

preferences(Matrix, Meals):-
	foldl(preferences(Meals), Matrix, 1, _).
preferences(Meals, Value, N, N1):-
	N1 is N+1,
	(member(N, Meals) -> Value #= 1 ; \+member(N, Meals) -> Value #= 0).

hated_timings(Meals, Hated):-
	foldl(hated_timings(Hated), Meals, 1, _).
hated_timings(Hated, M, N, N1):-
	N1 is N+1,
	((member(N, Hated), M #= 0);\+member(N, Hated)).

daily_bounds(Matrix, UPDL, UPDU):- summation_bounds(Matrix, UPDL, UPDU).

serving_bounds(BooleanMatrix, SPDL, SPDU):- summation_bounds(BooleanMatrix, SPDL, SPDU).

display(_, []).
display(N, [H|T]):-
	write("Meal "), write(N), writeln(": "),
	display(H),
	N1 is N+1,
	display(N1, T).
display([]).
display([H|T]):-
	H = eat(Quantity, Name),
	write("\t"), write(Name), write(": "), write(Quantity), writeln(" Serving(s)"),
	display(T).

meal_bounds(BooleanMatrix):-
	% No Empty Meals
	maplist(sum3(#>, 0), BooleanMatrix),
	% No Super Salad Meals
	maplist(sum3(#<, 6), BooleanMatrix),
	length(BooleanMatrix, N),
	(
		(N = 3, threeMealRules(BooleanMatrix));
		(N = 4, oneSnackRules(BooleanMatrix));
		(N = 5, twoSnacksRules(BooleanMatrix));
		(N < 3 ; N > 5)). % Not Applicable..

threeMealRules(_). %%%% TO BE IMPLEMENTED
oneSnackRules(_). %%%% TO BE IMPLEMENTED
twoSnacksRules(_). %%%% TO BE IMPLEMENTED

extract(Components, Matrix, Schedule):-
	foldr(extract(Components), Matrix, [], Schedule).
extract(Components, Meal, T, [H|T]):-
	foldl(extractPlan, Meal, Components, [], H).
extractPlan(0, _, A, A).
extractPlan(X, C, T, [H|T]):-
	X > 0,
	H = eat(X, C).

bind_aggregates(Nutrients, Aggregates, Meal):-
	maplist(bind_aggregate(Meal), Nutrients, Aggregates).
bind_aggregate(M, N, Aggregate):-
	product(M, N, P),
	sum(P, #=, Aggregate).

dom_limits(Row, Lower, Upper):- Row ins 0 \/ Lower..Upper.
boolean_dom_limits(Row):- Row ins 0 \/ 1.

% Component(UNIQUE Name, 			Protein, Carbs, Fats, 	Cals, 	Pref in, 	Hate in, 	Min U/S, 	Max U/S, 	Min U/D,	Max U/D,	Min S/D,	Max S/D)
component(banana,					1100,	300,	23000,	89,		[],			[],			1, 			2, 			0,			2, 			0,			1).
component(broccoli,					2800,	7000,	400,	34,		[],			[],			1, 			2, 			0,			2, 			0,			1).
component(carrots,					900,	10000,	200,	41,		[],			[],			1, 			2, 			0,			2, 			0,			1).
component(cheddar_cheese,			25000,	1300,	33000,	402,	[1,2,5],	[],			1, 			2, 			0,			2, 			0,			1).
component(chicken_breast_uncooked,	21200,	0,		2500,	114,	[3],		[1],		1, 			4, 			0,			4, 			0,			1).
component(egg_whites_uncooked,		11000,	700,	200,	52,		[],			[],			1, 			3, 			0,			3, 			0,			1).
component(fish,						19000,	0,		6000,	134,	[3],		[1],		1, 			4, 			0,			4, 			0,			1).
component(gouda_cheese,				25000,	2200,	27000,	356,	[1,2,5],	[],			1, 			3, 			0,			3, 			0,			1).
component(honey,					300,	82000,	0,		304,	[],			[],			1, 			1, 			0,			1, 			0,			1).
component(lean_beef_uncooked,		20000,	0,		6000,	137,	[3],		[1],		1, 			4, 			0,			4, 			0,			1).
component(milk_full_fat,			3300,	4600,	3700,	64,		[],			[],			1, 			2, 			0,			2, 			0,			1).
component(multi_grain_bread,		13000,	43000,	4200,	265,	[],			[],			1, 			2, 			0,			2, 			0,			1).
component(olive_oil,				0,		0,		100000,	884,	[],			[],			1, 			1, 			0,			1, 			0,			1).
component(pasta_uncooked,			13000,	75000,	1500,	371,	[3],		[1],		1, 			2, 			0,			2, 			0,			1).
component(peanut_butter,			25000,	20000,	50000,	588,	[],			[],			1, 			2, 			0,			2, 			0,			1).
component(pear,						400,	15000,	100,	57,		[],			[],			1, 			2, 			0,			2, 			0,			1).
component(potato_uncooked,			2000,	17000,	100,	77,		[],			[],			1, 			2, 			0,			2, 			0,			1).
component(salmon_uncooked,			20000,	0,		13000,	208,	[3],		[1],		1, 			2, 			0,			2, 			0,			1).
component(shrimp_uncooked,			20000,	0,		500,	85,		[3],		[1],		1, 			2, 			0,			2, 			0,			1).
component(skimmed_milk,				3400,	5000,	100,	34,		[],			[],			1, 			2, 			0,			2, 			0,			1).
component(spinach,					2900,	3600,	400,	23,		[],			[1],		1, 			3, 			0,			3, 			0,			1).
component(sweet_potato_uncooked,	1600,	20000,	0,		86,		[],			[],			1, 			2, 			0,			2, 			0,			1).
component(swiss_cheese,				27000,	5000,	28000,	380,	[1,2,5],	[],			1, 			2, 			0,			2, 			0,			1).
component(tomato,					900,	3900,	200,	18,		[],			[],			1, 			4, 			0,			4, 			0,			1).
component(tuna_brine,				26000,	0,		1000,	116,	[],			[1],		1, 			3, 			0,			3, 			0,			1).
component(whey_protein,				80000,	10000,	3300,	400,	[],			[],			1, 			1, 			0,			1, 			0,			1).
component(white_rice_uncoooked,		7000,	82000,	600,	370,	[],			[],			1, 			1, 			0,			1, 			0,			1).
component(whole_egg_uncooked,		13000,	700,	10000,	143,	[1],		[],			1, 			3, 			0,			3, 			0,			1).

% Unit Conversion Predicates
calories(bulk, TEE, CAL):- CAL is TEE * 1.2.
calories(cut, TEE, CAL):- CAL is TEE * 0.8.
protein(LBM, P):-
	mass(LBM, LB),
	P is 1.5 * LB.
fat(LBM, F):-
	mass(LBM, LB),
	F is 0.4 * LB.
carbs(CAL, P, F, C):-
	caloriesFat(F, FC),
	caloriesProt(P, PC),
	C is (CAL - FC - PC) / 4.

caloriesFat(F, C):- C is 9*F.
caloriesProt(P, C):- C is 4*P.

mass(KG, LB):- LB is 2.20462 * KG.

% Helper Predicates
length2(N, L):- length(L, N).

sum2(R, V, E):- sum(V, R, E).
sum3(R, E, V):- sum(V, R, E).

foldr(Goal, List, V0, V):-
	reverse(List, RList),
	foldl(Goal, RList, V0, V).
foldr(Goal, List1, List2, V0, V):-
	reverse(List1, RList1),
	reverse(List2, RList2),
	foldl(Goal, RList1, RList2, V0, V).

product(A, B, P):-
	foldr(product, A, B, [], P).
product(A, B, T, [H|T]):-
	H #= A*B.

summation_bounds(Summations, Lower, Upper):-
	maplist(sum2(#>=), Summations, Lower),
	maplist(sum2(#=<), Summations, Upper).

boolean_reduction(Integer, Boolean):- Boolean #= min(1, Integer).