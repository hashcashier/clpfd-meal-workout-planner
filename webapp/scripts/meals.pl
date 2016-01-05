:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(random)).
:- consult(nutrition_data).

% Plan/7 for a month
plan(Goal, MealsPerDay, Weight, FatPerc, ActivityVariable, Schedule):-
	plan(Goal, MealsPerDay, Weight, FatPerc, ActivityVariable, 4, Schedule).

% Plan/8 for Weeks.
plan(Goal, MealsPerDay, Weight, FatPerc, ActivityVariable, Weeks, Schedule):-
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
	%write("Protein: "), writeln(Protein),
	%write("Fat: "), writeln(Fat),
	%write("Carbs: "), writeln(Carbs),
	%write("Calories: "), writeln(Calories),
	% Generate Daily Plans And Distribute
	random_meals(Protein, Fat, Carbs, Calories, MealsPerDay, 16, DailyPlans),
	% Plan/3 for Weeks
	plan(DailyPlans, Weeks, Schedule).

random_meals(Protein, Fat, Carbs, Calories, MealsPerDay, N, Plans):-
	%findnsols(16, DailyPlan, random_meal(Protein, Fat, Carbs, Calories, MealsPerDay, DailyPlan), DailyPlans),
	length(Plans, N),
	maplist(random_meal(Protein, Fat, Carbs, Calories, MealsPerDay), Plans).


random_meal(Protein, Fat, Carbs, Calories, MealsPerDay, DailyPlan):-
	random(1, 100101001, RandomizationSeed),
	%writeln(RandomizationSeed),
	once(today(Protein, Fat, Carbs, Calories, MealsPerDay, RandomizationSeed, DailyPlan)).

% Plan/3 for Weeks.
plan(Repertoire, Weeks, Schedule):-
	length(Schedule, Weeks),
	maplist(length2(7), Schedule),
	partition_4(Repertoire, MealsInWeek),
	maplist(distribute, MealsInWeek, Schedule).

partition_4([], []).
partition_4([A, B, C, D | Rem], [[A, B, C, D] | T]):-
	partition_4(Rem, T).

distribute(DailyPlans, WeekPlan):-
	DailyPlans = 	[A, B, C, D],
	WeekPlan   = 	[A, B, C, D, A, B, C].

today(P, F, C, L, M, RandomizationSeed, Sched):-
	%%%%% Calculate Nutrient Bounds
	PL is integer(P * 950),
	PH is integer(P * 1100),
	FL is integer(F * 950),
	FH is integer(F * 1100),
	CL is integer(C * 950),
	CH is integer(C * 1100),
	LL is integer(L * 0.95),
	LH is integer(L * 1.10),

	%%%%% Compile components table
	findall(
		[Component, Ps, Fs, Cs, Ls, UPSLower, UPSUpper, UPDLower, UPDUpper, SPDLower, SPDUpper],
		component(Component, Ps, Cs, Fs, Ls, UPSLower, UPSUpper, UPDLower, UPDUpper, SPDLower, SPDUpper),
		Components),
	transpose(Components, ComponentsTranspose),
	ComponentsTranspose = [
		ComponentNames,
		ComponentProteins, ComponentFats, ComponentCarbs, ComponentCalories,
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

	%%%%% Combination Preferences
	findall([Combination, Likes, Hates], combination(Combination, Likes, Hates, M), Combinations),

	same_length(Combinations, CombinationMatrix),
	maplist(length2(M), CombinationMatrix),
	maplist(combine_mat(BooleanMatrix, ComponentNames), Combinations, CombinationMatrix),
	transpose(CombinationMatrix, CombinationMatrixTranspose),

	same_length(Combinations, CombinationFactorMatrix),
	maplist(length2(M), CombinationFactorMatrix),
	maplist(combination_factors, Combinations, CombinationFactorMatrix),
	transpose(CombinationFactorMatrix, CombinationFactorMatrixTranspose),

	maplist(maplist(no_hated_meals), CombinationFactorMatrix, CombinationMatrix),

	maplist(product, CombinationFactorMatrixTranspose, CombinationMatrixTranspose, CombinationValueMatrixTranspose),

	length(CombinationScoreMatrix, M),
	maplist(sum2(#=), CombinationValueMatrixTranspose, CombinationScoreMatrix),
	sum(CombinationScoreMatrix, #=, MealScore),

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
	sum(FlatBooleanMatrix, #=, MealCount),
	MealScore #>= MealCount//2 + 1,
	%labeling([max(MealScore)], FlatBooleanMatrix),
	%label(FlatBooleanMatrix),
	labeling([random_variable(RandomizationSeed),random_value(RandomizationSeed)], FlatBooleanMatrix),

	%extract(ComponentNames, BooleanMatrixTranspose, Sched2),
	%write("Items: "), write(MealCount), write(" Score: "), writeln(MealScore),
	%pretty_print(1, Sched2),

	%%%%% Component X Meal Flow Mat
	length(FlowMatrix, N),
	maplist(length2(M), FlowMatrix),
	maplist(dom_limits, FlowMatrix, ComponentUPSLowers, ComponentUPSUppers),
	%transpose(FlowMatrix, FlowMatrixTranspose),
	no_zero_factors(BooleanMatrix, FlowMatrix),
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
	once(label(FlatMatrix)),
	%write("One day plan generated with score: "), writeln(MealScore),
	extract(ComponentNames, MatrixTranspose, Sched).
	%pretty_print(1, Sched).

no_hated_meals(Factor, ANDResult):-
	(Factor < 0, ANDResult #= 0) ; Factor >= 0.

combination_factors([Components, LikedIn, HatedIn], Row):-
	length(Components, N),
	foldl(combination_factors(LikedIn, HatedIn, N), Row, 1, _).
combination_factors(LikedIn, HatedIn, Value, Variable, N, N1):-
	N1 is N+1,
	(
		(member(N, LikedIn), Variable #= Value) ; 
		(member(N, HatedIn), Variable #= -Value) ; 
		(\+member(N, LikedIn), \+member(N, HatedIn), Variable #= 0)).

combine_mat(BooleanMatrix, ComponentNames, Combination, ANDedComponents):-
	Combination = [Components, _, _],
	extract_submat(BooleanMatrix, ComponentNames, Components, BooleanSubMatrix),
	transpose(BooleanSubMatrix, BooleanSubMatrixTranspose),
	maplist(running_product(1), BooleanSubMatrixTranspose, ANDedComponents).

extract_submat(Matrix, Components, List, SubMatrix):-
	maplist(extract_row(Matrix, Components), List, SubMatrix).
extract_row(Matrix, Components, Component, Row):-
	nth1(I, Components, Component),
	nth1(I, Matrix, Row).

running_product(ACC, [], V):-
	V #= ACC.
running_product(ACC, [H|T], V):-
	running_product(H*ACC, T, V).

amongst(L, [C|_]):-
	member(C, L).

no_zero_factors(BooleanMatrix, FlowMatrix):-
	maplist(maplist(boolean_sync), BooleanMatrix, FlowMatrix).
boolean_sync(A, B):-
	(A > 0, B #> 0);
	(A = 0, B #= 0).

preferences(Matrix, Meals):-
	foldl(preferences(Meals), Matrix, 1, _).
preferences(Meals, Value, N, N1):-
	N1 is N+1,
	(member(N, Meals), Value #= 1 ; \+member(N, Meals), Value #= 0).

hated_timings(Meals, Hated):-
	foldl(hated_timings(Hated), Meals, 1, _).
hated_timings(Hated, M, N, N1):-
	N1 is N+1,
	((member(N, Hated), M #= 0);\+member(N, Hated)).

daily_bounds(Matrix, UPDL, UPDU):- summation_bounds(Matrix, UPDL, UPDU).

serving_bounds(BooleanMatrix, SPDL, SPDU):- summation_bounds(BooleanMatrix, SPDL, SPDU).

pretty_print(_, []).
pretty_print(N, [H|T]):-
	write("Meal "), write(N), writeln(": "),
	pretty_print(H),
	N1 is N+1,
	pretty_print(N1, T).
pretty_print([]).
pretty_print([H|T]):-
	H = eat(Quantity, Name),
	write("\t"), write(Name), write(": "), write(Quantity), writeln(" Serving(s)"),
	pretty_print(T).

meal_bounds(BooleanMatrixTranspose):-
	% No Empty Meals
	maplist(sum3(#>, 0), BooleanMatrixTranspose),
	% No Super Salad Meals
	maplist(sum3(#<, 5), BooleanMatrixTranspose),
	length(BooleanMatrixTranspose, N),
	(
		(N = 3, threeMealRules(BooleanMatrixTranspose));
		(N = 4, oneSnackRules(BooleanMatrixTranspose));
		(N = 5, twoSnacksRules(BooleanMatrixTranspose));
		(N < 3 ; N > 5)). % Not Applicable..

threeMealRules(BooleanMatrixTranspose):-
	BooleanMatrixTranspose = [Breakfast, Lunch, Dinner],
	sum(Breakfast, #>, 1), sum(Breakfast, #<, 4),%2..3
	sum(Lunch, #>, 2), %3..4
	sum(Dinner, #>, 1), sum(Dinner, #<, 4). %2..3
oneSnackRules(BooleanMatrixTranspose):-
	BooleanMatrixTranspose = [Breakfast, Snack, Lunch, Dinner],
	sum(Breakfast, #>, 1), sum(Breakfast, #<, 4),%2..3
	sum(Snack, #<, 3), %1..2
	sum(Lunch, #>, 2), %3..4
	sum(Dinner, #>, 1), sum(Dinner, #<, 4). %2..3

twoSnacksRules(BooleanMatrixTranspose):-
	BooleanMatrixTranspose = [Breakfast, Snack1, Lunch, Snack2, Dinner],
	sum(Breakfast, #>, 1), sum(Breakfast, #<, 4),%2..3
	sum(Snack1, #<, 3), %1..2
	sum(Lunch, #>, 2), %3..4
	sum(Snack2, #<, 3), %1..2
	sum(Dinner, #>, 1), sum(Dinner, #<, 4). %2..3
	

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

dom_limits(Row, Lower, Upper):-
	Row ins 0 \/ Lower..Upper.
boolean_dom_limits(Row):- Row ins 0 \/ 1.

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