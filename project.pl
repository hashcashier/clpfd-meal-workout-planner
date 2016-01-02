:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(apply)).

% Plan/6 for a month
plan(Goal, MealsPerDay, Weight, FatPerc, ActivityVariable, Schedule):-
	MealsPerDay > 2,
	MealsPerDay < 6,
	LBM is Weight*(100 - FatPerc) / 100,
	BMR is 370 + 21.6 * LBM,
	TEE is ActivityVariable * BMR,
	calories(Goal, TEE, CAL),
	protein(LBM, Protein),
	fat(LBM, Fat),
	carbs(CAL, Protein, Fat, Carbs),
	MinProtein is integer(Protein * 950),
	MinFat is integer(Fat * 950),
	MinCarbs is integer(Carbs * 950),
	MaxProtein is integer(Protein * 1050),
	MaxFat is integer(Fat * 1050),
	MaxCarbs is integer(Carbs * 1050),
	Calories is integer(CAL),
	writeln(Calories),
	plan( % Plan/10 for 4 weeks
	    MinProtein, MinFat, MinCarbs,
	    MaxProtein, MaxFat, MaxCarbs,
	    Calories, MealsPerDay, Schedule,
	    0, 1, [], []).

calories(bulk, TEE, CAL):-
	CAL is TEE * 1.2.
calories(cut, TEE, CAL):-
	CAL is TEE * 0.8.
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

caloriesFat(F, C):-
	C is 9*F.
%caloriesCarb(Carb, Cal):-
%	Cal is 4*Carb.
caloriesProt(P, C) :-
	C is 4*P.

mass(KG, LB):-
	LB is 0.453592 * KG.

% Plan/10 for a new week
plan(
    MinProtein, MinFat, MinCarbs,
    MaxProtein, MaxFat, MaxCarbs,
    CAL, MPD, Schedule,
    Weeks, 0, WACC, SACC):-
	Weeks > 0,
	RemWeeks is Weeks-1,
	%%%%%%%%%%%%%%%%%%%noWeeksRep(WACC, SACC),
	plan( % Plan/10 for this week
	    MinProtein, MinFat, MinCarbs,
	    MaxProtein, MaxFat, MaxCarbs,
	    CAL, MPD, Schedule,
	    RemWeeks, 7, [], [WACC|SACC]).

% Plan/10 complete. Dump Accumulators
plan(
    _, _, _,
    _, _, _,
    _, _, [WACC|SACC],
    0, 0, WACC, SACC).

% Plan/10 for this week
plan(
    MinProtein, MinFat, MinCarbs,
    MaxProtein, MaxFat, MaxCarbs,
    CAL, MPD, Schedule,
    Weeks, Days, WACC, SACC):-
	Days > 0,
	% Plan for today
	CPM is integer((1.05*CAL)/MPD),
	MinProtein2 is integer(MinProtein/MPD),
	MaxProtein2 is integer(MaxProtein/MPD),
	MinFat2 is integer(MinFat/MPD),
	MaxFat2 is integer(MaxFat/MPD),
	MinCarbs2 is integer(MinCarbs/MPD),
	MaxCarbs2 is integer(MaxCarbs/MPD),
	today(
	    0, 0, 0, 0,
	    MinProtein2, MinFat2, MinCarbs2,
	    MaxProtein2, MaxFat2, MaxCarbs2,
	    CPM, MPD, [], Today),
	%%%%%%%%%%%%%%%%%%%%%thirdMealRule(Today, WACC),
	RD is Days - 1,
	% Continue Week
	plan( %Plan/10
	    MinProtein, MinFat, MinCarbs,
	    MaxProtein, MaxFat, MaxCarbs,
	    CAL, MPD, Schedule,
	    Weeks, RD, [Today|WACC], SACC).

% Today/10 plan complete. Check if it satisfies needs and dump
% accumulators.
today(
    Protein, Fat, Carbs, Cals,
    MinProtein, MinFat, MinCarbs,
    MaxProtein, MaxFat, MaxCarbs,
    CAL, 0, ACC, ACC):-
	extractComponents(ACC, Raw),
	noRepeatedMeals(Raw),
	obeyDailyLimit(Raw).

% Today/10 plan
today(
    Protein, Fat, Carbs, Cals,
    MinProtein, MinFat, MinCarbs,
    MaxProtein, MaxFat, MaxCarbs,
    CAL, Meals, ACC, RES):-
	Meals > 0,
	meal(
	    TP, TF, TC, TJ,
	    MinProtein, MinFat, MinCarbs,
	    MaxProtein, MaxFat, MaxCarbs,
	    CAL,
	    Meals, Components),
	NP #= Protein + TP,
	NF #= Fat + TF,
	NC #= Carbs + TC,
	NJ #= Cals + TJ,
	NM #= Meals-1,
	today(
	    NP, NF, NC, NJ,
	    MinProtein, MinFat, MinCarbs,
	    MaxProtein, MaxFat, MaxCarbs,
	    CAL, NM, [Components|ACC], RES).

% Meal/10 for right now
meal(P, F, C, J, MinP, MinF, MinC, MaxP, MaxF, MaxC, MaxJ, M, Components):-
	component(E1, P1, C1, F1, J1, _, H1, S1, B1, _),
	\+member(M, H1),
	component(E2, P2, C2, F2, J2, _, H2, S2, B2, _),
	\+member(M, H2),
	(   E2 = empty ; (E2 @< E1, E2 \= E1)),
	component(E3, P3, C3, F3, J3, _, H3, S3, B3, _),
	\+member(M, H3),
	(   E3 = empty ; (E3 @< E2, E3 \= E1, E3 \= E2)), 
	component(E4, P4, C4, F4, J4, _, H4, S4, B4, _),
	\+member(M, H4),
	(   E4 = empty ; (E4 @< E3, E4 \= E1, E4 \= E2, E4 \= E3)),
	M1 in S1..B1,
	M2 in S2..B2,
	M3 in S3..B3,
	M4 in S4..B4,
	P #= M1*P1 + M2*P2 + M3*P3 + M4*P4,
	F #= M1*F1 + M2*F2 + M3*F3 + M4*F4,
	C #= M1*C1 + M2*C2 + M3*C3 + M4*C4,
	J #= M1*J1 + M2*J2 + M3*J3 + M4*J4,
	P #>= MinP,	P #=< MaxP,
	F #>= MinF,	F #=< MaxF,
	C #>= MinC,	C #=< MaxC,
	J #=< MaxJ,
	label([P, F, C, J]),
	Components = [eat(E1, M1), eat(E2, M2), eat(E3, M3), eat(E4, M4)].

% Component(Name, 					Protein, Carbs, Fats, 	Cals, 	Pref in, 	Hate in, 	Min units, 	Max units, Limit)
component(empty,					0,		0,		0,		0,		[],			[],			1, 			1, 			2).
component(banana,					1100,	300,	23000,	89,		[],			[],			1, 			2, 			2).
component(broccoli,					2800,	7000,	400,	34,		[],			[],			1, 			2, 			2).
component(carrots,					900,	10000,	200,	41,		[],			[],			1, 			2, 			2).
component(cheddar_cheese,			25000,	1300,	33000,	402,	[1,2,5],	[],			1, 			1, 			2).
component(chicken_breast_uncooked,	21200,	0,		2500,	114,	[3],		[1],		1, 			2, 			2).
component(egg_whites_uncooked,		11000,	700,	200,	52,		[],			[],			1, 			2, 			2).
component(fish,						19000,	0,		6000,	134,	[3],		[1],		1, 			2, 			2).
component(gouda_cheese,				25000,	2200,	27000,	356,	[1,2,5],	[],			1, 			1, 			2).
component(honey,					300,	82000,	0,		304,	[],			[],			1, 			1, 			2).
component(lean_beef_uncooked,		20000,	0,		6000,	137,	[3],		[1],		1, 			2, 			2).
component(milk_full_fat,			3300,	4600,	3700,	64,		[],			[],			1, 			1, 			2).
component(multi_grain_bread,		13000,	43000,	4200,	265,	[],			[],			1, 			2, 			2).
component(olive_oil,				0,		0,		100000,	884,	[],			[],			1, 			1, 			2).
component(pasta_uncooked,			13000,	75000,	1500,	371,	[3],		[1],		1, 			1, 			2).
component(peanut_butter,			25000,	20000,	50000,	588,	[],			[],			1, 			1, 			2).
component(pear,						400,	15000,	100,	57,		[],			[],			1, 			2, 			2).
component(potato_uncooked,			2000,	17000,	100,	77,		[],			[],			1, 			2, 			2).
component(salmon_uncooked,			20000,	0,		13000,	208,	[3],		[1],		1, 			2, 			2).
component(shrimp_uncooked,			20000,	0,		500,	85,		[3],		[1],		1, 			2, 			2).
component(skimmed_milk,				3400,	5000,	100,	34,		[],			[],			1, 			2, 			2).
component(spinach,					2900,	3600,	400,	23,		[],			[1],		1, 			3, 			2).
component(sweet_potato_uncooked,	1600,	20000,	0,		86,		[],			[],			1, 			2, 			2).
component(swiss_cheese,				27000,	5000,	28000,	380,	[1,2,5],	[],			1, 			1, 			2).
component(tomato,					900,	3900,	200,	18,		[],			[],			1, 			4, 			4).
component(tuna_brine,				26000,	0,		1000,	116,	[],			[1],		1, 			2, 			2).
component(whey_protein,				80000,	10000,	3300,	400,	[],			[],			1, 			1, 			2).
component(white_rice_uncoooked,		7000,	82000,	600,	370,	[],			[],			1, 			1, 			2).
component(whole_egg_uncooked,		13000,	700,	10000,	143,	[1],		[],			1, 			3, 			3).


noWeeksRep(A, B):-
	\+member(A, B).

%thirdMealRule([_|[_|[ThirdMeal|_]]], Meals):-
%	thirdMealRule(ThirdMeal, Meals, N),
%	N < 3.
%thirdMealRule(_, [], 0).
%thirdMealRule(M, [H|T], N):-
%	sameMeal(M, H),
%	N1 is N+1,
%	thirdMealRule(M, T, N1).
%thirdMealRule(M, [H|T], N):-
%	\+sameMeal(M, H),
%	thirdMealRule(M, T, N).

%sameMeal(meal(X, A), meal(X, B)):-
%	true.

%Write Meal Comparator

extractComponents(Eat, Raw):-
	foldr(extractComponents, Eat, [], Raw).
extractComponents(Eat, T, [H|T]):-
	foldr(extractComponentNames, Eat, [], H).
extractComponentNames(eat(Name, _), T, [Name|T]).

noRepeatedMeals([]).
noRepeatedMeals([H|T]):-
	maplist(notSameMeal(H), T).
notSameMeal(A, B):-
	\+sameMeal(A, B).
sameMeal(A, B):-
	list_to_set(A, AS), list_to_set(B, BS),
	subset(AS, BS), subset(BS, AS).
obeyDailyLimit(Raw):-
	flatten(Raw, Flat),
	list_to_set(Flat, Set),
	maplist(obeyDailyLimit(Flat), Set).
obeyDailyLimit(List, Elem):-
	component(Elem, _, _, _, _, _, _, _, _, Limit),
	count(Elem, List, Count),
	Count =< Limit.

count(_, [], 0).
count(E, [E|T], N):-
	count(E, T, N1),
	N is N1 + 1.
count(E, [H|T], N):-
	H \= E,
	count(E, T, N).

foldr(Goal, List, V0, V):-
	reverse(List, RList),
	foldl(Goal, RList, V0, V).









