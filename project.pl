:- use_module(library(clpfd)).
:- use_module(library(lists)).

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
	today(
	    0, 0, 0, 0,
	    MinProtein, MinFat, MinCarbs,
	    MaxProtein, MaxFat, MaxCarbs,
	    CAL, MPD, [], Today),
	%%%%%%%%%%%%%%%%%%%%%thirdMealRule(Today, WACC),
	RD is Days - 1,
	% Continue Week
	plan( %Plan/10
	    MinProtein, MinFat, MinCarbs,
	    MaxProtein, MaxFat, MaxCarbs,
	    CAL, MPD, Schedule,
	    Weeks, RD, [Today|WACC], SACC).

% Today/10 plan complete. Check if it satisfies needs.
today(
    Protein, Fat, Carbs, Cals,
    MinProtein, MinFat, MinCarbs,
    MaxProtein, MaxFat, MaxCarbs,
    CAL, 0, ACC, ACC):-
	Protein #>= MinProtein,
	Protein #=< MaxProtein,
	Fat #>= MinFat,
	Fat #=< MaxFat,
	Carbs #>= MinCarbs,
	Carbs #=< MaxCarbs,
	MAXCAL is integer(CAL * 1.05),
	Cals #=< MAXCAL,
	label([Protein, Fat, Carbs, Cals]).

% Today/10 plan
today(
    Protein, Fat, Carbs, Cals,
    MinProtein, MinFat, MinCarbs,
    MaxProtein, MaxFat, MaxCarbs,
    CAL, Meals, ACC, RES):-
	Meals > 0,
	meal(TP, TF, TC, TJ, Meals, Components),
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
meal(P, F, C, J, M, Components):-
	component(E1, P1, C1, F1, J1, _, H1, S1, B1),
	%\+member(M, H1),
	component(E2, P2, C2, F2, J2, _, H2, S2, B2),
	%\+member(M, H2),
	(   E2 = empty ; (E2 \= E1)),
	component(E3, P3, C3, F3, J3, _, H3, S3, B3),
	%\+member(M, H3),
	(   E3 = empty ; (E3 \= E1, E3 \= E2)),
	component(E4, P4, C4, F4, J4, _, H4, S4, B4),
	%\+member(M, H4),
	(   E4 = empty ; (E4 \= E1, E4 \= E2, E4 \= E3)),
	M1 in S1..B1,
	M2 in S2..B2,
	M3 in S3..B3,
	M4 in S4..B4,
	P #= M1*P1 + M2*P2 + M3*P3 + M4*P4,
	C #= M1*C1 + M2*C2 + M3*C3 + M4*C4,
	F #= M1*F1 + M2*F2 + M3*F3 + M4*F4,
	J #= M1*J1 + M2*J2 + M3*J3 + M4*J4,
	%P #=< MP, F #=< MF, C #=< MC, J #=< MJ,
	Components = [eat(E1, M1), eat(E2, M2), eat(E3, M3), eat(E4, M4)].

% Component(Name, Protein, Carbs, Fats, Calories, Pref in, Hate in, Min
% units, max units)

component(empty,			0,	0,	0,	0,	[],		[],	1, 1).
component(banana,			1100,	300,	23000,	89,	[],		[],	1, 2).
component(broccoli,			2800,	7000,	400,	34,	[],		[],	1, 2).
component(carrots,			900,	10000,	200,	41,	[],		[],	1, 2).
component(cheddar_cheese,		25000,	1300,	33000,	402,	[1,2,5],	[],	1, 1).
component(chicken_breast_uncooked,	21200,	0,	2500,	114,	[3],		[1],	1, 2).
component(egg_whites_uncooked,		11000,	700,	200,	52,	[],		[],	1, 2).
component(fish,				19000,	0,	6000,	134,	[3],		[1],	1, 2).
component(gouda_cheese,			25000,	2200,	27000,	356,	[1,2,5],	[],	1, 1).
component(honey,			300,	82000,	0,	304,	[],		[],	1, 1).
component(lean_beef_uncooked,		20000,	0,	6000,	137,	[3],		[1],	1, 2).
component(milk_full_fat,		3300,	4600,	3700,	64,	[],		[],	1, 1).
component(multi_grain_bread,		13000,	43000,	4200,	265,	[],		[],	1, 2).
component(olive_oil,			0,	0,	100000,	884,	[],		[],	1, 1).
component(pasta_uncooked,		13000,	75000,	1500,	371,	[3],		[1],	1, 1).
component(peanut_butter,		25000,	20000,	50000,	588,	[],		[],	1, 1).
component(pear,				400,	15000,	100,	57,	[],		[],	1, 2).
component(potato_uncooked,		2000,	17000,	100,	77,	[],		[],	1, 2).
component(salmon_uncooked,		20000,	0,	13000,	208,	[3],		[1],	1, 2).
component(shrimp_uncooked,		20000,	0,	500,	85,	[3],		[1],	1, 2).
component(skimmed_milk,			3400,	5000,	100,	34,	[],		[],	1, 2).
component(spinach,			2900,	3600,	400,	23,	[],		[1],	1, 3).
component(sweet_potato_uncooked,	1600,	20000,	0,	86,	[],		[],	1, 2).
component(swiss_cheese,			27000,	5000,	28000,	380,	[1,2,5],	[],	1, 1).
component(tomato,			900,	3900,	200,	18,	[],		[],	1, 4).
component(tuna_brine,			26000,	0,	1000,	116,	[],		[1],	1, 2).
component(whey_protein,			80000,	10000,	3300,	400,	[],		[],	1, 1).
component(white_rice_uncoooked,		7000,	82000,	600,	370,	[],		[],	1, 1).
component(whole_egg_uncooked,		13000,	700,	10000,	143,	[1],		[],	1, 3).


noWeeksRep(A, B):-
	\+member(A, B).

thirdMealRule([_|[_|[ThirdMeal|_]]], Meals):-
	thirdMealRule(ThirdMeal, Meals, N),
	N < 3.
thirdMealRule(_, [], 0).
thirdMealRule(M, [H|T], N):-
	sameMeal(M, H),
	N1 is N+1,
	thirdMealRule(M, T, N1).
thirdMealRule(M, [H|T], N):-
	\+sameMeal(M, H),
	thirdMealRule(M, T, N).

sameMeal(meal(X, A), meal(X, B)):-
	true.
%Write Meal Comparator
%Change GRAMS to Milligrams and use CLPFD









