:- use_module(library(clpfd)).
:- use_module(library(lists)).

plan(Goal, MealsPerDay, Weight, FatPerc, ActivityVariable, Schedule):-
	MealsPerDay > 2,
	MealsPerDay < 6,
	LBM #= Weight*(100 - FatPerc) / 100,
	BMR #= 370 + 21.6 * LBM,
	TEE #= ActivityVariable * BMR,
	calories(Goal, TEE, CAL),
	protein(LBM, Protein),
	fat(LBM, Fat),
	carbs(CAL, Protein, Fat, Carbs),
	plan(Protein, Fat, Carbs, MealsPerDay, Schedule, 4, 7, [], []).
	%uniqueSchedule(Schedule).

plan(Protein, Fat, Carbs, MPD, Schedule, Weeks, 0, WACC, SACC):-
	Weeks > 0,
	RemWeeks is Weeks-1,
	noWeeksRep(WACC, SACC),
	plan(Protein, Fat, Carbs, MPD, Schedule, RemWeeks, 7, [], [WACC|SACC]).

plan(_, _, _, _, Schedule, 0, 0, _, Schedule).
plan(Protein, Fat, Carbs, MPD, Schedule, Weeks, Days, WACC, SACC):-
	Days > 0,
	Weeks > 0,
	plan(Protein, Fat, Carbs, MPD, Today),
	thirdMealRule(Today, WACC),
	RD is Days - 1,
	plan(Protein, Fat, Carbs, MPD, Schedule, Weeks, RD, [Today|WACC], SACC).

plan(_, _, _, 0, []).
plan(P, F, C, M, [Components|T]):-
	M > 0,
	M1 is M-1,
	plan(P, F, C, M1, T),
	meal(M, Components),
	total(TP, TF, TC, [Components|T]),
	TP - P #> P * 0.05,
	TF - F #> F * 0.05,
	TC - C #> C * 0.05.


total(0, 0, 0, []).
total(P, F, C, [H|T]):-
	component(H, P1, F1, C1, _, _, _),
	total(P2, F2, C2, T),
	P #= P1 + P2,
	F #= F1 + F2,
	C #= C1 + C2.

component(banana,1.1, 0.3, 23, 89, [], []).

caloriesFat(F, C):-
	C #= 9*F.
%caloriesCarb(Carb, Cal):-
%	Cal #= 4*Carb.
caloriesProt(P, C) :-
	C #= 4*P.
fat(LBM, F):-
	mass(LBM, LB),
	F #= 0.4 * LB.
protein(LBM, P):-
	mass(LBM, LB),
	P #= 1.5 * LB.

carbs(CAL, P, F, C):-
	caloriesFat(F, FC),
	caloriesProt(P, PC),
	C #= (CAL - FC - PC) / 4.

mass(KG, LB):-
	LB #= 0.453592 * KG.

calories(bulk, TEE, CAL):-
	CAL #= TEE * 1.2.
calories(cut, TEE, CAL):-
	CAL #= TEE * 0.8.

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










