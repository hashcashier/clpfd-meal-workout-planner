:- use_module(library(clpfd)).
:- use_module(library(lists)).

exercise_plan_month(NumOfDaysPerWeek, Type, Plan) :-
  exercise_plan_week(NumOfDaysPerWeek, Type, MuscleDay1, Ex1),
  exercise_plan_week(NumOfDaysPerWeek, Type, MuscleDay2, Ex2),
  exercise_plan_week(NumOfDaysPerWeek, Type, MuscleDay3, Ex3),
  exercise_plan_week(NumOfDaysPerWeek, Type, MuscleDay4, Ex4),
  labeling([], MuscleDay1),
  labeling([], MuscleDay2),
  labeling([], MuscleDay3),
  labeling([], MuscleDay4),

  repetition_strategy(Strategies),
  nth0(0, Strategies, Rep1),
  nth0(1, Strategies, Rep2),
  nth0(2, Strategies, Rep3),
  nth0(3, Strategies, Rep4),

  format_exercises(Ex1, Rep1, Ex1Formated),
  format_exercises(Ex2, Rep2, Ex2Formated),
  format_exercises(Ex3, Rep3, Ex3Formated),
  format_exercises(Ex4, Rep4, Ex4Formated),

  format_week_plan(MuscleDay1, Ex1Formated, [chest, abs, bi, tri, shoulder, back, quads, hams, traps, calves], MuscleDay1Formated),
  format_week_plan(MuscleDay2, Ex2Formated, [chest, abs, bi, tri, shoulder, back, quads, hams, traps, calves], MuscleDay2Formated),
  format_week_plan(MuscleDay3, Ex3Formated, [chest, abs, bi, tri, shoulder, back, quads, hams, traps, calves], MuscleDay3Formated),
  format_week_plan(MuscleDay4, Ex4Formated, [chest, abs, bi, tri, shoulder, back, quads, hams, traps, calves], MuscleDay4Formated),

  Plan = [MuscleDay1Formated, MuscleDay2Formated, MuscleDay3Formated, MuscleDay4Formated]

  %pretty_print_plan(Plan)
  .


exercise_plan_week(NumOfDaysPerWeek, Type, MuscleDays, Exercises ) :-
  % Will try to assign each muscle a day
  MuscleDays = [ChestDay, AbsDay, BiDay, TriDay, ShoulderDay, BackDay, QuadsDay, HamsDay, TrapsDay, CalvesDay],
  MuscleDays ins 1 .. NumOfDaysPerWeek,

  % Muscles Constrains :
  HamsDay #= QuadsDay,
  (HamsDay #= BackDay ; HamsDay #= ChestDay),

  % Limit number of muslces per day:
  length(MuscleDays, NumOfMuscles),
  MaxMusclesPerDayTemp is ceil(NumOfMuscles / NumOfDaysPerWeek),
  max_list([MaxMusclesPerDayTemp, 3], MaxMusclesPerDay),
  cumulative([
    task(ChestDay,1,_,1,_),
    task(AbsDay,1,_,1,_),
    task(BiDay,1,_,1,_),
    task(TriDay,1,_,1,_),
    task(ShoulderDay,1,_,1,_),
    task(BackDay,1,_,1,_),
    task(QuadsDay,1,_,1,_),
    task(HamsDay,1,_,1,_),
    task(TrapsDay,1,_,1,_),
    task(CalvesDay,1,_,1,_)
  ], [limit(MaxMusclesPerDay)]),

  % Make sure that there's at least one exercise per day
  at_least_one_muscle_per_day(MuscleDays, 1, NumOfDaysPerWeek),

  % Get Exercises :
  exercise(chest, ChestExercises),
  exercise(abs, AbsExercises),
  exercise(bi, BiExercises),
  exercise(tri, TriExercises),
  exercise(shoulder, ShoulderExercises),
  exercise(back, BackExercises),
  exercise(quads, QuadsExercises),
  exercise(hams, HamsExercises),
  exercise(traps, TrapsExercises),
  exercise(calves, CalvesExercises),

  Exercises = [ ChestExercises, AbsExercises, BiExercises, TriExercises, ShoulderExercises, BackExercises,
                  QuadsExercises, HamsExercises, TrapsExercises, CalvesExercises ]
  .


at_least_one_muscle_per_day(MuscleDays, NumOfDaysPerWeek, NumOfDaysPerWeek) :-
  element(_, MuscleDays, NumOfDaysPerWeek).

at_least_one_muscle_per_day(MuscleDays, Day, NumOfDaysPerWeek) :-
  Day \= NumOfDaysPerWeek,
  element(_, MuscleDays, Day),
  NDay is Day + 1,
  at_least_one_muscle_per_day(MuscleDays, NDay, NumOfDaysPerWeek).

select_exercises(Rating5Exercises, AllExercises, Ret) :-
  random_subset(Rating5Exercises, 2, Subset),
  subtract(AllExercises, Subset, Remaining),
  random_subset(Remaining, 3, SelectedRemaining),
  append(Subset, SelectedRemaining, Ret).

random_subset(List, Length, Ret) :-
  random_permutation(List, RList),
  prefix(RList, Length, Ret).

prefix(_, 0, []).
prefix([H|T], X, [Z|T2]) :-
  Z = H,
  NewX is X -1,
  prefix(T, NewX, T2).

exercise(chest, ChestExercises) :- 
  Rating5 = [
      flat_bench_press,
      inclined_bench_press
  ],
  X = [
      flat_bench_press,
      inclined_bench_press,
      flat_dumbbell_press,
      incline_dumbbell_press,
      butterfly,
      incline_dumbbell_flys,
      low_cable_crossover,
      straight
  ],
  select_exercises(Rating5, X, ChestExercises)
  .

exercise(abs, AbsExercises) :- 
  Rating5 = [
      one-arm_high-pulley_cable_side_bends,
      cable_crunch
  ],
  X = [
      bent-knee_hip_raise,
      cable_crunch,
      cable_reverse_crunch,
      decline_reverse_crunch,
      decline_crunch,
      dumbbell_side_bend,
      elbow_to_knee,
      exercise_ball_crunch,
      jackknife_sit-up,
      knee/hip_raise_on_parallel_bars,
      one-arm_high-pulley_cable_side_bends
  ],
  select_exercises(Rating5, X, AbsExercises)
  .
exercise(bi, BiExercises) :- 
  Rating5 = [
      alternate_hammer_curl,
      barbell_curl,
      close-grip_ez_bar_curl
  ],
  X = [
      alternate_hammer_curl,
      alternate_incline_dumbbell_curl,
      barbell_curl,
      close-grip_ez_bar_curl,
      concentration_curls,
      dumbbell_alternate_bicep_curl,
      machine_preacher_curls,
      overhead_cable_curl,
      standing_one-arm_dumbbell_curl_over_incline_bench,
      wide-grip_standing_barbell_curl
  ],
  select_exercises(Rating5, X, BiExercises)
  .
exercise(tri, TriExercises) :- 
  Rating5 = [
      ez-bar_skullcrusher,
      triceps_pushdown
  ],
  X = [
      cable_one_arm_tricep_extension,
      cable_rope_overhead_triceps_extension,
      close-grip_barbell_bench_press,
      ez-bar_skullcrusher,
      incline_barbell_triceps_extension,
      reverse_grip_triceps_pushdown,
      seated_triceps_press,
      standing_low-pulley_one-arm_triceps_extension,
      triceps_pushdown,
      weighted_bench_dip
  ],
  select_exercises(Rating5, X, TriExercises)
  .
exercise(shoulder, ShoulderExercises) :- 
  Rating5 = [
      dumbbell_lying_one-arm_rear_lateral_raise,
      dumbbell_shoulder_press
  ],
  X = [
      alternating_deltoid_raise,
      barbell_shoulder_press,
      bent_over_dumbbell_rear_delt_raise_with_head_on_bench,
      dumbbell_lying_one-arm_rear_lateral_raise,
      dumbbell_shoulder_press,
      front_dumbbell_raise,
      machine_shoulder_military_press,
      seated_barbell_military_press,
      seated_side_lateral_raise,
      smith_machine_overhead_shoulder_press
  ],
  select_exercises(Rating5, X, ShoulderExercises)
  .
exercise(back, BackExercises) :- 
  Rating5 = [
      barbell_deadlift,
      bent_over_barbell_row,
      reverse_grip_bent-over_rows,
      wide-grip_lat_pulldown
  ],
  X = [
      barbell_deadlift,
      bent_over_barbell_row,
      close-grip_front_lat_pulldown,
      elevated_cable_rows,
      hyperextensions,
      one-arm_dumbbell_row,
      reverse_grip_bent-over_rows,
      v-bar_pulldown,
      wide-grip_lat_pulldown
  ],
  select_exercises(Rating5, X, BackExercises)
  .
exercise(quads, QuadsExercises) :- 
  Rating5 = [
      barbell_full_squat,
      front_barbell_squat,
      leg_press
  ],
  X = [
      barbell_full_squat,
      front_barbell_squat,
      hack_squat,
      leg_press,
      leg_extensions,
      narrow_stance_leg_press,
      smith_machine_squat,
      split_squat_with_dumbbells
  ],
  select_exercises(Rating5, X, QuadsExercises)
  .
exercise(hams, HamsExercises) :- 
  Rating5 = [
      lying_leg_curls,
      romanian_deadlift,
      seated_leg_curl
  ],
  X = [
      floor_glute-ham_raise,
      good_morning,
      lying_leg_curls,
      romanian_deadlift,
      seated_leg_curl,
      stiff-legged_dumbbell_deadlift
  ],
  select_exercises(Rating5, X, HamsExercises)
  .
exercise(traps, TrapsExercises) :- 
  Rating5 = [
      barbell_shrug_behind_the_back,
      barbell_shrug,
      dumbbell_shrug,
      smith_machine_shrug,
      cable_shrugs
  ],
  X = [
      barbell_shrug_behind_the_back,
      barbell_shrug,
      dumbbell_shrug,
      smith_machine_shrug,
      cable_shrugs
  ],
  select_exercises(Rating5, X, TrapsExercises)
  .
exercise(calves, CalvesExercises) :- 
  Rating5 = [
      barbell_seated_calf_raise,
      calf_press_on_the_leg_press_machine,
      seated_calf_raise,
      smith_machine_calf_raise,
      standing_dumbbell_calf_raise
  ],
  X = [
      barbell_seated_calf_raise,
      calf_press_on_the_leg_press_machine,
      seated_calf_raise,
      smith_machine_calf_raise,
      standing_dumbbell_calf_raise
  ],
  select_exercises(Rating5, X, CalvesExercises)
  .

repetition_strategy(Strategy) :-
  X = [ [20,20,20,20,20], [15,12,10,8,6], [12,10,8,6,4], [12,12,10,8,8], [5,5,5,5,5]],
  random_permutation(X, X2),
  prefix(X2, 4, Strategy).


format_exercises([], _, []).
format_exercises( [EHead|ETail], Rep, [H|T]) :-
  format_single_exercise(EHead, Rep, H),
  format_exercises(ETail, Rep, T).


format_single_exercise(MuscleExercise, Rep, Plan) :-
  format_single_exercise_set(MuscleExercise, Rep, P5),
  prefix(P5, 4, P4),
  prefix(P5, 3, P3),
  Plan = [ times(5, P5), times(4, P4), times(3, P3), times(3, P3), times(3,P3) ].

format_single_exercise_set([],[],[]).
format_single_exercise_set([H1|T1], [H2|T2], [HR1|TR2]):-
  HR1 = workout_rep(H1, H2),
  format_single_exercise_set(T1, T2, TR2).


format_week_plan_help([],[],[],[]).
format_week_plan_help([HM|TM], [HE|TE], [HN|TN], [HR|TR]) :-
  HR = day(HM, HN, HE),
  format_week_plan_help(TM, TE, TN, TR).


format_week_plan(Muscles, Exercises, Names, Res) :-
  format_week_plan_help(Muscles, Exercises, Names, Res1),
  sort(Res1, Res).


pretty_print_plan([W1,W2,W3,W4]) :-
  pretty_print_week(1,W1),
  pretty_print_week(2,W2),
  pretty_print_week(3,W3),
  pretty_print_week(4,W4)
  .


pretty_print_week_help([]).
pretty_print_week_help([H|T]) :-
  pretty_print_day(H),
  pretty_print_week_help(T).
pretty_print_week(WeekNum, WeekPlan) :-
  writef("Week "),
  print(WeekNum),
  writef("\n"),
  pretty_print_week_help(WeekPlan)
  .

pretty_print_day(day(DayNum, Muscle, Exercises)) :-
  writef("  Day "),
  print(DayNum),
  writef(", Muscle "),
  print(Muscle),
  writef(" \n"),
  pretty_print_exercise(Exercises).

pretty_print_exercise([
times(_, [
  workout_rep(Ex11, R11), 
  workout_rep(Ex12, R12), 
  workout_rep(Ex13, R13), 
  workout_rep(Ex14, R14), 
  workout_rep(Ex15, R15)]),
times(_, [
  workout_rep(Ex21, R21), 
  workout_rep(Ex22, R22), 
  workout_rep(Ex23, R23), 
  workout_rep(Ex24, R24)]),
times(_, [
  workout_rep(Ex31, R31), 
  workout_rep(Ex32, R32), 
  workout_rep(Ex33, R33)]),
times(_, [
  workout_rep(Ex41, R41), 
  workout_rep(Ex42, R42), 
  workout_rep(Ex43, R43)]),
times(_, [
  workout_rep(Ex51, R51), 
  workout_rep(Ex52, R52), 
  workout_rep(Ex53, R53)])]) :-

    writef("    5 Sets of :\n"),
    writef("      "), print(Ex11), writef(" x "), print(R11), writef("\n"),
    writef("      "), print(Ex12), writef(" x "), print(R12), writef("\n"),
    writef("      "), print(Ex13), writef(" x "), print(R13), writef("\n"),
    writef("      "), print(Ex14), writef(" x "), print(R14), writef("\n"),
    writef("      "), print(Ex15), writef(" x "), print(R15), writef("\n"),

    writef("    4 Sets of :\n"),
    writef("      "), print(Ex21), writef(" x "), print(R21), writef("\n"),
    writef("      "), print(Ex22), writef(" x "), print(R22), writef("\n"),
    writef("      "), print(Ex23), writef(" x "), print(R23), writef("\n"),
    writef("      "), print(Ex24), writef(" x "), print(R24), writef("\n"),


    writef("    3 Sets of :\n"),
    writef("      "), print(Ex31), writef(" x "), print(R31), writef("\n"),
    writef("      "), print(Ex32), writef(" x "), print(R32), writef("\n"),
    writef("      "), print(Ex33), writef(" x "), print(R33), writef("\n"),


    writef("    3 Sets of :\n"),
    writef("      "), print(Ex41), writef(" x "), print(R41), writef("\n"),
    writef("      "), print(Ex42), writef(" x "), print(R42), writef("\n"),
    writef("      "), print(Ex43), writef(" x "), print(R43), writef("\n"),


    writef("    3 Sets of :\n"),
    writef("      "), print(Ex51), writef(" x "), print(R51), writef("\n"),
    writef("      "), print(Ex52), writef(" x "), print(R52), writef("\n"),
    writef("      "), print(Ex53), writef(" x "), print(R53), writef("\n")
    .
