% Component(UNIQUE Name, 			Protein, Carbs, Fats, 	Cals, 	Min U/S, 	Max U/S, 	Min U/D,	Max U/D,	Min S/D,	Max S/D)
component(banana,					1100,	300,	23000,	89,		1, 			2, 			0,			2, 			0,			1).
component(broccoli,					2800,	7000,	400,	34,		1, 			2, 			0,			2, 			0,			1).
component(carrots,					900,	10000,	200,	41,		1, 			2, 			0,			2, 			0,			1).
component(cheddar_cheese,			25000,	1300,	33000,	402,	1, 			2, 			0,			2, 			0,			1).
component(chicken_breast_uncooked,	21200,	0,		2500,	114,	1, 			4, 			0,			4, 			0,			1).
component(egg_whites_uncooked,		11000,	700,	200,	52,		1, 			3, 			0,			3, 			0,			1).
component(fish,						19000,	0,		6000,	134,	1, 			4, 			0,			4, 			0,			1).
component(gouda_cheese,				25000,	2200,	27000,	356,	1, 			3, 			0,			3, 			0,			1).
component(honey,					300,	82000,	0,		304,	1, 			1, 			0,			1, 			0,			1).
component(lean_beef_uncooked,		20000,	0,		6000,	137,	1, 			4, 			0,			4, 			0,			1).
component(milk_full_fat,			3300,	4600,	3700,	64,		1, 			2, 			0,			2, 			0,			1).
component(multi_grain_bread,		13000,	43000,	4200,	265,	1, 			2, 			0,			2, 			0,			1).
component(olive_oil,				0,		0,		100000,	884,	1, 			1, 			0,			1, 			0,			1).
component(pasta_uncooked,			13000,	75000,	1500,	371,	1, 			2, 			0,			2, 			0,			1).
component(peanut_butter,			25000,	20000,	50000,	588,	1, 			2, 			0,			2, 			0,			1).
component(pear,						400,	15000,	100,	57,		1, 			2, 			0,			2, 			0,			1).
component(potato_uncooked,			2000,	17000,	100,	77,		1, 			2, 			0,			2, 			0,			1).
component(salmon_uncooked,			20000,	0,		13000,	208,	1, 			2, 			0,			2, 			0,			1).
component(shrimp_uncooked,			20000,	0,		500,	85,		1, 			2, 			0,			2, 			0,			1).
component(skimmed_milk,				3400,	5000,	100,	34,		1, 			2, 			0,			2, 			0,			1).
component(spinach,					2900,	3600,	400,	23,		1, 			3, 			0,			3, 			0,			1).
component(sweet_potato_uncooked,	1600,	20000,	0,		86,		1, 			2, 			0,			2, 			0,			1).
component(swiss_cheese,				27000,	5000,	28000,	380,	1, 			2, 			0,			2, 			0,			1).
component(tomato,					900,	3900,	200,	18,		1, 			2, 			0,			3, 			0,			2).
component(tuna_brine,				26000,	0,		1000,	116,	1, 			1, 			0,			2, 			0,			2).
component(whey_protein,				80000,	10000,	3300,	400,	1, 			1, 			0,			1, 			0,			1).
component(white_rice_uncoooked,		7000,	82000,	600,	370,	1, 			1, 			0,			1, 			0,			1).
component(whole_egg_uncooked,		13000,	700,	10000,	143,	1, 			3, 			0,			3, 			0,			1).

% Combination([Components], 											[Meals Preferred In], 		[Meals Hated In],		Rule Is Applicable When MPD Value Is)
combination([banana], 													[],							[],						_).

combination([broccoli], 												[],							[1],					_).

combination([carrots], 													[],							[],						_).

combination([cheddar_cheese], 											[],							[],						_).

combination([chicken_breast_uncooked], 									[2], 						[1],					3).
combination([chicken_breast_uncooked], 									[3], 						[1],					4).
combination([chicken_breast_uncooked], 									[3], 						[1],					5).

combination([egg_whites_uncooked], 										[1,3], 						[2],					3).
combination([egg_whites_uncooked], 										[1,4], 						[3],					4).
combination([egg_whites_uncooked], 										[1,4,5],					[3],					5).

combination([fish], 													[2],						[1,3],					3).
combination([fish], 													[3],						[1,4],					4).
combination([fish], 													[3],						[1,5],					5).

combination([gouda_cheese], 											[],							[],						_).

combination([honey], 													[],							[],						_).

combination([lean_beef_uncooked], 										[2],						[1],					3).
combination([lean_beef_uncooked], 										[3],						[1],					4).
combination([lean_beef_uncooked], 										[3],						[1],					5).

combination([milk_full_fat], 											[],							[],						_).

combination([multi_grain_bread], 										[],							[],						_).

combination([olive_oil], 												[],							[],						_).

combination([pasta_uncooked], 											[2],						[1,3],					3).
combination([pasta_uncooked], 											[3],						[1,4],					4).
combination([pasta_uncooked], 											[3],						[1,5],					5).

combination([peanut_butter], 											[],							[],						_).

combination([pear], 													[],							[],						_).

combination([potato_uncooked], 											[],							[],						_).

combination([salmon_uncooked], 											[1,2],						[],						3).
combination([salmon_uncooked], 											[1,3],						[],						4).
combination([salmon_uncooked], 											[1,3],						[],						5).

combination([shrimp_uncooked], 											[2],						[1,3],					3).
combination([shrimp_uncooked], 											[3],						[1,4],					4).
combination([shrimp_uncooked], 											[3],						[1,5],					5).

combination([skimmed_milk], 											[],							[],						_).

combination([spinach], 													[],							[1,3],					3).
combination([spinach], 													[],							[1,2,4],				4).
combination([spinach], 													[],							[1,2,4,5],				5).

combination([sweet_potato_uncooked], 									[],							[1],					_).

combination([swiss_cheese], 											[1],						[],						3).
combination([swiss_cheese], 											[1,2],						[],						4).
combination([swiss_cheese], 											[1,2,5],					[],						5).

combination([tomato], 													[], 						[1],					_).

combination([tuna_brine], 												[], 						[1],					_).

combination([whey_protein], 											[2], 						[],						4).
combination([whey_protein], 											[2,4], 						[],						5).

combination([white_rice_uncoooked], 									[2], 						[1,3],					3).
combination([white_rice_uncoooked], 									[3], 						[1,2,4],				4).
combination([white_rice_uncoooked], 									[3], 						[1,2,4,5],				5).

combination([whole_egg_uncooked], 										[1], 						[2],					3).
combination([whole_egg_uncooked], 										[1], 						[3],					4).
combination([whole_egg_uncooked], 										[1], 						[3],					5).

combination([tuna_brine, multi_grain_bread], 							[2], 						[],						4).
combination([tuna_brine, multi_grain_bread], 							[2, 4],						[],						5).

combination([pasta_uncooked, lean_beef_uncooked], 						[2], 						[1],					3).
combination([pasta_uncooked, lean_beef_uncooked], 						[3], 						[1],					4).
combination([pasta_uncooked, lean_beef_uncooked], 						[3], 						[1],					5).

combination([potato_uncooked, salmon_uncooked], 						[2], 						[1],					3).
combination([potato_uncooked, salmon_uncooked], 						[3], 						[1],					4).
combination([potato_uncooked, salmon_uncooked], 						[3], 						[1],					5).

combination([whole_egg_uncooked, gouda_cheese, multi_grain_bread], 		[1,3], 						[2],					3).
combination([whole_egg_uncooked, gouda_cheese, multi_grain_bread], 		[1,2,4], 					[3],					4).
combination([whole_egg_uncooked, gouda_cheese, multi_grain_bread], 		[1,2,5], 					[3],					5).

combination([lean_beef_uncooked, pasta_uncooked, carrots], 				[2], 						[1],					3).
combination([lean_beef_uncooked, pasta_uncooked, carrots], 				[3], 						[1],					4).
combination([lean_beef_uncooked, pasta_uncooked, carrots], 				[3], 						[1],					5).

combination([lean_beef_uncooked, pasta_uncooked, broccoli], 			[2], 						[1],					3).
combination([lean_beef_uncooked, pasta_uncooked, broccoli], 			[3], 						[1],					4).
combination([lean_beef_uncooked, pasta_uncooked, broccoli], 			[3], 						[1],					5).

combination([fish, white_rice_uncoooked], 								[2], 						[1],					3).
combination([fish, white_rice_uncoooked], 								[3], 						[1],					4).
combination([fish, white_rice_uncoooked], 								[3], 						[1],					5).

combination([lean_beef_uncooked, 
			salmon_uncooked, 
			shrimp_uncooked,
			chicken_breast_uncooked], 									[], 						[1,2,3,4,5],			_).