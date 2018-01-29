(* ::Package:: *)

BeginPackage["ProgressMap`"]

ProgressParallelMap::usage =
 "ParallelMap implementation with progress bar. levelspec is always {1}.
  For more details use \"ShowInfo\"\[Rule]True";
 
ProgressMap::usage =
 "Map implementation with progress bar. levelspec is always {1}.
  For more details use \"ShowInfo\"\[Rule]True";

Begin["`Private`"]

(* Reloj *)
GetSeconds[time_] := IntegerString[Round[Mod[time,60]], 10, 2];
GetMinutes[time_]:= IntegerString[Mod[Floor[time/60],60], 10, 2];
GetHours[time_] := IntegerString[Floor[time/3600], 10, 2];
ClockFormat[time_] :=StringJoin[GetHours[time], ":", GetMinutes[time], ":", GetSeconds[time]];

(* Mapeos con progreso *)
DefaultIndicator[indexProgress_, totalSize_] := ProgressIndicator[indexProgress, {1, totalSize}];

DetailedIndicator[indexProgress_, totalSize_, startTime_]:=
Module[{progressString, remainingTime, remainingTimeString, indicator, ellapsedTimeString},
	progressString = StringJoin["Progress ", ToString[indexProgress], "/", ToString[totalSize]];
	ellapsedTimeString = StringJoin["Ellapsed time: ", ClockFormat[AbsoluteTime[] - startTime]];

	If[indexProgress != 0,
		remainingTime = ((AbsoluteTime[]-startTime) / indexProgress)*(totalSize-indexProgress);
		remainingTimeString = StringJoin["Remaining: ", ClockFormat[remainingTime]];
	,
		remainingTimeString = "Remaining: Unknown";
	];

	indicator = Column[
	{
		DefaultIndicator[indexProgress,totalSize],
		progressString,
		ellapsedTimeString,
		remainingTimeString
	}
	];

	Return[indicator];
];

ProgressFunction[f_, arg_, index_] := Module[{output},
	output = f[arg];
	AppendTo[indexProgress, index];
	Return[output];
];

ParallelMapIndexed[f_, expr_, opts: OptionsPattern[]] :=
Parallelize[
	MapIndexed[ProgressFunction[f, #1, #2]&, expr],
	FilterRules[{opts}, Options[Parallelize]]
];

SetSharedVariable[indexProgress];
ProgressParallelMap[f_, expr_, opts: OptionsPattern[{"ShowInfo"->False, Parallelize}]] :=
Module[{startTime},
	indexProgress = {0};
	startTime = AbsoluteTime[];

	Monitor[
		ParallelMapIndexed[f, expr, opts]
		,
		If[OptionValue["ShowInfo"],
			DetailedIndicator[Max[indexProgress], Length[expr], startTime]
			,
			DefaultIndicator[Max[indexProgress], Length[expr]]
		]
	]
]; 

ProgressMap[f_, expr_, OptionsPattern[{"ShowInfo"->False}]] :=
Module[{startTime},
	indexProgress = {0};
	startTime = AbsoluteTime[];

	Monitor[
			MapIndexed[ProgressFunction[f, #1, #2]&, expr],

			If[OptionValue["ShowInfo"],
				DetailedIndicator[Max[indexProgress], Length[expr], startTime]
			,
				DefaultIndicator[Max[indexProgress], Length[expr]]
			]
	]
];

End[ ]

EndPackage[ ]
