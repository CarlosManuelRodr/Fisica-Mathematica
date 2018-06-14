(* ::Package:: *)

(* ::Title:: *)
(*Economic computations*)


(* ::Chapter:: *)
(*Begin package*)


BeginPackage["EconomicComputations`"]


(* ::Text:: *)
(*Returns definitions*)


Returns::usage = "Calculate log returns";
UncorrelatedReturns::usage = "Calculate uncorrelated log returns (delete returns with repeated prices)";
TrendDuration::usage = "Calculate the duration of each elemental trend";
TrendReturns::usage = "Calculate log returns using the elemental trends as start and end points";
DatedTrendReturns::usage = "TrendReturns with date info";
VelocityTrendReturns::usage = "Calculate the velocity of change of each elemental trend";
DatedVelocityTrendReturns::usage = "VelocityTrendReturns with date info";


(* ::Text:: *)
(*Power law distribution selection*)


PowerLawAndersonDarling::usage = "Calculate the value of the Anderson-Darling test assuming a power law distribution";
LeftCutoff::usage = "Calculate the cutoff point where the distribution starts behaving power law-like";
RightCutoff::usage = "Calculate the cutoff point where the distribution stops behaving power law-like. Points after this are considered extreme events.";
CalculateCutoff::usage = "Automatically calculate the left cutoff point";
SelectGaussianReturns::usage = "Return the gaussian part of a return distribution, removing the power law-like points.";


(* ::Text:: *)
(*Extreme events analysis*)


ExtremeEventThreshold::usage = "Calculate the return value threshold where extreme events begin";
GetExtremeEventDates::usage = "Get the dates where extreme events happened";
EventProbability::usage = "Calculate event probability";


(* ::Text:: *)
(*Symmetry analysis*)


UpperPercentagePoint::usage = "Calculate the upper percentage point in the Tn symmetry statistic";
Tn::usage = "Calculate Tn";
MeasureTn::usage = "Apply Tn symmetry test";
MeasureHeightRatio::usage = "Measure height ratio of a bimodal distribution";


(* ::Text:: *)
(*Database building*)


DatasetBuilderDialog::usage = "Launch dialog to create the database";


(* ::Text:: *)
(*Trend duration analysis*)


UpwardTrendDuration::usage = "Count the duration of positive elemental trends";
DownwardTrendDuration::usage = "Count the duration of negative elemental trends";


(* ::Text:: *)
(*Kullback-Leibler*)


KullbackLeibler::usage = "Calculate the Kullback-Leibler divergence on data assuming the distribution dist";
KullbackLeiblerInTime::usage = 
"Calculate the time evolution of Kullback-Leibler divergence 
on data assuming the distribution dist and using the window specified.";


(* ::Chapter:: *)
(*Definitions*)


Begin["`Private`"]


(* ::Section::Closed:: *)
(*Initialization*)


Needs["AdvancedMapping`"]


(* ::Section::Closed:: *)
(*Returns*)


Returns[prices_, lag_:1]:= Log[Drop[prices,lag]]-Log[Drop[prices,-lag]];
Returns[dates_, prices_, lag_]:= Transpose[{Drop[dates,-1],Log[Drop[prices,lag]]-Log[Drop[prices,-lag]]}];

UncorrelatedReturns[prices_, lag_]:= Take[Returns[prices, lag],{1, -1, lag+1}];
SimpleReturns[prices_, lag_:1]:= Drop[prices, lag]-Drop[prices, -lag];
TrendDuration[prices_] := Map[Length, Split[Sign[Returns[prices]]]];

If[$VersionNumber<11.2,
	TakeList[list_,seqs_] := FoldPairList[TakeDrop,list,seqs];
];

TrendReturns[prices_] := Block[{endpoints},
	endpoints = Map[First, TakeList[prices, Append[TrendDuration[prices], All]]];
	Return[Returns[endpoints]];
];

DatedTrendReturns[datedprices_] := Block[{endpoints},
	endpoints = Map[First, TakeList[datedprices, Append[TrendDuration[datedprices[[All, 2]]], All]]];
	Return[Transpose[{Drop[endpoints[[All, 1]], -1], Returns[endpoints[[All, 2]]]}]];
];

VelocityTrendReturns[prices_] := TrendReturns[prices] / TrendDuration[prices];

DatedVelocityTrendReturns[datedprices_] := Block[{tr},
	tr = DatedTrendReturns[datedprices];
	tr[[All, 2]] = tr[[All, 2]] / TrendDuration[datedprices[[All, 2]]];
	Return[tr];
];


(* ::Section::Closed:: *)
(*Power law cutoff*)


PowerLawAndersonDarling[data_, \[Gamma]_]:= Module[{s, n,\[Alpha],z,A2,i},
	s = Sort[data];
	n = Count[s, x_ /; x>=\[Gamma]];
	If[n>1,
		s = Take[s,-n];
		\[Alpha] = ((1/n)*Sum[Log[s[[i]]/\[Gamma]], {i,1,n}])^-1;
		z = Table[1-(\[Gamma]/s[[i]])^\[Alpha], {i,1,n}];
		A2 = -n - (1/n)*Sum[(2i-1)*(Log[z[[i]]]+Log[1-z[[n-i+1]]]), {i, 1, n}]; 
		Return[A2];
		,
		Return[\[Infinity]];
	]
];

LeftCutoff[data_, \[Gamma]min_, \[Gamma]max_,d\[Gamma]_]:=Module[{tbl,minPos,\[Gamma]},
	tbl = Table[{\[Gamma], PowerLawAndersonDarling[data, \[Gamma]]}, {\[Gamma], \[Gamma]min, \[Gamma]max, d\[Gamma]}];
	tbl = DeleteCases[tbl, {_, \[Infinity]}];
	MinimalBy[tbl, Last][[1,1]]
];

RightCutoff[data_,\[Gamma]left_,\[Gamma]max_,d\[Gamma]_]:=Module[{tbl,maxPos,\[Gamma]},
	tbl = Table[{\[Gamma], PowerLawAndersonDarling[data,\[Gamma]]},{\[Gamma], \[Gamma]max, \[Gamma]left, -d\[Gamma]}];
	tbl = DeleteCases[tbl, {_,\[Infinity]}];
	MaximalBy[tbl, Last][[1,1]]
];

CalculateCutoff[absreturns_] := Block[{\[CapitalDelta]},
	\[CapitalDelta] = (Max[absreturns] - Quantile[absreturns, 0.5])/1000;
	LeftCutoff[absreturns, Quantile[absreturns, 0.5], Max[absreturns], \[CapitalDelta]]
];

SelectGaussianReturns[returns_] := Block[{pos,neg,\[Gamma]pos,\[Gamma]neg},
	pos = Select[returns, Positive];
	neg = -Select[returns, Negative];
	\[Gamma]pos = CalculateCutoff[pos];
	\[Gamma]neg = CalculateCutoff[neg];
	pos = Select[pos, # < \[Gamma]pos&];
	neg = Select[neg, # < \[Gamma]neg&];
	Join[pos,-neg]
];


(* ::Section::Closed:: *)
(*Extreme events*)


ExtremeEventThreshold[returns_] := Block[{\[CapitalDelta],\[Gamma]left, \[Gamma]right},
	\[CapitalDelta] = (Max[returns] - Quantile[returns, 0.5])/1000;
	\[Gamma]left = LeftCutoff[returns, 0.02, Max[returns], \[CapitalDelta]];
	\[Gamma]right= RightCutoff[returns, \[Gamma]left, Max[returns], \[CapitalDelta]];
	Return[\[Gamma]right];
];

GetExtremeEventDates[datedReturns_] := Block[{pos, neg, \[Gamma], posDates, negDates},
	pos = Select[datedReturns, Last[Positive[#]]&];
	\[Gamma] = ExtremeEventThreshold[pos[[All,2]]];
	posDates = Select[datedReturns, Last[#]>\[Gamma]&];

	neg = Select[datedReturns, Last[Negative[#]]&];
	\[Gamma] = ExtremeEventThreshold[-neg[[All,2]]];
	negDates = Select[datedReturns, Last[-#] > \[Gamma] &];
	Return[Join[posDates, negDates]];
];

EventProbability[returns_, event_] := Module[{selection, \[Gamma], nonExtremeReturns, \[Lambda], fit, r, p},
	If[Positive[event],
		selection = Select[returns, Positive];
		\[Gamma] = ExtremeEventThreshold[selection];
		,
		selection = -Select[returns, Negative];
		\[Gamma] = ExtremeEventThreshold[selection];
	];

	nonExtremeReturns = Select[selection, #<\[Gamma] &];
	fit = FindDistributionParameters[nonExtremeReturns, ExponentialDistribution[\[Lambda]]];
	p = Probability[r>= Abs[event], r \[Distributed] ReplaceAll[ExponentialDistribution[\[Lambda]], fit]];
	Return[p];
]


(* ::Section::Closed:: *)
(*Symmetry analysis*)


(* ::Text:: *)
(*Using the Subscript[T, n] symmetry statistic*)


(* Load external executable *)
If[$OperatingSystem == "Windows",
	exeName = "TnStatisticMathLink.exe";
,
	exeName = "TnStatisticMathLink";
];
exeFile = FileNameJoin[{$UserBaseDirectory, "Applications", "TnSymmetryStatistic", "Mathematica", "bin", exeName}];
Install[exeFile];

(* Confidence level values *)
\[Alpha]s = {0.001,0.005,0.01,0.025,0.05,0.1,0.15,0.25,0.5};
qs = {7.803,5.768,4.909,3.798,2.983,2.200,1.768,1.258,0.659};
\[Alpha]q = Transpose[{\[Alpha]s,qs}];
upp = Interpolation[\[Alpha]q];
UpperPercentagePoint[p_]:= upp[p];

NestedFirst[list_]:=Nest[First, list, Depth[list]-1];
Tn[v_IntegerList, c_]:= Tn[N[v],c];
Tn[v_IntegerList, c_Integer]:= Tn[N[v],N[c]];
Tn[v_Real64List, c_Integer]:= Tn[v,N[c]];
Tn[v_]:= Tn[v, 0.0];

MeasureTn[returns_, CL_: 0.05, symmetryPoints_: 50] := Module[
{standardError, c, cmin, cmax, \[CapitalDelta]c, test, cSymm, plausiblePoints, plausibleMin = Nothing, plausibleMax = Nothing, testResult},

	standardError = StandardDeviation[returns] / Sqrt[Length[returns]];
	cmin = Mean[returns] - 3*standardError;
	cmax = Mean[returns] + 3*standardError;
	\[CapitalDelta]c = (cmax-cmin)/symmetryPoints;
	test = Table[{c, Tn[N[returns], c]}, {c, cmin, cmax, \[CapitalDelta]c}];
	cSymm = NestedFirst[MinimalBy[test, Last]];
	plausiblePoints = Select[test, Last[#] < UpperPercentagePoint[CL]&];
	If[Length[plausiblePoints] > 0,
		plausibleMin = NestedFirst[MinimalBy[plausiblePoints, First]];
		plausibleMax = NestedFirst[MaximalBy[plausiblePoints, First]];
	];
	
	testResult = <|
	"TnValues"->test, "BestSymmetry"->cSymm, "MinimumC"->cmin, 
	"MaximumC"->cmax, "PlausibleSymmMin"->plausibleMin, "PlausibleSymmMax"->plausibleMax
	|>;
	
	Return[testResult];
];


(* ::Text:: *)
(*Measuring height ratio (bimodal distributions)*)


MeasureHeightRatio[vtret_]:=Quiet[
	Block[{dist, m1, m2},
		dist = SmoothKernelDistribution[vtret];
		m1 = First[FindMaximum[{PDF[dist, x], x>0},{x, 0.01}]];
		m2 = First[FindMaximum[{PDF[dist, x], x<0},{x, -0.01}]];
		Return[m1/m2];
	]
];


(* ::Section::Closed:: *)
(*Dataset builder*)


(* ::Subsection::Closed:: *)
(*Menu elements*)


MarketSearch[name_]:=If[StringLength[name]>0,FinancialData[StringJoin[name,"*"],"Lookup"],""];

ShowMarketInfo[marketname_String]:=Block[{name,ipoDate,sector,marketCap},
	name = FinancialData[marketname,"Name"] // Quiet;
	ipoDate = FinancialData[marketname,"IPODate"] // Quiet;
	sector = FinancialData[marketname,"Sector"] // Quiet;
	marketCap = FinancialData[marketname,"MarketCap"] // Quiet;

	Panel[
		Grid[
		{
			If[!FailureQ[name],{"Name: " ,name},Nothing],
			If[!FailureQ[ipoDate],{"Fecha IPO: " ,ipoDate},Nothing],
			If[!FailureQ[sector],{"Sector: " ,sector},Nothing],
			If[!FailureQ[marketCap],{"MarketCap: " ,marketCap},Nothing]
		},
		Alignment->Left
		],
		Style["Informaci\[OAcute]n del mercado",Bold]
	]
];

PickPrevious[list_,selected_]:=Block[{idx},
	idx = Position[list,selected[[1]]][[1,1]];

	If[idx > 1,
		Return[{list[[idx-1]]}],
		Return[selected]
	];
];

PickNext[list_,selected_]:=Block[{idx},
	idx = Position[list,selected[[1]]][[1,1]];

	If[idx < Length[list],
		Return[{list[[idx+1]]}],
		Return[selected]
	];
];

ControlPicker[Dynamic[pick_],list_]:=
Grid[
	{{
		ListPicker[Dynamic[pick],list,Multiselection->False],
		Column[
		{
			Button["\[UpArrow]",pick = PickPrevious[list,pick]],
			Button["\[DownArrow]",pick = PickNext[list,pick]]
		}
		]
	}}
	,
	Alignment->Center
];

PickAnother[list_,selected_]:=Block[{idx},
	idx = Position[list,selected[[1]]][[1,1]];

	If[idx < Length[list],
		Return[{list[[idx+1]]}],
		Return[{list[[idx-1]]}]
	];
];

SetAttributes[ControlPickerRemove,HoldAll];
ControlPickerRemove[Dynamic[pick_],list_]:= DynamicModule[{nextPick},
	Grid[
		{{
			Dynamic[ListPicker[Dynamic[pick],list,Multiselection->False,ImageSize->{140,140}],TrackedSymbols:>{list}],
			Column[
			{
				Button["\[UpArrow]",pick = PickPrevious[list,pick]],
				Button["\[DownArrow]",pick = PickNext[list,pick]],
				Button["-",
					If[Length[list]>1,
						nextPick = PickAnother[list,pick];
						list = DeleteCases[list,pick[[1]]];
						pick = nextPick;
					];
				]
			}
			]
		}}
		,
		Alignment->Center
	]
];


(* ::Subsection::Closed:: *)
(*Database saving functions*)


SaveDatabase[selected_] := Block[{filepath,database},
	filepath = SystemDialogInput["FileSave",{"database",{"Mathematica binary (*.mx)"->{"*.mx"},"Plain Text Document (*.txt)"->{"*.txt"}}}];
	If[filepath=!= $Canceled,
		database = ProgressMap[CreateMarketDataset,selected,"Label"->"Downloading market data..."];
		Export[filepath,database];
	]
];

CreateMarketDataset[market_]:=Block[{marketname,data,dates,prices,returns,volume,datedreturns,database},
	marketname = FinancialData[market,"Name"];
	data = FinancialData[market,"OHLCV",{1900,1,1}];
	dates = Map[DateObject,data[[All,1]]];
	prices = data[[All,2,4]];
	volume = data[[All,2,5]];
	returns = Returns[prices];
	datedreturns = Transpose[{Drop[dates,-1],returns}];

	database = <|
		"Name"->marketname ,"Symbol"->market,"FirstDate"->First[dates],"LastDate"->Last[dates],
		"Dates"->dates,"Prices"->prices, "Returns"->returns,
		"DatedPrices"->Transpose[{dates,prices}], "DatedReturns"->datedreturns,
		"Volume"->volume
	|>;
	Return[database];
];


(* ::Subsection::Closed:: *)
(*Main panel*)


DatabaseBuilderPanel[] := DynamicModule[
	{
		pick = {"NYSE:UN"},name ="NYSE:UN",selected = {"NYSE:UN"},databasePick = {"NYSE:UN"},searchPanel,marketPanel,databasePanel,databaseCtrlPanel,indOpt,dateOpt,
		indicatorOptions = {"Prices","Returns","Volume"},
		dateOptions = {"Sin fecha","Con fechas"}
	},

	searchPanel = Panel[
		Grid[
			{
				{Style["Nombre",Bold],SpanFromLeft},
				{InputField[Dynamic[name],String,FieldSize->Small],Button["Buscar"]},
				{Style["Resultados",Bold],SpanFromLeft},
				{Dynamic[ControlPicker[Dynamic[pick],MarketSearch[name]]],SpanFromLeft}
			},
			Alignment->Left
		],
		Style["B\[UAcute]squeda",Bold]
	];
	marketPanel = Grid[
		{
			{Dynamic[ShowMarketInfo[First[pick]]]}, 
			{Button["Agregar >", If[!MemberQ[selected, First[pick]], AppendTo[selected, First[pick]]], ImageSize->Automatic]}
		}
	];
	databaseCtrlPanel = Panel[ControlPickerRemove[Dynamic[databasePick], selected], Style["Base de datos", Bold]];
	databasePanel = Grid[{{databaseCtrlPanel}, {Button["Generar", DialogReturn[selected]]}}];

	Panel[Grid[{{searchPanel,marketPanel,databasePanel}}, Alignment->Top]]
];

DatasetBuilderDialog[] := Block[{selected},
	selected = DialogInput[DialogNotebook[DatabaseBuilderPanel[], WindowTitle->"Construir base de datos"]];
	If[selected =!= $Canceled, SaveDatabase[selected]];
];


(* ::Section::Closed:: *)
(*Trend duration*)


UpwardTrendDuration[prices_] := Module[{splitted,posTrends},
	splitted = Split[Sign[SimpleReturns[prices]]];
	posTrends = Select[splitted, Apply[Or, Positive[#]]&];
	Return[Map[Length, posTrends]];
];
DownwardTrendDuration[prices_] := Module[{splitted,negTrends},
	splitted = Split[Sign[SimpleReturns[prices]]];
	negTrends = Select[splitted, Apply[Or, Negative[#]]&];
	Return[Map[Length, negTrends]];
];


(* ::Section:: *)
(*Kullback-Leiber Divergence*)


HackLog[x_]:=If[x>0, Log[x], 0];

KullbackLeibler[data_, dist_] := Module[{empirical},
	empirical = SmoothKernelDistribution[data];
	Quiet[NIntegrate[PDF[empirical,x]*HackLog[PDF[empirical,x]/PDF[dist,x]],{x,-\[Infinity],\[Infinity]}, MaxRecursion->100]]
];

KullbackLeiblerInTime[returns_, window_, dist_:NormalDistribution[], skip_:1]:=Module[{standarized,part},
	standarized = Standardize[returns];
	part = Partition[standarized, window, skip];
	ParallelMap[KullbackLeibler[#, dist[]]&, part, Method->"FinestGrained"]
];


(* ::Chapter:: *)
(*End of package*)


End[ ]

EndPackage[ ]
