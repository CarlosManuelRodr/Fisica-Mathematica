(* ::Package:: *)

(* ::Title:: *)
(*Dataset builder*)


(* ::Chapter:: *)
(*Begin package*)


BeginPackage["DatasetBuilder`"]

DatasetBuilderDialog::usage = "Launch dialog to create the database";

Begin["`Private`"]


(* ::Chapter:: *)
(*Definitions*)


Returns[prices_,lag_:1]:= N[Log[Drop[prices,lag]]-Log[Drop[prices,-lag]]];


(* ::Chapter:: *)
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


(* ::Chapter:: *)
(*Database saving functions*)


Needs["AdvancedMapping`"];
SaveDatabase[selected_]:=Block[{filepath,database},
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


(* ::Chapter:: *)
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
	marketPanel = Grid[{{Dynamic[ShowMarketInfo[First[pick]]]},{Button["Agregar >",If[!MemberQ[selected,First[pick]],AppendTo[selected,First[pick]]],ImageSize->Automatic]}}];
	databaseCtrlPanel =Panel[ControlPickerRemove[Dynamic[databasePick],selected],Style["Base de datos", Bold]];
	databasePanel = Grid[{{databaseCtrlPanel},{Button["Generar",DialogReturn[selected]]}}];

	Panel[Grid[{{searchPanel,marketPanel,databasePanel}},Alignment->Top]]
];

DatasetBuilderDialog[]:=Block[{selected},
	selected = DialogInput[DialogNotebook[DatabaseBuilderPanel[],WindowTitle->"Construir base de datos"]];
	If[selected =!= $Canceled,SaveDatabase[selected]];
];


(* ::Chapter:: *)
(*End of package*)


End[ ]

EndPackage[ ]
