(* ::Package:: *)

(* ::Title:: *)
(*Petal Diagrams in Mathematica*)


(* ::Subtitle:: *)
(*This package implements tools for working with Petal Diagrams in Mathematica.*)


BeginPackage["PetalDiagrams`"]
Needs["KnotTheory`"]
Needs["whitten`"]
Needs["Combinatorica`"]
Needs["PlotLegends`"]


(* ::Section:: *)
(*Whitten Action on Petal Diagrams*)


(* ::Text:: *)
(*In this section we will implement the action of the Whitten group on a petal diagram of a knot.*)
(**)
(*A petal diagram is represented as a tuple {\[Sigma], r} where \[Sigma] is a cyclic permutation of the form {1,i_2,i_3,...,i_n} and r gives the orientation on the petals with the convention that r=0 corresponds to the petals having a right-handed orientation and r=1 meaning that the petals are left-handed oriented.*)


(*Return our standard PD code corresponding to the provided petal diagram*)
(*The petal diagram is assumed to be valid*)
(*For now we only implement for 5 petal diagrams, will do nothing otherwise*)
GetPDCode[Petal_]:=Module[{CrossingList,PDCode,order=Petal[[1]]},
	If[Length[order]==5,
		
		PDCode="PD[";

		If[order[[2]]>order[[5]],PDCode=StringInsert[PDCode,"X[1,9,2,8],",-1],
									PDCode=StringInsert[PDCode,"X[8,1,9,2],",-1]];
		If[order[[2]]>order[[4]],PDCode=StringInsert[PDCode,"X[2,7,3,8],",-1],
									PDCode=StringInsert[PDCode,"X[7,3,8,2],",-1]];
		If[order[[3]]>order[[4]],PDCode=StringInsert[PDCode,"X[3,7,4,6],",-1],
									PDCode=StringInsert[PDCode,"X[6,3,7,4],",-1]];
		If[order[[3]]>order[[5]],PDCode=StringInsert[PDCode,"X[4,9,5,10],",-1],
									PDCode=StringInsert[PDCode,"X[9,5,10,4],",-1]];
		If[order[[4]]>order[[5]],PDCode=StringInsert[PDCode,"X[5,1,6,10]",-1],
									PDCode=StringInsert[PDCode,"X[10,5,1,6]",-1]];

		PDCode=StringInsert[PDCode,"]",-1];

		If[Petal[[2]]==0,Return[ToExpression[PDCode]],
							Return[ApplyWhitten[{-1,{1},{1}},ToExpression[PDCode]]]];
	,None];

	If[Length[order] == 7,

	PDCode="PD[";

	If[order[[4]]>order[[7]],PDCode=StringInsert[PDCode,"X[9,25,10,24],",-1],
									PDCode=StringInsert[PDCode,"X[24,9,25,10],",-1]];
	If[order[[6]]>order[[7]],PDCode=StringInsert[PDCode,"X[18,26,19,25],",-1],
									PDCode=StringInsert[PDCode,"X[25,18,26,19],",-1]];
	If[order[[5]]>order[[7]],PDCode=StringInsert[PDCode,"X[17,26,18,1],",-1],
									PDCode=StringInsert[PDCode,"X[26,18,1,17],",-1]];
	If[order[[4]]>order[[5]],PDCode=StringInsert[PDCode,"X[11,17,12,16],",-1],
									PDCode=StringInsert[PDCode,"X[16,11,17,12],",-1]];
	If[order[[4]]>order[[6]],PDCode=StringInsert[PDCode,"X[10,19,11,20],",-1],
									PDCode=StringInsert[PDCode,"X[19,11,20,10],",-1]];
	If[order[[3]]>order[[7]],PDCode=StringInsert[PDCode,"X[8,23,9,24],",-1],
									PDCode=StringInsert[PDCode,"X[23,9,24,8],",-1]];
	If[order[[2]]>order[[7]],PDCode=StringInsert[PDCode,"X[1,23,2,22],",-1],
									PDCode=StringInsert[PDCode,"X[22,1,23,2],",-1]];
	If[order[[3]]>order[[6]],PDCode=StringInsert[PDCode,"X[7,21,8,20],",-1],
									PDCode=StringInsert[PDCode,"X[20,7,21,8],",-1]];
	If[order[[3]]>order[[5]],PDCode=StringInsert[PDCode,"X[6,15,7,16],",-1],
									PDCode=StringInsert[PDCode,"X[15,7,16,6],",-1]];
	If[order[[3]]>order[[4]],PDCode=StringInsert[PDCode,"X[5,13,6,12],",-1],
									PDCode=StringInsert[PDCode,"X[12,5,13,6],",-1]];
	If[order[[2]]>order[[4]],PDCode=StringInsert[PDCode,"X[4,13,5,14],",-1],
									PDCode=StringInsert[PDCode,"X[13,5,14,4],",-1]];
	If[order[[2]]>order[[5]],PDCode=StringInsert[PDCode,"X[3,15,4,14],",-1],
									PDCode=StringInsert[PDCode,"X[14,3,15,4],",-1]];
	If[order[[2]]>order[[6]],PDCode=StringInsert[PDCode,"X[2,21,3,22]",-1],
									PDCode=StringInsert[PDCode,"X[21,3,22,2]",-1]];


	PDCode=StringInsert[PDCode,"]",-1];

	(*If[Pedal[[2]]==0,Return[ToExpression[PDCode]],
						Return[ApplyWhitten[{-1,{1},{1}},ToExpression[PDCode]]]];*)

	If[Petal[[2]]==0,Return[ToExpression[PDCode]],
						Return[MirrorPD[-1,ToExpression[PDCode]]]];

	,None];
];

(*Take the mirror of a petal diagram*)
PedalMirror[Petal_]:=Module[{},
	Return[{Petal[[1]],Mod[Petal[[2]]+1,2]}];
];

(*Invert the orientation of a petal diagram*)
PedalInvert[Petal_]:=Module[{},
	Return[{Join[{1},Reverse[Petal[[1]][[2;;Length[Petal[[1]]]]]]],Mod[Petal[[2]]+1,2]}];
];

(*Mirror and invert the orientation of a petal diagram*)
PedalMirrorInvert[Petal_]:=Module[{},
	Return[{Join[{1},Reverse[Petal[[1]][[2;;Length[Petal[[1]]]]]]],Petal[[2]]}];
];

ClassifyPetal[Petal_]:=Module[{},
	None
];


EndPackage[]
