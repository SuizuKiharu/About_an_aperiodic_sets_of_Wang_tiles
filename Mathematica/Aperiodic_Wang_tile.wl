(* ::Package:: *)

(* ::Chapter:: *)
(*Aperiodic_Wang_tile.wl*)
(*20211223*)
(*\:6c34\:6d25\:3000\:6a39\:6625*)


(* ::Section:: *)
(*Functions*)


(*\:8272\:306e\:5b9a\:7fa9*)
Color[i_,c_]:=Module[{x,r,g,b},x=i/(c+1);r=Max[0,Min[1,3-6x],3x-2];g=Max[0,Min[1,3x,2.5-3x]];b=Max[0,Min[1,6x-3,6-6x]];RGBColor[r,g,b]]


(*\:30bf\:30a4\:30eb\:306e\:4e2d\:5fc3\:90e8\:5206\:306e\:60c5\:5831\:306f\:5fc5\:8981\:306a\:3044\:306e\:3067\:305d\:306e\:90e8\:5206\:3092\:767d\:306b\:3059\:308b*)
Tiling[tile_]:=Module[{tw,te,ts,tn,n,m,c,TriT,TriB,TriL,TriR}, 
tw= tile[[1]];
te= tile[[2]];
ts= tile[[3]];
tn= tile[[4]];
n=Length[tw];(*\:7e26\:306e\:9577\:3055*)
m=Length[tn];(*\:6a2a\:306e\:9577\:3055*)
c=5; (*c:\:8272\:306e\:6570\:5024\:306e\:6700\:5927\:5024*)
TriT[s_,t_]:=If[s==1,{Color[tn[[t]],c],Polygon[{{2t-2,2-2s},{2t,2-2s},{2t-1,1-2s}}]},{RGBColor[1,1,1],Polygon[{{2t-2,2-2s},{2t,2-2s},{2t-1,1-2s}}]}];(*\:4e0a\:8fba\:306e\:4e09\:89d2\:5f62*)
TriB[s_,t_]:=If[s==n,{Color[ts[[t]],c],Polygon[{{2t-2,-2s},{2t,-2s},{2t-1,1-2s}}]},{RGBColor[1,1,1],Polygon[{{2t-2,-2s},{2t,-2s},{2t-1,1-2s}}]}];(*\:4e0b\:8fba\:306e\:4e09\:89d2\:5f62*)
TriL[s_,t_]:=If[t==1,{Color[tw[[s]],c],Polygon[{{2t-2,-2s},{2t-2,2-2s},{2t-1,1-2s}}]},{RGBColor[1,1,1],Polygon[{{2t-2,-2s},{2t-2,2-2s},{2t-1,1-2s}}]}];(*\:5de6\:8fba\:306e\:4e09\:89d2\:5f62*)
TriR[s_,t_]:=If[t==m,{Color[te[[s]],c],Polygon[{{2t,-2s},{2t,2-2s},{2t-1,1-2s}}]},{RGBColor[1,1,1],Polygon[{{2t,-2s},{2t,2-2s},{2t-1,1-2s}}]}];(*\:53f3\:8fba\:306e\:4e09\:89d2\:5f62*)
Graphics[{EdgeForm[Thick],Table[Table[{TriT[s,t],TriB[s,t],TriL[s,t],TriR[s,t]},{t,m}],{s,n}]}]]
(*\:30bf\:30a4\:30eb\:306e\:5408\:6210\:ff08\:5782\:76f4\:65b9\:5411\:ff09*)
Tilecompositionv[tiles1_,tiles2_]:= Module[{tiles={}},

fcompositionv[t1_,t2_]:={Join[t2[[1]],t1[[1]]],Join[t2[[2]],t1[[2]]],t1[[3]],t2[[4]]};
For[i=1,i<=Length[tiles1],i++,
For[j=1,j<=Length[tiles2],j++,
If[tiles2[[j]][[3]]==tiles1[[i]][[4]],
tiles=Append[tiles,fcompositionv[tiles1[[i]],tiles2[[j]]]]
]
]
];
tiles
]
(*\:30bf\:30a4\:30eb\:306e\:5408\:6210\:ff08\:6c34\:5e73\:65b9\:5411\:ff09*)
Tilecompositionh[tiles1_,tiles2_]:= Module[{tiles={}},

fcompositionh[t1_,t2_]:={t1[[1]],t2[[2]],Join[t1[[3]],t2[[3]]],Join[t1[[4]],t2[[4]]]};
For[i=1,i<=Length[tiles1],i++,
For[j=1,j<=Length[tiles2],j++,
If[tiles2[[j]][[1]]==tiles1[[i]][[2]],
tiles=Append[tiles,fcompositionh[tiles1[[i]],tiles2[[j]]]]
]
]
];
tiles
]
(*\:30bf\:30a4\:30eb\:96c6\:5408\:306e\:7c21\:7565\:5316*)
\:3000TileSimplify[tiles0_]:=Module[{i=1,tiles10},
tiles10 =DeleteDuplicates [tiles0];
While[i<=Length[tiles10],
If[MemberQ[Transpose[tiles10][[2]],tiles10[[i]][[1]]]&&
MemberQ[Transpose[tiles10][[1]],tiles10[[i]][[2]]],
i++,tiles10= Delete[tiles10,i] 
]
];
tiles10
]


(* ::PageBreak:: *)
(**)


(* ::Text:: *)
(* *)


(* ::Section:: *)
(*Generate_Graph _Function*)


(*n:Wang_tile\:306e\:6570\:ff08\:ff1d\:30a8\:30c3\:30b8\:306e\:6570\:ff09*)
Generategraphs[n_]:=Module[{graphs,test=100},
graphs = {};\:3000(*\:88dc\:984c\:ff14\:306e\:4eee\:8aac\:3092\:6e80\:305f\:3059\:30b0\:30e9\:30d5\:3092\:3053\:3053\:306b\:8ffd\:52a0\:3057\:3066\:3044\:304f*)
For[v=2,v<=n-2,v++, (*\:9802\:70b9\:306e\:6570\:3092\:6307\:5b9a\:ff1av*)
part=Table[1,v-1];
part= Append[part,n-v+1];
For[t=0,t<=10000,t++,\:3000(*\:5404\:8fba\:306e\:59cb\:70b9\:3092\:52d5\:304b\:3059\:305f\:3081\:306e\:30eb\:30fc\:30d7*)
(*edge\:306e\:8a18\:8ff0*)
from=Flatten[Table[Table[i,part[[i]]],{i,v}]];(*\:5404\:8fba\:306e\:59cb\:70b9\:306e\:30ea\:30b9\:30c8*)
to= Tuples[Range[1,v],n];\:3000(*\:5404\:8fba\:306e\:7d42\:70b9\:306e\:30ea\:30b9\:30c8\:306e\:5168\:30d1\:30bf\:30fc\:30f3*)
For[k=1,k<= Length[to],k++,(*\:5404\:8fba\:306e\:7d42\:70b9\:3092\:52d5\:304b\:3059\:30eb\:30fc\:30d7*)
graph =Table[{from[[i]],to[[k]][[i]]},{i,n}];
(*\:5bfe\:79f0\:6027\:306b\:3088\:308b\:524a\:9664*)
For[l=1,l<=n-1,l++,
ng=graph[[l,1]]==graph[[l+1,1]]&& graph[[l,2]]>graph[[l+1,2]];
If[ng,Break[]]
];
If[ng,Continue[]];
(*\:5f31\:9023\:7d50\:6210\:5206\[RightArrow]\:5f37\:9023\:7d50\:6210\:5206\:306e\:5224\:5b9a*)

ok=SubsetQ[Transpose[graph][[2]],Range[1,v]];

If[Not[ok],Continue[]];
array=ConstantArray[0,{v,v}];
For[i=1,i<=n,i++,
array[ [graph[[i,1]],graph[[i,2]] ] ]++
]; (*\:96a3\:63a5\:884c\:5217\:306e\:4f5c\:6210*)
adjacencygraph=AdjacencyGraph[array];
ok=Sort[ConnectedComponents[adjacencygraph]](*\:30b0\:30e9\:30d5\:306e\:5f37\:9023\:7d50\:6210\:5206*)==Sort[WeaklyConnectedComponents[adjacencygraph]] (*\:30b0\:30e9\:30d5\:306e\:5f31\:9023\:7d50\:6210\:5206*);
If[Not[ok],Continue[]];
(*\:30b5\:30a4\:30af\:30eb\:306e\:6709\:7121\:306e\:78ba\:8a8d*)
set={};
For[i=1,i<=v,i++,If[Length[Select[graph,#[[1]]==i&]]>=2,AppendTo[set,i]]]; (*set:\:51fa\:6b21\:6570\:304c\:ff12\:4ee5\:4e0a\:306e\:9802\:70b9\:306e\:96c6\:5408*)
For[l=1,l<= Length[ConnectedComponents[adjacencygraph]],l++,
ng =Intersection[ConnectedComponents[adjacencygraph][[l]],set]=={}; (*\:5404\:5f37\:9023\:7d50\:6210\:5206\:304c\:51fa\:6b21\:6570\:304c\:ff12\:4ee5\:4e0a\:306e\:9802\:70b9\:3092\:4e00\:3064\:3082\:6301\:305f\:306a\:3044\:ff1d\:ff1e\:30b5\:30a4\:30af\:30eb\:3068\:306a\:3063\:3066\:3044\:308b\:305f\:3081ng*)
If[ng,Break[]]
];
If[ng, Continue[]];
(*\:30b0\:30e9\:30d5\:3092\:7f6e\:63db\:3057\:305f\:5f8c\:30bd\:30fc\:30c8\:3057\:3066\:5bfe\:79f0\:6027\:306b\:3088\:308b\:88ab\:308a\:3092\:53d6\:308a\:9664\:304f*)
a=Permutations[Range[v]];
permutedfroms=Table[Table[a[ [i,graph[[j,1]]] ],{j,n}],{i,Length[a]}];
permutedtos = Table[Table[a[ [i,graph[[j,2]]] ],{j,n}],{i,Length[a]}];
permutedgraphs = Table[Sort[Transpose[{permutedfroms[[i]],permutedtos[[i]]}]],{i,Length[a]}];
ok =Intersection[permutedgraphs,graphs]== {};
If[Not[ok],Continue[]];
permutedgraphs = Table[Sort[Transpose[{permutedtos[[i]],permutedfroms[[i]]}]],{i,Length[a]}];
ok =Intersection[permutedgraphs,graphs]== {};
If[Not[ok],Continue[]];
(*\:30b0\:30e9\:30d5\:306e\:8ffd\:52a0*)
graphs=Append[graphs,graph]
];
(*part(from)\:306e\:5909\:66f4*)
If[part[[v]]-1>part[[v-1]],part[[v]]--;part[[v-1]]++,
For[j=v-1;s=part[[v]]+part[[v-1]]-1,part[[j]]>=part[[v]]-1&&j>0,j--,s=s+part[[j]]];
If[j<=0,Break[]];
x=part[[j]]+1; part[[j]]=x; j++;
While[j<v,part[[j]]=x;s=s-x;j++];
part[[v]]=s
];


If[t>=10000,Append[graphs,"error"]];
];
];
graphs
]


(* ::Section:: *)
(*\:5468\:671f\:7684\:306a\:3082\:306e\:3092\:62bd\:51fa\:3059\:308bfunction*)


(*\:975e\:5468\:671f\:7684\:3067\:306a\:3044\:3053\:3068\:306e\:8a3c\:660e*)
Nowang[tiles_]:= Module[{a=0,newtiles,oldtiles,subtiles,oldsubtiles,simpletiles},
newtiles=tiles;
oldtiles={};
subtiles={};
(*\:30bf\:30a4\:30eb\:306e\:7c21\:7565\:5316\:3092Wang\:96c6\:5408\:304c\:5909\:5316\:3057\:306a\:304f\:306a\:308b\:307e\:3067\:884c\:3046*)
While[newtiles!=oldtiles,oldtiles=newtiles;newtiles=TileSimplify[newtiles]];
simpletiles= newtiles;

For[k=1,k<100,k++,
(*\:7c21\:7565\:5316\:306e\:7d50\:679cWang\:96c6\:5408\:304c\:7a7a\:306b\:306a\:3063\:305f\:5834\:5408a=1\:3092\:51fa\:529b\:3057break*)
If[newtiles=={},a=1;Break[]];
(*\:5357\:5317\:304c\:540c\:8272\:306e\:30bf\:30a4\:30eb\:306e\:307f\:3092\:53d6\:308a\:51fa\:3057\:305f\:96c6\:5408subtiles\:3092\:8003\:3048\:308b*)
subtiles=Select[newtiles,#[[3]]==#[[4]]&];
oldsubtiles={};
(*subtiles\:3092\:7c21\:7565\:5316\:3057\:7d9a\:3051\:3066\:3082\:7a7a\:306b\:306a\:3089\:306a\:3044\:5834\:5408a=2\:3092\:51fa\:529b\:3057break*)
While[subtiles!=oldsubtiles,oldsubtiles=subtiles;subtiles=TileSimplify[subtiles]];
If[subtiles!={},a=2;Break[]];
newtiles=Tilecompositionv[newtiles,simpletiles];
(*\:30bf\:30a4\:30eb\:306e\:7c21\:7565\:5316\:3092Wang\:96c6\:5408\:304c\:5909\:5316\:3057\:306a\:304f\:306a\:308b\:307e\:3067\:884c\:3046*)
While[newtiles!=oldtiles,oldtiles=newtiles;newtiles=TileSimplify[newtiles]]
];
{a,subtiles}
]
jea(*\:5468\:671f\:7684\:306aWang\:96c6\:5408\:306e\:4e00\:5468\:671f\:306e\:4e00\:3064\:3092\:53ef\:8996\:5316*)
Periodictile[subtiles_]:=Module[{periodictiles,periodictile},periodictiles={subtiles[[1]]};
While[Select[periodictiles,#[[1]]==#[[2]]&]=={},
periodictiles=Tilecompositionh[periodictiles,subtiles];
periodictile=Map[Tiling,Select[periodictiles,#[[1]]==#[[2]]&]]];
periodictile
]


(* ::Section:: *)
(*\:30bf\:30a4\:30eb\:306e\:5b9a\:7fa9*)


Culiktiles2={{{3},{3},{3},{0}},{{3},{3},{2},{1}},{{3},{4},{1},{0}},{{3},{4},{1},{3}},{{4},{4},{3},{0}},{{4},{4},{2},{1}},{{4},{3},{1},{1}},{{0},{1},{1},{2}},{{0},{2},{1},{1}},{{1},{2},{1},{2}},{{1},{0},{0},{1}},{{2},{0},{0},{2}},{{2},{1},{0},{1}}}\:3000(*Culik\:306e13\:500b\:304b\:3089\:6210\:308bWang\:96c6\:5408\[Tau]=\:ff08H={0,1,2,3,4}\:3001V={0,1,2,3}\:3001T={{3,3,3,0},...,{2,1,0,1}}\:ff09*)
tiles1 = {{{3},{2},{2},{1}},{{2},{2},{3},{1}},{{2},{3},{0},{2}},{{1},{3},{1},{2}},{{3},{2},{2},{3}},{{2},{1},{1},{0}},{{2},{3},{1},{1}},{{3},{1},{1},{2}},{{1},{2},{2},{1}}}
tiles2={{{3},{1},{2},{0}},{{2},{4},{3},{1}},{{2},{3},{0},{2}}}
Jeandeltiles={{{3},{1},{1},{1}},{{3},{1},{2},{2}},{{3},{3},{3},{1}},{{2},{2},{1},{0}},{{2},{2},{0},{2}},{{0},{0},{1},{0}},{{0},{3},{2},{1}},{{1},{0},{2},{2}},{{1},{1},{0},{2}},{{1},{3},{2},{3}},{{3},{0},{1},{1}}}








