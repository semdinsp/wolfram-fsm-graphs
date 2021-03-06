(* ::Package:: *)

(* Mathematica Package *)

(* Created by the Wolfram Workbench Sep 5, 2010 *)
(* copy right Ficonab Pte Ltd, 2015 *)

BeginPackage["fsmEstorm`"]
discoverFun::usage= "Function to sow now if depth is 1";
findNextVertices::usage= "Find next vertices in a graph ";
chooseHighPriorityNode::usage= "Find node with highest priority from association ";
setDefaultFunction::usage="Set default  function for ggraph ";
setDefaultPriorityFunction::usage="Set default prioriy  function for ggraph ";
setDefaultActionFunction::usage="Set default action function for ggraph ";
setDefaultPriority::usage="Set priority for all nodes";
getNextVertex::usage="Get next vertex in state diagram ";
traverseGraph::usage="travers the graph return g,node,state   traverseGraph[g_Graph,start_,state_Association,exitNodes_List]";
currentVertexAction::usage="given a vertex rund the action associated with it ";
defaultPriorityFunction::usage="default function returns priority of node";
fsmPrintDebug::usage="print debug statements eg traverseGraph  //fsmPrintDebug";
defaultActionFunction::usage="default function returns state";
applyDefaultGraphSettings::usage="graph, priority, apply defaults settings to graph, return newgraph";
fsmGraphPlot::usage="plot fsm graph and show priority function on edges";
fsmGraphPlotAction::usage="plot fsm graph and show priority function on edges plus action functions";
Begin["`Private`"]
Attributes[fsmPrintDebug]={HoldAll};
fsmPrintDebug[expr_] := Block[{fsmDebugPrint = Print}, expr];
discoverFun[u_,v_,d_]:=Module[{}, (* Print["Discovered:", u," from ",v, " depth ", d ]; *)
 If[d==1,Sow[u,"vertex"]] ];
 
applyDefaultGraphSettings[g_Graph,priority_]:=Module[{ng},ng=g;
ng=setDefaultPriority[ng,priority];
ng=setDefaultPriorityFunction[ng,defaultPriorityFunction];
ng=setDefaultActionFunction[ng,defaultActionFunction];
ng];

findNextVertices[g_Graph,start_]:=Module[{nodes={}},
nodes=Reap[BreadthFirstScan[g,start,{"DiscoverVertex"-> discoverFun }],"vertex"];
(* Print["nodes",nodes];  *)
Flatten[nodes[[2]]]
];

chooseHighPriorityNode[nodes_Association]:=Module[{sorted,highKey},(* returned highest priority node *)
sorted=Sort[nodes];fsmDebugPrint["sorted list:",sorted];
highKey=Flatten[Position[sorted,Last[Sort[sorted]]]];
highKey[[1,1]]
 ];

fsmGraphPlot[nodes_List,g_Graph]:=Module[{newlist},

newlist={#,{PropertyValue[{g,#[[2]]},"priority"],PropertyValue[{g,#[[2]]},"priorityFunction"]}} & /@ nodes;
  GraphPlot[newlist,EdgeLabeling-> Automatic,VertexLabeling -> True, DirectedEdges-> True]  ];

fsmGraphPlotAction[nodes_List,g_Graph]:=Module[{newlist}, (* plot actionFunctions *)

newlist={StringJoin[#[[1]],"[",ToString[PropertyValue[{g,#[[1]]},"actionFunction"]],"]"] -> StringJoin[#[[2]],"[",ToString[PropertyValue[{g,#[[2]]},"actionFunction"]],"]"],{PropertyValue[{g,#[[2]]},"priority"],PropertyValue[{g,#[[2]]},"priorityFunction"]}} & /@ nodes;
  GraphPlot[newlist,EdgeLabeling-> Automatic,VertexLabeling -> True, DirectedEdges-> True]  ];


getNextVertex[g_Graph,start_,state_Association]:=Module[{nodes,prioritized},(* look at priority to figure out what is next *)
nodes=findNextVertices[g,start];
prioritized={# ->PropertyValue[{g,#},"priorityFunction" ][g,#,state]} & /@ nodes;
fsmDebugPrint["Prioritized nodes are: ",prioritized];
chooseHighPriorityNode[Association[prioritized]]
];

setDefaultPriority[g_Graph,priority_Integer]:=Module[{nodes,newgraph},
newgraph=g;
nodes=VertexList[newgraph];
{PropertyValue[{newgraph,#},"priority" ]= priority} & /@ nodes;
newgraph
];
setDefaultPriorityFunction[g_Graph,fun_Symbol]:=Module[{newgraph},
newgraph=setDefaultFunction[g,fun,"priorityFunction"];
newgraph];
setDefaultActionFunction[g_Graph,fun_Symbol]:=Module[{newgraph},
newgraph=setDefaultFunction[g,fun,"actionFunction"];
newgraph];

setDefaultFunction[g_Graph,fun_Symbol,fname_String]:=Module[{nodes,newgraph},newgraph=g;
(* name should be priorityFunction or actionFunction *)
nodes=VertexList[newgraph];
{PropertyValue[{newgraph,#},fname ]= fun} & /@ nodes;
newgraph
];

defaultPriorityFunction[g_Graph,node_,state_Association]:=Module[{}, (* just return default priorty *)
PropertyValue[{g,node},"priority" ]];

defaultActionFunction[g_Graph,node_,state_Association]:=Module[{ng,nstate,nnode}, (* just return default state *)
nstate=state;
nstate["currentNode"]=node;
nnode=node;
ng=g;
{ng,nnode,nstate}];

currentVertexAction[g_Graph,node_,state_Association]:=Module[{newgraph,newnode,newState,vfun,nstate},(* action for a specific vertex based on state *)
newgraph=g;
vfun=PropertyValue[{g,node},"actionFunction"];
fsmDebugPrint["currentVertexAction: ", node, " running: ",vfun];
{newgraph,newnode,nstate}=vfun[newgraph,node,state];
fsmDebugPrint["----currentVertexAction new node: ", newnode];
{newgraph,newnode,nstate}
];

traverseGraph[g_Graph,start_,state_Association,exitNodes_List]:=Module[{node,nstate,newgraph,enode},
(* traverse through the graph *)
fsmDebugPrint["TraverseGraph: startup: ",start," exit: ",exitNodes, " graph: ",g];
enode=exitNodes;
{newgraph,node,nstate}=currentVertexAction[g,start,state];
nstate["currentNode"]=node;
If[!ContainsAll[VertexList[g],Append[enode,start]],Print["IMPORTATANT: vertices in exidNode list and start node are not in graph"]];
(* While[nstate["currentNode"]!=exitNode, node=getNextVertex[newgraph,node,nstate]; *)
fsmDebugPrint["chceck exit: ",exitNodes, " curent: ",nstate["currentNode"]," contains flag: ",ContainsNone[exitNodes,{nstate["currentNode"]}]];
While[ContainsNone[exitNodes,{nstate["currentNode"]}], node=getNextVertex[newgraph,node,nstate];
fsmDebugPrint["TraverseGraph: next node: ",node];
{newgraph,node,nstate}=currentVertexAction[newgraph,node,nstate]; 
fsmDebugPrint["TraverseGraph: after VertextAction current node: ",node," nstate: ",nstate];
nstate["currentNode"]=node];  (* while *)
Print["TraverseGraph: final node: ",node, " final state: ",nstate];
{newgraph,node,nstate}
];

End[]

EndPackage[]







































