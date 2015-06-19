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
traverseGraph::usage="travers the graph return g,node,state ";
currentVertexAction::usage="given a vertex rund the action associated with it ";
defaultPriorityFunction::usage="default function returns priority of node";
defaultActionFunction::usage="default function returns state";
Begin["`Private`"]
discoverFun[u_,v_,d_]:=Module[{}, (* Print["Discovered:", u," from ",v, " depth ", d ]; *)
 If[d==1,Sow[u]] ];

findNextVertices[g_Graph,start_]:=Module[{nodes={}},nodes=Reap[BreadthFirstScan[g,start,{"DiscoverVertex"-> discoverFun }]];
Flatten[nodes[[2]]]];

chooseHighPriorityNode[nodes_Association]:=Module[{sorted,highKey},(* returned highest priority node *)
sorted=Sort[nodes];Print[sorted];
highKey=Flatten[Position[sorted,Last[Sort[sorted]]]];
highKey[[1,1]]
 ];
getNextVertex[g_Graph,start_,state_Association]:=Module[{nodes,prioritized},(* look at priority to figure out what is next *)
nodes=findNextVertices[g,start];
prioritized={# ->PropertyValue[{g,#},"priorityFunction" ][g,#,state]}  &/@ nodes;
Print["Prioritized nodes are: ",prioritized];
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
defaultActionFunction[g_Graph,node_,state_Association]:=Module[{nstate}, (* just return default state *)
nstate=state;
nstate["currentNode"]=node;
{g,node,nstate}];

currentVertexAction[g_Graph,node_,state_Association]:=Module[{newgraph,newnode,newState,vfun,nstate},(* action for a specific vertex based on state *)
newgraph=g;
vfun=PropertyValue[{g,node},"actionFunction"];
{newgraph,newnode,nstate}=vfun[newgraph,node,state];
{newgraph,newnode,nstate}
];

traverseGraph[g_Graph,start_,state_Association,exitNode_]:=Module[{node,nstate,newgraph,},(* traverse through the graph *)
newgraph=g; 
node=start;
{newgraph,node,nstate}=currentVertexAction[g,start,state];
While[nstate["currentNode"]!=exitNode, node=getNextVertex[newgraph,node,nstate];Print["Next node: ",node]; {newgraph,node,nstate}=currentVertexAction[newgraph,node,nstate]; 
Print[" current node: ",node," nstate: ",nstate]];
Print["Final node: ",node, " final state: ",nstate];
{newgraph,node,nstate}
];

End[]

EndPackage[]

















