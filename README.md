# wolfram-fsm-graphs

Finite state programming using graphs
============

develop a graph in mathematica.  Assign functions to it and traverse the graph programmatically based on state information.  Pas the graph and asociation and it will traverse ititfself based on actions and priorities


Typical usage
=================
    graph = {"open position" -> "get information", "get information" -> "stop loss", "stop loss" -> "close position",  "get information" -> "take profits", "take profits" -> "close position", "get information" -> "pause", "pause" -> "get information"}; 
   g = Graph[graph, VertexLabels -> "Name"]; 
   ng = applyDefaultGraphSettings[g, 5];
   pauseActionFunction[g_Graph, node_, state_Association] := Module[{nstate, dval, ng} , (* Randomly change priority *)
   nstate = state;
   ng = g;
   nstate["currentNode"] = node;
   dval = RandomChoice[{0, 0, 0, 1}];
   PropertyValue[{ng, "pause"}, "priority"] = 
   PropertyValue[{ng, "pause"}, "priority"] - dval;
   {ng, node, nstate}];
   PropertyValue[{ng, "pause"}, "actionFunction"] = pauseActionFunction;
   traverseGraph[ng, "open position", <||>, "close position"] 

Plotting Graphs
================
Use the plots to help debug problems.  Edges have tool tips priority function and priority.  The second plot variant will show vertex actionFunctions.
    fsmGraphPlot[graph, ng]
    fsmGraphPlotAction[graph,ng]
