BeginTestSection["fsmTest"]

VerificationTest[(* 1 *)
	CompoundExpression[Set[graph, List[Rule["open position", "get information"], Rule["get information", "stop loss"], Rule["stop loss", "close position"], Rule["get information", "take profits"], Rule["take profits", "close position"], Rule["get information", "pause"], Rule["pause", "get information"]]], Set[g, Graph[graph, Rule[VertexLabels, "Name"]]], findNextNodes[g, "get information"]]
	,
	List["stop loss", "take profits", "pause"]	
	,
	TestID->"5b707f9e-4716-4a58-93d0-5d3fd506eba5"
]

VerificationTest[(* 2 *)
	CompoundExpression[Set[graph, List[Rule["open position", "get information"], Rule["get information", "stop loss"], Rule["stop loss", "close position"], Rule["get information", "take profits"], Rule["take profits", "close position"], Rule["get information", "pause"], Rule["pause", "get information"]]], Set[g, Graph[graph, Rule[VertexLabels, "Name"]]], findNextNodes[g, "pause"]]
	,
	List["get information"]	
]

VerificationTest[(* 3 *)
	chooseHighPriorityNode[Association[Rule["stop loss", 3], Rule["test", 55], Rule["get information", 233]]]
	,
	"get information"	
]

VerificationTest[(* 4 *)
	chooseHighPriorityNode[Association[Rule["stop loss", 3], Rule["test", 55], Rule["fred", 55], Rule["get information", 2]]]
	,
	"fred"	
]

EndTestSection[]
