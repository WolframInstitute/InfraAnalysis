BeginTestSection["BlowUpTests"]

path3 = Graph[ { 1, 2, 3 }, { DirectedEdge[ 1, 2 ], DirectedEdge[ 2, 3 ] } ]
f = <| 1 -> 10, 2 -> 20, 3 -> 30 |>


(* --- GraphBlowUp (graph only) --- *)

VerificationTest[
	VertexCount @ GraphBlowUp[ path3, 2 ],
	6,
	TestID -> "BlowUp-VertexCount"
]

VerificationTest[
	EdgeCount @ GraphBlowUp[ path3, 2 ],
	8,
	TestID -> "BlowUp-EdgeCount"
]

VerificationTest[
	Sort @ VertexList @ GraphBlowUp[ path3, 2 ],
	Sort @ { {1,1}, {1,2}, {2,1}, {2,2}, {3,1}, {3,2} },
	TestID -> "BlowUp-VertexNames"
]

VerificationTest[
	Sort @ EdgeList @ GraphBlowUp[ path3, 2 ],
	Sort @ {
		DirectedEdge[ {1,1}, {2,1} ], DirectedEdge[ {1,1}, {2,2} ],
		DirectedEdge[ {1,2}, {2,1} ], DirectedEdge[ {1,2}, {2,2} ],
		DirectedEdge[ {2,1}, {3,1} ], DirectedEdge[ {2,1}, {3,2} ],
		DirectedEdge[ {2,2}, {3,1} ], DirectedEdge[ {2,2}, {3,2} ]
	},
	TestID -> "BlowUp-Edges"
]


(* --- GraphBlowUp with diffusion --- *)

VerificationTest[
	Sort @ Keys @ GraphBlowUp[ path3, 2, f ],
	Sort @ { "Graph", "Values" },
	TestID -> "BlowUp-WithValues-ReturnShape"
]

VerificationTest[
	GraphQ @ GraphBlowUp[ path3, 2, f ][ "Graph" ],
	True,
	TestID -> "BlowUp-WithValues-GraphIsGraph"
]

VerificationTest[
	GraphBlowUp[ path3, 2, f ][ "Values" ],
	<| {1,1} -> 5, {1,2} -> 5, {2,1} -> 10, {2,2} -> 10, {3,1} -> 15, {3,2} -> 15 |>,
	TestID -> "BlowUp-Diffusion-Values"
]

VerificationTest[
	Total @ Values @ GraphBlowUp[ path3, 2, f ][ "Values" ],
	Total @ Values @ f,
	TestID -> "BlowUp-Diffusion-TotalPreserved"
]


(* --- Non-uniform blow-up --- *)

VerificationTest[
	VertexCount @ GraphBlowUp[ path3, <| 1 -> 1, 2 -> 3, 3 -> 2 |> ],
	6,
	TestID -> "BlowUp-NonUniform-VertexCount"
]

VerificationTest[
	Total @ Values @ GraphBlowUp[ path3, <| 1 -> 1, 2 -> 3, 3 -> 2 |>, f ][ "Values" ],
	Total @ Values @ f,
	TestID -> "BlowUp-NonUniform-TotalPreserved"
]


(* --- GraphContract --- *)

VerificationTest[
	Sort @ Keys @ GraphContract[ GraphBlowUp[ path3, 2, f ][ "Graph" ], GraphBlowUp[ path3, 2, f ][ "Values" ] ],
	Sort @ { "Graph", "Values" },
	TestID -> "Contract-ReturnShape"
]

VerificationTest[
	Sort @ VertexList @ GraphContract[ GraphBlowUp[ path3, 2 ] ],
	Sort @ VertexList @ path3,
	TestID -> "Contract-VerticesMatchOriginal"
]

VerificationTest[
	Module[ { bg, result },
		bg = GraphBlowUp[ path3, 2, f ];
		result = GraphContract[ bg[ "Graph" ], bg[ "Values" ] ];
		result[ "Values" ]
	],
	f,
	TestID -> "Roundtrip-DiffuseContract"
]

VerificationTest[
	Module[ { bg, result },
		bg = GraphBlowUp[ path3, 3, f ];
		result = GraphContract[ bg[ "Graph" ], bg[ "Values" ] ];
		result[ "Values" ]
	],
	f,
	TestID -> "Roundtrip-DiffuseContract-n3"
]


(* --- Group law: BlowUp(BlowUp(G,n),m) ~ BlowUp(G,nm) --- *)

VerificationTest[
	VertexCount @ GraphBlowUp[ GraphBlowUp[ path3, 2 ], 3 ],
	VertexCount @ GraphBlowUp[ path3, 6 ],
	TestID -> "GroupLaw-VertexCount"
]

VerificationTest[
	EdgeCount @ GraphBlowUp[ GraphBlowUp[ path3, 2 ], 3 ],
	EdgeCount @ GraphBlowUp[ path3, 6 ],
	TestID -> "GroupLaw-EdgeCount"
]


EndTestSection[]
