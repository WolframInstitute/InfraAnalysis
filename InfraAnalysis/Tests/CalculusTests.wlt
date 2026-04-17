BeginTestSection["CalculusTests"]


(* ===== Test Fixtures ===== *)

path3 = Graph[ { 1, 2, 3 }, { DirectedEdge[ 1, 2 ], DirectedEdge[ 2, 3 ] } ]
path5 = Graph[ Range[ 5 ], DirectedEdge @@@ Partition[ Range[ 5 ], 2, 1 ] ]
diamond = Graph[ { 1, 2, 3, 4 }, { DirectedEdge[ 1, 2 ], DirectedEdge[ 1, 3 ], DirectedEdge[ 2, 4 ], DirectedEdge[ 3, 4 ] } ]
binaryTree = Graph[ Range[ 7 ], { DirectedEdge[ 1, 2 ], DirectedEdge[ 1, 3 ], DirectedEdge[ 2, 4 ], DirectedEdge[ 2, 5 ], DirectedEdge[ 3, 6 ], DirectedEdge[ 3, 7 ] } ]
forkJoin = Graph[ Range[ 5 ], { DirectedEdge[ 1, 2 ], DirectedEdge[ 1, 3 ], DirectedEdge[ 1, 4 ], DirectedEdge[ 2, 5 ], DirectedEdge[ 3, 5 ], DirectedEdge[ 4, 5 ] } ]
singleVertex = Graph[ { 1 }, {} ]
disconnected = Graph[ { 1, 2, 3, 4 }, { DirectedEdge[ 1, 2 ], DirectedEdge[ 3, 4 ] } ]

f3 = <| 1 -> 2, 2 -> -1, 3 -> 3 |>
f5 = <| 1 -> 1, 2 -> 3, 3 -> -2, 4 -> 5, 5 -> -1 |>
fDiamond = <| 1 -> 1, 2 -> 2, 3 -> -1, 4 -> 3 |>
fTree = <| 1 -> 1, 2 -> -1, 3 -> 2, 4 -> 3, 5 -> -2, 6 -> 1, 7 -> -3 |>
fFork = <| 1 -> 2, 2 -> -1, 3 -> 3, 4 -> -2, 5 -> 4 |>
fSingle = <| 1 -> 7 |>
fDisc = <| 1 -> 1, 2 -> -1, 3 -> 2, 4 -> 3 |>


(* ===== 1. Fundamental Theorem D∘I = id: Ordered (Mobius inversion) ===== *)

VerificationTest[
	GraphMobiusInversion[ path3, GraphZetaConvolution[ path3, f3 ] ],
	f3,
	TestID -> "FT-Ordered-Path3"
]

VerificationTest[
	GraphMobiusInversion[ path5, GraphZetaConvolution[ path5, f5 ] ],
	f5,
	TestID -> "FT-Ordered-Path5"
]

VerificationTest[
	GraphMobiusInversion[ diamond, GraphZetaConvolution[ diamond, fDiamond ] ],
	fDiamond,
	TestID -> "FT-Ordered-Diamond"
]

VerificationTest[
	GraphMobiusInversion[ binaryTree, GraphZetaConvolution[ binaryTree, fTree ] ],
	fTree,
	TestID -> "FT-Ordered-BinaryTree"
]

VerificationTest[
	GraphMobiusInversion[ forkJoin, GraphZetaConvolution[ forkJoin, fFork ] ],
	fFork,
	TestID -> "FT-Ordered-ForkJoin"
]


(* ===== 2. Fundamental Theorem D∘I = id: all methods via unified API ===== *)

VerificationTest[
	GraphFundamentalTheorem[ path3, f3 ],
	<| "Ordered" -> True, "Cumulative" -> True, "Conservative" -> True, "Laminar" -> True |>,
	TestID -> "FT-All-Path3"
]

VerificationTest[
	GraphFundamentalTheorem[ path5, f5 ],
	<| "Ordered" -> True, "Cumulative" -> True, "Conservative" -> True, "Laminar" -> True |>,
	TestID -> "FT-All-Path5"
]

VerificationTest[
	GraphFundamentalTheorem[ diamond, fDiamond ],
	<| "Ordered" -> True, "Cumulative" -> True, "Conservative" -> True, "Laminar" -> True |>,
	TestID -> "FT-All-Diamond"
]

VerificationTest[
	GraphFundamentalTheorem[ binaryTree, fTree ],
	<| "Ordered" -> True, "Cumulative" -> True, "Conservative" -> True, "Laminar" -> True |>,
	TestID -> "FT-All-BinaryTree"
]

VerificationTest[
	GraphFundamentalTheorem[ forkJoin, fFork ],
	<| "Ordered" -> True, "Cumulative" -> True, "Conservative" -> True, "Laminar" -> True |>,
	TestID -> "FT-All-ForkJoin"
]


(* ===== 3. Fundamental Theorem: single-method queries ===== *)

VerificationTest[
	GraphFundamentalTheorem[ diamond, fDiamond, Method -> "Ordered" ],
	True,
	TestID -> "FT-Single-Ordered"
]

VerificationTest[
	GraphFundamentalTheorem[ diamond, fDiamond, Method -> "Cumulative" ],
	True,
	TestID -> "FT-Single-Cumulative"
]

VerificationTest[
	GraphFundamentalTheorem[ diamond, fDiamond, Method -> "Conservative" ],
	True,
	TestID -> "FT-Single-Conservative"
]

VerificationTest[
	GraphFundamentalTheorem[ diamond, fDiamond, Method -> "Laminar" ],
	True,
	TestID -> "FT-Single-Laminar"
]


(* ===== 4. Mobius Inversion Theorem: I∘D = id for Ordered ===== *)

VerificationTest[
	GraphZetaConvolution[ path3, GraphMobiusInversion[ path3, f3 ] ],
	f3,
	TestID -> "MIT-Path3"
]

VerificationTest[
	GraphZetaConvolution[ diamond, GraphMobiusInversion[ diamond, fDiamond ] ],
	fDiamond,
	TestID -> "MIT-Diamond"
]

VerificationTest[
	GraphZetaConvolution[ forkJoin, GraphMobiusInversion[ forkJoin, fFork ] ],
	fFork,
	TestID -> "MIT-ForkJoin"
]


(* ===== 5. Mobius Function: specific values ===== *)

VerificationTest[
	Module[ { mobius },
		mobius = GraphMobiusFunction[ path3 ];
		{ Lookup[ mobius, Key[ { 1, 1 } ], 0 ], Lookup[ mobius, Key[ { 1, 2 } ], 0 ], Lookup[ mobius, Key[ { 1, 3 } ], 0 ],
		  Lookup[ mobius, Key[ { 2, 2 } ], 0 ], Lookup[ mobius, Key[ { 2, 3 } ], 0 ],
		  Lookup[ mobius, Key[ { 3, 3 } ], 0 ] }
	],
	{ 1, -1, 0, 1, -1, 1 },
	TestID -> "Mobius-Values-Path3"
]

VerificationTest[
	Module[ { mobius },
		mobius = GraphMobiusFunction[ diamond ];
		Lookup[ mobius, Key[ { 1, 4 } ], 0 ]
	],
	1,
	TestID -> "Mobius-Diamond-SourceToSink"
]

VerificationTest[
	GraphMobiusFunction[ singleVertex ],
	<| { 1, 1 } -> 1 |>,
	TestID -> "MobiusFunction-SingleVertex"
]


(* ===== 6. Mobius Function: mu * zeta = delta ===== *)

VerificationTest[
	Module[ { mobius, vertices, n, mMatrix, zMatrix },
		mobius = GraphMobiusFunction[ path3 ];
		vertices = VertexList[ path3 ];
		n = Length[ vertices ];
		mMatrix = Table[ Lookup[ mobius, Key[ { vertices[[ i ]], vertices[[ j ]] } ], 0 ], { i, n }, { j, n } ];
		zMatrix = Table[ If[ MemberQ[ VertexOutComponent[ path3, vertices[[ i ]] ], vertices[[ j ]] ], 1, 0 ], { i, n }, { j, n } ];
		mMatrix . zMatrix
	],
	IdentityMatrix[ 3 ],
	TestID -> "Mobius-ZetaProduct-Path3"
]

VerificationTest[
	Module[ { mobius, vertices, n, mMatrix, zMatrix },
		mobius = GraphMobiusFunction[ diamond ];
		vertices = VertexList[ diamond ];
		n = Length[ vertices ];
		mMatrix = Table[ Lookup[ mobius, Key[ { vertices[[ i ]], vertices[[ j ]] } ], 0 ], { i, n }, { j, n } ];
		zMatrix = Table[ If[ MemberQ[ VertexOutComponent[ diamond, vertices[[ i ]] ], vertices[[ j ]] ], 1, 0 ], { i, n }, { j, n } ];
		mMatrix . zMatrix
	],
	IdentityMatrix[ 4 ],
	TestID -> "Mobius-ZetaProduct-Diamond"
]


(* ===== 7. Edge Cases ===== *)

VerificationTest[
	GraphFundamentalTheorem[ singleVertex, fSingle ],
	<| "Ordered" -> True, "Cumulative" -> True, "Conservative" -> True, "Laminar" -> True |>,
	TestID -> "FT-SingleVertex"
]

VerificationTest[
	GraphFundamentalTheorem[ disconnected, fDisc ],
	<| "Ordered" -> True, "Cumulative" -> True, "Conservative" -> True, "Laminar" -> True |>,
	TestID -> "FT-Disconnected"
]

VerificationTest[
	GraphZetaConvolution[ singleVertex, fSingle ],
	<| 1 -> 7 |>,
	TestID -> "Zeta-SingleVertex"
]

VerificationTest[
	GraphMobiusInversion[ singleVertex, fSingle ],
	<| 1 -> 7 |>,
	TestID -> "Mobius-SingleVertex"
]


(* ===== 8. Point Integration ===== *)

VerificationTest[
	GraphIntegral[ path3, f3, 3 ],
	2 + (-1) + 3,
	TestID -> "PointIntegral-Path3-Sink"
]

VerificationTest[
	GraphIntegral[ path3, f3, 1 ],
	2,
	TestID -> "PointIntegral-Path3-Source"
]

VerificationTest[
	GraphIntegral[ diamond, fDiamond, 4 ],
	1 + 2 + (-1) + 3,
	TestID -> "PointIntegral-Diamond-Sink"
]


(* ===== 9. Unified API returns Graph ===== *)

VerificationTest[
	GraphQ[ GraphIntegrate[ path3, f3 ] ],
	True,
	TestID -> "API-Integrate-ReturnsGraph"
]

VerificationTest[
	GraphQ[ GraphDerivative[ path3, f3 ] ],
	True,
	TestID -> "API-Derivative-ReturnsGraph"
]

VerificationTest[
	GraphQ[ GraphIntegrate[ diamond, fDiamond, Method -> "Cumulative" ] ],
	True,
	TestID -> "API-Integrate-Cumulative-ReturnsGraph"
]

VerificationTest[
	GraphQ[ GraphDerivative[ diamond, fDiamond, Method -> "Conservative" ] ],
	True,
	TestID -> "API-Derivative-Conservative-ReturnsGraph"
]


EndTestSection[]
