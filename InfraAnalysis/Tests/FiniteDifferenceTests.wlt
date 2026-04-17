BeginTestSection["FiniteDifferenceTests"]


path3 = Graph[ { 1, 2, 3 }, { DirectedEdge[ 1, 2 ], DirectedEdge[ 2, 3 ] } ]
diamond = Graph[ { 1, 2, 3, 4 }, { DirectedEdge[ 1, 2 ], DirectedEdge[ 1, 3 ], DirectedEdge[ 2, 4 ], DirectedEdge[ 3, 4 ] } ]

f3 = <| 1 -> 10, 2 -> 20, 3 -> 30 |>
h3 = <| 1 -> 7, 2 -> 11, 3 -> 13 |>
X3 = <| 1 -> 2, 2 -> 3, 3 -> 2 |>


VerificationTest[
	GraphVectorFieldQ[ path3, X3 ],
	True,
	TestID -> "VF-Q-Valid"
]

VerificationTest[
	GraphVectorFieldQ[ path3, <| 1 -> 3, 2 -> 3, 3 -> 2 |> ],
	False,
	TestID -> "VF-Q-Invalid-NonNeighbor"
]

VerificationTest[
	GraphVectorFieldQ[ path3, <| 1 -> 2, 2 -> 3 |> ],
	False,
	TestID -> "VF-Q-Invalid-MissingKey"
]

VerificationTest[
	Length @ GraphVectorFields[ path3 ],
	2,
	TestID -> "VF-Count-Path3"
]

VerificationTest[
	Length @ GraphVectorFields[ diamond ],
	16,
	TestID -> "VF-Count-Diamond"
]

VerificationTest[
	GraphVectorFieldEndomorphism[ path3, X3 ][ f3 ],
	<| 1 -> 20, 2 -> 30, 3 -> 20 |>,
	TestID -> "VF-Endo-Path3"
]


VerificationTest[
	GraphFiniteDifference[ path3, f3 ],
	<| 1 -> 10, 2 -> 10, 3 -> 10 |>,
	TestID -> "FiniteDiff-Path3"
]


VerificationTest[
	GraphDirectionalDifference[ path3, X3, f3 ],
	<| 1 -> 10, 2 -> 10, 3 -> -10 |>,
	TestID -> "Dir-Diff-Path3"
]


VerificationTest[
	GraphWeightedDerivation[ path3, X3, f3, 1 ],
	GraphDirectionalDifference[ path3, X3, f3 ],
	TestID -> "Wgt-Derivation-Identity"
]

VerificationTest[
	GraphWeightedDerivation[ path3, X3, f3, 2 ],
	<| 1 -> 5, 2 -> 5, 3 -> -5 |>,
	TestID -> "Wgt-Derivation-Scalar"
]

VerificationTest[
	GraphWeightedDerivation[ path3, X3, f3, <| 1 -> 2, 2 -> 5, 3 -> 1 |> ],
	<| 1 -> 5, 2 -> 2, 3 -> -10 |>,
	TestID -> "Wgt-Derivation-Assoc"
]


VerificationTest[
	GraphTwistedProduct[ path3, X3, <| 1 -> 2, 2 -> 3, 3 -> 5 |>, h3 ],
	<| 1 -> 21, 2 -> 55, 3 -> 39 |>,
	TestID -> "Twisted-Path3"
]


VerificationTest[
	GraphWeightedLeibnizQ[ path3, X3, f3, h3, 1 ],
	True,
	TestID -> "Leibniz-True"
]

VerificationTest[
	GraphWeightedLeibnizQ[ path3, X3, <| 1 -> 1, 2 -> 0, 3 -> 0 |>, <| 1 -> 1, 2 -> 0, 3 -> 0 |>, 2 ],
	False,
	TestID -> "Leibniz-False"
]


VerificationTest[
	GraphQ @ GraphDerivative[ path3, f3, Method -> "Directional", "VectorField" -> X3 ],
	True,
	TestID -> "API-Derivative-Directional-ReturnsGraph"
]

VerificationTest[
	GraphQ @ GraphDerivative[ path3, f3, Method -> "Weighted", "VectorField" -> X3, "Weight" -> 2 ],
	True,
	TestID -> "API-Derivative-Weighted-ReturnsGraph"
]


EndTestSection[]
