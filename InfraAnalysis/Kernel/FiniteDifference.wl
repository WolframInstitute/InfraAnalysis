Package["WolframInstitute`InfraAnalysis`"]

PackageExport[GraphVectorFieldQ]
PackageExport[GraphVectorFields]
PackageExport[GraphVectorFieldEndomorphism]
PackageExport[GraphFiniteDifference]
PackageExport[GraphDirectionalDifference]
PackageExport[GraphWeightedDerivation]
PackageExport[GraphTwistedProduct]
PackageExport[GraphWeightedLeibnizQ]


GraphVectorFieldQ[ graph_Graph, X_Association ] :=
	Sort[ Keys[ X ] ] === Sort[ VertexList[ graph ] ] &&
		AllTrue[ Keys[ X ], MemberQ[ AdjacencyList[ graph, # ], X[ # ] ] & ]

GraphVectorFields[ graph_Graph ] :=
	Module[ { vertices, neighborLists },
		vertices = VertexList[ graph ];
		neighborLists = AdjacencyList[ graph, # ] & /@ vertices;
		AssociationThread[ vertices, # ] & /@ Tuples[ neighborLists ]
	]

GraphVectorFieldEndomorphism[ graph_Graph, X_Association ] :=
	f |-> AssociationMap[ vertex |-> Lookup[ f, X[ vertex ], 0 ], VertexList[ graph ] ]


GraphFiniteDifference[ graph_Graph, f_Association ] :=
	AssociationMap[
		vertex |-> Lookup[ f, vertex, 0 ] - Total[ Lookup[ f, #, 0 ] & /@ VertexInComponent[ graph, vertex, { 1 } ] ],
		VertexList[ graph ]
	]

GraphDirectionalDifference[ graph_Graph, X_Association, f_Association ] :=
	AssociationMap[
		vertex |-> Lookup[ f, X[ vertex ], 0 ] - Lookup[ f, vertex, 0 ],
		VertexList[ graph ]
	]

GraphWeightedDerivation[ graph_Graph, X_Association, f_Association, epsilon_ ] :=
	AssociationMap[
		vertex |-> ( Lookup[ f, X[ vertex ], 0 ] - Lookup[ f, vertex, 0 ] ) /
			If[ AssociationQ[ epsilon ], Lookup[ epsilon, vertex, 1 ], epsilon ],
		VertexList[ graph ]
	]

GraphWeightedDerivation[ graph_Graph, X_Association, f_Association ] :=
	GraphDirectionalDifference[ graph, X, f ]


GraphTwistedProduct[ graph_Graph, X_Association, f_Association, h_Association ] :=
	AssociationMap[
		vertex |-> Lookup[ f, X[ vertex ], 0 ] * Lookup[ h, vertex, 0 ],
		VertexList[ graph ]
	]

GraphWeightedLeibnizQ[ graph_Graph, X_Association, f_Association, h_Association, epsilon_ : 1 ] :=
	Module[ { vertices, lhs, rhs, deltaF, deltaH, weight },
		vertices = VertexList[ graph ];
		weight = vertex |-> If[ AssociationQ[ epsilon ], Lookup[ epsilon, vertex, 1 ], epsilon ];
		deltaF = AssociationMap[ vertex |-> Lookup[ f, X[ vertex ], 0 ] - Lookup[ f, vertex, 0 ], vertices ];
		deltaH = AssociationMap[ vertex |-> Lookup[ h, X[ vertex ], 0 ] - Lookup[ h, vertex, 0 ], vertices ];
		lhs = AssociationMap[
			vertex |-> Lookup[ f, X[ vertex ], 0 ] * Lookup[ h, X[ vertex ], 0 ] - Lookup[ f, vertex, 0 ] * Lookup[ h, vertex, 0 ],
			vertices
		];
		rhs = AssociationMap[
			vertex |-> Lookup[ f, vertex, 0 ] * deltaH[ vertex ] + deltaF[ vertex ] * Lookup[ h, vertex, 0 ] +
				weight[ vertex ] * deltaF[ vertex ] * deltaH[ vertex ],
			vertices
		];
		lhs === rhs
	]
