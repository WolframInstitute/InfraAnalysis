Package["WolframInstitute`InfraAnalysis`"]

PackageExport[GraphIntegral]
PackageExport[GraphIntegration]
PackageExport[GraphIntegrationGeneral]
PackageExport[GraphMobiusFunction]
PackageExport[GraphZetaFunction]
PackageExport[MobiusInversionTheorem]
PackageExport[GraphDerivation]
PackageExport[GraphFiniteDifference]
PackageExport[GraphFundamentalTheorem]


(* ===== Integration ===== *)

(* Integral of f over predecessors of vertex a in g *)
GraphIntegral[ graph_Graph, f_Association, vertex_ ] :=
	Total @ Lookup[ f, VertexInComponent[ graph, { vertex }, Infinity ] ]

GraphIntegral[ graph_Graph, f_Association, sources_List, sinks_List ] :=
	Total @ Lookup[ f, VertexInComponent[ VertexOutComponent[ graph, sources, Infinity ], sinks, Infinity ] ]

GraphIntegral[ graph_Graph, f_Association, sources_List ] :=
	GraphIntegral[ graph, f, sources, GraphSinks[ graph ] ]

(* Cumulative integration along topological order *)
GraphIntegration[ graph_Graph ] :=
	GraphIntegration[ graph, Association @ AnnotationValue[ graph, VertexLabels ] ]

GraphIntegration[ graph_Graph, f_Association ] :=
	Module[
		{ vertices, ordering, values, coords, edgeStyle, vertexSize },

		vertices = VertexList[ graph ];
		ordering = TopologicalSort[ graph ];

		values = Fold[
			{ acc, vertex } |-> Append[ acc, vertex -> ( Lookup[ f, vertex, 0 ] + Total[ Lookup[ acc, #, 0 ] & /@ VertexInComponent[ graph, vertex, { 1 } ] ] ) ],
			Association[ ],
			ordering
		];

		coords = Association[ Thread[ vertices -> ( AnnotationValue[ { graph, # }, VertexCoordinates ] & /@ vertices ) ] ];
		edgeStyle = AnnotationValue[ graph, EdgeStyle ];
		vertexSize = AnnotationValue[ graph, VertexSize ];
		Graph[ vertices, EdgeList[ graph ],
			VertexLabels -> Normal[ values ],
			VertexCoordinates -> Normal[ coords ],
			EdgeStyle -> edgeStyle,
			VertexSize -> vertexSize,
			DirectedEdges -> True
		]
	]

(* General integration via full reachability *)
GraphIntegrationGeneral[ graph_Graph ] :=
	GraphIntegrationGeneral[ graph, AssociationThread[ VertexList[ graph ], AnnotationValue[ { graph, # }, VertexLabels ] & /@ VertexList[ graph ] ] ]

GraphIntegrationGeneral[ graph_Graph, f_Association ] :=
	Module[
		{ vertices, values, coords, edgeStyle, vertexSize },
		vertices = VertexList[ graph ];
		values = AssociationMap[
			vertex |-> Total[ Lookup[ f, #, 0 ] & /@ Select[ vertices, u |-> MemberQ[ VertexOutComponent[ graph, u ], vertex ] || u == vertex ] ],
			vertices
		];
		coords = Association[ Thread[ vertices -> ( AnnotationValue[ { graph, # }, VertexCoordinates ] & /@ vertices ) ] ];
		edgeStyle = AnnotationValue[ graph, EdgeStyle ];
		vertexSize = AnnotationValue[ graph, VertexSize ];
		Graph[ vertices, EdgeList[ graph ],
			VertexLabels -> Normal[ values ],
			VertexCoordinates -> Normal[ coords ],
			EdgeStyle -> edgeStyle,
			VertexSize -> vertexSize,
			DirectedEdges -> True
		]
	]

(* ===== Incidence Algebra ===== *)

GraphMobiusFunction[ graph_Graph ] :=
	Module[
		{ ordering, n, reachable, computeMobius },
		ordering = TopologicalSort[ graph ];
		n = Length[ ordering ];
		reachable = AssociationMap[
			i |-> ( Position[ ordering, # ][[ 1, 1 ]] & /@ VertexOutComponent[ graph, ordering[[ i ]] ] ),
			Range[ n ]
		];
		computeMobius = Fold[
			Function[ { mobius, i },
				Fold[
					Function[ { m, j },
						If[ j > i,
							Append[ m, { i, j } -> -Total[ m[[ Key[ { i, # } ] ]] & /@ Select[ Range[ i, j - 1 ], k |-> MemberQ[ reachable[ i ], k ] && MemberQ[ reachable[ k ], j ] ] ] ],
							m
						]
					],
					mobius,
					Sort[ reachable[ i ] ]
				]
			],
			Association[ Thread[ Table[ { i, i }, { i, n } ] -> 1 ] ],
			Range[ n ]
		];
		Association[ Flatten[ Table[ { ordering[[ i ]], ordering[[ j ]] } -> Lookup[ computeMobius, Key[ { i, j } ], 0 ], { i, n }, { j, n } ] ] ]
	]

GraphZetaFunction[ graph_Graph, f_Association ] :=
	AssociationMap[
		vertex |-> Total[ Lookup[ f, #, 0 ] & /@ VertexInComponent[ graph, vertex ] ],
		VertexList[ graph ]
	]

MobiusInversionTheorem[ graph_Graph, f_Association ] :=
	Module[
		{ mobius, vertices },
		mobius = GraphMobiusFunction[ graph ];
		vertices = VertexList[ graph ];
		AssociationMap[
			vertex |-> Total[ ( Lookup[ mobius, Key[ { #, vertex } ], 0 ] * Lookup[ f, #, 0 ] ) & /@ VertexInComponent[ graph, vertex ] ],
			vertices
		]
	]

(* ===== Derivative ===== *)

(* Non-local derivative via Moebius inversion *)
GraphDerivation[ graph_Graph, f_Association ] :=
	Module[
		{ mobius, vertices, values, coords, edgeStyle, vertexSize },
		mobius = GraphMobiusFunction[ graph ];
		vertices = VertexList[ graph ];
		values = AssociationMap[
			vertex |-> Total[ ( Lookup[ mobius, Key[ { #, vertex } ], 0 ] * Lookup[ f, #, 0 ] ) & /@ VertexInComponent[ graph, vertex ] ],
			vertices
		];
		coords = Association[ Thread[ vertices -> ( AnnotationValue[ { graph, # }, VertexCoordinates ] & /@ vertices ) ] ];
		edgeStyle = AnnotationValue[ graph, EdgeStyle ];
		vertexSize = AnnotationValue[ graph, VertexSize ];
		Graph[ vertices, EdgeList[ graph ],
			VertexLabels -> Normal[ values ],
			VertexCoordinates -> Normal[ coords ],
			EdgeStyle -> edgeStyle,
			VertexSize -> vertexSize,
			DirectedEdges -> True
		]
	]

GraphDerivation[ graph_Graph ] :=
	GraphDerivation[ graph, AssociationThread[ VertexList[ graph ], AnnotationValue[ { graph, # }, VertexLabels ] & /@ VertexList[ graph ] ] ]

(* Local finite difference *)
GraphFiniteDifference[ graph_Graph, f_Association ] :=
	Module[
		{ values, coords, edgeStyle, vertexSize },
		values = AssociationMap[
			vertex |-> Lookup[ f, vertex, 0 ] - Total[ Lookup[ f, #, 0 ] & /@ VertexInComponent[ graph, vertex, { 1 } ] ],
			VertexList[ graph ]
		];
		coords = Association[ Thread[ VertexList[ graph ] -> ( AnnotationValue[ { graph, # }, VertexCoordinates ] & /@ VertexList[ graph ] ) ] ];
		edgeStyle = AnnotationValue[ graph, EdgeStyle ];
		vertexSize = AnnotationValue[ graph, VertexSize ];
		Graph[ VertexList[ graph ], EdgeList[ graph ],
			VertexLabels -> Normal[ values ],
			VertexCoordinates -> Normal[ coords ],
			EdgeStyle -> edgeStyle,
			VertexSize -> vertexSize,
			DirectedEdges -> True
		]
	]

GraphFiniteDifference[ graph_Graph ] :=
	GraphFiniteDifference[ graph, AssociationThread[ VertexList[ graph ], AnnotationValue[ { graph, # }, VertexLabels ] & /@ VertexList[ graph ] ] ]

(* ===== Fundamental Theorem ===== *)

GraphFundamentalTheorem[ graph_Graph, f_Association ] :=
	Module[
		{ integratedVals, derivedVals },
		integratedVals = GraphZetaFunction[ graph, f ];
		derivedVals = MobiusInversionTheorem[ graph, integratedVals ];
		derivedVals === f
	]
