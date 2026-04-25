Package["WolframInstitute`InfraAnalysis`"]

PackageExport[RadialExpansion]
PackageExport[GraphBlowUp]
PackageExport[GraphContract]


(* --- RadialExpansion --- *)

Options[ RadialExpansion ] = Join[ Options[ Graph ], { "Connectivity" -> 1 } ];

RadialExpansion[ graph_Graph, layers_List, opts : OptionsPattern[ ] ] :=
	Module[
		{ sources, newVertices, newEdges, connectivity, graphOpts },

		connectivity = OptionValue[ "Connectivity" ];
		graphOpts = FilterRules[ { opts }, Options[ Graph ] ];
		sources = GraphSources[ graph ];

		newVertices = With[ { symbol = Unique[ "rb" ] },
			MapIndexed[ Table[ Indexed[ symbol, { First @ #2, i } ], { i, 1, #1 } ] &, layers ]
		];

		newEdges = (
			{ target, source } |-> With[ { nt = Length[ target ], ns = Length[ source ] },
				DirectedEdge[ source[[ First @ # ]], target[[ Last @ # ]] ] & /@
					Select[ Tuples[ { Range[ ns ], Range[ nt ] } ],
						Mod[ First[ # ] * nt - Last[ # ] * ns, nt * ns ] < connectivity * Max[ nt, ns ] &
					]
			]
		) @@@ Partition[ If[ Length @ sources > 0, Prepend[ #, sources ], # ] & @ newVertices, 2, 1 ];

		Graph[ VertexList @ graph, Join[ EdgeList @ graph, Flatten[ newEdges, 1 ] ], graphOpts ]
	]


(* --- GraphBlowUp --- *)

GraphBlowUp[ graph_Graph, n_Integer ] :=
	Module[ { vertices, fiberVertices, fiberEdges },
		vertices = VertexList[ graph ];
		fiberVertices = Flatten[ Table[ { v, i }, { v, vertices }, { i, n } ], 1 ];
		fiberEdges = Flatten[
			Table[ DirectedEdge[ { u, i }, { v, j } ], { DirectedEdge[ u, v ], EdgeList[ graph ] }, { i, n }, { j, n } ],
			2
		];
		Graph[ fiberVertices, fiberEdges,
			AnnotationRules -> Flatten @ Table[ { v, i } -> { "Origin" -> v }, { v, vertices }, { i, n } ]
		]
	]

GraphBlowUp[ graph_Graph, fiberSizes_Association ] :=
	Module[ { vertices, fiberVertices, fiberEdges },
		vertices = VertexList[ graph ];
		fiberVertices = Flatten[ Table[ { v, i }, { v, vertices }, { i, Lookup[ fiberSizes, v, 1 ] } ], 1 ];
		fiberEdges = Flatten[
			Table[
				DirectedEdge[ { u, i }, { v, j } ],
				{ DirectedEdge[ u, v ], EdgeList[ graph ] },
				{ i, Lookup[ fiberSizes, u, 1 ] },
				{ j, Lookup[ fiberSizes, v, 1 ] }
			],
			2
		];
		Graph[ fiberVertices, fiberEdges,
			AnnotationRules -> Flatten @ Table[ { v, i } -> { "Origin" -> v }, { v, vertices }, { i, Lookup[ fiberSizes, v, 1 ] } ]
		]
	]

GraphBlowUp[ graph_Graph, n_Integer, f_Association ] :=
	Module[ { bg, diffused },
		bg = GraphBlowUp[ graph, n ];
		diffused = Association @ Flatten @ Table[
			Table[ { v, i } -> Lookup[ f, v, 0 ] / n, { i, n } ],
			{ v, VertexList[ graph ] }
		];
		<| "Graph" -> bg, "Values" -> diffused |>
	]

GraphBlowUp[ graph_Graph, fiberSizes_Association, f_Association ] :=
	Module[ { bg, diffused },
		bg = GraphBlowUp[ graph, fiberSizes ];
		diffused = Association @ Flatten @ Table[
			With[ { n = Lookup[ fiberSizes, v, 1 ] },
				Table[ { v, i } -> Lookup[ f, v, 0 ] / n, { i, n } ]
			],
			{ v, VertexList[ graph ] }
		];
		<| "Graph" -> bg, "Values" -> diffused |>
	]


(* --- GraphContract --- *)

GraphContract[ graph_Graph ] :=
	Module[ { vertices, origins, originalVertices, contractedEdges },
		vertices = VertexList[ graph ];
		origins = AssociationMap[ v |-> AnnotationValue[ { graph, v }, "Origin" ], vertices ];
		originalVertices = DeleteDuplicates @ Values[ origins ];
		contractedEdges = DeleteDuplicates @ Map[
			edge |-> With[ { ou = origins[ edge[[ 1 ]] ], ov = origins[ edge[[ 2 ]] ] },
				If[ ou =!= ov, edge[[ 0 ]][ ou, ov ], Nothing ]
			],
			EdgeList[ graph ]
		];
		Graph[ originalVertices, contractedEdges ]
	]

GraphContract[ graph_Graph, f_Association ] :=
	Module[ { vertices, origins, originGroups, contractedValues },
		vertices = VertexList[ graph ];
		origins = AssociationMap[ v |-> AnnotationValue[ { graph, v }, "Origin" ], vertices ];
		originGroups = GroupBy[ Normal[ origins ], Last -> First ];
		contractedValues = Map[ Total[ Lookup[ f, #, 0 ] & /@ # ] &, originGroups ];
		<| "Graph" -> GraphContract[ graph ], "Values" -> contractedValues |>
	]
