Package["WolframInstitute`InfraAnalysis`"]

PackageExport[LaminarDecomposition]
PackageExport[LayerDAGBySources]


Options[ LaminarDecomposition ] = {
	"Direction" -> Automatic,
	"StructuredOutput" -> False
};

LaminarDecomposition[ graph_Graph, opts : OptionsPattern[ ] ] :=
	Module[
		{ sources, root, layers, vertices, edges, structuredOutputOpt },

		structuredOutputOpt = OptionValue[ "StructuredOutput" ];
		sources = GraphSources[ graph ];
		If[ Length @ sources == 0, Return[ { }, Module ] ];

		root = Unique[ ];
		layers = <| |>;
		BreadthFirstScan[
			EdgeAdd[ VertexAdd[ graph, root ], DirectedEdge[ root, # ] & /@ sources ],
			root,
			{ "DiscoverVertex" -> ( { vertex, parent, depth } |-> If[ !KeyExistsQ[ layers, vertex ], layers[ vertex ] = depth ] ) }
		];

		vertices = Values @ GroupBy[ Normal @ KeyDrop[ layers, root ], Last -> First ];
		edges = GroupBy[ EdgeList[ graph ], Map[ layers ] ];

		If[ structuredOutputOpt,
			<|
				"LayeredVertices" -> KeyDrop[ layers, root ],
				"LayeredEdges" -> edges
			|>,
			Prepend[
				Thread[ Graph[ vertices, Array[ Lookup[ edges, DirectedEdge[ #, # ], { } ] &, Length @ vertices ] ] ],
				Graph[ Catenate @ vertices, Flatten[ Values @ KeySelect[ edges, First @ # != Last @ # & ], 1 ] ]
			]
		]
	]

LayerDAGBySources[ graph_Graph ] :=
	Module[
		{ sources, distances },
		sources = GraphSources[ graph ];
		If[ Length[ sources ] == 0, Return[ <| 0 -> VertexList[ graph ] |> ] ];
		distances = Association @ Map[
			vertex |-> vertex -> Min[ GraphDistance[ graph, #, vertex ] & /@ sources ],
			VertexList[ graph ]
		];
		GroupBy[ Normal[ distances ], Last -> First ]
	]
