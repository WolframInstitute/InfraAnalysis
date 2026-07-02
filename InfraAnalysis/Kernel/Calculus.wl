Package["WolframInstitute`InfraAnalysis`"]

PackageExport[GraphIntegrate]
PackageExport[GraphDerivative]
PackageExport[GraphIntegral]

PackageScope[GraphIntegrateValues]
PackageScope[GraphDerivativeValues]


Options[ GraphIntegrate ] = { Method -> "Ordered" };

GraphIntegrate[ graph_Graph, f_Association, opts : OptionsPattern[] ] :=
	AnnotateGraph[ graph, GraphIntegrateValues[ graph, f, OptionValue[ Method ] ] ]

GraphIntegrate[ graph_Graph, f_Association, vertex_, opts : OptionsPattern[] ] :=
	GraphIntegral[ graph, f, vertex ]

GraphIntegrate[ graph_Graph, f_Association, sources_List, sinks_List, opts : OptionsPattern[] ] :=
	GraphIntegral[ graph, f, sources, sinks ]

GraphIntegrate[ graph_Graph, opts : OptionsPattern[] ] :=
	GraphIntegrate[ graph, ExtractVertexValues[ graph ], opts ]


Options[ GraphDerivative ] = { Method -> "Ordered", "VectorField" -> None, "Weight" -> 1 };

GraphDerivative[ graph_Graph, f_Association, opts : OptionsPattern[] ] :=
	AnnotateGraph[ graph,
		GraphDerivativeValues[ graph, f,
			OptionValue[ Method ], OptionValue[ "VectorField" ], OptionValue[ "Weight" ] ]
	]

GraphDerivative[ graph_Graph, opts : OptionsPattern[] ] :=
	GraphDerivative[ graph, ExtractVertexValues[ graph ], opts ]


GraphIntegral[ graph_Graph, f_Association, vertex_ ] :=
	Total @ Lookup[ f, VertexInComponent[ graph, { vertex }, Infinity ] ]

GraphIntegral[ graph_Graph, f_Association, sources_List, sinks_List ] :=
	Total @ Lookup[ f, Intersection[ VertexOutComponent[ graph, sources, Infinity ], VertexInComponent[ graph, sinks, Infinity ] ] ]

GraphIntegral[ graph_Graph, f_Association, sources_List ] :=
	GraphIntegral[ graph, f, sources, GraphSinks[ graph ] ]


GraphIntegrateValues[ graph_Graph, f_Association, method_String ] :=
	Switch[ method,
		"Ordered",      orderedIntegration[ graph, f ],
		"Cumulative",   cumulativeIntegration[ graph, f ],
		"Conservative", conservativeIntegration[ graph, f ],
		"Laminar",      laminarIntegration[ graph, f ],
		_,              orderedIntegration[ graph, f ]
	]

GraphDerivativeValues[ graph_Graph, f_Association, method_String, vectorField_ : None, weight_ : 1 ] :=
	Switch[ method,
		"Ordered",      orderedDerivative[ graph, f ],
		"Cumulative",   GraphFiniteDifference[ graph, f ],
		"Conservative", conservativeDerivative[ graph, f ],
		"Laminar",      laminarDerivative[ graph, f ],
		"Directional",  GraphDirectionalDifference[ graph, vectorField, f ],
		"Weighted",     GraphWeightedDerivation[ graph, vectorField, f, weight ],
		_,              orderedDerivative[ graph, f ]
	]


orderedIntegration[ graph_Graph, f_Association ] :=
	AssociationMap[
		vertex |-> Total[ Lookup[ f, #, 0 ] & /@ VertexInComponent[ graph, vertex ] ],
		VertexList[ graph ]
	]

orderedDerivative[ graph_Graph, f_Association ] :=
	Module[ { ordering, n, successors, predecessors, mobius },
		ordering = TopologicalSort[ graph ];
		n = Length[ ordering ];
		successors = AssociationMap[
			v |-> Association[ Thread[ VertexOutComponent[ graph, v ] -> True ] ],
			ordering
		];
		predecessors = AssociationMap[
			v |-> Association[ Thread[ VertexInComponent[ graph, v ] -> True ] ],
			ordering
		];
		mobius = Association[];
		Do[
			With[ { vi = ordering[[ i ]] },
				mobius[ { vi, vi } ] = 1;
				Do[
					With[ { vj = ordering[[ j ]] },
						If[ TrueQ @ successors[ vi ][ vj ],
							mobius[ { vi, vj } ] = -Total[
								Lookup[ mobius, Key[ { vi, # } ], 0 ] & /@
									Select[ Keys @ predecessors[ vj ],
										z |-> z =!= vj && TrueQ @ successors[ vi ][ z ]
									]
							]
						]
					],
					{ j, i + 1, n }
				]
			],
			{ i, n }
		];
		AssociationMap[
			vertex |-> Total[
				( Lookup[ mobius, Key[ { #, vertex } ], 0 ] * Lookup[ f, #, 0 ] ) & /@
					VertexInComponent[ graph, vertex ]
			],
			VertexList[ graph ]
		]
	]


cumulativeIntegration[ graph_Graph, f_Association ] :=
	Fold[
		{ acc, vertex } |-> Append[ acc,
			vertex -> ( Lookup[ f, vertex, 0 ] + Total[ Lookup[ acc, #, 0 ] & /@ VertexInComponent[ graph, vertex, { 1 } ] ] )
		],
		Association[],
		TopologicalSort[ graph ]
	]

conservativeIntegration[ graph_Graph, f_Association ] :=
	Fold[
		{ acc, vertex } |-> Append[ acc,
			vertex -> ( Lookup[ f, vertex, 0 ] + Total[ Lookup[ acc, #, 0 ] / VertexOutDegree[ graph, # ] & /@ VertexInComponent[ graph, vertex, { 1 } ] ] )
		],
		Association[],
		TopologicalSort[ graph ]
	]

conservativeDerivative[ graph_Graph, f_Association ] :=
	AssociationMap[
		vertex |-> Lookup[ f, vertex, 0 ] - Total[ Lookup[ f, #, 0 ] / VertexOutDegree[ graph, # ] & /@ VertexInComponent[ graph, vertex, { 1 } ] ],
		VertexList[ graph ]
	]

laminarIntegration[ graph_Graph, f_Association ] :=
	laminarIntegration[ graph, f,
		LaminarDecomposition[ graph, "StructuredOutput" -> True ][ "LayeredVertices" ]
	]

laminarIntegration[ graph_Graph, f_Association, layers_Association ] :=
	Fold[
		{ acc, vertex } |-> Append[ acc,
			vertex -> ( Lookup[ f, vertex, 0 ] + Total[ Lookup[ acc, #, 0 ] & /@ Select[ VertexInComponent[ graph, vertex, { 1 } ], u |-> layers[ u ] =!= layers[ vertex ] ] ] )
		],
		Association[],
		TopologicalSort[ graph ]
	]

laminarDerivative[ graph_Graph, f_Association ] :=
	laminarDerivative[ graph, f,
		LaminarDecomposition[ graph, "StructuredOutput" -> True ][ "LayeredVertices" ]
	]

laminarDerivative[ graph_Graph, f_Association, layers_Association ] :=
	AssociationMap[
		vertex |-> Lookup[ f, vertex, 0 ] - Total[ Lookup[ f, #, 0 ] & /@ Select[ VertexInComponent[ graph, vertex, { 1 } ], u |-> layers[ u ] =!= layers[ vertex ] ] ],
		VertexList[ graph ]
	]
