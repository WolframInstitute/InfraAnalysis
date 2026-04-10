Package["WolframInstitute`InfraAnalysis`"]

PackageExport[RadialExpansion]
PackageExport[RadialBlowUp]
PackageExport[LaminarBlowUp]
PackageExport[LaminarExpansion]
PackageExport[Graph3DUnion]
PackageExport[BlowUpGraph]
PackageExport[ContractGraph]


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

(* --- RadialBlowUp --- *)

Options[ RadialBlowUp ] = {
	"Distribution" -> "Uniform",
	"Connectivity" -> "Medium",
	"IntraConnectivity" -> "NearestNeighbor"
};

RadialBlowUp[ graph_Graph, layerCounts_List, opts : OptionsPattern[ ] ] :=
	Module[
		{ distribution, connectivity, intraConnectivity, connectivityFactor, sources,
		  n, layers, allVertices, totalValue, totalVertices, allLabels, allEdges, radialDistances },

		distribution = OptionValue[ "Distribution" ];
		connectivity = OptionValue[ "Connectivity" ];
		intraConnectivity = OptionValue[ "IntraConnectivity" ];
		connectivityFactor = Switch[ connectivity, "Low", 0.3, "Medium", 0.6, "High", 0.9, _, 0.6 ];

		sources = GraphSources[ graph ];
		totalValue = Total[ AnnotationValue[ { graph, # }, VertexLabels ] & /@ VertexList[ graph ] /. Missing[ _ ] -> 0 ];
		n = Length[ layerCounts ];

		layers = Table[ Table[ { "Layer", i, j }, { j, layerCounts[[ i ]] } ], { i, n } ];
		allVertices = Join[ sources, Flatten[ layers ] ];
		totalVertices = Length[ allVertices ];

		radialDistances = Association[
			Sequence @@ Thread[ sources -> ConstantArray[ 0, Length[ sources ] ] ],
			Sequence @@ Flatten @ Table[
				Table[ layers[[ i, j ]] -> n - i + 1, { j, layerCounts[[ i ]] } ],
				{ i, n }
			]
		];

		allLabels = Association[
			Sequence @@ Table[
				sink -> Switch[ distribution,
					"Gaussian", totalValue / Length[ sources ],
					"Uniform", totalValue / totalVertices,
					"Random", RandomReal[ { 0, totalValue } ],
					_, totalValue / Length[ sources ]
				],
				{ sink, sources }
			],
			Sequence @@ Flatten @ Table[
				Module[ { dist, value },
					dist = ( n - i + 1 ) / n;
					value = Switch[ distribution,
						"Gaussian", ( totalValue / totalVertices ) * Exp[ -( 1 - dist )^2 / 0.2 ],
						"Uniform", totalValue / totalVertices,
						"Random", RandomReal[ { 0, totalValue / n } ],
						_, 0
					];
					Table[ layers[[ i, j ]] -> value, { j, layerCounts[[ i ]] } ]
				],
				{ i, n }
			]
		];

		allEdges = Join[
			Which[
				intraConnectivity == "None", { },
				intraConnectivity == "Full",
					Flatten @ Table[
						Flatten @ Table[
							DirectedEdge[ layers[[ i, j ]], layers[[ i, k ]] ],
							{ j, layerCounts[[ i ]] }, { k, layerCounts[[ i ]] }
						],
						{ i, n }
					],
				True,
					Flatten @ Table[
						Table[
							UndirectedEdge[ layers[[ i, j ]], layers[[ i, Mod[ j, layerCounts[[ i ]] ] + 1 ]] ],
							{ j, layerCounts[[ i ]] }
						],
						{ i, n }
					]
			],
			Flatten @ Table[
				Module[ { outer, inner, guaranteed, extra },
					outer = layers[[ i ]];
					inner = If[ i == n, sources, layers[[ i + 1 ]] ];
					guaranteed = Table[ DirectedEdge[ vertex, RandomChoice[ inner ] ], { vertex, outer } ];
					extra = Table[
						DirectedEdge[ RandomChoice[ outer ], RandomChoice[ inner ] ],
						{ Round[ connectivityFactor * Length[ outer ] * Length[ inner ] ] }
					];
					DeleteDuplicates @ Join[ guaranteed, extra ]
				],
				{ i, n }
			]
		];

		Module[ { result },
			result = Graph[ allVertices, allEdges, VertexLabels -> allLabels ];
			Scan[ vertex |-> ( result = Annotate[ result, { vertex, "RadialDistance" -> radialDistances[ vertex ] } ] ), allVertices ];
			result
		]
	]

(* --- LaminarBlowUp --- *)

Options[ LaminarBlowUp ] = Join[ Options[ RadialBlowUp ], Options[ Graph ] ];

LaminarBlowUp[ graph_Graph, layerCounts_, opts : OptionsPattern[ ] ] :=
	Module[
		{ sinks, layersBySources, blownUpLayers, allVertices, allLabels, allEdges,
		  connectivity, connectivityFactor, laminarDistances, radialDistances },

		connectivity = OptionValue[ "Connectivity" ];
		connectivityFactor = Switch[ connectivity, "Low", 0.3, "Medium", 0.6, "High", 0.9, _, 0.6 ];
		sinks = GraphSinks[ graph ];
		layersBySources = LayerDAGBySources[ graph ];

		blownUpLayers = KeyValueMap[
			{ dist, verts } |-> Module[ { subgraph },
				subgraph = Subgraph[ graph, verts ];
				RadialBlowUp[ subgraph, layerCounts, FilterRules[ { opts }, Options[ RadialBlowUp ] ] ]
			],
			layersBySources
		];

		allVertices = Flatten[ VertexList /@ blownUpLayers ];
		allLabels = Join @@ ( AnnotationValue[ #, VertexLabels ] & /@ blownUpLayers );

		laminarDistances = Association @ Flatten @ KeyValueMap[
			{ dist, blownUpGraph } |->
				Table[ vertex -> dist, { vertex, VertexList[ blownUpGraph ] } ],
			AssociationThread[ Keys[ layersBySources ], blownUpLayers ]
		];

		radialDistances = Association @ Flatten @ Map[
			blownUpGraph |->
				Table[ vertex -> AnnotationValue[ { blownUpGraph, vertex }, "RadialDistance" ], { vertex, VertexList[ blownUpGraph ] } ],
			blownUpLayers
		];

		allEdges = Join[
			Flatten[ EdgeList /@ blownUpLayers ],
			Flatten @ Map[
				edge |-> Module[ { fromV, toV, fromDist, toDist, fromLayer, toLayer },
					{ fromV, toV } = List @@ edge;
					fromDist = laminarDistances[ fromV ];
					toDist = laminarDistances[ toV ];
					If[ fromDist > toDist,
						fromLayer = Select[ allVertices, MatchQ[ #, { fromV, ___ } ] & ];
						toLayer = Select[ allVertices, MatchQ[ #, { toV, ___ } ] & ];
						Join[
							Table[ DirectedEdge[ RandomChoice[ fromLayer ], RandomChoice[ toLayer ] ], { 3 } ],
							Table[ DirectedEdge[ RandomChoice[ fromLayer ], RandomChoice[ toLayer ] ],
								{ Round[ connectivityFactor * Length[ fromLayer ] * Length[ toLayer ] / 20 ] } ]
						],
						{ }
					]
				],
				EdgeList[ graph ]
			]
		];

		Module[ { result },
			result = Graph[ allVertices, DeleteDuplicates[ allEdges ],
				VertexLabels -> allLabels,
				FilterRules[ { opts }, Options[ Graph ] ]
			];
			Scan[ vertex |-> (
				result = Annotate[ result, { vertex, "RadialDistance" -> Lookup[ radialDistances, vertex, Missing[ ] ] } ];
				result = Annotate[ result, { vertex, "LaminarDistance" -> Lookup[ laminarDistances, vertex, Missing[ ] ] } ]
			), allVertices ];
			result
		]
	]

(* --- LaminarExpansion --- *)

Options[ LaminarExpansion ] = {
	"RadialConnectivity" -> 1,
	"LaminarConnectivity" -> 1,
	"StructuredOutput" -> False
};

LaminarExpansion[ graph_Graph, radialLayers : { __List }, opts : OptionsPattern[ ] ] :=
	Module[
		{ decomposition, laminarGraph, radialGraphs, radialBlowUps, getLayer, getPosition,
		  newVertices, laminarEdges, radialConnectivity, laminarConnectivity },

		radialConnectivity = OptionValue[ "RadialConnectivity" ];
		laminarConnectivity = OptionValue[ "LaminarConnectivity" ];
		decomposition = LaminarDecomposition[ graph ];
		If[ Length @ decomposition < 2, Return[ graph, Module ] ];

		laminarGraph = decomposition[[ 1 ]];
		radialGraphs = decomposition[[ 2 ;; ]];

		radialBlowUps = Thread[ RadialExpansion[ radialGraphs,
			PadRight[ radialLayers, Length @ radialGraphs, radialLayers ],
			"Connectivity" -> radialConnectivity, GraphLayout -> "RadialEmbedding"
		] ];

		getLayer[ Indexed[ _, { i_, _ } ] ] := i;
		getPosition[ Indexed[ _, { _, j_ } ] ] := j;

		newVertices = Map[ SortBy[ #, getPosition ] &,
			GatherBy[
				Cases[ VertexList[ # ], ( v : Indexed[ sym_Symbol, { _, _ } ] /; StringStartsQ[ SymbolName[ sym ], "rb" ] ) :> v ],
				getLayer
			] & /@ radialBlowUps,
			{ 2 }
		];

		laminarEdges = (
			{ ls, lt } |-> With[ { nlt = Length[ lt ], nls = Length[ ls ] },
				(
					{ source, target } |-> With[ { nt = Length[ target ], ns = Length[ source ] },
						DirectedEdge[ source[[ First @ # ]], target[[ Last @ # ]] ] & /@
							Select[ Tuples[ { Range[ ns ], Range[ nt ] } ],
								Mod[ First[ # ] * nt - Last[ # ] * ns, nt * ns ] < laminarConnectivity * Max[ nt, ns ] &
							]
					]
				) @@@ ( Transpose @ { Take[ ls, Min[ nlt, nls ] ], Take[ lt, Min[ nlt, nls ] ] } )
			]
		) @@@ Partition[ newVertices, 2, 1 ];

		HighlightGraph[
			Graph3D[
				EdgeAdd[ EdgeAdd[ Graph3DUnion[ radialBlowUps ], EdgeList[ laminarGraph ] ], Flatten[ laminarEdges, 2 ] ],
				EdgeStyle -> Thread[ Flatten[ laminarEdges, 2 ] -> Yellow ]
			],
			graph
		]
	]

Graph3DUnion[ gs : { graph_Graph, ___Graph } ] :=
	Graph3D[
		Catenate[ VertexList /@ gs ],
		Catenate[ EdgeList /@ gs ],
		VertexCoordinates -> Catenate @ MapIndexed[
			Append[ -#2[[ 1 ]] ] /@ If[ VertexCount[ # ] > 1, Standardize[ #, Mean, 1 & ] &, Identity ][ GraphEmbedding[ #1 ] ] &,
			gs
		],
		VertexStyle -> AnnotationValue[ graph, EdgeStyle ],
		EdgeStyle -> AnnotationValue[ graph, EdgeStyle ]
	]

(* --- BlowUpGraph --- *)

Options[ BlowUpGraph ] = {
	"Connectivity" -> "Full",
	"PreserveGraphDistance" -> False
};

BlowUpGraph[ graph_Graph, blowupRules_Association, opts : OptionsPattern[ ] ] :=
	Module[
		{ connectivity, preserveDistance, isDirected, gDim, normalizeDim, processVertex,
		  processEdge, vertexData, vertexMapping, vertexCoords, vertexLabels,
		  vertexOrigins, vertexDistances, subEdges, originalEdges, newEdges, edgeLabels },

		connectivity = OptionValue[ "Connectivity" ];
		preserveDistance = OptionValue[ "PreserveGraphDistance" ];
		isDirected = DirectedGraphQ[ graph ];

		gDim = Module[ { sampleCoord },
			sampleCoord = AnnotationValue[ { graph, First[ VertexList[ graph ] ] }, VertexCoordinates ];
			If[ MissingQ[ sampleCoord ], 3, Length[ sampleCoord ] ]
		];

		normalizeDim = { coord, targetDim } |-> If[ MissingQ[ coord ],
			ConstantArray[ 0, targetDim ],
			Which[
				Length[ coord ] == targetDim, coord,
				Length[ coord ] < targetDim, PadRight[ coord, targetDim, 0 ],
				True, Take[ coord, targetDim ]
			]
		];

		processVertex = { state, vertex } |-> If[ KeyExistsQ[ blowupRules, vertex ],
			Module[
				{ blowup, subgraph, rootVertex, scale, subVertices, origCoord, subCoords,
				  hasSubCoords, rootCoord, subLabels, nextID, mapping, coords, labels },

				blowup = blowupRules[ vertex ];
				subgraph = blowup[ "Subgraph" ];
				rootVertex = blowup[ "Root" ];
				scale = Lookup[ blowup, "Scale", 1.0 ];
				subVertices = VertexList[ subgraph ];
				hasSubCoords = !MissingQ[ AnnotationValue[ { subgraph, First[ subVertices ] }, VertexCoordinates ] ];

				subCoords = If[ hasSubCoords,
					AssociationThread[ subVertices, normalizeDim[ AnnotationValue[ { subgraph, # }, VertexCoordinates ], gDim ] & /@ subVertices ],
					Module[ { numPoints, positions },
						origCoord = normalizeDim[ AnnotationValue[ { graph, vertex }, VertexCoordinates ], gDim ];
						numPoints = Length[ subVertices ];
						positions = If[ numPoints == 1,
							{ origCoord },
							Which[
								gDim == 2,
									Table[ origCoord + scale * { Cos[ 2 Pi i / numPoints ], Sin[ 2 Pi i / numPoints ] }, { i, 0, numPoints - 1 } ],
								gDim == 3,
									Module[ { phi, theta },
										Table[
											phi = Pi * ( 3 - Sqrt[ 5 ] ) * i;
											theta = ArcCos[ 1 - 2 ( i + 0.5 ) / numPoints ];
											origCoord + scale * { Sin[ theta ] Cos[ phi ], Sin[ theta ] Sin[ phi ], Cos[ theta ] },
											{ i, 0, numPoints - 1 }
										]
									],
								True,
									Table[ origCoord + scale * RandomReal[ { -1, 1 }, gDim ], { numPoints } ]
							]
						];
						AssociationThread[ subVertices, positions ]
					]
				];

				rootCoord = Lookup[ subCoords, rootVertex, ConstantArray[ 0, gDim ] ];
				subLabels = AssociationThread[ subVertices, AnnotationValue[ { subgraph, # }, VertexLabels ] & /@ subVertices ];
				nextID = state[ "nextID" ];
				mapping = AssociationThread[ subVertices, Table[ If[ sv === rootVertex, vertex, nextID++ ], { sv, subVertices } ] ];
				coords = Association @ Table[ mapping[ sv ] -> subCoords[ sv ], { sv, subVertices } ];
				labels = Association @ KeyValueMap[ #1 -> #2 &, KeySelect[ AssociationThread[ mapping /@ subVertices, subLabels /@ subVertices ], !MissingQ ] ];

				<|
					"nextID" -> nextID,
					"mapping" -> Join[ state[ "mapping" ], mapping ],
					"coords" -> Join[ state[ "coords" ], coords ],
					"labels" -> Join[ state[ "labels" ], labels ],
					"origins" -> Join[ state[ "origins" ], AssociationThread[ Values[ mapping ], vertex ] ],
					"subEdges" -> Join[ state[ "subEdges" ], { vertex -> EdgeList[ subgraph ] } ]
				|>
			],
			Module[ { origCoord, origLabel },
				origCoord = AnnotationValue[ { graph, vertex }, VertexCoordinates ];
				origLabel = AnnotationValue[ { graph, vertex }, VertexLabels ];
				<|
					"nextID" -> state[ "nextID" ],
					"mapping" -> Join[ state[ "mapping" ], <| vertex -> vertex |> ],
					"coords" -> If[ MissingQ[ origCoord ], state[ "coords" ], Join[ state[ "coords" ], <| vertex -> origCoord |> ] ],
					"labels" -> If[ MissingQ[ origLabel ], state[ "labels" ], Join[ state[ "labels" ], <| vertex -> origLabel |> ] ],
					"origins" -> Join[ state[ "origins" ], <| vertex -> vertex |> ],
					"subEdges" -> state[ "subEdges" ]
				|>
			]
		];

		vertexData = Fold[ processVertex,
			<| "nextID" -> Max[ VertexList[ graph ] ] + 1, "mapping" -> <| |>, "coords" -> <| |>, "labels" -> <| |>, "origins" -> <| |>, "subEdges" -> <| |> |>,
			VertexList[ graph ]
		];

		vertexMapping = vertexData[ "mapping" ];
		vertexCoords = vertexData[ "coords" ];
		vertexLabels = vertexData[ "labels" ];
		vertexOrigins = vertexData[ "origins" ];

		vertexDistances = If[ preserveDistance,
			Association @ Flatten @ KeyValueMap[
				{ origV, layer } |-> Module[ { subgraph, rootVertex, distances },
					If[ KeyExistsQ[ blowupRules, origV ],
						subgraph = blowupRules[ origV ][ "Subgraph" ];
						rootVertex = blowupRules[ origV ][ "Root" ];
						distances = GraphDistance[ subgraph, rootVertex, # ] & /@ VertexList[ subgraph ];
						Thread[ ( vertexMapping /@ VertexList[ subgraph ] ) -> distances ],
						{ vertexMapping[ origV ] -> 0 }
					]
				],
				vertexData[ "mapping" ]
			],
			<| |>
		];

		subEdges = Flatten @ KeyValueMap[
			{ vertex, edges } |-> Map[
				edge |-> Module[ { fromV, toV, edgeConstructor },
					{ fromV, toV } = If[ DirectedGraphQ[ blowupRules[ vertex ][ "Subgraph" ] ], { edge[[ 1 ]], edge[[ 2 ]] }, List @@ edge ];
					edgeConstructor = If[ DirectedGraphQ[ blowupRules[ vertex ][ "Subgraph" ] ], DirectedEdge, UndirectedEdge ];
					edgeConstructor[ vertexMapping[ fromV ], vertexMapping[ toV ] ] -> AnnotationValue[ { blowupRules[ vertex ][ "Subgraph" ], edge }, EdgeLabels ]
				],
				edges
			],
			vertexData[ "subEdges" ]
		];

		processEdge = edge |-> Module[
			{ fromV, toV, fromLayer, toLayer, connectivityFactor, newEdgeSet, label },

			{ fromV, toV } = If[ isDirected, { edge[[ 1 ]], edge[[ 2 ]] }, List @@ edge ];
			fromLayer = If[ KeyExistsQ[ blowupRules, fromV ], Values[ KeySelect[ vertexMapping, MemberQ[ VertexList[ blowupRules[ fromV ][ "Subgraph" ] ], # ] & ] ], { fromV } ];
			toLayer = If[ KeyExistsQ[ blowupRules, toV ], Values[ KeySelect[ vertexMapping, MemberQ[ VertexList[ blowupRules[ toV ][ "Subgraph" ] ], # ] & ] ], { toV } ];
			connectivityFactor = Switch[ connectivity, "Full", 1.0, "Low", 0.3, "Medium", 0.6, "High", 0.9, _, 1.0 ];
			label = AnnotationValue[ { graph, edge }, EdgeLabels ];

			newEdgeSet = If[ preserveDistance,
				Flatten[ Table[
					Module[ { fDist, candidates },
						fDist = Lookup[ vertexDistances, fv, 0 ];
						candidates = Select[ toLayer, Lookup[ vertexDistances, #, 0 ] == fDist & ];
						If[ Length[ candidates ] > 0,
							If[ connectivity === "Full",
								If[ isDirected, DirectedEdge, UndirectedEdge ][ fv, # ] & /@ candidates,
								{ If[ isDirected, DirectedEdge, UndirectedEdge ][ fv, RandomChoice[ candidates ] ] }
							],
							{ }
						]
					],
					{ fv, fromLayer }
				] ],
				If[ connectivity === "Full",
					Flatten[ Outer[ If[ isDirected, DirectedEdge, UndirectedEdge ], fromLayer, toLayer, 1 ] ],
					Join[
						Table[ If[ isDirected, DirectedEdge, UndirectedEdge ][ fromLayer[[ Mod[ i - 1, Length[ fromLayer ] ] + 1 ]], toLayer[[ i ]] ], { i, Length[ toLayer ] } ],
						DeleteDuplicates[ Table[ If[ isDirected, DirectedEdge, UndirectedEdge ][ RandomChoice[ fromLayer ], RandomChoice[ toLayer ] ], { Round[ connectivityFactor * Length[ fromLayer ] * Length[ toLayer ] ] } ] ]
					]
				]
			];
			Thread[ newEdgeSet -> If[ MissingQ[ label ], Missing[ ], label ] ]
		];

		originalEdges = Flatten[ processEdge /@ EdgeList[ graph ], 1 ];
		newEdges = Join[ subEdges, originalEdges ];
		edgeLabels = Association @ Select[ newEdges, !MissingQ[ #[[ 2 ]] ] & ];

		Graph[ Keys[ vertexCoords ], Keys[ newEdges ],
			DirectedEdges -> isDirected,
			VertexCoordinates -> Normal[ vertexCoords ],
			VertexLabels -> Normal[ vertexLabels ],
			EdgeLabels -> Normal[ edgeLabels ],
			AnnotationRules -> Table[ vertex -> { "Origin" -> vertexOrigins[ vertex ] }, { vertex, Keys[ vertexOrigins ] } ],
			VertexSize -> Medium,
			EdgeStyle -> Directive[ Arrowheads[ 0.02 ] ]
		]
	]

(* --- ContractGraph --- *)

ContractGraph[ graph_Graph ] :=
	Module[
		{ vertices, origins, originalVertices, labels, isDirected,
		  contractedCoords, contractedLabels, contractedEdges },

		vertices = VertexList[ graph ];
		origins = Association @ Table[ vertex -> AnnotationValue[ graph, vertex, "Origin" ], { vertex, vertices } ];
		originalVertices = DeleteDuplicates @ Values[ origins ];
		isDirected = DirectedGraphQ[ graph ];

		contractedCoords = Association @ Table[
			ov -> Mean[ Keys[ Select[ origins, # == ov & ] ] /. Thread[ vertices -> ( AnnotationValue[ { graph, # }, VertexCoordinates ] & /@ vertices ) ] ],
			{ ov, originalVertices }
		];

		labels = Association @ Table[ vertex -> AnnotationValue[ { graph, vertex }, VertexLabels ], { vertex, vertices } ];
		contractedLabels = Association @ Table[
			ov -> Total @ Values[ Select[ labels, !MissingQ[ # ] &, KeySelect[ origins, # == ov & ] ] ],
			{ ov, originalVertices }
		];

		contractedEdges = DeleteDuplicates @ Map[
			edge |-> Module[ { fromV, toV, fromOrigin, toOrigin },
				{ fromV, toV } = If[ isDirected, { edge[[ 1 ]], edge[[ 2 ]] }, List @@ edge ];
				fromOrigin = origins[ fromV ];
				toOrigin = origins[ toV ];
				If[ fromOrigin =!= toOrigin,
					If[ isDirected, DirectedEdge[ fromOrigin, toOrigin ], UndirectedEdge[ fromOrigin, toOrigin ] ],
					Nothing
				]
			],
			EdgeList[ graph ]
		];

		Graph[ originalVertices, contractedEdges,
			DirectedEdges -> isDirected,
			VertexCoordinates -> Normal[ contractedCoords ],
			VertexLabels -> Normal[ contractedLabels ]
		]
	]
