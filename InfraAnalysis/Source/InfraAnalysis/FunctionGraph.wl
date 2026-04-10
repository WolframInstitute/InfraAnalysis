Options[ FunctionGraph ] = {
	"EdgeLengthScaling" -> "Proportional",
	"MinimalEdgeLength" -> None
};

FunctionGraph[ f_, pointSpec_, opts : OptionsPattern[ ] ] :=
	Module[
		{ n, vertices, edges, edgeLengthScaling, minEdgeLength, xVals, fVals,
		  edgeLengths, displayedLengths, minLen, scaleFactor, coords },

		xVals = Which[
			MatchQ[ pointSpec, { { _?NumericQ .. } } ],
				First[ pointSpec ],
			MatchQ[ pointSpec, { _?NumericQ, _?NumericQ, _?NumericQ } ],
				Range[ pointSpec[[ 1 ]], pointSpec[[ 2 ]], pointSpec[[ 3 ]] ],
			MatchQ[ pointSpec, { _?NumericQ, _?NumericQ } ],
				Range[ pointSpec[[ 1 ]], pointSpec[[ 2 ]], 1 ],
			True,
				Range[ 0, pointSpec ]
		];

		n = Length[ xVals ] - 1;
		fVals = f /@ xVals;
		vertices = Range[ Length[ xVals ] ];
		edges = DirectedEdge @@@ Partition[ vertices, 2, 1 ];
		edgeLengths = Differences[ xVals ];

		edgeLengthScaling = OptionValue[ "EdgeLengthScaling" ];
		minEdgeLength = OptionValue[ "MinimalEdgeLength" ];

		displayedLengths = Which[
			edgeLengthScaling == "Proportional", edgeLengths,
			edgeLengthScaling == "Logarithmic", Log /@ edgeLengths,
			True, ConstantArray[ 1, Length[ edgeLengths ] ]
		];

		scaleFactor = If[ NumericQ[ minEdgeLength ],
			minLen = Min[ displayedLengths ];
			If[ minLen != 0, minEdgeLength / minLen, 1 ],
			1
		];

		coords = FoldList[ { #1[[ 1 ]] + #2, 0 } &, { 0, 0 }, scaleFactor * displayedLengths ];
		Graph[ vertices, edges,
			VertexCoordinates -> Thread[ vertices -> coords ],
			VertexLabels -> Thread[ vertices -> ( Placed[ #, Above ] & /@ fVals ) ],
			EdgeLabels -> Thread[ edges -> ( Placed[ #, { 1/2, { 0, 0 } } ] & /@ edgeLengths ) ],
			EdgeStyle -> Directive[ Arrowheads[ 0.02 ] ],
			AnnotationRules -> Table[
				vertices[[ i ]] -> { "Point" -> Point[ { xVals[[ i ]], fVals[[ i ]] } ] },
				{ i, Length[ vertices ] }
			]
		]
	]

Options[ ThickFunctionGraph ] = Join[
	Options[ FunctionGraph ],
	Options[ RadialBlowUp ],
	{ "DiskStructure" -> "Disjoint" }
];

ThickFunctionGraph[ graph_Graph, diskSpec_, opts : OptionsPattern[ ] ] :=
	Module[
		{ connectivity, distribution, diskStructure, originalVertices, originalEdges,
		  n, diskSizes, vertexLayers, newVertices, newEdges, vertexCoords, vertexLabels, xCoords },

		connectivity = OptionValue[ "Connectivity" ];
		distribution = OptionValue[ "Distribution" ];
		diskStructure = OptionValue[ "DiskStructure" ];

		originalVertices = VertexList[ graph ];
		originalEdges = EdgeList[ graph ];
		n = Length[ originalVertices ];

		diskSizes = Which[
			MatchQ[ diskSpec, { { _?IntegerQ .. } } ], First[ diskSpec ],
			MatchQ[ diskSpec, { _?IntegerQ, _?IntegerQ } ], Table[ RandomInteger[ diskSpec ], n ],
			IntegerQ[ diskSpec ], ConstantArray[ diskSpec, n ],
			True, diskSpec
		];

		xCoords = AnnotationValue[ { graph, # }, VertexCoordinates ] & /@ originalVertices;

		vertexLayers = If[ diskStructure === "RadialDAG",
			AssociationThread[ originalVertices, MapIndexed[
				{ vertex, idx } |-> Module[
					{ numPoints, pointData, fValue, totalValue, xPos, singleVertexGraph,
					  blownUp, blownUpVertices, blownUpCoords },
					numPoints = diskSizes[[ First[ idx ] ]];
					xPos = xCoords[[ First[ idx ], 1 ]];
					pointData = AnnotationValue[ graph, vertex, "Point" ];
					totalValue = If[ MissingQ[ pointData ],
						fValue = AnnotationValue[ { graph, vertex }, VertexLabels ];
						If[ MissingQ[ fValue ], vertex, fValue ],
						pointData[[ 2 ]]
					];
					singleVertexGraph = Graph[ { vertex }, { },
						VertexCoordinates -> { vertex -> { xPos, 0, 0 } },
						VertexLabels -> { vertex -> totalValue }
					];
					blownUp = RadialBlowUp[ singleVertexGraph,
						Table[ Max[ 1, Round[ numPoints * ( 1 - i / 4 ) ] ], { i, 0, 2 } ],
						FilterRules[ { opts }, Options[ RadialBlowUp ] ]
					];
					blownUpVertices = VertexList[ blownUp ];
					blownUpCoords = AnnotationValue[ blownUp, VertexCoordinates ];
					<|
						"Vertices" -> blownUpVertices,
						"Coords" -> blownUpCoords,
						"Labels" -> Thread[ blownUpVertices -> ( Placed[ N[ AnnotationValue[ { blownUp, # }, VertexLabels ] ], Center ] & /@ blownUpVertices ) ],
						"Edges" -> EdgeList[ blownUp ]
					|>
				],
				originalVertices
			] ],
			AssociationThread[ originalVertices, MapIndexed[
				{ vertex, idx } |-> Module[
					{ numPoints, pointData, fValue, totalValue, weights, xPos, layerVertices, layerCoords, layerLabels },
					numPoints = diskSizes[[ First[ idx ] ]];
					xPos = xCoords[[ First[ idx ], 1 ]];
					pointData = AnnotationValue[ graph, vertex, "Point" ];
					totalValue = If[ MissingQ[ pointData ],
						fValue = AnnotationValue[ { graph, vertex }, VertexLabels ];
						If[ MissingQ[ fValue ], vertex, fValue ],
						pointData[[ 2 ]]
					];
					weights = Which[
						distribution == "Gaussian", ConstantArray[ totalValue / numPoints, numPoints ],
						distribution == "Random", RandomReal[ { 0, totalValue }, numPoints ],
						True, ConstantArray[ totalValue / numPoints, numPoints ]
					];
					layerVertices = Table[ { vertex, i }, { i, numPoints } ];
					layerCoords = If[ numPoints == 1,
						{ { xPos, 0, 0 } },
						Table[ { xPos, 0.5 Cos[ 2 Pi i / numPoints ], 0.5 Sin[ 2 Pi i / numPoints ] }, { i, 0, numPoints - 1 } ]
					];
					layerLabels = Thread[ layerVertices -> ( Placed[ N[ # ], Center ] & /@ weights ) ];
					<| "Vertices" -> layerVertices, "Coords" -> Thread[ layerVertices -> layerCoords ], "Labels" -> layerLabels, "Edges" -> { } |>
				],
				originalVertices
			] ]
		];

		newVertices = Flatten[ #[ "Vertices" ] & /@ Values[ vertexLayers ] ];
		vertexCoords = Join @@ ( #[ "Coords" ] & /@ Values[ vertexLayers ] );
		vertexLabels = Join @@ ( #[ "Labels" ] & /@ Values[ vertexLayers ] );

		newEdges = Join[
			Flatten[ #[ "Edges" ] & /@ Values[ vertexLayers ] ],
			Flatten @ Map[
				edge |-> Module[ { fromV, toV, fromLayer, toLayer, connectivityFactor },
					{ fromV, toV } = { edge[[ 1 ]], edge[[ 2 ]] };
					fromLayer = vertexLayers[ fromV ][ "Vertices" ];
					toLayer = vertexLayers[ toV ][ "Vertices" ];
					connectivityFactor = Switch[ connectivity, "Full", 1.0, "Low", 0.3, "Medium", 0.6, "High", 0.9, _, 1.0 ];
					If[ connectivity === "Full",
						Flatten[ Outer[ UndirectedEdge, fromLayer, toLayer, 1 ] ],
						Join[
							Table[ UndirectedEdge[ fromLayer[[ Mod[ i - 1, Length[ fromLayer ] ] + 1 ]], toLayer[[ i ]] ], { i, Length[ toLayer ] } ],
							DeleteDuplicates[ Table[ UndirectedEdge[ RandomChoice[ fromLayer ], RandomChoice[ toLayer ] ], { Round[ connectivityFactor * Length[ fromLayer ] * Length[ toLayer ] ] } ] ]
						]
					]
				],
				originalEdges
			]
		];

		Graph[ newVertices, newEdges,
			VertexCoordinates -> vertexCoords,
			VertexLabels -> vertexLabels,
			VertexSize -> Medium,
			EdgeStyle -> Directive[ Opacity[ 0.5 ] ]
		]
	]

ThickFunctionGraph[ f_, pointSpec_, diskSpec_, opts : OptionsPattern[ ] ] :=
	Module[ { graph },
		graph = FunctionGraph[ f, pointSpec, FilterRules[ { opts }, Options[ FunctionGraph ] ] ];
		ThickFunctionGraph[ graph, diskSpec, FilterRules[ { opts }, Options[ ThickFunctionGraph ] ] ]
	]
