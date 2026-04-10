Package["WolframInstitute`InfraAnalysis`"]

PackageExport[RadialView]
PackageExport[LaminalView]


(* --- RadialView --- *)

Options[ RadialView ] = {
	"Center" -> { 0, 0, 0 },
	"Radius" -> 1.0,
	"Dimension" -> 2
};

RadialView[ graph_Graph, opts : OptionsPattern[ ] ] :=
	Module[
		{ center, radius, dim, vertices, radialDists, maxDist, verticesByDist, newCoords },

		center = OptionValue[ "Center" ];
		radius = OptionValue[ "Radius" ];
		dim = OptionValue[ "Dimension" ];
		vertices = VertexList[ graph ];

		radialDists = Association @ Table[ vertex -> AnnotationValue[ { graph, vertex }, "RadialDistance" ], { vertex, vertices } ];
		If[ AnyTrue[ Values[ radialDists ], MissingQ ],
			Return[ graph ]
		];

		maxDist = Max[ Values[ radialDists ] ];

		verticesByDist = GroupBy[ Normal[ radialDists ], Last -> First ];

		newCoords = Association @ Flatten @ KeyValueMap[
			{ dist, verts } |-> Module[ { r, n },
				r = If[ dist == 0, 0, radius * dist / If[ maxDist == 0, 1, maxDist ] ];
				n = Length[ verts ];
				Table[
					verts[[ i ]] -> If[ dim == 3,
						center + If[ r == 0, { 0, 0, 0 }, { 0, r * Cos[ 2 Pi ( i - 1 ) / n ], r * Sin[ 2 Pi ( i - 1 ) / n ] } ],
						center[[ ;; 2 ]] + If[ r == 0, { 0, 0 }, { r * Cos[ 2 Pi ( i - 1 ) / n ], r * Sin[ 2 Pi ( i - 1 ) / n ] } ]
					],
					{ i, n }
				]
			],
			verticesByDist
		];

		Graph[ vertices, EdgeList[ graph ],
			VertexCoordinates -> Normal[ newCoords ],
			VertexLabels -> AnnotationValue[ graph, VertexLabels ],
			EdgeStyle -> AnnotationValue[ graph, EdgeStyle ],
			VertexSize -> AnnotationValue[ graph, VertexSize ],
			DirectedEdges -> DirectedGraphQ[ graph ],
			Options[ graph ]
		]
	]

(* --- LaminalView --- *)

Options[ LaminalView ] = {
	"Origin" -> { 0, 0, 0 },
	"Spacing" -> 1.0,
	"Axis" -> 1
};

LaminalView[ graph_Graph, opts : OptionsPattern[ ] ] :=
	LaminalView[ graph, LayerDAGBySources[ graph ], opts ]

LaminalView[ graph_Graph, layersBySources_Association, opts : OptionsPattern[ ] ] :=
	Module[
		{ origin, spacing, axis, vertices, laminarDists, radialDists, verticesByLayer, newCoords },

		origin = OptionValue[ "Origin" ];
		spacing = OptionValue[ "Spacing" ];
		axis = OptionValue[ "Axis" ];
		vertices = VertexList[ graph ];

		laminarDists = Association @ Table[ vertex -> AnnotationValue[ { graph, vertex }, "LaminarDistance" ], { vertex, vertices } ];
		If[ AnyTrue[ Values[ laminarDists ], MissingQ ],
			Return[ graph ]
		];

		radialDists = Association @ Table[ vertex -> AnnotationValue[ { graph, vertex }, "RadialDistance" ], { vertex, vertices } ];
		verticesByLayer = GroupBy[ Normal[ laminarDists ], Last -> First ];

		newCoords = Association @ Flatten @ KeyValueMap[
			{ layerDist, verts } |-> Module[ { layerPos },
				layerPos = origin + spacing * layerDist * UnitVector[ 3, axis ];
				Module[ { byRadial },
					byRadial = GroupBy[ Normal[ Association @ Table[ vertex -> Lookup[ radialDists, vertex, 0 ], { vertex, verts } ] ], Last -> First ];
					Flatten @ KeyValueMap[
						{ rad, radVerts } |-> Module[ { r, n, perpAxes, coord },
							r = If[ rad == 0 || Max[ Values[ Association @ Table[ vertex -> Lookup[ radialDists, vertex, 0 ], { vertex, verts } ] ] ] == 0, 0, 0.5 * rad / Max[ Values[ Association @ Table[ vertex -> Lookup[ radialDists, vertex, 0 ], { vertex, verts } ] ] ] ];
							n = Length[ radVerts ];
							perpAxes = Delete[ { 1, 2, 3 }, axis ];
							Table[
								coord = layerPos;
								If[ r > 0,
									coord[[ perpAxes[[ 1 ]] ]] += r * Cos[ 2 Pi ( i - 1 ) / n ];
									coord[[ perpAxes[[ 2 ]] ]] += r * Sin[ 2 Pi ( i - 1 ) / n ]
								];
								radVerts[[ i ]] -> coord,
								{ i, n }
							]
						],
						byRadial
					]
				]
			],
			verticesByLayer
		];

		Graph[ vertices, EdgeList[ graph ],
			VertexCoordinates -> Normal[ newCoords ],
			VertexLabels -> AnnotationValue[ graph, VertexLabels ],
			EdgeStyle -> AnnotationValue[ graph, EdgeStyle ],
			VertexSize -> AnnotationValue[ graph, VertexSize ],
			DirectedEdges -> DirectedGraphQ[ graph ],
			Options[ graph ]
		]
	]
