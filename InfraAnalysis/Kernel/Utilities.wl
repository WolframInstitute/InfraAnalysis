Package["WolframInstitute`InfraAnalysis`"]

PackageExport[GraphSources]
PackageExport[GraphSinks]
PackageExport[DirectedPath]
PackageExport[ConeGraph]

PackageScope[AnnotateGraph]
PackageScope[ExtractVertexValues]


GraphSources[ graph_Graph ] :=
	Pick[ VertexList @ graph, VertexInDegree @ graph, 0 ]

GraphSinks[ graph_Graph ] :=
	Pick[ VertexList @ graph, VertexOutDegree @ graph, 0 ]

DirectedPath[ n_Integer ] :=
	Graph[ Range[ n ], DirectedEdge @@@ Partition[ Range[ n ], 2, 1 ] ]

ConeGraph[ graph_Graph, vertex_ ] :=
	EdgeAdd[ graph, DirectedEdge[ vertex, # ] & /@ VertexList[ graph ] ]

ConeGraph[ graph_Graph ] :=
	ConeGraph[ graph, Unique[ ] ]


AnnotateGraph[ graph_Graph, values_Association ] :=
	Module[ { vertices },
		vertices = VertexList[ graph ];
		Graph[ vertices, EdgeList[ graph ],
			VertexLabels -> Normal[ values ],
			VertexCoordinates -> Normal[ AssociationThread[ vertices, AnnotationValue[ { graph, # }, VertexCoordinates ] & /@ vertices ] ],
			EdgeStyle -> AnnotationValue[ graph, EdgeStyle ],
			VertexSize -> AnnotationValue[ graph, VertexSize ],
			DirectedEdges -> True
		]
	]

ExtractVertexValues[ graph_Graph ] :=
	AssociationThread[ VertexList[ graph ], AnnotationValue[ { graph, # }, VertexLabels ] & /@ VertexList[ graph ] ]
