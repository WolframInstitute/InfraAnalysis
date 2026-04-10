GraphSources[ graph_Graph ] :=
	Pick[ VertexList @ graph, VertexInDegree @ graph, 0 ]

GraphSinks[ graph_Graph ] :=
	Pick[ VertexList @ graph, VertexOutDegree @ graph, 0 ]

DirectedPathGraph[ n_Integer ] :=
	Graph[ Range[ n ], DirectedEdge @@@ Partition[ Range[ n ], 2, 1 ] ]

ConeGraph[ graph_Graph, vertex_ ] :=
	EdgeAdd[ graph, DirectedEdge[ vertex, # ] & /@ VertexList[ graph ] ]

ConeGraph[ graph_Graph ] :=
	ConeGraph[ graph, Unique[ ] ]
