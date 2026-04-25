Package["WolframInstitute`InfraAnalysis`"]

GraphSources::usage =
	"GraphSources[g] returns the source vertices (in-degree 0) of graph g.";

GraphSinks::usage =
	"GraphSinks[g] returns the sink vertices (out-degree 0) of graph g.";

DirectedPath::usage =
	"DirectedPath[n] returns the directed path graph on n vertices.";

ConeGraph::usage =
	"ConeGraph[g] adds a cone vertex connected to all vertices of g.";


LaminarDecomposition::usage =
	"LaminarDecomposition[g] decomposes a directed graph into laminar layers (a DAG of layers plus per-layer subgraphs).";


RadialExpansion::usage =
	"RadialExpansion[g, layers] grows g by appending concentric vertex rings outside the sources; layers is a list of ring sizes."

GraphBlowUp::usage =
	"GraphBlowUp[g, n] replaces each vertex v with a fiber of n copies {v,1}..{v,n} and each edge u->v with the complete bipartite K_{n,n}. GraphBlowUp[g, n, f] also diffuses f uniformly across each fiber, returning <|\"Graph\" -> ..., \"Values\" -> ...|>. Use an Association for n to specify per-vertex fiber sizes.";

GraphContract::usage =
	"GraphContract[g] collapses a blown-up graph back to original vertices (using Origin annotations). GraphContract[g, f] also sums f over each fiber, returning <|\"Graph\" -> ..., \"Values\" -> ...|>.";


GraphIntegrate::usage =
	"GraphIntegrate[g, f] returns g annotated with the integral of f. Method -> \"Ordered\" | \"Cumulative\" | \"Conservative\" | \"Laminar\".";

GraphDerivative::usage =
	"GraphDerivative[g, f] returns g annotated with the derivative of f. Method -> \"Ordered\" | \"Cumulative\" | \"Conservative\" | \"Laminar\" | \"Directional\" | \"Weighted\".";

GraphIntegral::usage =
	"GraphIntegral[g, f, v] returns the total of f over all predecessors of v. GraphIntegral[g, f, sources, sinks] integrates over the subdag between sources and sinks.";


GraphVectorFieldQ::usage =
	"GraphVectorFieldQ[g, X] returns True if X is a vector field on g (each X[v] is a neighbor of v).";

GraphVectorFields::usage =
	"GraphVectorFields[g] enumerates all vector fields on g.";

GraphVectorFieldEndomorphism::usage =
	"GraphVectorFieldEndomorphism[g, X] returns the pullback endomorphism f |-> (v |-> f(X(v))).";

GraphFiniteDifference::usage =
	"GraphFiniteDifference[g, f] returns f(v) minus the sum of f over in-neighbors of v.";

GraphDirectionalDifference::usage =
	"GraphDirectionalDifference[g, X, f] returns f(X(v)) - f(v) along vector field X.";

GraphWeightedDerivation::usage =
	"GraphWeightedDerivation[g, X, f, eps] returns (f(X(v)) - f(v)) / eps; eps may be a scalar or a per-vertex association.";

GraphTwistedProduct::usage =
	"GraphTwistedProduct[g, X, f, h] returns the shifted product (f star h)(v) = f(X(v)) h(v).";

GraphWeightedLeibnizQ::usage =
	"GraphWeightedLeibnizQ[g, X, f, h, eps] returns True if the directional difference satisfies the weighted Leibniz rule with weight eps.";
