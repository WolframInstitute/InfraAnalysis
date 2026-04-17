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
	"LaminarDecomposition[g] decomposes a directed graph into radial and laminar subgraphs.";

LayerDAGBySources::usage =
	"LayerDAGBySources[g] layers a DAG by minimal distance from sources.";


RadialExpansion::usage =
	"RadialExpansion[g, layers] adds concentric radial layers to a graph.";

RadialBlowUp::usage =
	"RadialBlowUp[g, layerCounts] organizes a DAG into concentric layers with edges toward sinks.";

BlowUpGraph::usage =
	"BlowUpGraph[g, rules] blows up vertices according to subgraph replacement rules.";

ContractGraph::usage =
	"ContractGraph[g] contracts a blown-up graph back to the original vertices.";


GraphIntegrate::usage =
	"GraphIntegrate[g, f] returns g annotated with the integral of f.";

GraphDerivative::usage =
	"GraphDerivative[g, f] returns g annotated with the derivative of f.";

GraphIntegral::usage =
	"GraphIntegral[g, f, v] returns the total of f over predecessors of v.";

GraphMobiusFunction::usage =
	"GraphMobiusFunction[g] returns the Moebius function of the DAG partial order as an association on vertex pairs.";

GraphZetaConvolution::usage =
	"GraphZetaConvolution[g, f] returns the sum of f over predecessors of each vertex.";

GraphMobiusInversion::usage =
	"GraphMobiusInversion[g, f] returns the Moebius inversion of f on the DAG partial order.";

GraphFundamentalTheorem::usage =
	"GraphFundamentalTheorem[g, f] verifies that derivative inverts integration; Method -> m restricts to a single method.";


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
