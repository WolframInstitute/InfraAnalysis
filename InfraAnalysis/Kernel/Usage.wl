Package["WolframInstitute`InfraAnalysis`"]

(* --- Graph utilities --- *)
GraphSources::usage = "GraphSources[ g ] returns the source vertices (in-degree 0) of graph g.";
GraphSinks::usage = "GraphSinks[ g ] returns the sink vertices (out-degree 0) of graph g.";
DirectedPathGraph::usage = "DirectedPathGraph[ n ] creates a directed path on n vertices.";
ConeGraph::usage = "ConeGraph[ g ] adds a cone vertex connected to all vertices of g.";

(* --- Decomposition --- *)
LaminarDecomposition::usage = "LaminarDecomposition[ g ] decomposes a directed graph into radial and laminar subgraphs.";
LayerDAGBySources::usage = "LayerDAGBySources[ g ] layers a DAG by minimal distance from sources.";

(* --- Blow-up and contraction --- *)
RadialExpansion::usage = "RadialExpansion[ g, layers ] adds concentric radial layers to a graph.";
RadialBlowUp::usage = "RadialBlowUp[ g, layerCounts ] organizes a DAG into concentric layers with edges toward sinks.";
BlowUpGraph::usage = "BlowUpGraph[ g, rules ] blows up vertices according to subgraph replacement rules.";
ContractGraph::usage = "ContractGraph[ g ] contracts a blown-up graph back to the original vertices.";

(* --- Calculus: Integration --- *)
GraphIntegral::usage = "GraphIntegral[ g, f, a ] computes the integral of f over predecessors of a in g.";
GraphIntegration::usage = "GraphIntegration[ g, f ] computes cumulative integration along topological order.";
GraphIntegrationGeneral::usage = "GraphIntegrationGeneral[ g, f ] computes integration via full reachability.";

(* --- Calculus: Derivative --- *)
GraphDerivation::usage = "GraphDerivation[ g, f ] computes the derivative via Moebius inversion (non-local).";
GraphFiniteDifference::usage = "GraphFiniteDifference[ g, f ] computes the local finite difference.";

(* --- Incidence algebra --- *)
GraphMobiusFunction::usage = "GraphMobiusFunction[ g ] computes the Moebius function of the DAG partial order.";
GraphZetaFunction::usage = "GraphZetaFunction[ g, f ] computes the zeta function (sum over reachable vertices).";
MobiusInversionTheorem::usage = "MobiusInversionTheorem[ g, f ] applies Moebius inversion to f.";

(* --- Fundamental theorem --- *)
GraphFundamentalTheorem::usage = "GraphFundamentalTheorem[ g, f ] verifies the fundamental theorem of graph calculus.";
