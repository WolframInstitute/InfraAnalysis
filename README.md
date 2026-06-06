# 📐 InfraAnalysis

This repository contains experimental code for exploring mathematical analysis on graphs, in particular for developing a discrete theory of calculus. Integration is about directional accumulation, and differentiation measures its rate of change. It has traditionally been developed on the real line, which is a macro-observer's idealization. We consider a DAG to be the natural generalization of the concept of directional accumulation. This leads to various versions of integrals, depending on how branching is handled, and to corresponding derivatives satisfying the fundamental theorem. We connect this with orthogonal coordinatizationsk, introduce partial derivatives, and develop multivariate calculus. We study natural blow-ups and contractions of DAGs that commute with integration and differentiation and thus give rise to renormalization.
We also want to ask the dual question: to what extent is the graph represented by dual structures related to function spaces, i.e. the algebraic dual of the graph? This connects to the conversion between various notions of differential forms — on k-tuples of edges or as cochains — and the tangent space, possibly using sheaves.

Our goals include:

- defining multiple notions of integration and differentiation on DAGs and comparing their locality properties
- finding for which pairs of integral and derivative a fundamental theorem holds
- studying how blow-up and contraction interact with the calculus constructions
- coordinatizing graphs via axes and shortest-path projections and defining partial derivatives
- formulating and proving a discrete Gauss theorem on coordinatized graphs
- developing the duality approach via function spaces and the algebraic dual of the graph
- converting between differential forms on k-tuples of edges or cochains and the tangent space, using sheaves

## ✨ Usage

```wolfram
PacletDirectoryLoad["/path/to/InfraAnalysis/InfraAnalysis"]
Needs["WolframInstitute`InfraAnalysis`"]
```

## ∴ License

MIT
