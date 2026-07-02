# ∫ InfraAnalysis

> ⚠️ **Actively developed, experimental research code.** It undergoes frequent cleanings and refactors, and the API may change without notice.

This repository contains experimental code for studying a foundational theory of calculus on graphs — one that recovers ordinary continuous calculus as an effective theory as the size of the graph and the scale increase.
The concrete idea is to view the real line as an idealization of a DAG, and integration as an idealization of accumulation.
It turns out there are various versions of accumulation on a DAG, depending on how branching is handled; the corresponding derivative for which the fundamental theorem holds may then be local or non-local.

By using our method of orthogonal coordinatization along orthogonal DAGs as axes, we introduce partial derivatives and develop multivariate calculus.

We study renormalization by considering natural blow-ups and contractions of DAGs that commute with integration and differentiation.

We also consider the dual approach via function spaces, and ask for an algebraic dual of a graph.
This leads to sheaf-theoretic and algebraic-geometry-like approaches.

## 🎯 Goals

Our goals include:

- defining multiple notions of integration and differentiation on DAGs and comparing their locality properties
- finding for which pairs of integral and derivative a fundamental theorem holds
- studying how blow-up and contraction interact with the calculus constructions
- coordinatizing graphs via axes and shortest-path projections and defining partial derivatives
- formulating and proving a discrete Gauss theorem on coordinatized graphs
- studying properties of function spaces on graphs and the operator algebras on them, and defining the algebraic dual of the graph
- studying dependence on coordinates, differential forms, and the Stokes theorem

## ✨ Usage

Install from the Wolfram Cloud:

```wolfram
PacletInstall["https://www.wolframcloud.com/obj/hajek_pavel/InfraAnalysis.paclet", ForceVersionInstall -> True]
Needs["WolframInstitute`InfraAnalysis`"]
```

Explore the paclet in the **[LLM-generated presentation notebook](https://www.wolframcloud.com/obj/hajek_pavel/InfraAnalysis/Presentation.nb)** (runs on the Wolfram Cloud).

## ⚖️ License

MIT
