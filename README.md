# InfraAnalysis

Wolfram Language paclet for analysis on directed graphs: integration, derivative, coordinatization, and renormalization.

## Installation

```wolfram
PacletInstall["https://github.com/WolframInstitute/InfraAnalysis"]
```

Or from a local clone:

```wolfram
PacletDirectoryLoad["path/to/InfraAnalysis/InfraAnalysis"]
Needs["WolframInstitute`InfraAnalysis`"]
```

## Features

- **Graph calculus**: integration, derivative (Moebius inversion), finite differences
- **Fundamental theorem** of graph calculus
- **Incidence algebra**: Moebius function, zeta function, Moebius inversion
- **Blow-up / contraction**: radial and laminar blow-ups, vertex replacement
- **Decomposition**: laminar decomposition, layer DAG by sources
- **Visualization**: radial and laminar graph layouts

## Running tests

```bash
wolframscript -f run_tests.wls
```
