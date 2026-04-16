# Code Coverage Testing

We can watch and create code coverage reports using `coverlet` and `reportgenerator` for the DenseCheck.Tests project.

## Prerequisites

Install the `reportgenerator` global tool (once):


```bash
dotnet tool install -g dotnet-reportgenerator-globaltool
```

## Run Tests in Watch Mode with Coverage

From the repo root, run:

```bash
dotnet watch test --project src/DenseCheck.Tests -- --collect:"XPlat Code Coverage" --results-directory ./TestResults
```

`dotnet watch` will rebuild and rerun the tests automatically whenever a source file changes. Coverage data is written to `./TestResults/` in Cobertura XML format by `coverlet.collector`, which is already referenced in `DenseCheck.Tests.fsproj`.

## Run Tests

```
dotnet watch --project src/DenseCheck.Tests test /p:CollectCoverage=true /p:CoverletOutputFormat=lcov /p:CoverletOutput=../../lcov.info
```

## Generate an HTML Coverage Report

After the tests have run at least once, generate a human-readable report:

```bash
reportgenerator -reports:"TestResults/**/coverage.cobertura.xml" -targetdir:"coverage-report" -reporttypes:Html
```

Then open `coverage-report/index.html` in a browser to view the results.

## One-liner (PowerShell)

To run the tests and immediately regenerate the report in a single command:

```powershell
dotnet test --project src/DenseCheck.Tests -- --collect:"XPlat Code Coverage" --results-directory ./TestResults ; reportgenerator -reports:"TestResults/**/coverage.cobertura.xml" -targetdir:"coverage-report" -reporttypes:Html
```

> **Tip:** Combine this with `dotnet watch` by wrapping the two commands in a PowerShell script and pointing `dotnet watch run` at it, or simply re-run the one-liner after each batch of changes.

