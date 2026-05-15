# DenseCheck Agent Notes

## Project

- F#/.NET solution: `DenseCheck.sln`
- Library project: `src/DenseCheck/DenseCheck.fsproj`
- Test project: `src/DenseCheck.Tests/DenseCheck.Tests.fsproj`
- Target framework: `net10.0`

## Required Environment

- Use the .NET SDK pinned in `global.json`.
- If the SDK is unavailable in a cloud environment, install .NET SDK `10.0.x` before restoring packages.

## Validation

Run these from the repository root:

```sh
dotnet restore DenseCheck.sln
dotnet build src/DenseCheck.Tests/DenseCheck.Tests.fsproj --configuration Release --no-restore --disable-build-servers
dotnet test src/DenseCheck.Tests/DenseCheck.Tests.fsproj --configuration Release --no-build --disable-build-servers
```

The canonical local check command is:

```sh
./scripts/check.sh
```

## F# Notes

- F# compile order is explicit and significant. When adding files, update the relevant `.fsproj` in dependency order.
- Prefer forward slashes in project file paths for cross-platform compatibility.
