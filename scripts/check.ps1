Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

$solutionFile = 'DenseCheck.slnx'

Write-Host "Restoring, building, and testing DenseCheck..."
dotnet restore $solutionFile
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
Write-Host "Restored $solutionFile"

Write-Host "Building and testing DenseCheck.Tests..."
dotnet build src/DenseCheck.Tests/DenseCheck.Tests.fsproj --configuration Release --no-restore --disable-build-servers
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }

Write-Host "Running tests for DenseCheck.Tests..."
dotnet test src/DenseCheck.Tests/DenseCheck.Tests.fsproj --configuration Release --no-build --disable-build-servers
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }

Write-Host "Tests for DenseCheck.Tests passed"
