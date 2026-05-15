#!/usr/bin/env sh
set -eu

# curl https://api.nuget.org/v3/index.json | wc

echo "Restoring, building, and testing DenseCheck..."
dotnet restore DenseCheck.sln
echo "Restored DenseCheck.sln"

echo "Building and testing DenseCheck.Tests..."
dotnet build src/DenseCheck.Tests/DenseCheck.Tests.fsproj --configuration Release --no-restore --disable-build-servers
echo "Running tests for DenseCheck.Tests..."
dotnet test src/DenseCheck.Tests/DenseCheck.Tests.fsproj --configuration Release --no-build --disable-build-servers
echo "Tests for DenseCheck.Tests passed"
