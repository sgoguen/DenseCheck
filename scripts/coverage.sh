#!/usr/bin/env sh
set -eu

ROOT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
cd "$ROOT_DIR"

RESULTS_DIR=${COVERAGE_RESULTS_DIR:-TestResults/Coverage}
REPORT_DIR=${COVERAGE_REPORT_DIR:-TestResults/CoverageReport}
TEST_PROJECT=src/DenseCheck.Tests/DenseCheck.Tests.fsproj

echo "Restoring DenseCheck.sln..."
dotnet restore DenseCheck.sln

echo "Building DenseCheck.Tests..."
dotnet build "$TEST_PROJECT" \
  --configuration Release \
  --no-restore \
  --disable-build-servers

echo "Running tests with coverage..."
dotnet test "$TEST_PROJECT" \
  --configuration Release \
  --no-build \
  --disable-build-servers \
  --collect:"XPlat Code Coverage" \
  --results-directory "$RESULTS_DIR" \
  -- \
  'DataCollectionRunSettings.DataCollectors.DataCollector.Configuration.Format=cobertura' \
  'DataCollectionRunSettings.DataCollectors.DataCollector.Configuration.Include=[DenseCheck]*' \
  'DataCollectionRunSettings.DataCollectors.DataCollector.Configuration.Exclude=[DenseCheck.Tests]*'

COVERAGE_FILE=$(find "$RESULTS_DIR" -name coverage.cobertura.xml -type f | sort | tail -n 1)

if [ -z "$COVERAGE_FILE" ]; then
  echo "Coverage failed: no coverage.cobertura.xml was written under $RESULTS_DIR" >&2
  exit 1
fi

echo "Coverage XML: $COVERAGE_FILE"

# if dotnet reportgenerator --help >/dev/null 2>&1; then
  echo "Generating HTML coverage report..."
  dotnet reportgenerator \
    "-reports:$COVERAGE_FILE" \
    "-targetdir:$REPORT_DIR" \
    "-reporttypes:Html;TextSummary"

  echo "Coverage report: $REPORT_DIR/index.html"
  if [ -f "$REPORT_DIR/Summary.txt" ]; then
    echo "Coverage summary:"
    cat "$REPORT_DIR/Summary.txt"
  fi
# else
#   echo "Skipping HTML report: dotnet-reportgenerator-globaltool is not installed."
#   echo "To enable it, run:"
#   echo "  dotnet new tool-manifest"
#   echo "  dotnet tool install dotnet-reportgenerator-globaltool"
# fi