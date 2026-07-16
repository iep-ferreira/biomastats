$ErrorActionPreference = "Stop"

$root = (Resolve-Path (Join-Path $PSScriptRoot "..")).Path
$image = "biomastats-check:local"

docker build --tag $image --file (Join-Path $root "docker/Dockerfile") $root
docker run --rm $image
