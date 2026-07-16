library(devtools)

# Feedback rápido durante o desenvolvimento.
# O build/check oficial ocorre no Docker Linux e no GitHub Actions.
devtools::test()
# no power shell 
# .\scripts\test-sandbox.ps1