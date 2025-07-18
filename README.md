
Repo using the latest Chisel and some small examples for testing:

sbt "runMain gcd.GCD"
sbt "runMain dinocpu.SingleCycleCPUNoDebug"
sbt "runMain dinocpu.SingleCycleCPUDebug"
sbt "runMain dinocpu.pipelined.PipelinedDualIssueDebug"

