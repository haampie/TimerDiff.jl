# TimerDiff.jl

Tree diffing on output from https://github.com/AdhocMan/rt_graph for comparing performance reports across versions.

To compare two json reports, run:

```bash
julia --project apps/diff.jl timers_a.json timers_b.json
```