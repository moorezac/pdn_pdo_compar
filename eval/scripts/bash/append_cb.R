EnvironmentModules::module_load("python/3.8.8")

phyloRNA::bamtagregex(
  input = here("data", "raw", "splitpipe", "sub_1", "process", "sorted.bam"),
  output = here("data", "raw", "splitpipe", "new.bam"),
  tag = "CB",
  pattern = "$",
  replace = "__s1",
  remake = TRUE
)
