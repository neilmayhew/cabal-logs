# cabal-logs #

This project contains utilities for examining Cabal logs.

## test-failures ##

This utility reads the Cabal build plan file and uses it to learn the target names and log file locations of all tests within the Cabal project. It then parses any logs that exist and extracts the information needed to reproduce any failures (seed and pattern). It outputs a summary of this information in Markdown format.

This output is suitable for including in a GitHub job summary, so if this utility is run in CI users can quickly see what failed in a CI run without having to open the log files themselves.

The utility understands the output of both Hspec- and Tasty-based tests.
