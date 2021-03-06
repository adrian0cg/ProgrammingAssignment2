## Run this file to test an example

library('RUnit')
source('cachematrix.R')

test.suite <- defineTestSuite(
  "CacheMatrix",
  dirs = file.path("tests"),
  testFileRegexp = '^\\d+\\.R'
)

test.result <- runTestSuite(test.suite)

printTextProtocol(test.result)