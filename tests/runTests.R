library('RUnit')

source('../code/dataPreparation.R')


test.suite <- defineTestSuite("run-tests",
                              dirs = file.path("."),
                              testFileRegexp = "^runit.+\\.r",
                              testFuncRegexp = "^test.+")

test.result <- runTestSuite(test.suite)

printTextProtocol(test.result)