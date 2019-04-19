# Framework of integration tests
1. Go to resources/tests/endToEndTests and create a new bash script
2. In order to generate java code that will be deployed on hyperledger,
you must run sbt to generate it. To generate it in the Obsidian root directory,
first cd to this directory and do
`
sbt "runMain edu.cmu.cs.obsidian.Main [path to obs code]"
`
3. Next step is to cd to network-framework and write the actual test code
4. Always bring down the container and remove the generated java code keep
make sure this test is standalone and portable