This is a tool to help analyze GcAMP data.

How to use:
- Drag unzipped folder into your R Studio Project folder.
- Include the line ‘ source(‘pf/peakFinder.R’) ‘ at the top of your document.
- Call analyzeExperiment(dataframe) where dataframe is your data, with the
first column containing the ‘Time’ and all other columns containing
samples.
- Call runTest(dataframe) to output a graph to check that the data looks
ok:
	Green = peak calls
	Red = minimum calls
	Purple/Orange = T50 calls
Make sure those look OK.

Enjoy!