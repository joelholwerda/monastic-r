# Monastic (R package)

> "Only try to do it yourself and you shall learn how arduous is the writer's task. It dims your eyes, makes your back ache, and knits your chest and belly together---it is a terrible ordeal for the whole body." Florentius of Valeranica ([10th Century scribe](https://www.google.com.au/books/edition/The_Art_of_Medieval_Spain_A_D_500_1200/WdXQnaME1gMC?hl=en&gbpv=1&pg=PA162&printsec=frontcover))

Before the invention of the printing press, monastic scribes spent countless hours hand-copying manuscripts. This onerous process required them to remain vigilant against transcription errors. It was superseded by printing methods that were both more accurate and more efficient. Nonetheless, modern researchers continue this monastic tradition by conducting statistical analyses in R or Python and manually copying the output into their manuscripts.

The Monastic project aims to modernise this workflow by allowing researchers to interactively insert statistical output into their documents. It is currently under development and will involve two components:

1. An [R package](https://github.com/joelholwerda/monastic-r) that exports formatted statistical output as a JSON file
2. A [Microsoft Word add-in](https://github.com/joelholwerda/monastic-word) that provides a GUI to interactively insert this output in the document
