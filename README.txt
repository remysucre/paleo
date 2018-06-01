The example tests from the report are contained in examples.rkt.

To run the examples, simply run "racket examples.rkt" from the command-line.

To run your own tests, use the following format: (synth R1 Psi <input list> <output list> <*debug*> <*learn?*>)

Paleo only supports one input-output example, and may not terminate (especially if *learn?* is #f).
Input and output must be (non-empty) standard racket lists of integers.

When *debug* is #t, intermediate partial programs and knowledge bases are printed.

When *learn?* is #t, Paleo will use AnalyzeConclict to learn new lemmas to add to the knowledge base.