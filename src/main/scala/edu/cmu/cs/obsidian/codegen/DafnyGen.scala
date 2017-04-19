package edu.cmu.cs.obsidian.codegen

import edu.cmu.cs.obsidian.parser.{Contract, Program}

/**
  * Created by mcoblenz on 4/27/17.
  */
class DafnyGen {
    def translateProgram(p: Program, outputFile: java.io.File): Unit = {
        for (c: Contract <- p.contracts) {
            translateContract(c)
        }
    }

    def translateContract(c: Contract): Unit = {
        
    }
}
