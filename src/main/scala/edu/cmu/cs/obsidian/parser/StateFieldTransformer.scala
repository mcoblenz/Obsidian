package edu.cmu.cs.obsidian.parser

/*
    The StateFieldProcessor moves all fields that are lexically declared in states into the containing contract
    with an appropriate "available in" clause.
 */

object StateFieldTransformer {
    def transformProgram(p: Program): Program = {
        val newContracts = p.contracts.map(transformContract)

        p.copy(contracts = newContracts).setLoc(p)
    }

    def transformContract(contract: Contract): Contract = {
        val transformedDeclarations = contract.declarations.flatMap((d: Declaration) => transformDeclaration(d))
        contract.copy(declarations = transformedDeclarations).setLoc(contract)
    }


    def transformDeclaration(decl: Declaration): Seq[Declaration] = {
        decl match {
            case s: State => transformState(s)
            case d: Declaration => Seq(d)
        }
    }

    // returns the new state and a sequence of lifted fields.
    def transformState(state: State): Seq[Declaration] = {
        val transformedStateFields: Seq[Field] =
            state.fields.foldLeft(Seq.empty[Field])((fieldsSoFar: Seq[Field], d: Declaration) =>
                if (d.isInstanceOf[Field]) {
                    val f = d.asInstanceOf[Field]
                    val newField = f.copy(availableIn = Some(Set(state.name)))
                    newField.setLoc(f)
                    newField +: fieldsSoFar
                }
                else {
                    fieldsSoFar
                }
            )
        val remainingStateDecls = state.fields.filterNot((d: Declaration) => d.isInstanceOf[Field])

        new State(state.name, remainingStateDecls, state.isResource) +: transformedStateFields
    }
}
