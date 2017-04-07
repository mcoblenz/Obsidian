package edu.cmu.cs.obsidian.lexer

import scala.util.parsing.input._

class TokenReader(tokens : Seq[Token]) extends Reader[Token] {
    override def atEnd : Boolean = tokens.isEmpty
    override def pos: Position = NoPosition

    override def first : Token = tokens.head
    override def rest : Reader[Token] = new TokenReader(tokens.tail)
}
