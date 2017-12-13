package edu.cmu.cs.obsidian.parser

sealed trait ParsableType extends HasLocation

/*
sealed abstract class TypeModifier() extends HasLocation
case class IsReadOnly() extends TypeModifier
case class IsRemote() extends TypeModifier

case class ParsableIntType() extends ParsableType
case class ParsableBoolType() extends ParsableType
case class ParsableStringType() extends ParsableType

case class ParsableNonPrimitiveType(mods: Seq[TypeModifier],
                                    identifiers: Seq[String]) extends ParsableType

*/