package edu.cmu.cs.obsidian.util

/**
  * Created by mcoblenz on 2/24/17.
  */
object Util {
    // Protobuf specifications are required to be in snake case, but Java and Obsidian use camel case.
    def snakeCaseToCamelCase(s: String): String = {
        // TODO
        ""
    }

    def protobufOuterClassNameForClass(className: String): String = {
        val initialOuterClassName = className.replace(".obs", "") + "OuterClass"

        initialOuterClassName.substring(0, 1).toUpperCase(java.util.Locale.US) + initialOuterClassName.substring(1)
    }
}
