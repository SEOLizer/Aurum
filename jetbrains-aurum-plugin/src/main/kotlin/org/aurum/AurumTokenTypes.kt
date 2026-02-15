package org.aurum

import com.intellij.psi.tree.IElementType

class AurumTokenType(debugName: String) : IElementType(debugName, AurumLanguage)

object AurumTokenTypes {
    val KEYWORD = AurumTokenType("AURUM_KEYWORD")
    val IDENT = AurumTokenType("AURUM_IDENT")
    val NUMBER = AurumTokenType("AURUM_NUMBER")
    val STRING = AurumTokenType("AURUM_STRING")
    val COMMENT = AurumTokenType("AURUM_COMMENT")
    val OP = AurumTokenType("AURUM_OP")
    val BAD_CHAR = AurumTokenType("AURUM_BAD_CHAR")
    val BRACE = AurumTokenType("AURUM_BRACE")
}
