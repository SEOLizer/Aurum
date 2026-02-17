package org.aurum

import com.intellij.psi.tree.IElementType

class LyxTokenType(debugName: String) : IElementType(debugName, LyxLanguage)

object LyxTokenTypes {
    val KEYWORD = LyxTokenType("LYX_KEYWORD")
    val IDENT = LyxTokenType("LYX_IDENT")
    val NUMBER = LyxTokenType("LYX_NUMBER")
    val STRING = LyxTokenType("LYX_STRING")
    val COMMENT = LyxTokenType("LYX_COMMENT")
    val OP = LyxTokenType("LYX_OP")
    val BAD_CHAR = LyxTokenType("LYX_BAD_CHAR")
    val BRACE = LyxTokenType("LYX_BRACE")
}
