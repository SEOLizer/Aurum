package org.aurum

import com.intellij.lexer.Lexer
import com.intellij.openapi.editor.DefaultLanguageHighlighterColors
import com.intellij.openapi.editor.HighlighterColors
import com.intellij.openapi.editor.colors.TextAttributesKey
import com.intellij.openapi.fileTypes.SyntaxHighlighterBase
import com.intellij.psi.tree.IElementType

class LyxSyntaxHighlighter : SyntaxHighlighterBase() {
    companion object {
        val KEYWORD = TextAttributesKey.createTextAttributesKey("LYX_KEYWORD", DefaultLanguageHighlighterColors.KEYWORD)
        val IDENT = TextAttributesKey.createTextAttributesKey("LYX_IDENT", DefaultLanguageHighlighterColors.IDENTIFIER)
        val NUMBER = TextAttributesKey.createTextAttributesKey("LYX_NUMBER", DefaultLanguageHighlighterColors.NUMBER)
        val STRING = TextAttributesKey.createTextAttributesKey("LYX_STRING", DefaultLanguageHighlighterColors.STRING)
        val COMMENT = TextAttributesKey.createTextAttributesKey("LYX_COMMENT", DefaultLanguageHighlighterColors.LINE_COMMENT)
        val OP = TextAttributesKey.createTextAttributesKey("LYX_OP", DefaultLanguageHighlighterColors.OPERATION_SIGN)
        val BRACE = TextAttributesKey.createTextAttributesKey("LYX_BRACE", DefaultLanguageHighlighterColors.BRACES)
        val BAD = TextAttributesKey.createTextAttributesKey("LYX_BAD", HighlighterColors.BAD_CHARACTER)

        private val KEYWORD_KEYS = arrayOf(KEYWORD)
        private val IDENT_KEYS = arrayOf(IDENT)
        private val NUMBER_KEYS = arrayOf(NUMBER)
        private val STRING_KEYS = arrayOf(STRING)
        private val COMMENT_KEYS = arrayOf(COMMENT)
        private val OP_KEYS = arrayOf(OP)
        private val BRACE_KEYS = arrayOf(BRACE)
        private val BAD_KEYS = arrayOf(BAD)
        private val EMPTY = emptyArray<TextAttributesKey>()
    }

    override fun getHighlightingLexer(): Lexer = LyxLexer()

    override fun getTokenHighlights(tokenType: IElementType?): Array<TextAttributesKey> {
        return when (tokenType) {
            LyxTokenTypes.KEYWORD -> KEYWORD_KEYS
            LyxTokenTypes.IDENT  -> IDENT_KEYS
            LyxTokenTypes.NUMBER -> NUMBER_KEYS
            LyxTokenTypes.STRING -> STRING_KEYS
            LyxTokenTypes.COMMENT -> COMMENT_KEYS
            LyxTokenTypes.OP -> OP_KEYS
            LyxTokenTypes.BRACE -> BRACE_KEYS
            LyxTokenTypes.BAD_CHAR -> BAD_KEYS
            else -> EMPTY
        }
    }
}
