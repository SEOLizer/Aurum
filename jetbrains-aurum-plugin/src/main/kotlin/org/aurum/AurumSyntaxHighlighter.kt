package org.aurum

import com.intellij.lexer.Lexer
import com.intellij.openapi.editor.DefaultLanguageHighlighterColors
import com.intellij.openapi.editor.HighlighterColors
import com.intellij.openapi.editor.colors.TextAttributesKey
import com.intellij.openapi.fileTypes.SyntaxHighlighterBase
import com.intellij.psi.tree.IElementType

class AurumSyntaxHighlighter : SyntaxHighlighterBase() {
    companion object {
        val KEYWORD = TextAttributesKey.createTextAttributesKey("AURUM_KEYWORD", DefaultLanguageHighlighterColors.KEYWORD)
        val IDENT = TextAttributesKey.createTextAttributesKey("AURUM_IDENT", DefaultLanguageHighlighterColors.IDENTIFIER)
        val NUMBER = TextAttributesKey.createTextAttributesKey("AURUM_NUMBER", DefaultLanguageHighlighterColors.NUMBER)
        val STRING = TextAttributesKey.createTextAttributesKey("AURUM_STRING", DefaultLanguageHighlighterColors.STRING)
        val COMMENT = TextAttributesKey.createTextAttributesKey("AURUM_COMMENT", DefaultLanguageHighlighterColors.LINE_COMMENT)
        val OP = TextAttributesKey.createTextAttributesKey("AURUM_OP", DefaultLanguageHighlighterColors.OPERATION_SIGN)
        val BRACE = TextAttributesKey.createTextAttributesKey("AURUM_BRACE", DefaultLanguageHighlighterColors.BRACES)
        val BAD = TextAttributesKey.createTextAttributesKey("AURUM_BAD", HighlighterColors.BAD_CHARACTER)

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

    override fun getHighlightingLexer(): Lexer = AurumLexer()

    override fun getTokenHighlights(tokenType: IElementType?): Array<TextAttributesKey> {
        return when (tokenType) {
            AurumTokenTypes.KEYWORD -> KEYWORD_KEYS
            AurumTokenTypes.IDENT  -> IDENT_KEYS
            AurumTokenTypes.NUMBER -> NUMBER_KEYS
            AurumTokenTypes.STRING -> STRING_KEYS
            AurumTokenTypes.COMMENT -> COMMENT_KEYS
            AurumTokenTypes.OP -> OP_KEYS
            AurumTokenTypes.BRACE -> BRACE_KEYS
            AurumTokenTypes.BAD_CHAR -> BAD_KEYS
            else -> EMPTY
        }
    }
}
