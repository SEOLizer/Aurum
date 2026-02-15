package org.aurum

import com.intellij.lexer.LexerBase
import com.intellij.psi.tree.IElementType
import com.intellij.util.text.CharArrayUtil

class AurumLexer : LexerBase() {
    private lateinit var buffer: CharSequence
    private var startOffset = 0
    private var endOffset = 0
    private var tokenStart = 0
    private var tokenEnd = 0
    private var tokenType: IElementType? = null

    private val keywords = setOf(
        "fn", "var", "let", "co", "con",
        "if", "else", "while", "return",
        "true", "false", "extern", "print_str", "print_int", "exit"
    )

    override fun start(buffer: CharSequence, startOffset: Int, endOffset: Int, initialState: Int) {
        this.buffer = buffer
        this.startOffset = startOffset
        this.endOffset = endOffset
        this.tokenStart = startOffset
        this.tokenEnd = startOffset
        this.tokenType = null
        advance()
    }

    override fun getState(): Int = 0

    override fun getTokenType(): IElementType? = tokenType

    override fun getTokenStart(): Int = tokenStart

    override fun getTokenEnd(): Int = tokenEnd

    override fun getBufferSequence(): CharSequence = buffer

    override fun getBufferEnd(): Int = endOffset

    override fun advance() {
        tokenType = null
        var i = tokenEnd
        val n = endOffset
        if (i >= n) return

        // skip whitespace
        while (i < n && buffer[i].isWhitespace()) i++
        if (i >= n) {
            tokenStart = i
            tokenEnd = i
            tokenType = null
            return
        }

        tokenStart = i

        // comments: // to eol
        if (buffer[i] == '/' && i + 1 < n && buffer[i + 1] == '/') {
            i += 2
            while (i < n && buffer[i] != '\n') i++
            tokenEnd = i
            tokenType = AurumTokenTypes.COMMENT
            return
        }

        // string literal
        if (buffer[i] == '"') {
            i++
            while (i < n) {
                if (buffer[i] == '\\') {
                    i += 2
                } else if (buffer[i] == '"') {
                    i++
                    break
                } else {
                    i++
                }
            }
            tokenEnd = i
            tokenType = AurumTokenTypes.STRING
            return
        }

        // number
        val c = buffer[i]
        if (c.isDigit()) {
            i++
            while (i < n && buffer[i].isDigit()) i++
            tokenEnd = i
            tokenType = AurumTokenTypes.NUMBER
            return
        }

        // identifier or keyword
        if (c.isLetter() || c == '_') {
            i++
            while (i < n && (buffer[i].isLetterOrDigit() || buffer[i] == '_')) i++
            val word = buffer.subSequence(tokenStart, i).toString()
            tokenEnd = i
            tokenType = if (keywords.contains(word)) AurumTokenTypes.KEYWORD else AurumTokenTypes.IDENT
            return
        }

        // two-char operators
        if (i + 1 < n) {
            val two = buffer.subSequence(i, i + 2).toString()
            when (two) {
                "==", "!=", "<=", ">=", "&&", "||", "<<", ">>", ":=" -> {
                    tokenEnd = i + 2
                    tokenType = AurumTokenTypes.OP
                    return
                }
            }
        }

        // single-char operators / braces
        val ch = buffer[i]
        val ops = "+-*/%&|^~!<>=(){}[],;:".toSet()
        if (ops.contains(ch)) {
            tokenEnd = i + 1
            tokenType = if ("(){}[],;".contains(ch)) AurumTokenTypes.BRACE else AurumTokenTypes.OP
            return
        }

        // unknown
        tokenEnd = i + 1
        tokenType = AurumTokenTypes.BAD_CHAR
    }
}
