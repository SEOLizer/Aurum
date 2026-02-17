package org.aurum

import com.intellij.openapi.options.colors.AttributesDescriptor
import com.intellij.openapi.options.colors.ColorSettingsPage
import javax.swing.Icon
import com.intellij.openapi.editor.colors.TextAttributesKey

class LyxColorSettingsPage : ColorSettingsPage {
    private val DESCRIPTORS = arrayOf(
        AttributesDescriptor("Keyword", LyxSyntaxHighlighter.KEYWORD),
        AttributesDescriptor("Identifier", LyxSyntaxHighlighter.IDENT),
        AttributesDescriptor("Number", LyxSyntaxHighlighter.NUMBER),
        AttributesDescriptor("String", LyxSyntaxHighlighter.STRING),
        AttributesDescriptor("Comment", LyxSyntaxHighlighter.COMMENT),
        AttributesDescriptor("Operator", LyxSyntaxHighlighter.OP),
        AttributesDescriptor("Braces", LyxSyntaxHighlighter.BRACE)
    )

    override fun getDisplayName(): String = "Lyx"
    override fun getIcon(): Icon? = null
    override fun getHighlighter() = LyxSyntaxHighlighter()
    override fun getDemoText(): String = "fn main(): int64 {\n  // Hello\n  print_str(\"Hello Lyx\\n\");\n  return 0;\n}\n"
    override fun getAdditionalHighlightingTagToDescriptorMap(): MutableMap<String, TextAttributesKey>? = null
    override fun getAttributeDescriptors(): Array<AttributesDescriptor> = DESCRIPTORS
    override fun getColorDescriptors(): Array<com.intellij.openapi.options.colors.ColorDescriptor> = com.intellij.openapi.options.colors.ColorDescriptor.EMPTY_ARRAY
}
