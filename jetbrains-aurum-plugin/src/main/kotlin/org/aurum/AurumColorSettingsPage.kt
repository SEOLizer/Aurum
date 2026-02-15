package org.aurum

import com.intellij.openapi.options.colors.AttributesDescriptor
import com.intellij.openapi.options.colors.ColorSettingsPage
import javax.swing.Icon
import com.intellij.openapi.editor.colors.TextAttributesKey

class AurumColorSettingsPage : ColorSettingsPage {
    private val DESCRIPTORS = arrayOf(
        AttributesDescriptor("Keyword", AurumSyntaxHighlighter.KEYWORD),
        AttributesDescriptor("Identifier", AurumSyntaxHighlighter.IDENT),
        AttributesDescriptor("Number", AurumSyntaxHighlighter.NUMBER),
        AttributesDescriptor("String", AurumSyntaxHighlighter.STRING),
        AttributesDescriptor("Comment", AurumSyntaxHighlighter.COMMENT),
        AttributesDescriptor("Operator", AurumSyntaxHighlighter.OP),
        AttributesDescriptor("Braces", AurumSyntaxHighlighter.BRACE)
    )

    override fun getDisplayName(): String = "Aurum"
    override fun getIcon(): Icon? = null
    override fun getHighlighter() = AurumSyntaxHighlighter()
    override fun getDemoText(): String = "fn main(): int64 {\n  // Hello\n  print_str(\"Hello Aurum\\n\");\n  return 0;\n}\n"
    override fun getAdditionalHighlightingTagToDescriptorMap(): MutableMap<String, TextAttributesKey>? = null
    override fun getAttributeDescriptors(): Array<AttributesDescriptor> = DESCRIPTORS
    override fun getColorDescriptors(): Array<com.intellij.openapi.options.colors.ColorDescriptor> = com.intellij.openapi.options.colors.ColorDescriptor.EMPTY_ARRAY
}
