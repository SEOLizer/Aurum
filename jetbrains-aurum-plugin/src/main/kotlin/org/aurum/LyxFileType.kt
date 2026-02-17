package org.aurum

import com.intellij.openapi.fileTypes.LanguageFileType
import com.intellij.openapi.fileTypes.FileTypeConsumer
import com.intellij.openapi.fileTypes.FileTypeFactory
import javax.swing.Icon

class LyxFileType : LanguageFileType(LyxLanguage) {
    companion object {
        val INSTANCE = LyxFileType()
    }

    override fun getName(): String = "Lyx"
    override fun getDescription(): String = "Lyx source file"
    override fun getDefaultExtension(): String = "lyx"
    override fun getIcon(): Icon? = null
}

class LyxFileTypeFactory : FileTypeFactory() {
    override fun createFileTypes(consumer: FileTypeConsumer) {
        consumer.consume(LyxFileType.INSTANCE, "lyx")
    }
}
