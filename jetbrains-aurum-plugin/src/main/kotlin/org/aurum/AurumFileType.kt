package org.aurum

import com.intellij.openapi.fileTypes.LanguageFileType
import com.intellij.openapi.fileTypes.FileTypeConsumer
import com.intellij.openapi.fileTypes.FileTypeFactory
import javax.swing.Icon

class AurumFileType : LanguageFileType(AurumLanguage) {
    companion object {
        val INSTANCE = AurumFileType()
    }

    override fun getName(): String = "Aurum"
    override fun getDescription(): String = "Aurum source file"
    override fun getDefaultExtension(): String = "au"
    override fun getIcon(): Icon? = null
}

class AurumFileTypeFactory : FileTypeFactory() {
    override fun createFileTypes(consumer: FileTypeConsumer) {
        consumer.consume(AurumFileType.INSTANCE, "au")
    }
}
