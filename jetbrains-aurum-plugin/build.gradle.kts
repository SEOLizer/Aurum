plugins {
    kotlin("jvm") version "2.3.10"
    id("org.jetbrains.intellij.platform") version "2.11.0"
}

group = "org.aurum"
version = "0.1.0"

repositories {
    mavenCentral()
    intellijPlatform {
        defaultRepositories()
    }
}

dependencies {
    intellijPlatform {
        // Minimal: IntelliJ IDEA 2024.3 (Beispiel aus den JetBrains-Doks)
        intellijIdea("2024.3")

        // Wenn du wirklich PhpStorm targeten willst:
        // create("PS", "2024.3")
    }
}

java {
    toolchain {
        languageVersion.set(JavaLanguageVersion.of(17))
    }
}

tasks.withType<org.jetbrains.kotlin.gradle.tasks.KotlinCompile>().configureEach {
    compilerOptions {
        jvmTarget.set(org.jetbrains.kotlin.gradle.dsl.JvmTarget.JVM_17)
    }
}
