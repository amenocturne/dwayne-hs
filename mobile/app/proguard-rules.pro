# Kotlinx Serialization
-keepattributes *Annotation*, InnerClasses
-dontnote kotlinx.serialization.AnnotationsKt
-keepclassmembers class kotlinx.serialization.json.** { *** Companion; }
-keepclasseswithmembers class kotlinx.serialization.json.** { kotlinx.serialization.KSerializer serializer(...); }
-keep,includedescriptorclasses class com.skril.dwayne.**$$serializer { *; }
-keepclassmembers class com.skril.dwayne.** { *** Companion; }
-keepclasseswithmembers class com.skril.dwayne.** { kotlinx.serialization.KSerializer serializer(...); }
