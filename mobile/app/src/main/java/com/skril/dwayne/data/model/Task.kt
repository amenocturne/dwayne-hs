package com.skril.dwayne.data.model

import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class Task(
    val level: Int,
    val todoKeyword: String,
    val priority: Int? = null,
    val title: List<TextNode>,
    val tags: List<String> = emptyList(),
    val scheduled: OrgTime? = null,
    val deadline: OrgTime? = null,
    val createdProp: OrgTime? = null,
    val closed: OrgTime? = null,
    val properties: List<List<String>> = emptyList(),
    val description: List<TextNode> = emptyList(),
)

@Serializable
sealed class TextNode {
    @Serializable
    @SerialName("plain")
    data class Plain(val text: String) : TextNode()

    @Serializable
    @SerialName("link")
    data class Link(val url: String, val title: String? = null) : TextNode()
}

@Serializable
data class OrgTime(
    val date: String,
    val time: String? = null,
    val repeater: Repeater? = null,
    val delay: Delay? = null,
)

@Serializable
data class Repeater(
    val type: String,
    val value: Int,
    val unit: String,
)

@Serializable
data class Delay(
    val type: String,
    val value: Int,
    val unit: String,
)

@Serializable
data class TaskPointer(
    val file: String,
    val taskIndex: Int,
)

@Serializable
data class TaskWithPointer(
    val task: Task,
    val pointer: TaskPointer,
)

@Serializable
data class PaginatedResponse(
    val data: List<TaskWithPointer>,
    val metadata: PaginationMetadata,
)

@Serializable
data class PaginationMetadata(
    val total: Int,
)

@Serializable
data class ProjectTreeNode(
    val task: Task,
    val pointer: TaskPointer,
    val children: List<ProjectTreeNode> = emptyList(),
)

@Serializable
data class ProjectTreeResponse(
    val root: ProjectTreeNode,
)

// Request types

@Serializable
data class CaptureRequest(
    val captureTitle: String,
)

@Serializable
data class ChangeKeywordRequest(
    val ckrFile: String,
    val ckrTaskIndex: Int,
    val ckrKeyword: String,
)

@Serializable
data class ChangePriorityRequest(
    val cprFile: String,
    val cprTaskIndex: Int,
    val cprPriority: Int?,
)

@Serializable
data class TagRequest(
    val trFile: String,
    val trTaskIndex: Int,
    val trTag: String,
)

@Serializable
data class TaskPointerRequest(
    val tprFile: String,
    val tprTaskIndex: Int,
)
