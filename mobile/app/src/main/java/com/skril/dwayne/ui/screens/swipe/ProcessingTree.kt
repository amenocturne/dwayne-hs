package com.skril.dwayne.ui.screens.swipe

import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.toArgb
import com.skril.dwayne.ui.theme.KeywordDefer
import com.skril.dwayne.ui.theme.KeywordDone
import com.skril.dwayne.ui.theme.KeywordSoon
import com.skril.dwayne.ui.theme.KeywordToday
import com.skril.dwayne.ui.theme.KeywordTodo
import com.skril.dwayne.ui.theme.KeywordTrash
import com.skril.dwayne.ui.theme.KeywordWaiting
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

// Clockwise. Index = ordinal; Branch.children stores nodes by this index.
enum class Dir { Up, UpRight, Right, DownRight, Down, DownLeft, Left, UpLeft }

fun Dir.opposite(): Dir = Dir.entries[(ordinal + 4) % 8]

val Dir.glyph: String
    get() = when (this) {
        Dir.Up -> "↑"
        Dir.UpRight -> "↗"
        Dir.Right -> "→"
        Dir.DownRight -> "↘"
        Dir.Down -> "↓"
        Dir.DownLeft -> "↙"
        Dir.Left -> "←"
        Dir.UpLeft -> "↖"
    }

const val ChildrenSlots = 8

sealed class Modification {
    data class SetKeyword(val keyword: String) : Modification()
}

sealed class TreeNode {
    abstract val label: String
    abstract val color: Color
}

data class Terminal(
    override val label: String,
    override val color: Color,
    val modification: Modification,
) : TreeNode()

data class Branch(
    override val label: String,
    override val color: Color,
    val children: List<TreeNode?>, // size == ChildrenSlots, index aligns with Dir.ordinal
) : TreeNode()

fun Branch.childAt(dir: Dir): TreeNode? = children.getOrNull(dir.ordinal)

fun Branch.withChild(dir: Dir, node: TreeNode?): Branch =
    copy(children = children.toMutableList().also { it[dir.ordinal] = node })

fun Branch.withChildren(newChildren: List<TreeNode?>): Branch =
    copy(children = newChildren.let {
        if (it.size == ChildrenSlots) it else it.padNullsTo(ChildrenSlots)
    })

private fun List<TreeNode?>.padNullsTo(size: Int): List<TreeNode?> =
    if (this.size >= size) take(size) else this + List(size - this.size) { null }

fun emptyBranch(label: String, color: Color): Branch =
    Branch(label = label, color = color, children = List(ChildrenSlots) { null })

val DefaultProcessingTree: Branch = Branch(
    label = "root",
    color = Color.Unspecified,
    children = listOf(
        Terminal("TODAY", KeywordToday, Modification.SetKeyword("TODAY")),
        Terminal("SOON", KeywordSoon, Modification.SetKeyword("SOON")),
        Terminal("TODO", KeywordTodo, Modification.SetKeyword("TODO")),
        Terminal("DONE", KeywordDone, Modification.SetKeyword("DONE")),
        Terminal("TRASH", KeywordTrash, Modification.SetKeyword("TRASH")),
        Terminal("WAITING", KeywordWaiting, Modification.SetKeyword("WAITING")),
        Terminal("SOMEDAY", KeywordWaiting, Modification.SetKeyword("SOMEDAY")),
        Terminal("DEFER", KeywordDefer, Modification.SetKeyword("DEFER")),
    ),
)

fun migrateProcessingTree(tree: Branch): Branch =
    tree.copy(children = tree.children.map(::migrateProcessingNode))

private fun migrateProcessingNode(node: TreeNode?): TreeNode? = when (node) {
    null -> null
    is Branch -> migrateProcessingTree(node)
    is Terminal -> {
        val setKeyword = node.modification as? Modification.SetKeyword
        if (setKeyword?.keyword == "PROJECT") {
            Terminal("DEFER", KeywordDefer, Modification.SetKeyword("DEFER"))
        } else {
            node
        }
    }
}

// --- Persistence: intermediate JSON-shaped types ----------------------------

@Serializable
sealed class TreeNodeJson {
    abstract val label: String
    abstract val colorArgb: Int
}

@Serializable
@SerialName("terminal")
data class TerminalJson(
    override val label: String,
    override val colorArgb: Int,
    val modification: ModificationJson,
) : TreeNodeJson()

@Serializable
@SerialName("branch")
data class BranchJson(
    override val label: String,
    override val colorArgb: Int,
    val children: List<TreeNodeJson?>,
) : TreeNodeJson()

@Serializable
sealed class ModificationJson {
    @Serializable
    @SerialName("set-keyword")
    data class SetKeyword(val keyword: String) : ModificationJson()
}

fun TreeNode.toJson(): TreeNodeJson = when (this) {
    is Terminal -> TerminalJson(label, color.toArgb(), modification.toJson())
    is Branch -> BranchJson(label, color.toArgb(), children.map { it?.toJson() })
}

private fun Modification.toJson(): ModificationJson = when (this) {
    is Modification.SetKeyword -> ModificationJson.SetKeyword(keyword)
}

fun TreeNodeJson.toRuntime(): TreeNode = when (this) {
    is TerminalJson -> Terminal(label, Color(colorArgb), modification.toRuntime())
    is BranchJson -> Branch(
        label = label,
        color = Color(colorArgb),
        children = children
            .map { it?.toRuntime() }
            .let { if (it.size == ChildrenSlots) it else it.padNullsTo(ChildrenSlots) },
    )
}

private fun ModificationJson.toRuntime(): Modification = when (this) {
    is ModificationJson.SetKeyword -> Modification.SetKeyword(keyword)
}
