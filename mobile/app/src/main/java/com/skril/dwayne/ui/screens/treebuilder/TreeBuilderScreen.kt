package com.skril.dwayne.ui.screens.treebuilder

import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.automirrored.filled.ArrowBack
import androidx.compose.material.icons.automirrored.filled.KeyboardArrowRight
import androidx.compose.material.icons.filled.ArrowDownward
import androidx.compose.material.icons.filled.ArrowUpward
import androidx.compose.material.icons.filled.Delete
import androidx.compose.material.icons.filled.Edit
import androidx.compose.material.icons.filled.Refresh
import androidx.compose.material3.AlertDialog
import androidx.compose.material3.AssistChip
import androidx.compose.material3.AssistChipDefaults
import androidx.compose.material3.ExperimentalMaterial3Api
import androidx.compose.material3.FilterChip
import androidx.compose.material3.HorizontalDivider
import androidx.compose.material3.Icon
import androidx.compose.material3.IconButton
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.OutlinedButton
import androidx.compose.material3.OutlinedTextField
import androidx.compose.material3.Scaffold
import androidx.compose.material3.Text
import androidx.compose.material3.TextButton
import androidx.compose.material3.TopAppBar
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.unit.dp
import com.skril.dwayne.data.repository.TreeStore
import com.skril.dwayne.ui.screens.swipe.Branch
import com.skril.dwayne.ui.screens.swipe.ChildrenSlots
import com.skril.dwayne.ui.screens.swipe.DefaultProcessingTree
import com.skril.dwayne.ui.screens.swipe.Dir
import com.skril.dwayne.ui.screens.swipe.Modification
import com.skril.dwayne.ui.screens.swipe.Terminal
import com.skril.dwayne.ui.screens.swipe.TreeNode
import com.skril.dwayne.ui.screens.swipe.emptyBranch
import com.skril.dwayne.ui.screens.swipe.glyph
import com.skril.dwayne.ui.theme.KeywordDone
import com.skril.dwayne.ui.theme.KeywordInbox
import com.skril.dwayne.ui.theme.KeywordProject
import com.skril.dwayne.ui.theme.KeywordSoon
import com.skril.dwayne.ui.theme.KeywordToday
import com.skril.dwayne.ui.theme.KeywordTodo
import com.skril.dwayne.ui.theme.KeywordTrash
import com.skril.dwayne.ui.theme.KeywordWaiting
import com.skril.dwayne.ui.theme.OnSurfaceVariant
import com.skril.dwayne.ui.theme.keywordColor
import kotlinx.coroutines.launch

private val palette: List<Pair<String, Color>> = listOf(
    "Today" to KeywordToday,
    "Soon" to KeywordSoon,
    "Todo" to KeywordTodo,
    "Done" to KeywordDone,
    "Trash" to KeywordTrash,
    "Waiting" to KeywordWaiting,
    "Project" to KeywordProject,
    "Inbox" to KeywordInbox,
    "Neutral" to OnSurfaceVariant,
)

private val keywordChoices = listOf(
    "INBOX", "DEFER", "TODAY", "SOON", "TODO", "RELEVANT",
    "SOMEDAY", "WAITING", "NOTES", "LIST", "DONE", "TRASH",
)

@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun TreeBuilderScreen(
    store: TreeStore,
    onBack: () -> Unit,
) {
    val rootTree by store.tree.collectAsState(initial = DefaultProcessingTree)
    // Path is a list of slot indices into nested Branches; empty = at root.
    var path by remember { mutableStateOf<List<Int>>(emptyList()) }
    val scope = rememberCoroutineScope()

    val currentBranch: Branch = remember(rootTree, path) {
        var node: Branch = rootTree
        for (idx in path) {
            val child = node.children.getOrNull(idx)
            node = child as? Branch ?: return@remember rootTree.also { path = emptyList() }
        }
        node
    }

    fun persist(newRoot: Branch) {
        scope.launch { store.save(newRoot) }
    }

    fun replaceCurrentBranch(transform: (Branch) -> Branch) {
        // Walk down 'path' building a new root with the transformed leaf.
        fun rebuild(node: Branch, depth: Int): Branch =
            if (depth >= path.size) transform(node)
            else {
                val idx = path[depth]
                val sub = node.children[idx] as? Branch ?: return node
                val rebuiltSub = rebuild(sub, depth + 1)
                Branch(
                    label = node.label,
                    color = node.color,
                    children = node.children.toMutableList()
                        .also { it[idx] = rebuiltSub },
                )
            }
        persist(rebuild(rootTree, 0))
    }

    var editing by remember { mutableStateOf<Pair<Int, TreeNode?>?>(null) }
    var resetConfirm by remember { mutableStateOf(false) }

    Scaffold(
        topBar = {
            TopAppBar(
                title = {
                    Text(
                        text = if (path.isEmpty()) "Processing tree"
                        else "Processing › ${currentBranch.label}",
                    )
                },
                navigationIcon = {
                    IconButton(onClick = {
                        if (path.isEmpty()) onBack() else path = path.dropLast(1)
                    }) {
                        Icon(Icons.AutoMirrored.Default.ArrowBack, contentDescription = "Back")
                    }
                },
                actions = {
                    if (path.isEmpty()) {
                        IconButton(onClick = { resetConfirm = true }) {
                            Icon(Icons.Default.Refresh, contentDescription = "Reset to default")
                        }
                    }
                },
            )
        },
    ) { padding ->
        Column(
            modifier = Modifier
                .fillMaxSize()
                .padding(padding)
                .padding(16.dp),
            verticalArrangement = Arrangement.spacedBy(8.dp),
        ) {
            Text(
                text = "Slots are clockwise from the top. Drag-to-reorder lands later — for now use the ↑/↓ buttons.",
                style = MaterialTheme.typography.labelMedium,
                color = MaterialTheme.colorScheme.onSurfaceVariant,
            )
            HorizontalDivider()

            for (i in 0 until ChildrenSlots) {
                val node = currentBranch.children.getOrNull(i)
                val dir = Dir.entries[i]
                SlotRow(
                    dir = dir,
                    node = node,
                    canMoveUp = i > 0,
                    canMoveDown = i < ChildrenSlots - 1,
                    onMoveUp = {
                        replaceCurrentBranch { b ->
                            b.copy(children = b.children.toMutableList().also {
                                val tmp = it[i]; it[i] = it[i - 1]; it[i - 1] = tmp
                            })
                        }
                    },
                    onMoveDown = {
                        replaceCurrentBranch { b ->
                            b.copy(children = b.children.toMutableList().also {
                                val tmp = it[i]; it[i] = it[i + 1]; it[i + 1] = tmp
                            })
                        }
                    },
                    onEdit = { editing = i to node },
                    onDelete = {
                        replaceCurrentBranch { b ->
                            b.copy(children = b.children.toMutableList().also { it[i] = null })
                        }
                    },
                    onDescend = if (node is Branch) {
                        { path = path + i }
                    } else null,
                )
            }
        }
    }

    val editingState = editing
    if (editingState != null) {
        EditNodeDialog(
            initial = editingState.second,
            onDismiss = { editing = null },
            onConfirm = { newNode ->
                val idx = editingState.first
                replaceCurrentBranch { b ->
                    b.copy(children = b.children.toMutableList().also { it[idx] = newNode })
                }
                editing = null
            },
        )
    }

    if (resetConfirm) {
        AlertDialog(
            onDismissRequest = { resetConfirm = false },
            title = { Text("Reset to default?") },
            text = { Text("Replaces the current tree with the built-in default.") },
            confirmButton = {
                TextButton(onClick = {
                    scope.launch { store.reset() }
                    resetConfirm = false
                    path = emptyList()
                }) { Text("Reset") }
            },
            dismissButton = {
                TextButton(onClick = { resetConfirm = false }) { Text("Cancel") }
            },
        )
    }
}

@Composable
private fun SlotRow(
    dir: Dir,
    node: TreeNode?,
    canMoveUp: Boolean,
    canMoveDown: Boolean,
    onMoveUp: () -> Unit,
    onMoveDown: () -> Unit,
    onEdit: () -> Unit,
    onDelete: () -> Unit,
    onDescend: (() -> Unit)?,
) {
    Row(
        modifier = Modifier
            .fillMaxWidth()
            .clip(RoundedCornerShape(8.dp))
            .background(MaterialTheme.colorScheme.surfaceVariant.copy(alpha = 0.3f))
            .let { if (onDescend != null) it.clickable(onClick = onDescend) else it }
            .padding(horizontal = 12.dp, vertical = 8.dp),
        verticalAlignment = Alignment.CenterVertically,
        horizontalArrangement = Arrangement.spacedBy(8.dp),
    ) {
        Text(
            text = dir.glyph,
            style = MaterialTheme.typography.titleLarge,
            color = MaterialTheme.colorScheme.onSurfaceVariant,
            modifier = Modifier.width(28.dp),
        )

        if (node == null) {
            Text(
                text = "(empty)",
                style = MaterialTheme.typography.bodyMedium,
                color = MaterialTheme.colorScheme.onSurfaceVariant,
                fontFamily = FontFamily.Monospace,
                modifier = Modifier.weight(1f),
            )
        } else {
            Box(
                modifier = Modifier
                    .size(14.dp)
                    .clip(CircleShape)
                    .background(node.color),
            )
            Spacer(modifier = Modifier.width(4.dp))
            Column(modifier = Modifier.weight(1f)) {
                Row(verticalAlignment = Alignment.CenterVertically) {
                    Text(
                        text = node.label,
                        style = MaterialTheme.typography.bodyLarge,
                    )
                    if (node is Branch) {
                        Spacer(modifier = Modifier.width(4.dp))
                        Icon(
                            imageVector = Icons.AutoMirrored.Default.KeyboardArrowRight,
                            contentDescription = "Branch",
                            tint = MaterialTheme.colorScheme.onSurfaceVariant,
                        )
                    }
                }
                Text(
                    text = when (node) {
                        is Terminal -> when (val m = node.modification) {
                            is Modification.SetKeyword -> "→ ${m.keyword}"
                        }
                        is Branch -> "${node.children.count { it != null }} sub-slots"
                    },
                    style = MaterialTheme.typography.labelSmall,
                    color = MaterialTheme.colorScheme.onSurfaceVariant,
                )
            }
        }

        IconButton(onClick = onMoveUp, enabled = canMoveUp) {
            Icon(Icons.Default.ArrowUpward, contentDescription = "Move up")
        }
        IconButton(onClick = onMoveDown, enabled = canMoveDown) {
            Icon(Icons.Default.ArrowDownward, contentDescription = "Move down")
        }
        IconButton(onClick = onEdit) {
            Icon(Icons.Default.Edit, contentDescription = if (node == null) "Add" else "Edit")
        }
        if (node != null) {
            IconButton(onClick = onDelete) {
                Icon(Icons.Default.Delete, contentDescription = "Delete")
            }
        }
    }
}

@OptIn(ExperimentalMaterial3Api::class)
@Composable
private fun EditNodeDialog(
    initial: TreeNode?,
    onDismiss: () -> Unit,
    onConfirm: (TreeNode) -> Unit,
) {
    var isBranch by remember(initial) {
        mutableStateOf(initial is Branch || initial == null)
            .also { /* default = Terminal for new (initial==null) so we can fill keyword fast */ }
    }
    // Override: default new nodes to Terminal
    LaunchedEffect(Unit) {
        if (initial == null) isBranch = false
    }

    var label by remember(initial) {
        mutableStateOf(initial?.label ?: "")
    }
    var color by remember(initial) {
        mutableStateOf(initial?.color ?: keywordColor("TODO"))
    }
    var keyword by remember(initial) {
        mutableStateOf(
            (initial as? Terminal)?.let { (it.modification as? Modification.SetKeyword)?.keyword }
                ?: "TODO",
        )
    }

    AlertDialog(
        onDismissRequest = onDismiss,
        title = { Text(if (initial == null) "Add slot" else "Edit slot") },
        text = {
            Column(
                modifier = Modifier.fillMaxWidth(),
                verticalArrangement = Arrangement.spacedBy(12.dp),
            ) {
                Row(horizontalArrangement = Arrangement.spacedBy(8.dp)) {
                    FilterChip(
                        selected = !isBranch,
                        onClick = { isBranch = false },
                        label = { Text("Terminal") },
                    )
                    FilterChip(
                        selected = isBranch,
                        onClick = { isBranch = true },
                        label = { Text("Branch") },
                    )
                }

                if (!isBranch) {
                    Column(verticalArrangement = Arrangement.spacedBy(4.dp)) {
                        Text(
                            text = "Keyword",
                            style = MaterialTheme.typography.labelMedium,
                            color = MaterialTheme.colorScheme.onSurfaceVariant,
                        )
                        Row(
                            modifier = Modifier.fillMaxWidth(),
                            horizontalArrangement = Arrangement.spacedBy(6.dp),
                        ) {
                            // Two columns of keyword chips so they fit
                        }
                        // Wrap keywordChoices in a flow-like layout using a simple Column of Rows
                        val rows = keywordChoices.chunked(3)
                        for (row in rows) {
                            Row(horizontalArrangement = Arrangement.spacedBy(6.dp)) {
                                for (kw in row) {
                                    FilterChip(
                                        selected = keyword == kw,
                                        onClick = {
                                            keyword = kw
                                            color = keywordColor(kw)
                                            if (label.isBlank()) label = kw
                                        },
                                        label = { Text(kw) },
                                    )
                                }
                            }
                        }
                    }
                }

                OutlinedTextField(
                    value = label,
                    onValueChange = { label = it },
                    modifier = Modifier.fillMaxWidth(),
                    label = { Text("Label") },
                    singleLine = true,
                )

                Column(verticalArrangement = Arrangement.spacedBy(4.dp)) {
                    Text(
                        text = "Color",
                        style = MaterialTheme.typography.labelMedium,
                        color = MaterialTheme.colorScheme.onSurfaceVariant,
                    )
                    val rows = palette.chunked(5)
                    for (row in rows) {
                        Row(horizontalArrangement = Arrangement.spacedBy(8.dp)) {
                            for ((name, swatch) in row) {
                                Box(
                                    modifier = Modifier
                                        .size(28.dp)
                                        .clip(CircleShape)
                                        .background(swatch)
                                        .clickable { color = swatch },
                                ) {
                                    if (color == swatch) {
                                        Box(
                                            modifier = Modifier
                                                .size(28.dp)
                                                .clip(CircleShape)
                                                .background(Color.White.copy(alpha = 0.2f))
                                        )
                                    }
                                }
                            }
                        }
                    }
                }
            }
        },
        confirmButton = {
            TextButton(
                onClick = {
                    val finalLabel = label.trim().ifBlank {
                        if (isBranch) "Branch" else keyword
                    }
                    val node: TreeNode = if (isBranch) {
                        // Preserve children if we were already a branch; otherwise empty.
                        when (initial) {
                            is Branch -> Branch(finalLabel, color, initial.children)
                            else -> emptyBranch(finalLabel, color)
                        }
                    } else {
                        Terminal(finalLabel, color, Modification.SetKeyword(keyword))
                    }
                    onConfirm(node)
                },
            ) { Text("Save") }
        },
        dismissButton = {
            TextButton(onClick = onDismiss) { Text("Cancel") }
        },
    )
}
