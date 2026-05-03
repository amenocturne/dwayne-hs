package com.skril.dwayne.data.mock

import com.skril.dwayne.data.model.*

object MockData {

    private fun plainText(text: String) = TextNode.Plain(text)
    private fun linkText(url: String, title: String? = null) = TextNode.Link(url, title)
    private fun date(d: String) = OrgTime(date = d)
    private fun dateTime(d: String, t: String) = OrgTime(date = d, time = t)

    val inboxTasks = listOf(
        TaskWithPointer(
            task = Task(
                level = 1,
                todoKeyword = "INBOX",
                title = listOf(plainText("Research Kotlin Multiplatform for shared logic")),
                tags = listOf("dev", "research"),
                createdProp = date("2026-03-05"),
                description = listOf(plainText("Check if KMP can replace the Haskell cross-compile approach")),
            ),
            pointer = TaskPointer(file = "/inbox.org", taskIndex = 0),
        ),
        TaskWithPointer(
            task = Task(
                level = 1,
                todoKeyword = "INBOX",
                title = listOf(plainText("Buy new headphone pads")),
                tags = listOf("errands"),
                createdProp = date("2026-03-06"),
            ),
            pointer = TaskPointer(file = "/inbox.org", taskIndex = 1),
        ),
        TaskWithPointer(
            task = Task(
                level = 1,
                todoKeyword = "INBOX",
                title = listOf(
                    plainText("Watch "),
                    linkText("https://youtu.be/example", "Compose animations talk"),
                ),
                createdProp = date("2026-03-07"),
            ),
            pointer = TaskPointer(file = "/inbox.org", taskIndex = 2),
        ),
        TaskWithPointer(
            task = Task(
                level = 1,
                todoKeyword = "INBOX",
                title = listOf(plainText("Fix broken headphone jack on mixer")),
                tags = listOf("studio"),
                createdProp = date("2026-03-07"),
                description = listOf(plainText("Right channel cuts out intermittently")),
            ),
            pointer = TaskPointer(file = "/inbox.org", taskIndex = 3),
        ),
    )

    val todayTasks = listOf(
        TaskWithPointer(
            task = Task(
                level = 1,
                todoKeyword = "TODAY",
                priority = 1,
                title = listOf(plainText("Deploy Dwayne API to home server")),
                tags = listOf("dev", "infra"),
                scheduled = date("2026-03-07"),
                createdProp = date("2026-03-01"),
                description = listOf(plainText("Fix reverse tunnel, run full Ansible playbook")),
            ),
            pointer = TaskPointer(file = "/tasks.org", taskIndex = 0),
        ),
        TaskWithPointer(
            task = Task(
                level = 1,
                todoKeyword = "TODAY",
                priority = 2,
                title = listOf(plainText("Review PR for postquill telegram bot")),
                tags = listOf("dev"),
                scheduled = date("2026-03-07"),
                createdProp = date("2026-03-06"),
            ),
            pointer = TaskPointer(file = "/tasks.org", taskIndex = 1),
        ),
        TaskWithPointer(
            task = Task(
                level = 1,
                todoKeyword = "TODAY",
                priority = 3,
                title = listOf(plainText("Grocery run — eggs, milk, coffee")),
                tags = listOf("errands"),
                createdProp = date("2026-03-07"),
            ),
            pointer = TaskPointer(file = "/tasks.org", taskIndex = 2),
        ),
    )

    val soonTasks = listOf(
        TaskWithPointer(
            task = Task(
                level = 1,
                todoKeyword = "SOON",
                priority = 1,
                title = listOf(plainText("Set up Authelia for API authentication")),
                tags = listOf("dev", "security"),
                deadline = dateTime("2026-03-15", "17:00"),
                createdProp = date("2026-02-28"),
            ),
            pointer = TaskPointer(file = "/tasks.org", taskIndex = 3),
        ),
        TaskWithPointer(
            task = Task(
                level = 1,
                todoKeyword = "SOON",
                priority = 2,
                title = listOf(plainText("Write mobile app scaffold spec")),
                tags = listOf("dev", "dwayne"),
                createdProp = date("2026-03-05"),
            ),
            pointer = TaskPointer(file = "/tasks.org", taskIndex = 4),
        ),
    )

    val todoTasks = listOf(
        TaskWithPointer(
            task = Task(
                level = 1,
                todoKeyword = "TODO",
                priority = 1,
                title = listOf(plainText("Add vault reader API endpoints")),
                tags = listOf("dev", "dwayne"),
                createdProp = date("2026-02-20"),
                description = listOf(plainText("Serve Mirror vault files for mobile reader")),
            ),
            pointer = TaskPointer(file = "/tasks.org", taskIndex = 5),
        ),
        TaskWithPointer(
            task = Task(
                level = 1,
                todoKeyword = "TODO",
                title = listOf(plainText("Clean up studio cable management")),
                tags = listOf("studio"),
                createdProp = date("2026-02-15"),
            ),
            pointer = TaskPointer(file = "/tasks.org", taskIndex = 6),
        ),
    )

    val workQueueTasks = todayTasks + soonTasks

    val allTasks = inboxTasks + todayTasks + soonTasks + todoTasks

    fun paginatedResponse(tasks: List<TaskWithPointer>, offset: Int = 0, limit: Int = 100): PaginatedResponse {
        val page = tasks.drop(offset).take(limit)
        return PaginatedResponse(data = page, metadata = PaginationMetadata(total = tasks.size))
    }
}
