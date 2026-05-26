i_task = interfacer::iface(
  filename = character ~ "The relative path to a file from the project root",
  timestamp = integer ~ "The last modified timestamp of the file",
  status = interfacer::enum(
    PENDING,
    IN_PROGRESS,
    COMPLETE,
    CANCELLED
  ) ~ "The task status",
  details = character ~ "The task details"
)


i_file_revision = interfacer::iface(
  revision_id = character + unique_id ~ "A uuid of this file revision",
  filename = character ~ "The relative path to a file from the project root",
  timestamp = integer ~ "The last modified timestamp of the file",
  status = interfacer::enum(
    ACTIVE,
    SUPERCEDED,
    DELETED
  ) ~ "The status of the revision"
)

i_revision_history = interfacer::iface(
  original_revision_id = character ~ "A uuid of the old file revision",
  new_revision_id = character ~ "A uuid of the new file revision",
)

i_revision_task = interfacer::iface(
  revision_id = character + unique_id ~ "A uuid of this file revision",
  process = interfacer::enum(
    PENDING,
    IN_PROGRESS,
    COMPLETE,
    INACTIVE
  ) ~ "The processing status of the file"
)

i_nodes = interfacer::iface(
  revision_id = character ~ "A uuid of this file revision",
  start = integer ~ "The start position in the file of the symbol",
  end = integer ~ "The end position in the file of the symbol",
  name = character ~ "The name of the symbol",
  kind = integer ~ "The type of the resource (See: SymbolKind list for values)"
)
