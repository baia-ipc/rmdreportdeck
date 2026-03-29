local has_named_default_section = false
local opened_first_section = false
local default_open_identifiers = {
  goal = true,
  description = true,
  methods = true,
  workflow = true,
  pipeline = true
}

local function has_class(el, class_name)
  for _, class in ipairs(el.classes) do
    if class == class_name then
      return true
    end
  end
  return false
end

local function escape_html(text)
  local escaped = text:gsub("&", "&amp;")
  escaped = escaped:gsub("<", "&lt;")
  escaped = escaped:gsub(">", "&gt;")
  escaped = escaped:gsub('"', "&quot;")
  return escaped
end

local function clone_classes(classes)
  local copied = {}
  for _, class in ipairs(classes) do
    table.insert(copied, class)
  end
  return copied
end

local function is_named_default_section(header)
  return default_open_identifiers[header.identifier] == true
end

local function should_open_section(header)
  if has_class(header, "open-by-default") or is_named_default_section(header) then
    return true
  end

  if not has_named_default_section and not opened_first_section then
    opened_first_section = true
    return true
  end

  return false
end

local function build_section_body(header, blocks)
  local processed_blocks = wrap_level2_sections(blocks)
  local classes = { "section", "level1" }
  for _, class in ipairs(clone_classes(header.classes)) do
    table.insert(classes, class)
  end
  table.insert(classes, "report-section-body")

  return pandoc.Div(processed_blocks, pandoc.Attr("", classes, header.attributes))
end

local function build_subsection_body(header, blocks)
  local classes = { "report-subsection-body" }
  for _, class in ipairs(clone_classes(header.classes)) do
    table.insert(classes, class)
  end

  return pandoc.Div(blocks, pandoc.Attr("", classes, header.attributes))
end

local function wrap_subsection(header, blocks)
  if has_class(header, "no-collapse") then
    local output = { header }
    for _, block in ipairs(blocks) do
      table.insert(output, block)
    end
    return output
  end

  local title = escape_html(pandoc.utils.stringify(header.content))
  local detail_attrs = 'class="report-subsection"'
  if header.identifier ~= "" then
    detail_attrs = 'id="' .. escape_html(header.identifier) .. '" ' .. detail_attrs
  end
  if has_class(header, "open-by-default") then
    detail_attrs = detail_attrs .. " open"
  end

  return {
    pandoc.RawBlock("html", "<details " .. detail_attrs .. '><summary><span class="report-subsection-title">' .. title .. "</span></summary>"),
    build_subsection_body(header, blocks),
    pandoc.RawBlock("html", "</details>")
  }
end

function wrap_level2_sections(blocks)
  local rebuilt = {}
  local current_header = nil
  local current_blocks = {}

  local function flush_subsection()
    if current_header == nil then
      return
    end

    for _, block in ipairs(wrap_subsection(current_header, current_blocks)) do
      table.insert(rebuilt, block)
    end

    current_header = nil
    current_blocks = {}
  end

  for _, block in ipairs(blocks) do
    if block.t == "Header" and block.level == 2 then
      flush_subsection()
      current_header = block
    elseif current_header ~= nil then
      table.insert(current_blocks, block)
    else
      table.insert(rebuilt, block)
    end
  end

  flush_subsection()
  return rebuilt
end

local function wrap_section(header, blocks)
  if has_class(header, "no-collapse") then
    local output = { header }
    for _, block in ipairs(blocks) do
      table.insert(output, block)
    end
    return output
  end

  local title = escape_html(pandoc.utils.stringify(header.content))
  local detail_attrs = 'class="report-section"'
  if header.identifier ~= "" then
    detail_attrs = 'id="' .. escape_html(header.identifier) .. '" ' .. detail_attrs
  end
  if should_open_section(header) then
    detail_attrs = detail_attrs .. " open"
  end

  return {
    pandoc.RawBlock("html", "<details " .. detail_attrs .. '><summary><span class="report-section-title">' .. title .. "</span></summary>"),
    build_section_body(header, blocks),
    pandoc.RawBlock("html", "</details>")
  }
end

function Pandoc(doc)
  has_named_default_section = false
  opened_first_section = false

  for _, block in ipairs(doc.blocks) do
    if block.t == "Header" and block.level == 1 and is_named_default_section(block) then
      has_named_default_section = true
      break
    end
  end

  local rebuilt = {}
  local current_header = nil
  local current_blocks = {}

  local function flush_section()
    if current_header == nil then
      return
    end

    for _, block in ipairs(wrap_section(current_header, current_blocks)) do
      table.insert(rebuilt, block)
    end

    current_header = nil
    current_blocks = {}
  end

  for _, block in ipairs(doc.blocks) do
    if block.t == "Header" and block.level == 1 then
      flush_section()
      current_header = block
    elseif current_header ~= nil then
      table.insert(current_blocks, block)
    else
      table.insert(rebuilt, block)
    end
  end

  flush_section()
  doc.blocks = rebuilt
  return doc
end
