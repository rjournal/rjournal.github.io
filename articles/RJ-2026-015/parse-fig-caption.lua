-- parse-fig-caption.lua
-- This filter processes figure captions as Markdown for HTML output only.
function Image(elem)
  if FORMAT:match("html") and elem.caption and #elem.caption > 0 then
    local cap_str = pandoc.utils.stringify(elem.caption)
    local parsed = pandoc.read(cap_str, "markdown").blocks
    elem.caption = parsed
  end
  return elem
end
