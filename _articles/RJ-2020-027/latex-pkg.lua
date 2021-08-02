
function Str(elem)
  local out = {}
  table.insert(out, pandoc.Str(string.gsub(elem.text, '\\CRANpkg%((%w+)%)', '**%1**')))
  
  return out
end
