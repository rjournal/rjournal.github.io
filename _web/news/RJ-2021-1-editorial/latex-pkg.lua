local cranpkg = {
    html = 'https://cran.r-project.org/package=%1',
    pdf = '\\CRANpkg{%s}'
}
local biopkg = {
    html = 'https://www.bioconductor.org/packages/%1/',
    pdf = '\\BIOpkg{%s}'
}
local pkg = {
    html = '%1',
    pdf = '\\pkg{%s}'
}


function RawInline(elem)
  local out = {}
  
  local pkg = string.match(elem.text, '\\pkg%{(%w+)%}.*')
  
  if pkg ~= nil then
--     return pandoc.Para{pandoc.Strong(pkg)}
      
    if FORMAT:match 'html.*' then
        table.insert(out, pandoc.Link(pkg, '#'))
    else
        table.insert(out, pandoc.Str(string.gsub(elem.text, '%@pkg%((%w+)%)', pkg.cranpkg)))
    end
  else
    out = elem
  end
  return out
end
