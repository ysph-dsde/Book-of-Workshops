local function readMeta(meta)
    if meta['embed-fonts'] then
        quarto.doc.add_html_dependency({
            name = "bookup_fonts_gwf",
            version = "0.0",
            stylesheets = { "fonts-embed.css" }
        })
    else
        quarto.doc.add_html_dependency({
            name = "bookup_fonts_embed",
            version = "0.0",
            stylesheets = { "fonts-download.css" }
        })
    end
end

return {
    { Meta = readMeta }
}