local files = require "files"
local guide = require "core.guide"
local util  = require 'utility'

local Care = {
    ['function'] = function (source, text, results)
        local folding = {
            start  = source.start,
            finish = source.finish,
            kind   = 'region',
        }
        results[#results+1] = folding
        if source.bindDocs then
            results[#results+1] = {
                start        = source.bindDocs[1].start,
                finish       = source.bindDocs[#source.bindDocs].finish,
                kind         = 'comment',
                hideLastLine = true,
            }
        end
    end,
    ['do'] = function (source, text, results)
        local folding = {
            start  = source.start,
            finish = source.finish,
            kind   = 'region',
        }
        results[#results+1] = folding
    end,
    ['if'] = function (source, text, results)
        for i = 1, #source do
            local block = source[i]
            local nblock = source[i + 1]
            results[#results+1] = {
                start  = block.start,
                finish = nblock and nblock.start or source.finish,
                kind   = 'region',
            }
        end
    end,
    ['loop'] = function (source, text, results)
        local folding = {
            start  = source.start,
            finish = source.finish,
            kind   = 'region',
        }
        results[#results+1] = folding
    end,
    ['in'] = function (source, text, results)
        local folding = {
            start  = source.start,
            finish = source.finish,
            kind   = 'region',
        }
        results[#results+1] = folding
    end,
    ['while'] = function (source, text, results)
        local folding = {
            start  = source.start,
            finish = source.finish,
            kind   = 'region',
        }
        results[#results+1] = folding
    end,
    ['repeat'] = function (source, text, results)
        local start  = source.start
        local finish = source.keyword[#source.keyword]
        if text:sub(finish - #'until' + 1, finish) ~= 'until' then
            return
        end
        local folding = {
            start  = start,
            finish = finish,
            kind   = 'region',
        }
        results[#results+1] = folding
    end,
    ['table'] = function (source, text, results)
        local folding = {
            start  = source.start,
            finish = source.finish,
            kind   = 'region',
        }
        results[#results+1] = folding
    end,
    ['string'] = function (source, text, results)
        local folding = {
            start  = source.start,
            finish = source.finish,
            kind   = 'region',
        }
        results[#results+1] = folding
    end,
    ['comment.short'] = function (source, text, results, status)
        local ltext = source.text:lower()
        ltext = util.trim(ltext, 'left')
        if ltext:sub(1, #'region') == 'region'
        or ltext:sub(1, #'#region') == '#region' then
            if not status.regions then
                status.regions = {}
            end
            status.regions[#status.regions+1] = source
        elseif ltext:sub(1, #'endregion') == 'endregion'
        or     ltext:sub(1, #'#endregion') == '#endregion' then
            if not status.regions then
                status.regions = {}
            end
            local start = table.remove(status.regions)
            if not start then
                return
            end
            results[#results+1] = {
                start        = start.start,
                finish       = source.finish,
                kind         = 'region',
                hideLastLine = true,
            }
        end
    end,
    ['comment.long'] = function (source, text, results)
        local folding = {
            start  = source.start,
            finish = source.finish,
            kind   = 'comment',
        }
        results[#results+1] = folding
    end,
    ['comment.clong'] = function (source, text, results)
        local folding = {
            start  = source.start,
            finish = source.finish,
            kind   = 'comment',
        }
        results[#results+1] = folding
    end,
    ['doc.class'] = function (source, text, results)
        local folding = {
            start        = source.start,
            finish       = source.bindGroup[#source.bindGroup].finish,
            kind         = 'comment',
            hideLastLine = true,
        }
        results[#results+1] = folding
    end,
}

return function (uri)
    local ast  = files.getAst(uri)
    local text = files.getText(uri)
    if not ast or not text then
        return nil
    end
    local regions = {}
    local status = {}

    guide.eachSource(ast.ast, function (source)
        local tp = source.type
        if Care[tp] then
            Care[tp](source, text, regions)
        end
    end)
    for _, source in ipairs(ast.comms) do
        local tp = source.type
        if Care[tp] then
            Care[tp](source, text, regions, status)
        end
    end

    return regions
end
