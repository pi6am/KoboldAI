-- Improved repetition suppression
-- Makes sequences of tokens less likely to appear if they already have appeared in the context

kobold = require("bridge")()  -- This line is optional and is only for EmmyLua type annotations
require "io"
require "string"
require "table"

local userscript = {}  ---@class KoboldUserScript


-- TODO: Actually add a configuration file

---@class PhraseBiasEntry
---@field starting_bias number
---@field ending_bias number
---@field tokens table<integer, integer>
---@field n_tokens integer

local example_config = [[# Repetition bias parameters
#
# TODO: Actually implement a conf file
#
]]

-- If config file is empty, write example config
local f = kobold.get_config_file()
f:seek("set")
if f:read(1) == nil then
    f:write(example_config)
end
f:seek("set")
example_config = nil

-- Read config
--print("Loading repetition bias config...")
--local bias_array = {}  ---@type table<integer, PhraseBiasEntry>
--local bias_array_count = 0
--local val_count = 0
--local line_count = 0
--local row = {}  ---@type PhraseBiasEntry
--local val_orig
--for line in f:lines("l") do
--    line_count = line_count + 1
--    if line:find("^ *#") == nil and line:find("%S") ~= nil then
--        bias_array_count = bias_array_count + 1
--        val_count = 0
--        row = {}
--        row.tokens = {}
--        row.n_tokens = 0
--        for val in line:gmatch("[^,%s]+") do
--            val_count = val_count + 1
--            val_orig = val
--            if val_count <= 2 then
--                val = val:lower()
--                if val:sub(-3) == "inf" then
--                    val = math.tointeger(val:sub(1, -4) .. "1")
--                    if val ~= val or type(val) ~= "number" or val > 0 then
--                        f:close()
--                        error("First two values of line " .. line_count .. " of config file must be finite floating-point numbers or -inf, but got '" .. val_orig .. "' as value #" .. val_count)
--                    end
--                    val = val * math.huge
--                else
--                    val = tonumber(val)
--                    if val ~= val or type(val) ~= "number" then
--                        f:close()
--                        error("First two values of line " .. line_count .. " of config file must be finite floating-point numbers or -inf, but got '" .. val_orig .. "' as value #" .. val_count)
--                    end
--                end
--                if val_count == 1 then
--                    row.starting_bias = val
--                else
--                    row.ending_bias = val
--                end
--            else
--                val = math.tointeger(val)
--                if type(val) ~= "number" or val < 0 then
--                    f:close()
--                    error("All values after the first two values of line " .. line_count .. " of config file must be nonnegative integers, but got '" .. val_orig .. "' as value #" .. val_count)
--                end
--                row.n_tokens = row.n_tokens + 1
--                row.tokens[row.n_tokens] = val
--            end
--        end
--        if val_count < 3 then
--            f:close()
--            error("Line " .. line_count .. " of config file must contain at least 3 values, but found " .. val_count)
--        end
--        bias_array[bias_array_count] = row
--    end
--end
--f:close()
--print("Successfully loaded " .. bias_array_count .. " phrase bias entr" .. (bias_array_count == 1 and "y" or "ies") .. ".")


-- Increase this to add progressively more debug logging
local verbose = 0

local genmod_run = false


local cached_modeltype = nil ---@type string
-- Mapping of overlapping tokens that are as long as or shorter than the key token
local cached_subsumed_tokens = nil ---@type table<int, table<int, int>>
-- Mapping of overlapping tokens that are longer than the key token
local cached_subsuming_tokens = nil ---@type table<int, table<int, int>>
-- Mapping from token to decoded token (single token only)
local cached_decoded = nil ---@type table<int, string>

--- Reverse the table x in-place
--@param x table
local function inplace_reverse(x)
    local n, m = #x, #x/2
    for i=1, m do
        x[i], x[n-i+1] = x[n-i+1], x[i]
    end
    return x
end

local function quote_token(decoded)
    return string.format("[%s]", string.gsub(decoded, "\n", "\\n"))
end

--- Decode a single token to a string, caching the result
--@param token integer
--@return string
local function cached_decode(token)
    if token ~= nil then
        local decoded = cached_decoded[token]
        if decoded == nil then
            decoded = kobold.decode(token)
            cached_decoded[token] = decoded
        end
        return decoded
    end
end

---@class RepetitionInfo
---@field token_index integer
---@field char_index integer
---@field word_index integer
---@field n_tokens integer
---@field n_chars integer
---@field n_words integer
---@field delta_chars integer
---@field delta_words integer
---@field final_contiguous string nil unless the next tokens follow without space.  If so, this is the concatenation of the final and contiguous following tokens
---@field bias_cache number


local function print_rep_info(rep_info, tokens, bias, falloff, falloff_penalty)
    local token = tokens[rep_info.token_index]
    local decoded = kobold.decode(inplace_reverse({ table.unpack(tokens, rep_info.token_index, rep_info.token_index + rep_info.n_tokens - 1) }))
    print(token, quote_token(decoded), rep_info.token_index,
        -bias,
        rep_info.char_index, rep_info.word_index,
        rep_info.n_tokens, rep_info.n_chars, rep_info.n_words,
        rep_info.delta_chars, rep_info.delta_words,
        falloff, falloff_penalty)
end

--- Return a penalty for a repetition of the given length.
-- The penalty is larger when for longer and closer sequences
-- and also may increase if a repetition of this length has
-- appeared multiple times recently.
--@param token integer
--@param reps table<integer, RepetitionInfo>
--@param tokens table<integer, integer>
--@return number
local function repetition_penalty(token, reps, tokens)
    -- Very simple first proof of concept penalty function
    -- Simple but naive implementation
    --return seq_length * #distances
 
    -- Second still pretty simple penalty function
    -- Ignore single repetitions
    --local s2 = seq_length * seq_length
    --local s3 = s2 * seq_length
    --local falloff = 10 + seq_length * 10 + s2 * 10 + s3 -- TODO: Pull coefficients from config
    --local base_penalty = 1 + seq_length * 1 + seq_length * seq_length * .1 -- TODO: Pull factor from config, add more coefficients
    --local penalty = 0
    --for k, dist in ipairs(distances) do
    --    if dist >= falloff then
    --        break
    --    end
    --    penalty = penalty + base_penalty * (1 - dist / falloff) -- TODO: Should this be more complex/configurable?
    --end

    -- V3 penalty function
    local penalty = 0
    for _, rep in ipairs(reps) do

        -- The falloff distance could increases the more times this pattern appears, or be based on pattern length,
        -- or other factors related to the repetition.
        local falloff = 512 + 1024 * ((rep.n_words - 3) * (rep.n_words - 3)) + 128 * #reps

        local rep_bias = 0
        local falloff_penalty = 0
        if rep.token_index < falloff then
            falloff_penalty = (1 - rep.token_index / falloff)
            if rep.n_words > 2 and rep.n_chars > 2 then
                rep_bias = (((rep.n_words - 1) * (rep.n_words - 1)) * 0.2) * falloff_penalty
            end
        end
        if verbose > 2 or (rep_bias ~= 0 and verbose > 1)  then
            print_rep_info(rep, tokens, rep_bias, falloff, falloff_penalty)
        end
        penalty = penalty + rep_bias
        rep.bias_cache = rep_bias
    end
    return penalty
end

-- Convert an array table arr into a set keyed by the table values.
local function to_set(arr)
    local result = {}
    for _, v in ipairs(arr) do
        result[v] = true
    end
    return result
end


--- Return a string consisting of the concatenation of all contiguous "alpha-numeric" tokens
-- in the context beginning with index i.  Return nil if context[i] does not have contiguous following tokens.
local function contiguous_tokens(context, i)
    local result = nil
    local decoded = cached_decode(context[i])
    while i > 1 do
        local next_decoded = cached_decode(context[i - 1])
        if string.find(decoded, "%w$") and string.find(next_decoded, "^%w") then
            if result == nil then
                result = decoded .. next_decoded
            else
                result = result .. next_decoded
            end
        else
            break
        end
        i = i - 1
        decoded = next_decoded
    end
    return result
end

-- From https://github.com/lua-nucleo/lua-nucleo/blob/v0.1.0/lua-nucleo/string.lua#L245-L267
-- MIT license
local escape_lua_pattern
do
    local matches =
    {
        ["^"] = "%^";
        ["$"] = "%$";
        ["("] = "%(";
        [")"] = "%)";
        ["%"] = "%%";
        ["."] = "%.";
        ["["] = "%[";
        ["]"] = "%]";
        ["*"] = "%*";
        ["+"] = "%+";
        ["-"] = "%-";
        ["?"] = "%?";
        ["\0"] = "%z";
    }

    escape_lua_pattern = function(s)
        return (s:gsub(".", matches))
    end
end

-- Tokens that should be treated as synonyms for the purpose of suppression
-- Adding pronouns prevents gender and count miss-attribution due
-- to suppressing only e.g. him but not her.
-- TODO: Actually hook this up.
local synsets = {
    {"him", "her", "it", "me", "them", "us", "you"},
    {"his", "her", "its", "my", "their", "our", "your"},
    {"his", "hers", "its", "mine", "theirs", "ours", "yours"},
    {"he", "she", "it", "I", "they", "we", "you"},
}

--- Return the given string parameter with its first letter capitalized
local function title_case(x)
    return x:gsub("^%l", string.upper)
end

local case_transformers = { function(x) return x end, string.upper, title_case }

--- Return a pair of lists all tokens that overlap with the given token.
-- The pair is (shorter and equal length tokens, longer tokens)
-- The token itself is not included.
-- Comparisons are performed in a case-insensitive manner.
-- Note - this iterates over all tokens and so is relatively slow.
-- The results should ideally be cached.
local function get_overlapping_tokens(token)
    local not_longer = {} ---@type table<int, int>
    local longer = {} ---@type table<int, int>

    local decodedraw = cached_decode(token)
    local decoded = escape_lua_pattern(decodedraw)
    decoded = string.lower(decoded) -- Make optional?
    local dlen = #decoded
    local numtokens = kobold.logits_cols
    local decoded_space = string.find(decoded, "^%s") == 1

    -- The last token throws an error when decoding
    for t = 1, numtokens - 1 do
        local tdecodedraw = cached_decode(t)
        -- Skip the token itself
        if t ~= token then
            local tdecoded = escape_lua_pattern(tdecodedraw)
            tdecoded = string.lower(tdecoded)

            -- If token begins with a space, and t begins with an alpha-numeric
            -- then insert a space in front of t.
            -- Solves space eliding as in "allwork and noplay makes Jacka dull boy."
            if decoded_space and string.find(tdecoded, "^%w") == 1 then
                tdecoded = " " .. tdecoded
            end

            local tlen = #tdecoded
            if tlen > dlen then
                -- Search for decoded at the beginning of tdecoded
                if string.find(tdecoded, decoded, 1) == 1 then
                    --print(quote_token(decodedraw), "subsumed by", quote_token(tdecodedraw))
                    table.insert(longer, t)
                end
            else
                -- Search for tdecoded at the beginning of decoded
                if string.find(decoded, tdecoded, 1) == 1 then
                    --print(quote_token(decodedraw), "subsumes", quote_token(tdecodedraw))
                    table.insert(not_longer, t)
                end
            end
        end
    end
    return not_longer, longer
end

--- Apply a bias from source_token to dest_token
-- Note that while logits is keyed by token+1, biases is keyed by token.
local function apply_bias(source_token, dest_token, logits, biases, bias)
    -- Only apply a bias to a related token if the source token has
    -- a higher value.
    if source_token and source_token ~= dest_token then
        if logits[source_token + 1] < logits[dest_token + 1] then
            return false
        end
    end

    local current_bias = biases[dest_token]
    if current_bias == nil or bias > current_bias then
        biases[dest_token] = bias
        return true
    end
    return false
end

---@return string
local function strip_punctuation(text)
    local stripped = text
    -- Protect contractions such as (don't and topsy-turvy)
    stripped = string.gsub(stripped, "(%w)'(%w)", "%1\u{0795}%2")
    stripped = string.gsub(stripped, "(%w)-(%w)", "%1\u{0796}%2")

    -- Strip all non-alpha numeric characters
    stripped = string.gsub(stripped, "[^%w\u{0795}\u{0796}]+", " ")
    stripped = string.gsub(stripped, "%s+$", "")

    -- Restore contractions
    stripped = string.gsub(stripped, "(%w)\u{0795}(%w)", "%1'%2")
    stripped = string.gsub(stripped, "(%w)\u{0796}(%w)", "%1-%2")
    return stripped
end

-- Tokens to never penalize to minimize meltdowns
-- Use strings instead of token ids to make more portable across tokenizers.
-- Try to limit the number of tokens here because they are
--  1. Brittle (a vocabulary that combines these strings with other tokens
--     will make composites that won't be ignored; and
--  2. Dangerous, since these can participate in repetitions and won't be
--     suppressed.  I.e., "and and and and and and and..." can repeat indefinitely.
-- TODO: Load from config
local token_blacklist = to_set({
    --".",     -- to avoid run-on lists and sentances, never penalize a period.
    --" and",  -- to avoid run-on lists once they have started.
})


function userscript.genmod()
    genmod_run = true

    -- To be future proof, clear caches if the model type changes.
    -- In the current release versino of KoboldAI, the kobold.modeltype
    -- function throws an error.  Currently, the Lua environment is also
    -- restarted when the model changes, so this code is unnecessary,
    -- but we'll do it anyway just to be safe.

    -- kobold.modeltype throws a Python exception get_hidden_size_from_model
    -- I've fixed this in united, but the change hasn't made it into main yet.  I
    -- tried wrapping the call to kobold.modeltype with pcall, but the Python/Lua
    -- binding doesn't seem to wrap Python exceptions properly into a pcall error.
    -- So to make it easier for people to test this script, don't ever reset the
    -- cache.  This is fine for now since reloading the model currently resets
    -- the Lua state.  But it would be better to use kobold.modeltype
    -- TODO: Replace the call to kobold.modeltype once united is merged to main.
    --local modeltype = kobold.modeltype
    local modeltype = "unknown"
    if modeltype ~= cached_modeltype then
        cached_modeltype = modeltype
        cached_subsumed_tokens = {}
        cached_subsuming_tokens = {}
        cached_decoded = {}
    end

    local context_text = kobold.worldinfo:compute_context(kobold.submission, nil, {include_anote = false})

    -- The 'include_anote = false' kwarg above will strip out the author's note, but this code has not
    -- been merged from united into main.  In order to make it easier for people to test this script,
    -- using a pattern to strip out the author's note if it exists.  This is not quite as ideal since
    -- it's slower and has the chance of stripping other directives, to it should eventually be removed.
    -- TODO: Remove this once 'include_anote' support has been merged to main.
    context_text = string.gsub(context_text, "\n%b[]\n", "")
    local context_tokens = kobold.encode(context_text)

    -- Use dynamic programming to generate a sequence of 

    -- For each partially-generated sequence...
    for seq, generated_row in ipairs(kobold.generated) do

        -- Build an array `tokens` as the concatenation of the context
        -- tokens and the generated tokens of this sequence

        local tokens = {}
        n_tokens = 0
        for k, v in ipairs(context_tokens) do
            n_tokens = n_tokens + 1
            tokens[n_tokens] = v
        end
        for k, v in ipairs(generated_row) do
            n_tokens = n_tokens + 1
            tokens[n_tokens] = v
        end
        inplace_reverse(tokens)

        -- Early out if the previous token doesn't end with an alpha-numeric character.
        -- Not doing this causes too many formatting artifacts, for example prioritizing
        -- newlines after sentance stops becasue ". You" appears frequently
        local previous = cached_decode(tokens[1])
        --if not string.find(previous, "%w$") then
        --    goto nextseq
        --end

        -- Use dynamic programming to compute the maximum overlap between the Nth token
        -- and the first i tokens starting from the beginning of the sequence.  By convention,
        -- the first token is considered to not match itself.  We then back-propogate the
        -- maximum sequence counts, resetting at zero elements.
        --                                                   1 1
        -- In other words,               0 1 2 3 4 5 6 7 8 9 0 1
        -- given a squence like this:    A A B C A B A A B C B A  
        -- the overlap counts would be:  0 1 0 0 1 0 1 2 3 4 0 1
        -- the back-propagated values:   1 1 0 1 1 4 4 4 4 4 1 1
        --
        -- We next read off the indices with overlap counts of 0 as (index, token, count), i.e.
        --  (0, A, 1), (2, B, 0), (3, C, 1), (5, B, 4), (10, B, 1)
        -- And add the to a table mapping from token -> length -> positions if count > 0
        --  { [A] = {[1] = {0}}, [B] = {[1] = {10}, [4] = {5}}, [C] = {1: {3}}}
        --
        -- The purpose of this is to identify the logits that would extend the length
        -- of an existing sequence.  For example, adding a new token C would cause
        -- the CA at the beginning to overlap with the existing sequence CA starting at
        -- positing 3.  Adding a new token B would cause a lone B to overlap with B at position 2,
        -- BAABC to overlap with BAABC starting at position 5, and BA to overlap at position 10.
        --
        -- This pre-computation means we don't need to iterate over each logit, only the ones
        -- that will contribute meaningfully to adding new repetitions.
        --

        local cursor = 1
        local counts = {} ---@type table<integer, integer>
        local words = {} ---@type table<integer, integer>
        local characters = {} ---@type table<integer, integer>
        local back = {} ---@type table<integer, integer>
        local token_reps = {} ---@type table<integer, table<integer, RepetitionInfo>>
        local word_index = 0
        local had_space = false -- The next token (prev in iteration order) begins with a non alpha character
        local had_alpha = false -- The next token (prev in iteration order) had alpha-numeric characters

        -- Iterate in forward order to compute counts
        for k, v in ipairs(tokens) do

            local decoded = cached_decode(v)

            -- Increment words.  We advance at most one word per token.  If
            -- the first character is non-alpha-numeric, we increment the word
            -- count for the previous word (and also set "had_space" to true).
            -- Treat single quote and hyphen as alpha-numeric to avoid breaking
            -- up contractions like don't and compound words like alpha-numeric
            local non_alpha_ix = string.find(decoded, "[^%w'-]")
            local alpha_ix = string.find(decoded, "[%w'-]")
            if (had_space and alpha_ix) or (prev_alpha and not alpha_ix) then
                word_index = word_index + 1
            end
            prev_alpha = alpha_ix

            words[k] = word_index
            if non_alpha_ix and non_alpha_ix == 1 then
                had_space = true
            else
                had_space = false
            end

            local tokenlen = #decoded
            if k == 1 then
                counts[k] = 0
                characters[k] = tokenlen
            else
                characters[k] = tokenlen + characters[k - 1]
                if tokens[cursor] == v then
                    counts[k] = counts[k - 1] + 1
                    cursor = cursor + 1
                else
                    counts[k] = 0
                    cursor = 1
                end
            end
        end

        -- Now iterate in reverse order to compute back counts.
        local run ---@type integer
        for i = n_tokens, 1, -1 do
            if i == n_tokens or counts[i + 1] == 0 then
                run = counts[i] + 1
            end
            back[i] = run
        end

        -- For debugging, display the table of precomputed values for the context
        if verbose > 3 then
            for k, v in ipairs(tokens) do
                if back[k] == 0 or verbose > 4 then
                    print(k, counts[k], back[k], words[k], characters[k], v, quote_token(cached_decode(v)))
                end
            end
        end

        -- Next, build the table mapping from tokens to their repetition info
        for k, v in ipairs(counts) do
            if v == 0 and back[k] > 1 then
                local t = tokens[k]

                local decoded = cached_decode(t)

                -- Skip hardcoded blacklisted tokens that are prone to causing meltdowns.
                if token_blacklist[decoded] then
                    goto continue
                end

                local prev_decoded = cached_decode(tokens[k + 1])
                if prev_decoded then
                    local match_ix
                    -- Skip tokens that don't begin with a space.  Note that initially this also
                    -- included a check for whether the previous token ended with a letter, but
                    -- that test was lifted up as an early-out for the entire biasing process.
                    --match_ix = string.find(decoded, "^%s")
                    --if not match_ix then
                    --    goto continue
                    --end

                    -- Skip penalizing repetitions that look like a name, surname
                    -- or title.  For example, this prevents King Arthur from turning
                    -- into King Arthurs.  I.e., check for a pattern
                    -- that begins with (space, upper, lower) following a token
                    -- that ends with a lower case letter.
                    -- Note: This could lead to the model shouting, but its worth it
                    -- to avoid spelling errors in proper names.
                    match_ix = string.find(decoded, "^%s%u")
                    if match_ix then
                        match_ix = string.find(prev_decoded, "%l$")
                        if match_ix then
                            goto continue
                        end
                    end
                end

                -- Collect various properties of the repetion to be used in penalty scoring
                local n_tokens = back[k]
                local char_index = characters[k]
                local word_index = words[k]
                local char_index_end = characters[k + n_tokens - 1]
                if char_index_end == nil then
                    char_index_end = characters[#characters]
                end
                local word_index_end = words[k + n_tokens - 1]
                if word_index_end == nil then
                    word_index_end = words[#words]
                end

                local rep_infos = token_reps[t]
                if rep_infos == nil then
                    rep_infos = {} ---@table<integer, RepetitionInfo>
                    token_reps[t] = rep_infos
                end

                -- Determine whether the next token, if any, continues a trailing word.
                -- This determines whether we should apply the bias to longer overlapping tokens.
                local final_contiguous = contiguous_tokens(tokens, k)

                -- Create the repetition info record
                local rep_info = {} ---@type RepetitionInfo
                rep_info.token_index = k
                rep_info.char_index = char_index
                rep_info.word_index = word_index
                rep_info.n_tokens = n_tokens
                rep_info.n_chars = char_index_end - char_index + #decoded
                rep_info.n_words = word_index_end - word_index + 1
                rep_info.delta_chars = #decoded
                local nwords = words[k + 1]
                if nwords then
                    rep_info.delta_words = nwords - word_index
                else
                    rep_info.delta_words = 0
                end
                rep_info.final_contiguous = final_contiguous

                if rep_info.delta_words == 0 then
                    goto continue
                end

                table.insert(rep_infos, rep_info)

                ::continue::
            end
        end

        -- Mapping from token to "worst" applicable bias
        -- Tokens can apply their computed bias to "similar" tokens, but in order to avoid excessive
        -- distortion we don't sum all the applicable biases, but instead take the max.
        -- This table maps from token to the max bias so far.
        local biases = {} ---@type table<int, float>

        -- Finally, modify logits based on a computed bias
        local logits = kobold.logits[seq]
        for t, rep_infos in pairs(token_reps) do
            local bias = repetition_penalty(t, rep_infos, tokens)
            if bias ~= 0 then
                apply_bias(t, t, logits, biases, bias)

                -- Also apply the bias to subsumed and potentially subsuming tokens.

                local subsumed = cached_subsumed_tokens[t]
                if subsumed == nil then
                    subsumed, subsuming = get_overlapping_tokens(t)
                    cached_subsumed_tokens[t] = subsumed
                    cached_subsuming_tokens[t] = subsuming
                end

                -- Penalize subsumed (shorter overlapping) tokens
                for _, st in ipairs(subsumed) do
                    apply_bias(t, st, logits, biases, bias)
                    if verbose > 1 then
                        --print(">> " .. quote_token(cached_decode(st)), -bias, logits[st + 1], st)
                    end
                end

                -- Penalize subsuming (longer overlapping tokens.  This is much harder to do accurately,
                -- since the tokens may or may not overlap with the end of the repetition.  We store
                -- final_contiguous on the repetition info to make this possible.  However, due to the
                -- "penumbra" effect, it's not clear that we should suppress these perfectly accurately.
                -- Instead, just suppress subsuming tokens also.
                -- TODO is there a better approach?
                for _, st in ipairs(subsuming) do
                    apply_bias(t, st, logits, biases, bias)
                    if verbose > 1 then
                        --print(">>> " .. quote_token(cached_decode(st)), -bias, logits[st + 1], st)
                    end
                end


                if verbose > 1 then
                    print("> " .. quote_token(cached_decode(t)), -bias, logits[t + 1], t)
                elseif verbose > 0 then
                    print("> " .. quote_token(cached_decode(t)), -bias)
                end
            end
        end

        -- Actually apply the biases
        for t, bias in pairs(biases) do
            logits[t + 1] = logits[t + 1] - bias
        end

        ::nextseq::
    end
end

function userscript.outmod()
    if not genmod_run then
        warn("WARNING:  Generation modifier was not executed, so this script has had no effect")
    end
end

return userscript
