-- important global constants
WORD_SIZE = 3 -- word size is default to 3 bytes

-- utilities
local function clone(tbl)
  local out = {}
  for i = 1, #tbl do out[i] = tbl[i] end
  return out
end

local function parse_hex(tok)
  if not tok then return nil end
  if tok:sub(1, 2) == "0x" or tok:sub(1, 2) == "0X" then
    return tonumber(tok:sub(3), 16)
  end
  -- allow plain decimal
  return tonumber(tok)
end

-- error handling stuff
local function perror_at(token, msg)
  io.stderr:write(string.format("Error at '%s': %s\n", token, msg))
  os.exit(1)
end

local function perror(msg)
  io.stderr:write(string.format("Error: %s\n", msg))
  os.exit(1)
end

-- tokenization stuff
local function read_source(path)
  local f = io.open(path, "r")
  if not f then perror(string.format("Could not open file %s", path)) end
  local text = f:read("*a")
  f:close()
  return text
end

local function tokenize(source)
  local tokens = {}

  local in_comment = false
  for tok in source:gmatch("%S+") do
    if tok == "(" then in_comment = true
    elseif tok == ")" then in_comment = false
    elseif not in_comment then table.insert(tokens, tok)
    end
  end

  return tokens
end

-- expanding macros
local function expand_macros(tokens)
  local macros = {}
  local expanded = {} -- final output
  local macro_expansion_counter = 0 -- for label mangling

  local assembler_directives = {
    pad = true,
    offset = true,
    padto = true,
  }
  
  local i = 1
  while i <= #tokens do
    local tok = tokens[i]
    -- if we get a %def, save the token and continue
    if tok == "%def" then
      local name = tokens[i + 1]
      if not name then 
        perror_at(tok, "macro defined without a name")
      end 

      local body = {}
      i = i + 2 -- skip %def and name
      while i <= #tokens and tokens[i] ~= "%end" do
        table.insert(body, tokens[i])
        i = i + 1
      end
      if tokens[i] ~= "%end" then
        perror("macro definition is not closed")
      end

      macros[name] = body
      i = i + 1 -- skip %end
    elseif tok:sub(1, 1) == "%" then -- we get a macro call, expand it
      local name = tok:sub(2)

      -- check if its an assembler directive- %pad, %padto, %offset
      if assembler_directives[name] then
        table.insert(expanded, tok)
        -- also insert its arguments to be used later
        if tokens[i + 1] then
          i = i + 1
          table.insert(expanded, tokens[i])
        end
        i = i + 1
      else
        -- otherwise just expand the macro
        local body = macros[name]
        if not body then 
          perror_at(tok, "macro definition not found")
        end
  
        -- for mangling
        macro_expansion_counter = macro_expansion_counter + 1
        local expansion_id = name .. "$" .. macro_expansion_counter 
    
        -- clone the macro body so we can safely modify it
        local cloned_body = clone(body)
  
        local local_labels = {} -- storing label declarations
        for j = 1, #cloned_body do
          local t = cloned_body[j]
          if t:sub(1, 1) == "." then
            local_labels[t:sub(2)] = true
            cloned_body[j] = t .. "$" .. expansion_id
          end
        end
  
        for j = 1, #cloned_body do -- manging label references when needed
          local t = cloned_body[j]
          if t:sub(1, 1) == "@" then -- label reference
            local has_skip = t:sub(-1) == "?"
            local label_name = has_skip and t:sub(2, -2) or t:sub(2)
            
            if local_labels[label_name] then
              -- mangle only local labels
              cloned_body[j] = "@" .. label_name .. "$" .. expansion_id
              if has_skip then
                cloned_body[j] = cloned_body[j] .. "?"
              end
            end
            -- otherwise, don't touch labels declared outside the macro
          end
        end
  
        -- append macro body into expanded list
        for _, t in ipairs(cloned_body) do
          table.insert(expanded, t)
        end
        
        i = i + 1 -- move past macro call
      end

    else
      -- normal token
      table.insert(expanded, tok)
      i = i + 1
    end
  end

  return expanded
end

local function handle_labels(tokens)
  local labels = {}
  local pc = 0
  
  local i = 1
  while i <= #tokens do -- first pass, store all label declarations with pc value
    local tok = tokens[i]
    
    -- %pad n macro inserts n NOP bytes to pad the ROM
    if tok == "%pad" then
      local n = parse_hex(tokens[i + 1])
      if not n or n < 0 then perror_at(tok, "expected non-negative number after %pad") end
      pc = pc + n
      i = i + 2
      goto continue
      
    -- %padto addr inserts NOP bytes until the PC reaches a specific address
    elseif tok == "%padto" then
      local addr = parse_hex(tokens[i + 1])
      if not addr then perror_at(tok, "expected address after %padto") end
      if addr < pc then 
        perror_at(tok, string.format("cannot pad to 0x%X, pc already at 0x%X", addr, pc))
      end
      pc = addr
      i = i + 2
      goto continue
      
    -- %offset addr sets the pc to that address
    elseif tok == "%offset" then
      local addr = parse_hex(tokens[i + 1])
      if not addr then perror_at(tok, "expected address in after %offset") end
      pc = addr
      i = i + 2
      goto continue
    
    elseif tok:sub(1, 1) == "." then -- label declaration
      local label_name = tok:sub(2)
      labels[label_name] = pc
      i = i + 1
      
    elseif tok:sub(1, 1) == "@" then -- increment pc by size of word
      pc = pc + 1 + WORD_SIZE -- plus extra 1 for a "LIT" opcode
      i = i + 1

    elseif tok:sub(1, 1) == "#" then -- hex literal
      local has_skip = tok:sub(-1) == "?"
      local digits = has_skip and tok:sub(2, -2) or tok:sub(2)
      if (#digits ~= 2) and (#digits ~= 4) and (#digits ~= 6) and (#digits ~= 8) then
        perror_at(tok, "invalid number of hex digits in literal (must be 2, 4, 6, or 8)")
      end
      -- 1 byte for the LIT opcode, plus n more for immediate value
      pc = pc + 1 + (#digits // 2)
      i = i + 1
      
    else 
      pc = pc + 1 -- normal opcode
      i = i + 1
    end

    ::continue::
  end

  -- second pass, convert all labels into lit instructions
  -- keep everything else (%offset, %padto, %pad included- handled later)
  local expanded = {}
  local j = 1
  while j <= #tokens do
    local tok = tokens[j]

    if tok:sub(1, 1) == "@" then
      local has_skip = tok:sub(-1) == "?"
      local name = has_skip and tok:sub(2, -2) or tok:sub(2)
      local pc_value = labels[name]
      if not pc_value then 
        perror_at(tok, "label declaration not found")
      end
      
      local pc_hex = string.format("%0" .. (WORD_SIZE * 2) .. "x", pc_value)
      local literal = "#" .. pc_hex .. (has_skip and "?" or "")
      table.insert(expanded, literal)
    elseif tok:sub(1, 1) ~= "." then -- don't insert label declarations!
      table.insert(expanded, tok)
    end

    j = j + 1
  end

  return expanded
end

local function assemble(opcodes)
  local binary = {}
  local pc = 0 -- tracking for %pad, %padto, and %offset

  local opmap = {
    ADD = 0x00, AND = 0x01, XOR = 0x02, SHF = 0x03, 
    CMP = 0x04, SWP = 0x05, STR = 0x06, LOD = 0x07, 
    DUP = 0x08, DRP = 0x09, PSH = 0x0a, POP = 0x0b, 
    JMP = 0x0c, LIT = 0x0d, INT = 0x0e, NOP = 0x0f,
  }

  local function emit_op(op, a, skip)
    local instr = 0x80
    if skip then instr = instr | 0x40 end
    instr = instr | (a & 0x03 << 4)
    instr = instr | (op & 0x0f)
    table.insert(binary, instr)
    pc = pc + 1
  end
  
  local i = 1
  while i <= #opcodes do
    local tok = opcodes[i]

    -- %pad n directive
    if tok == "%pad" then
      local n = parse_hex(opcodes[i + 1])
      if not n or n < 0 then 
        perror_at(tok, "expected non-negative number after %pad") 
      end
      -- emit n NOPs (1 byte each)
      for _ = 1, n do
        -- NOP with default a and skip=false
        emit_op(opmap.NOP, (WORD_SIZE - 1) & 0x03, false)
      end
      i = i + 2
      goto continue

    -- %padto addr directive
    elseif tok == "%padto" then
      local addr = parse_hex(opcodes[i + 1])
      if not addr then perror_at(tok, "expected address after %padto") end
      if addr < pc then 
        perror_at(tok, string.format("cannot pad to 0x%X, pc already at 0x%X", addr, pc))
      end
      while pc < addr do
        emit_op(opmap.NOP, (WORD_SIZE - 1) & 0x03, false)
      end
      i = i + 2
      goto continue

    -- %offset addr directive
    elseif tok == "%offset" then
      local addr = parse_hex(opcodes[i + 1])
      if not addr then perror_at(tok, "expected address after %offset") end
      pc = addr -- move pc without emitting bytes
      i = i + 2
      goto continue
    end

    if tok == "HLT" then
      table.insert(binary, 0) -- just write a 0 for halt
      pc = pc + 1
      
    elseif tok:sub(1, 1) == "#" then -- LIT instruction
      local has_skip = tok:sub(-1) == "?"
      local digits = has_skip and tok:sub(2, -2) or tok:sub(2)
      -- we shouldn't need to check this again, but i will just in case
      if (#digits ~= 2) and (#digits ~= 4) and (#digits ~= 6) and (#digits ~= 8) then
        perror_at(tok, "invalid number of hex digits in literal (must be 2, 4, 6, or 8)")
      end
      local a = (#digits // 2) - 1

      local instruction = 0x8d -- emitting LIT opcode
      if has_skip then instruction = instruction | 0x40 end
      instruction = instruction | (a << 4)
      table.insert(binary, instruction)
      pc = pc + 1

      -- insert every 2 hex digits as a byte, big-endian order
      for j = 1, #digits, 2 do
        local byte_value = tonumber(digits:sub(j, j + 1), 16)
        if not byte_value then
          perror_at(tok, "invalid hex literal digits.")
        end
        table.insert(binary, byte_value)
        pc = pc + 1
      end
      
    else -- standard opcode
      -- pattern matching can grab a lot of this stuff
      local op, num_suffix, skip = tok:match("^([A-Z][A-Z][A-Z])([1-4]?)(%??)$")
      if not op then
        perror_at(tok, "Invalid opcode")
      end

      local opcode = opmap[op]
      if not opcode then
        perror_at(tok, "Invalid opcode")
      end

      local has_skip = skip == "?" -- skip flag

      local a = WORD_SIZE - 1 -- default 'a' value
      if num_suffix ~= "" then
        local num = tonumber(num_suffix)
        a = num - 1
      end
      emit_op(opcode, a, has_skip)
    end

    i = i + 1
    ::continue::
  end

  return binary
end

-- writing bytes to a file
local function write_binary(filename, bytes)
  local f, err = io.open(filename, "wb")
  if not f then error("Failed to open " .. filename) end

  local data = {}
  for i = 1, #bytes do
    data[i] = string.char(bytes[i] & 0xff) -- masking just in case
  end

  f:write(table.concat(data))
  f:close()
end

-- "main", the assembler loop
local input = arg[1]
local output = arg[2] or "out.okb"

local function print_usage()
  io.stdout:write("Usage: lua mkasm.lua input.kid [output.okb]\n")
end

if not input then
  print_usage()
  os.exit(1)
end

local source = read_source(input)
local tokens = tokenize(source)
local expanded_macros = expand_macros(tokens)

-- for testing macro expansions
-- local i = 1
-- while i <= #expanded_macros do
--   print("EXPANDED: " .. expanded_macros[i])
--   i = i + 1
-- end

local handled_labels = handle_labels(expanded_macros)

-- for testing label handling
-- local i = 1
-- while i <= #handled_labels do
--   print("LBL HANDLING: " .. handled_labels[i])
--   i = i + 1
-- end

local binary = assemble(handled_labels)

-- for testing the binary output
-- local i = 1
-- while i <= #binary do
--   print("BINARY: " .. string.format("%2x", binary[i]))
--   i = i + 1
-- end

write_binary(output, binary)

-- TODO more testing
