local lpeg = require "lpeg"
local cjson = require "cjson.safe"

local encode = cjson.encode
local R, S, V, P, C, Ct, Cs, Cf, Cmt, Cg, Cb, Cc
    = lpeg.R, lpeg.S, lpeg.V, lpeg.P, lpeg.C,
      lpeg.Ct, lpeg.Cs, lpeg.Cf, lpeg.Cmt, lpeg.Cg, lpeg.Cb, lpeg.Cc


local any = P(1)         -- pattern that accepts one character
local space = S(" \t\n") -- a set with the given chars
local digit = R("09")    -- a set with the range 0-9
local lower = R("az")    -- a set with the range a-z
local upper = R("AZ")    -- a set with the range A-Z

local letter = lower + upper
local alnum = letter + digit


--------------------------------------------------------------------------------
-- Notes
--------------------------------------------------------------------------------
-- P: matches occur against the start of the string, and successful matches
-- return the position immediately after the successful match, or nil if
-- unsuccesful.

-- (LPeg uses the * operator [instead of the more obvious ..] both because it
-- has the right priority and because in formal languages it is common to use
-- a dot for denoting concatenation.)
-- +(lower priority) -> or, *(higher priority) -> and


--------------------------------------------------------------------------------
-- Utility
--------------------------------------------------------------------------------
local _tostring = tostring

tostring = function(v)
    if type(v) == "table" then
        return encode(v)
    end

    return _tostring(v)
end


--------------------------------------------------------------------------------
-- Split
--------------------------------------------------------------------------------
local function split(s, sep)
  sep = P(sep)
  local elem = C((1 - sep)^0)
  local p = Ct(elem * (sep * elem)^0) -- make a table capture
  return p:match(s)
end


local p = P(",") * space^0
print(split("a,b,,c,d,ee,f,g,g, i,,jk,, ", p))


--------------------------------------------------------------------------------
-- KV parsing
--------------------------------------------------------------------------------
lpeg.locale(lpeg)

local alpha = lpeg.alpha
local space = space^0
local name = C(alpha^1) * space
local sep = S(",;") * space
local pair = Cg(name * "=" * space * name) * sep^-1
local list = Cf(Ct("") * pair^0, rawset) -- Cf -> folder

print(list:match("a=b, c = hi; next = pi"))
--> { a = "b", c = "hi", next = "pi" }


--------------------------------------------------------------------------------
-- Balanced parentheses
--------------------------------------------------------------------------------
local p = P{"(" * ((1 - S"()") + V(1))^0 * ")"}
print("Balanced end at: " .. p:match("(((((())))))"))


--------------------------------------------------------------------------------
-- Captures
--------------------------------------------------------------------------------
print(Ct(""):match"")

local t1, t2 = (Ct("") * Ct("")):match""
print(encode(t1), encode(t2))

local m = Ct(Ct("") * Ct(""))
print(encode(m:match("")))

-- Cc"baz" got lost in the process
print(Ct(Cc"foo" * Cg(Cc"bar" * Cc"baz", "TAG")* Cc"qux"):match"")

print(Ct(Cc"foo" * Ct(Cc"bar" * Cc"baz", "TAG")* Cc"qux"):match"")

print(P(P"bar" * P"baz"):match("barbaz"))

-- push the capture to the result stack, search the capture tree breadth first?
print((1 * C(C"b" * C(C"c" * Cc"d")) * 1):match"abcd")

-- http://lua-users.org/wiki/LpegTutorial
-- Some captures operate on the values produced by their subcaptures, while
-- others operate on the capture objects. This is sometimes counter-intuitive.

print((1 * C(C"b" * C"c") * 1):match"abcd")
--> "bc", "b", "c"

print(Ct(1 * C(C"b" * C"c") * 1):match"abcd")
--> { "bc", "b", "c" }

print((1 * Cg(C"b" * C"c" * C"d") * 1):match"abcde")
--> "b", "c", "d"

print(Ct(1 * Cg(C"b" * C"c" * C"d") * 1):match"abcde")
--> { "b", "c", "d" }

print(Cs(1 * Cg(C"b" * C"c" * C"d") * 1):match"abcde")
--> "abe" -- "c" and "d" are dropped.

print((1 * Cg(C"bc", "FOOO") * C"d" * 1 * Cb"FOOO" * Cb"FOOO"):match"abcde")
-- > "d", "bc", "bc"

-- Cb"FOOO" will look back for a corresponding Cg() that has succeeded. It goes
-- back and up in the tree, and consumes captures. In other words, it searches
-- its elder siblings, and the elder siblings of its parents, but not the
-- parents themselves. Neither does it test the children of the
-- siblings/siblings of ancestors.
-- the values captured in the named Cg() are not inserted locally
print((1 * Cg(C"b" * C"c" * C"d", "FOOO") * C"e" * Ct(Cb"FOOO")):match"abcde")
--> "e", { "b", "c", "d" }