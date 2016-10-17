local lpeg = require "lpeg"
local cjson = require "cjson.safe"

local encode = cjson.encode
local M, R, S, V, P, C, Ct, Cs, Cf, Cmt, Cg, Cb, Cc, Carg
    = lpeg.match, lpeg.R, lpeg.S, lpeg.V, lpeg.P, lpeg.C,
      lpeg.Ct, lpeg.Cs, lpeg.Cf, lpeg.Cmt, lpeg.Cg, lpeg.Cb, lpeg.Cc, lpeg.Carg


local any = P(1)         -- pattern that accepts one character
local space = S(" \t\n") -- a set with the given chars
local digit = R("09")    -- a set with the range 0-9
local lower = R("az")    -- a set with the range a-z
local upper = R("AZ")    -- a set with the range A-Z

local letter = lower + upper
local alnum = letter + digit


local mt = getmetatable(P(0))


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

local _div = mt.__div
local _add = mt.__add

mt.__div = function(...)
    print("__div", ...)
    return _div(...)
end

mt.__add = function(...)
    print("__add", ...)
    return _add(...)
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

print("==== Cg /")
local p1 = Cc(true) * Cc("X")
local p2 = Cg(Cc(true), "G") * Cc("Y") / print
print("==== Cg, p2")
print(p2:match"")
--> Y
--> 1
print("==== Cg, p1 * p2")
print((p1 * p2):match"")
--> Y
--> ture, X

print("==== Cg, inline")
local p1 = Cc(true) * Cc("X")
local p2 = p1 * Cg(Cc(true)) * Cc("Y") / print
print(p2:match"")
--> true, X, true, Y
--> 1
local p = Cc(9, mt.__pow)
print(p:match"")
--> 9, function

p = P"+" * p
print(p:match"+")
--> 9, function

print(p:match"x")
--> nil

local I = P(function (s, i) print(i, s:sub(1, i-1)); return i+1 end)
local p = I * I * I
print(p:match"abc")

print("==== Cf(folder) and Cc")
-- the initial value is the first value produced by the first capture
Cf(Cc("x", "y", "z") * Cc("b", "c", "d") * Cc("e", "f", "g"), print):match""
--> x, b, c, d
--> nil, e, f, g

print("==== Carg")
print(M(m.Carg(1), 'a', 1, print) == print)
--> true
print(Carg(1) * Carg(2):match("", 1, "a", "b"))
--> a, b

local name = R("AZ", "az", "__") * R("AZ", "az", "__", "09")^0
name = C(name)
local def = name * Carg(1)

print(def:match("id", 1, print))
-- print

local p = Cmt(C((alpha * digit)^1) * Carg(1), function(_, _, m, fn)
    fn(m)
    return true
end)
p = (p + 1)^1
print(p:match("abcd0xe1f3opam", 1, print))

print(M(Cs((C(1)/{a=".", d=".."})^0), "abcdde"))
print(M(Ct((C(1)/{a=".", d=".."})^0), "abcdde"))


print("==== class")
local alpha = C(alpha)
local class = "["
              * Cc("X")
              * C(P"^"^-1)
              * Cf(alpha * (alpha - "]")^0, mt.__add)
              / function (x, c, p) print("div__", x, c == "", p) return c == "^" and 1 - p or p end
              * "]"
print("class", class:match"[abc]")


local p = P"-" / ""
print(p:match"-" == "")
