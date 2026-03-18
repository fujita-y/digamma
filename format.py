import sys
def format_scheme(text):
    out = ""
    indent = 0
    in_str = False
    escape = False
    for i, c in enumerate(text):
        if escape:
            out += c
            escape = False
            continue
        if c == '\\' and in_str:
            escape = True
            out += c
            continue
        if c == '"':
            in_str = not in_str
            out += c
            continue
            
        if in_str:
            out += c
            continue
            
        if c == ';':
            idx = text.find('\n', i)
            if idx == -1: break
            continue
            
        if c == '(':
            indent += 1
            out += c
        elif c == ')':
            indent -= 1
            out += c
            if indent < 0:
                print(f"Negative indent at index {i}!")
                return indent
        else:
            out += c
    return indent

with open(sys.argv[1]) as f:
    text = f.read()

print("Final balance:", format_scheme(text))
