import re
import sys

def safeFloat(s):
  m=re.search("([0-9.]*) s", s)
  if m is None:
    return None
  try:
    return float(m.group(1))
  except ValueError:
    return None

f = open(sys.argv[1], "r")
ls = f.readlines()
f.close()

ls = [l[:-1] for l in ls]

data = []
out = []

for l in ls:
  ml = re.search("line", l)
  if ml is None:
    assert(l[-2:] == "\\\\")
    data.append(l[:-2].split('&'))
assert(len(data) > 0)

mins = [None for i in range(len(data[0]))]
for l in data:
  ml = re.search("multicolumn", '&'.join(l))
  if ml is not None:
    continue
  assert(len(l) == len(mins))
  for j in range(len(l)):
    v = safeFloat(l[j])
    if v is not None:
      if mins[j] is None:
        mins[j] = v
      else:
        mins[j] = min(mins[j], v)
for l in data:
  o = []
  for j in range(len(l)):
    if mins[j] is not None and safeFloat(l[j]) == mins[j] and sys.argv[2] == "yes":
      o.append('\\textbf{}{}{}'.format('{', l[j], '}'))
    else:
      o.append(l[j])
  out.append(o)

sep = ('|' if sys.argv[3] == "sep" else '')

f = open(sys.argv[1], "w")
for l in ls:
  ml = re.search("line", l)
  if ml is None:
    cs = out[0]
    out = out[1:]
    if sys.argv[3] != "none":
      cs[5] = '\\multicolumn{}{}{}{}{}'.format('{1}{r@{\\cspace}', sep, '@{\\cspace}}{', cs[5], '}')
    print('{}\\\\'.format('&'.join(cs)), file=f)
  else:
    print(l, file=f)
f.close()
