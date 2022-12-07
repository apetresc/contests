from pathlib import Path
import fs

print(sum(fs.du(d) for d in fs.scan(Path('/')) if fs.du(d) <= 100_000))
