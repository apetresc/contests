from pathlib import Path
import fs

TARGET = 30_000_000 - (70_000_000 - fs.du(Path('/')))
print(min(fs.du(d) for d in fs.scan(Path('/')) if fs.du(d) >= TARGET))
