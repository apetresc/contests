import re
import sys

class VM(object):
    def __init__(self, mem):
        self.acc = 0
        self.pc = 0
        self.mem = mem

    @staticmethod
    def parse_instruction(i):
        opcode, sgn, arg = re.match(r'(.*) ([+\-])(\d+)', i).groups()
        return opcode, 1 if sgn == '+' else -1, int(arg)
    
    def execute(self):
        if self.pc == len(self.mem):
            return 0
        opcode, sgn, arg = self.mem[self.pc]
        print("Executing", opcode, '+' if sgn > 0 else '-', arg, "(", self.acc, self.pc, ")")
        if opcode == 'acc':
            self.acc += sgn * arg
            self.pc += 1
        elif opcode == 'jmp':
            self.pc += sgn * arg
        else:
            self.pc += 1
        return 1

M = []
for line in sys.stdin:
    M.append(VM.parse_instruction(line.strip()))

def find_corruption(M):
    for i, instr in enumerate(M):
        if instr[0] == 'nop':
            M2 = list(M)
            M2[i] = ('jmp', M2[i][1], M2[i][2])
            yield M2
        elif instr[0] == 'jmp':
            M2 = list(M)
            M2[i] = ('nop', M2[i][1], M2[i][2])
            yield M2
        else:
            pass

for cm in find_corruption(M):
    vm = VM(cm)
    for _ in range(len(cm) + 1):
        vm.execute()
    if vm.pc == len(cm):
        print("FOUND IT!", vm.acc)
        sys.exit()
    else:
        print("Not it")
