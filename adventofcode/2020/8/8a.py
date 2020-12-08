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
        opcode, sgn, arg = self.mem[self.pc]
        print("Executing", opcode, '+' if sgn > 0 else '-', arg, "(", self.acc, self.pc, ")")
        if opcode == 'acc':
            self.acc += sgn * arg
            self.pc += 1
        elif opcode == 'jmp':
            self.pc += sgn * arg
        else:
            self.pc += 1

M = []
for line in sys.stdin:
    M.append(VM.parse_instruction(line.strip()))

vm = VM(M)
states = set()
while True:
    if vm.pc in states:
        print("EXIT", vm.acc)
        sys.exit(0)
    states.add(vm.pc)
    vm.execute()
