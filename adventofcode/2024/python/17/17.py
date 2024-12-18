import re
import sys


class Computer:
    A: int
    B: int
    C: int
    PC: int
    instructions: list[int]
    output: list[int]

    def __init__(self, A: int, B: int, C: int, instructions: list[int]):
        self.A = A
        self.B = B
        self.C = C
        self.PC = 0
        self.instructions = instructions
        self.output = []

    def __repr__(self):
        return f"[A={self.A} B={self.B} C={self.C} PC={self.PC} OUT={self.output}]"

    def combo(self, operand: int) -> int:
        match operand:
            case 0 | 1 | 2 | 3:
                return operand
            case 4:
                return self.A
            case 5:
                return self.B
            case 6:
                return self.C
            case _:
                raise ValueError(f"Invalid combo operand {operand}")

    def step(self) -> bool:
        if self.PC >= len(self.instructions):
            return False

        instruction = self.instructions[self.PC]
        operand = self.instructions[self.PC + 1]
        jumped = False
        match instruction:
            case 0:  # adv
                self.A = self.A >> self.combo(operand)
            case 1:  # bxl
                self.B = self.B ^ operand
            case 2:  # bst
                self.B = self.combo(operand) % 8
            case 3:  # jnz
                if self.A > 0:
                    self.PC = operand
                    jumped = True
            case 4:  # bxc
                self.B = self.B ^ self.C
            case 5:  # out
                self.output.append(self.combo(operand) % 8)
            case 6:  # bdv
                self.B = self.A >> self.combo(operand)
            case 7:  # bdv
                self.C = self.A >> self.combo(operand)

        if not jumped:
            self.PC += 2
        return True

    def run(self, debug=False) -> list[int]:
        while self.step():
            if debug:
                print(self)
        return self.output


def a(A: int, B: int, C: int, program: list[int]) -> str:
    return ",".join(map(str, Computer(A, B, C, program).run()))


def b(A: int, B: int, C: int, program: list[int]) -> int:
    CHUNK_SIZE = 3
    target = 0
    candidates = [0]

    while target < len(program):
        target += 1
        new_candidates = []
        for candidate in candidates:
            for i in range(2**CHUNK_SIZE):
                a = (candidate << CHUNK_SIZE) + i
                output = Computer(a, B, C, program).run(debug=False)
                if output[0:target] == program[-target:]:
                    new_candidates.append(a)
        candidates = list(new_candidates)

    return min(candidates)


def parse_input(path: str) -> tuple[int, int, int, list[int]]:
    with open(path, "r") as f:
        A = int(re.match(r"Register A: (\d+)", f.readline()).group(1))
        B = int(re.match(r"Register B: (\d+)", f.readline()).group(1))
        C = int(re.match(r"Register C: (\d+)", f.readline()).group(1))
        f.readline()
        instructions = list(
            map(int, re.match(r"Program: (.*)", f.readline()).group(1).split(","))
        )
    return A, B, C, instructions


if __name__ == "__main__":
    A, B, C, program = parse_input(sys.argv[1])
    print(a(A, B, C, program))
    print(b(A, B, C, program))
