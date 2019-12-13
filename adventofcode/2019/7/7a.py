class IntcodeComputer(object):

    def __init__(self, M):
        self.M = list(M)
        self.ip = 0
        self.input_buffer = lambda: (yield input())
        self.output_buffer = lambda x: print(x)

    def extract_param(self, p, pmodes):
        if p > len(pmodes):
            mode = 0
        else:
            mode = pmodes[-p]

        if mode == 0:
            return self.M[self.M[self.ip + p]]
        elif mode == 1:
            return self.M[self.ip + p]

    def input(self):
        i = next(self.input_buffer())
        print("Got input: %s" % i)
        return i

    def output(self, s):
        self.output_buffer(s)

    def step(self):
        reg = self.M[self.ip]
        op = reg % 100
        pmodes = []

        reg //= 100
        while reg:
            pmodes.insert(0, reg % 10)
            reg //= 10

        if op == 1:    # ADD
            self.M[self.M[self.ip + 3]] = self.extract_param(1, pmodes) + self.extract_param(2, pmodes)
            return self.ip + 4
        elif op == 2:  # MULT
            self.M[self.M[self.ip + 3]] = self.extract_param(1, pmodes) * self.extract_param(2, pmodes)
            return self.ip + 4
        elif op == 3:  # READ
            self.M[self.M[self.ip + 1]] = int(self.input())
            return self.ip + 2
        elif op == 4:  # WRITE
            self.output(self.extract_param(1, pmodes))
            return self.ip + 2
        elif op == 5:  # JIT
            if self.extract_param(1, pmodes):
                return self.extract_param(2, pmodes)
            else:
                return self.ip + 3
        elif op == 6:  # JIF
            if not self.extract_param(1, pmodes):
                return self.extract_param(2, pmodes)
            else:
                return self.ip + 3
        elif op == 7:  # LE
            if self.extract_param(1, pmodes) < self.extract_param(2, pmodes):
                self.M[self.M[self.ip + 3]] = 1
            else:
                self.M[self.M[self.ip + 3]] = 0
            return self.ip + 4
        elif op == 8:  # EQ
            if self.extract_param(1, pmodes) == self.extract_param(2, pmodes):
                self.M[self.M[self.ip + 3]] = 1
            else:
                self.M[self.M[self.ip + 3]] = 0
            return self.ip + 4
        elif op == 99: # RET
            return -1
        else:
            raise ValueError("Invalid opcode %d at position %d" % (self.M[self.ip], self.ip))

    def run(self):
        steps = 0
        while True:
            self.ip = self.step()
            steps += 1
            if self.ip < 0:
                return


def simulate_setting(M, p):
    chain = [IntcodeComputer(M) for _ in p]
    chain[0].input_buffer = lambda: ((yield 0), (yield p[0]))
    chain[0].run()


if __name__ == "__main__":
    M = list(map(int, input().split(",")))
    #simulate_setting(M, [4, 3, 2, 1, 0])
    IntcodeComputer(M).run()
